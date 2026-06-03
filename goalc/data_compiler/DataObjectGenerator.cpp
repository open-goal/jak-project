#include "DataObjectGenerator.h"

#include <algorithm>
#include <cstring>

#include "common/link_types.h"
#include "common/util/Assert.h"
#include "common/util/BitUtils.h"

namespace {
template <typename T>
void add_data_to_vector(const T& data, std::vector<u8>* vec) {
  auto loc = vec->size();
  vec->resize(loc + sizeof(T));
  memcpy(vec->data() + loc, &data, sizeof(T));
}

void push_variable_length_integer(u32 value, std::vector<u8>* vec) {
  while (value > UINT8_MAX) {
    vec->push_back(UINT8_MAX);
    value -= UINT8_MAX;
  }

  if (value == UINT8_MAX) {
    vec->push_back(UINT8_MAX);
    vec->push_back(0);
  } else {
    vec->push_back(value);
  }
}

void push_better_variable_length_integer(u32 value, std::vector<u8>* vec) {
  if (value > 0xffffff) {
    vec->push_back((value & 0xff) | 3);
    vec->push_back((value >> 8) & 0xff);
    vec->push_back((value >> 16) & 0xff);
    vec->push_back((value >> 24) & 0xff);
  } else if (value > 0xffff) {
    vec->push_back((value & 0xff) | 2);
    vec->push_back((value >> 8) & 0xff);
    vec->push_back((value >> 16) & 0xff);
  } else if (value > 0xff) {
    vec->push_back((value & 0xff) | 1);
    vec->push_back((value >> 8) & 0xff);
  } else {
    vec->push_back(value & 0xff);
  }
}
}  // namespace

int DataObjectGenerator::add_word(u32 word) {
  auto result = int(m_words.size());
  m_words.push_back(word);
  return result;
}

int DataObjectGenerator::add_word_float(float f) {
  auto result = int(m_words.size());
  m_words.push_back(0);
  memcpy(&m_words.back(), &f, sizeof(float));
  return result;
}

void DataObjectGenerator::add_goos_obj(
    const goos::Object& obj,
    const bool last_obj,
    const std::optional<std::map<std::string, size_t>>& entity_slots,
    const std::optional<std::map<int, size_t>>& actor_group_slots) {
  // lg::info("add_goos_obj: {}", obj.inspect());
  size_t cur_slot = 0;
  size_t next_slot = 0;
  if (obj.type == goos::ObjectType::INTEGER || obj.type == goos::ObjectType::FLOAT) {
    cur_slot = add_word(0);
    next_slot = add_word(0);
  }
  switch (obj.type) {
    case goos::ObjectType::INTEGER: {
      add_type_tag("binteger");
      link_word_to_word(cur_slot, add_word(obj.as_int()));
    } break;
    case goos::ObjectType::FLOAT: {
      add_type_tag("bfloat");
      link_word_to_word(cur_slot, add_word_float(obj.as_float()));
    } break;
    case goos::ObjectType::SYMBOL:
      add_symbol_link(obj.as_symbol().name_ptr);
      break;
    case goos::ObjectType::STRING: {
      std::string str = obj.as_string()->print();
      std::erase(str, '\"');
      add_ref_to_string_in_pool(str);
    } break;
    case goos::ObjectType::PAIR: {
      auto pair_slot = add_word(0);
      if (last_obj) {
        add_empty_list();
      } else {
        next_slot = add_word(0);
      }
      link_word_to_byte(pair_slot, add_pair(obj, entity_slots, actor_group_slots));
      if (!last_obj) {
        link_word_to_byte(next_slot, current_offset_bytes() + 2);
      }
    } break;
    default:
      ASSERT_MSG(false, fmt::format("Unsupported object type in pair: {}", obj.inspect()));
  }
  if (!obj.is_pair()) {
    if (last_obj) {
      if (obj.type == goos::ObjectType::INTEGER || obj.type == goos::ObjectType::FLOAT) {
        link_word_to_symbol("_empty_", next_slot);
      } else {
        add_empty_list();
      }
    } else {
      if (obj.type == goos::ObjectType::INTEGER || obj.type == goos::ObjectType::FLOAT) {
        link_word_to_byte(next_slot, current_offset_bytes() + 2);
      } else {
        link_word_to_byte(add_word(0), current_offset_bytes() + 2);
      }
    }
  }
}

int DataObjectGenerator::add_pair(const goos::Object& obj,
                                  const std::optional<std::map<std::string, size_t>>& entity_slots,
                                  const std::optional<std::map<int, size_t>>& actor_group_slots) {
  size_t pair_slot = current_offset_bytes() + 2;
  // lg::info("add_pair: parsing {}", obj.print());

  goos::Object current = obj;
  while (!current.is_empty_list()) {
    // lg::info("add_pair: current {}", current.print());
    auto next = current.as_pair()->car;
    // special case for entities or actor groups: script pairs can have references to actor groups
    // or entities from the level data
    if (next.is_pair() && next.as_pair()->car.is_symbol()) {
      if (!strcmp(next.as_pair()->car.as_symbol().name_ptr, "entity-actor")) {
        if (entity_slots.has_value()) {
          auto pair = next.as_pair();
          auto type = pair->car;
          auto val = pair->cdr.as_pair()->car.as_symbol().name_ptr;
          auto slot = entity_slots->find(val);
          if (slot != entity_slots->end()) {
            link_word_to_byte(add_word(0), slot->second);
            if (current.as_pair()->cdr.is_empty_list()) {
              add_empty_list();
            } else {
              link_word_to_byte(add_word(0), current_offset_bytes() + 2);
            }
          } else {
            ASSERT_MSG(false, fmt::format("error in pair {}: for {}, entity-actor {} not found",
                                          obj.print(), pair->print(), val));
          }
        }
      } else if (!strcmp(next.as_pair()->car.as_symbol().name_ptr, "actor-group")) {
        if (actor_group_slots.has_value()) {
          auto pair = next.as_pair();
          auto type = pair->car;
          auto val = pair->cdr.as_pair()->car.as_int();
          auto slot = actor_group_slots->find(val);
          if (slot != actor_group_slots->end()) {
            link_word_to_byte(add_word(0), slot->second);
            if (current.as_pair()->cdr.is_empty_list()) {
              add_empty_list();
            } else {
              link_word_to_byte(add_word(0), current_offset_bytes() + 2);
            }
          } else {
            ASSERT_MSG(false, fmt::format("error in pair {}: for {}, got invalid id {}",
                                          obj.print(), pair->print(), val));
          }
        }
      } else {
        if (current.as_pair()->cdr.is_empty_list()) {
          // lg::info("add_pair: next (last elem) {}", next.print());
          add_goos_obj(next, true, entity_slots, actor_group_slots);
        } else {
          // lg::info("add_pair: next {}", next.print());
          add_goos_obj(next, false, entity_slots, actor_group_slots);
        }
      }
    } else {
      if (current.as_pair()->cdr.is_empty_list()) {
        // lg::info("add_pair: next (last elem) {}", next.print());
        add_goos_obj(next, true, entity_slots, actor_group_slots);
      } else {
        // lg::info("add_pair: next {}", next.print());
        add_goos_obj(next, false, entity_slots, actor_group_slots);
      }
    }
    current = current.as_pair()->cdr;
  }
  return pair_slot;
}

int DataObjectGenerator::add_empty_list() {
  auto result = int(m_words.size());
  m_words.push_back(0);
  m_symbol_links["_empty_"].push_back(result);
  return result;
}

void DataObjectGenerator::set_word(u32 word_idx, u32 val) {
  m_words.at(word_idx) = val;
}

void DataObjectGenerator::link_word_to_word(int source, int target, int offset) {
  link_word_to_byte(source, target * 4 + offset);
}

void DataObjectGenerator::link_word_to_byte(int source_word, int target_byte) {
  auto& rec = m_ptr_links.emplace_back();
  rec.source_word = source_word;
  rec.target_byte = target_byte;
}

int DataObjectGenerator::add_ref_to_string_in_pool(const std::string& str) {
  auto result = int(m_words.size());
  m_words.push_back(0);
  m_string_pool[str].push_back(result);
  return result;
}

void DataObjectGenerator::link_word_to_string_in_pool(const std::string& str, int word_idx) {
  m_string_pool[str].push_back(word_idx);
}

int DataObjectGenerator::add_type_tag(const std::string& str) {
  auto result = int(m_words.size());
  m_words.push_back(0);
  m_type_links[str].push_back(result);
  return result;
}

int DataObjectGenerator::add_symbol_link(const std::string& str) {
  auto result = int(m_words.size());
  m_words.push_back(0);
  m_symbol_links[str].push_back(result);
  return result;
}

void DataObjectGenerator::link_word_to_symbol(const std::string& str, int word_idx) {
  m_symbol_links[str].push_back(word_idx);
}

void DataObjectGenerator::align(int alignment_words) {
  while (m_words.size() % alignment_words) {
    m_words.push_back(0);
  }
}

void DataObjectGenerator::align_to_basic() {
  align(4);
}

int DataObjectGenerator::words() const {
  return int(m_words.size());
}

std::vector<u8> DataObjectGenerator::generate_v2() {
  // add string data at the end.
  add_strings();

  // Generate the link table.
  std::vector<u8> link = generate_link_table();

  // header
  LinkHeaderV2 header;
  header.type_tag = 0xffffffff;
  header.version = 2;
  header.length = sizeof(LinkHeaderV2) + link.size();

  // build
  std::vector<u8> result;
  add_data_to_vector(header, &result);
  result.insert(result.end(), link.begin(), link.end());

  auto start = result.size();
  result.resize(result.size() + m_words.size() * 4);
  memcpy(result.data() + start, m_words.data(), m_words.size() * 4);

  while (result.size() % 16) {
    result.push_back(0);
  }

  return result;
}

std::vector<u8> DataObjectGenerator::generate_v4() {
  // add string data at the end.
  add_strings();

  // Generate the link table.
  std::vector<u8> link = generate_link_table();

  // header (first)
  LinkHeaderV4 first_header;
  first_header.type_tag = 0xffffffff;
  first_header.length = sizeof(LinkHeaderV2) + link.size();
  first_header.version = 4;
  first_header.code_size = align16(4 * m_words.size());

  LinkHeaderV2 second_header;
  second_header.version = 2;
  second_header.type_tag = 0xffffffff;
  second_header.length = first_header.length;

  std::vector<u8> result;
  add_data_to_vector(first_header, &result);
  auto start = result.size();
  result.resize(result.size() + m_words.size() * 4);
  memcpy(result.data() + start, m_words.data(), m_words.size() * 4);
  while (result.size() % 16) {
    result.push_back(0);
  }
  add_data_to_vector(second_header, &result);
  result.insert(result.end(), link.begin(), link.end());
  return result;
}

std::vector<u8> DataObjectGenerator::generate_link_table() {
  std::vector<u8> link;

  // pointer links are in source order.
  std::sort(m_ptr_links.begin(), m_ptr_links.end(),
            [](const PointerLinkRecord& a, const PointerLinkRecord& b) {
              return a.source_word < b.source_word;
            });

  int i = 0;

  u32 last_word = 0;
  while (i < int(m_ptr_links.size())) {
    // seeking
    auto& entry = m_ptr_links.at(i);
    int diff = int(entry.source_word) - int(last_word);
    last_word = entry.source_word + 1;
    ASSERT(diff >= 0);
    push_variable_length_integer(diff, &link);
    m_words.at(entry.source_word) = entry.target_byte;

    // count.
    int consecutive = 1;
    for (;;) {
      if (i + 1 < int(m_ptr_links.size()) &&
          m_ptr_links.at(i + 1).source_word == m_ptr_links.at(i).source_word + 1) {
        m_words.at(m_ptr_links.at(i + 1).source_word) = m_ptr_links.at(i + 1).target_byte;
        last_word = m_ptr_links.at(i + 1).source_word + 1;
        consecutive++;
        i++;
      } else {
        break;
      }
    }

    push_variable_length_integer(consecutive, &link);
    i++;
  }
  push_variable_length_integer(0, &link);

  for (auto& sl : m_symbol_links) {
    // insert name. first char won't have the highest bit set
    for (auto c : sl.first) {
      link.push_back(c);
    }
    link.push_back(0);
    std::sort(sl.second.begin(), sl.second.end());
    int prev = 0;

    for (auto& x : sl.second) {
      int diff = x - prev;
      ASSERT(diff >= 0);
      push_better_variable_length_integer(diff * 4, &link);
      m_words.at(x) = 0xffffffff;
      prev = x;
    }
    link.push_back(0);
  }

  // types
  for (auto& tl : m_type_links) {
    link.push_back(0x80);
    for (auto c : tl.first) {
      link.push_back(c);
    }
    link.push_back(0);

    std::sort(tl.second.begin(), tl.second.end());
    int prev = 0;

    for (auto& x : tl.second) {
      int diff = x - prev;
      ASSERT(diff >= 0);
      push_better_variable_length_integer(diff * 4, &link);
      m_words.at(x) = 0xffffffff;
      prev = x;
    }
    link.push_back(0);
  }
  push_variable_length_integer(0, &link);

  // align to 16 bytes for data start!
  while ((link.size() + sizeof(LinkHeaderV2)) % 64) {
    link.push_back(0);
  }
  return link;
}

void DataObjectGenerator::add_strings() {
  for (auto& entry : m_string_pool) {
    // add the string
    align(4);
    add_type_tag("string");
    auto target_word = add_word(entry.first.length());
    std::vector<u8> string_buff;
    for (auto c : entry.first) {
      string_buff.push_back(c);
    }
    string_buff.push_back(0);
    while (string_buff.size() & 3) {
      string_buff.push_back(0);
    }

    for (int i = 0; i < int(string_buff.size()) / 4; i++) {
      add_word(*(u32*)(string_buff.data() + i * 4));
    }

    for (auto& source : entry.second) {
      link_word_to_word(source, target_word);
    }
  }
}
