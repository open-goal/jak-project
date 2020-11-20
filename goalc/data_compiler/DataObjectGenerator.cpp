#include <cstring>
#include <algorithm>
#include <cassert>
#include "DataObjectGenerator.h"
#include "common/link_types.h"

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

void DataObjectGenerator::link_word_to_word(int source, int target, int offset) {
  link_word_to_byte(source, target * 4 + offset);
}

void DataObjectGenerator::link_word_to_byte(int source_word, int target_byte) {
  PointerLinkRecord rec;
  rec.source_word = source_word;
  rec.target_byte = target_byte;
  m_ptr_links.push_back(rec);
}

int DataObjectGenerator::add_ref_to_string_in_pool(const std::string& str) {
  auto result = int(m_words.size());
  m_words.push_back(0);
  m_string_pool[str].push_back(result);
  return result;
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

void DataObjectGenerator::align(int alignment_words) {
  while (m_words.size() % alignment_words) {
    m_words.push_back(0);
  }
}

int DataObjectGenerator::words() const {
  return int(m_words.size());
}

std::vector<u8> DataObjectGenerator::generate_v2() {
  // add string data at the end.
  add_strings();

  // Generate the link table.
  std::vector<u8> link = generate_link_table();

  // add words

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
    assert(diff >= 0);
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

  // todo symbols
  assert(m_symbol_links.empty());

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
      assert(diff >= 0);
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