#include "ResLump.h"

#include <algorithm>

#include "common/util/BitUtils.h"

#include "goalc/data_compiler/DataObjectGenerator.h"

#include "third-party/fmt/core.h"

/*
 *  name: crate-3141
 *  .symbol name
    .word 0xce6e6b28
    .type string
    .word 0x10000

scale = 1., 1., 1., 1.,
    .symbol scale
    .word 0xce6e6b28
    .type vector
    .word 0x80010010

    .symbol visvol
    .word 0xce6e6b28
    .type vector
    .word 0x80020020

    .symbol shadow-mask
    .word 0xce6e6b28
    .type uint8
    .word 0x80010040

    .symbol eco-info
    .word 0xce6e6b28
    .type int32
    .word 0x80020044

    .symbol movie-pos
    .word 0xce6e6b28
    .type vector
    .word 0x80010050
    .symbol vis-dist
    .word 0xce6e6b28
    .type float
    .word 0x80010060
 */

Res::Res(const std::string& name, float key_frame) : m_name(name), m_key_frame(key_frame) {}

ResFloat::ResFloat(const std::string& name, const std::vector<float>& values, float key_frame)
    : Res(name, key_frame), m_values(values) {}

TagInfo ResFloat::get_tag_info() const {
  TagInfo result;
  result.elt_type = "float";
  result.elt_count = m_values.size();
  result.inlined = true;
  result.data_size = m_values.size() * sizeof(float);
  return result;
}

void ResFloat::write_data(DataObjectGenerator& gen) const {
  for (auto& val : m_values) {
    gen.add_word_float(val);
  }
}

int ResFloat::get_alignment() const {
  return 16;
}

ResInt32::ResInt32(const std::string& name, const std::vector<s32>& values, float key_frame)
    : Res(name, key_frame), m_values(values) {}

TagInfo ResInt32::get_tag_info() const {
  TagInfo result;
  result.elt_type = "int32";
  result.elt_count = m_values.size();
  result.inlined = true;
  result.data_size = m_values.size() * sizeof(s32);
  return result;
}

void ResInt32::write_data(DataObjectGenerator& gen) const {
  for (auto& val : m_values) {
    gen.add_word(val);
  }
}

int ResInt32::get_alignment() const {
  return 16;
}

ResUint8::ResUint8(const std::string& name, const std::vector<u8>& values, float key_frame)
    : Res(name, key_frame), m_values(values) {}

TagInfo ResUint8::get_tag_info() const {
  TagInfo result;
  result.elt_type = "uint8";
  result.elt_count = m_values.size();
  result.inlined = true;
  result.data_size = align4(m_values.size()) * sizeof(u8);
  return result;
}

void ResUint8::write_data(DataObjectGenerator& gen) const {
  u32 size_words = align4(m_values.size()) / 4;
  auto offset = gen.current_offset_bytes();
  for (u32 i = 0; i < size_words; i++) {
    gen.add_word(0);
  }
  memcpy(gen.data() + offset, m_values.data(), m_values.size());
}

int ResUint8::get_alignment() const {
  return 16;
}

ResUint32::ResUint32(const std::string& name, const std::vector<u32>& values, float key_frame)
    : Res(name, key_frame), m_values(values) {}

TagInfo ResUint32::get_tag_info() const {
  TagInfo result;
  result.elt_type = "uint32";
  result.elt_count = m_values.size();
  result.inlined = true;
  result.data_size = m_values.size() * sizeof(u32);
  return result;
}

void ResUint32::write_data(DataObjectGenerator& gen) const {
  for (auto& val : m_values) {
    gen.add_word(val);
  }
}

int ResUint32::get_alignment() const {
  return 16;
}

ResVector::ResVector(const std::string& name,
                     const std::vector<math::Vector4f>& values,
                     float key_frame)
    : Res(name, key_frame), m_values(values) {}

TagInfo ResVector::get_tag_info() const {
  TagInfo result;
  result.elt_type = "vector";
  result.elt_count = m_values.size();
  result.inlined = true;
  result.data_size = m_values.size() * sizeof(math::Vector4f);
  return result;
}

void ResVector::write_data(DataObjectGenerator& gen) const {
  for (auto& val : m_values) {
    for (int i = 0; i < 4; i++) {
      gen.add_word_float(val[i]);
    }
  }
}

int ResVector::get_alignment() const {
  return 16;
}

ResString::ResString(const std::string& name, const std::vector<std::string>& str, float key_frame)
    : Res(name, key_frame), m_str(str) {}

ResString::ResString(const std::string& name, const std::string& str, float key_frame)
    : Res(name, key_frame), m_str({str}) {}

TagInfo ResString::get_tag_info() const {
  TagInfo result;
  result.elt_type = "string";
  result.elt_count = m_str.size();
  result.inlined = false;
  result.data_size = 4 * m_str.size();
  return result;
}

void ResString::write_data(DataObjectGenerator& gen) const {
  for (auto& str : m_str) {
    gen.add_ref_to_string_in_pool(str);
  }
}

int ResString::get_alignment() const {
  return 4;
}

ResSymbol::ResSymbol(const std::string& name, const std::vector<std::string>& str, float key_frame)
    : Res(name, key_frame), m_str(str) {}

ResSymbol::ResSymbol(const std::string& name, const std::string& str, float key_frame)
    : Res(name, key_frame), m_str({str}) {}

TagInfo ResSymbol::get_tag_info() const {
  TagInfo result;
  result.elt_type = "symbol";
  result.elt_count = m_str.size();
  result.inlined = false;
  result.data_size = 4 * m_str.size();
  return result;
}

void ResSymbol::write_data(DataObjectGenerator& gen) const {
  for (auto& str : m_str) {
    gen.add_symbol_link(str);
  }
}

int ResSymbol::get_alignment() const {
  return 4;
}

void ResLump::add_res(std::unique_ptr<Res> res) {
  m_sorted = false;
  m_res.emplace_back(std::move(res));
}

constexpr int kExtraTagSlots = 10;

void ResLump::sort_res() {
  std::stable_sort(m_res.begin(), m_res.end(),
                   [](const std::unique_ptr<Res>& a, const std::unique_ptr<Res>& b) {
                     u64 a_chars = 0;
                     u64 b_chars = 0;
                     auto& a_name = a->name();
                     auto& b_name = b->name();
                     memcpy(&a_chars, a_name.data(), std::min(sizeof(u64), a_name.size()));
                     memcpy(&b_chars, b_name.data(), std::min(sizeof(u64), b_name.size()));
                     return a_chars < b_chars;
                   });
  m_sorted = true;
}

size_t ResLump::generate_header(DataObjectGenerator& gen,
                                const std::string& most_specific_type) const {
  gen.align_to_basic();
  gen.add_type_tag(most_specific_type);
  auto result = gen.current_offset_bytes();
  gen.add_word(m_res.size());                   // length;
  gen.add_word(m_res.size() + kExtraTagSlots);  // allocated-length
  gen.add_word(0);                              // data base
  gen.add_word(0);                              // data top
  gen.add_word(0);                              // data size;
  gen.add_word(0);                              // extra
  gen.add_word(0);                              // tag.
  return result;
}

void ResLump::generate_tag_list_and_data(DataObjectGenerator& gen, size_t header_to_update) const {
  ASSERT(m_sorted);
  gen.align_to_basic();
  // first is the tag array.
  const size_t tag_array_start = gen.current_offset_bytes();
  const size_t tag_array_size = 16 * (m_res.size() + kExtraTagSlots);
  const size_t tag_array_end = tag_array_start + tag_array_size;

  // next is data
  const size_t data_start = align16(tag_array_end);
  size_t current_data_ptr = data_start;

  // on the first pass through, we'll also build these:
  struct ResRec {
    size_t align;
    size_t data;
    size_t reported_size;
  };
  std::vector<ResRec> recs;

  // first pass to write tags and figure out data layout
  for (auto& res : m_res) {
    auto& rec = recs.emplace_back();
    auto alignment = res->get_alignment();
    while (current_data_ptr % alignment) {
      current_data_ptr++;
    }
    auto tag = res->get_tag_info();

    // name:
    gen.add_symbol_link(res->name());
    // key frame
    gen.add_word_float(res->key_frame());
    // elt type
    gen.add_type_tag(tag.elt_type);

    // packed
    u32 packed = 0;
    ASSERT(current_data_ptr - data_start < UINT16_MAX);
    packed |= (u16)(current_data_ptr - data_start);
    ASSERT(tag.elt_count < (UINT16_MAX >> 1));
    packed |= (((u16)tag.elt_count) << 16);
    if (tag.inlined) {
      packed |= (1 << 31);
    }
    gen.add_word(packed);
    rec.data = current_data_ptr;
    rec.reported_size = tag.data_size;
    rec.align = alignment;
    current_data_ptr += tag.data_size;
  }
  for (int i = 0; i < kExtraTagSlots * 4; i++) {
    gen.add_word(0);
  }
  const size_t data_end = current_data_ptr;  // todo, does this get rounded up at all?

  gen.align_to_basic();
  ASSERT(gen.current_offset_bytes() == data_start);
  current_data_ptr = data_start;

  // second pass to write data
  for (size_t res_idx = 0; res_idx < m_res.size(); res_idx++) {
    const auto& res = m_res[res_idx];
    const auto& rec = recs[res_idx];
    // pad!
    while (current_data_ptr % rec.align) {
      current_data_ptr += 4;
      gen.add_word(0);
    }

    res->write_data(gen);
    ASSERT(gen.current_offset_bytes() - current_data_ptr == rec.reported_size);
    current_data_ptr = gen.current_offset_bytes();
  }
  ASSERT(gen.current_offset_bytes() == data_end);
  ASSERT(data_end == current_data_ptr);

  // update header
  gen.link_word_to_byte((header_to_update + 2 * 4) / 4, data_start);
  gen.link_word_to_byte((header_to_update + 3 * 4) / 4, data_end);
  gen.set_word((header_to_update + 4 * 4) / 4, data_end - data_start);
  gen.link_word_to_byte((header_to_update + 6 * 4) / 4, tag_array_start);
}