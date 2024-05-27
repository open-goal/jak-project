#pragma once

#include <functional>
#include <memory>
#include <string>
#include <vector>

#include "common/common_types.h"
#include "common/math/Vector.h"

#include "goalc/data_compiler/DataObjectGenerator.h"

/*
 (deftype res-tag (uint128)
  ((name        symbol  :offset 0)
   (key-frame   float   :offset 32)
   (elt-type    type    :offset 64)
   (data-offset uint16  :offset 96)
   (elt-count   uint32  :offset 112 :size 15)
   (inlined?    uint8   :offset 127 :size 1) ;; guess.
   )
  :flag-assert #x900000010
  )
 */

struct TagInfo {
  std::string elt_type;
  u32 elt_count = 0;
  bool inlined = false;
  u32 data_size = 0;
};

class Res {
 public:
  Res(const std::string& name, float key_frame);
  const std::string& name() const { return m_name; }
  float key_frame() const { return m_key_frame; }
  virtual TagInfo get_tag_info() const = 0;
  virtual void write_data(DataObjectGenerator& gen) const = 0;
  virtual int get_alignment() const = 0;
  virtual ~Res() = default;

 private:
  std::string m_name;
  float m_key_frame = 0;
};

class ResFloat : public Res {
 public:
  ResFloat(const std::string& name, const std::vector<float>& values, float key_frame);
  TagInfo get_tag_info() const override;
  void write_data(DataObjectGenerator& gen) const override;
  int get_alignment() const override;

 private:
  std::vector<float> m_values;
};

class ResInt32 : public Res {
 public:
  ResInt32(const std::string& name, const std::vector<s32>& values, float key_frame);
  TagInfo get_tag_info() const override;
  void write_data(DataObjectGenerator& gen) const override;
  int get_alignment() const override;

 private:
  std::vector<s32> m_values;
};

class ResUint8 : public Res {
 public:
  ResUint8(const std::string& name, const std::vector<u8>& values, float key_frame);
  TagInfo get_tag_info() const override;
  void write_data(DataObjectGenerator& gen) const override;
  int get_alignment() const override;

 private:
  std::vector<u8> m_values;
};

class ResUint32 : public Res {
 public:
  ResUint32(const std::string& name, const std::vector<u32>& values, float key_frame);
  TagInfo get_tag_info() const override;
  void write_data(DataObjectGenerator& gen) const override;
  int get_alignment() const override;

 private:
  std::vector<u32> m_values;
};

class ResVector : public Res {
 public:
  ResVector(const std::string& name, const std::vector<math::Vector4f>& values, float key_frame);
  TagInfo get_tag_info() const override;
  void write_data(DataObjectGenerator& gen) const override;
  int get_alignment() const override;

 private:
  std::vector<math::Vector4f> m_values;
};

class ResString : public Res {
 public:
  ResString(const std::string& name, const std::vector<std::string>& str, float key_frame);
  ResString(const std::string& name, const std::string& str, float key_frame);

  TagInfo get_tag_info() const override;
  void write_data(DataObjectGenerator& gen) const override;
  int get_alignment() const override;

 private:
  std::vector<std::string> m_str;
};

class ResSymbol : public Res {
 public:
  ResSymbol(const std::string& name, const std::vector<std::string>& str, float key_frame);
  ResSymbol(const std::string& name, const std::string& str, float key_frame);

  TagInfo get_tag_info() const override;
  void write_data(DataObjectGenerator& gen) const override;
  int get_alignment() const override;

 private:
  std::vector<std::string> m_str;
};

class ResType : public Res {
 public:
  ResType(const std::string& name, const std::vector<std::string>& str, float key_frame);
  ResType(const std::string& name, const std::string& str, float key_frame);

  TagInfo get_tag_info() const override;
  void write_data(DataObjectGenerator& gen) const override;
  int get_alignment() const override;

 private:
  std::vector<std::string> m_str;
};

template <typename T>
class ResArray : public Res {
 public:
  ResArray(const std::string& name,
           const std::string& content_type,
           bool basic,
           bool packed,
           const std::vector<T>& data,
           std::function<size_t(T, DataObjectGenerator&, bool)> write_data_func,
           float key_frame)
      : Res(name, key_frame), m_data(data) {
    m_content_type = content_type;
    m_basic = basic;
    m_packed = packed;
    m_write_data_func = write_data_func;
  }

  TagInfo get_tag_info() const {
    TagInfo result;
    result.elt_type = "array";
    result.elt_count = 1;
    result.inlined = false;
    size_t total_elts_size = 0;
    for (auto& data : m_data) {
      total_elts_size += data.calc_data_size();
    }
    result.data_size = 4;
    return result;
  }
  void write_data(DataObjectGenerator& gen) const override {
    gen.link_word_to_byte(gen.add_word(0), write_array_data(gen));
  }
  size_t get_actual_data_size() {
    size_t total_elts_size = 0;
    for (auto& data : m_data) {
      total_elts_size += data.calc_data_size();
    }
    return total_elts_size;
  }
  size_t write_array_data(DataObjectGenerator& gen) const {
    std::vector<size_t> data_slots;
    std::vector<size_t> content_slots;
    gen.align_to_basic();
    gen.add_type_tag("array");
    size_t result = gen.current_offset_bytes();
    gen.add_word(m_data.size());       // 0 (length)
    gen.add_word(m_data.size());       // 4 (allocated-length)
    gen.add_type_tag(m_content_type);  // 8 (content-type)
    for (auto& data : m_data) {
      content_slots.push_back(gen.add_word(0));
    }
    gen.align(4);
    for (size_t i = 0; i < content_slots.size(); i++) {
      auto data = m_data.at(i);
      if (m_basic) {
        gen.align_to_basic();
        gen.add_type_tag(m_content_type);
      }
      data_slots.push_back(gen.current_offset_bytes());
      m_write_data_func(data, gen, m_packed);
    }
    for (size_t i = 0; i < content_slots.size(); i++) {
      gen.link_word_to_byte(content_slots.at(i), data_slots.at(i));
    }
    return result;
  }
  int get_alignment() const override { return 4; }

 private:
  std::vector<T> m_data;
  std::string m_content_type;
  bool m_basic;
  bool m_packed;
  std::function<size_t(T, DataObjectGenerator&, bool)> m_write_data_func;
  size_t m_actual_size;
};

/*
(deftype res-lump (basic)
  ((length           int32              :offset-assert 4)
   (allocated-length int32              :offset-assert 8)
   (data-base        pointer            :offset-assert 12)
   (data-top         pointer            :offset-assert 16)
   (data-size        int32              :offset-assert 20)
   (extra            entity-links       :offset-assert 24) ; looks like 0 here
   (tag              (pointer res-tag)  :offset-assert 28)
 */
class ResLump {
 public:
  void add_res(std::unique_ptr<Res> res);
  void sort_res();
  // extra tag slots seems to be 10, in all cases?
  size_t generate_header(DataObjectGenerator& gen, const std::string& most_specific_type) const;
  void generate_tag_list_and_data(DataObjectGenerator& gen, size_t header_to_update) const;

 private:
  std::vector<std::unique_ptr<Res>> m_res;
  bool m_sorted = false;
};