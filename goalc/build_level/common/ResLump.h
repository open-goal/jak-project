#pragma once

#include <memory>
#include <string>
#include <vector>

#include "common/common_types.h"
#include "common/math/Vector.h"

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

class DataObjectGenerator;

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

class ResRef : public Res {
 public:
  ResRef(const std::string& name, const std::string& type, size_t ref, float key_frame);

  TagInfo get_tag_info() const override;
  void write_data(DataObjectGenerator& gen) const override;
  int get_alignment() const override;

 private:
  size_t m_ref;
  std::string m_type;
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