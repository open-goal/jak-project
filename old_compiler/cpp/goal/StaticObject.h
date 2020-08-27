#ifndef JAK_STATICOBJECT_H
#define JAK_STATICOBJECT_H

#include <string>
#include <vector>
#include <unordered_map>
#include <cstdint>
#include "codegen/StaticLinkRecord.h"

class StaticObject {
 public:
  virtual std::string print() = 0;
  virtual int emit_into(
      std::vector<uint8_t>& data,
      std::unordered_map<std::string, std::vector<StaticLinkRecord>>& type_links) = 0;

  virtual int load_size() = 0;
  int segment = -1;
  int offset = -1;
};

class StaticFloat : public StaticObject {
 public:
  union {
    float as_float;
    uint32_t as_u32;
  };

  int load_size() override { return 4; }

  std::string print() override;
  int emit_into(
      std::vector<uint8_t>& data,
      std::unordered_map<std::string, std::vector<StaticLinkRecord>>& type_links) override;
};

class StaticString : public StaticObject {
 public:
  std::string data;
  std::string print() override;

  int load_size() override { return -1; }

  int emit_into(
      std::vector<uint8_t>& data,
      std::unordered_map<std::string, std::vector<StaticLinkRecord>>& type_links) override;
};

class StaticStructure : public StaticObject {
 public:
  std::vector<uint8_t> data;
  std::string print() override;
  std::unordered_map<std::string, std::vector<int>> symbol_ptr_recs;
  int emit_into(
      std::vector<uint8_t>& data,
      std::unordered_map<std::string, std::vector<StaticLinkRecord>>& type_links) override;

  int load_size() override { return -1; }
};

class StaticBasic : public StaticStructure {
 public:
  std::string type_name;
  std::string print() override;
  int emit_into(
      std::vector<uint8_t>& data,
      std::unordered_map<std::string, std::vector<StaticLinkRecord>>& type_links) override;

  int load_size() override { return -1; }
};

#endif  // JAK_STATICOBJECT_H
