#pragma once

#include <vector>
#include <cstdlib>
#include <cstring>
#include "common/common_types.h"
#include "common/util/Assert.h"
#include "common/util/Serializer.h"

// Description of the FBX format:
// The "Nodes" are the organization structure and are arranged into a tree
// Nodes can store "Properties" which are the actual data
// There is a special "FbxRoot" that is the top level node for the file.
// Note that this tree structure is completely separate from the tree structure described in the
// connections section.

namespace fbx {

// null should be 25 bytes, use u64's for the first three fields of node record.

enum class PropertyType : char {
  // single values
  INT16 = 'Y',
  BOOL = 'C',
  INT32 = 'I',
  FLOAT = 'F',
  DOUBLE = 'D',
  INT64 = 'L',

  // array of values
  FLOAT_ARRAY = 'f',
  // DOUBLE_ARRAY = 'd',
  INT64_ARRAY = 'l',
  INT32_ARRAY = 'i',
  // BOOL_ARRAY = 'b',

  // specials
  STRING = 'S',
  RAW_BINARY = 'R',
};

inline size_t array_element_size(PropertyType type) {
  switch (type) {
    case PropertyType::FLOAT_ARRAY:
      return sizeof(float);
    case PropertyType::INT64_ARRAY:
      return sizeof(s64);
    case PropertyType::INT32_ARRAY:
      return sizeof(s32);
    case PropertyType::STRING:
    case PropertyType::RAW_BINARY:
      return sizeof(u8);
    default:
      ASSERT(false);
  }
}

class Property {
 public:
  ~Property();
  Property(const Property& other);
  Property& operator=(const Property& other);

  explicit Property(s16 val) {
    m_type = PropertyType::INT16;
    m_value.int16 = val;
  }

  //  explicit Property(bool val) {
  //    m_type = PropertyType::BOOL;
  //    m_value.boolean = val;
  //  }

  explicit Property(s32 val) {
    m_type = PropertyType::INT32;
    m_value.int32 = val;
  }

  explicit Property(float val) {
    m_type = PropertyType::FLOAT;
    m_value.f = val;
  }

  explicit Property(double val) {
    m_type = PropertyType::DOUBLE;
    m_value.d = val;
  }

  explicit Property(s64 val) {
    m_type = PropertyType::INT64;
    m_value.int64 = val;
  }

  Property(const void* data, size_t count, PropertyType type);

  Property(const s32* val, size_t sz) : Property(val, sz, PropertyType::INT32_ARRAY) {}
  Property(const s64* val, size_t sz) : Property(val, sz, PropertyType::INT64_ARRAY) {}
  Property(const float* val, size_t sz) : Property(val, sz, PropertyType::FLOAT_ARRAY) {}
  explicit Property(const std::vector<float>& vals) : Property(vals.data(), vals.size()) {}
  explicit Property(const std::vector<s32>& vals) : Property(vals.data(), vals.size()) {}
  explicit Property(const std::string& str)
      : Property(str.data(), str.size(), PropertyType::STRING) {}

  void serialize(Serializer& ser);

 private:
  union PropertyValue {
    s16 int16;
    s32 int32;
    s64 int64;
    bool boolean;
    double d;
    float f;
  };
  PropertyType m_type;
  PropertyValue m_value;
  void* m_array = nullptr;
  size_t m_array_length = 0;
};

struct Node {
  Node() = default;
  Node(const std::string& str) : node_type(str) {}
  std::string node_type;
  std::vector<Property> properties;
  std::vector<Node> children;
  Node& add_node(const std::string& name) { return children.emplace_back(name); }
  void serialize(Serializer& ser);
};

struct FbxRoot {
  std::vector<Node> top_level_nodes;
  void serialize(Serializer& ser);
  Node& add_node(const std::string& name) { return top_level_nodes.emplace_back(name); }
};

}  // namespace fbx