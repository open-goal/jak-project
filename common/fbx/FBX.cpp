#include "FBX.h"

namespace fbx {

Property::~Property() {
  free(m_array);
}

Property::Property(const Property& other) {
  m_type = other.m_type;
  m_array_length = other.m_array_length;
  if (other.m_array) {
    size_t array_size_bytes = m_array_length * array_element_size(m_type);
    m_array = malloc(array_size_bytes);
    memcpy(m_array, other.m_array, array_size_bytes);
  } else {
    m_value = other.m_value;
  }
}

Property& Property::operator=(const Property& other) {
  if (this != &other) {
    m_type = other.m_type;
    m_array_length = other.m_array_length;

    if (m_array) {
      free(m_array);
      m_array = other.m_array;
    }

    if (other.m_array) {
      size_t array_size_bytes = m_array_length * array_element_size(m_type);
      m_array = malloc(array_size_bytes);
      memcpy(m_array, other.m_array, array_size_bytes);
    } else {
      m_value = other.m_value;
    }
  }

  return *this;
}

Property::Property(const void* data, size_t count, PropertyType type) {
  m_type = type;
  m_array_length = count;
  size_t s = count * array_element_size(type);
  m_array = malloc(s);
  memcpy(m_array, data, s);
}

void Property::serialize(Serializer& ser) {
  ASSERT(ser.is_saving());
  ser.save(m_type);
  switch (m_type) {
    case PropertyType::INT16:
      ser.save(m_value.int16);
      break;
    case PropertyType::BOOL:
      ser.save(m_value.boolean ? 1 : 0);
      break;
    case PropertyType::INT32:
      ser.save(m_value.int32);
      break;
    case PropertyType::FLOAT:
      ser.save(m_value.f);
      break;
    case PropertyType::DOUBLE:
      ser.save(m_value.d);
      break;
    case PropertyType::INT64:
      ser.save(m_value.int64);
      break;
    case PropertyType::STRING:
    case PropertyType::RAW_BINARY:
      ser.save<u32>(m_array_length);  // maybe u64?
      ser.from_raw_data(m_array, m_array_length);
      break;
    case PropertyType::FLOAT_ARRAY:
    case PropertyType::INT64_ARRAY:
    case PropertyType::INT32_ARRAY:
      ser.save<u32>(m_array_length);
      ser.save<u32>(0);
      ser.save<u32>(m_array_length * array_element_size(m_type));
      ser.from_raw_data(m_array, m_array_length * array_element_size(m_type));
      break;
  }
}

void serialize_object_record(Serializer& ser,
                             std::string& name,
                             std::vector<Property>& properties,
                             std::vector<Node>& children) {
  // ENDOFFSET
  size_t start_offset = ser.current_offset();
  ser.save<u64>(UINT64_MAX);

  // NUMPROPERTIES
  ser.save<u64>(properties.size());

  // PROPERTYLISTLEN
  size_t property_list_len_slot = ser.current_offset();
  ser.save<u64>(UINT64_MAX);

  // NAMELEN
  ASSERT(name.size() <= UINT8_MAX);
  ser.save<u8>(name.size());

  // NAME
  ser.from_raw_data(name.data(), name.size());
  size_t start_of_property_list_offset = ser.current_offset();

  for (auto& prop : properties) {
    prop.serialize(ser);
  }
  ser.save_at_offset<u64>(ser.current_offset() - start_of_property_list_offset,
                          property_list_len_slot);

  for (auto& child : children) {
    child.serialize(ser);
  }

  ser.save<u64>(0);
  ser.save<u64>(0);
  ser.save<u64>(0);
  ser.save<u8>(0);

  size_t end_offset = ser.current_offset();
  ser.from_ptr_at_offset(&end_offset, start_offset);
}

void Node::serialize(Serializer& ser) {
  serialize_object_record(ser, node_type, properties, children);
}

void FbxRoot::serialize(Serializer& ser) {
  ASSERT(ser.is_saving());

  const char magic_header[] = "Kaydara FBX Binary  ";
  ser.from_raw_data((void*)magic_header, 21);
  uint8_t magic_bytes[2] = {0x1a, 0x00};
  ser.from_raw_data(magic_bytes, 2);

  uint32_t version = 7500;  // a comment from "Bill" on some blog says that 7.5 uses 64-bit offsets
  ser.from_ptr(&version);

  //  std::vector<Property> empty_properties;
  //  std::string empty_name;
  //  serialize_object_record(ser, empty_name, empty_properties, top_level_nodes);

  for (auto& c : top_level_nodes) {
    c.serialize(ser);
  }
  ser.save<u64>(0);
  ser.save<u64>(0);
  ser.save<u64>(0);
  ser.save<u8>(0);
}

}  // namespace fbx