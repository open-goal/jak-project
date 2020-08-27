#include <cassert>
#include "StaticObject.h"

template <typename T>
uint32_t push_data_to_byte_vector(T data, std::vector<uint8_t>& v) {
  auto* ptr = (uint8_t*)(&data);
  for (std::size_t i = 0; i < sizeof(T); i++) {
    v.push_back(ptr[i]);
  }
  return sizeof(T);
}

std::string StaticString::print() {
  return "static string: " + data;
}

int StaticString::emit_into(
    std::vector<uint8_t>& out_data,
    std::unordered_map<std::string, std::vector<StaticLinkRecord>>& type_links) {
  while (out_data.size() & 7) {
    out_data.push_back(0);
  }

  type_links["string"].emplace_back(StaticLinkRecord::TYPE_PTR, out_data.size());

  for (int i = 0; i < 4; i++) {
    out_data.push_back(0xbe);  // slot for type tag
  }

  offset = out_data.size();

  push_data_to_byte_vector<uint32_t>(data.size(), out_data);

  for (auto c : data) {
    out_data.push_back(c);
  }
  out_data.push_back(0);  // null terminate!

  return offset;
}

std::string StaticFloat::print() {
  return "static float: " + std::to_string(as_float);
}

int StaticFloat::emit_into(
    std::vector<uint8_t>& data,
    std::unordered_map<std::string, std::vector<StaticLinkRecord>>& type_links) {
  (void)type_links;
  while (data.size() & 3) {
    data.push_back(0);
  }

  // no type link needed
  offset = data.size();

  push_data_to_byte_vector<uint32_t>(as_u32, data);
  return offset;
}

std::string StaticStructure::print() {
  return "static structure of size " + std::to_string(data.size());
}

int StaticStructure::emit_into(
    std::vector<uint8_t>& out_data,
    std::unordered_map<std::string, std::vector<StaticLinkRecord>>& type_links) {
  (void)type_links;
  while (out_data.size() & 7) {
    out_data.push_back(0);
  }

  offset = out_data.size();
  out_data.insert(out_data.end(), data.begin(), data.end());

  for (auto& sym : symbol_ptr_recs) {
    auto& dst_vector = type_links[sym.first];
    for (auto& rec : sym.second) {
      dst_vector.emplace_back(StaticLinkRecord::SYMBOL_PTR, rec);
    }
  }
  return offset;
}

std::string StaticBasic::print() {
  return "static basic of size " + std::to_string(data.size());
}

int StaticBasic::emit_into(
    std::vector<uint8_t>& out_data,
    std::unordered_map<std::string, std::vector<StaticLinkRecord>>& type_links) {
  while (out_data.size() & 7) {
    out_data.push_back(0);
  }

  offset = out_data.size();
  type_links[type_name].emplace_back(StaticLinkRecord::TYPE_PTR, offset);

  assert(data.size() >= 4);
  offset += 4;

  out_data.insert(out_data.end(), data.begin(), data.end());

  for (auto& sym : symbol_ptr_recs) {
    auto& dst_vector = type_links[sym.first];
    for (auto& rec : sym.second) {
      dst_vector.emplace_back(StaticLinkRecord::SYMBOL_PTR, rec + offset - 4);
    }
  }
  return offset;
}