#pragma once

#include <string>
#include <unordered_map>
#include <cassert>
#include "common/common_types.h"

struct FunctionDebugInfo {
  u32 offset_in_seg;  // not including type tag.
  u32 length;
  u8 seg;
};

class DebugInfo {
 public:
  explicit DebugInfo(std::string obj_name);

  FunctionDebugInfo& add_function(const std::string& name) {
    if (m_functions.find(name) != m_functions.end()) {
      assert(false);
    }
    return m_functions[name];
  }

  bool lookup_function(FunctionDebugInfo** info, std::string* name, u32 offset, u8 seg) {
    for (auto& kv : m_functions) {
      auto start = kv.second.offset_in_seg;
      auto end = start + kv.second.length;
      if (offset >= start && offset < end && seg == kv.second.seg) {
        *info = &kv.second;
        *name = kv.first;
        return true;
      }
    }
    return false;
  }

  void clear() { m_functions.clear(); }

 private:
  std::string m_obj_name;
  std::unordered_map<std::string, FunctionDebugInfo> m_functions;
};
