#pragma once

#include <vector>
#include <string>
#include <unordered_map>
#include <cassert>
#include "common/common_types.h"
#include "goalc/emitter/Instruction.h"
#include "goalc/debugger/disassemble.h"

struct FunctionDebugInfo {
  u32 offset_in_seg;  // not including type tag.
  u32 length;
  u8 seg;
  std::string name;

  std::vector<std::string> irs;
  std::vector<InstructionInfo> instructions;

  std::string disassemble_debug_info(bool* had_failure);
};

class DebugInfo {
 public:
  explicit DebugInfo(std::string obj_name);

  FunctionDebugInfo& add_function(const std::string& name) {
    if (m_functions.find(name) != m_functions.end()) {
      assert(false);
    }
    auto& result = m_functions[name];
    result.name = name;
    return result;
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

  std::string disassemble_all_functions(bool* had_failure);
  std::string disassemble_function_by_name(const std::string& name, bool* had_failure);

 private:
  std::string m_obj_name;
  std::unordered_map<std::string, FunctionDebugInfo> m_functions;
};
