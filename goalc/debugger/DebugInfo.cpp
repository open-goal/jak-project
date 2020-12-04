#include <utility>
#include <vector>
#include "DebugInfo.h"
#include "third-party/fmt/core.h"

DebugInfo::DebugInfo(std::string obj_name) : m_obj_name(std::move(obj_name)) {}

std::string FunctionDebugInfo::disassemble_debug_info(bool* had_failure) {
  std::string result = fmt::format("[{}]\n", name);
  std::vector<u8> data;
  u8 temp[128];
  for (const auto& x : instructions) {
    auto count = x.instruction.emit(temp);
    for (int i = 0; i < count; i++) {
      data.push_back(temp[i]);
    }
  }

  result += disassemble_x86_function(data.data(), data.size(), 0x10000, 0x10000, instructions, irs,
                                     had_failure);

  return result;
}

std::string DebugInfo::disassemble_all_functions(bool* had_failure) {
  std::string result;
  for (auto& kv : m_functions) {
    result += kv.second.disassemble_debug_info(had_failure) + "\n\n";
  }
  return result;
}

std::string DebugInfo::disassemble_function_by_name(const std::string& name, bool* had_failure) {
  std::string result;
  for (auto& kv : m_functions) {
    if (kv.second.name == name) {
      result += kv.second.disassemble_debug_info(had_failure) + "\n\n";
    }
  }
  return result;
}