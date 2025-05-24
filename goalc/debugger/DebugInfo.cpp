#include "DebugInfo.h"

#include <utility>

#include "fmt/core.h"

DebugInfo::DebugInfo(std::string obj_name) : m_obj_name(std::move(obj_name)) {}

std::string FunctionDebugInfo::disassemble_debug_info(bool* had_failure,
                                                      const goos::Reader* reader,
                                                      bool omit_ir) {
  std::string result = fmt::format("[{}]\n", name);
  result += disassemble_x86_function(generated_code.data(), generated_code.size(), reader, 0x10000,
                                     0x10000, instructions, code_sources, ir_strings, had_failure,
                                     true, omit_ir);

  return result;
}

std::string DebugInfo::disassemble_all_functions(bool* had_failure,
                                                 const goos::Reader* reader,
                                                 bool omit_ir) {
  std::string result;
  for (auto& kv : m_functions) {
    result += kv.second.disassemble_debug_info(had_failure, reader, omit_ir) + "\n\n";
  }
  return result;
}

std::string DebugInfo::disassemble_function_by_name(const std::string& name,
                                                    bool* had_failure,
                                                    const goos::Reader* reader) {
  std::string result;
  for (auto& kv : m_functions) {
    if (kv.second.name == name) {
      result += kv.second.disassemble_debug_info(had_failure, reader, false) + "\n\n";
    }
  }
  return result;
}
