#pragma once

#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

#include "common/common_types.h"
#include "common/type_system/TypeSpec.h"
#include "common/util/Assert.h"

#include "goalc/debugger/disassemble.h"
#include "goalc/emitter/Instruction.h"

class FunctionEnv;

namespace goos {
class Object;
class HeapObject;
}  // namespace goos

// location of a variable over a range of IR instructions.
struct VariableLocation {
  int start_ir = 0;
  int end_ir = 0;
  enum class Kind : u8 { REGISTER, STACK } kind = Kind::REGISTER;
  int reg = -1;
  int stack_offset = 0;
};

struct LocalVariableDebugInfo {
  std::string name;
  TypeSpec type;
  bool is_parameter = false;
  std::vector<VariableLocation> locations;

  const VariableLocation* location_at(int ir_idx) const {
    for (const auto& loc : locations) {
      if (ir_idx >= loc.start_ir && ir_idx <= loc.end_ir) {
        return &loc;
      }
    }
    return nullptr;
  }
};

/*!
 * FunctionDebugInfo stores per-function debugging information.
 */
struct FunctionDebugInfo {
  u32 offset_in_seg;  // not including type tag.
  u32 length;
  u8 seg;
  std::string name;
  std::string obj_name;

  std::vector<InstructionInfo> instructions;  // contains mapping to IRs

  std::vector<std::shared_ptr<goos::HeapObject>> code_sources;
  std::vector<std::string> ir_strings;

  // the actual bytes in the object file.
  std::vector<u8> generated_code;
  std::optional<int> stack_usage;

  // named locals and parameters, for showing variable values at a breakpoint
  std::vector<LocalVariableDebugInfo> locals;

  std::string disassemble_debug_info(bool* had_failure, const goos::Reader* reader, bool omit_ir);
};

class DebugInfo {
 public:
  explicit DebugInfo(std::string obj_name);

  FunctionDebugInfo& add_function(const std::string& name, const std::string& obj_name) {
    if (m_functions.find(name) != m_functions.end()) {
      ASSERT(false);
    }
    auto& result = m_functions[name];
    result.name = name;
    result.obj_name = obj_name;
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

  FunctionDebugInfo& function_by_name(const std::string& name) { return m_functions.at(name); }

  const std::unordered_map<std::string, FunctionDebugInfo>& functions() const {
    return m_functions;
  }

  void clear() { m_functions.clear(); }

  std::string disassemble_all_functions(bool* had_failure,
                                        const goos::Reader* reader,
                                        bool omit_ir);
  std::string disassemble_function_by_name(const std::string& name,
                                           bool* had_failure,
                                           const goos::Reader* reader);

 private:
  std::string m_obj_name;
  std::unordered_map<std::string, FunctionDebugInfo> m_functions;
};
