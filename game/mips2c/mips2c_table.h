#pragma once

#include <unordered_map>
#include <vector>
#include <string>

#include "game/kernel/Ptr.h"
#include "common/common_types.h"
#include "common/util/assert.h"

namespace Mips2C {

class LinkedFunctionTable {
 public:
  void reg(const std::string& name, u64 (*exec)(void*), u32 goal_stack_size);
  u32 get(const std::string& name);

 private:
  struct Func {
    u64 (*c_func)(void*);
    Ptr<u8> goal_trampoline;
  };
  std::unordered_map<std::string, Func> m_executes;
};

extern std::unordered_map<std::string, std::vector<void (*)()>> gMips2CLinkCallbacks;
extern LinkedFunctionTable gLinkedFunctionTable;

}  // namespace Mips2C