#pragma once

#include <unordered_map>
#include <vector>
#include <string>

#include "common/common_types.h"
#include "common/util/assert.h"

namespace Mips2C {

class LinkedFunctionTable {
 public:
  void reg(const std::string& name, u64 (*exec)(void*));

 private:
  struct Func {
    u64 (*c_func)(void*);
    void* goal_trampoline = nullptr;
  };
  std::unordered_map<std::string, Func> m_executes;
};

extern std::unordered_map<std::string, std::vector<void (*)()>> gMips2CLinkCallbacks;
extern LinkedFunctionTable gLinkedFunctionTable;

}  // namespace Mips2C