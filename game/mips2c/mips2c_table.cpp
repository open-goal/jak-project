#include "mips2c_table.h"
#include "common/log/log.h"

namespace draw_string {
extern void link();
}

namespace Mips2C {
LinkedFunctionTable gLinkedFunctionTable;
std::unordered_map<std::string, std::vector<void (*)()>> gMips2CLinkCallbacks = {
    {"font", {draw_string::link}}};

void LinkedFunctionTable::reg(const std::string& name, u64 (*exec)(void*)) {
  if (!m_executes.insert({name, {exec}}).second) {
    lg::error("MIPS2C Function {} is registered multiple times, ignoring later registrations.",
              name);
    return;
  }

  // TODO: the actual code.
}
}  // namespace Mips2C