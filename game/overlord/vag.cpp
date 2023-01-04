#include "vag.h"

#include <array>
#include <cstring>

std::array<VagCommand2, 4> gVagCmds;

VagCommand2* FindThisVagStream(const char* name, u32 id) {
  for (auto& cmd : gVagCmds) {
    if (!strcmp(cmd.name, name) && cmd.id == id) {
      return &cmd;
    }
  }

  return nullptr;
}
