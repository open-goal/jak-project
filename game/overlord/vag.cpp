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

u32 CalculateVAGPitch(u32 a1, s32 a2) {
  if (a2 == 0) {
    return a1;
  }

  if (a2 <= 0) {
    return 0x5f4 * a1 / (0x5f4 - a2);
  } else {
    return a1 * (a2 + 0x5f4) / 0x5f4;
  }
}
