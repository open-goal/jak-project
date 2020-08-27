#include "registers.h"

namespace goal {
bool is_gpr(u8 reg) {
  return reg <= R15;
}

u8 get_nth_xmm(u8 id) {
  return id + XMM0;
}

bool is_xmm(u8 reg) {
  return reg >= XMM0 && reg <= XMM15;
}

u8 xmm_to_id(u8 reg) {
  return reg - 16;
}

}  // namespace goal