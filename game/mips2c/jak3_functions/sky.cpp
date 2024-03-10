
#include "sky.h"

#include "game/kernel/jak3/kscheme.h"
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak3 {

ExecutionContext sky_regs_vfs;

namespace set_sky_vf27 {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  // sky_regs_vfs.vfs[27]
  memcpy(&sky_regs_vfs.vfs[27].f[0], g_ee_main_mem + c->gpr_addr(a0), 16);
  return 0;
}

void link() {
  gLinkedFunctionTable.reg("set-sky-vf27", execute, 64);
}

}  // namespace set_sky_vf27
}  // namespace Mips2C::jak3

namespace Mips2C::jak3 {
namespace set_sky_vf23_value {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  // sky_regs_vfs.vfs[27]
  u64 value = c->sgpr64(a0);
  memcpy(&sky_regs_vfs.vfs[23].f[0], &value, 8);
  return 0;
}

void link() {
  gLinkedFunctionTable.reg("set-sky-vf23-value", execute, 64);
}

}  // namespace set_sky_vf23_value
}  // namespace Mips2C::jak3

namespace Mips2C::jak3 {
namespace clip_polygon_against_positive_hyperplane {
u64 execute(void*) {
  ASSERT_NOT_REACHED();
}
}  // namespace clip_polygon_against_positive_hyperplane
namespace clip_polygon_against_negative_hyperplane {
u64 execute(void*) {
  ASSERT_NOT_REACHED();
}
}  // namespace clip_polygon_against_negative_hyperplane
}  // namespace Mips2C::jak3