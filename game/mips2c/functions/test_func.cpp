//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/kscheme.h"
namespace Mips2C {
namespace test_func {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  u64 result = 0;

  for (int i = a0; i < t4; i++) {
    result += c->sgpr64(i);
  }

  c->gprs[v0].du64[0] = result;
  return 0;
}
}  // namespace test_func
}  // namespace Mips2C