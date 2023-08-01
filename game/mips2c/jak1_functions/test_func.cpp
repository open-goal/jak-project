//--------------------------MIPS2C---------------------
#include "game/kernel/jak1/kscheme.h"
#include "game/mips2c/mips2c_private.h"
using namespace jak1;
namespace Mips2C::jak1 {
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
}  // namespace Mips2C::jak1

//--------------------------MIPS2C---------------------

#include "game/mips2c/mips2c_private.h"
namespace Mips2C::jak1 {
namespace goal_call_test {
struct Cache {
  void* goal_check_function;
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -128);  // daddiu sp, sp, -128
  c->sd(ra, 0, sp);         // sd ra, 0(sp)
  c->sq(s0, 16, sp);        // sq s0, 16(sp)
  c->sq(s1, 32, sp);        // sq s1, 32(sp)
  c->sq(s2, 48, sp);        // sq s2, 48(sp)
  c->sq(s3, 64, sp);        // sq s3, 64(sp)
  c->sq(s4, 80, sp);        // sq s4, 80(sp)
  c->sq(s5, 96, sp);        // sq s5, 96(sp)
  c->sq(gp, 112, sp);       // sq gp, 112(sp)

  c->load_symbol(t9, cache.goal_check_function);
  call_addr = c->gprs[t9].du32[0];  // function call:
  c->sll(v0, ra, 0);                // sll v0, ra, 0
  c->jalr(call_addr);               // jalr ra, t9

  c->ld(ra, 0, sp);        // ld ra, 0(sp)
  c->lq(gp, 112, sp);      // lq gp, 112(sp)
  c->lq(s5, 96, sp);       // lq s5, 96(sp)
  c->lq(s4, 80, sp);       // lq s4, 80(sp)
  c->lq(s3, 64, sp);       // lq s3, 64(sp)
  c->lq(s2, 48, sp);       // lq s2, 48(sp)
  c->lq(s1, 32, sp);       // lq s1, 32(sp)
  c->lq(s0, 16, sp);       // lq s0, 16(sp)
  c->daddiu(sp, sp, 128);  // daddiu sp, sp, 128
  goto end_of_function;    // return

end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.goal_check_function = intern_from_c("goal_check_function").c();
}

}  // namespace goal_call_test
}  // namespace Mips2C::jak1
