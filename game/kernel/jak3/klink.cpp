#include "klink.h"

#include "common/common_types.h"
#include "common/symbols.h"

#include "game/kernel/common/klink.h"

namespace jak3 {

Ptr<uint8_t> link_and_exec(Ptr<uint8_t> data,
                           const char* name,
                           int32_t size,
                           Ptr<kheapinfo> heap,
                           uint32_t flags,
                           bool jump_from_c_to_goal) {
  ASSERT_NOT_REACHED();
}

u64 link_and_exec_wrapper(u64* args) {
  ASSERT_NOT_REACHED();
}

u32 link_busy() {
  ASSERT_NOT_REACHED();
}
void link_reset() {
  ASSERT_NOT_REACHED();
}
uint64_t link_begin(u64* args) {
  ASSERT_NOT_REACHED();
}
uint64_t link_resume() {
  ASSERT_NOT_REACHED();
}

// Note: update_goal_fns changed to skip the hashtable lookup since symlink2/symlink3 are now fixed
// symbols.

/*!
 * The ULTIMATE MEMORY COPY
 * IT IS VERY FAST
 * but it may use the scratchpad.  It is implemented in GOAL, and falls back to normal C memcpy
 * if GOAL isn't loaded, or if the alignment isn't good enough.
 */
void ultimate_memcpy(void* dst, void* src, uint32_t size) {
  // only possible if alignment is good.
  if (!(u64(dst) & 0xf) && !(u64(src) & 0xf) && !(u64(size) & 0xf) && size > 0xfff) {
    if (!gfunc_774.offset) {
      // GOAL function is unknown, lets see if its loaded:
      auto sym_val = *((s7 + jak3_symbols::FIX_SYM_ULTIMATE_MEMCPY - 1).cast<u32>());
      if (sym_val == 0) {
        memmove(dst, src, size);
        return;
      }
      gfunc_774.offset = sym_val;
    }

    Ptr<u8>(call_goal(gfunc_774, make_u8_ptr(dst).offset, make_u8_ptr(src).offset, size, s7.offset,
                      g_ee_main_mem))
        .c();
  } else {
    memmove(dst, src, size);
  }
}

}  // namespace jak3