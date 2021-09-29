#include "mips2c_table.h"
#include "common/log/log.h"

#include "game/kernel/kmalloc.h"
#include "game/kernel/kscheme.h"
#include "common/symbols.h"

extern "C" {
void _mips2c_call_linux();
void _mips2c_call_windows();
}

namespace Mips2C {

namespace draw_string {
extern void link();
}

namespace sp_init_fields {
extern void link();
}

namespace particle_adgif {
extern void link();
}

namespace sp_launch_particles_var {
extern void link();
}

namespace sp_process_block_3d {
extern void link();
}

namespace sp_process_block_2d {
extern void link();
}

LinkedFunctionTable gLinkedFunctionTable;
Rng gRng;
std::unordered_map<std::string, std::vector<void (*)()>> gMips2CLinkCallbacks = {
    {"font", {draw_string::link}},
    {"sparticle-launcher",
     {sp_init_fields::link, particle_adgif::link, sp_launch_particles_var::link}},
    {"sparticle", {sp_process_block_3d::link, sp_process_block_2d::link}}};

void LinkedFunctionTable::reg(const std::string& name, u64 (*exec)(void*), u32 stack_size) {
  const auto& it = m_executes.insert({name, {exec, Ptr<u8>()}});
  if (!it.second) {
    lg::error("MIPS2C Function {} is registered multiple times, ignoring later registrations.",
              name);
    return;
  }

  // this is short stub that will jump to the appropriate function.
  auto jump_to_asm = Ptr<u8>(alloc_heap_object(s7.offset + FIX_SYM_GLOBAL_HEAP,
                                               *(s7 + FIX_SYM_FUNCTION_TYPE), 0x40, UNKNOWN_PP));
  it.first->second.goal_trampoline = jump_to_asm;

  u8* ptr = jump_to_asm.c();

  {
    // linux

    // push the function
    u64 addr = (u64)exec;
    *ptr = 0x48;
    ptr++;
    *ptr = 0xb8;
    ptr++;
    memcpy(ptr, &addr, 8);
    ptr += 8;
    *ptr = 0x50;
    ptr++;

    // push the stack size
    addr = stack_size;
    *ptr = 0x48;
    ptr++;
    *ptr = 0xb8;
    ptr++;
    memcpy(ptr, &addr, 8);
    ptr += 8;
    *ptr = 0x50;
    ptr++;

    // call the other function
#ifdef __linux__
    addr = (u64)_mips2c_call_linux;
#elif _WIN32
    addr = (u64)_mips2c_call_windows;
#endif

    *ptr = 0x48;
    ptr++;
    *ptr = 0xb8;
    ptr++;
    memcpy(ptr, &addr, 8);
    ptr += 8;

    // jumps to the mips2c call, which will return to the caller of this stub.
    *ptr = 0xff;
    ptr++;
    *ptr = 0xe0;
  }
}

u32 LinkedFunctionTable::get(const std::string& name) {
  auto it = m_executes.find(name);
  if (it == m_executes.end()) {
    assert(false);
  }
  return it->second.goal_trampoline.offset;
}
}  // namespace Mips2C