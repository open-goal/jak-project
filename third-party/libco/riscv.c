#define LIBCO_C
#include "libco.h"
#include "settings.h"

#include <stdlib.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

static thread_local uint64_t co_active_buffer[64];
static thread_local cothread_t co_active_handle = 0;

#if __riscv_xlen == 32
#define I_STORE "sw"
#define I_LOAD  "lw"
#elif __riscv_xlen == 64
#define I_STORE "sd"
#define I_LOAD  "ld"
#else
#error Unsupported RISC-V XLEN
#endif

#if !defined(__riscv_flen)
#define F_STORE "#"
#define F_LOAD  "#"
#elif __riscv_flen == 32
#define F_STORE "fsw"
#define F_LOAD  "flw"
#elif __riscv_flen == 64
#define F_STORE "fsd"
#define F_LOAD  "fld"
#else
#error Unsupported RISC-V FLEN
#endif

__attribute__((naked))
static void co_swap(cothread_t active, cothread_t previous) {
__asm__(
  I_STORE " ra,   0 *8(a1)\n"
  I_STORE " sp,   1 *8(a1)\n"
  I_STORE " s0,   2 *8(a1)\n"
  I_STORE " s1,   3 *8(a1)\n"
  I_STORE " s2,   4 *8(a1)\n"
  I_STORE " s3,   5 *8(a1)\n"
  I_STORE " s4,   6 *8(a1)\n"
  I_STORE " s5,   7 *8(a1)\n"
  I_STORE " s6,   8 *8(a1)\n"
  I_STORE " s7,   9 *8(a1)\n"
  I_STORE " s8,   10*8(a1)\n"
  I_STORE " s9,   11*8(a1)\n"
  I_STORE " s10,  12*8(a1)\n"
  I_STORE " s11,  13*8(a1)\n"

  F_STORE " fs0,  14*8(a1)\n"
  F_STORE " fs1,  15*8(a1)\n"
  F_STORE " fs2,  16*8(a1)\n"
  F_STORE " fs3,  17*8(a1)\n"
  F_STORE " fs4,  18*8(a1)\n"
  F_STORE " fs5,  19*8(a1)\n"
  F_STORE " fs6,  20*8(a1)\n"
  F_STORE " fs7,  21*8(a1)\n"
  F_STORE " fs8,  22*8(a1)\n"
  F_STORE " fs9,  23*8(a1)\n"
  F_STORE " fs10, 24*8(a1)\n"
  F_STORE " fs11, 25*8(a1)\n"

  I_LOAD  " ra,   0 *8(a0)\n"
  I_LOAD  " sp,   1 *8(a0)\n"
  I_LOAD  " s0,   2 *8(a0)\n"
  I_LOAD  " s1,   3 *8(a0)\n"
  I_LOAD  " s2,   4 *8(a0)\n"
  I_LOAD  " s3,   5 *8(a0)\n"
  I_LOAD  " s4,   6 *8(a0)\n"
  I_LOAD  " s5,   7 *8(a0)\n"
  I_LOAD  " s6,   8 *8(a0)\n"
  I_LOAD  " s7,   9 *8(a0)\n"
  I_LOAD  " s8,   10*8(a0)\n"
  I_LOAD  " s9,   11*8(a0)\n"
  I_LOAD  " s10,  12*8(a0)\n"
  I_LOAD  " s11,  13*8(a0)\n"

  F_LOAD  " fs0,  14*8(a0)\n"
  F_LOAD  " fs1,  15*8(a0)\n"
  F_LOAD  " fs2,  16*8(a0)\n"
  F_LOAD  " fs3,  17*8(a0)\n"
  F_LOAD  " fs4,  18*8(a0)\n"
  F_LOAD  " fs5,  19*8(a0)\n"
  F_LOAD  " fs6,  20*8(a0)\n"
  F_LOAD  " fs7,  21*8(a0)\n"
  F_LOAD  " fs8,  22*8(a0)\n"
  F_LOAD  " fs9,  23*8(a0)\n"
  F_LOAD  " fs10, 24*8(a0)\n"
  F_LOAD  " fs11, 25*8(a0)\n"

  "ret\n"
);
}

static void co_entrypoint(cothread_t handle) {
  uint64_t* buffer = (uint64_t*)handle;
  void (*entrypoint)(void) = (void (*)(void))(uintptr_t)buffer[3];
  entrypoint();
  abort();  /* called only if cothread_t entrypoint returns */
}

cothread_t co_active() {
  if(!co_active_handle) co_active_handle = &co_active_buffer;
  return co_active_handle;
}

cothread_t co_derive(void* memory, unsigned int size, void (*entrypoint)(void)) {
  uint64_t* handle;
  if(!co_active_handle) co_active_handle = &co_active_buffer;

  if(handle = (uint64_t*)memory) {
    unsigned int offset = (size & ~15);
    uint64_t* p = (uint64_t*)((uint8_t*)handle + offset);
    *(uintptr_t*)&handle[0] = (uintptr_t)co_entrypoint;  /* ra (return address) */
    *(uintptr_t*)&handle[1] = (uintptr_t)p;              /* sp (stack pointer) */
    *(uintptr_t*)&handle[2] = (uintptr_t)p;              /* s0 (frame pointer) */
    *(uintptr_t*)&handle[3] = (uintptr_t)entrypoint;     /* s1 (entry point) */
  }

  return handle;
}

cothread_t co_create(unsigned int size, void (*entrypoint)(void)) {
  void* memory = malloc(size);
  if(!memory) return (cothread_t)0;
  return co_derive(memory, size, entrypoint);
}

void co_delete(cothread_t handle) {
  free(handle);
}

void co_switch(cothread_t handle) {
  cothread_t co_previous_handle = co_active_handle;
  co_swap(co_active_handle = handle, co_previous_handle);
}

int co_serializable() {
  return 1;
}

#ifdef __cplusplus
}
#endif
