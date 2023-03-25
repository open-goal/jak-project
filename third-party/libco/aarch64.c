#define LIBCO_C
#include "libco.h"
#include "settings.h"

#include <assert.h>
#include <stdlib.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

static thread_local uintptr_t co_active_buffer[64];
static thread_local cothread_t co_active_handle = 0;
static void (*co_swap)(cothread_t, cothread_t) = 0;

#ifdef LIBCO_MPROTECT
  alignas(4096)
#else
  section(text)
#endif
static const uint32_t co_swap_function[1024] = {
  0x910003f0,  /* mov x16,sp           */
  0xa9007830,  /* stp x16,x30,[x1]     */
  0xa9407810,  /* ldp x16,x30,[x0]     */
  0x9100021f,  /* mov sp,x16           */
  0xa9015033,  /* stp x19,x20,[x1, 16] */
  0xa9415013,  /* ldp x19,x20,[x0, 16] */
  0xa9025835,  /* stp x21,x22,[x1, 32] */
  0xa9425815,  /* ldp x21,x22,[x0, 32] */
  0xa9036037,  /* stp x23,x24,[x1, 48] */
  0xa9436017,  /* ldp x23,x24,[x0, 48] */
  0xa9046839,  /* stp x25,x26,[x1, 64] */
  0xa9446819,  /* ldp x25,x26,[x0, 64] */
  0xa905703b,  /* stp x27,x28,[x1, 80] */
  0xa945701b,  /* ldp x27,x28,[x0, 80] */
  0xf900303d,  /* str x29,    [x1, 96] */
  0xf940301d,  /* ldr x29,    [x0, 96] */
  0x6d072428,  /* stp d8, d9, [x1,112] */
  0x6d472408,  /* ldp d8, d9, [x0,112] */
  0x6d082c2a,  /* stp d10,d11,[x1,128] */
  0x6d482c0a,  /* ldp d10,d11,[x0,128] */
  0x6d09342c,  /* stp d12,d13,[x1,144] */
  0x6d49340c,  /* ldp d12,d13,[x0,144] */
  0x6d0a3c2e,  /* stp d14,d15,[x1,160] */
  0x6d4a3c0e,  /* ldp d14,d15,[x0,160] */
#if defined(_WIN32) && !defined(LIBCO_NO_TIB)
  0xa940c650,  /* ldp x16,x17,[x18, 8] */
  0xa90b4430,  /* stp x16,x17,[x1,176] */
  0xa94b4410,  /* ldp x16,x17,[x0,176] */
  0xa900c650,  /* stp x16,x17,[x18, 8] */
#endif
  0xd61f03c0,  /* br x30               */
};

#ifdef _WIN32
  #include <windows.h>

  static void co_init() {
    #ifdef LIBCO_MPROTECT
    DWORD old_privileges;
    VirtualProtect((void*)co_swap_function, sizeof co_swap_function, PAGE_EXECUTE_READ, &old_privileges);
    #endif
  }
#else
#ifdef LIBCO_MPROTECT
  #include <unistd.h>
  #include <sys/mman.h>
#endif

static void co_init() {
  #ifdef LIBCO_MPROTECT
  uintptr_t addr = (uintptr_t)co_swap_function;
  uintptr_t base = addr - (addr % sysconf(_SC_PAGESIZE));
  uintptr_t size = (addr - base) + sizeof co_swap_function;
  mprotect((void*)base, size, PROT_READ | PROT_EXEC);
  #endif
}
#endif

static void co_entrypoint(cothread_t handle) {
  uintptr_t* buffer = (uintptr_t*)handle;
  void (*entrypoint)(void) = (void (*)(void))buffer[2];
  entrypoint();
  abort();  /* called only if cothread_t entrypoint returns */
}

cothread_t co_active() {
  if(!co_active_handle) co_active_handle = &co_active_buffer;
  return co_active_handle;
}

cothread_t co_derive(void* memory, unsigned int size, void (*entrypoint)(void)) {
  uintptr_t* handle;
  if(!co_swap) {
    co_init();
    co_swap = (void (*)(cothread_t, cothread_t))co_swap_function;
  }
  if(!co_active_handle) co_active_handle = &co_active_buffer;

  if(handle = (uintptr_t*)memory) {
    unsigned int offset = (size & ~15);
    uintptr_t* p = (uintptr_t*)((unsigned char*)handle + offset);
    handle[0]  = (uintptr_t)p;              /* x16 (stack pointer) */
    handle[1]  = (uintptr_t)co_entrypoint;  /* x30 (link register) */
    handle[2]  = (uintptr_t)entrypoint;     /* x19 (entry point) */
    handle[12] = (uintptr_t)p;              /* x29 (frame pointer) */
#if defined(_WIN32) && !defined(LIBCO_NO_TIB)
    handle[22] = (uintptr_t)handle + size;  /* stack base */
    handle[23] = (uintptr_t)handle;         /* stack limit */
#endif
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
