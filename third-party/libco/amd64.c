#define LIBCO_C
#include "libco.h"
#include "settings.h"

#include <assert.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

static thread_local long long co_active_buffer[64];
static thread_local cothread_t co_active_handle = 0;
static void (*co_swap)(cothread_t, cothread_t) = 0;

#ifdef LIBCO_MPROTECT
  alignas(4096)
#else
  section(text)
#endif
#ifdef _WIN32
  /* ABI: Win64 */
  static const unsigned char co_swap_function[4096] = {
    0x48, 0x89, 0x22,              /* mov [rdx],rsp           */
    0x48, 0x8b, 0x21,              /* mov rsp,[rcx]           */
    0x58,                          /* pop rax                 */
    0x48, 0x83, 0xe9, 0x80,        /* sub rcx,-0x80           */
    0x48, 0x83, 0xea, 0x80,        /* sub rdx,-0x80           */
    0x48, 0x89, 0x6a, 0x88,        /* mov [rdx-0x78],rbp      */
    0x48, 0x89, 0x72, 0x90,        /* mov [rdx-0x70],rsi      */
    0x48, 0x89, 0x7a, 0x98,        /* mov [rdx-0x68],rdi      */
    0x48, 0x89, 0x5a, 0xa0,        /* mov [rdx-0x60],rbx      */
    0x4c, 0x89, 0x62, 0xa8,        /* mov [rdx-0x58],r12      */
    0x4c, 0x89, 0x6a, 0xb0,        /* mov [rdx-0x50],r13      */
    0x4c, 0x89, 0x72, 0xb8,        /* mov [rdx-0x48],r14      */
    0x4c, 0x89, 0x7a, 0xc0,        /* mov [rdx-0x40],r15      */
  #if !defined(LIBCO_NO_SSE)
    0x0f, 0x29, 0x72, 0xd0,        /* movaps [rdx-0x30],xmm6  */
    0x0f, 0x29, 0x7a, 0xe0,        /* movaps [rdx-0x20],xmm7  */
    0x44, 0x0f, 0x29, 0x42, 0xf0,  /* movaps [rdx-0x10],xmm8  */
    0x44, 0x0f, 0x29, 0x0a,        /* movaps [rdx],     xmm9  */
    0x44, 0x0f, 0x29, 0x52, 0x10,  /* movaps [rdx+0x10],xmm10 */
    0x44, 0x0f, 0x29, 0x5a, 0x20,  /* movaps [rdx+0x20],xmm11 */
    0x44, 0x0f, 0x29, 0x62, 0x30,  /* movaps [rdx+0x30],xmm12 */
    0x44, 0x0f, 0x29, 0x6a, 0x40,  /* movaps [rdx+0x40],xmm13 */
    0x44, 0x0f, 0x29, 0x72, 0x50,  /* movaps [rdx+0x50],xmm14 */
    0x44, 0x0f, 0x29, 0x7a, 0x60,  /* movaps [rdx+0x60],xmm15 */
  #endif
    0x48, 0x8b, 0x69, 0x88,        /* mov rbp,[rcx-0x78]      */
    0x48, 0x8b, 0x71, 0x90,        /* mov rsi,[rcx-0x70]      */
    0x48, 0x8b, 0x79, 0x98,        /* mov rdi,[rcx-0x68]      */
    0x48, 0x8b, 0x59, 0xa0,        /* mov rbx,[rcx-0x60]      */
    0x4c, 0x8b, 0x61, 0xa8,        /* mov r12,[rcx-0x58]      */
    0x4c, 0x8b, 0x69, 0xb0,        /* mov r13,[rcx-0x50]      */
    0x4c, 0x8b, 0x71, 0xb8,        /* mov r14,[rcx-0x48]      */
    0x4c, 0x8b, 0x79, 0xc0,        /* mov r15,[rcx-0x40]      */
  #if !defined(LIBCO_NO_SSE)
    0x0f, 0x28, 0x71, 0xd0,        /* movaps xmm6, [rcx-0x30] */
    0x0f, 0x28, 0x79, 0xe0,        /* movaps xmm7, [rcx-0x20] */
    0x44, 0x0f, 0x28, 0x41, 0xf0,  /* movaps xmm8, [rcx-0x10] */
    0x44, 0x0f, 0x28, 0x09,        /* movaps xmm9, [rcx]      */
    0x44, 0x0f, 0x28, 0x51, 0x10,  /* movaps xmm10,[rcx+0x10] */
    0x44, 0x0f, 0x28, 0x59, 0x20,  /* movaps xmm11,[rcx+0x20] */
    0x44, 0x0f, 0x28, 0x61, 0x30,  /* movaps xmm12,[rcx+0x30] */
    0x44, 0x0f, 0x28, 0x69, 0x40,  /* movaps xmm13,[rcx+0x40] */
    0x44, 0x0f, 0x28, 0x71, 0x50,  /* movaps xmm14,[rcx+0x50] */
    0x44, 0x0f, 0x28, 0x79, 0x60,  /* movaps xmm15,[rcx+0x60] */
  #endif
  #if !defined(LIBCO_NO_TIB)
    0x65, 0x4c, 0x8b, 0x04, 0x25,  /* mov r8,gs:0x30          */
    0x30, 0x00, 0x00, 0x00,
    0x41, 0x0f, 0x10, 0x40, 0x08,  /* movups xmm0,[r8+0x8]    */
    0x0f, 0x29, 0x42, 0x70,        /* movaps [rdx+0x70],xmm0  */
    0x0f, 0x28, 0x41, 0x70,        /* movaps xmm0,[rcx+0x70]  */
    0x41, 0x0f, 0x11, 0x40, 0x08,  /* movups [r8+0x8],xmm0    */
  #endif
    0xff, 0xe0,                    /* jmp rax                 */
  };

  #include <windows.h>

  static void co_init() {
    #ifdef LIBCO_MPROTECT
    DWORD old_privileges;
    VirtualProtect((void*)co_swap_function, sizeof co_swap_function, PAGE_EXECUTE_READ, &old_privileges);
    #endif
  }
#else
  /* ABI: SystemV */
  static const unsigned char co_swap_function[4096] = {
    0x48, 0x89, 0x26,        /* mov [rsi],rsp    */
    0x48, 0x8b, 0x27,        /* mov rsp,[rdi]    */
    0x58,                    /* pop rax          */
    0x48, 0x89, 0x6e, 0x08,  /* mov [rsi+ 8],rbp */
    0x48, 0x89, 0x5e, 0x10,  /* mov [rsi+16],rbx */
    0x4c, 0x89, 0x66, 0x18,  /* mov [rsi+24],r12 */
    0x4c, 0x89, 0x6e, 0x20,  /* mov [rsi+32],r13 */
    0x4c, 0x89, 0x76, 0x28,  /* mov [rsi+40],r14 */
    0x4c, 0x89, 0x7e, 0x30,  /* mov [rsi+48],r15 */
    0x48, 0x8b, 0x6f, 0x08,  /* mov rbp,[rdi+ 8] */
    0x48, 0x8b, 0x5f, 0x10,  /* mov rbx,[rdi+16] */
    0x4c, 0x8b, 0x67, 0x18,  /* mov r12,[rdi+24] */
    0x4c, 0x8b, 0x6f, 0x20,  /* mov r13,[rdi+32] */
    0x4c, 0x8b, 0x77, 0x28,  /* mov r14,[rdi+40] */
    0x4c, 0x8b, 0x7f, 0x30,  /* mov r15,[rdi+48] */
    0xff, 0xe0,              /* jmp rax          */
  };

  #ifdef LIBCO_MPROTECT
    #include <unistd.h>
    #include <sys/mman.h>
  #endif

  static void co_init() {
    #ifdef LIBCO_MPROTECT
    unsigned long long addr = (unsigned long long)co_swap_function;
    unsigned long long base = addr - (addr % sysconf(_SC_PAGESIZE));
    unsigned long long size = (addr - base) + sizeof co_swap_function;
    mprotect((void*)base, size, PROT_READ | PROT_EXEC);
    #endif
  }
#endif

static void co_entrypoint(cothread_t handle) {
  long long* buffer = (long long*)handle;
  #ifdef _WIN32
  buffer -= 16;
  #endif
  void (*entrypoint)(void) = (void (*)(void))buffer[1];
  entrypoint();
  abort();  /* called only if cothread_t entrypoint returns */
}

cothread_t co_active() {
  if(!co_active_handle) co_active_handle = &co_active_buffer;
  return co_active_handle;
}

cothread_t co_derive(void* memory, unsigned int size, void (*entrypoint)(void)) {
  cothread_t handle;
  if(!co_swap) {
    co_init();
    co_swap = (void (*)(cothread_t, cothread_t))co_swap_function;
  }
  if(!co_active_handle) co_active_handle = &co_active_buffer;

  if(handle = (cothread_t)memory) {
    unsigned int offset = (size & ~15) - 32;
    long long *p = (long long*)((char*)handle + offset);  /* seek to top of stack */
    *--p = (long long)0;                                  /* crash if entrypoint returns */
    *--p = (long long)co_entrypoint;
    ((long long*)handle)[0] = (long long)p;               /* stack pointer */
    ((long long*)handle)[1] = (long long)entrypoint;      /* start of function */
#if defined(_WIN32) && !defined(LIBCO_NO_TIB)
    ((long long*)handle)[30] = (long long)handle + size;  /* stack base */
    ((long long*)handle)[31] = (long long)handle;         /* stack limit */
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
  register cothread_t co_previous_handle = co_active_handle;
  co_swap(co_active_handle = handle, co_previous_handle);
}

int co_serializable() {
  return 1;
}

#ifdef __cplusplus
}
#endif
