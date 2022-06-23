#pragma once

/*!
 * @file kmalloc.h
 * GOAL Kernel memory allocator.
 * Simple two-sided bump allocator.
 * DONE
 */

#ifndef JAK_KMALLOC_H
#define JAK_KMALLOC_H

#include "Ptr.h"
#include "kmachine.h"

#include "common/common_types.h"

/*!
 * A kheap has a top/bottom linear allocator
 */
struct kheapinfo {
  Ptr<u8> base;      //! beginning of heap
  Ptr<u8> top;       //! current location of bottom of top allocations
  Ptr<u8> current;   //! current location of top of bottom allocations
  Ptr<u8> top_base;  //! end of heap
};

// Kernel heaps
extern Ptr<kheapinfo> kglobalheap;
extern Ptr<kheapinfo> kdebugheap;

// flags for kmalloc/ksmalloc
constexpr u32 KMALLOC_TOP = 0x2000;     //! Flag to allocate temporary memory from heap top
constexpr u32 KMALLOC_MEMSET = 0x1000;  //! Flag to clear memory
constexpr u32 KMALLOC_ALIGN_256 = 0x100;
constexpr u32 KMALLOC_ALIGN_64 = 0x40;
constexpr u32 KMALLOC_ALIGN_16 = 0x10;

// kmalloc funcions
Ptr<u8> ksmalloc(Ptr<kheapinfo> heap, s32 size, u32 flags, char const* name);
void kheapstatus(Ptr<kheapinfo> heap);
Ptr<kheapinfo> kinitheap(Ptr<kheapinfo> heap, Ptr<u8> mem, s32 size);
u32 kheapused(Ptr<kheapinfo> heap);
Ptr<u8> kmalloc(Ptr<kheapinfo> heap, s32 size, u32 flags, char const* name);
void kfree(Ptr<u8> a);

void kmalloc_init_globals();

#endif  // JAK_KMALLOC_H
