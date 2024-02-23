#pragma once

/*!
 * @file Ptr.h
 * Representation of a GOAL pointer which can be converted to/from a C pointer.
 */

#include "common/common_types.h"
#include "common/util/Assert.h"

#include "game/runtime.h"

/*!
 * GOAL pointer to a T.  Represented as a 32-bit unsigned offset from g_ee_main_mem.
 * A NULL pointer has an offset of 0.
 *
 * This doesn't have to be very efficient, as this implementation is only used in the C Kernel.
 * The GOAL implementation is much more efficient.
 *
 * Consider putting size checks on these?
 */
template <typename T>
struct Ptr {
  u32 offset;

  /*!
   * Default pointer is NULL.
   */
  Ptr() { offset = 0; }

  /*!
   * Pointer from manual offset.
   */
  explicit Ptr(u32 v) { offset = v; }

  /*!
   * Dereference a pointer. Will error if you do this on a null pointer.
   */
  T* operator->() {
    ASSERT(offset);
    return (T*)(g_ee_main_mem + offset);
  }

  /*!
   * Dereference a pointer. Will error if you do this on a null pointer.
   */
  T& operator*() {
    ASSERT(offset);
    return *(T*)(g_ee_main_mem + offset);
  }

  // pointer math
  Ptr operator+(s32 diff) { return Ptr(offset + diff); }
  s32 operator-(Ptr<T> x) { return offset - x.offset; }
  Ptr operator-(s32 diff) { return Ptr(offset - diff); }
  bool operator==(const Ptr<T>& x) const { return offset == x.offset; }
  bool operator!=(const Ptr<T>& x) const { return offset != x.offset; }

  /*!
   * Convert to a C pointer.
   */
  T* c() {
    if (!offset) {
      return nullptr;
    }
    return (T*)(g_ee_main_mem + offset);
  }

  template <typename T2>
  Ptr<T2> cast() {
    return Ptr<T2>(offset);
  }
};

template <typename T>
Ptr<T> make_ptr(T* x) {
  if (!x) {
    return Ptr<T>(0);
  }
  return Ptr<T>((u8*)x - g_ee_main_mem);
}

template <typename T>
Ptr<u8> make_u8_ptr(T* x) {
  if (!x) {
    return Ptr<u8>(0);
  }
  return Ptr<u8>((u8*)x - g_ee_main_mem);
}
