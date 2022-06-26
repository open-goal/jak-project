#include "kmalloc.h"

/*
 * Jak 2 adds a few "memopen" functions. I think these were used to categorize memory allocations.
 * GOAL and C++ code will call "memopen" with an appropriate category name before making
 * allocations.
 *
 * These functions do nothing in the release version (and all known versions), but they might be
 * interested for us to track memory usage.
 */

namespace jak2 {
void kmemopen_from_c(Ptr<kheapinfo> heap, const char* name) {
  (void)heap;
  (void)name;
}

void kmemopen(u32 heap, u32 name) {
  (void)heap;
  (void)name;
}

void kmemclose() {}
}  // namespace jak2
