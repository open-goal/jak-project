#include "kmalloc.h"

namespace jak3 {
// these functions are all stubs in all known copies of the ELF.
void kmemopen_from_c(Ptr<kheapinfo> heap, const char* name) {
  (void)heap;
  (void)name;
}

void kmemopen(u32 heap, u32 name) {
  (void)heap;
  (void)name;
}

void kmemclose() {}
}  // namespace jak3
