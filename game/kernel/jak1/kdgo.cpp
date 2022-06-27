#include "kdgo.h"

#include "common/common_types.h"
#include "common/log/log.h"

#include "game/kernel/common/Ptr.h"
#include "game/kernel/common/fileio.h"
#include "game/kernel/common/kdgo.h"
#include "game/kernel/common/kmalloc.h"
#include "game/kernel/jak1/klink.h"

namespace jak1 {
/*!
 * Load and link a DGO file.
 * This does not use the mutli-threaded linker and will block until the entire file is done.
 */
void load_and_link_dgo(u64 name_gstr, u64 heap_info, u64 flag, u64 buffer_size) {
  auto name = Ptr<char>(name_gstr + 4).c();
  auto heap = Ptr<kheapinfo>(heap_info);
  load_and_link_dgo_from_c(name, heap, flag, buffer_size, false);
}

/*!
 * Load and link a DGO file.
 * This does not use the mutli-threaded linker and will block until the entire file is done.e
 */
void load_and_link_dgo_from_c(const char* name,
                              Ptr<kheapinfo> heap,
                              u32 linkFlag,
                              s32 bufferSize,
                              bool jump_from_c_to_goal) {
  lg::debug("[Load and Link DGO From C] {}", name);
  u32 oldShowStall = sShowStallMsg;

  // remember where the heap top point is so we can clear temporary allocations
  auto oldHeapTop = heap->top;

  // allocate temporary buffers from top of the given heap
  // align 64 for IOP DMA
  // note: both buffers named dgo-buffer-2
  auto buffer2 = kmalloc(heap, bufferSize, KMALLOC_TOP | KMALLOC_ALIGN_64, "dgo-buffer-2");
  auto buffer1 = kmalloc(heap, bufferSize, KMALLOC_TOP | KMALLOC_ALIGN_64, "dgo-buffer-2");

  // build filename.  If no extension is given, default to CGO.
  char fileName[16];
  kstrcpyup(fileName, name);
  if (fileName[strlen(fileName) - 4] != '.') {
    strcat(fileName, ".CGO");
  }

  // no stall messages, as this is a blocking load and when spending 100% CPU time on linking,
  // the linker can beat the DVD drive.
  sShowStallMsg = 0;

  // start load on IOP.
  BeginLoadingDGO(
      fileName, buffer1, buffer2,
      Ptr<u8>((heap->current + 0x3f).offset & 0xffffffc0));  // 64-byte aligned for IOP DMA

  u32 lastObjectLoaded = 0;
  while (!lastObjectLoaded) {
    // check to see if next object is loaded (I believe it always is?)
    auto dgoObj = GetNextDGO(&lastObjectLoaded);
    if (!dgoObj.offset) {
      continue;
    }

    // if we're on the last object, it is loaded at cheap->current.  So we can safely reset the two
    // dgo-buffer allocations. We do this _before_ we link! This way, the last file loaded has more
    // heap available, which is important when we need to use the entire memory.
    if (lastObjectLoaded) {
      heap->top = oldHeapTop;
    }

    // determine the size and name of the object we got
    auto obj = dgoObj + 0x40;             // seek past dgo object header
    u32 objSize = *(dgoObj.cast<u32>());  // size from object's link block

    char objName[64];
    strcpy(objName, (dgoObj + 4).cast<char>().c());  // name from dgo object header
    lg::debug("[link and exec] {:18s} {} {:6d} heap-use {:8d} {:8d}: 0x{:x}", objName,
              lastObjectLoaded, objSize, kheapused(kglobalheap),
              kdebugheap.offset ? kheapused(kdebugheap) : 0, kglobalheap->current.offset);
    link_and_exec(obj, objName, objSize, heap, linkFlag, jump_from_c_to_goal);  // link now!

    // inform IOP we are done
    if (!lastObjectLoaded) {
      ContinueLoadingDGO(Ptr<u8>((heap->current + 0x3f).offset & 0xffffffc0));
    }
  }
  sShowStallMsg = oldShowStall;
}
}  // namespace jak1