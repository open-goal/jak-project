#include "kdgo.h"

#include "common/global_profiler/GlobalProfiler.h"
#include "common/link_types.h"
#include "common/log/log.h"
#include "common/util/BitUtils.h"
#include "common/util/FileUtil.h"
#include "common/util/Timer.h"

#include "game/kernel/common/Ptr.h"
#include "game/kernel/common/fileio.h"
#include "game/kernel/common/kdgo.h"
#include "game/kernel/common/kmalloc.h"
#include "game/kernel/jak2/klink.h"

namespace jak2 {

/*!
 * Instruct the IOP to continue loading the next object.
 * Only should be called once it is safe to overwrite the previous.
 * @param heapPtr : pointer to heap so the IOP could try to load directly into a heap if it wants.
 * This should be updated after each object file load to make sure the IOP knows the exact location
 * of the end of the GOAL heap data.
 *
 * Unlike jak 1, we update buffer1 and buffer2 here for borrow heap loads.
 */
void ContinueLoadingDGO(Ptr<u8> b1, Ptr<u8> b2, Ptr<u8> heapPtr) {
  u32 msgID = sMsgNum;
  RPC_Dgo_Cmd* sendBuff = sMsg + sMsgNum;
  sMsgNum = sMsgNum ^ 1;
  sendBuff->result = DGO_RPC_RESULT_INIT;
  sMsg[msgID].buffer1 = b1.offset;
  sMsg[msgID].buffer2 = b2.offset;
  sMsg[msgID].buffer_heap_top = heapPtr.offset;
  // the IOP will wait for this RpcCall to continue the DGO state machine.
  RpcCall(DGO_RPC_CHANNEL, DGO_RPC_LOAD_NEXT_FNO, true, sendBuff, sizeof(RPC_Dgo_Cmd), sendBuff,
          sizeof(RPC_Dgo_Cmd));
  // this async RPC call will complete when the next object is fully loaded.
  sLastMsg = sendBuff;
}

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
 * Faster version of load_and_link_dgo_from_c that skips the IOP and reads the file directly
 * to GOAL memory.
 */
void load_and_link_dgo_from_c_fast(const char* name,
                                   Ptr<kheapinfo> heap,
                                   u32 linkFlag,
                                   s32 bufferSize) {
  Timer timer;
  lg::debug("[Load and Link DGO From C (fast)] {}", name);

  // append CGO if needed
  char name_on_cd[16];
  kstrcpyup(name_on_cd, name);
  if (name_on_cd[strlen(name_on_cd) - 4] != '.') {
    strcat(name_on_cd, ".CGO");
  }

  // open the DGO file:
  auto file_path = file_util::get_jak_project_dir() / "out" / game_version_names[g_game_version] /
                   "iso" / name_on_cd;
  auto fp = fopen(file_path.string().c_str(), "rb");
  if (!fp) {
    lg::die("Failed to open DGO: {}, path {}\n", name, file_path.string());
  }

  // allocate temporary buffers for linking:
  auto old_heap_top = heap->top;
  auto buffer1 = kmalloc(heap, bufferSize, KMALLOC_TOP | KMALLOC_ALIGN_64, "dgo-buffer-1");

  // read the header
  DgoHeader header;
  if (fread(&header, sizeof(DgoHeader), 1, fp) != 1) {
    lg::die("failed to read dgo header");
  }
  lg::info("got {} objects, name {}\n", header.object_count, header.name);

  // load all but the final
  for (int i = 0; i < (int)header.object_count - 1; i++) {
    if (fread(buffer1.c(), sizeof(ObjectHeader), 1, fp) != 1) {
      lg::die("failed to read object header");
    }
    auto* obj_header = (ObjectHeader*)buffer1.c();
    u32 aligned_size = align16(obj_header->size);
    auto* obj_dest = buffer1.c() + sizeof(ObjectHeader);
    if (fread(obj_dest, aligned_size, 1, fp) != 1) {
      lg::die("Failed to read object data");
    }
    link_and_exec(buffer1 + sizeof(ObjectHeader), obj_header->name, obj_header->size, heap,
                  linkFlag, true);
  }

  auto final_object_dest = Ptr<u8>((heap->current + 0x3f).offset & 0xffffffc0);
  if (fread(final_object_dest.c(), sizeof(ObjectHeader), 1, fp) != 1) {
    lg::die("failed to read final object header");
  }
  auto* obj_header = (ObjectHeader*)final_object_dest.c();
  u32 aligned_size = align16(obj_header->size);
  auto* obj_dest = (final_object_dest + sizeof(ObjectHeader)).c();
  if (fread(obj_dest, aligned_size, 1, fp) != 1) {
    lg::die("Failed to read object data");
  }
  link_and_exec(final_object_dest + sizeof(ObjectHeader), obj_header->name, obj_header->size, heap,
                linkFlag, true);

  heap->top = old_heap_top;
  fclose(fp);
  lg::info("load_and_link_dgo_from_c_fast took {:.3f} s\n", timer.getSeconds());
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
  Timer timer;
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
    {
      auto p = scoped_prof(fmt::format("link-{}", objName).c_str());
      link_and_exec(obj, objName, objSize, heap, linkFlag, jump_from_c_to_goal);  // link now!
    }

    // inform IOP we are done
    if (!lastObjectLoaded) {
      ContinueLoadingDGO(buffer1, buffer2, Ptr<u8>((heap->current + 0x3f).offset & 0xffffffc0));
    }
  }
  lg::info("load_and_link_dgo_from_c took {:.3f} s\n", timer.getSeconds());
  sShowStallMsg = oldShowStall;
}

}  // namespace jak2