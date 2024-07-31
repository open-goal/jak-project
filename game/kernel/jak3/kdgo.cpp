#include "kdgo.h"

#include "common/global_profiler/GlobalProfiler.h"
#include "common/log/log.h"
#include "common/util/Timer.h"

#include "game/kernel/common/fileio.h"
#include "game/kernel/common/kdgo.h"
#include "game/kernel/common/kmalloc.h"
#include "game/kernel/jak3/klink.h"
#include "game/overlord/jak3/rpc_interface.h"

namespace jak3 {

jak3::RPC_Dgo_Cmd* sLastMsg;  //! Last DGO command sent to IOP
jak3::RPC_Dgo_Cmd sMsg[2];    //! DGO message buffers
uint16_t cgo_id = 10;

void kdgo_init_globals() {
  sLastMsg = nullptr;
  memset(sMsg, 0, sizeof(sMsg));
  cgo_id = 10;
}

/*!
 * Send message to IOP to start loading a new DGO file
 * Uses a double-buffered message buffer
 * @param name: the name of the DGO file
 * @param buffer1 : one of the two file loading buffers
 * @param buffer2 : the other of the two file loading buffers
 * @param currentHeap : the current heap (for loading directly into the heap).
 *
 * DONE,
 * MODIFIED : Added print statement to indicate when DGO load starts.
 */
void BeginLoadingDGO(const char* name, Ptr<u8> buffer1, Ptr<u8> buffer2, Ptr<u8> currentHeap) {
  u8 msgID = sMsgNum;
  RPC_Dgo_Cmd* mess = sMsg + sMsgNum;
  sMsgNum = sMsgNum ^ 1;     // toggle message buffer.
  RpcSync(DGO_RPC_CHANNEL);  // make sure old RPC is finished

  // put a dummy value here just to make sure the IOP overwrites it.
  sMsg[msgID].status = DGO_RPC_RESULT_INIT;  // !! this is 666

  // inform IOP of buffers
  sMsg[msgID].buffer1 = buffer1.offset;
  sMsg[msgID].buffer2 = buffer2.offset;

  // also give a heap pointer so it can load the last object file directly into the heap to save the
  // precious time.
  sMsg[msgID].buffer_heap_top = currentHeap.offset;

  // new for Jak 3: a unique ID.
  sMsg[msgID].cgo_id = cgo_id;
  cgo_id++;

  // file name
  strcpy(sMsg[msgID].name, name);
  lg::debug("[Begin Loading DGO RPC] {}, 0x{:x}, 0x{:x}, 0x{:x}", name, buffer1.offset,
            buffer2.offset, currentHeap.offset);
  // this RPC will return once we have loaded the first object file.
  // but we call async, so we don't block here.
  RpcCall(DGO_RPC_CHANNEL, DGO_RPC_LOAD_FNO, true, mess, sizeof(RPC_Dgo_Cmd), mess,
          sizeof(RPC_Dgo_Cmd));
  sLastMsg = mess;
}

/*!
 * Get the next object in the DGO.  Will block until something is loaded.
 * @param lastObjectFlag: will get set to 1 if this is the last object.
 *
 * DONE,
 * MODIFIED : added exception if the sLastMessage isn't set (game just returns null as buffer)
 */
Ptr<u8> GetNextDGO(u32* lastObjectFlag) {
  *lastObjectFlag = 1;
  // Wait for RPC function to respond. This will happen once the first object file is loaded.
  RpcSync(DGO_RPC_CHANNEL);
  Ptr<u8> buffer(0);
  if (sLastMsg) {
    // if we got a good result, get pointer to object
    if ((sLastMsg->status == DGO_RPC_RESULT_MORE) || (sLastMsg->status == DGO_RPC_RESULT_DONE)) {
      buffer.offset =
          sLastMsg->buffer1;  // buffer 1 always contains location of most recently loaded object.
    }

    // not the last one, so don't set the flag.
    if (sLastMsg->status == DGO_RPC_RESULT_MORE) {
      *lastObjectFlag = 0;
    }

    // no pending message.
    sLastMsg = nullptr;
  } else {
    // I don't see how this case can happen unless there's a bug. The game does check for this and
    // nothing in this case. (maybe from GOAL this can happen?)
    printf("last message not set!\n");
  }
  return buffer;
}

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
  jak3::RPC_Dgo_Cmd* sendBuff = sMsg + sMsgNum;
  sMsgNum = sMsgNum ^ 1;
  sendBuff->status = DGO_RPC_RESULT_INIT;
  sMsg[msgID].buffer1 = b1.offset;
  sMsg[msgID].buffer2 = b2.offset;
  sMsg[msgID].buffer_heap_top = heapPtr.offset;
  // the IOP will wait for this RpcCall to continue the DGO state machine.
  RpcCall(DGO_RPC_CHANNEL, DGO_RPC_LOAD_NEXT_FNO, true, sendBuff, sizeof(jak3::RPC_Dgo_Cmd),
          sendBuff, sizeof(jak3::RPC_Dgo_Cmd));
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

}  // namespace jak3
