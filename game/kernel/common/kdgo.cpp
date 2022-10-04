#include "kdgo.h"

#include <cstring>

#include "common/log/log.h"

#include "game/common/dgo_rpc_types.h"
#include "game/common/loader_rpc_types.h"
#include "game/common/play_rpc_types.h"
#include "game/common/player_rpc_types.h"
#include "game/common/ramdisk_rpc_types.h"
#include "game/common/str_rpc_types.h"
#include "game/kernel/common/Ptr.h"
#include "game/kernel/common/kprint.h"
#include "game/sce/sif_ee.h"

ee::sceSifClientData cd[6];  //! client data for each IOP Remove Procedure Call.
u32 sShowStallMsg;           //! setting to show a "stalled on iop" message
u16 x[8];                    //! stupid temporary for storing a message
u32 sMsgNum;                 //! Toggle for double buffered message sending.
RPC_Dgo_Cmd* sLastMsg;       //! Last DGO command sent to IOP
RPC_Dgo_Cmd sMsg[2];         //! DGO message buffers

void kdgo_init_globals() {
  memset(x, 0, sizeof(x));
  memset(cd, 0, sizeof(cd));
  sShowStallMsg = 1;
  sLastMsg = nullptr;
  memset(sMsg, 0, sizeof(sMsg));
}

/*!
 * Call the given RPC with the given function number and buffers.
 */
s32 RpcCall(s32 rpcChannel,
            u32 fno,
            bool async,
            void* sendBuff,
            s32 sendSize,
            void* recvBuff,
            s32 recvSize) {
  return sceSifCallRpc(&cd[rpcChannel], fno, async, sendBuff, sendSize, recvBuff, recvSize, nullptr,
                       nullptr);
}

namespace {
struct GoalStackArgs {
  u64 args[8];
  template <typename T>
  T get_as(int i) {
    static_assert(sizeof(T) <= 8, "arg size");
    T result;
    memcpy(&result, args + i, sizeof(T));
    return result;
  }
};
}  // namespace

/*!
 * GOAL Wrapper for RpcCall.
 */
u64 RpcCall_wrapper(void* _args) {
  GoalStackArgs* args = (GoalStackArgs*)_args;
  auto rpcChannel = args->get_as<s32>(0);
  auto fno = args->get_as<u32>(1);
  auto async = args->get_as<u32>(2);
  auto send_buff = args->get_as<u64>(3);
  auto send_size = args->get_as<s32>(4);
  auto recv_buff = args->get_as<u64>(5);
  auto recv_size = args->get_as<s32>(6);
  return sceSifCallRpc(&cd[rpcChannel], fno, async, Ptr<u8>(send_buff).c(), send_size,
                       Ptr<u8>(recv_buff).c(), recv_size, nullptr, nullptr);
}

/*!
 * Check if the given RPC is busy, by channel.
 */
u32 RpcBusy(s32 channel) {
  return sceSifCheckStatRpc(&cd[channel].rpcd);
}

/*!
 * Wait for an RPC to not be busy. Prints a stall message if sShowStallMsg is true and we have
 * to wait on the IOP.  Stalling here is bad because it means the rest of the game can't run.
 */
void RpcSync(s32 channel) {
  if (RpcBusy(channel)) {
    if (sShowStallMsg) {
      Msg(6, "STALL: [kernel] waiting for IOP on RPC port #%d\n", channel);
    }
    while (RpcBusy(channel)) {
      // an attempt to avoid spamming SIF?
      u32 i = 0;
      while (i < 1000) {
        i++;
      }
    }
  }
}

/*!
 * Setup an RPC.
 */
u32 RpcBind(s32 channel, s32 id) {
  while (true) {
    if (sceSifBindRpc(&cd[channel], id, 1) < 0) {
      MsgErr("Error: RpcBind failed on port #%d [%4.4X]\n", channel, id);
      return 1;
    }
    Msg(6, "kernel: RPC port #%d started [%4.4X]\n", channel, id);
    //    FlushCache(0);
    // In Jak 2 they do a sceSifCheckStatRpc, but we can just skip that.

    // this was not optimized out in Jak 1, but is _almost_ optimized out in Jak 2 and later.
    u32 i = 0;
    while (i < 10000) {
      i++;
    }

    if (cd[channel].serve) {
      break;
    }
    Msg(6, "kernel: RPC port #%d not responding.\n", channel);
    // it might seem like looping here is a bad idea (unclear if sceSifBindRpc can be called
    // multiple times!) but this actually happens sometimes, at least on development hardware!
    // (also, it's not clear that the "serve" field having data in it really means anything - maybe
    // the sceSifBindRpc doesn't wait for the connection to be fully set up?  This seems likely
    // because they had to put that little delay in there before checking.)
  }
  return 0;
}

/*!
 * Setup all RPCs
 */
u32 InitRPC() {
  if (!RpcBind(PLAYER_RPC_CHANNEL, PLAYER_RPC_ID[g_game_version]) &&
      !RpcBind(LOADER_RPC_CHANNEL, LOADER_RPC_ID[g_game_version]) &&
      !RpcBind(RAMDISK_RPC_CHANNEL, RAMDISK_RPC_ID[g_game_version]) &&
      !RpcBind(DGO_RPC_CHANNEL, DGO_RPC_ID[g_game_version]) &&
      !RpcBind(STR_RPC_CHANNEL, STR_RPC_ID[g_game_version]) &&
      !RpcBind(PLAY_RPC_CHANNEL, PLAY_RPC_ID[g_game_version])) {
    return 0;
  }
  printf("Entering endless loop ... please wait\n");
  for (;;) {
  }
}

/*!
 * Send a message to the IOP to stop it.
 */
void StopIOP() {
  x[2] = 0x14;  // todo - this type and message
  //  RpcSync(PLAYER_RPC_CHANNEL);
  //  RpcCall(PLAYER_RPC_CHANNEL, 0, false, x, 0x50, nullptr, 0);
  printf("IOP shut down\n");
  //  sceDmaSync(0x10009000, 0, 0);
  printf("DMA shut down\n");
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
  sMsg[msgID].result = DGO_RPC_RESULT_INIT;  // !! this is 666

  // inform IOP of buffers
  sMsg[msgID].buffer1 = buffer1.offset;
  sMsg[msgID].buffer2 = buffer2.offset;

  // also give a heap pointer so it can load the last object file directly into the heap to save the
  // precious time.
  sMsg[msgID].buffer_heap_top = currentHeap.offset;

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
    if ((sLastMsg->result == DGO_RPC_RESULT_MORE) || (sLastMsg->result == DGO_RPC_RESULT_DONE)) {
      buffer.offset =
          sLastMsg->buffer1;  // buffer 1 always contains location of most recently loaded object.
    }

    // not the last one, so don't set the flag.
    if (sLastMsg->result == DGO_RPC_RESULT_MORE) {
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
 * Load the TEST.DGO file.
 * Presumably used for debugging DGO loads.
 * We don't have the TEST.DGO file, so this isn't very useful.
 *
 * DONE,
 * EXACT,
 * UNUSED
 */
void LoadDGOTest() {
  u32 lastObject = 0;

  // backup show stall message and set it to false
  // EE will be loading DGO in a loop, so it will always be stalling
  // no need to print it.
  u32 lastShowStall = sShowStallMsg;
  sShowStallMsg = 0;

  // pick somewhat arbitrary memory to load the DGO into
  BeginLoadingDGO("TEST.DGO", Ptr<u8>(0x4800000), Ptr<u8>(0x4c00000), Ptr<u8>(0x4000000));
  while (true) {
    // keep trying to load.
    Ptr<u8> dest_buffer(0);
    do {
      dest_buffer = GetNextDGO(&lastObject);
    } while (!dest_buffer.offset);

    // print the name of the object we loaded, its destination, and its size.
    Msg(6, "Loaded %s at %8.8X length %d\n", (dest_buffer + 4).cast<char>().c(), dest_buffer.offset,
        *(dest_buffer.cast<u32>()));
    if (lastObject) {
      break;
    }

    // okay to load the next one
    ASSERT(false);  // this is different per version, annoyingly. This function is unused though,
    // so let's be lazy for now...
    // ContinueLoadingDGO(Ptr<u8>(0x4000000));
  }

  sShowStallMsg = lastShowStall;
}