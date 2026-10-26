#include "kdgo.h"

#include <cstring>

#include "common/log/log.h"
#include "common/versions/versions.h"

#include "game/common/dgo_rpc_types.h"
#include "game/common/loader_rpc_types.h"
#include "game/common/play_rpc_types.h"
#include "game/common/player_rpc_types.h"
#include "game/common/ramdisk_rpc_types.h"
#include "game/common/str_rpc_types.h"
#include "game/kernel/common/Ptr.h"
#include "game/kernel/common/kprint.h"
#include "game/runtime.h"
#include "game/sce/sif_ee.h"

ee::sceSifClientData cd[6];  //! client data for each IOP Remove Procedure Call.
u32 sShowStallMsg;           //! setting to show a "stalled on iop" message
u16 x[8];                    //! stupid temporary for storing a message
u32 sMsgNum;                 //! Toggle for double buffered message sending.
bool IOP_RUNNING_W;
bool RPC_Initialized_G;

typedef void EndFunctionType(int idk1, int idk2);

const s32 NUMBER_OF_RPC_CHANNELS = 7;
struct RpcCallEndFunctionArg_W {
  int sema_id;
  void* callback;
  int third;
  int fourth;
};
RpcCallEndFunctionArg_W RpcCallEndFunctionArgs_W[NUMBER_OF_RPC_CHANNELS];

const s32 NUMBER_OF_RPC_CHANNEL_PAIRS = 6;  // FIXME: Why is this 6 and not 7?
struct RpcChannelIdPair_W {
  int channel;
  int id;
};
RpcChannelIdPair_W RpcChannels_W[NUMBER_OF_RPC_CHANNEL_PAIRS];

void kdgo_init_globals() {
  memset(x, 0, sizeof(x));
  memset(cd, 0, sizeof(cd));
  sShowStallMsg = 1;
}

int RpcCallEndFunction_W(RpcCallEndFunctionArg_W* arg) {
  if (arg != nullptr) {
    // int* args = (int*)arg;
    if (arg->callback)
      ((EndFunctionType*)arg->callback)(arg->third, arg->fourth);
    // FIXME: What to do with this?
    // return iSignalSema(arg->sema_id);
    return 0;
  } else {
    // int in_v0_lo; My guess is that this code was meant to be unreachable
    // return in_v0_lo;
    return -1;
  }
}

/*!
 * Call the given RPC with the given function number and buffers.
 * In Jak X, errors are written out. The conditions have been negated.
 */
s32 RpcCall(s32 rpcChannel,
            u32 fno,
            bool async,
            void* sendBuff,
            s32 sendSize,
            void* recvBuff,
            s32 recvSize,
            void* callback) {
  if (g_game_version != GameVersion::JakX) {
    return sceSifCallRpc(&cd[rpcChannel], fno, async, sendBuff, sendSize, recvBuff, recvSize,
                         nullptr, nullptr);
  }

  if (rpcChannel >= NUMBER_OF_RPC_CHANNELS) {
    MsgErr("dkernel: RpcCall() error; invalid port id %d\n", rpcChannel);
  } else if (sendSize >= 0xffff1) {
    MsgErr("dkernel: RpcCall() error; invalid send/receive sizes (ssize=%d rsize=%d)\n",
           (s32)sendSize, recvSize);
  } else if (!((((u32)sendSize < 0xffff1) && (-1 < recvSize)) && (recvSize < 0xffff1))) {
    MsgErr("dkernel: RpcCall() error; NULL send buffer (secv=0x%08x ssize=%d)\n", nullptr,
           sendSize);
  } else if (!(recvSize < 1) || (recvBuff != nullptr)) {
    MsgErr("dkernel: RpcCall() error; NULL receive buffer (recv=0x%08x rsize=%d)\n", nullptr,
           recvSize);
  } else if ((uintptr_t)sendBuff & 0xf) {
    MsgErr("dkernel: RpcCall() error; misaligned send buffer (send=0x%08x ssize=%d)\n", sendBuff,
           sendSize);  // added missing parenthesis
  } else if ((uintptr_t)recvBuff & 0xf) {
    MsgErr("dkernel: RpcCall() error; misaligned receive buffer (recv=0x%08x rsize=%d\n", recvBuff,
           recvSize);
  } else {
    // FIXME: What to do with this?
    // WaitSema(RpcCallEndFunctionArgs_W[rpcChannel].sema_id);
    RpcCallEndFunctionArgs_W[rpcChannel].callback = callback;
    RpcCallEndFunctionArgs_W[rpcChannel].fourth = 0;  // in_stack_00000008;
    RpcCallEndFunctionArgs_W[rpcChannel].third = 0;   // in_stack_00000000;
    return sceSifCallRpc(&cd[rpcChannel], fno, (int)async, sendBuff, sendSize, recvBuff, recvSize,
                         (void*)RpcCallEndFunction_W, &RpcCallEndFunctionArgs_W[rpcChannel]);
  }
  return -1;
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
  // FIXME: Pass to RpcCall?
  return sceSifCallRpc(&cd[rpcChannel], fno, async, Ptr<u8>(send_buff).c(), send_size,
                       Ptr<u8>(recv_buff).c(), recv_size, nullptr, nullptr);
}

/*!
 * Check if the given RPC is busy, by channel.
 */
u32 RpcBusy(s32 channel) {
  if (channel < NUMBER_OF_RPC_CHANNELS) {
    return sceSifCheckStatRpc(&cd[channel].rpcd);
  } else {
    return 1;
  }
}

/*!
 * Wait for an RPC to not be busy. Prints a stall message if sShowStallMsg is true and we have
 * to wait on the IOP.  Stalling here is bad because it means the rest of the game can't run.
 */
void RpcSync(s32 channel) {
  if (6 < channel) {
    MsgErr("dkernel: RpcSync() error; invalid port id %d\n", channel);
  }
  if (RpcBusy(channel)) {
    if (sShowStallMsg) {
      Msg(6, "dkernel: RpcSync() warning; port #%d stalled; waiting...\n", channel);
    }
    // FIXME: What to do with this?
    // WaitSema(RpcCallEndFunctionArgs_W[channel].sema_id);
    // FIXME: What to do with this?
    // SignalSema(RpcCallEndFunctionArgs_W[channel].sema_id);
    while (RpcBusy(channel)) {
      // FIXME: What to do with this?
      // DelayThread(10000);
    }
    if (sShowStallMsg) {
      Msg(6, "dkernel: RpcSync(); port #%d acquired\n", channel);
    }
  }
}

/*!
 * Setup an RPC for Jak X
 * Remains to be seen if the two ways to call rpc bind can be merged.
 */
s32 RpcBindJakX(s32 channel, s32 id) {
  if (channel < NUMBER_OF_RPC_CHANNELS) {
    bool displayedWarning = false;
    while (true) {
      if (sceSifBindRpc(&cd[channel], id, 0) < 0) {
        MsgErr("dkernel: RpcBind() error; bind failed on port #%d id 0x%08x\n", channel, id);
        return -1;
      } else if (cd[channel].serve) {
        MsgErr("dkernel: RpcBind() port #%d id 0x%08x bound\n", channel, id);
        // In Jak 2 they do a sceSifCheckStatRpc, but we can just skip that.
        return 0;
      } else if (!displayedWarning) {
        displayedWarning = true;
        MsgErr("dkernel: RpcBind() warning; port #%d id 0x%08x not responding; retrying...\n",
               channel, id);
      }
      // FIXME: What to do with this?
      // DelayThread(10000);
      // it might seem like looping here is a bad idea (unclear if sceSifBindRpc can be called
      // multiple times!) but this actually happens sometimes, at least on development hardware!
      // (also, it's not clear that the "serve" field having data in it really means anything -
      // maybe the sceSifBindRpc doesn't wait for the connection to be fully set up?  This seems
      // likely because they had to put that little delay in there before checking.)
    }
  } else {
    MsgErr("dkernel: RpcBind() error; invalid port id %d\n", channel);
    return -1;
  }
}

s32 RpcBind(s32 channel, s32 id) {
  if (g_game_version == GameVersion::JakX) {
    return RpcBindJakX(channel, id);
  }

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
s32 InitRPC() {
  if (g_game_version == GameVersion::JakX) {
    if (RPC_Initialized_G) {
      MsgErr("dkernel: InitRPC() error; multiple initializations attempted");
      return -1;
    } else {
      for (int i = 0; i < NUMBER_OF_RPC_CHANNELS; i++) {
        RpcCallEndFunctionArgs_W[i].fourth = 0;
        RpcCallEndFunctionArgs_W[i].sema_id = -1;
        RpcCallEndFunctionArgs_W[i].callback = nullptr;
        RpcCallEndFunctionArgs_W[i].third = 0;
      }
      for (int i = 0; i < NUMBER_OF_RPC_CHANNEL_PAIRS; i++) {
        if (!RpcBind(RpcChannels_W[i].channel, RpcChannels_W[i].id)) {
          return -1;
        }
      }
      for (int i = 0; i < NUMBER_OF_RPC_CHANNELS; i++) {
        // FIXME: What to do with this?
        // ee_sema_t someSema;
        // memset(&someSema, 0, 0x18); // MACRO
        // someSema.init_count = 1;
        // someSema.max_count = 1;
        // int semaId = CreateSema(&someSema);
        // RpcCallEndFunctionArgs_W[i].sema_id = semaId;
        // if (semaId < 0) {
        //   for (int j = i; j > 0; --j) {
        //     DeleteSema(RpcCallEndFunctionArgs_W[j].sema_id);
        //     RpcCallEndFunctionArgs_W[j].sema_id = -1;
        //   }
        //   return -1;
        // }
      }
      RPC_Initialized_G = true;
    }
  } else {
    RpcBind(PLAYER_RPC_CHANNEL, PLAYER_RPC_ID[g_game_version]);
    RpcBind(LOADER_RPC_CHANNEL, LOADER_RPC_ID[g_game_version]);
    RpcBind(RAMDISK_RPC_CHANNEL, RAMDISK_RPC_ID[g_game_version]);
    RpcBind(DGO_RPC_CHANNEL, DGO_RPC_ID[g_game_version]);
    RpcBind(STR_RPC_CHANNEL, STR_RPC_ID[g_game_version]);
    RpcBind(PLAY_RPC_CHANNEL, PLAY_RPC_ID[g_game_version]);
  }
  return 0;
}

/*!
 * Send a message to the IOP to stop it.
 */
int StopIOPJakX() {
  if (Is_RPC_Initialized_G() && IOP_RUNNING_W) {
    // x[2] = 0x10;
    // x[3] = 0;  // todo - this type and message
    //   RpcSync(1); // FIXME: PLAYER_RPC_CHANNEL at 1? Previously at 0.
    IOP_RUNNING_W = false;
    // return RpcCall(1, 0, false, x, 0x30, nullptr, 0);
    lg::print("IOP shut down\n");
  }
  return 0;
}

/*!
 * Send a message to the IOP to stop it.
 */
int StopIOP() {
  if (g_game_version == GameVersion::JakX) {
    return StopIOPJakX();
  }

  // 0x19 not 0x14 for Jak 3
  // x[2] = 0x19;  // todo - this type and message
  //   RpcSync(PLAYER_RPC_CHANNEL);
  //   RpcCall(PLAYER_RPC_CHANNEL, 0, false, x, 0x50, nullptr, 0);
  lg::print("IOP shut down\n");
  //  sceDmaSync(0x10009000, 0, 0);
  lg::print("DMA shut down\n");
  return 0;
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
  ASSERT_NOT_REACHED();  // no longer supported.
  /*
  u32 lastObject = 0;

  // backup show stall message and set it to false
  // EE will be loading DGO in a loop, so it will always be stalling
  // no need to print it.
  bool lastShowStall = setStallMsg_G(false);

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
    // ContinueLoadingDGO(Ptr<u8>(0x4800000));
  }

  setStallMsg_G(lastShowStall);
  */
}

bool setStallMsg_GW(bool show) {
  bool oldShowStallMsg;

  oldShowStallMsg = sShowStallMsg;
  sShowStallMsg = show;
  return oldShowStallMsg;
}

bool Is_RPC_Initialized_G(void) {
  return RPC_Initialized_G;
}
