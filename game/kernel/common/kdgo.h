#pragma once

#include "common/common_types.h"

#include "game/common/dgo_rpc_types.h"
#include "game/kernel/common/Ptr.h"

extern u32 sMsgNum;
s32 RpcCall(s32 rpcChannel,
            u32 fno,
            bool async,
            void* sendBuff,
            s32 sendSize,
            void* recvBuff,
            s32 recvSize);
u64 RpcCall_wrapper(void* _args);
u32 RpcBusy(s32 channel);
void RpcSync(s32 channel);
void LoadDGOTest();
void kdgo_init_globals();
u32 InitRPC();
void StopIOP();

extern u32 sShowStallMsg;
