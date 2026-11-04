#pragma once

#include "common/common_types.h"

#include "game/common/dgo_rpc_types.h"
#include "game/kernel/common/Ptr.h"

extern u32 sMsgNum;

extern bool IOP_RUNNING_W;

extern u32 sShowStallMsg;

s32 RpcCall(s32 rpcChannel,
            u32 fno,
            bool async,
            void* sendBuff,
            s32 sendSize,
            void* recvBuff,
            s32 recvSize,
            void* callback = nullptr);
u64 RpcCall_wrapper(void* _args);
u32 RpcBusy(s32 channel);
void RpcSync(s32 channel);
void LoadDGOTest();
void kdgo_init_globals();
s32 InitRPC();
int StopIOP();

bool setStallMsg_GW(bool show);
bool Is_RPC_Initialized_G();
