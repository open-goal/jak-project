#pragma once

#include "common/common_types.h"

#include "game/kernel/common/Ptr.h"

s32 RpcCall(s32 rpcChannel,
            u32 fno,
            bool async,
            void* sendBuff,
            s32 sendSize,
            void* recvBuff,
            s32 recvSize);
void BeginLoadingDGO(const char* name, Ptr<u8> buffer1, Ptr<u8> buffer2, Ptr<u8> currentHeap);
Ptr<u8> GetNextDGO(u32* lastObjectFlag);
void ContinueLoadingDGO(Ptr<u8> heapPtr);
u64 RpcCall_wrapper(void* _args);
u32 RpcBusy(s32 channel);
void RpcSync(s32 channel);
void LoadDGOTest();
void kdgo_init_globals();
u32 InitRPC();
void StopIOP();

extern u32 sShowStallMsg;
