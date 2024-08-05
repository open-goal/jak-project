#pragma once
#include "common/common_types.h"

#include "game/common/dgo_rpc_types.h"
#include "game/kernel/common/Ptr.h"
#include "game/kernel/common/kmalloc.h"

namespace jak2 {
void load_and_link_dgo_from_c(const char* name,
                              Ptr<kheapinfo> heap,
                              u32 linkFlag,
                              s32 bufferSize,
                              bool jump_from_c_to_goal);
void load_and_link_dgo(u64 name_gstr, u64 heap_info, u64 flag, u64 buffer_size);
void load_and_link_dgo_from_c_fast(const char* name,
                                   Ptr<kheapinfo> heap,
                                   u32 linkFlag,
                                   s32 bufferSize);
void kdgo_init_globals();
extern RPC_Dgo_Cmd sMsg[2];
extern RPC_Dgo_Cmd* sLastMsg;
}  // namespace jak2