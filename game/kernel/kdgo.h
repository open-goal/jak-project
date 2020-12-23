#pragma once

/*!
 * @file kdgo.h
 * Loading DGO Files.  Also has some general SIF RPC stuff used for RPCs other than DGO loading.
 * DONE!
 */

#ifndef JAK_V2_KDGO_H
#define JAK_V2_KDGO_H

#include "common/common_types.h"
#include "Ptr.h"
#include "kmalloc.h"

void kdgo_init_globals();
u32 InitRPC();
void load_and_link_dgo_from_c(const char* name, Ptr<kheapinfo> heap, u32 linkFlag, s32 bufferSize);
void load_and_link_dgo(u64 name_gstr, u64 heap_info, u64 flag, u64 buffer_size);
void StopIOP();

u64 RpcCall_wrapper(void* args);
u32 RpcBusy(s32 channel);
void LoadDGOTest();
#endif  // JAK_V2_KDGO_H
