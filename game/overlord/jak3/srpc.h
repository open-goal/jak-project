#pragma once

#include "common/common_types.h"

namespace jak3 {
void jak3_overlord_init_globals_srpc();
u32 Thread_Player();
u32 Thread_Loader();
struct ISO_VAGCommand;
void SetVagStreamName(ISO_VAGCommand* cmd, int len);
void* RPC_Player(unsigned int fno, void* msg, int size);
void* RPC_Loader(unsigned int fno, void* msg, int size);
extern const char* g_pszLanguage;
extern u8 g_nFPS;

}  // namespace jak3