#pragma once

#include "common/common_types.h"

namespace jakx {
void jakx_overlord_init_globals_stream();

u32 PLAYThread();
u32 STRThread();
void* RPC_STR(unsigned int fno, void* msg, int size);
void* RPC_PLAY(unsigned int fno, void* msg, int size);

}  // namespace jakx