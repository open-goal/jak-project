#pragma once

#ifndef JAK1_STUBS_H
#define JAK1_STUBS_H

#include "common/common_types.h"

#define SCE_PAD_DMA_BUFFER_SIZE 0x100

namespace ee {
void sceGsSyncV();
void sceGsSyncPath();
void sceGsResetPath();
void sceGsResetGraph();
void sceDmaSync();
void sceGsPutIMR();
void sceGsGetIMR();
void sceGsExecStoreImage();
void FlushCache();
int scePadPortOpen(int port, int slot, void* data);
}  // namespace ee

namespace iop {
u32 snd_BankLoadByLoc(u32 sector, u32 unk);
u32 snd_GetLastLoadError();
void snd_ResolveBankXREFS();
}  // namespace iop

#endif  // JAK1_STUBS_H
