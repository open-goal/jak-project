#pragma once

#include "common/common_types.h"

namespace ee {
void sceGsPutIMR();
void sceGsGetIMR();
void sceGsExecStoreImage();
void FlushCache();
}  // namespace ee

namespace iop {
u32 snd_BankLoadByLoc(u32 sector, u32 unk);
u32 snd_GetLastLoadError();
void snd_ResolveBankXREFS();
}  // namespace iop
