#ifndef JAK1_STUBS_H
#define JAK1_STUBS_H

#include "common/common_types.h"

#ifndef SCE_SEEK_SET
#define SCE_SEEK_SET        (0)
#endif
#ifndef SCE_SEEK_CUR
#define SCE_SEEK_CUR        (1)
#endif
#ifndef SCE_SEEK_END
#define SCE_SEEK_END        (2)
#endif

#define SCE_RDONLY      0x0001
#define SCE_WRONLY      0x0002
#define SCE_RDWR        0x0003
#define SCE_NBLOCK      0x0010
#define SCE_APPEND      0x0100
#define SCE_CREAT       0x0200
#define SCE_TRUNC       0x0400
#define SCE_EXCL        0x0800
#define SCE_NOBUF       0x4000
#define SCE_NOWAIT      0x8000

#define SCE_PAD_DMA_BUFFER_SIZE 0x100

namespace ee {
s32 sceOpen(const char *filename, s32 flag);
s32 sceClose(s32 fd);
s32 sceRead(s32 fd, void *buf, s32 nbyte);
s32 sceWrite(s32 fd, const void *buf, s32 nbyte);
s32 sceLseek(s32 fd, s32 offset, s32 where);
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
}

namespace iop {
u32 snd_BankLoadByLoc(u32 sector, u32 unk);
u32 snd_GetLastLoadError();
void snd_ResolveBankXREFS();
}


#endif  // JAK1_STUBS_H
