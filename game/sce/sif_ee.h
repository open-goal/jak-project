#pragma once

#include "common/common_types.h"

class IOP;

namespace ee {

#ifndef SCE_SEEK_SET
#define SCE_SEEK_SET (0)
#endif
#ifndef SCE_SEEK_CUR
#define SCE_SEEK_CUR (1)
#endif
#ifndef SCE_SEEK_END
#define SCE_SEEK_END (2)
#endif

#define SCE_RDONLY 0x0001
#define SCE_WRONLY 0x0002
#define SCE_RDWR 0x0003
#define SCE_NBLOCK 0x0010
#define SCE_APPEND 0x0100
#define SCE_CREAT 0x0200
#define SCE_TRUNC 0x0400
#define SCE_EXCL 0x0800
#define SCE_NOBUF 0x4000
#define SCE_NOWAIT 0x8000
struct sceSifRpcData {
  u8 dummy;
  u32 id;
};

struct sceSifServeData {
  u8 dummy;
};

struct sceSifClientData {
  sceSifRpcData rpcd;
  //  unsigned int  command;
  void* buff;
  void* gp;
  //  sceSifEndFunc func;
  void* para;
  //  struct _sif_serve_data    *serve;
  sceSifServeData* serve;
};

void LIBRARY_sceSif_register(::IOP* i);
void LIBRARY_INIT_sceSif();

void sceSifInitRpc(unsigned int mode);
int sceSifRebootIop(const char* imgfile);
int sceSifSyncIop();
void sceFsReset();
int sceSifLoadModule(const char* name, int arg_size, const char* args);
int sceMcInit();
s32 sceSifCallRpc(sceSifClientData* bd,
                  u32 fno,
                  u32 mode,
                  void* send,
                  s32 ssize,
                  void* recv,
                  s32 rsize,
                  void* end_func,
                  void* end_para);
s32 sceSifCheckStatRpc(sceSifRpcData* bd);
s32 sceSifBindRpc(sceSifClientData* bd, u32 request, u32 mode);

s32 sceOpen(const char* filename, s32 flag);
s32 sceClose(s32 fd);
s32 sceRead(s32 fd, void* buf, s32 nbyte);
s32 sceWrite(s32 fd, const void* buf, s32 nbyte);
s32 sceLseek(s32 fd, s32 offset, s32 where);
}  // namespace ee
