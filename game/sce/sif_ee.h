#pragma once

#include "common/common_types.h"

class IOP;

namespace ee {
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

#define sceMcExecIdle (-1)
#define sceMcExecRun 0
#define sceMcExecFinish 1

#define sceMcResSucceed 0
#define sceMcResNoFormat (-2)
#define sceMcResNoEntry (-4)
#define sceMcResDeniedPermit (-5)
#define sceMcResNotEmpty (-6)

#define sceMcFuncNoCardInfo 1
#define sceMcFuncNoOpen 2
#define sceMcFuncNoClose 3
#define sceMcFuncNoWrite 6
#define sceMcFuncNoFormat 16
#define sceMcFuncNoDelete 15
#define sceMcFuncNoUnformat 17

#define sceMcTypePS2 2

s32 sceOpen(const char* filename, s32 flag);
s32 sceClose(s32 fd);
s32 sceRead(s32 fd, void* buf, s32 nbyte);
s32 sceWrite(s32 fd, const void* buf, s32 nbyte);
s32 sceLseek(s32 fd, s32 offset, s32 where);

s32 sceMcMkdir(s32 port, s32 slot, const char* name);
s32 sceMcSync(s32 mode, s32* cmd, s32* result);
s32 sceMcOpen(s32 port, s32 slot, const char* name, s32 mode);
s32 sceMcWrite(s32 fd, const void* buff, s32 size);
s32 sceMcClose(s32 fd);

s32 sceMcGetInfo(s32 port, s32 slot, s32* type, s32* free, s32* format);
s32 sceMcFormat(s32 port, s32 slot);
s32 sceMcUnformat(s32 port, s32 slot);
s32 sceMcDelete(s32 port, s32 slot, const char* name);

}  // namespace ee
