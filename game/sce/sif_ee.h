#ifndef JAK1_SIF_EE_H
#define JAK1_SIF_EE_H

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

}  // namespace ee
#endif  // JAK1_SIF_EE_H
