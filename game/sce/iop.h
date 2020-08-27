#ifndef JAK1_IOP_H
#define JAK1_IOP_H

#include "common/common_types.h"

#define SMEM_Low (0)
#define SMEM_High (1)
#define SMEM_Addr (2)

#define SCECdCD 1
#define SCECdDVD 2

#define SCECdIllgalMedia 0xff
#define SCECdIllegalMedia 0xff
#define SCECdDVDV 0xfe
#define SCECdCDDA 0xfd
#define SCECdPS2DVD 0x14
#define SCECdPS2CD 0x12
#define SCECdDETCT 0x01

#define SCECdComplete 0x02
#define SCECdNotReady 0x06
#define KE_MBOX_NOMSG -424

#define TH_C 0x02000000

class IOP;

namespace iop {
typedef void* (*sceSifRpcFunc)(unsigned int, void*, int);

struct sceSifServeData {
  unsigned int command;  // the RPC ID
  sceSifRpcFunc func;
  void* buff;
};

struct sceSifQueueData {
  int key = -1;
  sceSifServeData* serve_data = nullptr;
};

struct sceCdRMode {
  uint8_t trycount;
  uint8_t spindlctrl;
  uint8_t datapattern;
  uint8_t pad;
};

struct sceSifDmaData {
  void* data;
  void* addr;
  unsigned int size;
  unsigned int mode;
};

struct SysClock {
  uint32_t hi, lo;
};

struct MsgPacket {
  u32 dummy = 0;
};

struct MbxParam {
  u32 attr;
  u32 option;
};

struct ThreadParam {
  u32 attr;
  u32 option;
  void* entry;
  int stackSize;
  int initPriority;

  // added!
  char name[64];
};

struct SemaParam {
  uint32_t attr;
  int32_t init_count;
  int32_t max_count;
  uint32_t option;
};

// void PS2_RegisterIOP(IOP *iop);
int QueryTotalFreeMemSize();
void* AllocSysMemory(int type, unsigned long size, void* addr);

int GetThreadId();
void CpuDisableIntr();
void CpuEnableIntr();
void SleepThread();
void DelayThread(u32 usec);
s32 CreateThread(ThreadParam* param);
s32 StartThread(s32 thid, u32 arg);
s32 WakeupThread(s32 thid);

void sceSifInitRpc(int mode);
void sceSifInitRpc(unsigned int mode);
void sceSifSetRpcQueue(sceSifQueueData* dq, int key);
void sceSifRegisterRpc(sceSifServeData* serve,
                       unsigned int request,
                       sceSifRpcFunc func,
                       void* buff,
                       sceSifRpcFunc cfunc,
                       void* cbuff,
                       sceSifQueueData* qd);
void sceSifRpcLoop(sceSifQueueData* pd);

int sceCdRead(uint32_t logical_sector, uint32_t sectors, void* buf, sceCdRMode* mode);
int sceCdSync(int mode);
int sceCdGetError();
int sceCdGetDiskType();
int sceCdMmode(int media);
int sceCdBreak();
int sceCdDiskReady(int mode);

u32 sceSifSetDma(sceSifDmaData* sdd, int len);

s32 SendMbx(int mbxid, void* sendmsg);
s32 PollMbx(MsgPacket** recvmsg, int mbxid);
s32 CreateMbx(MbxParam* param);

void GetSystemTime(SysClock* time);

s32 CreateSema(SemaParam* param);
s32 WaitSema(s32 sema);
s32 SignalSema(s32 sema);

void FlushDcache();

u32 sceSifCheckInit();
void sceSifInit();

void LIBRARY_INIT();
void LIBRARY_register(::IOP* i);
void LIBRARY_kill();
}  // namespace iop

#endif  // JAK1_IOP_H
