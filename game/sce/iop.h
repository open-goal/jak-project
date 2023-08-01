#pragma once

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
#define KE_OK 0
#define KE_SEMA_ZERO (-419)
#define KE_SEMA_OVF -420
#define KE_MBOX_NOMSG -424
#define KE_WAIT_DELETE -425

#define TH_C 0x02000000

#define SA_THFIFO 0
#define SA_THPRI 1

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
  MsgPacket* next = nullptr;
  u8 priority;
  u8 dummy[3];
};

struct MbxParam {
  u32 attr;
  u32 option;
};

struct ThreadParam {
  u32 attr;
  u32 option;
  u32 (*entry)();
  int stackSize;
  int initPriority;

  // added!
  char name[64];
};

struct SemaParam {
  uint32_t attr;
  uint32_t option;
  int32_t init_count;
  int32_t max_count;
};

// void PS2_RegisterIOP(IOP *iop);
int QueryTotalFreeMemSize();
void* AllocSysMemory(int type, unsigned long size, void* addr);
void* AllocScratchPad(int mode);

int GetThreadId();
void CpuDisableIntr();
void CpuEnableIntr();
void SleepThread();
void DelayThread(u32 usec);
s32 CreateThread(ThreadParam* param);
s32 ExitThread();
s32 StartThread(s32 thid, u32 arg);
s32 WakeupThread(s32 thid);
s32 iWakeupThread(s32 thid);

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

int sceCdSync(int mode);
int sceCdGetError();
int sceCdGetDiskType();
int sceCdMmode(int media);
int sceCdBreak();
int sceCdDiskReady(int mode);

u32 sceSifSetDma(sceSifDmaData* sdd, int len);

s32 SendMbx(int mbxid, void* sendmsg);
s32 PollMbx(MsgPacket** recvmsg, int mbxid);
s32 PeekMbx(s32 mbx);
s32 CreateMbx(MbxParam* param);

void GetSystemTime(SysClock* time);

s32 CreateSema(SemaParam* param);
s32 WaitSema(s32 sema);
s32 SignalSema(s32 sema);
s32 PollSema(s32 sema);

s32 RegisterVblankHandler(int edge, int priority, int (*handler)(void*), void* userdata);

void FlushDcache();

u32 sceSifCheckInit();
void sceSifInit();

void LIBRARY_INIT();
void LIBRARY_register(::IOP* i);
void LIBRARY_kill();
}  // namespace iop
