#include "iop.h"

#include <cstring>

#include "common/util/Assert.h"

#include "game/system/iop_thread.h"

namespace iop {
/*!
 * Is the SIF initialized?
 */
u32 sceSifCheckInit() {
  // the SIF is always initialized by the time OVERLORD starts.
  // it would only be on an ancient dev kit where this might not be true.
  return 1;
}

/*!
 * Initialize SIF
 */
void sceSifInit() {
  // do nothing!
}

/*!
 * Initialize RPC
 */
void sceSifInitRpc(int mode) {
  ASSERT(mode == 0);
}

/*!
 * Flush Data Cache
 */
void FlushDcache() {
  // Do nothing! The data cache does not need to be flushed on x86 as we have no DMA which bypasses
  // cache.
}

/*!
 * Enable CPU Interrupts
 */
void CpuDisableIntr() {}

/*!
 * Disable CPU Interrupts
 */
void CpuEnableIntr() {}

namespace {
::IOP* iop;
}

void LIBRARY_INIT() {
  iop = nullptr;
}

void LIBRARY_register(::IOP* i) {
  iop = i;
}

void LIBRARY_kill() {
  iop->kill_from_ee();
}

/*!
 * How much free memory is there, in bytes?
 */
int QueryTotalFreeMemSize() {
  // this value is somewhat arbitrary - it's a lot, but not enough to make OVERLORD think it is
  // running on an 8MB-of-IOP-RAM development machine.
  return 0x100000;
}

/*!
 * Allocate memory.
 */
void* AllocSysMemory(int type, unsigned long size, void* addr) {
  ASSERT(type == SMEM_Low);
  ASSERT(addr == nullptr);
  return iop->iop_alloc(size);
}

/*!
 * Allocate the 1 kB scratchpad memory. On PS2, this would give you a pointer to the actual
 * scratchpad of the IOP, but this is just normal memory.
 */
void* AllocScratchPad(int mode) {
  ASSERT(mode == 0);
  constexpr int kScratchpadSize = 1024 * 16;
  return iop->iop_alloc(kScratchpadSize);
}

/*!
 * Create a new thread
 */
s32 CreateThread(ThreadParam* param) {
  return iop->kernel.CreateThread(param->name, (void (*)())param->entry, param->initPriority);
}

/*!
 * Exit current thread
 */
s32 ExitThread() {
  return iop->kernel.ExitThread();
}

/*!
 * Create a new message box.
 */
s32 CreateMbx(MbxParam* param) {
  (void)param;
  return iop->kernel.CreateMbx();
}

s32 StartThread(s32 thid, u32 arg) {
  ASSERT(!arg);
  iop->kernel.StartThread(thid);
  return 0;
}

int GetThreadId() {
  return iop->kernel.getCurrentThread();
}

void sceSifSetRpcQueue(sceSifQueueData* dq, int key) {
  dq->key = key;
  iop->kernel.set_rpc_queue(dq, key);
}

void sceSifRegisterRpc(sceSifServeData* serve,
                       unsigned int request,
                       sceSifRpcFunc func,
                       void* buff,
                       int buff_size,
                       sceSifRpcFunc cfunc,
                       void* cbuff,
                       sceSifQueueData* qd) {
  serve->command = request;
  serve->func = func;
  serve->buff = buff;
  serve->buff_size = buff_size;
  (void)cfunc;
  (void)cbuff;
  ASSERT(!cfunc);
  ASSERT(!cbuff);
  qd->serve_data = serve;
}

void sceSifRpcLoop(sceSifQueueData* pd) {
  iop->kernel.rpc_loop(pd);
}

int sceCdSync(int mode) {
  (void)mode;
  return 0;
}

int sceCdGetError() {
  return 0;  // no error
}

int sceCdGetDiskType() {
  return SCECdPS2DVD;  // always a DVD (for now)
}

int sceCdMmode(int media) {
  (void)media;
  return 1;
}

void DelayThread(u32 usec) {
  iop->kernel.DelayThread(usec);
}

void YieldThread() {
  iop->kernel.YieldThread();
}

int sceCdBreak() {
  return 1;
}

int sceCdDiskReady(int mode) {
  (void)mode;
  return SCECdComplete;
}

u32 sceSifSetDma(sceSifDmaData* sdd, int len) {
  ASSERT(len == 1);
  ASSERT(len <= 0xc000);
  // todo - sanity check the destination address.
  memcpy(iop->ee_main_mem + (u64)(sdd->addr), sdd->data, sdd->size);
  return 1;
}

s32 SendMbx(s32 mbxid, void* sendmsg) {
  return iop->kernel.SendMbx(mbxid, sendmsg);
}

s32 PollMbx(MsgPacket** recvmsg, int mbxid) {
  return iop->kernel.PollMbx((void**)recvmsg, mbxid);
}

s32 ReceiveMbx(MsgPacket** recvmsg, int mbxid) {
  return iop->kernel.ReceiveMbx((void**)recvmsg, mbxid);
}

s32 PeekMbx(s32 mbx) {
  return iop->kernel.PeekMbx(mbx);
}

s32 MbxSize(s32 mbx) {
  return iop->kernel.MbxSize(mbx);
}

u32 GetSystemTimeLow() {
  return iop->kernel.GetSystemTimeLow();
}

void SleepThread() {
  iop->kernel.SleepThread();
}

s32 CreateSema(SemaParam* param) {
  return iop->kernel.CreateSema(param->attr, param->option, param->init_count, param->max_count);
}

s32 WaitSema(s32 sema) {
  return iop->kernel.WaitSema(sema);
}

s32 SignalSema(s32 sema) {
  return iop->kernel.SignalSema(sema);
}

s32 PollSema(s32 sema) {
  return iop->kernel.PollSema(sema);
}

s32 CreateEventFlag(const EventFlagParam* param) {
  return iop->kernel.CreateEventFlag(param->attr, param->option, param->init_pattern);
}

s32 ClearEventFlag(s32 flag, u32 pattern) {
  return iop->kernel.ClearEventFlag(flag, pattern);
}

s32 WaitEventFlag(s32 flag, u32 pattern, u32 mode) {
  return iop->kernel.WaitEventFlag(flag, pattern, mode);
}

s32 SetEventFlag(s32 flag, u32 pattern) {
  return iop->kernel.SetEventFlag(flag, pattern);
}

s32 WakeupThread(s32 thid) {
  iop->kernel.WakeupThread(thid);
  return 0;
}

s32 iWakeupThread(s32 thid) {
  iop->kernel.iWakeupThread(thid);
  return 0;
}

s32 RegisterVblankHandler(int edge, int priority, int (*handler)(void*), void* /*userdata*/) {
  (void)edge;
  (void)priority;
  return iop->kernel.RegisterVblankHandler(handler);
}

}  // namespace iop
