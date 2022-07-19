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
 * Create a new thread
 */
s32 CreateThread(ThreadParam* param) {
  return iop->kernel.CreateThread(param->name, (void (*)())param->entry);
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
                       sceSifRpcFunc cfunc,
                       void* cbuff,
                       sceSifQueueData* qd) {
  serve->command = request;
  serve->func = func;
  serve->buff = buff;
  (void)cfunc;
  (void)cbuff;
  ASSERT(!cfunc);
  ASSERT(!cbuff);
  qd->serve_data = serve;
}

void sceSifRpcLoop(sceSifQueueData* pd) {
  iop->kernel.rpc_loop(pd);
}

int sceCdRead(uint32_t logical_sector, uint32_t sectors, void* buf, sceCdRMode* mode) {
  (void)mode;
  iop->kernel.read_disc_sectors(logical_sector, sectors, buf);
  return 1;
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
  iop->kernel.SuspendThread();
  (void)usec;
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

static int now = 0;

void GetSystemTime(SysClock* time) {
  time->lo = 0;
  time->hi = now;
  now += 10;
}

void SleepThread() {
  iop->kernel.SleepThread();
}

s32 CreateSema(SemaParam* param) {
  (void)param;
  return iop->kernel.CreateSema();
}

s32 WaitSema(s32 sema) {
  (void)sema;
  ASSERT(false);  // nyi
  return 0;
}

s32 SignalSema(s32 sema) {
  (void)sema;
  ASSERT(false);  // nyi
  return 0;
}

s32 WakeupThread(s32 thid) {
  iop->kernel.WakeupThread(thid);
  return 0;
}
}  // namespace iop
