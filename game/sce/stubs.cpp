#include <stdexcept>
#include <cassert>
#include "stubs.h"

namespace ee {
s32 sceOpen(const char *filename, s32 flag) {
  (void)filename;
  (void)flag;
  throw std::exception("sceOpen NYI");
}

s32 sceClose(s32 fd) {
  (void)fd;
  throw std::exception("sceClose NYI");
}

s32 sceRead(s32 fd, void *buf, s32 nbyte) {
  (void)fd;
  (void)buf;
  (void)nbyte;
  throw std::exception("sceRead NYI");
}

s32 sceWrite(s32 fd, const void *buf, s32 nbyte) {
  (void)fd;
  (void)buf;
  (void)nbyte;
  throw std::exception("sceWrite NYI");
}

s32 sceLseek(s32 fd, s32 offset, s32 where) {
  (void)fd;
  (void)offset;
  (void)where;
  throw std::exception("sceLseek NYI");
}

int scePadPortOpen(int port, int slot, void* data) {
  (void)port;
  (void)slot;
  (void)data;
  assert(false);
  return 0;
}

void sceGsSyncV() {
  assert(false);
}

void sceGsSyncPath() {
  assert(false);
}

void sceGsResetPath() {
  assert(false);
}

void sceGsResetGraph() {
  assert(false);
}

void sceDmaSync() {
  assert(false);
}

void sceGsPutIMR() {
  assert(false);
}

void sceGsGetIMR() {
  assert(false);
}

void sceGsExecStoreImage() {
  assert(false);
}

void FlushCache() {
  assert(false);
}

}

namespace iop {
u32 snd_BankLoadByLoc(u32 sector, u32 unk) {
  (void)sector;
  (void)unk;
  assert(false);
  return 0;
}

u32 snd_GetLastLoadError() {
  assert(false);
  return 0;
}

void snd_ResolveBankXREFS() {
  assert(false);
}

}
