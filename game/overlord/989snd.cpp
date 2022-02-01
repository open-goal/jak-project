#include <stdio.h>

#include "game/sce/iop.h"
#include "game/sce/libsd.h"

/*!
 * @file 989snd.cpp
 * Stub implementation of the 989 sound library
 */

using namespace iop;

int* gDMAInUse;
u32 VAGStreamDMAList;
int gStartDMASema;

void snd_init_globals() {
  gDMAInUse = nullptr;
  VAGStreamDMAList = 0;
  gStartDMASema = 0;
}

// TODO
int snd_GetFreeSPUDMA() {
  int v1;
  int v2 = CpuSuspendIntr(&v1);
  if (gDMAInUse[0]) {
    if (gDMAInUse[1]) {
      if (!v2)
        CpuResumeIntr(v1);
      return 0xFFFFFFFF;
    } else {
      gDMAInUse[1] = 1;
      if (!v2)
        CpuResumeIntr(v1);
      // sceSdSetTransIntrHandler(1, 0, 0);
      return 1;
    }
  } else {
    gDMAInUse[0] = 1;
    if (!v2)
      CpuResumeIntr(v1);
    // sceSdSetTransIntrHandler(0, 0, 0);
    return 0;
  }
}

// TODO
void snd_FreeSPUDMA(int loc) {
  gDMAInUse[loc] = 0;
  if (VAGStreamDMAList) {
    SignalSema(gStartDMASema);
  }
}
