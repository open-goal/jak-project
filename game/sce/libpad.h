#pragma once

/*!
 * @file libpad.h
 * Stub implementation of the EE pad (controller) library
 */

#include "common/common_types.h"

#define SCE_PAD_DMA_BUFFER_SIZE 0x100

// pad status
#define scePadStateDiscon 0
#define scePadStateFindPad 1
#define scePadStateFindCTP1 2
#define scePadStateExecCmd 5
#define scePadStateStable 6
#define scePadStateError 7
#define scePadStateClosed 99

// pad mode info checks
#define InfoModeCurID 1
#define InfoModeCurExID 2
#define InfoModeCurExOffs 3
#define InfoModeIdTable 4

// pad async request states
#define scePadReqStateComplete 0
#define scePadReqStateFaild 1  // lol
#define scePadReqStateFailed 1
#define scePadReqStateBusy 2

// pad actuator info checks
#define InfoActFunc 1
#define InfoActSub 2
#define InfoActSize 3
#define InfoActCurr 4

struct CPadInfo;

namespace ee {

// controller modes (not in the lib)
enum PadMode {
  Controller = 4,
  DualShock = 7,
  DualShock2 = DualShock,
  NeGcon = 2,
  Joystick = 5,
  NamcoGun = 6
};

int scePadPortOpen(int port, int slot, void* data);
int scePadGetState(int port, int slot);
int scePadInfoMode(int port, int slot, int term, int offs);
int scePadRead(int port, int slot, u8* rdata);
int scePadSetActDirect(int port, int slot, const u8* data);
int scePadSetActAlign(int port, int slot, const u8* data);
int scePadSetMainMode(int port, int slot, int offs, int lock);
int scePadGetReqState(int port, int slot);
int scePadInfoAct(int port, int slot, int actno, int term);
int scePadInfoPressMode(int port, int slot);
int scePadEnterPressMode(int port, int slot);
}  // namespace ee
