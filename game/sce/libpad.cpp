#include "libpad.h"

#include "common/util/Assert.h"

#include "game/graphics/gfx.h"
#include "game/kernel/common/kernel_types.h"

/*!
 * @file libpad.cpp
 * Stub implementation of the EE pad (controller) library
 */

namespace ee {

int scePadPortOpen(int port, int slot, void*) {
  // we are expected to return a non-zero file descriptor.
  // we return the port + 1 and succeed always.
  // the game just opens this once at the beginning, we don't have to implement closing/reopening.
  ASSERT(slot == 0);
  return port + 1;
}

int scePadGetState(int /*port*/, int /*slot*/) {
  // pretend we always have a controller connected
  return scePadStateStable;
}

// controller mode array for DS2s
// the game will try very hard to force into dualshock mode so let's not even try
static const int libpad_DualShock2_ModeIDs[2] = {
    (int)PadMode::Controller,  // no vibration or pressure sensitive buttons
    (int)PadMode::DualShock2   // vibration + pressure sensitive buttons
};
int scePadInfoMode(int /*port*/, int /*slot*/, int term, int offs) {
  if (term == InfoModeCurExID) {
    // return vibration mode ID. that's just dualshock mode.
    return libpad_DualShock2_ModeIDs[1];

  } else if (term == InfoModeCurID) {
    // return mode ID. just dualshock.
    return libpad_DualShock2_ModeIDs[1];

  } else if (term == InfoModeCurExOffs) {
    // offset of current mode ID
    return 1;

  } else if (term == InfoModeIdTable) {
    if (offs == -1)
      return 2;

    return libpad_DualShock2_ModeIDs[offs];  // ?
  }

  // invalid controller or some other error
  return 0;
}

// order of pressure sensitive buttons in memory (not the same as their bit order...).
static const Pad::Button libpad_PadPressureButtons[] = {
    Pad::Button::Right,    Pad::Button::Left,   Pad::Button::Up, Pad::Button::Down,
    Pad::Button::Triangle, Pad::Button::Circle, Pad::Button::X,  Pad::Button::Square,
    Pad::Button::L1,       Pad::Button::R1,     Pad::Button::L2, Pad::Button::R2};
// reads controller data and writes it to a buffer in rdata (must be at least 32 bytes large).
// returns buffer size (32) or 0 on error.
int scePadRead(int port, int /*slot*/, u8* rdata) {
  auto cpad = (CPadInfo*)(rdata);
  // Gfx::poll_events();

  cpad->valid = 0;  // success

  cpad->status = 0x70 /* (dualshock2) */ | (20 / 2); /* (dualshock2 data size) */

  cpad->rightx = Gfx::PadGetAnalogValue(Pad::Analog::Right_X, port);
  cpad->righty = Gfx::PadGetAnalogValue(Pad::Analog::Right_Y, port);
  cpad->leftx = Gfx::PadGetAnalogValue(Pad::Analog::Left_X, port);
  cpad->lefty = Gfx::PadGetAnalogValue(Pad::Analog::Left_Y, port);

  // pressure sensitivity. ignore for now.
  for (int i = 0; i < 12; ++i) {
    cpad->abutton[i] = Gfx::PadIsPressed(libpad_PadPressureButtons[i], port) * 255;
  }

  cpad->button0 = 0;
  for (int i = 0; i < 16; ++i) {
    cpad->button0 |= Gfx::PadIsPressed((Pad::Button)i, port) << i;
  }

  // keys polled and read, prepare for new ones.
  Pad::ClearKeys();

  return 32;
}

int scePadSetActDirect(int port, int /*slot*/, const u8* data) {
  // offsets are set by scePadSetActAlign, but we already know the game uses 0 for big motor and 1
  // for small motor
  // also, the "slow" motor corresponds to the "large" motor on the PS2
  return Pad::rumble(port, ((float)data[1]) / 255, ((float)data[0]));
}

int scePadSetActAlign(int /*port*/, int /*slot*/, const u8* /*data*/) {
  return 1;
}

// we also don't care
int scePadSetMainMode(int /*port*/, int /*slot*/, int /*offs*/, int /*lock*/) {
  return 1;
}

// async pad functions are gonna be synchronous so this always succeeds
int scePadGetReqState(int /*port*/, int /*slot*/) {
  return scePadReqStateComplete;
}

int scePadInfoAct(int /*port*/, int /*slot*/, int actno, int term) {
  if (actno == -1)
    return 2;  // i think?
  if (actno < 2) {
    if (term == InfoActSub) {
      return 1;  // whatever
    } else if (term == InfoActFunc) {
      return 1;  // whatever
    } else if (term == InfoActSize) {
      return 0;  // whatever
    } else if (term == InfoActCurr) {
      return 1;  // whatever
    }
  }
  return 0;
}

int scePadInfoPressMode(int /*port*/, int /*slot*/) {
  return 0;  // we do NOT support pressure sensitive buttons right now
}

int scePadEnterPressMode(int /*port*/, int /*slot*/) {
  return 1;  // we dont support pressure button, but if we did this would work straight away
}

}  // namespace ee
