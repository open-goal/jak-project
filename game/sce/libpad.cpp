#include "libpad.h"

#include "common/util/Assert.h"

#include "game/graphics/display.h"
#include "game/graphics/gfx.h"
#include "game/kernel/common/kernel_types.h"
#include "game/system/hid/input_bindings.h"

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

// reads controller data and writes it to a buffer in rdata (must be at least 32 bytes large).
// returns buffer size (32) or 0 on error.
int scePadRead(int port, int /*slot*/, u8* rdata) {
  auto cpad = (CPadInfo*)(rdata);

  cpad->valid = 0;  // success

  cpad->status = 0x70 /* (dualshock2) */ | (20 / 2); /* (dualshock2 data size) */

  std::optional<std::shared_ptr<PadData>> pad_data = std::nullopt;
  if (Display::GetMainDisplay()) {
    if (Gfx::g_debug_settings.treat_pad0_as_pad1) {
      if (port == 0) {
        pad_data = Display::GetMainDisplay()->get_input_manager()->get_current_data(1);
      } else {
        pad_data = Display::GetMainDisplay()->get_input_manager()->get_current_data(0);
      }
    } else {
      pad_data = Display::GetMainDisplay()->get_input_manager()->get_current_data(port);
    }
  }

  if (pad_data) {
    std::tie(cpad->rightx, cpad->righty) = pad_data.value()->analog_right();
    std::tie(cpad->leftx, cpad->lefty) = pad_data.value()->analog_left();

    // pressure sensitivity. ignore for now.
    for (size_t i = 0; i < PAD_DATA_PRESSURE_INDEX_ORDER.size(); i++) {
      cpad->abutton[i] =
          pad_data.value()->button_data.at(PAD_DATA_PRESSURE_INDEX_ORDER.at(i)) * 255;
    }

    cpad->button0 = 0;
    for (size_t i = 0; i < pad_data.value()->button_data.size(); i++) {
      cpad->button0 |= pad_data.value()->button_data.at(i) << i;
    }
  }

  return 32;
}

int scePadSetActDirect(int port, int /*slot*/, const u8* data) {
  // offsets are set by scePadSetActAlign, but we already know the game uses 0 for big motor and 1
  // for small motor
  // also, the "slow" motor corresponds to the "large" motor on the PS2
  if (Display::GetMainDisplay()) {
    if (!Display::GetMainDisplay()->get_input_manager()->controller_has_rumble(port)) {
      return 0;
    }
    Display::GetMainDisplay()->get_input_manager()->enqueue_update_rumble(port, data[1],
                                                                          data[0] * 255);
    return 1;  // TODO - assuming 1 means rumble is supported, and 0 is no?
  }
  return 0;
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
