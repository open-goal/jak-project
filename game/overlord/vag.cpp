#include "vag.h"

#include <array>
#include <cstring>

#include "game/overlord/iso.h"
#include "game/overlord/iso_queue.h"
#include "game/overlord/srpc.h"

static constexpr s32 LOOP_END = 1;
static constexpr s32 LOOP_REPEAT = 2;
static constexpr s32 LOOP_START = 4;

// Empty ADPCM block with loop flags
// clang-format off
u8 VAG_SilentLoop[0x60] = {
    0x0, LOOP_START | LOOP_REPEAT, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    0x0, LOOP_REPEAT,              0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    0x0, LOOP_END | LOOP_REPEAT,   0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
};
// clang-format on

int StreamVoice[4];
u32 StreamSRAM[4];
u32 TrapSRAM[4];

VagCommand2* VagCmdsPriList[10][4];
s32 VagCmdsPriCounter[10];

std::array<VagCommand2, 4> gVagCmds;

void InitVAGCmd(VagCommand2* cmd, int unk) {
  for (auto& s : cmd->stat_arr) {
    s = 0;
  }

  cmd->unk0xec = 0;
  cmd->unk0x8c = 0;
  if (unk) {
    cmd->unk0xd2 = 1;
  } else {
    cmd->unk0xd2 = 0;
  }
  cmd->unk0x108 = 0x4000;
  cmd->callback_function = NullCallback;
  cmd->unk0x38 = -1;
  cmd->unk0xc4 = 0;
  cmd->unk0xc8 = 0;
  cmd->unk0xcc = 0;
  cmd->unk0xb4 = 0;
  cmd->unk0xb8 = 0;
  cmd->unk0xbc = 0;
  cmd->unk0xc0 = 0;
  cmd->unk0xd5 = 0;
  cmd->unk0xd6 = 0;
  cmd->unk0xf0 = 0;
  cmd->unk0x3c = 0;
  cmd->unk0xf4 = 0;
  cmd->unk0xf8 = 0;
  cmd->unk0x104 = 0;
  cmd->unk0x10c = 0;
  cmd->callback_buffer = nullptr;
  cmd->ready_for_data = 1;
  cmd->fd = nullptr;
  cmd->sibling = nullptr;
  cmd->unk0x34 = 0;
  cmd->positioned = 0;
  cmd->trans.x = 0;
  cmd->trans.y = 0;
  cmd->trans.z = 0;
  cmd->fo_min = 5;
  cmd->fo_max = 30;
  cmd->fo_curve = 1;
}

void InitVagCmds() {
  s32 idx = 0;
  for (auto& cmd : gVagCmds) {
    for (auto& s : cmd.stat_arr) {
      s = 0;
    }

    cmd.unk0xec = 0;
    cmd.unk0x8c = 0;
    cmd.unk0xd2 = 1;
    cmd.unk0xc4 = 0;
    cmd.unk0xc8 = 0;
    cmd.unk0xcc = 0;
    cmd.unk0xb4 = 0;
    cmd.unk0xb8 = 0;
    cmd.unk0xbc = 0;
    cmd.unk0xc0 = 0;
    cmd.unk0xd5 = 0;
    cmd.unk0xd6 = 0;
    cmd.unk0xf0 = 0;
    cmd.unk0x3c = 1;
    cmd.unk0xf4 = 0;
    cmd.unk0xf8 = 0;
    cmd.unk0x104 = 0;
    cmd.unk0x108 = 0x4000;
    cmd.unk0x10c = 0;
    cmd.callback_buffer = nullptr;
    cmd.ready_for_data = 1;
    cmd.callback_function = NullCallback;
    cmd.fd = nullptr;
    cmd.sibling = nullptr;
    cmd.unk0x34 = 0;
    cmd.unk0x38 = -1;
    cmd.unk0x100 = 0;
    cmd.positioned = 0;
    cmd.trans.x = 0;
    cmd.trans.y = 0;
    cmd.trans.z = 0;
    cmd.fo_min = 5;
    cmd.fo_max = 30;
    cmd.fo_curve = 1;
    cmd.unk0x78 = StreamSRAM[idx];
    cmd.unk0x7c = TrapSRAM[idx];
    cmd.unk0x84 = idx;
    cmd.unk0x28 = 0;
    cmd.unk0x2c = 0;
    cmd.unk0xd1 = 0;
    cmd.volume = 0;
    cmd.unk0x114 = 0;
    cmd.unk0x118 = 0;
    cmd.unk0x88 = 0;
    cmd.unk0xb0 = 0;
    cmd.unk0x11c = 0;
    cmd.unk0x120 = 0;
    cmd.unk0x124 = 0;
    cmd.unk0x124 = 0;
    cmd.unk0x80 = StreamVoice[idx];

    idx++;
  }

  // something like this?
  for (int i = 0; i < 10; i++) {
    VagCmdsPriList[i][0] = nullptr;
  }

  for (int i = 0; i < 10; i++) {
    VagCmdsPriCounter[i] = 0;
  }
  VagCmdsPriCounter[0] = 4;
}

VagCommand2* FindThisVagStream(const char* name, u32 id) {
  for (auto& cmd : gVagCmds) {
    if (!strcmp(cmd.name, name) && cmd.id == id) {
      return &cmd;
    }
  }

  return nullptr;
}

u32 CalculateVAGPitch(u32 a1, s32 a2) {
  if (a2 == 0) {
    return a1;
  }

  if (a2 <= 0) {
    return 0x5f4 * a1 / (0x5f4 - a2);
  } else {
    return a1 * (a2 + 0x5f4) / 0x5f4;
  }
}

void CalculateVAGVolumes(VagCommand2* cmd, s32* left, s32* right) {
  s32 vol;
  if (cmd->positioned) {
    vol = CalculateFallofVolume(&cmd->trans, (cmd->volume * MasterVolume[2]) >> 10, cmd->fo_curve,
                                cmd->fo_min, cmd->fo_max);
    auto* pan = &gPanTable[(630 - CalculateAngle(&cmd->trans)) % 360];
    *left = (pan->left * vol) >> 10;
    *right = (pan->right * vol) >> 10;
    if (*left >= 0x4000) {
      *left = 0x3FFF;
    }
    if (*right >= 0x4000) {
      *right = 0x3FFF;
    }
  } else {
    vol = (cmd->volume * MasterVolume[2]) >> 6;
    if (vol > 0x3fff) {
      vol = 0x3fff;
    }

    *left = vol;
    *right = vol;
  }
}
