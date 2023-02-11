#include "vag.h"

#include <array>
#include <cstring>

#include "game/overlord/iso.h"
#include "game/overlord/iso_queue.h"

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

std::array<VagCommand2, 4> gVagCmds;

void InitVagCmds() {
  s32 idx = 0;
  for (auto& vag : gVagCmds) {
    for (auto& s : vag.stat_arr) {
      s = 0;
    }

    vag.unk0xec = 0;
    vag.unk0x8c = 0;
    vag.unk0xd2 = 1;
    vag.unk0xc4 = 0;
    vag.unk0xc8 = 0;
    vag.unk0xcc = 0;
    vag.unk0xb4 = 0;
    vag.unk0xb8 = 0;
    vag.unk0xbc = 0;
    vag.unk0xc0 = 0;
    vag.unk0xd5 = 0;
    vag.unk0xd6 = 0;
    vag.unk0xf0 = 0;
    vag.unk0x3c = 1;
    vag.unk0xf4 = 0;
    vag.unk0xf8 = 0;
    vag.unk0x104 = 0;
    vag.unk0x108 = 0x4000;
    vag.unk0x10c = 0;
    vag.callback_buffer = nullptr;
    vag.ready_for_data = 1;
    vag.callback_function = NullCallback;
    vag.fd = nullptr;
    vag.sibling = nullptr;
    vag.unk0x34 = 0;
    vag.unk0x38 = -1;
    vag.unk0x100 = 0;
    vag.unk0x128 = 0;
    vag.unk0x12c.x = 0;
    vag.unk0x12c.y = 0;
    vag.unk0x12c.z = 0;
    vag.fo_min = 5;
    vag.fo_max = 30;
    vag.fo_curve = 1;
    vag.unk0x78 = StreamSRAM[idx];
    vag.unk0x7c = TrapSRAM[idx];
    vag.unk0x84 = idx;
    vag.unk0x28 = 0;
    vag.unk0x2c = 0;
    vag.unk0xd1 = 0;
    vag.unk0x110 = 0;
    vag.unk0x114 = 0;
    vag.unk0x118 = 0;
    vag.unk0x88 = 0;
    vag.unk0xb0 = 0;
    vag.unk0x11c = 0;
    vag.unk0x120 = 0;
    vag.unk0x124 = 0;
    vag.unk0x124 = 0;
    vag.unk0x80 = StreamVoice[idx];

    idx++;
  }
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
