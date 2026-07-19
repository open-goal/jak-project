#pragma once

#include "common/common_types.h"

namespace snd {
using PluginHandler =
    s32 (*)(u32 id, u32 index, u32 owner, const u8* data, const s8* regs, s32 cur_vol);

constexpr u32 VAG_STREAM_PLUGIN_ID = 0x53545256;  // 'STRV'

struct VagStreamPluginData {
  char name[8];
  u32 priority;
  u32 vol;
  u32 pan;
  u32 use_regs;
};
static_assert(sizeof(VagStreamPluginData) == 24);

void RegisterPluginHandler(PluginHandler handler);
s32 HandlePluginMessage(u32 id, u32 index, u32 owner, const u8* data, const s8* regs, s32 cur_vol);

}  // namespace snd
