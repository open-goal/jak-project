// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#pragma once
#include "ame_handler.h"
#include "third-party/cubeb/cubeb/include/cubeb/cubeb.h"
#include "midi_handler.h"
#include "sound_handler.h"
#include "loader.h"
#include "../common/synth.h"
#include "common/common_types.h"
#include "automap.h"
#include <filesystem>
#include <unordered_map>
#include <memory>
#include <mutex>
#include <unordered_map>
#include <vector>

namespace snd {

class player {
 public:
  player();
  ~player();
  player(const player&) = delete;
  player operator=(const player&) = delete;

  // player(player&& other) noexcept = default;
  // player& operator=(player&& other) noexcept = default;

  u32 load_bank(std::filesystem::path& path, size_t offset);

  u32 play_sound(u32 bank, u32 sound);
  void set_midi_reg(u32 sound_id, u8 reg, u8 value);
  bool sound_still_active(u32 sound_id);
  void set_master_volume(u32 group, s32 volume);
  void unload_bank(u32 bank_handle);
  void stop_sound(u32 sound_handle);

 private:
  std::recursive_mutex m_ticklock;  // TODO does not need to recursive with some light restructuring
  automap<std::unique_ptr<sound_handler>> m_handlers;

  u32 play_midi(u32 bank, MIDISound& sound, s32 vol, s32 pan);
  u32 play_ame(u32 bank, MIDISound& sound, s32 vol, s32 pan);
  void tick(s16_output* stream, int samples);

  loader m_loader;
  synth m_synth;

  cubeb* m_ctx{nullptr};
  cubeb_stream* m_stream{nullptr};

  static long sound_callback(cubeb_stream* stream,
                             void* user,
                             const void* input,
                             void* output_buffer,
                             long len);
  static void state_callback(cubeb_stream* stream, void* user, cubeb_state state);
};
}  // namespace snd
