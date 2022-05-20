// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#pragma once
#include "ame_handler.h"
#include "game/sound/989snd/vagvoice.h"
#include "third-party/cubeb/cubeb/include/cubeb/cubeb.h"
#include "midi_handler.h"
#include "sound_handler.h"
#include "loader.h"
#include "../common/synth.h"
#include "common/common_types.h"
#include "handle_allocator.h"
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

  u32 play_sound(u32 bank, u32 sound, s32 vol, s32 pan, s32 pm, s32 pb);
  void set_midi_reg(u32 sound_id, u8 reg, u8 value);
  bool sound_still_active(u32 sound_id);
  void set_master_volume(u32 group, s32 volume);
  void unload_bank(u32 bank_handle);
  void stop_sound(u32 sound_handle);
  void set_pan_table(vol_pair* pantable);
  void set_playback_mode(s32 mode);
  void pause_sound(s32 sound_handle);
  void continue_sound(s32 sound_handle);
  void pause_all_sounds_in_group(u8 group);
  void continue_all_sounds_in_group(u8 group);
  void set_sound_vol_pan(s32 sound_handle, s32 vol, s32 pan);
  void submit_voice(std::shared_ptr<voice>& voice) { m_synth.add_voice(voice); };
  void set_sound_pmod(s32 sound_handle, s32 mod);
  void init_cubeb();
  void destroy_cubeb();
  s32 get_tick() { return m_tick; };

 private:
  std::recursive_mutex m_ticklock;  // TODO does not need to recursive with some light restructuring
  id_allocator m_handle_allocator;
  std::unordered_map<u32, std::unique_ptr<sound_handler>> m_handlers;

  void tick(s16_output* stream, int samples);

#ifdef _WIN32
  bool m_coinitialized = false;
#endif

  loader m_loader;
  synth m_synth;
  voice_manager m_vmanager;
  s32 m_tick{0};

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
