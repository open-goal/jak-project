// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#include "player.h"

#include <fstream>

#include "third-party/fmt/core.h"

#ifdef _WIN32
#include <combaseapi.h>
#include <windows.h>
#endif
#include "common/log/log.h"

namespace snd {

u8 g_global_excite = 0;

player::player() : m_vmanager(m_synth, m_loader) {
  init_cubeb();
}

player::~player() {
  destroy_cubeb();
}

void player::init_cubeb() {
#ifdef _WIN32
  HRESULT hr = CoInitializeEx(nullptr, COINIT_MULTITHREADED);
  m_coinitialized = SUCCEEDED(hr);
  if (FAILED(hr) && hr != RPC_E_CHANGED_MODE) {
    lg::error("Couldn't initialize COM");
    lg::error("Cubeb init failed");
    return;
  }
#endif

  cubeb_init(&m_ctx, "OpenGOAL", nullptr);

  cubeb_stream_params outparam = {};
  outparam.channels = 2;
  outparam.format = CUBEB_SAMPLE_S16LE;
  outparam.rate = 48000;
  outparam.layout = CUBEB_LAYOUT_STEREO;
  outparam.prefs = CUBEB_STREAM_PREF_NONE;

  s32 err = 0;
  u32 latency = 0;
  err = cubeb_get_min_latency(m_ctx, &outparam, &latency);
  if (err != CUBEB_OK) {
    lg::error("Cubeb init failed");
    return;
  }

  err = cubeb_stream_init(m_ctx, &m_stream, "OpenGOAL", nullptr, nullptr, nullptr, &outparam,
                          latency, &sound_callback, &state_callback, this);
  if (err != CUBEB_OK) {
    lg::error("Cubeb init failed");
    return;
  }

  err = cubeb_stream_start(m_stream);
  if (err != CUBEB_OK) {
    lg::error("Cubeb init failed");
    return;
  }
}

void player::destroy_cubeb() {
  cubeb_stream_stop(m_stream);
  cubeb_stream_destroy(m_stream);
  cubeb_destroy(m_ctx);
#ifdef _WIN32
  if (m_coinitialized) {
    CoUninitialize();
    m_coinitialized = false;
  }
#endif
}

long player::sound_callback([[maybe_unused]] cubeb_stream* stream,
                            void* user,
                            [[maybe_unused]] const void* input,
                            void* output_buffer,
                            long nframes) {
  ((player*)user)->tick((s16_output*)output_buffer, nframes);
  return nframes;
}

void player::state_callback([[maybe_unused]] cubeb_stream* stream,
                            [[maybe_unused]] void* user,
                            [[maybe_unused]] cubeb_state state) {}

void player::tick(s16_output* stream, int samples) {
  std::scoped_lock lock(m_ticklock);
  m_tick++;
  static int htick = 200;
  static int stick = 48000;
  for (int i = 0; i < samples; i++) {
    // The handlers expect to tick at 240hz
    // 48000/240 = 200
    if (htick == 200) {
      for (auto it = m_handlers.begin(); it != m_handlers.end();) {
        bool done = it->second->tick();
        if (done) {
          // fmt::print("erasing handler\n");
          m_handle_allocator.free_id(it->first);
          it = m_handlers.erase(it);
        } else {
          ++it;
        }
      }

      htick = 0;
    }

    if (stick == 48000) {
      // fmt::print("{} handlers active\n", m_handlers.size());
      stick = 0;
    }

    stick++;
    htick++;
    *stream++ = m_synth.tick();
  }
}

u32 player::play_sound(u32 bank_id, u32 sound_id, s32 vol, s32 pan, s32 pm, s32 pb) {
  std::scoped_lock lock(m_ticklock);
  auto bank = m_loader.get_bank_by_handle(bank_id);
  if (bank == nullptr) {
    lg::error("play_sound: Bank {} does not exist", bank_id);
    return 0;
  }

  auto handler = bank->make_handler(m_vmanager, sound_id, vol, pan, pm, pb);
  if (!handler.has_value()) {
    return 0;
  }

  u32 handle = m_handle_allocator.get_id();
  m_handlers.emplace(handle, std::move(handler.value()));
  // fmt::print("play_sound {}:{} - {}\n", bank_id, sound_id, handle);

  return handle;
}

u32 player::play_sound_by_name(u32 bank_id,
                               char* bank_name,
                               char* sound_name,
                               s32 vol,
                               s32 pan,
                               s32 pm,
                               s32 pb) {
  std::scoped_lock lock(m_ticklock);
  SoundBank* bank = nullptr;
  if (bank_id == 0 && bank_name != nullptr) {
    bank = m_loader.get_bank_by_name(bank_name);
  } else if (bank_id != 0) {
    bank = m_loader.get_bank_by_handle(bank_id);
  } else {
    bank = m_loader.get_bank_with_sound(sound_name);
  }

  if (bank == nullptr) {
    // lg::error("play_sound_by_name: failed to find bank for sound {}", sound_name);
    return 0;
  }

  auto sound = bank->get_sound_by_name(sound_name);
  if (sound.has_value()) {
    return play_sound(bank->bank_id, sound.value(), vol, pan, pm, pb);
  }

  // lg::error("play_sound_by_name: failed to find sound {}", sound_name);

  return 0;
}

void player::stop_sound(u32 sound_id) {
  std::scoped_lock lock(m_ticklock);
  auto handler = m_handlers.find(sound_id);
  if (handler == m_handlers.end())
    return;

  handler->second->stop();

  // m_handle_allocator.free_id(sound_id);
  // m_handlers.erase(sound_id);
}

void player::set_sound_reg(u32 sound_id, u8 reg, u8 value) {
  std::scoped_lock lock(m_ticklock);
  if (m_handlers.find(sound_id) == m_handlers.end()) {
    // fmt::print("set_midi_reg: Handler {} does not exist\n", sound_id);
    return;
  }

  auto* handler = m_handlers.at(sound_id).get();
  handler->set_register(reg, value);
}

bool player::sound_still_active(u32 sound_id) {
  std::scoped_lock lock(m_ticklock);
  auto handler = m_handlers.find(sound_id);
  if (handler == m_handlers.end())
    return false;

  // fmt::print("sound_still_active {}\n", sound_id);
  return true;
}

void player::set_master_volume(u32 group, s32 volume) {
  std::scoped_lock lock(m_ticklock);
  if (volume > 0x400)
    volume = 0x400;

  if (volume < 0)
    volume = 0;

  if (group == 15)
    return;

  m_vmanager.set_master_vol(group, volume);

  // Master volume
  if (group == 16) {
    m_synth.set_master_vol(0x3ffff * volume / 0x400);
  }
}

u32 player::load_bank(fs::path& filepath, size_t offset) {
  std::scoped_lock lock(m_ticklock);
  std::fstream in(filepath, std::fstream::binary | std::fstream::in);
  in.seekg(offset, std::fstream::beg);

  return m_loader.read_bank(in);
}

void player::unload_bank(u32 bank_handle) {
  std::scoped_lock lock(m_ticklock);
  auto* bank = m_loader.get_bank_by_handle(bank_handle);
  if (bank == nullptr)
    return;

  for (auto it = m_handlers.begin(); it != m_handlers.end();) {
    if (it->second->bank().bank_id == bank_handle) {
      m_handle_allocator.free_id(it->first);
      it = m_handlers.erase(it);
    } else {
      ++it;
    }
  }

  m_loader.unload_bank(bank_handle);
}

void player::set_pan_table(vol_pair* pantable) {
  std::scoped_lock lock(m_ticklock);
  m_vmanager.set_pan_table(pantable);
}

void player::set_playback_mode(s32 mode) {
  std::scoped_lock lock(m_ticklock);
  m_vmanager.set_playback_mode(mode);
}

void player::pause_sound(s32 sound_id) {
  std::scoped_lock lock(m_ticklock);
  auto handler = m_handlers.find(sound_id);
  if (handler == m_handlers.end())
    return;

  handler->second->pause();
}

void player::continue_sound(s32 sound_id) {
  std::scoped_lock lock(m_ticklock);
  auto handler = m_handlers.find(sound_id);
  if (handler == m_handlers.end())
    return;

  handler->second->unpause();
}

void player::pause_all_sounds_in_group(u8 group) {
  std::scoped_lock lock(m_ticklock);

  for (auto& h : m_handlers) {
    if ((1 << h.second->group()) & group) {
      h.second->pause();
    }
  }
}

void player::continue_all_sounds_in_group(u8 group) {
  std::scoped_lock lock(m_ticklock);

  for (auto& h : m_handlers) {
    if ((1 << h.second->group()) & group) {
      h.second->unpause();
    }
  }
}

void player::set_sound_vol_pan(s32 sound_id, s32 vol, s32 pan) {
  std::scoped_lock lock(m_ticklock);
  auto handler = m_handlers.find(sound_id);
  if (handler == m_handlers.end())
    return;

  handler->second->set_vol_pan(vol, pan);
}

void player::set_sound_pmod(s32 sound_handle, s32 mod) {
  auto handler = m_handlers.find(sound_handle);
  if (handler == m_handlers.end())
    return;

  handler->second->set_pmod(mod);
}

void player::stop_all_sounds() {
  for (auto it = m_handlers.begin(); it != m_handlers.end();) {
    m_handle_allocator.free_id(it->first);
    it = m_handlers.erase(it);
  }
}

s32 player::get_sound_user_data(s32 block_handle,
                                char* block_name,
                                s32 sound_id,
                                char* sound_name,
                                SFXUserData* dst) {
  std::scoped_lock lock(m_ticklock);
  SoundBank* bank = nullptr;
  if (block_handle == 0 && block_name != nullptr) {
    bank = m_loader.get_bank_by_name(block_name);
  } else if (block_handle != 0) {
    bank = m_loader.get_bank_by_handle(block_handle);
  } else {
    bank = m_loader.get_bank_with_sound(sound_name);
  }

  if (bank == nullptr) {
    return 0;
  }

  if (sound_id == -1) {
    auto sound = bank->get_sound_by_name(sound_name);
    if (sound.has_value()) {
      sound_id = sound.value();
    } else {
      return 0;
    }
  }

  auto ud = bank->get_sound_user_data(sound_id);
  if (ud.has_value()) {
    dst->data[0] = ud.value()->data[0];
    dst->data[1] = ud.value()->data[1];
    dst->data[2] = ud.value()->data[2];
    dst->data[3] = ud.value()->data[3];
    return 1;
  } else {
    return 0;
  }

  return 0;
}

}  // namespace snd
