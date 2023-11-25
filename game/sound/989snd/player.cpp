// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#include "player.h"

#include <fstream>

#include "loader.h"
#include "sfxblock.h"
#include "sound_handler.h"
#include "vagvoice.h"

#include "third-party/fmt/core.h"

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <combaseapi.h>
#include <windows.h>
#endif
#include "common/log/log.h"

namespace snd {

u8 g_global_excite = 0;
std::recursive_mutex gTickLock;  // TODO does not need to recursive with some light restructuring
std::vector<SoundHandler*> gHandlers;
Synth gSynth;
s32 gTick{0};

cubeb* gCtx{nullptr};
cubeb_stream* gStream{nullptr};

#ifdef _WIN32
bool m_coinitialized = false;
#endif

void InitCubeb();
void Tick(s16Output* stream, int samples);

void StartSoundSystem() {
  InitCubeb();
  VoiceManagerInit(gSynth);
}

void StopSoundSystem() {
  DestroyCubeb();
}

static long sound_callback([[maybe_unused]] cubeb_stream* stream,
                           void* user,
                           [[maybe_unused]] const void* input,
                           void* output_buffer,
                           long nframes) {
  Tick((s16Output*)output_buffer, nframes);
  return nframes;
}

static void state_callback([[maybe_unused]] cubeb_stream* stream,
                           [[maybe_unused]] void* user,
                           [[maybe_unused]] cubeb_state state) {}

void InitCubeb() {
#ifdef _WIN32
  HRESULT hr = CoInitializeEx(nullptr, COINIT_MULTITHREADED);
  m_coinitialized = SUCCEEDED(hr);
  if (FAILED(hr) && hr != RPC_E_CHANGED_MODE) {
    lg::error("Couldn't initialize COM");
    lg::error("Cubeb init failed");
    return;
  }
#endif

  cubeb_init(&gCtx, "OpenGOAL", nullptr);

  cubeb_stream_params outparam = {};
  outparam.channels = 2;
  outparam.format = CUBEB_SAMPLE_S16LE;
  outparam.rate = 48000;
  outparam.layout = CUBEB_LAYOUT_STEREO;
  outparam.prefs = CUBEB_STREAM_PREF_NONE;

  s32 err = 0;
  u32 latency = 0;
  err = cubeb_get_min_latency(gCtx, &outparam, &latency);
  if (err != CUBEB_OK) {
    lg::error("Cubeb init failed");
    return;
  }

  err = cubeb_stream_init(gCtx, &gStream, "OpenGOAL", nullptr, nullptr, nullptr, &outparam, latency,
                          &sound_callback, &state_callback, NULL);
  if (err != CUBEB_OK) {
    lg::error("Cubeb init failed");
    return;
  }

  err = cubeb_stream_start(gStream);
  if (err != CUBEB_OK) {
    lg::error("Cubeb init failed");
    return;
  }
}

void DestroyCubeb() {
  cubeb_stream_stop(gStream);
  cubeb_stream_destroy(gStream);
  cubeb_destroy(gCtx);
#ifdef _WIN32
  if (m_coinitialized) {
    CoUninitialize();
    m_coinitialized = false;
  }
#endif
}

void SubmitVoice(std::shared_ptr<Voice>& voice) {
  gSynth.AddVoice(voice);
};

s32 GetTick() {
  return gTick;
};

void SetGlobalExcite(u8 value) {
  GlobalExcite = value;
};

void Tick(s16Output* stream, int samples) {
  std::scoped_lock lock(gTickLock);
  static int htick = 200;
  static int stick = 48000;
  for (int i = 0; i < samples; i++) {
    // The handlers expect to tick at 240hz
    // 48000/240 = 200
    if (htick == 200) {
      gTick++;

      for (auto it = gHandlers.begin(); it != gHandlers.end();) {
        bool done = (*it)->Tick();
        if (done) {
          // fmt::print("erasing handler\n");
          FreeSound(*it);
          it = gHandlers.erase(it);
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
    *stream++ = gSynth.Tick();
  }
}

u32 PlaySound(BankHandle bank_id, u32 sound_id, s32 vol, s32 pan, s32 pm, s32 pb) {
  std::scoped_lock lock(gTickLock);
  auto bank = GetBankByHandle(bank_id);
  if (bank == nullptr) {
    lg::error("play_sound: Bank {} does not exist", static_cast<void*>(bank_id));
    return 0;
  }

  auto handler = bank->MakeHandler(sound_id, vol, pan, pm, pb);
  if (!handler) {
    return 0;
  }

  gHandlers.push_back(handler);
  return handler->Handle();
}

u32 PlaySoundByName(BankHandle bank_id,
                    char* bank_name,
                    char* sound_name,
                    s32 vol,
                    s32 pan,
                    s32 pm,
                    s32 pb) {
  std::scoped_lock lock(gTickLock);
  SoundBank* bank = nullptr;
  if (bank_id == 0 && bank_name != nullptr) {
    bank = GetBankByName(bank_name);
  } else if (bank_id != 0) {
    bank = GetBankByHandle(bank_id);
  } else {
    bank = GetBankWithSound(sound_name);
  }

  if (bank == nullptr) {
    // lg::error("play_sound_by_name: failed to find bank for sound {}", sound_name);
    return 0;
  }

  auto sound = bank->GetSoundByName(sound_name);
  if (sound.has_value()) {
    // lg::error("play_sound_by_name: playing {}", sound_name);
    return PlaySound(bank, sound.value(), vol, pan, pm, pb);
  }

  // lg::error("play_sound_by_name: failed to find sound {}", sound_name);

  return 0;
}

void StopSound(u32 sound_id) {
  std::scoped_lock lock(gTickLock);
  auto s = GetSound(sound_id);
  if (s == nullptr)
    return;

  s->Stop();
}

void SetSoundReg(u32 sound_id, u8 reg, u8 value) {
  std::scoped_lock lock(gTickLock);
  auto s = GetSound(sound_id);
  if (s == nullptr) {
    // fmt::print("set_midi_reg: Handler {} does not exist\n", sound_id);
    return;
  }

  s->SetRegister(reg, value);
}

u32 SoundStillActive(u32 sound_id) {
  std::scoped_lock lock(gTickLock);
  auto s = GetSound(sound_id);
  if (s == nullptr) {
    return 0;
  }

  return s->Handle();
}

void SetMasterVolume(u32 group, s32 volume) {
  std::scoped_lock lock(gTickLock);
  if (volume > 0x400)
    volume = 0x400;

  if (volume < 0)
    volume = 0;

  if (group == 15)
    return;

  snd::SetMasterVol(group, volume);

  // Master volume
  if (group == 16) {
    gSynth.SetMasterVol(0x3ffff * volume / 0x400);
  }
}

BankHandle LoadBank(nonstd::span<u8> bank) {
  std::scoped_lock lock(gTickLock);
  return BankLoad(bank);
}

void UnloadBank(BankHandle bank_handle) {
  std::scoped_lock lock(gTickLock);
  auto* bank = GetBankByHandle(bank_handle);
  if (bank == nullptr)
    return;

  for (auto it = gHandlers.begin(); it != gHandlers.end();) {
    if (&((*it)->Bank()) == bank) {
      FreeSound(*it);
      it = gHandlers.erase(it);
    } else {
      ++it;
    }
  }

  BankLoad(bank_handle);
}

void PauseSound(s32 sound_id) {
  std::scoped_lock lock(gTickLock);
  auto s = GetSound(sound_id);
  if (s == nullptr)
    return;

  s->Pause();
}

void ContinueSound(s32 sound_id) {
  std::scoped_lock lock(gTickLock);
  auto s = GetSound(sound_id);
  if (s == nullptr)
    return;

  s->Unpause();
}

void PauseAllSoundsInGroup(u8 group) {
  std::scoped_lock lock(gTickLock);

  for (auto& h : gHandlers) {
    if ((1 << h->Group()) & group) {
      h->Pause();
    }
  }
}

void ContinueAllSoundsInGroup(u8 group) {
  std::scoped_lock lock(gTickLock);

  for (auto& h : gHandlers) {
    if ((1 << h->Group()) & group) {
      h->Unpause();
    }
  }
}

void SetSoundVolPan(s32 sound_id, s32 vol, s32 pan) {
  std::scoped_lock lock(gTickLock);
  auto s = GetSound(sound_id);
  if (s == nullptr)
    return;

  s->SetVolPan(vol, pan);
}

void SetSoundPmod(s32 sound_handle, s32 mod) {
  std::scoped_lock lock(gTickLock);
  auto s = GetSound(sound_handle);
  if (s == nullptr)
    return;

  s->SetPMod(mod);
}

void StopAllSounds() {
  std::scoped_lock lock(gTickLock);
  for (auto it = gHandlers.begin(); it != gHandlers.end();) {
    FreeSound(*it);
    it = gHandlers.erase(it);
  }
}

s32 GetSoundUserData(BankHandle block_handle,
                     char* block_name,
                     s32 sound_id,
                     char* sound_name,
                     SFXUserData* dst) {
  std::scoped_lock lock(gTickLock);
  SoundBank* bank = nullptr;
  if (block_handle == nullptr && block_name != nullptr) {
    bank = GetBankByName(block_name);
  } else if (block_handle != nullptr) {
    bank = GetBankByHandle(block_handle);
  } else {
    bank = GetBankWithSound(sound_name);
  }

  if (bank == nullptr) {
    return 0;
  }

  if (sound_id == -1) {
    auto sound = bank->GetSoundByName(sound_name);
    if (sound.has_value()) {
      sound_id = sound.value();
    } else {
      return 0;
    }
  }

  auto ud = bank->GetSoundUserData(sound_id);
  if (ud.has_value()) {
    *dst = *ud.value();
    return 1;
  } else {
    return 0;
  }

  return 0;
}

}  // namespace snd
