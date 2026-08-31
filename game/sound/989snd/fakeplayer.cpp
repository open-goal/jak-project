#include "fakeplayer.h"

#include "sfxblock.h"

#include "fmt/format.h"
#include "game/sound/common/sound_types.h"

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <combaseapi.h>
#include <windows.h>
#endif
#include "common/log/log.h"

namespace snd {

FakePlayer::FakePlayer() : mVmanager(mSynth) {
}

FakePlayer::~FakePlayer() {
}

bool FakePlayer::Tick(std::vector<s16>& leftStream, std::vector<s16>& rightStream, long samples) {
  static int htick = 200;
  static int stick = snd::SAMPLE_RATE;
  
  for (long i = 0; i < samples; i++) {
    // The handlers expect to tick at 240hz
    // 48000/240 = 200
    if (htick == 200) {
      mTick++;

      //m_handler->Tick returns true if the handler is done.
      if(m_handler && m_handler->Tick())
      {
        m_handler = NULL; //deletes the handler.
        return true;
      }

      htick = 0;
    }

    if (stick == snd::SAMPLE_RATE) {
      // fmt::print("{} handlers active\n", m_handlers.size());
      stick = 0;
    }

    stick++;
    htick++;
    auto s16Output = mSynth.Tick();
    leftStream.push_back(s16Output.left);
    rightStream.push_back( s16Output.right);
  }
  return false;
}

u32 FakePlayer::PlaySound(BankHandle bank_id, u32 sound_id, s32 vol, s32 pan, s32 pm, s32 pb) {
  assert(m_handler == nullptr);
  auto bank = mLoader.GetBankByHandle(bank_id);
  if (bank == nullptr) {
    lg::error("play_sound: Bank {} does not exist", static_cast<void*>(bank_id));
    return 0;
  }

  auto handler = bank->MakeHandler(mVmanager, sound_id, vol, pan, pm, pb, GetTick(), 0);
  if (!handler.has_value()) {
    return 0;
  }

  handler.value()->m_sound_handle = 0;
  m_handler = std::move(handler.value());
  // fmt::print("play_sound {}:{} - {}\n", bank_id, sound_id, handle);

  return 0;
}

u32 FakePlayer::PlaySoundByName(BankHandle bank_id,
                            char* bank_name,
                            char* sound_name,
                            s32 vol,
                            s32 pan,
                            s32 pm,
                            s32 pb) {
  SoundBank* bank = nullptr;
  if (bank_id == 0 && bank_name != nullptr) {
    bank = mLoader.GetBankByName(bank_name);
  } else if (bank_id != 0) {
    bank = mLoader.GetBankByHandle(bank_id);
  } else {
    bank = mLoader.GetBankWithSound(sound_name);
  }

  if (bank == nullptr) {
    return 0;
  }

  auto sound = bank->GetSoundByName(sound_name);
  if (sound.has_value()) {
    return PlaySound(bank, sound.value(), vol, pan, pm, pb);
  }

  return 0;
}

u32 FakePlayer::GetSoundID(u32 sound_handle) {
  return m_handler ? m_handler->SoundID() : -1;
}

void FakePlayer::SetSoundReg(u8 reg, u8 value) {
  if(m_handler)
    m_handler->SetRegister(reg, value);
}

u8 FakePlayer::GetSoundGroup(u32 sound_id) {
  return m_handler ? m_handler->Group() : - 1;
}


void FakePlayer::SetMasterVolume(u32 group, s32 volume) {
  if (volume > snd::MAX_VOLUME)
    volume = snd::MAX_VOLUME;

  if (volume < 0)
    volume = 0;

  if (group == 15)
    return;

  mVmanager.SetMasterVol(group, volume);

  // Master volume
  if (group == 16) {
    mSynth.SetMasterVol(0x3ffff * volume / snd::MAX_VOLUME);
  }
}

BankHandle FakePlayer::LoadBank(std::span<u8> bank) {
  return mLoader.BankLoad(bank);
}

void FakePlayer::UnloadBank(BankHandle bank_handle) {
  assert(m_handler == nullptr);
  auto* bank = mLoader.GetBankByHandle(bank_handle);
  if (bank == nullptr)
    return;

  mLoader.UnloadBank(bank_handle);
}

void FakePlayer::SetPanTable(VolPair* pantable) {
  mVmanager.SetPanTable(pantable);
}

void FakePlayer::SetPlaybackMode(s32 mode) {
  mVmanager.SetPlaybackMode(mode);
}

void FakePlayer::SetSoundVolPan(s32 sound_id, s32 vol, s32 pan) {
  if(m_handler)
    m_handler->SetVolPan(vol, pan);
}

void FakePlayer::SetSoundPmod(s32 sound_handle, s32 mod) {
  if(m_handler)
    m_handler->SetPMod(mod);
}

s32 FakePlayer::GetSoundUserData(BankHandle block_handle,
                             char* block_name,
                             s32 sound_id,
                             char* sound_name,
                             SFXUserData* dst) {
  SoundBank* bank = nullptr;
  if (block_handle == nullptr && block_name != nullptr) {
    bank = mLoader.GetBankByName(block_name);
  } else if (block_handle != nullptr) {
    bank = mLoader.GetBankByHandle(block_handle);
  } else {
    bank = mLoader.GetBankWithSound(sound_name);
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
