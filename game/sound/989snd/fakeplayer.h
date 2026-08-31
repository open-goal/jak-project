#pragma once

#include <memory>
#include <span>

#include "ame_handler.h"
#include "loader.h"
#include "sound_handler.h"

#include "common/common_types.h"

#include "../common/synth.h"
#include "game/sound/989snd/vagvoice.h"

namespace snd {

class FakePlayer {
 public:
  FakePlayer();
  ~FakePlayer();
  FakePlayer(const FakePlayer&) = delete;
  FakePlayer operator=(const FakePlayer&) = delete;

  BankHandle LoadBank(std::span<u8> bank);

  u32 PlaySound(BankHandle bank, u32 sound, s32 vol, s32 pan, s32 pm, s32 pb);
  void StopSound();
  u32 PlaySoundByName(BankHandle bank,
                      char* bank_name,
                      char* sound_name,
                      s32 vol,
                      s32 pan,
                      s32 pm,
                      s32 pb);
  void SetSoundReg(u8 reg, u8 value);
  u8 GetSoundGroup(u32 sound_id);
  void SetGlobalExcite(u8 value) { GlobalExcite = value; };
  void SetMasterVolume(u32 group, s32 volume);
  void UnloadBank(BankHandle bank_handle);
  u32 GetSoundID(u32 sound_handle);
  void SetPanTable(VolPair* pantable);
  void SetPlaybackMode(s32 mode);
  void SetSoundVolPan(s32 sound_handle, s32 vol, s32 pan);
  void SubmitVoice(std::shared_ptr<Voice>& voice) { mSynth.AddVoice(voice); };
  void SetSoundPmod(s32 sound_handle, s32 mod);
  s32 GetTick() { return mTick; };
  s32 GetSoundUserData(BankHandle block_handle,
                       char* block_name,
                       s32 sound_id,
                       char* sound_name,
                       SFXUserData* dst);

  bool Tick(std::vector<s16>& leftStream, std::vector<s16>& rightStream, long samples);
  
 private:
  std::unique_ptr<SoundHandler> m_handler;

  Loader mLoader;
  Synth mSynth;
  VoiceManager mVmanager;
  s32 mTick{0};

  // static long sound_callback(cubeb_stream* stream,
  //                            void* user,
  //                            const void* input,
  //                            void* output_buffer,
  //                            long len);
  // static void state_callback(cubeb_stream* stream, void* user, cubeb_state state);
};
}  // namespace snd
