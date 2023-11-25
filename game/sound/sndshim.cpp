#include "sndshim.h"

#include <cstdio>

#include "sdshim.h"

#include "common/log/log.h"
#include "common/util/FileUtil.h"

#include "989snd/player.h"

void snd_StartSoundSystem() {
  snd::StartSoundSystem();

  for (auto& voice : voices) {
    voice = std::make_shared<snd::Voice>(snd::Voice::AllocationType::Permanent);
    voice->SetSample((u16*)spu_memory);
    snd::SubmitVoice(voice);
  }
}

void snd_StopSoundSystem() {
  snd::StopSoundSystem();
}

// dma is always instant, allocation not required
s32 snd_GetFreeSPUDMA() {
  return 0;
}

void snd_FreeSPUDMA([[maybe_unused]] s32 channel) {}

s32 snd_GetTick() {
  return snd::GetTick();
}

void snd_RegisterIOPMemAllocator(AllocFun, FreeFun) {
  // printf("snd_RegisterIOPMemAllocator\n");
}

int snd_LockVoiceAllocator(bool block) {
  // printf("snd_LockVoiceAllocator\n");
  return 0;
}

void snd_UnlockVoiceAllocator() {
  // printf("snd_UnlockVoiceAllocator\n");
}

s32 snd_ExternVoiceAlloc(s32 vol_group, s32 priority) {
  // printf("snd_ExternVoiceVoiceAlloc\n");
  return 0;
}

u32 snd_SRAMMalloc(u32 size) {
  // spu memory currently hardcoded
  return 0;
}

void snd_SRAMMarkUsed(u32 addr, u32 size) {
  // hope this doesn't matter...
}

void snd_SetMixerMode(s32 channel_mode, s32 reverb_mode) {}

void snd_SetGroupVoiceRange(s32 group, s32 min, s32 max) {}

void snd_SetReverbDepth(s32 core, s32 left, s32 right) {}

void snd_SetReverbType(s32 core, s32 type) {}

void snd_SetPanTable(s16* table) {
  snd::SetPanTable((snd::VolPair*)table);
}

void snd_SetPlayBackMode(s32 mode) {
  snd::SetPlaybackMode(mode);
}

s32 snd_SoundIsStillPlaying(s32 sound_handle) {
  return snd::SoundStillActive(sound_handle);
}

void snd_StopSound(s32 sound_handle) {
  snd::StopSound(sound_handle);
}

void snd_SetSoundVolPan(s32 sound_handle, s32 vol, s32 pan) {
  snd::SetSoundVolPan(sound_handle, vol, pan);
}

void snd_SetMasterVolume(s32 which, s32 volume) {
  snd::SetMasterVolume(which, volume);
}

void snd_UnloadBank(snd::BankHandle bank_handle) {
  snd::UnloadBank(bank_handle);
}

void snd_ResolveBankXREFS() {
  // Currently no-op, idk if we'd ever need it
}

void snd_ContinueAllSoundsInGroup(u8 groups) {
  snd::ContinueAllSoundsInGroup(groups);
}

void snd_PauseAllSoundsInGroup(u8 groups) {
  snd::PauseAllSoundsInGroup(groups);
}

void snd_SetMIDIRegister(s32 sound_handle, u8 reg, u8 value) {
  snd::SetSoundReg(sound_handle, reg, value);
}

s32 snd_PlaySoundVolPanPMPB(snd::BankHandle bank,
                            s32 sound,
                            s32 vol,
                            s32 pan,
                            s32 pitch_mod,
                            s32 pitch_bend) {
  return snd::PlaySound(bank, sound, vol, pan, pitch_mod, pitch_bend);
}

s32 snd_PlaySoundByNameVolPanPMPB(snd::BankHandle bank_handle,
                                  char* bank_name,
                                  char* sound_name,
                                  s32 vol,
                                  s32 pan,
                                  s32 pitch_mod,
                                  s32 pitch_bend) {
  return snd::PlaySoundByName(bank_handle, bank_name, sound_name, vol, pan, pitch_mod, pitch_bend);
}

void snd_SetSoundPitchModifier(s32 sound_handle, s32 pitch_mod) {
  snd::SetSoundPmod(sound_handle, pitch_mod);
}

void snd_SetSoundPitchBend(s32 sound_handle, s32 bend) {
  // lg::warn("unimplemented snd_SetSoundPitchBend({:x}, {})", sound_handle, bend);
}

void snd_PauseSound(s32 sound_handle) {
  snd::PauseSound(sound_handle);
}

void snd_ContinueSound(s32 sound_handle) {
  snd::ContinueSound(sound_handle);
}

void snd_AutoPitch(s32 sound_handle, s32 pitch, s32 delta_time, s32 delta_from) {
  // TODO
  lg::warn("Unimplemented snd_AutoPitch\n");
}
void snd_AutoPitchBend(s32 sound_handle, s32 pitch, s32 delta_time, s32 delta_from) {
  // TODO
  lg::warn("Unimplemented snd_AutoPitchBend\n");
}

snd::BankHandle snd_BankLoadEx(const char* filename,
                               s32 offset,
                               u32 spu_mem_loc,
                               u32 spu_mem_size) {
  auto file_buf = file_util::read_binary_file(std::string(filename));
  return snd::LoadBank(nonstd::span(file_buf).subspan(offset));
}

s32 snd_GetVoiceStatus(s32 voice) {
  // hacky thincg to say that voice 0 is uses allocated
  if (voice == 0) {
    return 2;
  }

  return 0;
}

void snd_keyOnVoiceRaw(u32 core, u32 voice_id) {
  if (voices[0]) {
    voices[0]->KeyOn();
  }
}

void snd_keyOffVoiceRaw(u32 core, u32 voice_id) {
  if (voices[0]) {
    voices[0]->KeyOff();
  }
}

s32 snd_GetSoundUserData(snd::BankHandle block_handle,
                         char* block_name,
                         s32 sound_id,
                         char* sound_name,
                         SFXUserData* dst) {
  return snd::GetSoundUserData(block_handle, block_name, sound_id, sound_name,
                               (snd::SFXUserData*)dst);
}

void snd_SetSoundReg(s32 sound_handle, s32 which, u8 val) {
  snd::SetSoundReg(sound_handle, which, val);
}

void snd_SetGlobalExcite(u8 value) {
  snd::SetGlobalExcite(value);
}
