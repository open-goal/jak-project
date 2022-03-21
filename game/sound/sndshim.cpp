#include "sndshim.h"
#include "989snd/player.h"
#include <cstdio>

std::unique_ptr<snd::player> player;
void snd_StartSoundSystem() {
  player = std::make_unique<snd::player>();
}
void snd_StopSoundSystem() {
  player.reset();
}

void snd_RegisterIOPMemAllocator(AllocFun, FreeFun) {
  // printf("snd_RegisterIOPMemAllocator\n");
}
void snd_LockVoiceAllocator(s32) {
  // printf("snd_LockVoiceAllocator\n");
}
void snd_UnlockVoiceAllocator() {
  // printf("snd_UnlockVoiceAllocator\n");
}
s32 snd_ExternVoiceVoiceAlloc(s32, s32) {
  // printf("snd_ExternVoiceVoiceAlloc\n");
  return 0;
}
u32 snd_SRAMMalloc(u32) {
  // printf("snd_SRAMMalloc\n");
  return 0;
}
void snd_SetMixerMode(s32, s32) {
  printf("snd_SetMixerMode\n");
}
void snd_SetGroupVoiceRange(s32, s32, s32) {
  printf("snd_SetGroupVoiceRange\n");
}
void snd_SetReverbDepth(s32, s32, s32) {
  printf("snd_SetReverbDepth\n");
}
void snd_SetReverbType(s32, s32) {
  printf("snd_SetReverbType\n");
}
void snd_SetPanTable(s16*) {
  printf("snd_SetPanTable\n");
}
void snd_SetPlayBackMode(s32) {
  printf("snd_SetPlayBackMode\n");
}
s32 snd_SoundIsStillPlaying(s32 sound_handle) {
  if (player->sound_still_active(sound_handle)) {
    return sound_handle;
  }

  return 0;
}
void snd_StopSound(s32) {
  printf("snd_StopSound\n");
}
void snd_SetSoundVolPan(s32, s32, s32) {
  // printf("snd_SetSoundVolPan\n");
}
void snd_SetMasterVolume(s32 group, s32 volume) {
  // printf("setting group %d to %d\n", group, volume);

  player->set_master_volume(group, volume);
}
void snd_UnloadBank(s32 bank_handle) {
  player->unload_bank(bank_handle);
}
void snd_ResolveBankXREFS() {
  printf("snd_ResolveBankXREFS\n");
}
void snd_ContinueAllSoundsInGroup(u8) {
  printf("snd_ContinueAllSoundsInGroup\n");
}
void snd_PauseAllSoundsInGroup(u8) {
  printf("snd_PauseAllSoundsInGroup\n");
}

void snd_SetMIDIRegister(s32 sound_handle, u8 reg, u8 value) {
  player->set_midi_reg(sound_handle, reg, value);
}

s32 snd_PlaySoundVolPanPMPB(s32 bank, s32 sound, s32 vol, s32 pan, s32 pm, s32 pb) {
  return player->play_sound(bank, sound);
}
void snd_SetSoundPitchModifier(s32, s32) {
  printf("snd_SetSoundPitchModifier\n");
}
void snd_SetSoundPitchBend(s32, s32) {
  printf("snd_SetSoundPitchBend\n");
}
void snd_PauseSound(s32) {
  printf("snd_PauseSound\n");
}
void snd_ContinueSound(s32) {
  printf("snd_ContinueSound\n");
}
void snd_AutoPitch(s32, s32, s32, s32) {
  printf("snd_AutoPitch\n");
}
void snd_AutoPitchBend(s32, s32, s32, s32) {
  printf("snd_AutoPitchBend\n");
}

s32 snd_BankLoadEx(const char* filename, s32 offset, s32, s32) {
  // printf("snd_BankLoadEx\n");
  std::filesystem::path path = filename;
  return player->load_bank(path, offset);
}
