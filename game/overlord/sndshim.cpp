#include "sndshim.h"
#include <cstdio>

void snd_StartSoundSystem() {
  // printf("snd_StartSoundSystem\n");
}
void snd_StopSoundSystem() {
  // printf("snd_StopSoundSystem\n");
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
  // printf("snd_SetMixerMode\n");
}
void snd_SetGroupVoiceRange(s32, s32, s32) {
  // printf("snd_SetGroupVoiceRange\n");
}
void snd_SetReverbDepth(s32, s32, s32) {
  // printf("snd_SetReverbDepth\n");
}
void snd_SetReverbType(s32, s32) {
  // printf("snd_SetReverbType\n");
}
void snd_SetPanTable(s16*) {
  // printf("snd_SetPanTable\n");
}
void snd_SetPlayBackMode(s32) {
  // printf("snd_SetPlayBackMode\n");
}
s32 snd_SoundIsStillPlaying(s32) {
  // printf("snd_SoundIsStillPlaying\n");
  return 0;
}
void snd_StopSound(s32) {
  // printf("snd_StopSound\n");
}
void snd_SetSoundVolPan(s32, s32, s32) {
  // printf("snd_SetSoundVolPan\n");
}
void snd_SetMasterVolume(s32, s32) {
  // printf("snd_SetMasterVolume\n");
}
void snd_UnloadBank(s32) {
  // printf("snd_UnloadBank\n");
}
void snd_ResolveBankXREFS() {
  // printf("snd_ResolveBankXREFS\n");
}
void snd_ContinueAllSoundsInGroup(u8) {
  // printf("snd_ContinueAllSoundsInGroup\n");
}
void snd_PauseAllSoundsInGroup(u8) {
  // printf("snd_PauseAllSoundsInGroup\n");
}

void snd_SetMIDIRegister(s32, u8, u8) {
  // printf("snd_SetMIDIRegister\n");
}

s32 snd_PlaySoundVolPanPMPB(s32, s32, s32, s32, s32, s32) {
  // printf("snd_PlaySoundVolPanPMPB\n");
  return 0;
}
void snd_SetSoundPitchModifier(s32, s32) {
  // printf("snd_SetSoundPitchModifier\n");
}
void snd_SetSoundPitchBend(s32, s32) {
  // printf("snd_SetSoundPitchBend\n");
}
void snd_PauseSound(s32) {
  // printf("snd_PauseSound\n");
}
void snd_ContinueSound(s32) {
  // printf("snd_ContinueSound\n");
}
void snd_AutoPitch(s32, s32, s32, s32) {
  // printf("snd_AutoPitch\n");
}
void snd_AutoPitchBend(s32, s32, s32, s32) {
  // printf("snd_AutoPitchBend\n");
}

s32 snd_BankLoadEx(const char*, s32, s32, s32) {
  // printf("snd_BankLoadEx\n");
  return 0;
}
