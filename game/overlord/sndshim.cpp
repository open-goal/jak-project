#include "sndshim.h"

void snd_StartSoundSystem() {}
void snd_RegisterIOPMemAllocator(AllocFun, FreeFun) {}
void snd_LockVoiceAllocator(s32) {}
void snd_UnlockVoiceAllocator() {}
s32 snd_ExternVoiceVoiceAlloc(s32, s32) {
  return 0;
}
u32 snd_SRAMMalloc(u32) {
  return 0;
}
void snd_SetMixerMode(s32, s32) {}
void snd_SetGroupVoiceRange(s32, s32, s32) {}
void snd_SetReverbDepth(s32, s32, s32) {}
void snd_SetReverbType(s32, s32) {}
void snd_SetPanTable(s16*) {}
void snd_SetPlayBackMode(s32) {}
s32 snd_SoundIsStillPlaying(s32) {
  return 0;
}
void snd_StopSound(s32) {}
void snd_SetSoundVolPan(s32, s32, s32) {}
void snd_SetMasterVolume(s32, s32) {}
