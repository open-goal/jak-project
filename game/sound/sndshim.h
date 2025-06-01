
#pragma once
#include "common/common_types.h"

constexpr int SND_CORE_0 = 1;
constexpr int SND_CORE_1 = 2;
constexpr int SD_REV_MODE_OFF = 0;

struct SFXUserData {
  u32 data[4];
};

typedef void* (*AllocFun)();
typedef void (*FreeFun)(void*);

namespace snd {
class SoundBank;
using BankHandle = SoundBank*;
};  // namespace snd

void snd_StartSoundSystem();
void snd_StopSoundSystem();
s32 snd_GetTick();
void snd_RegisterIOPMemAllocator(AllocFun alloc, FreeFun free);
int snd_LockVoiceAllocator(bool block);
void snd_UnlockVoiceAllocator();
s32 snd_ExternVoiceAlloc(s32 vol_group, s32 priority);
u32 snd_SRAMMalloc(u32 size);
void snd_SRAMMarkUsed(u32 addr, u32 size);
void snd_SetMixerMode(s32 channel_mode, s32 reverb_mode);
void snd_SetGroupVoiceRange(s32 group, s32 min, s32 max);
void snd_SetReverbDepth(s32 core, s32 left, s32 right);
void snd_SetReverbType(s32 core, s32 type);
void snd_SetPanTable(s16* table);
void snd_SetPlayBackMode(s32 mode);
s32 snd_SoundIsStillPlaying(s32 sound_handle);
void snd_StopSound(s32 sound_handle);
u32 snd_GetSoundID(s32 sound_handle);
void snd_SetSoundVolPan(s32 sound_handle, s32 vol, s32 pan);
void snd_SetMasterVolume(s32 which, s32 volume);
void snd_UnloadBank(snd::BankHandle bank_handle);
void snd_ResolveBankXREFS();
void snd_ContinueAllSoundsInGroup(u8 groups);
void snd_PauseAllSoundsInGroup(u8 groups);
void snd_SetMIDIRegister(s32 handle, u8 reg, u8 value);
void snd_SetGlobalExcite(u8 value);

s32 snd_PlaySoundVolPanPMPB(snd::BankHandle bank_handle,
                            s32 sound_id,
                            s32 vol,
                            s32 pan,
                            s32 pitch_mod,
                            s32 pitch_bend);

s32 snd_PlaySoundByNameVolPanPMPB(snd::BankHandle bank_handle,
                                  char* bank_name,
                                  char* sound_name,
                                  s32 vol,
                                  s32 pan,
                                  s32 pitch_mod,
                                  s32 pitch_bend);

void snd_SetSoundPitchModifier(s32 sound_handle, s32 pitch_mod);
void snd_SetSoundPitchBend(s32 sound_handle, s32 pitch_bend);
void snd_PauseSound(s32 sound_handle);
void snd_ContinueSound(s32 sound_handle);
void snd_AutoPitch(s32 sound_handle, s32 pitch, s32 delta_time, s32 delta_from);
void snd_AutoPitchBend(s32 sound_handle, s32 bend, s32 delta_time, s32 delta_from);
snd::BankHandle snd_BankLoadEx(const char* filepath,
                               s32 data_offset,
                               u32 spu_mem_loc,
                               u32 spu_mem_size);

void snd_BankLoadFromIOPPartialEx_Start();
void snd_BankLoadFromIOPPartialEx(const u8* data, u32 length, u32 spu_mem_loc, u32 spu_mem_size);
snd::BankHandle snd_BankLoadFromIOPPartialEx_Completion();

s32 snd_GetVoiceStatus(s32 voice);
s32 snd_GetFreeSPUDMA();
void snd_FreeSPUDMA(s32 channel);
void snd_keyOnVoiceRaw(u32 core, u32 voice);
void snd_keyOffVoiceRaw(u32 core, u32 voice);
s32 snd_GetSoundUserData(snd::BankHandle block_handle,
                         char* block_name,
                         s32 sound_id,
                         char* sound_name,
                         SFXUserData* dst);
void snd_SetSoundReg(s32 sound_handle, s32 which, u8 val);
