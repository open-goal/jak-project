// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#pragma once

#include <memory>
#include <mutex>
#include <unordered_map>
#include <vector>

#include "ame_handler.h"
#include "handle_allocator.h"
#include "sound_handler.h"

#include "common/common_types.h"

#include "../common/synth.h"

#include "third-party/cubeb/cubeb/include/cubeb/cubeb.h"
#include "third-party/span.hpp"

namespace snd {

BankHandle LoadBank(nonstd::span<u8> bank);

void StartSoundSystem();
void StopSoundSystem();
u32 PlaySound(BankHandle bank, u32 sound, s32 vol, s32 pan, s32 pm, s32 pb);
u32 PlaySoundByName(BankHandle bank,
                    char* bank_name,
                    char* sound_name,
                    s32 vol,
                    s32 pan,
                    s32 pm,
                    s32 pb);
void SetSoundReg(u32 sound_id, u8 reg, u8 value);
void SetGlobalExcite(u8 value);
bool SoundStillActive(u32 sound_id);
void SetMasterVolume(u32 group, s32 volume);
void UnloadBank(BankHandle bank_handle);
void StopSound(u32 sound_handle);
void SetPanTable(VolPair* pantable);
void SetPlaybackMode(s32 mode);
void PauseSound(s32 sound_handle);
void ContinueSound(s32 sound_handle);
void PauseAllSoundsInGroup(u8 group);
void ContinueAllSoundsInGroup(u8 group);
void SetSoundVolPan(s32 sound_handle, s32 vol, s32 pan);
void SubmitVoice(std::shared_ptr<Voice>& voice);
void SetSoundPmod(s32 sound_handle, s32 mod);
void InitCubeb();
void DestroyCubeb();
s32 GetTick();
void StopAllSounds();
s32 GetSoundUserData(BankHandle block_handle,
                     char* block_name,
                     s32 sound_id,
                     char* sound_name,
                     SFXUserData* dst);

extern std::recursive_mutex gTickLock;

}  // namespace snd
