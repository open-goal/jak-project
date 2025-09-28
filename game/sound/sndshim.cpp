#include "sndshim.h"

#include <cstdio>

#include "sdshim.h"

#include "common/log/log.h"
#include "common/util/FileUtil.h"

#include "989snd/player.h"

std::unique_ptr<snd::Player> player;

void snd_StartSoundSystem() {
  player = std::make_unique<snd::Player>();

  for (auto& voice : voices) {
    voice = std::make_shared<snd::Voice>(snd::Voice::AllocationType::Permanent);
    voice->SetSample((u16*)spu_memory);
    player->SubmitVoice(voice);
  }
}

void snd_StopSoundSystem() {
  if (player) {
    player.reset();
  }
}

// dma is always instant, allocation not required
s32 snd_GetFreeSPUDMA() {
  return 0;
}

void snd_FreeSPUDMA([[maybe_unused]] s32 channel) {}

s32 snd_GetTick() {
  if (player) {
    return player->GetTick();
  } else {
    return 0;
  }
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
  if (player) {
    player->SetPanTable((snd::VolPair*)table);
  }
}

void snd_SetPlayBackMode(s32 mode) {
  if (player) {
    player->SetPlaybackMode(mode);
  }
}

s32 snd_SoundIsStillPlaying(s32 sound_handle) {
  if (player) {
    if (player->SoundStillActive(sound_handle)) {
      return sound_handle;
    }
  }

  return 0;
}

void snd_StopSound(s32 sound_handle) {
  if (player) {
    player->StopSound(sound_handle);
  }
}

u32 snd_GetSoundID(s32 sound_handle) {
  if (player) {
    return player->GetSoundID(sound_handle);
  } else {
    return -1;
  }
}

void snd_SetSoundVolPan(s32 sound_handle, s32 vol, s32 pan) {
  if (player) {
    player->SetSoundVolPan(sound_handle, vol, pan);
  }
}

void snd_SetMasterVolume(s32 which, s32 volume) {
  if (player) {
    player->SetMasterVolume(which, volume);
  }
}

void snd_UnloadBank(snd::BankHandle bank_handle) {
  if (player) {
    player->UnloadBank(bank_handle);
  }
}

void snd_ResolveBankXREFS() {
  // Currently no-op, idk if we'd ever need it
}

void snd_ContinueAllSoundsInGroup(u8 groups) {
  if (player) {
    player->ContinueAllSoundsInGroup(groups);
  }
}

void snd_PauseAllSoundsInGroup(u8 groups) {
  if (player) {
    player->PauseAllSoundsInGroup(groups);
  }
}

void snd_SetMIDIRegister(s32 sound_handle, u8 reg, u8 value) {
  if (player) {
    player->SetSoundReg(sound_handle, reg, value);
  }
}

s32 snd_PlaySoundVolPanPMPB(snd::BankHandle bank,
                            s32 sound,
                            s32 vol,
                            s32 pan,
                            s32 pitch_mod,
                            s32 pitch_bend) {
  if (player) {
    return player->PlaySound(bank, sound, vol, pan, pitch_mod, pitch_bend);
  } else {
    return 0;
  }
}

s32 snd_PlaySoundByNameVolPanPMPB(snd::BankHandle bank_handle,
                                  char* bank_name,
                                  char* sound_name,
                                  s32 vol,
                                  s32 pan,
                                  s32 pitch_mod,
                                  s32 pitch_bend) {
  if (player) {
    return player->PlaySoundByName(bank_handle, bank_name, sound_name, vol, pan, pitch_mod,
                                   pitch_bend);
  } else {
    return 0;
  }
}

void snd_SetSoundPitchModifier(s32 sound_handle, s32 pitch_mod) {
  if (player) {
    player->SetSoundPmod(sound_handle, pitch_mod);
  }
}

void snd_SetSoundPitchBend(s32 sound_handle, s32 bend) {
  // TODO
  if (bend != 0) {
  }
}

void snd_PauseSound(s32 sound_handle) {
  if (player) {
    player->PauseSound(sound_handle);
  }
}

void snd_ContinueSound(s32 sound_handle) {
  if (player) {
    player->ContinueSound(sound_handle);
  }
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
  // printf("snd_BankLoadEx\n");
  if (player) {
    // TODO put the load on the thread pool?
    auto file_buf = file_util::read_binary_file(std::string(filename));
    return player->LoadBank(std::span(file_buf).subspan(offset));
  } else {
    return 0;
  }
}

namespace {
bool started = false;
std::vector<u8> sbk_data;
}  // namespace

void snd_BankLoadFromIOPPartialEx_Start() {
  started = true;
  sbk_data.clear();
}

void snd_BankLoadFromIOPPartialEx(const u8* data, u32 length, u32 spu_mem_loc, u32 spu_mem_size) {
  sbk_data.insert(sbk_data.end(), data, data + length);
}
snd::BankHandle snd_BankLoadFromIOPPartialEx_Completion() {
  ASSERT(started);
  started = false;
  auto ret = player->LoadBank(std::span(sbk_data));
  sbk_data.clear();
  return ret;
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
  if (player) {
    return player->GetSoundUserData(block_handle, block_name, sound_id, sound_name,
                                    (snd::SFXUserData*)dst);
  }
  return 0;
}

void snd_SetSoundReg(s32 sound_handle, s32 which, u8 val) {
  if (player) {
    player->SetSoundReg(sound_handle, which, val);
  }
}

void snd_SetGlobalExcite(u8 value) {
  if (player) {
    player->SetGlobalExcite(value);
  }
}
