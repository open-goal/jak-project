#include "sndshim.h"

#include <cstdio>

#include "sdshim.h"

#include "989snd/player.h"

std::unique_ptr<snd::player> player;

void snd_StartSoundSystem() {
  player = std::make_unique<snd::player>();

  for (auto& voice : voices) {
    voice = std::make_shared<snd::voice>(snd::voice::AllocationType::permanent);
    voice->set_sample((u16*)spu_memory);
    player->submit_voice(voice);
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
    return player->get_tick();
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
    player->set_pan_table((snd::vol_pair*)table);
  }
}

void snd_SetPlayBackMode(s32 mode) {
  if (player) {
    player->set_playback_mode(mode);
  }
}

s32 snd_SoundIsStillPlaying(s32 sound_handle) {
  if (player) {
    if (player->sound_still_active(sound_handle)) {
      return sound_handle;
    }
  }

  return 0;
}

void snd_StopSound(s32 sound_handle) {
  if (player) {
    player->stop_sound(sound_handle);
  }
}

void snd_SetSoundVolPan(s32 sound_handle, s32 vol, s32 pan) {
  if (player) {
    player->set_sound_vol_pan(sound_handle, vol, pan);
  }
}

void snd_SetMasterVolume(s32 which, s32 volume) {
  if (player) {
    player->set_master_volume(which, volume);
  }
}

void snd_UnloadBank(s32 bank_handle) {
  if (player) {
    player->unload_bank(bank_handle);
  }
}

void snd_ResolveBankXREFS() {
  // Currently no-op, idk if we'd ever need it
}

void snd_ContinueAllSoundsInGroup(u8 groups) {
  if (player) {
    player->continue_all_sounds_in_group(groups);
  }
}

void snd_PauseAllSoundsInGroup(u8 groups) {
  if (player) {
    player->pause_all_sounds_in_group(groups);
  }
}

void snd_SetMIDIRegister(s32 sound_handle, u8 reg, u8 value) {
  if (player) {
    player->set_sound_reg(sound_handle, reg, value);
  }
}

s32 snd_PlaySoundVolPanPMPB(s32 bank, s32 sound, s32 vol, s32 pan, s32 pitch_mod, s32 pitch_bend) {
  if (player) {
    return player->play_sound(bank, sound, vol, pan, pitch_mod, pitch_bend);
  } else {
    return 0;
  }
}

s32 snd_PlaySoundByNameVolPanPMPB(s32 bank_handle,
                                  char* bank_name,
                                  char* sound_name,
                                  s32 vol,
                                  s32 pan,
                                  s32 pitch_mod,
                                  s32 pitch_bend) {
  if (player) {
    return player->play_sound_by_name(bank_handle, bank_name, sound_name, vol, pan, pitch_mod,
                                      pitch_bend);
  } else {
    return 0;
  }
}

void snd_SetSoundPitchModifier(s32 sound_handle, s32 pitch_mod) {
  if (player) {
    player->set_sound_pmod(sound_handle, pitch_mod);
  }
}

void snd_SetSoundPitchBend(s32 sound_handle, s32 bend) {
  // TODO
  if (bend != 0) {
  }
}

void snd_PauseSound(s32 sound_handle) {
  if (player) {
    player->pause_sound(sound_handle);
  }
}

void snd_ContinueSound(s32 sound_handle) {
  if (player) {
    player->continue_sound(sound_handle);
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

s32 snd_BankLoadEx(const char* filename, s32 offset, u32 spu_mem_loc, u32 spu_mem_size) {
  // printf("snd_BankLoadEx\n");
  if (player) {
    fs::path path = filename;
    return player->load_bank(path, offset);
  } else {
    return 0;
  }
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
    voices[0]->key_on();
  }
}

void snd_keyOffVoiceRaw(u32 core, u32 voice_id) {
  if (voices[0]) {
    voices[0]->key_off();
  }
}

s32 snd_GetSoundUserData(s32 block_handle,
                         char* block_name,
                         s32 sound_id,
                         char* sound_name,
                         SFXUserData* dst) {
  if (player) {
    return player->get_sound_user_data(block_handle, block_name, sound_id, sound_name,
                                       (snd::SFXUserData*)dst);
  }
  return 0;
}

void snd_SetSoundReg(s32 sound_handle, s32 which, u8 val) {
  if (player) {
    player->set_sound_reg(sound_handle, which, val);
  }
}

void snd_SetGlobalExcite(u8 value) {
  if (player) {
    player->set_global_excite(value);
  }
}
