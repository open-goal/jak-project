#include "sndshim.h"

#include <cstdio>

#include "sdshim.h"

#include "989snd/player.h"

std::unique_ptr<snd::player> player;

void snd_StartSoundSystem() {
  player = std::make_unique<snd::player>();

  voice = std::make_shared<snd::voice>(snd::voice::AllocationType::permanent);
  voice->set_sample((u16*)spu_memory);
  player->submit_voice(voice);
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
  // spu memory currently hardcoded
  return 0;
}

void snd_SetMixerMode(s32, s32) {}

void snd_SetGroupVoiceRange(s32, s32, s32) {}

void snd_SetReverbDepth(s32, s32, s32) {}

void snd_SetReverbType(s32, s32) {}

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

void snd_StopSound(s32 handle) {
  if (player) {
    player->stop_sound(handle);
  }
}

void snd_SetSoundVolPan(s32 sound_handle, s32 vol, s32 pan) {
  if (player) {
    player->set_sound_vol_pan(sound_handle, vol, pan);
  }
}

void snd_SetMasterVolume(s32 group, s32 volume) {
  if (player) {
    player->set_master_volume(group, volume);
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

void snd_ContinueAllSoundsInGroup(u8 group) {
  if (player) {
    player->continue_all_sounds_in_group(group);
  }
}

void snd_PauseAllSoundsInGroup(u8 group) {
  if (player) {
    player->pause_all_sounds_in_group(group);
  }
}

void snd_SetMIDIRegister(s32 sound_handle, u8 reg, u8 value) {
  if (player) {
    player->set_midi_reg(sound_handle, reg, value);
  }
}

s32 snd_PlaySoundVolPanPMPB(s32 bank, s32 sound, s32 vol, s32 pan, s32 pm, s32 pb) {
  if (player) {
    return player->play_sound(bank, sound, vol, pan, pm, pb);
  } else {
    return 0;
  }
}

void snd_SetSoundPitchModifier(s32 sound, s32 mod) {
  if (player) {
    player->set_sound_pmod(sound, mod);
  }
}

void snd_SetSoundPitchBend(s32 sound, s32 bend) {
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

void snd_AutoPitch(s32, s32, s32, s32) {
  // TODO
  printf("snd_AutoPitch\n");
}
void snd_AutoPitchBend(s32, s32, s32, s32) {
  // TODO
  printf("snd_AutoPitchBend\n");
}

s32 snd_BankLoadEx(const char* filename, s32 offset, s32, s32) {
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
  if (voice) {
    voice->key_on();
  }
}

void snd_keyOffVoiceRaw(u32 core, u32 voice_id) {
  if (voice) {
    voice->key_off();
  }
}
