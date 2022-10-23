#include "sfxgrain.h"

#include "blocksound_handler.h"

#include "common/log/log.h"

namespace snd {

SFXGrain_Null::SFXGrain_Null(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_Null::SFXGrain_Null(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_Null::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_Tone::SFXGrain_Tone(SFXGrain& grain)
    : Grain(grain.Delay), m_tone(grain.GrainParams.tone) {}
SFXGrain_Tone::SFXGrain_Tone(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {
  m_tone = *(Tone*)(data + (grain.OpcodeData.Opcode & 0xFFFFFF));
}

s32 SFXGrain_Tone::execute(blocksound_handler& handler) {
  handler.m_cur_volume =
      ((handler.m_app_volume * handler.m_orig_volume) >> 10) + handler.m_lfo_volume;
  handler.m_cur_volume = std::clamp(handler.m_cur_volume, 0, 127);

  handler.m_cur_pan = handler.m_app_pan + handler.m_lfo_pan;
  while (handler.m_cur_pan >= 360)
    handler.m_cur_pan -= 360;
  while (handler.m_cur_pan < 0)
    handler.m_cur_pan += 360;

  if ((m_tone.Flags & 8) != 0) {
    // Noise unsupported
    return 0;
  }

  auto voice = std::make_shared<vag_voice>(m_tone);

  s8 vol = m_tone.Vol;
  if (m_tone.Vol < 0) {
    if (m_tone.Vol >= -4) {
      vol = g_block_reg.at(-m_tone.Vol - 1);
    } else if (m_tone.Vol == -5) {
      vol = rand() & 0x7f;
    } else {
      vol = handler.m_registers.at(-m_tone.Vol - 6);
    }
  }

  if (vol < 0) {
    vol = 0;
  }

  s16 pan = m_tone.Pan;
  if (m_tone.Pan < 0) {
    if (m_tone.Pan >= -4) {
      pan = g_block_reg.at(-m_tone.Pan - 1);
    } else if (m_tone.Pan == -5) {
      pan = rand() % 360;
    } else {
      pan = 360 * handler.m_registers.at(-m_tone.Pan - 6) / 127;
    }
  }

  while (pan >= 360)
    pan -= 360;
  while (pan < 0)
    pan += 360;

  voice->start_note = handler.m_note;
  voice->start_fine = handler.m_fine;
  voice->group = handler.m_group;

  voice->basevol =
      handler.m_vm.make_volume(127, 0, handler.m_cur_volume, handler.m_cur_pan, vol, pan);

  handler.m_vm.start_tone(voice, handler.m_bank);
  handler.m_voices.emplace_front(voice);

  return 0;
}

SFXGrain_XrefID::SFXGrain_XrefID(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_XrefID::SFXGrain_XrefID(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_XrefID::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_XrefNum::SFXGrain_XrefNum(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_XrefNum::SFXGrain_XrefNum(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_XrefNum::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_LfoSettings::SFXGrain_LfoSettings(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_LfoSettings::SFXGrain_LfoSettings(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_LfoSettings::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_StartChildSound::SFXGrain_StartChildSound(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_StartChildSound::SFXGrain_StartChildSound(SFXGrain2& grain, u8* data)
    : Grain(grain.Delay) {}
s32 SFXGrain_StartChildSound::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_StopChildSound::SFXGrain_StopChildSound(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_StopChildSound::SFXGrain_StopChildSound(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_StopChildSound::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_PluginMessage::SFXGrain_PluginMessage(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_PluginMessage::SFXGrain_PluginMessage(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_PluginMessage::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_Branch::SFXGrain_Branch(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_Branch::SFXGrain_Branch(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_Branch::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_ControlNull::SFXGrain_ControlNull(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_ControlNull::SFXGrain_ControlNull(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_ControlNull::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_LoopStart::SFXGrain_LoopStart(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_LoopStart::SFXGrain_LoopStart(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_LoopStart::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_LoopEnd::SFXGrain_LoopEnd(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_LoopEnd::SFXGrain_LoopEnd(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_LoopEnd::execute(blocksound_handler& handler) {
  for (int i = handler.m_next_grain - 1; i >= 0; i--) {
  }

  return 0;
}

SFXGrain_LoopContinue::SFXGrain_LoopContinue(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_LoopContinue::SFXGrain_LoopContinue(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_LoopContinue::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_Stop::SFXGrain_Stop(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_Stop::SFXGrain_Stop(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_Stop::execute(blocksound_handler& handler) {
  handler.m_done = true;

  return 0;
}

SFXGrain_RandPlay::SFXGrain_RandPlay(SFXGrain& grain) : Grain(grain.Delay) {
  options = grain.GrainParams.control.param[0];
  count = grain.GrainParams.control.param[1];
  previous = grain.GrainParams.control.param[2];
}

SFXGrain_RandPlay::SFXGrain_RandPlay(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {
  options = grain.OpcodeData.arg[0];
  count = grain.OpcodeData.arg[1];
  previous = grain.OpcodeData.arg[2];
}

s32 SFXGrain_RandPlay::execute(blocksound_handler& handler) {
  int rnd = rand() % options;
  if (rnd == previous) {
    rnd++;
    if (rnd >= options) {
      rnd = 0;
    }
  }

  previous = rnd;
  handler.m_next_grain += rnd * count;
  handler.m_grains_to_play = count + 1;
  handler.m_grains_to_skip = (options - 1 - rnd) * count;
  handler.m_skip_grains = true;
  return 0;
}

SFXGrain_RandDelay::SFXGrain_RandDelay(SFXGrain& grain) : Grain(grain.Delay) {
  m_max = grain.GrainParams.delay.Amount;
}

SFXGrain_RandDelay::SFXGrain_RandDelay(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {
  m_max = (grain.OpcodeData.Opcode & 0xFFFFFF) + 1;
}
s32 SFXGrain_RandDelay::execute(blocksound_handler& handler) {
  return rand() % m_max;
}

SFXGrain_RandPB::SFXGrain_RandPB(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_RandPB::SFXGrain_RandPB(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_RandPB::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_PB::SFXGrain_PB(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_PB::SFXGrain_PB(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_PB::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_AddPB::SFXGrain_AddPB(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_AddPB::SFXGrain_AddPB(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_AddPB::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_SetRegister::SFXGrain_SetRegister(SFXGrain& grain) : Grain(grain.Delay) {
  m_reg = grain.GrainParams.control.param[0];
  m_value = grain.GrainParams.control.param[1];
}
SFXGrain_SetRegister::SFXGrain_SetRegister(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {
  m_reg = grain.OpcodeData.arg[0];
  m_value = grain.OpcodeData.arg[1];
}
s32 SFXGrain_SetRegister::execute(blocksound_handler& handler) {
  if (m_reg < 0) {
    g_block_reg.at(-m_reg - 1) = m_value;
  } else {
    handler.m_registers.at(m_reg) = m_value;
  }

  return 0;
}

SFXGrain_SetRegisterRand::SFXGrain_SetRegisterRand(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_SetRegisterRand::SFXGrain_SetRegisterRand(SFXGrain2& grain, u8* data)
    : Grain(grain.Delay) {}
s32 SFXGrain_SetRegisterRand::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_IncRegister::SFXGrain_IncRegister(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_IncRegister::SFXGrain_IncRegister(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_IncRegister::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_DecRegister::SFXGrain_DecRegister(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_DecRegister::SFXGrain_DecRegister(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_DecRegister::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_TestRegister::SFXGrain_TestRegister(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_TestRegister::SFXGrain_TestRegister(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_TestRegister::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_Marker::SFXGrain_Marker(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_Marker::SFXGrain_Marker(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_Marker::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_GotoMarker::SFXGrain_GotoMarker(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_GotoMarker::SFXGrain_GotoMarker(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_GotoMarker::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_GotoRandomMarker::SFXGrain_GotoRandomMarker(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_GotoRandomMarker::SFXGrain_GotoRandomMarker(SFXGrain2& grain, u8* data)
    : Grain(grain.Delay) {}
s32 SFXGrain_GotoRandomMarker::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_WaitForAllVoices::SFXGrain_WaitForAllVoices(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_WaitForAllVoices::SFXGrain_WaitForAllVoices(SFXGrain2& grain, u8* data)
    : Grain(grain.Delay) {}
s32 SFXGrain_WaitForAllVoices::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_PlayCycle::SFXGrain_PlayCycle(SFXGrain& grain) : Grain(grain.Delay) {
  m_group_size = grain.GrainParams.control.param[0];
  m_group_count = grain.GrainParams.control.param[1];
  m_index = grain.GrainParams.control.param[2];
}

SFXGrain_PlayCycle::SFXGrain_PlayCycle(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {
  m_group_size = grain.OpcodeData.arg[0];
  m_group_count = grain.OpcodeData.arg[1];
  m_index = grain.OpcodeData.arg[2];
}

s32 SFXGrain_PlayCycle::execute(blocksound_handler& handler) {
  auto a = m_index++;
  if (m_index == m_group_size) {
    m_index = 0;
  }

  handler.m_next_grain += m_group_count * a;
  handler.m_grains_to_play = m_group_count + 1;
  handler.m_grains_to_skip = (m_group_size - 1 - a) * m_group_count;
  handler.m_skip_grains = true;
  return 0;
}

SFXGrain_AddRegister::SFXGrain_AddRegister(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_AddRegister::SFXGrain_AddRegister(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_AddRegister::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_KeyOffVoices::SFXGrain_KeyOffVoices(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_KeyOffVoices::SFXGrain_KeyOffVoices(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_KeyOffVoices::execute(blocksound_handler& handler) {
  for (auto& p : handler.m_voices) {
    auto v = p.lock();
    if (v == nullptr) {
      continue;
    }

    v->key_off();
  }
  return 0;
}

SFXGrain_KillVoices::SFXGrain_KillVoices(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_KillVoices::SFXGrain_KillVoices(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_KillVoices::execute(blocksound_handler& handler) {
  for (auto& p : handler.m_voices) {
    auto v = p.lock();
    if (v == nullptr) {
      continue;
    }

    v->key_off();
  }

  return 0;
}

SFXGrain_OnStopMarker::SFXGrain_OnStopMarker(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_OnStopMarker::SFXGrain_OnStopMarker(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_OnStopMarker::execute(blocksound_handler& handler) {
  handler.m_next_grain = handler.m_sfx.grains.size() - 1;
  return 0;
}

SFXGrain_CopyRegister::SFXGrain_CopyRegister(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_CopyRegister::SFXGrain_CopyRegister(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
s32 SFXGrain_CopyRegister::execute(blocksound_handler& handler) {
  return 0;
}

}  // namespace snd
