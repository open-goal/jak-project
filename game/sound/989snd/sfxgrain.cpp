#include "sfxgrain.h"

#include "blocksound_handler.h"
#include "lfo.h"

#include "common/log/log.h"

namespace snd {

SFXGrain_Tone::SFXGrain_Tone(SFXGrain& grain) : Grain(grain), m_tone(grain.GrainParams.tone) {}
SFXGrain_Tone::SFXGrain_Tone(SFXGrain2& grain, u8* data) : Grain(grain) {
  m_tone = *(Tone*)(data + (grain.OpcodeData.Opcode & 0xFFFFFF));
}

s32 SFXGrain_Tone::execute(blocksound_handler& handler) {
  handler.m_cur_volume =
      ((handler.m_app_volume * handler.m_orig_volume) >> 10) + handler.m_lfo_volume;
  handler.m_cur_volume = std::clamp<s32>(handler.m_cur_volume, 0, 127);

  handler.m_cur_pan = handler.m_app_pan + handler.m_lfo_pan;
  while (handler.m_cur_pan >= 360)
    handler.m_cur_pan -= 360;
  while (handler.m_cur_pan < 0)
    handler.m_cur_pan += 360;

  if ((m_tone.Flags & 8) != 0) {
    // Noise unsupported
    return 0;
  }

  auto voice = std::make_shared<blocksound_voice>(m_tone);

  s32 vol = m_tone.Vol;

  if (vol < 0) {
    if (vol >= -4) {
      vol = handler.m_registers.at(-vol - 1);
    } else if (vol == -5) {
      vol = rand() % 0x7f;
    } else {
      vol = g_block_reg.at(-vol - 6);
    }
  }

  vol = std::max(vol, 0);

  s32 pan = m_tone.Pan;
  if (pan < 0) {
    if (pan >= -4) {
      pan = 360 * handler.m_registers.at(-pan - 1) / 127;
    } else if (pan == -5) {
      pan = rand() % 360;
    } else {
      pan = 360 * g_block_reg.at(-pan - 6) / 127;
    }
  }

  while (pan >= 360)
    pan -= 360;
  while (pan < 0)
    pan += 360;

  voice->start_note = handler.m_note;
  voice->start_fine = handler.m_fine;
  voice->current_pb = handler.m_cur_pb;
  voice->current_pm = handler.m_cur_pm;
  voice->group = handler.m_group;
  voice->g_vol = vol;
  voice->g_pan = pan;

  voice->basevol =
      handler.m_vm.make_volume(127, 0, handler.m_cur_volume, handler.m_cur_pan, vol, pan);

  handler.m_vm.start_tone(voice, handler.m_bank.bank_id);
  handler.m_voices.emplace_front(voice);

  return 0;
}

SFXGrain_LfoSettings::SFXGrain_LfoSettings(SFXGrain& grain) : Grain(grain) {
  m_lfop = grain.GrainParams.lfo;
}
SFXGrain_LfoSettings::SFXGrain_LfoSettings(SFXGrain2& grain, u8* data) : Grain(grain) {
  m_lfop = *(LFOParams*)(data + (grain.OpcodeData.Opcode & 0xFFFFFF));
}
s32 SFXGrain_LfoSettings::execute(blocksound_handler& handler) {
  auto& lfo = handler.m_lfo.at(m_lfop.which_lfo);
  lfo.m_target = static_cast<lfo_target>(m_lfop.target);
  if (lfo.m_target != lfo_target::NONE) {
    lfo.m_type = static_cast<lfo_type>(m_lfop.shape);
    lfo.m_target_extra = m_lfop.target_extra;
    lfo.m_setup_flags = m_lfop.flags;
    lfo.m_depth = m_lfop.depth;
    lfo.m_orig_depth = m_lfop.depth;
    lfo.m_step_size = m_lfop.step_size;
    lfo.m_orig_step_size = m_lfop.step_size;
    lfo.m_state_hold1 = 0;
    lfo.m_last_lfo = 0;
    if (lfo.m_type == lfo_type::SQUARE) {
      lfo.m_state_hold1 = m_lfop.duty_cycle;
    }
    lfo.m_state_hold2 = 0;
    if ((lfo.m_setup_flags & 2) != 0) {
      lfo.m_next_step = (rand() & 0x7ff) << 16;
    } else {
      lfo.m_next_step = m_lfop.start_offset << 16;
    }

    // lg::info("starting LFO type {} for {}", magic_enum::enum_name(lfo.m_type),
    //          magic_enum::enum_name(lfo.m_target));
    lfo.init();
  } else {
    lfo.m_type = lfo_type::OFF;
  }

  return 0;
}

SFXGrain_StartChildSound::SFXGrain_StartChildSound(SFXGrain& grain) : Grain(grain) {
  m_psp = grain.GrainParams.play_sound;
}
SFXGrain_StartChildSound::SFXGrain_StartChildSound(SFXGrain2& grain, u8* data) : Grain(grain) {
  m_psp = *(PlaySoundParams*)(data + (grain.OpcodeData.Opcode & 0xFFFFFF));
}
s32 SFXGrain_StartChildSound::execute(blocksound_handler& handler) {
  s32 vol = m_psp.vol;
  if (vol < 0) {
    if (vol >= -4) {
      vol = handler.m_registers.at(-vol - 1);
    } else if (vol == -5) {
      vol = rand() % 0x7f;
    } else {
      vol = g_block_reg.at(-vol - 6);
    }
  }

  vol = std::clamp(std::abs(vol), 0, 127);

  s32 pan = m_psp.pan;
  if (pan < 0) {
    if (pan >= -4) {
      pan = 360 * std::min(std::abs(handler.m_registers.at(-pan - 1)), 127) / 127;
    } else if (pan == -5) {
      pan = rand() % 360;
    } else {
      pan = 360 * std::min(std::abs(g_block_reg.at(-pan - 6)), 127) / 127;
    }
  }

  SndPlayParams params{};
  params.vol = handler.m_app_volume * handler.m_orig_volume / 127;
  params.pan = handler.m_app_pan;
  params.pitch_mod = handler.m_app_pm;
  params.pitch_bend = handler.m_app_pb;
  params.registers = handler.m_registers;

  auto& block = static_cast<SoundBank&>(handler.bank());
  s32 index = m_psp.sound_id;

  if (index >= 0) {
    auto child_handler = block.make_handler(handler.m_vm, index, vol, pan, params);
    if (child_handler.has_value()) {
      handler.m_children.emplace_front(std::move(child_handler.value()));
    }

    return 0;
  }

  lg::error("indirect createchildsound");

  return 0;
}

SFXGrain_StopChildSound::SFXGrain_StopChildSound(SFXGrain& grain) : Grain(grain) {
  m_psp = grain.GrainParams.play_sound;
}
SFXGrain_StopChildSound::SFXGrain_StopChildSound(SFXGrain2& grain, u8* data) : Grain(grain) {
  m_psp = *(PlaySoundParams*)(data + (grain.OpcodeData.Opcode & 0xFFFFFF));
}
s32 SFXGrain_StopChildSound::execute(blocksound_handler& handler) {
  if (m_psp.sound_id >= 0) {
    for (auto it = handler.m_children.begin(); it != handler.m_children.end();) {
      auto* sound = static_cast<blocksound_handler*>(it->get());
      if (sound->m_sfx.index == m_psp.sound_id) {
        it = handler.m_children.erase(it);
      } else {
        ++it;
      }
    }

    return 0;
  }

  lg::error("indirect createchildsound");
  return 0;
}

SFXGrain_PluginMessage::SFXGrain_PluginMessage(SFXGrain& grain) : Grain(grain) {}
SFXGrain_PluginMessage::SFXGrain_PluginMessage(SFXGrain2& grain, u8* data) : Grain(grain) {}
s32 SFXGrain_PluginMessage::execute(blocksound_handler& handler) {
  // lg::warn("plugin message");
  //  TODO probably used
  return 0;
}

SFXGrain_Branch::SFXGrain_Branch(SFXGrain& grain) : Grain(grain) {}
SFXGrain_Branch::SFXGrain_Branch(SFXGrain2& grain, u8* data) : Grain(grain) {}
s32 SFXGrain_Branch::execute(blocksound_handler& handler) {
  return 0;
}

SFXGrain_LoopEnd::SFXGrain_LoopEnd(SFXGrain& grain) : Grain(grain) {}
SFXGrain_LoopEnd::SFXGrain_LoopEnd(SFXGrain2& grain, u8* data) : Grain(grain) {}
s32 SFXGrain_LoopEnd::execute(blocksound_handler& handler) {
  bool found = false;
  for (int i = handler.m_next_grain - 1; i >= 0 && !found; i--) {
    if (handler.m_sfx.grains[i]->type() == grain_type::LOOP_START) {
      handler.m_next_grain = i - 1;
      found = true;
    }
  }

  if (!found) {
    lg::error("LOOP_END could not find LOOP_START");
  }

  return 0;
}

SFXGrain_LoopContinue::SFXGrain_LoopContinue(SFXGrain& grain) : Grain(grain) {}
SFXGrain_LoopContinue::SFXGrain_LoopContinue(SFXGrain2& grain, u8* data) : Grain(grain) {}
s32 SFXGrain_LoopContinue::execute(blocksound_handler& handler) {
  bool found = false;
  for (int i = handler.m_next_grain + 1; i < (int)handler.m_sfx.grains.size() && !found; i++) {
    if (handler.m_sfx.grains[i]->type() == grain_type::LOOP_END) {
      handler.m_next_grain = i;
      found = true;
    }
  }

  if (!found) {
    lg::error("LOOP_CONTINUE could not find LOOP_END");
  }

  return 0;
}

SFXGrain_Stop::SFXGrain_Stop(SFXGrain& grain) : Grain(grain) {}
SFXGrain_Stop::SFXGrain_Stop(SFXGrain2& grain, u8* data) : Grain(grain) {}
s32 SFXGrain_Stop::execute(blocksound_handler& handler) {
  handler.m_done = true;

  return 0;
}

SFXGrain_RandPlay::SFXGrain_RandPlay(SFXGrain& grain) : Grain(grain) {
  options = grain.GrainParams.control.param[0];
  count = grain.GrainParams.control.param[1];
  previous = grain.GrainParams.control.param[2];
}

SFXGrain_RandPlay::SFXGrain_RandPlay(SFXGrain2& grain, u8* data) : Grain(grain) {
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

SFXGrain_RandDelay::SFXGrain_RandDelay(SFXGrain& grain) : Grain(grain) {
  m_max = grain.GrainParams.delay.Amount;
}

SFXGrain_RandDelay::SFXGrain_RandDelay(SFXGrain2& grain, u8* data) : Grain(grain) {
  m_max = (grain.OpcodeData.Opcode & 0xFFFFFF) + 1;
}
s32 SFXGrain_RandDelay::execute(blocksound_handler& handler) {
  return rand() % m_max;
}

SFXGrain_RandPB::SFXGrain_RandPB(SFXGrain& grain) : Grain(grain) {
  m_pb = grain.GrainParams.control.param[0];
}
SFXGrain_RandPB::SFXGrain_RandPB(SFXGrain2& grain, u8* data) : Grain(grain) {
  m_pb = grain.OpcodeData.arg[0];
}
s32 SFXGrain_RandPB::execute(blocksound_handler& handler) {
  s32 rnd = rand();
  handler.set_pbend(m_pb * ((0xffff * (rnd % 0x7fff)) / 0x7fff - 0x8000) / 100);

  return 0;
}

SFXGrain_PB::SFXGrain_PB(SFXGrain& grain) : Grain(grain) {
  m_pb = grain.GrainParams.control.param[0];
}
SFXGrain_PB::SFXGrain_PB(SFXGrain2& grain, u8* data) : Grain(grain) {
  m_pb = grain.OpcodeData.arg[0];
}
s32 SFXGrain_PB::execute(blocksound_handler& handler) {
  if (m_pb >= 0) {
    handler.set_pbend(0x7fff * m_pb / 127);
  } else {
    handler.set_pbend(-0x8000 * m_pb / -128);
  }

  return 0;
}

SFXGrain_AddPB::SFXGrain_AddPB(SFXGrain& grain) : Grain(grain) {
  m_pb = grain.GrainParams.control.param[0];
}
SFXGrain_AddPB::SFXGrain_AddPB(SFXGrain2& grain, u8* data) : Grain(grain) {
  m_pb = grain.OpcodeData.arg[0];
}
s32 SFXGrain_AddPB::execute(blocksound_handler& handler) {
  s32 new_pb = handler.m_cur_pb + 0x7fff * m_pb / 127;
  new_pb = std::clamp<s32>(new_pb, INT16_MIN, INT16_MAX);

  handler.set_pbend(new_pb);

  return 0;
}

SFXGrain_SetRegister::SFXGrain_SetRegister(SFXGrain& grain) : Grain(grain) {
  m_reg = grain.GrainParams.control.param[0];
  m_value = grain.GrainParams.control.param[1];
}
SFXGrain_SetRegister::SFXGrain_SetRegister(SFXGrain2& grain, u8* data) : Grain(grain) {
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

SFXGrain_SetRegisterRand::SFXGrain_SetRegisterRand(SFXGrain& grain) : Grain(grain) {
  m_reg = grain.GrainParams.control.param[0];
  m_lower_bound = grain.GrainParams.control.param[1];
  m_upper_bound = grain.GrainParams.control.param[2];
}
SFXGrain_SetRegisterRand::SFXGrain_SetRegisterRand(SFXGrain2& grain, u8* data) : Grain(grain) {
  m_reg = grain.OpcodeData.arg[0];
  m_lower_bound = grain.OpcodeData.arg[1];
  m_upper_bound = grain.OpcodeData.arg[2];
}
s32 SFXGrain_SetRegisterRand::execute(blocksound_handler& handler) {
  s32 range = m_upper_bound - m_lower_bound + 1;
  s32 rnd = (rand() % range) + m_lower_bound;
  if (m_reg < 0) {
    g_block_reg.at(-m_reg - 1) = rnd;
  } else {
    handler.m_registers.at(m_reg) = rnd;
  }

  return 0;
}

SFXGrain_IncRegister::SFXGrain_IncRegister(SFXGrain& grain) : Grain(grain) {
  m_reg = grain.GrainParams.control.param[0];
}
SFXGrain_IncRegister::SFXGrain_IncRegister(SFXGrain2& grain, u8* data) : Grain(grain) {
  m_reg = grain.OpcodeData.arg[0];
}
s32 SFXGrain_IncRegister::execute(blocksound_handler& handler) {
  if (m_reg < 0) {
    s32 new_val = g_block_reg.at(-m_reg - 1) + 1;
    g_block_reg.at(-m_reg - 1) = std::clamp<s32>(new_val, INT8_MIN, INT8_MAX);

  } else {
    s32 new_val = handler.m_registers.at(m_reg) + 1;
    handler.m_registers.at(m_reg) = std::clamp<s32>(new_val, INT8_MIN, INT8_MAX);
  }
  return 0;
}

SFXGrain_DecRegister::SFXGrain_DecRegister(SFXGrain& grain) : Grain(grain) {
  m_reg = grain.GrainParams.control.param[0];
}
SFXGrain_DecRegister::SFXGrain_DecRegister(SFXGrain2& grain, u8* data) : Grain(grain) {
  m_reg = grain.OpcodeData.arg[0];
}
s32 SFXGrain_DecRegister::execute(blocksound_handler& handler) {
  if (m_reg < 0) {
    s32 new_val = g_block_reg.at(-m_reg - 1) - 1;
    g_block_reg.at(-m_reg - 1) = std::clamp<s32>(new_val, INT8_MIN, INT8_MAX);

  } else {
    s32 new_val = handler.m_registers.at(m_reg) - 1;
    handler.m_registers.at(m_reg) = std::clamp<s32>(new_val, INT8_MIN, INT8_MAX);
  }

  return 0;
}

SFXGrain_TestRegister::SFXGrain_TestRegister(SFXGrain& grain) : Grain(grain) {
  m_reg = grain.GrainParams.control.param[0];
  m_action = grain.GrainParams.control.param[1];
  m_cmp = grain.GrainParams.control.param[2];
}
SFXGrain_TestRegister::SFXGrain_TestRegister(SFXGrain2& grain, u8* data) : Grain(grain) {
  m_reg = grain.OpcodeData.arg[0];
  m_action = grain.OpcodeData.arg[1];
  m_cmp = grain.OpcodeData.arg[2];
}
s32 SFXGrain_TestRegister::execute(blocksound_handler& handler) {
  s32 value;
  if (m_reg < 0) {
    value = g_block_reg[m_reg - 1];
  } else {
    value = handler.m_registers.at(m_reg);
  }

  if (m_action == 0) {
    if (value >= m_cmp) {
      handler.m_next_grain++;
    }
  } else if (m_action == 1) {
    if (value != m_cmp) {
      handler.m_next_grain++;
    }
  } else if (m_action >= 2) {
    if (m_cmp >= value)
      handler.m_next_grain++;
  }

  return 0;
}

SFXGrain_GotoMarker::SFXGrain_GotoMarker(SFXGrain& grain) : Grain(grain) {
  m_mark = grain.GrainParams.control.param[0];
}
SFXGrain_GotoMarker::SFXGrain_GotoMarker(SFXGrain2& grain, u8* data) : Grain(grain) {
  m_mark = grain.OpcodeData.arg[0];
}
s32 SFXGrain_GotoMarker::execute(blocksound_handler& handler) {
  bool found = false;
  for (int i = 0; i < (int)handler.m_sfx.grains.size() && !found; i++) {
    if (handler.m_sfx.grains.at(i)->type() == grain_type::MARKER) {
      if (static_cast<SFXGrain_Marker*>(handler.m_sfx.grains.at(i).get())->marker() == m_mark) {
        handler.m_next_grain = i - 1;
        found = true;
      }
    }
  }

  if (!found) {
    lg::error("GOTO_MARKER to non-existing marker");
  }

  return 0;
}

SFXGrain_GotoRandomMarker::SFXGrain_GotoRandomMarker(SFXGrain& grain) : Grain(grain) {
  m_lower_bound = grain.GrainParams.control.param[0];
  m_upper_bound = grain.GrainParams.control.param[1];
}
SFXGrain_GotoRandomMarker::SFXGrain_GotoRandomMarker(SFXGrain2& grain, u8* data) : Grain(grain) {
  m_lower_bound = grain.OpcodeData.arg[0];
  m_upper_bound = grain.OpcodeData.arg[1];
}
s32 SFXGrain_GotoRandomMarker::execute(blocksound_handler& handler) {
  bool found = false;
  s32 range = m_upper_bound - m_lower_bound + 1;
  s32 mark = (rand() % range) + m_lower_bound;

  for (int i = 0; i < (int)handler.m_sfx.grains.size() && !found; i++) {
    if (handler.m_sfx.grains.at(i)->type() == grain_type::MARKER) {
      if (static_cast<SFXGrain_Marker*>(handler.m_sfx.grains.at(i).get())->marker() == mark) {
        handler.m_next_grain = i - 1;
        found = true;
      }
    }
  }

  if (!found) {
    lg::error("GOTO_RANDOM_MARKER to non-existing marker");
  }

  return 0;
}

SFXGrain_WaitForAllVoices::SFXGrain_WaitForAllVoices(SFXGrain& grain) : Grain(grain) {}
SFXGrain_WaitForAllVoices::SFXGrain_WaitForAllVoices(SFXGrain2& grain, u8* data) : Grain(grain) {}
s32 SFXGrain_WaitForAllVoices::execute(blocksound_handler& handler) {
  if (!handler.m_voices.empty()) {
    handler.m_next_grain--;
    return 1;
  }

  return 0;
}

SFXGrain_PlayCycle::SFXGrain_PlayCycle(SFXGrain& grain) : Grain(grain) {
  m_group_size = grain.GrainParams.control.param[0];
  m_group_count = grain.GrainParams.control.param[1];
  m_index = grain.GrainParams.control.param[2];
}

SFXGrain_PlayCycle::SFXGrain_PlayCycle(SFXGrain2& grain, u8* data) : Grain(grain) {
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

SFXGrain_AddRegister::SFXGrain_AddRegister(SFXGrain& grain) : Grain(grain) {
  m_val = grain.GrainParams.control.param[0];
  m_reg = grain.GrainParams.control.param[1];
}
SFXGrain_AddRegister::SFXGrain_AddRegister(SFXGrain2& grain, u8* data) : Grain(grain) {
  m_val = grain.OpcodeData.arg[0];
  m_reg = grain.OpcodeData.arg[1];
}
s32 SFXGrain_AddRegister::execute(blocksound_handler& handler) {
  if (m_reg < 0) {
    s32 new_val = g_block_reg.at(-m_reg - 1) + m_val;
    g_block_reg.at(-m_reg - 1) = std::clamp<s32>(new_val, INT8_MIN, INT8_MAX);
  } else {
    s32 new_val = handler.m_registers.at(m_reg) + m_val;
    handler.m_registers.at(m_reg) = std::clamp<s32>(new_val, INT8_MIN, INT8_MAX);
  }
  return 0;
}

SFXGrain_KeyOffVoices::SFXGrain_KeyOffVoices(SFXGrain& grain) : Grain(grain) {}
SFXGrain_KeyOffVoices::SFXGrain_KeyOffVoices(SFXGrain2& grain, u8* data) : Grain(grain) {}
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

SFXGrain_KillVoices::SFXGrain_KillVoices(SFXGrain& grain) : Grain(grain) {}
SFXGrain_KillVoices::SFXGrain_KillVoices(SFXGrain2& grain, u8* data) : Grain(grain) {}
s32 SFXGrain_KillVoices::execute(blocksound_handler& handler) {
  for (auto& p : handler.m_voices) {
    auto v = p.lock();
    if (v == nullptr) {
      continue;
    }

    v->key_off();
    v->set_volume_l(0);
    v->set_volume_r(0);
  }

  return 0;
}

SFXGrain_OnStopMarker::SFXGrain_OnStopMarker(SFXGrain& grain) : Grain(grain) {}
SFXGrain_OnStopMarker::SFXGrain_OnStopMarker(SFXGrain2& grain, u8* data) : Grain(grain) {}
s32 SFXGrain_OnStopMarker::execute(blocksound_handler& handler) {
  handler.m_next_grain = handler.m_sfx.grains.size() - 1;
  return 0;
}

SFXGrain_CopyRegister::SFXGrain_CopyRegister(SFXGrain& grain) : Grain(grain) {
  m_src = grain.GrainParams.control.param[0];
  m_dst = grain.GrainParams.control.param[1];
}
SFXGrain_CopyRegister::SFXGrain_CopyRegister(SFXGrain2& grain, u8* data) : Grain(grain) {
  m_src = grain.OpcodeData.arg[0];
  m_dst = grain.OpcodeData.arg[1];
}
s32 SFXGrain_CopyRegister::execute(blocksound_handler& handler) {
  s8 value = 0;
  if (m_src < 0) {
    value = g_block_reg.at(-m_src - 1);
  } else {
    value = handler.m_registers.at(m_src);
  }

  if (m_dst < 0) {
    g_block_reg.at(-m_dst - 1) = value;
  } else {
    handler.m_registers.at(m_dst) = value;
  }

  return 0;
}

}  // namespace snd
