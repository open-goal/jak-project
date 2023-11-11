#include "sfxgrain.h"

#include "blocksound_handler.h"
#include "lfo.h"

#include "common/log/log.h"

namespace snd {

s32 Grain::snd_SFX_GRAIN_TYPE_NULL(blocksound_handler& handler) {
  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_TONE(blocksound_handler& handler) {
  auto& tone = std::get<Tone>(data);

  handler.m_cur_volume =
      ((handler.m_app_volume * handler.m_orig_volume) >> 10) + handler.m_lfo_volume;
  handler.m_cur_volume = std::clamp<s32>(handler.m_cur_volume, 0, 127);

  handler.m_cur_pan = handler.m_app_pan + handler.m_lfo_pan;
  while (handler.m_cur_pan >= 360)
    handler.m_cur_pan -= 360;
  while (handler.m_cur_pan < 0)
    handler.m_cur_pan += 360;

  if ((tone.Flags & 8) != 0) {
    // Noise unsupported
    return 0;
  }

  auto voice = std::make_shared<blocksound_voice>(tone);

  s32 vol = tone.Vol;

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

  s32 pan = tone.Pan;
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

  handler.m_vm.start_tone(voice);
  handler.m_voices.emplace_front(voice);

  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_XREF_ID(blocksound_handler& handler) {
  return 0;
}
s32 Grain::snd_SFX_GRAIN_TYPE_XREF_NUM(blocksound_handler& handler) {
  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_LFO_SETTINGS(blocksound_handler& handler) {
  auto lfop = std::get<LFOParams>(data);
  auto& lfo = handler.m_lfo.at(lfop.which_lfo);

  lfo.m_target = static_cast<lfo_target>(lfop.target);
  if (lfo.m_target != lfo_target::NONE) {
    lfo.m_type = static_cast<lfo_type>(lfop.shape);
    lfo.m_target_extra = lfop.target_extra;
    lfo.m_setup_flags = lfop.flags;
    lfo.m_depth = lfop.depth;
    lfo.m_orig_depth = lfop.depth;
    lfo.m_step_size = lfop.step_size;
    lfo.m_orig_step_size = lfop.step_size;
    lfo.m_state_hold1 = 0;
    lfo.m_last_lfo = 0;
    if (lfo.m_type == lfo_type::SQUARE) {
      lfo.m_state_hold1 = lfop.duty_cycle;
    }
    lfo.m_state_hold2 = 0;
    if ((lfo.m_setup_flags & 2) != 0) {
      lfo.m_next_step = (rand() & 0x7ff) << 16;
    } else {
      lfo.m_next_step = lfop.start_offset << 16;
    }

    // lg::info("starting LFO type {} for {}", magic_enum::enum_name(lfo.m_type),
    //          magic_enum::enum_name(lfo.m_target));
    lfo.init();
  } else {
    lfo.m_type = lfo_type::OFF;
  }

  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_STARTCHILDSOUND(blocksound_handler& handler) {
  auto psp = std::get<PlaySoundParams>(data);

  s32 vol = psp.vol;
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

  s32 pan = psp.pan;
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
  s32 index = psp.sound_id;

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

s32 Grain::snd_SFX_GRAIN_TYPE_STOPCHILDSOUND(blocksound_handler& handler) {
  auto psp = std::get<PlaySoundParams>(data);
  auto& block = static_cast<SFXBlock&>(handler.m_bank);

  if (psp.sound_id >= 0) {
    for (auto it = handler.m_children.begin(); it != handler.m_children.end();) {
      auto* sound = static_cast<blocksound_handler*>(it->get());
      // TODO VERIFY that this works
      if (&sound->m_sfx == &block.Sounds[psp.sound_id]) {
        it = handler.m_children.erase(it);
      } else {
        ++it;
      }
    }

    return 0;
  }

  lg::error("indirect stopchildsound");
  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_PLUGIN_MESSAGE(blocksound_handler& handler) {
  // lg::warn("plugin message");
  //  TODO probably used
  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_BRANCH(blocksound_handler& handler) {
  return 0;
}

s32 Grain::snd_SFX_UNKNOWN_GRAIN_TYPE(blocksound_handler& handler) {
  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_CONTROL_NULL(blocksound_handler& handler) {
  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_LOOP_START(blocksound_handler& handler) {
  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_LOOP_END(blocksound_handler& handler) {
  bool found = false;
  for (int i = handler.m_next_grain - 1; i >= 0 && !found; i--) {
    if (handler.m_sfx.Grains[i].Type == GrainType::LOOP_START) {
      handler.m_next_grain = i - 1;
      found = true;
    }
  }

  if (!found) {
    lg::error("LOOP_END could not find LOOP_START");
  }

  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_LOOP_CONTINUE(blocksound_handler& handler) {
  bool found = false;
  for (int i = handler.m_next_grain + 1; i < (int)handler.m_sfx.Grains.size() && !found; i++) {
    if (handler.m_sfx.Grains[i].Type == GrainType::LOOP_END) {
      handler.m_next_grain = i;
      found = true;
    }
  }

  if (!found) {
    lg::error("LOOP_CONTINUE could not find LOOP_END");
  }

  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_STOP(blocksound_handler& handler) {
  handler.m_done = true;

  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_RAND_PLAY(blocksound_handler& handler) {
  auto cp = std::get<ControlParams>(data);
  auto options = cp.param[0];
  auto count = cp.param[1];
  auto previous = cp.param[2];

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

s32 Grain::snd_SFX_GRAIN_TYPE_RAND_DELAY(blocksound_handler& handler) {
  auto max = std::get<RandDelayParams>(data);
  return rand() % max.Amount;
}

s32 Grain::snd_SFX_GRAIN_TYPE_RAND_PB(blocksound_handler& handler) {
  auto cp = std::get<ControlParams>(data);
  auto pb = cp.param[0];

  s32 rnd = rand();
  handler.set_pbend(pb * ((0xffff * (rnd % 0x7fff)) / 0x7fff - 0x8000) / 100);

  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_ADD_PB(blocksound_handler& handler) {
  auto cp = std::get<ControlParams>(data);
  auto pb = cp.param[0];

  s32 new_pb = handler.m_cur_pb + 0x7fff * pb / 127;
  new_pb = std::clamp<s32>(new_pb, INT16_MIN, INT16_MAX);

  handler.set_pbend(new_pb);

  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_PB(blocksound_handler& handler) {
  auto cp = std::get<ControlParams>(data);
  auto pb = cp.param[0];

  if (pb >= 0) {
    handler.set_pbend(0x7fff * pb / 127);
  } else {
    handler.set_pbend(-0x8000 * pb / -128);
  }

  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_SET_REGISTER(blocksound_handler& handler) {
  auto cp = std::get<ControlParams>(data);
  auto reg = cp.param[0];
  auto value = cp.param[1];

  if (reg < 0) {
    g_block_reg.at(-reg - 1) = value;
  } else {
    handler.m_registers.at(reg) = value;
  }

  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_SET_REGISTER_RAND(blocksound_handler& handler) {
  auto cp = std::get<ControlParams>(data);
  auto reg = cp.param[0];
  auto lower_bound = cp.param[1];
  auto upper_bound = cp.param[2];

  s32 range = upper_bound - lower_bound + 1;
  s32 rnd = (rand() % range) + lower_bound;
  if (reg < 0) {
    g_block_reg.at(-reg - 1) = rnd;
  } else {
    handler.m_registers.at(reg) = rnd;
  }

  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_INC_REGISTER(blocksound_handler& handler) {
  auto cp = std::get<ControlParams>(data);
  auto reg = cp.param[0];

  if (reg < 0) {
    s32 new_val = g_block_reg.at(-reg - 1) + 1;
    g_block_reg.at(-reg - 1) = std::clamp<s32>(new_val, INT8_MIN, INT8_MAX);

  } else {
    s32 new_val = handler.m_registers.at(reg) + 1;
    handler.m_registers.at(reg) = std::clamp<s32>(new_val, INT8_MIN, INT8_MAX);
  }
  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_DEC_REGISTER(blocksound_handler& handler) {
  auto cp = std::get<ControlParams>(data);
  auto reg = cp.param[0];

  if (reg < 0) {
    s32 new_val = g_block_reg.at(-reg - 1) - 1;
    g_block_reg.at(-reg - 1) = std::clamp<s32>(new_val, INT8_MIN, INT8_MAX);

  } else {
    s32 new_val = handler.m_registers.at(reg) - 1;
    handler.m_registers.at(reg) = std::clamp<s32>(new_val, INT8_MIN, INT8_MAX);
  }

  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_TEST_REGISTER(blocksound_handler& handler) {
  auto cp = std::get<ControlParams>(data);
  auto reg = cp.param[0];
  auto action = cp.param[1];
  auto cmp = cp.param[2];

  s32 value;
  if (reg < 0) {
    value = g_block_reg[-reg - 1];
  } else {
    value = handler.m_registers.at(reg);
  }

  if (action == 0) {
    if (value >= cmp) {
      handler.m_next_grain++;
    }
  } else if (action == 1) {
    if (value != cmp) {
      handler.m_next_grain++;
    }
  } else if (action >= 2) {
    if (cmp >= value)
      handler.m_next_grain++;
  }

  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_MARKER(blocksound_handler& handler) {
  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_GOTO_MARKER(blocksound_handler& handler) {
  auto cp = std::get<ControlParams>(data);
  auto target_mark = cp.param[0];
  bool found = false;

  for (int i = 0; i < (int)handler.m_sfx.Grains.size() && !found; i++) {
    if (handler.m_sfx.Grains.at(i).Type == GrainType::MARKER) {
      auto mcp = std::get<ControlParams>(handler.m_sfx.Grains.at(i).data);
      auto mark = mcp.param[0];

      if (mark == target_mark) {
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

s32 Grain::snd_SFX_GRAIN_TYPE_GOTO_RANDOM_MARKER(blocksound_handler& handler) {
  auto cp = std::get<ControlParams>(data);
  auto lower_bound = cp.param[0];
  auto upper_bound = cp.param[1];

  bool found = false;
  s32 range = upper_bound - lower_bound + 1;
  s32 target_mark = (rand() % range) + lower_bound;

  for (int i = 0; i < (int)handler.m_sfx.Grains.size() && !found; i++) {
    if (handler.m_sfx.Grains.at(i).Type == GrainType::MARKER) {
      auto mcp = std::get<ControlParams>(handler.m_sfx.Grains.at(i).data);
      auto mark = mcp.param[0];

      if (mark == target_mark) {
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

s32 Grain::snd_SFX_GRAIN_TYPE_WAIT_FOR_ALL_VOICES(blocksound_handler& handler) {
  if (!handler.m_voices.empty()) {
    handler.m_next_grain--;
    return 1;
  }

  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_PLAY_CYCLE(blocksound_handler& handler) {
  auto& cp = std::get<ControlParams>(data);
  auto group_size = cp.param[0];
  auto group_count = cp.param[1];
  auto index = cp.param[2];

  auto a = index++;
  if (index == group_size) {
    index = 0;
  }
  cp.param[2] = index;

  handler.m_next_grain += group_count * a;
  handler.m_grains_to_play = group_count + 1;
  handler.m_grains_to_skip = (group_size - 1 - a) * group_count;
  handler.m_skip_grains = true;
  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_ADD_REGISTER(blocksound_handler& handler) {
  auto cp = std::get<ControlParams>(data);
  auto val = cp.param[0];
  auto reg = cp.param[1];

  if (reg < 0) {
    s32 new_val = g_block_reg.at(-reg - 1) + val;
    g_block_reg.at(-reg - 1) = std::clamp<s32>(new_val, INT8_MIN, INT8_MAX);
  } else {
    s32 new_val = handler.m_registers.at(reg) + val;
    handler.m_registers.at(reg) = std::clamp<s32>(new_val, INT8_MIN, INT8_MAX);
  }
  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_KEY_OFF_VOICES(blocksound_handler& handler) {
  for (auto& p : handler.m_voices) {
    auto v = p.lock();
    if (v == nullptr) {
      continue;
    }

    v->key_off();
  }
  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_KILL_VOICES(blocksound_handler& handler) {
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

s32 Grain::snd_SFX_GRAIN_TYPE_ON_STOP_MARKER(blocksound_handler& handler) {
  handler.m_next_grain = handler.m_sfx.Grains.size() - 1;
  return 0;
}

s32 Grain::snd_SFX_GRAIN_TYPE_COPY_REGISTER(blocksound_handler& handler) {
  auto cp = std::get<ControlParams>(data);
  auto src = cp.param[0];
  auto dst = cp.param[1];

  s8 value = 0;
  if (src < 0) {
    value = g_block_reg.at(-src - 1);
  } else {
    value = handler.m_registers.at(src);
  }

  if (dst < 0) {
    g_block_reg.at(-dst - 1) = value;
  } else {
    handler.m_registers.at(dst) = value;
  }

  return 0;
}

}  // namespace snd
