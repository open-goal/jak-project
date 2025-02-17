#include "blocksound_handler.h"

#include <random>
#include <stdexcept>

#include "util.h"

#include "common/log/log.h"

namespace snd {
std::array<s8, 32> g_block_reg{};

BlockSoundHandler::BlockSoundHandler(SoundBank& bank,
                                     SFXBlock::SFX& sfx,
                                     VoiceManager& vm,
                                     s32 sfx_vol,
                                     s32 sfx_pan,
                                     SndPlayParams& params,
                                     u32 sound_id,
                                     s32 start_tick)
    : m_group(sfx.VolGroup),
      m_sfx(sfx),
      m_vm(vm),
      m_bank(bank),
      m_sound_id(sound_id),
      m_start_tick(start_tick) {
  s32 vol, pan, pitch_mod, pitch_bend;
  if (sfx_vol == -1) {
    sfx_vol = sfx.Vol;
  }
  if (sfx_pan == -1) {
    sfx_pan = sfx.Pan;
  }

  if (params.vol.has_value()) {
    vol = params.vol.value();
  } else {
    vol = 1024;
  }

  if (params.pan.has_value()) {
    pan = params.pan.value();
  } else {
    pan = -1;
  }

  if (params.pitch_mod.has_value()) {
    pitch_mod = params.pitch_mod.value();
  } else {
    pitch_mod = 0;
  }

  if (params.pitch_bend.has_value()) {
    pitch_bend = params.pitch_bend.value();
  } else {
    pitch_bend = 0;
  }

  if (vol == VOLUME_DONT_CHANGE) {
    vol = 1024;
  }
  s32 play_vol = (sfx_vol * vol) >> 10;
  if (play_vol >= 128) {
    play_vol = 127;
  }

  if (pan == PAN_RESET || pan == PAN_DONT_CHANGE) {
    pan = sfx_pan;
  }

  m_orig_volume = sfx_vol;
  m_orig_pan = sfx_pan;

  m_cur_volume = play_vol;
  m_cur_pan = pan;
  m_cur_pb = pitch_bend;
  m_cur_pm = pitch_mod;

  m_app_volume = vol;
  m_app_pan = pan;
  m_app_pb = pitch_bend;
  m_app_pm = pitch_mod;

  m_lfo_volume = 0;
  m_lfo_pan = 0;
  m_lfo_pb = 0;
  m_lfo_pm = 0;

  if (params.registers.has_value()) {
    m_registers = params.registers.value();
  }

  // Figure this stuff out properly someday
  // if (m_sfx.d.Flags & 2) {
  //   fmt::print("solo flag\n");
  //   m_done = true;
  //   return;
  // }

  m_next_grain = 0;
  m_countdown = m_sfx.Grains[0].Delay;
  while (m_countdown <= 0 && !m_done) {
    DoGrain();
  }
}

BlockSoundHandler::~BlockSoundHandler() {
  for (auto& p : m_voices) {
    auto v = p.lock();
    if (v != nullptr) {
      v->Stop();
    }
  }
}

bool BlockSoundHandler::Tick() {
  m_voices.remove_if([](std::weak_ptr<BlockSoundVoice>& p) { return p.expired(); });

  for (auto& lfo : m_lfo) {
    lfo.Tick();
  }

  for (auto it = m_children.begin(); it != m_children.end();) {
    bool done = it->get()->Tick();
    if (done) {
      it = m_children.erase(it);
    } else {
      ++it;
    }
  }

  if (m_done && m_children.empty()) {
    if (m_voices.empty()) {
      return m_done;
    } else {
      return false;
    }
  }

  if (m_paused)
    return false;

  m_countdown--;
  while (m_countdown <= 0 && !m_done) {
    DoGrain();
  }

  return false;
};

void BlockSoundHandler::Pause() {
  m_paused = true;

  for (auto& c : m_children) {
    c->Pause();
  }

  for (auto& p : m_voices) {
    auto voice = p.lock();
    if (voice == nullptr) {
      continue;
    }

    m_vm.Pause(voice);
  }
}

void BlockSoundHandler::Unpause() {
  m_paused = false;

  for (auto& c : m_children) {
    c->Unpause();
  }

  for (auto& p : m_voices) {
    auto voice = p.lock();
    if (voice == nullptr) {
      continue;
    }

    m_vm.Unpause(voice);
  }
}

void BlockSoundHandler::Stop() {
  m_done = true;

  for (auto& c : m_children) {
    c->Stop();
  }

  for (auto& p : m_voices) {
    auto voice = p.lock();
    if (voice == nullptr) {
      continue;
    }

    voice->KeyOff();
  }
}

void BlockSoundHandler::SetVolPan(s32 vol, s32 pan) {
  if (vol >= 0) {
    if (vol != VOLUME_DONT_CHANGE) {
      m_app_volume = vol;
    }
  } else {
    m_app_volume = -1024 * vol / 127;
  }

  if (pan == PAN_RESET) {
    m_app_pan = m_sfx.Pan;
  } else if (pan != PAN_DONT_CHANGE) {
    m_app_pan = pan;
  }

  s32 new_vol = ((m_app_volume * m_orig_volume) >> 10) + m_lfo_volume;
  new_vol = std::clamp(new_vol, 0, 127);

  s32 new_pan = m_app_pan + m_lfo_pan;
  while (new_pan >= 360) {
    new_pan -= 360;
  }
  while (new_pan < 0) {
    new_pan += 360;
  }

  if (new_pan != m_cur_pan || new_vol != m_cur_volume) {
    m_cur_volume = new_vol;
    m_cur_pan = new_pan;

    for (auto& c : m_children) {
      c->SetVolPan(m_app_volume * m_orig_volume / 127, pan);
    }

    for (auto& p : m_voices) {
      auto voice = p.lock();
      if (voice == nullptr) {
        continue;
      }

      auto volume = m_vm.MakeVolume(127, 0, m_cur_volume, m_cur_pan, voice->g_vol, voice->g_pan);
      auto left = m_vm.AdjustVolToGroup(volume.left, m_group);
      auto right = m_vm.AdjustVolToGroup(volume.right, m_group);

      voice->SetVolume(left >> 1, right >> 1);
    }
  }
}

void BlockSoundHandler::UpdatePitch() {
  m_cur_pm = m_app_pm + m_lfo_pm;
  m_cur_pb = std::clamp<s32>(m_app_pb + m_lfo_pb, INT16_MIN, INT16_MAX);

  for (auto& p : m_voices) {
    auto voice = p.lock();
    if (voice == nullptr) {
      continue;
    }

    auto note = PitchBend(voice->tone, m_cur_pb, m_cur_pm, m_note, m_fine);
    auto pitch =
        PS1Note2Pitch(voice->tone.CenterNote, voice->tone.CenterFine, note.first, note.second);

    voice->SetPitch(pitch);
  }
}

void BlockSoundHandler::SetPMod(s32 mod) {
  for (auto& c : m_children) {
    c->SetPMod(mod);
  }
  m_app_pm = mod;
  UpdatePitch();
}

void BlockSoundHandler::SetPBend(s32 bend) {
  for (auto& c : m_children) {
    c->SetPBend(bend);
  }
  m_app_pb = bend;
  UpdatePitch();
}

void BlockSoundHandler::DoGrain() {
  auto& grain = m_sfx.Grains[m_next_grain];

  s32 ret = grain(*this);

  if (m_skip_grains) {
    m_grains_to_play--;
    if (m_grains_to_play == 0) {
      m_next_grain += m_grains_to_skip;
      m_skip_grains = false;
    }
  }

  m_next_grain++;
  if (m_next_grain >= m_sfx.Grains.size()) {
    m_done = true;
    return;
  }

  m_countdown = m_sfx.Grains[m_next_grain].Delay + ret;
}

SoundHandler* BlockSoundHandler::CheckInstanceLimit(
    const std::map<u32, std::unique_ptr<SoundHandler>>& handlers,
    s32 vol) {
  if (!m_sfx.InstanceLimit) {
    return nullptr;
  }

  if (!m_sfx.Flags.has_instlimit()) {
    return nullptr;
  }

  BlockSoundHandler* weakest = nullptr;
  int inst = 0;

  for (const auto& [id, handler_ptr] : handlers) {
    // Only compare to BlockSoundHandlers
    auto* handler = dynamic_cast<BlockSoundHandler*>(handler_ptr.get());
    if (!handler) {
      continue;
    }

    // See if this is playing the same sound
    // 989snd checks both an orig_sound and a SH.Sound, but we never change the sound.
    // We'd need to revisit this if we eventually support BRANCH grains.
    if (&handler->m_sfx == &m_sfx) {
      inst++;
      if (!weakest ||                                                                        //
          (m_sfx.Flags.instlimit_vol() && handler->m_app_volume < weakest->m_app_volume) ||  //
          (m_sfx.Flags.instlimit_tick() && handler->m_start_tick < weakest->m_start_tick)) {
        weakest = handler;
      }
    }
  }

  // See if this handler would cause us to exceed the limit
  if (m_sfx.InstanceLimit - 1 < inst) {
    if (weakest && ((m_sfx.Flags.instlimit_vol() && weakest->m_app_volume < vol) ||
                    m_sfx.Flags.instlimit_tick())) {
      // existing weakest is worst
      return weakest;
    } else {
      // new sound is weakest
      return this;
    }
  } else {
    return nullptr;
  }
}

}  // namespace snd
