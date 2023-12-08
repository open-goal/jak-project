#include "blocksound_handler.h"

#include <random>
#include <stdexcept>

#include "util.h"

#include "game/sound/989snd/player.h"

namespace snd {
std::array<s8, 32> g_block_reg{};

BlockSoundHandler* BlockSoundHandler::MakeBlockSound(SoundBank& bank,
                                                     SFXBlock::SFX& sfx,
                                                     s32 sfx_vol,
                                                     s32 sfx_pan,
                                                     SndPlayParams& params) {
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

  if (sfx.Flags.solo()) {
    StopAllHandlersForSound(sfx);
  }

  auto* hnd = AllocBlockSound(bank, sfx, vol);
  if (hnd == nullptr) {
    return nullptr;
  }

  hnd->m_orig_volume = sfx_vol;
  hnd->m_orig_pan = sfx_pan;

  hnd->m_cur_volume = play_vol;
  hnd->m_cur_pan = pan;
  hnd->m_cur_pb = pitch_bend;
  hnd->m_cur_pm = pitch_mod;

  hnd->m_app_volume = vol;
  hnd->m_app_pan = pan;
  hnd->m_app_pb = pitch_bend;
  hnd->m_app_pm = pitch_mod;

  hnd->m_lfo_volume = 0;
  hnd->m_lfo_pan = 0;
  hnd->m_lfo_pb = 0;
  hnd->m_lfo_pm = 0;

  if (params.registers.has_value()) {
    hnd->m_registers = params.registers.value();
  }

  // Bug from PS2, this was never set
  hnd->m_start_tick = GetTick();

  hnd->m_next_grain = 0;
  hnd->m_countdown = hnd->m_sfx.Grains[0].Delay;
  while (hnd->m_countdown <= 0 && !hnd->m_done) {
    hnd->DoGrain();
  }

  return hnd;
}

BlockSoundHandler::BlockSoundHandler(SoundHandle oid, SoundBank& bank, SFXBlock::SFX& sfx)
    : SoundHandler(oid), m_group(sfx.VolGroup), m_sfx(sfx), m_bank(bank) {}

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
    bool done = (*it)->Tick();
    if (done) {
      FreeSound(*it);
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

    PauseTone(voice);
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

    UnpauseTone(voice);
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

      auto volume = MakeVolume(127, 0, m_cur_volume, m_cur_pan, voice->g_vol, voice->g_pan);
      auto left = AdjustVolToGroup(volume.left, m_group);
      auto right = AdjustVolToGroup(volume.right, m_group);

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

}  // namespace snd
