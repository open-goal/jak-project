#include "blocksound_handler.h"

#include <random>
#include <stdexcept>

#include "util.h"

#include "common/log/log.h"

namespace snd {
std::array<s8, 32> g_block_reg{};

bool blocksound_handler::tick() {
  m_voices.remove_if([](std::weak_ptr<blocksound_voice>& p) { return p.expired(); });

  for (auto& lfo : m_lfo) {
    lfo.tick();
  }

  for (auto it = m_children.begin(); it != m_children.end();) {
    bool done = it->get()->tick();
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
    do_grain();
  }

  return false;
};

void blocksound_handler::pause() {
  m_paused = true;

  for (auto& c : m_children) {
    c->pause();
  }

  for (auto& p : m_voices) {
    auto voice = p.lock();
    if (voice == nullptr) {
      continue;
    }

    m_vm.pause(voice);
  }
}

void blocksound_handler::unpause() {
  m_paused = false;

  for (auto& c : m_children) {
    c->unpause();
  }

  for (auto& p : m_voices) {
    auto voice = p.lock();
    if (voice == nullptr) {
      continue;
    }

    m_vm.unpause(voice);
  }
}

void blocksound_handler::stop() {
  m_done = true;

  for (auto& c : m_children) {
    c->stop();
  }

  for (auto& p : m_voices) {
    auto voice = p.lock();
    if (voice == nullptr) {
      continue;
    }

    voice->key_off();
  }
}

void blocksound_handler::set_vol_pan(s32 vol, s32 pan) {
  if (vol >= 0) {
    if (vol != VOLUME_DONT_CHANGE) {
      m_app_volume = vol;
    }
  } else {
    m_app_volume = -1024 * vol / 127;
  }

  if (pan == PAN_RESET) {
    m_app_pan = m_sfx.d.Pan;
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
      c->set_vol_pan(m_app_volume * m_orig_volume / 127, pan);
    }

    for (auto& p : m_voices) {
      auto voice = p.lock();
      if (voice == nullptr) {
        continue;
      }

      auto volume = m_vm.make_volume(127, 0, m_cur_volume, m_cur_pan, voice->g_vol, voice->g_pan);
      auto left = m_vm.adjust_vol_to_group(volume.left, m_group);
      auto right = m_vm.adjust_vol_to_group(volume.right, m_group);

      voice->set_volume(left >> 1, right >> 1);
    }
  }
}

void blocksound_handler::update_pitch() {
  m_cur_pm = m_app_pm + m_lfo_pm;
  m_cur_pb = std::clamp<s32>(m_app_pb + m_lfo_pb, INT16_MIN, INT16_MAX);

  for (auto& p : m_voices) {
    auto voice = p.lock();
    if (voice == nullptr) {
      continue;
    }

    auto note = pitchbend(voice->tone, m_cur_pb, m_cur_pm, m_note, m_fine);
    auto pitch =
        PS1Note2Pitch(voice->tone.CenterNote, voice->tone.CenterFine, note.first, note.second);

    voice->set_pitch(pitch);
  }
}

void blocksound_handler::set_pmod(s32 mod) {
  for (auto& c : m_children) {
    c->set_pmod(mod);
  }
  m_app_pm = mod;
  update_pitch();
}

void blocksound_handler::set_pbend(s32 bend) {
  for (auto& c : m_children) {
    c->set_pbend(bend);
  }
  m_app_pb = bend;
  update_pitch();
}

void blocksound_handler::do_grain() {
  auto& grain = m_sfx.grains[m_next_grain];

  s32 ret = grain->execute(*this);

  if (m_skip_grains) {
    m_grains_to_play--;
    if (m_grains_to_play == 0) {
      m_next_grain += m_grains_to_skip;
      m_skip_grains = false;
    }
  }

  m_next_grain++;
  if (m_next_grain >= m_sfx.grains.size()) {
    m_done = true;
    return;
  }

  m_countdown = m_sfx.grains[m_next_grain]->delay() + ret;
}

}  // namespace snd
