#include "blocksound_handler.h"

#include <random>
#include <stdexcept>

#include "util.h"

#include "common/log/log.h"

namespace snd {
std::array<u8, 32> g_block_reg{};

void blocksound_handler::init() {
  m_next_grain = 0;
  m_countdown = m_sfx.grains[0]->delay();

  // if (m_sfx.d.Flags & 2) {
  //   fmt::print("solo flag\n");
  //   m_done = true;
  //   return;
  // }

  while (m_countdown <= 0 && !m_done) {
    do_grain();
  }
}

bool blocksound_handler::tick() {
  m_voices.remove_if([](std::weak_ptr<vag_voice>& p) { return p.expired(); });

  if (m_done) {
    if (m_voices.empty()) {
      // fmt::print("{}: voices empty\n", (void*)this);
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
  if (new_vol >= 128) {
    new_vol = 127;
  }
  if (new_vol < 0) {
    new_vol = 0;
  }

  s32 new_pan = m_app_pan + m_lfo_pan;
  while (pan >= 360) {
    pan -= 360;
  }
  while (pan < 0) {
    pan += 360;
  }

  if (pan != m_cur_pan || new_vol != m_cur_volume) {
    m_cur_volume = new_vol;
    m_cur_pan = pan;

    for (auto& p : m_voices) {
      auto voice = p.lock();
      if (voice == nullptr) {
        continue;
      }

      auto volume =
          m_vm.make_volume(127, 0, m_cur_volume, m_cur_pan, voice->tone.Vol, voice->tone.Pan);
      auto left = m_vm.adjust_vol_to_group(volume.left, m_sfx.d.VolGroup);
      auto right = m_vm.adjust_vol_to_group(volume.right, m_sfx.d.VolGroup);

      voice->set_volume(left >> 1, right >> 1);
    }
  }
}

void blocksound_handler::update_pitch() {
  m_cur_pm = m_app_pm;
  m_cur_pb = m_app_pb;

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
  m_app_pm = mod;
  update_pitch();
}

void blocksound_handler::set_pbend(s32 bend) {
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
