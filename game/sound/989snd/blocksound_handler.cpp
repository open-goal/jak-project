#include "blocksound_handler.h"

#include <random>
#include <stdexcept>

#include "util.h"

namespace snd {
void blocksound_handler::init() {
  m_next_grain = 0;
  m_countdown = m_sfx.grains[0].Delay;

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
      m_app_volume = (vol * m_sfx.d.Vol) >> 10;
    }
  } else {
    m_app_volume = -vol;
  }

  if (m_app_volume >= 128) {
    m_app_volume = 127;
  }

  if (pan == PAN_RESET) {
    m_app_pan = m_sfx.d.Pan;
  } else if (pan != PAN_DONT_CHANGE) {
    m_app_pan = pan;
  }

  vol = m_app_volume;  // + lfo vol
  // TODO LFO logic here

  pan = m_app_pan;  // + lfo pan
  while (pan >= 360) {
    pan -= 360;
  }

  while (pan < 0) {
    pan += 360;
  }

  if (pan != m_cur_pan || vol != m_cur_volume) {
    m_cur_volume = vol;
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

s32 blocksound_handler::null(SFXGrain& grain) {
  return 0;
}

s32 blocksound_handler::play_tone(SFXGrain& grain) {
  auto voice = std::make_shared<vag_voice>(grain.GrainParams.tone);

  voice->basevol = m_vm.make_volume(127, 0, m_cur_volume, m_cur_pan, grain.GrainParams.tone.Vol,
                                    grain.GrainParams.tone.Pan);

  voice->start_note = m_note;
  voice->start_fine = m_fine;
  voice->group = m_group;

  m_vm.start_tone(voice);
  m_voices.emplace_front(voice);

  return 0;
}

s32 blocksound_handler::rand_play(SFXGrain& grain) {
  int options = grain.GrainParams.control.param[0];
  int count = grain.GrainParams.control.param[1];
  int previous = grain.GrainParams.control.param[2];

  int rnd = rand() % options;
  if (rnd == previous) {
    rnd++;
    if (rnd >= options) {
      rnd = 0;
    }
  }

  grain.GrainParams.control.param[2] = rnd;
  m_next_grain += rnd * count;
  m_grains_to_play = count + 1;
  m_grains_to_skip = (options - 1 - rnd) * count;
  m_skip_grains = true;

  return 0;
}

void blocksound_handler::do_grain() {
  auto& grain = m_sfx.grains[m_next_grain];

  auto handler = m_grain_handler.find((grain_type)grain.Type);
  if (handler != m_grain_handler.end()) {
    (this->*(handler->second))(grain);
  } else {
    throw std::runtime_error(
        fmt::format("{}: Ignoring grain {}, type {}\n", (void*)this, m_next_grain, grain.Type));
  }

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

  m_countdown = m_sfx.grains[m_next_grain].Delay;
}

}  // namespace snd
