// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#include "midi_handler.h"

#include "ame_handler.h"

#include "common/log/log.h"

#include "game/sound/989snd/util.h"

#include "third-party/fmt/core.h"

namespace snd {
/*
** In the original 989snd, the player struct can live in different places
** depending on the type of file.
**
** For files with multiple tracks it lives in-place before the sequence data
** where the file is loaded. For single track (like sunken) it lives separetely
**
** the sequencer ticks at 240hz
**
*/

midi_handler::midi_handler(MIDIBlockHeader* block,
                           voice_manager& vm,
                           MIDISound& sound,
                           s32 vol,
                           s32 pan,
                           locator& loc,
                           SoundBank& bank)
    : m_sound(sound),
      m_locator(loc),
      m_repeats(sound.Repeats),
      m_bank(bank),
      m_header(block),
      m_vm(vm) {
  if (vol == VOLUME_DONT_CHANGE) {
    vol = 1024;
  }

  m_vol = (vol * m_sound.Vol) >> 10;
  if (m_vol >= 128) {
    m_vol = 127;
  }

  if (pan == PAN_DONT_CHANGE || pan == PAN_RESET) {
    m_pan = m_sound.Pan;
  } else {
    m_pan = pan;
  }

  init_midi();
}

midi_handler::midi_handler(MIDIBlockHeader* block,
                           voice_manager& vm,
                           MIDISound& sound,
                           s32 vol,
                           s32 pan,
                           locator& loc,
                           SoundBank& bank,
                           std::optional<ame_handler*> parent)
    : m_parent(parent),
      m_sound(sound),
      m_locator(loc),
      m_vol(vol),
      m_pan(pan),
      m_repeats(sound.Repeats),
      m_bank(bank),
      m_header(block),
      m_vm(vm) {
  init_midi();
}

void midi_handler::init_midi() {
  m_seq_data_start = (u8*)((uintptr_t)m_header + (uintptr_t)m_header->DataStart);
  m_seq_ptr = m_seq_data_start;
  m_tempo = m_header->Tempo;
  m_ppq = m_header->PPQ;
  m_chanvol.fill(0x7f);
  m_chanpan.fill(0);
}

std::pair<size_t, u32> midi_handler::read_vlq(u8* value) {
  size_t len = 1;
  u32 out = *value & 0x7f;
  // fmt::print("starting with {:x}\n", *value);

  if ((*value & 0x80) != 0) {
    while ((*value & 0x80) != 0) {
      len++;
      value++;
      out = (out << 7) + (*value & 0x7f);
    }
  }

  return {len, out};
}

void midi_handler::pause() {
  m_paused = true;

  for (auto& p : m_voices) {
    auto voice = p.lock();
    if (voice == nullptr) {
      continue;
    }

    m_vm.pause(voice);
  }
}

void midi_handler::unpause() {
  m_paused = false;

  for (auto& p : m_voices) {
    auto voice = p.lock();
    if (voice == nullptr) {
      continue;
    }

    m_vm.unpause(voice);
  }
}

void midi_handler::stop() {
  m_track_complete = true;

  for (auto& p : m_voices) {
    auto voice = p.lock();
    if (voice == nullptr) {
      continue;
    }

    voice->key_off();
  }
}

void midi_handler::set_vol_pan(s32 vol, s32 pan) {
  if (vol != VOLUME_DONT_CHANGE) {
    if (vol >= 0) {
      m_vol = (m_sound.Vol * vol) >> 10;
    } else {
      m_vol = -vol;
    }
  }

  if (m_vol > 127) {
    m_vol = 127;
  }

  if (pan != PAN_DONT_CHANGE) {
    if (pan == PAN_RESET) {
      m_pan = m_sound.Pan;
    } else {
      m_pan = pan;
    }
  }

  for (auto& v : m_voices) {
    auto voice = v.lock();
    if (voice == nullptr) {
      continue;
    }

    s16 pan = m_chanpan[voice->channel] + m_pan;
    if (pan >= 360) {
      pan -= 360;
    }

    voice->basevol =
        m_vm.make_volume_b(m_vol, voice->velocity * m_chanvol[voice->channel] / 127, pan,
                           voice->prog.Vol, voice->prog.Pan, voice->tone.Vol, voice->tone.Pan);

    auto left = m_vm.adjust_vol_to_group(voice->basevol.left, voice->group);
    auto right = m_vm.adjust_vol_to_group(voice->basevol.right, voice->group);
    voice->set_volume(left >> 1, right >> 1);
  }
}

void midi_handler::set_pmod(s32 mod) {
  m_cur_pm = mod;

  for (auto& v : m_voices) {
    auto voice = v.lock();
    if (voice == nullptr) {
      continue;
    }

    voice->current_pm = m_cur_pm;
    auto note = pitchbend(voice->tone, voice->current_pb, voice->current_pm, voice->start_note,
                          voice->start_fine);
    auto pitch =
        PS1Note2Pitch(voice->tone.CenterNote, voice->tone.CenterFine, note.first, note.second);
    voice->set_pitch(pitch);
  }
}

void midi_handler::mute_channel(u8 channel) {
  // fmt::print("{:x} ame muting channel {}\n", (u64)this, channel);
  m_mute_state[channel] = true;
}

void midi_handler::unmute_channel(u8 channel) {
  // fmt::print("{:x} ame unmuting channel {}\n", (u64)this, channel);
  m_mute_state[channel] = false;
}

void midi_handler::note_on() {
  u8 channel = m_status & 0xf;
  u8 note = m_seq_ptr[0];
  u8 velocity = m_seq_ptr[1];

  if (velocity == 0) {
    note_off();
    return;
  }

  if (m_mute_state[channel]) {
    m_seq_ptr += 2;
    return;
  }

  // fmt::print("{:x} {}: [ch{:01x}] note on {:02x} {:02x}\n", (u64)this, m_time, channel, note,
  //            velocity);

  // Key on all the applicable tones for the program
  auto bank = dynamic_cast<MusicBank*>(m_locator.get_bank_by_id(m_header->BankID));
  auto& program = bank->m_programs[m_programs[channel]];

  for (auto& t : program.tones) {
    if (note >= t.MapLow && note <= t.MapHigh) {
      s16 pan = m_chanpan[channel] + m_pan;
      if (pan >= 360) {
        pan -= 360;
      }

      auto voice = std::make_shared<midi_voice>(t, program.d);
      voice->basevol = m_vm.make_volume_b(m_vol, (velocity * m_chanvol[channel]) / 0x7f, pan,
                                          program.d.Vol, program.d.Pan, t.Vol, t.Pan);

      voice->note = note;
      voice->channel = channel;
      voice->velocity = velocity;

      voice->start_note = note;
      voice->start_fine = 0;

      voice->current_pm = m_pitch_bend[channel];
      voice->current_pb = m_cur_pm;

      voice->group = m_sound.VolGroup;
      m_vm.start_tone(voice, m_bank.bank_id);
      m_voices.emplace_front(voice);
    }
  }

  m_seq_ptr += 2;
}

void midi_handler::note_off() {
  u8 channel = m_status & 0xf;
  u8 note = m_seq_ptr[0];
  // Yep, no velocity for note-offs
  [[maybe_unused]] u8 velocity = m_seq_ptr[1];

  // fmt::print("{}: note off {:02x} {:02x} {:02x}\n", m_time, m_status, m_seq_ptr[0],
  // m_seq_ptr[1]);

  for (auto& v : m_voices) {
    auto voice = v.lock();
    if (voice == nullptr) {
      continue;
    }

    if (voice->channel == channel && voice->note == note) {
      voice->key_off();
    }
  }

  m_seq_ptr += 2;
}

void midi_handler::program_change() {
  u8 channel = m_status & 0xf;
  u8 program = m_seq_ptr[0];

  m_programs[channel] = program;

  // fmt::print("{:x} {}: [ch{:01x}] program change {:02x} -> {:02x}\n", (u64)this, m_time, channel,
  // m_programs[channel], program);
  m_seq_ptr += 1;
}

void midi_handler::channel_pressure() {
  u8 channel = m_status & 0xf;
  u8 note = m_seq_ptr[0];
  // fmt::print("{}: channel pressure {:02x} {:02x}\n", m_time, m_status, m_seq_ptr[0]);

  for (auto& v : m_voices) {
    auto voice = v.lock();
    if (voice == nullptr) {
      continue;
    }

    if (voice->channel == channel && voice->note == note) {
      voice->key_off();
    }
  }

  m_seq_ptr += 1;
}

void midi_handler::channel_pitch() {
  u8 channel = m_status & 0xF;
  s32 pitch = 0xFFFF * ((m_seq_ptr[0] & 0x7f) | ((m_seq_ptr[1] & 0x7f) << 7)) / 0x3FFF;
  // lg::debug("{}: pitch ch{:01x} {:04x}", m_time, channel, pitch);

  m_pitch_bend[channel] = pitch + 0x8000;
  for (auto& v : m_voices) {
    auto voice = v.lock();
    if (voice == nullptr) {
      continue;
    }

    if (voice->channel == channel) {
      voice->current_pb = m_pitch_bend[channel];
      auto note = pitchbend(voice->tone, voice->current_pb, voice->current_pm, voice->start_note,
                            voice->start_fine);
      auto pitch =
          PS1Note2Pitch(voice->tone.CenterNote, voice->tone.CenterFine, note.first, note.second);
      voice->set_pitch(pitch);
    }
  }

  m_seq_ptr += 2;
}

void midi_handler::meta_event() {
  // fmt::print("{}: meta event {:02x}\n", m_time, *m_seq_ptr);
  size_t len = m_seq_ptr[1];

  if (*m_seq_ptr == 0x2f) {
    m_seq_ptr = m_seq_data_start;

    // If repeats was 0 we'll go negative, fail this test, and loop infinitely as intended
    m_repeats--;
    if (m_repeats == 0) {
      m_track_complete = true;
    }

    if (m_repeats < 0) {
      m_repeats = 0;
    }

    return;
  }

  if (*m_seq_ptr == 0x51) {
    m_tempo = (m_seq_ptr[2] << 16) | (m_seq_ptr[3] << 8) | (m_seq_ptr[4]);
    m_ppt = 100 * mics_per_tick / (m_tempo / m_ppq);
  }

  m_seq_ptr += len + 2;
}

static s16 midiTo360Pan(u8 pan) {
  if (pan >= 64) {
    return (s16)(90 * (pan - 64) / 63);
  } else {
    return (s16)(90 * pan / 64 + 270);
  }
}

void midi_handler::controller_change() {
  u8 channel = m_status & 0xf;
  u8 controller = m_seq_ptr[0];
  u8 value = m_seq_ptr[1];

  switch (controller) {
    // case 0x0: {} break; // TODO bank select
    case 0x7: {
      m_chanvol[channel] = static_cast<s8>(value);
    } break;
    case 0xa: {
      m_chanpan[channel] = midiTo360Pan(static_cast<s8>(value));
    } break;
    // case 0x40: {} break; // TODO damper
    default:
      throw midi_error(fmt::format("invalid midi controller change {}", controller));
      break;
  }

  m_seq_ptr += 2;
}

void midi_handler::system_event() {
  // fmt::print("{}: system event {:02x}\n", m_time, *m_seq_ptr);

  switch (*m_seq_ptr) {
    case 0x75:
      m_seq_ptr++;
      if (m_parent.has_value()) {
        auto [cont, ptr] = m_parent.value()->run_ame(*this, m_seq_ptr);
        m_seq_ptr = ptr;

        if (!cont) {
          // lg::debug("{:x} track stopped by ame", (u64)this);
          m_track_complete = true;
        }
      } else {
        throw midi_error("MIDI tried to run AME without an AME handler");
      }
      break;
    default:
      throw midi_error(fmt::format("Unknown system message {:02x}", *m_seq_ptr));
  }
}

bool midi_handler::tick() {
  if (m_paused) {
    return m_track_complete;
  }

  try {
    m_voices.remove_if([](auto& v) { return v.expired(); });
    step();
  } catch (midi_error& e) {
    m_track_complete = true;
    lg::error("MIDI Error: {}", e.what());
    fmt::print("Sequence following: ");
    for (int i = 0; i < 10; i++) {
      fmt::print("{:x} ", m_seq_ptr[i]);
    }
    fmt::print("\n");
  }

  return m_track_complete;
}

void midi_handler::new_delta() {
  auto [len, delta] = read_vlq(m_seq_ptr);

  m_seq_ptr += len;
  m_time += delta;

  m_ppt = 100 * mics_per_tick / (m_tempo / m_ppq);
  m_tickdelta = 100 * delta + m_tickerror;
  if (m_tickdelta < 0 || m_tickdelta < m_ppt / 2) {
    m_tickerror = m_tickdelta;
    m_tickdelta = 0;
  }
  if (m_tickdelta != 0) {
    m_tick_countdown = (m_tickdelta / 100 * m_tempo / m_ppq - 1 + mics_per_tick) / mics_per_tick;
    m_tickerror = m_tickdelta - m_ppt * m_tick_countdown;
  }
}

void midi_handler::step() {
  if (m_get_delta) {
    new_delta();
    m_get_delta = false;
  } else {
    m_tick_countdown--;
  }

  while (!m_tick_countdown && !m_track_complete) {
    // running status, new events always have top bit
    if (*m_seq_ptr & 0x80) {
      m_status = *m_seq_ptr;
      m_seq_ptr++;
    }

    switch (m_status >> 4) {
      case 0x8:
        note_off();
        break;
      case 0x9:
        note_on();
        break;
      case 0xB:
        controller_change();
        break;
      case 0xD:
        channel_pressure();
        break;
      case 0xC:
        program_change();
        break;
      case 0xE:
        channel_pitch();
        break;
      case 0xF:
        // normal meta-event
        if (m_status == 0xFF) {
          meta_event();
          break;
        }
        if (m_status == 0xF0) {
          system_event();
          break;
        }
        [[fallthrough]];
      default:
        throw midi_error(fmt::format("invalid status {}", m_status));
        return;
    }

    new_delta();
  }
}

}  // namespace snd
