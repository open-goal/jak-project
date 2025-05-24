// Copyright: 2021 - 2024, Ziemas
// SPDX-License-Identifier: ISC
#include "midi_handler.h"

#include "ame_handler.h"

#include "common/log/log.h"

#include "game/sound/989snd/util.h"

#include "fmt/core.h"

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

MidiHandler::MidiHandler(Midi* block,
                         VoiceManager& vm,
                         MusicBank::MIDISound& sound,
                         s32 vol,
                         s32 pan,
                         SoundBank& bank)
    : m_sound(sound), m_repeats(sound.Repeats), m_bank(bank), m_header(block), m_vm(vm) {
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

  InitMidi();
}

MidiHandler::MidiHandler(Midi* block,
                         VoiceManager& vm,
                         MusicBank::MIDISound& sound,
                         s32 vol,
                         s32 pan,
                         SoundBank& bank,
                         std::optional<AmeHandler*> parent)
    : m_parent(parent),
      m_sound(sound),
      m_vol(vol),
      m_pan(pan),
      m_repeats(sound.Repeats),
      m_bank(bank),
      m_header(block),
      m_vm(vm) {
  InitMidi();
}

void MidiHandler::InitMidi() {
  m_seq_data_start = m_header->DataStart;
  m_seq_ptr = m_seq_data_start;
  m_tempo = m_header->Tempo;
  m_ppq = m_header->PPQ;
  m_chanvol.fill(0x7f);
  m_chanpan.fill(0);
}

std::pair<size_t, u32> MidiHandler::ReadVLQ(u8* value) {
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

void MidiHandler::Pause() {
  m_paused = true;

  for (auto& p : m_voices) {
    auto voice = p.lock();
    if (voice == nullptr) {
      continue;
    }

    m_vm.Pause(voice);
  }
}

void MidiHandler::Unpause() {
  m_paused = false;

  for (auto& p : m_voices) {
    auto voice = p.lock();
    if (voice == nullptr) {
      continue;
    }

    m_vm.Unpause(voice);
  }
}

void MidiHandler::Stop() {
  m_track_complete = true;

  for (auto& p : m_voices) {
    auto voice = p.lock();
    if (voice == nullptr) {
      continue;
    }

    voice->KeyOff();
  }
}

void MidiHandler::SetVolPan(s32 vol, s32 pan) {
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
        m_vm.MakeVolumeB(m_vol, voice->velocity * m_chanvol[voice->channel] / 127, pan,
                         voice->prog.Vol, voice->prog.Pan, voice->tone.Vol, voice->tone.Pan);

    auto left = m_vm.AdjustVolToGroup(voice->basevol.left, voice->group);
    auto right = m_vm.AdjustVolToGroup(voice->basevol.right, voice->group);
    voice->SetVolume(left >> 1, right >> 1);
  }
}

void MidiHandler::SetPMod(s32 mod) {
  m_cur_pm = mod;

  for (auto& v : m_voices) {
    auto voice = v.lock();
    if (voice == nullptr) {
      continue;
    }

    voice->current_pm = m_cur_pm;
    auto note = PitchBend(voice->tone, voice->current_pb, voice->current_pm, voice->start_note,
                          voice->start_fine);
    auto pitch =
        PS1Note2Pitch(voice->tone.CenterNote, voice->tone.CenterFine, note.first, note.second);
    voice->SetPitch(pitch);
  }
}

void MidiHandler::MuteChannel(u8 channel) {
  // fmt::print("{:x} ame muting channel {}\n", (u64)this, channel);
  m_mute_state[channel] = true;
}

void MidiHandler::UnmuteChannel(u8 channel) {
  // fmt::print("{:x} ame unmuting channel {}\n", (u64)this, channel);
  m_mute_state[channel] = false;
}

void MidiHandler::NoteOn() {
  u8 channel = m_status & 0xf;
  u8 note = m_seq_ptr[0];
  u8 velocity = m_seq_ptr[1];

  if (velocity == 0) {
    NoteOff();
    return;
  }

  if (m_mute_state[channel]) {
    m_seq_ptr += 2;
    return;
  }

  // fmt::print("{:x} {}: [ch{:01x}] note on {:02x} {:02x}\n", (u64)this, m_time, channel, note,
  //            velocity);

  // Key on all the applicable tones for the program
  // FIXME bank from midi
  // auto bank = dynamic_cast<MusicBank*>(m_locator.get_bank_by_id(m_header->BankID));
  auto bank = static_cast<MusicBank*>(&m_bank);
  auto& program = bank->Progs[m_programs[channel]];

  for (auto& t : program.Tones) {
    if (note >= t.MapLow && note <= t.MapHigh) {
      s16 pan = m_chanpan[channel] + m_pan;
      if (pan >= 360) {
        pan -= 360;
      }

      auto voice = std::make_shared<midi_voice>(t, program);
      voice->basevol = m_vm.MakeVolumeB(m_vol, (velocity * m_chanvol[channel]) / 0x7f, pan,
                                        program.Vol, program.Pan, t.Vol, t.Pan);

      voice->note = note;
      voice->channel = channel;
      voice->velocity = velocity;

      voice->start_note = note;
      voice->start_fine = 0;

      voice->current_pm = m_pitch_bend[channel];
      voice->current_pb = m_cur_pm;

      voice->group = m_sound.VolGroup;
      m_vm.StartTone(voice);
      m_voices.emplace_front(voice);
    }
  }

  m_seq_ptr += 2;
}

void MidiHandler::NoteOff() {
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
      voice->KeyOff();
    }
  }

  m_seq_ptr += 2;
}

void MidiHandler::ProgramChange() {
  u8 channel = m_status & 0xf;
  u8 program = m_seq_ptr[0];

  m_programs[channel] = program;

  // fmt::print("{:x} {}: [ch{:01x}] program change {:02x} -> {:02x}\n", (u64)this, m_time, channel,
  // m_programs[channel], program);
  m_seq_ptr += 1;
}

void MidiHandler::ChannelPressure() {
  u8 channel = m_status & 0xf;
  u8 note = m_seq_ptr[0];
  // fmt::print("{}: channel pressure {:02x} {:02x}\n", m_time, m_status, m_seq_ptr[0]);

  for (auto& v : m_voices) {
    auto voice = v.lock();
    if (voice == nullptr) {
      continue;
    }

    if (voice->channel == channel && voice->note == note) {
      voice->KeyOff();
    }
  }

  m_seq_ptr += 1;
}

void MidiHandler::ChannelPitch() {
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
      auto note = PitchBend(voice->tone, voice->current_pb, voice->current_pm, voice->start_note,
                            voice->start_fine);
      auto pitch =
          PS1Note2Pitch(voice->tone.CenterNote, voice->tone.CenterFine, note.first, note.second);
      voice->SetPitch(pitch);
    }
  }

  m_seq_ptr += 2;
}

void MidiHandler::MetaEvent() {
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

void MidiHandler::ControllerChange() {
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
      throw MidiError(fmt::format("invalid midi controller change {}", controller));
      break;
  }

  m_seq_ptr += 2;
}

void MidiHandler::SystemEvent() {
  // fmt::print("{}: system event {:02x}\n", m_time, *m_seq_ptr);

  switch (*m_seq_ptr) {
    case 0x75:
      m_seq_ptr++;
      if (m_parent.has_value()) {
        auto [cont, ptr] = m_parent.value()->RunAME(*this, m_seq_ptr);
        m_seq_ptr = ptr;

        if (!cont) {
          // lg::debug("{:x} track stopped by ame", (u64)this);
          m_track_complete = true;
        }
      } else {
        throw MidiError("MIDI tried to run AME without an AME handler");
      }
      break;
    default:
      throw MidiError(fmt::format("Unknown system message {:02x}", *m_seq_ptr));
  }
}

bool MidiHandler::Tick() {
  if (m_paused) {
    return m_track_complete;
  }

  try {
    m_voices.remove_if([](auto& v) { return v.expired(); });
    Step();
  } catch (MidiError& e) {
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

void MidiHandler::NewDelta() {
  auto [len, delta] = ReadVLQ(m_seq_ptr);

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

void MidiHandler::Step() {
  if (m_get_delta) {
    NewDelta();
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
        NoteOff();
        break;
      case 0x9:
        NoteOn();
        break;
      case 0xB:
        ControllerChange();
        break;
      case 0xD:
        ChannelPressure();
        break;
      case 0xC:
        ProgramChange();
        break;
      case 0xE:
        ChannelPitch();
        break;
      case 0xF:
        // normal meta-event
        if (m_status == 0xFF) {
          MetaEvent();
          break;
        }
        if (m_status == 0xF0) {
          SystemEvent();
          break;
        }
        [[fallthrough]];
      default:
        throw MidiError(fmt::format("invalid status {}", m_status));
        return;
    }

    NewDelta();
  }
}

}  // namespace snd
