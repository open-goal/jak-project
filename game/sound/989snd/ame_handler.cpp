// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#include "ame_handler.h"

#include "common/log/log.h"

#include "game/sound/989snd/blocksound_handler.h"

namespace snd {

// added!
u64 SoundFlavaHack = 0;
u8 GlobalExcite = 0;

ame_handler::ame_handler(MultiMIDIBlockHeader* block,
                         voice_manager& vm,
                         MIDISound& sound,
                         s32 vol,
                         s32 pan,
                         locator& loc,
                         SoundBank& bank)
    : m_sound(sound),
      m_bank(bank),
      m_header(block),
      m_locator(loc),
      m_vm(vm),
      m_repeats(sound.Repeats) {
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

  start_segment(0);
};

bool ame_handler::tick() {
  for (auto it = m_midis.begin(); it != m_midis.end();) {
    bool done = it->second->tick();
    if (done) {
      it = m_midis.erase(it);
    } else {
      it++;
    }
  }

  return m_midis.empty();
};

void ame_handler::start_segment(u32 id) {
  if (m_midis.find(id) == m_midis.end()) {
    auto midiblock = (MIDIBlockHeader*)(m_header->BlockPtr[id] + (uintptr_t)m_header);
    auto sound_handler = (MIDISoundHandler*)((uintptr_t)midiblock + sizeof(MIDIBlockHeader));

    // Skip adding if not midi type
    u32 type = (sound_handler->OwnerID >> 24) & 0xf;
    if (type == 1 || type == 3) {
      m_midis.emplace(id, std::make_unique<midi_handler>(midiblock, m_vm, m_sound, m_vol, m_pan,
                                                         m_locator, m_bank, this));
    }
  }
}

void ame_handler::stop() {
  for (auto it = m_midis.begin(); it != m_midis.end();) {
    it->second->stop();
    it = m_midis.erase(it);
  }
}

void ame_handler::stop_segment(u32 id) {
  auto m = m_midis.find(id);
  if (m == m_midis.end())
    return;

  m->second->stop();
}

void ame_handler::pause() {
  for (auto& m : m_midis) {
    m.second->pause();
  }
}

void ame_handler::unpause() {
  for (auto& m : m_midis) {
    m.second->unpause();
  }
}

void ame_handler::set_vol_pan(s32 vol, s32 pan) {
  if (vol >= 0) {
    if (vol != VOLUME_DONT_CHANGE) {
      m_vol = (m_sound.Vol * vol) >> 10;
    }
  } else {
    m_vol = -vol;
  }

  if (m_vol >= 128) {
    m_vol = 127;
  }

  if (pan == PAN_RESET) {
    m_pan = m_sound.Pan;
  } else if (pan != PAN_DONT_CHANGE) {
    m_pan = pan;
  }

  for (auto& m : m_midis) {
    m.second->set_vol_pan(vol, pan);
  }
}

void ame_handler::set_pmod(s32 mod) {
  for (auto& m : m_midis) {
    m.second->set_pmod(mod);
  }
}

#define AME_BEGIN(op) \
  if (skip) {         \
    if (skip == 1) {  \
      skip = 0;       \
    }                 \
  } else              \
    do {
#define AME_END(x) \
  }                \
  while (0)        \
    ;              \
  stream += (x);

std::pair<bool, u8*> ame_handler::run_ame(midi_handler& midi, u8* stream) {
  int skip = 0;
  bool done = false;
  bool cont = true;

  // fmt::print("AME SCRIPT ----\n");
  // u8* dbgstream = stream;
  // while (!done) {
  //   fmt::print("{:02x} ", *dbgstream);
  //   dbgstream++;

  //  if (*dbgstream == 0xf7) {
  //    dbgstream++;
  //    done = true;
  //  }
  //}
  // done = false;
  // fmt::print("\n -------\n");

  while (!done) {
    auto op = static_cast<u8>(*stream++);
    switch (op) {
      case 0x0: {
        AME_BEGIN(op)
        if (GlobalExcite <= (stream[0] + 1)) {
          skip = 1;
        }
        AME_END(1)
      } break;
      case 0x1: {
        AME_BEGIN(op)
        if (GlobalExcite != (stream[0] + 1)) {
          skip = 1;
        }
        AME_END(1)
      } break;
      case 0x2: {
        AME_BEGIN(op)
        if (GlobalExcite > (stream[0] + 1)) {
          skip = 1;
        }
        AME_END(1)
      } break;
      case 0x3: {
        AME_BEGIN(op)
        stop_segment(stream[0]);
        AME_END(1)
      } break;
      case 0x4: {
        // fmt::print("ame trace 4\n");
        if (skip == 1) {
          skip = 2;
        }
      } break;
      case 0x5: {
        // fmt::print("ame trace 5\n");
        if (skip == 2) {
          skip = 0;
        }
      } break;
      case 0x6: {
        AME_BEGIN(op)
        if (m_register[stream[0]] > (stream[1] - 1)) {
          skip = 1;
        }
        AME_END(2)
      } break;
      case 0x7: {
        AME_BEGIN(op)
        if (m_register[stream[0]] < (stream[1] + 1)) {
          skip = 1;
        }
        AME_END(2)
      } break;
      case 0xB: {
        // fmt::print("ame trace b\n");
        m_macro[stream[0]] = &stream[1];
        while (*stream != 0xf7) {
          stream++;
        }
        stream++;
      } break;
      case 0xc: {
        AME_BEGIN(op)
        auto [sub_cont, ptr] = run_ame(midi, m_macro[stream[0]]);
        if (!sub_cont) {
          cont = false;
          done = true;
        }
        AME_END(1)
      } break;
      case 0xd: {
        AME_BEGIN(op)
        cont = false;
        done = true;
        start_segment(m_register[stream[0]] - 1);
        AME_END(1)
      } break;
      case 0xe: {
        AME_BEGIN(op)
        start_segment(m_register[stream[0]] - 1);
        AME_END(1)
      } break;
      case 0xf: {
        // fmt::print("ame trace f\n");
        if (skip) {
          while (*stream != 0x7f) {
            stream++;
          }
          stream++;
          if (skip == 1)
            skip = 0;
        } else {
          auto group = *stream++;
          m_groups[group].basis = *stream++;
          u8 channel = 0;
          while (*stream != 0xf7) {
            m_groups[group].channel[channel] = *stream++;
            m_groups[group].excite_min[channel] = *stream++;
            m_groups[group].excite_max[channel] = *stream++;
            channel++;
          }
          m_groups[group].num_channels = channel;
          stream++;
        }
      } break;
      case 0x10: {
        AME_BEGIN(op)
        u8 group = stream[0];
        u8 comp = 0;
        if (m_groups[group].basis == 0) {
          comp = GlobalExcite;
        } else {
          comp = m_register[m_groups[group].basis - 1];
        }
        // fmt::print("group: {} basis: {} excite: {}\n", group, m_groups[group].basis, comp);
        for (int i = 0; i < m_groups[group].num_channels; i++) {
          // auto xmin = m_groups[group].excite_min[i];
          // auto xmax = m_groups[group].excite_max[i];
          // fmt::print("chan {} excite: {}-{}\n", i, xmin, xmax);

          // note : added hack here! :-)
          if (!SoundFlavaHack &&
              (comp < m_groups[group].excite_min[i] || comp > m_groups[group].excite_max[i])) {
            midi.mute_channel(m_groups[group].channel[i]);
          } else {
            midi.unmute_channel(m_groups[group].channel[i]);
          }
        }
        AME_END(1)
      } break;
      case 0x11: {
        AME_BEGIN(op)
        done = true;
        cont = false;
        start_segment(stream[0]);
        AME_END(1)
      } break;
      case 0x12: {
        AME_BEGIN(op)
        start_segment(stream[0]);
        AME_END(1)
      } break;
      case 0x13: {
        AME_BEGIN(op)
        m_register[stream[0]] = stream[1];
        AME_END(2)
      } break;
      case 0x14: {
        AME_BEGIN(op)
        if (m_register[stream[0]] < 0x7f) {
          m_register[stream[0]]++;
        }
        AME_END(1)
      } break;
      case 0x15: {
        AME_BEGIN(op)
        if (m_register[stream[0]] > 0) {
          m_register[stream[0]]--;
        }
        AME_END(1)
      } break;
      case 0x16: {
        AME_BEGIN(op)
        if (m_register[stream[0]] != stream[1]) {
          // if (stream[0] == 3) {
          //   fmt::print("AME x16 reg[{}] == {}\n", stream[0], stream[1]);
          // }
          skip = 1;
        }
        AME_END(2)
      } break;
      default: {
        throw ame_error(fmt::format("Unhandled AME event {:02x}", (u8)op));
      } break;
    }

    if (*stream == 0xf7) {
      // fmt::print("ame done\n");
      stream++;
      done = true;
    }
  }

  return {cont, stream};
}

}  // namespace snd
#undef AME_BEGIN
#undef AME_END
