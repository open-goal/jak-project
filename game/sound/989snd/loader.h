// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#pragma once

#include <memory>
#include <vector>

#include "handle_allocator.h"
#include "musicbank.h"
#include "sfxblock.h"
#include "sound_handler.h"
#include "soundbank.h"

#include "common/common_types.h"

#include "../common/synth.h"

namespace snd {

#define FOURCC(a, b, c, d) ((u32)(((d) << 24) | ((c) << 16) | ((b) << 8) | (a)))
struct LocAndSize {
  /*   0 */ u32 offset;
  /*   4 */ u32 size;
};

template <size_t chunks>
struct FileAttributes {
  /*   0 */ u32 type;
  /*   4 */ u32 num_chunks;
  /*   8 */ LocAndSize where[chunks];
};

class loader : public locator {
 public:
  SoundBank* get_bank_by_handle(u32 id) override;
  MusicBank* get_bank_by_id(u32 id) override;
  MIDIBlock* get_midi(u32 id) override;
  u8* get_bank_samples(u32 id) override;

  SoundBank* get_bank_by_name(const char* name);
  SoundBank* get_bank_with_sound(const char* name);

  void unload_bank(u32 id);

  u32 read_bank(std::fstream& in);
  void load_midi(std::fstream& in);

  bool read_midi();

 private:
  void load_samples(u32 bank, std::unique_ptr<u8[]> samples);

  id_allocator m_id_allocator;
  std::unordered_map<u32, std::unique_ptr<SoundBank>> m_soundbanks;

  std::vector<std::unique_ptr<u8[]>> m_midi_chunks;

  std::unordered_map<u32, MIDIBlock*> m_midi;

  u32 m_next_id{0};
};
}  // namespace snd
