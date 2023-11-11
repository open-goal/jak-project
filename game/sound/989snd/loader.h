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
#include "common/util/BinaryReader.h"

#include "../common/synth.h"

#include "third-party/span.hpp"

namespace snd {

class FileAttributes {
 public:
  struct LocAndSize {
    u32 offset;
    u32 size;
  };

  u32 type;
  u32 num_chunks;
  std::vector<LocAndSize> where;
  void Read(BinaryReader& data);
};

class Loader {
 public:
  SoundBank* get_bank_by_handle(u32 id);
  SoundBank* get_bank_by_name(const char* name);
  SoundBank* get_bank_with_sound(const char* name);

  void unload_bank(u32 id);

  u32 BankLoad(nonstd::span<u8> bank);
  void load_midi(std::fstream& in);

  bool read_midi();

 private:
  id_allocator m_id_allocator;
  std::unordered_map<u32, std::unique_ptr<SoundBank>> m_soundbanks;
};
}  // namespace snd
