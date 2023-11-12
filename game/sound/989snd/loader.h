// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#pragma once

#include <memory>
#include <vector>

#include "soundbank.h"

#include "common/common_types.h"
#include "common/util/BinaryReader.h"

#include "third-party/span.hpp"

namespace snd {

using BankHandle = SoundBank*;

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
  SoundBank* get_bank_by_handle(BankHandle id);
  SoundBank* get_bank_by_name(const char* name);
  SoundBank* get_bank_with_sound(const char* name);

  void unload_bank(BankHandle id);

  BankHandle BankLoad(nonstd::span<u8> bank);
  void load_midi(std::fstream& in);

  bool read_midi();

 private:
  std::vector<std::unique_ptr<SoundBank>> mBanks;
};
}  // namespace snd
