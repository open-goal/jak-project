// Copyright: 2021 - 2024, Ziemas
// SPDX-License-Identifier: ISC
#pragma once

#include <memory>
#include <span>
#include <vector>

#include "soundbank.h"

#include "common/common_types.h"
#include "common/util/BinaryReader.h"

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
  SoundBank* GetBankByHandle(BankHandle id);
  SoundBank* GetBankByName(const char* name);
  SoundBank* GetBankWithSound(const char* name);

  void UnloadBank(BankHandle id);

  BankHandle BankLoad(std::span<u8> bank);

 private:
  std::vector<std::unique_ptr<SoundBank>> mBanks;
};
}  // namespace snd
