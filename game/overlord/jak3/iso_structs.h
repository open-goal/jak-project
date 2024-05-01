#pragma once

#include "common/common_types.h"

namespace jak3 {
/* TODO check values */
enum class EFileComp { MODE0, MODE1, KNOWN_NOT_BLZO };

struct ISOBuffer {
  void AdjustDataLength(int);
  void AdvanceCurrentData(int);
};

struct VagDirEntryJak3 {
  union {
    u64 data;
    struct {
      u64 name : 42;
      bool stereo : 1;
      bool international : 1;
      u8 param : 4;
      u64 offset : 16;
    };
  };
};

struct VagDirJak3 {
  u32 id[2];
  u32 version;
  u32 count;
  VagDirEntryJak3 entries[0];
};

struct VagDirEntry {};

struct ISOFileDef {};
}  // namespace jak3
