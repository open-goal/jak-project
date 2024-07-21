#pragma once

#include "common/common_types.h"

#include "game/overlord/jak3/pagemanager.h"

namespace jak3 {
/* TODO check values */
enum class EFileComp { MODE0, MODE1, KNOWN_NOT_BLZO };

struct ISOBuffer {
 public:
  void AdjustDataLength(int);
  void AdvanceCurrentData(int);
  void SetDataLength(int length) { m_nDataLength = length; }
  int GetDataLength() { return m_nDataLength; }

 private:
  CPageManager::CPageList* m_pActivePages;
  u8* decomp_buffer;
  int decompressed_size;
  int m_nDataLength;
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

static constexpr int MAX_VAGDIR_ENTRIES = 4096;

struct VagDirJak3 {
  u32 id[2];
  u32 version;
  u32 count;
  VagDirEntryJak3 entries[MAX_VAGDIR_ENTRIES];
};

struct VagDirEntry {};

struct ISOFileDef {};
}  // namespace jak3
