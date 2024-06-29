#pragma once

#include <string>

#include "common/common_types.h"

namespace jak3 {
void jak3_overlord_init_globals_isocommon();

struct CBaseFile;

struct ISOFileDef;

struct ISO_Hdr {
  int unka;
  int unkb;
  int status;
  int8_t active_a;
  int8_t active_b;
  int8_t active_c;
  int8_t pad;

  enum class MsgType {
    MSG_0 = 0,
    MSG_0x100 = 0x100,
    MSG_0x101 = 0x101,
    MSG_0x102 = 0x102,
    MSG_0x103 = 0x103,
  } msg_type = MsgType::MSG_0;

  int mbox_reply;
  int thread_to_wake;

  void (*callback)(ISO_Hdr*) = nullptr;
  CBaseFile* m_pBaseFile = nullptr;
  int unkc;
  ISOFileDef* file_def = nullptr;
};

struct CPage;

struct BlockParams {
  void* destination;
  int num_sectors;
  int sector_num;
  CPage* page;
  char* flag;
};

struct ISOName {
  char data[12];

  bool operator==(const ISOName& other) {
    for (int i = 0; i < 12; i++) {
      if (data[i] != other.data[i]) {
        return false;
      }
    }
    return true;
  }
};

struct ISOFileDef {
  ISOName name;
  std::string full_path;  // pc
  u32 length;
};

struct VagDirEntry {
  u32 words[2];
};

struct VagDir {
  int vag_magic_1 = 0;
  int vag_magic_2 = 0;
  int vag_version = 0;
  int num_entries = 0;
  VagDirEntry entries[4096];
};
static_assert(sizeof(VagDir) == 0x8010);

constexpr int MUSIC_TWEAK_COUNT = 0x40;
struct MusicTweaks {
  u32 TweakCount = 0;
  struct {
    char MusicName[12] = "\0";
    s32 VolumeAdjust = 0;
  } MusicTweak[MUSIC_TWEAK_COUNT];
};

int PackVAGFileName(u32* out, const char* name);
void MakeISOName(ISOName* dst, const char* src);

}  // namespace jak3