#pragma once

#include <string>

#include "common/common_types.h"

namespace jak3 {
void jak3_overlord_init_globals_isocommon();

struct CBaseFile;

struct ISOFileDef;

enum class EIsoStatus {
  NONE_0 = 0,
  IDLE_1 = 0x1,
  OK_2 = 0x2,  // already reading, or no need to read, or no mem to read.
  FAILED_TO_QUEUE_4 = 0x4,
  ERROR_OPENING_FILE_8 = 0x8,
  NULL_CALLBACK = 9,
  ERROR_NO_FILE = 0xa,
  ERROR_b = 0xb,  // tried to do BeginRead on a file that's not allocated.
  ERROR_NO_SOUND = 0xc,
};

struct SoundBankInfo;

struct ISO_Hdr {
  int unka;
  int unkb;
  EIsoStatus status;
  int8_t active_a;
  int8_t active_b;
  int8_t active_c;
  int8_t pad;

  enum class MsgType : u32 {
    MSG_0 = 0,
    LOAD_EE = 0x100,
    LOAD_IOP = 0x101,
    LOAD_EE_CHUNK = 0x102,
    LOAD_SOUNDBANK = 0x103,
    DGO_LOAD = 0x200,
    VAG_QUEUE = 0x400,
    VAG_STOP = 0x402,
    VAG_PAUSE = 0x403,
    VAG_UNPAUSE = 0x404,
    VAG_SET_PITCH_VOL = 0x406,
    PLAY_MUSIC_STREAM = 0x408,
    ABADBABE = 0xabadbabe,
    ADEADBEE = 0xadeadbee,
  } msg_type = MsgType::MSG_0;

  int mbox_reply;
  int thread_to_wake;

  EIsoStatus (*callback)(ISO_Hdr*) = nullptr;
  CBaseFile* m_pBaseFile = nullptr;
  int priority = 0;
  const ISOFileDef* file_def = nullptr;
};

struct ISO_LoadCommon : public ISO_Hdr {
  u8* addr = 0;  // 44
  int maxlen = 0;
  int length_to_copy = 0;  // 52
  u8* dest_ptr = 0;        // 68 (not truly common??) what are they doing here.
  int progress_bytes = 0;  // 72
};

struct ISO_LoadSingle : public ISO_LoadCommon {
  // addr
  // maxlen
  // maybe size
  int sector_offset = 0;  // 56
  // 60
  // 64
  // dest_ptr
  // maybe ofset
};

struct ISO_LoadSoundbank : public ISO_LoadCommon {
  const char* name = nullptr;  // 60
  int priority;                // 64
  SoundBankInfo* bank_info = nullptr;
};

struct CPage;

struct BlockParams {
  void* destination;
  int num_sectors;
  int sector_num;
  // ADDED
  const ISOFileDef* file_def;
  CPage* page;
  char* flag;
};

struct CDescriptor;

struct Block {
  BlockParams params;
  CDescriptor* descriptor;
  Block* next;
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