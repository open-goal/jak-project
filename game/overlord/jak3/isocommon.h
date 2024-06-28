#pragma once

#include <string>

#include "common/common_types.h"

namespace jak3 {
void jak3_overlord_init_globals_isocommon();

struct ISO_Hdr {
  enum class MsgType {
    MSG_0x100 = 0x100,
    MSG_0x101 = 0x101,
    MSG_0x102 = 0x102,
    MSG_0x103 = 0x103,
  } msg_type;

  void (*callback)();
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
}  // namespace jak3