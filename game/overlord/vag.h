#ifndef VAG_H_
#define VAG_H_

#include "list.h"

#include "common/common_types.h"

#include "game/overlord/isocommon.h"

struct VagCommand2 {
  char unk0xd4;
  char unk0xda;
  char unk0xe8;
  int unk0x120;
  int unk0x124;
};

struct VagStream {
  ListElement l;
  char name[48];
  int unk0x3c;
  u32 id;
  int unk0x44;
  int unk0x48;
  int unk0x4c;
  int unk0x50;
  int unk0x54;
  int unk0x58;
  int unk0x5c;
  int unk0x60;
  int unk0x64;
};

VagCommand2* FindThisVagStream(const char* name, u32 id);

#endif  // VAG_H_
