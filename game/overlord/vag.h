#ifndef VAG_H_
#define VAG_H_

#include <array>

#include "list.h"

#include "common/common_types.h"

#include "game/overlord/isocommon.h"

struct VagCommand2 {
  char name[48];
  u32 id;
  union {
    /* 0xd0 */ char stat_arr[25];
    struct {
      /* 0xd0 */ char unk0xd0;
      char unk0xd1;
      char unk0xd2;
      char unk0xd3;
      char unk0xd4;
      char unk0xd5;
      char unk0xd6;
      char unk0xd7;
      char unk0xd8;
      char unk0xd9;
      char unk0xda;
    };
  };
  u32 unk0xc0;
  u32 unk0xc8;
  char unk0xe8;
  s32 unk0x100;
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

extern std::array<VagCommand2, 4> gVagCmds;

VagCommand2* FindThisVagStream(const char* name, u32 id);
u32 CalculateVAGPitch(u32 a1, s32 a2);

#endif  // VAG_H_
