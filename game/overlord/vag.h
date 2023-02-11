#ifndef VAG_H_
#define VAG_H_

#include <array>

#include "list.h"

#include "common/common_types.h"

#include "game/overlord/isocommon.h"

extern u32 StreamSRAM[4];
extern u32 TrapSRAM[4];
extern u8 VAG_SilentLoop[0x60];
extern int StreamVoice[4];

struct VagCommand2 : public IsoMessage {
  char name[48];
  u32 id;
  /* 0x30 */ VagCommand2* sibling;
  u32 unk0x28;
  u32 unk0x2c;
  u32 unk0x34;
  s32 unk0x38;
  int unk0x3c;
  u32 unk0x78;
  u32 unk0x7c;
  int unk0x80;
  int unk0x84;
  int unk0x88;
  int unk0x8c;
  u32 unk0xb0;
  u32 unk0xb4;
  u32 unk0xb8;
  u32 unk0xbc;
  u32 unk0xc0;
  u32 unk0xc4;
  u32 unk0xc8;
  u32 unk0xcc;

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
      char unk0xdb;
      char unk0xdc;
      char unk0xdd;
      char unk0xde;
      char unk0xdf;
      char unk0xe0;
      char unk0xe1;
      char unk0xe2;
      char unk0xe3;
      char unk0xe4;
      char unk0xe5;
      char unk0xe6;
      char unk0xe7;
    };
  };

  char unk0xe8;
  int unk0xec;
  u32 unk0xf0;
  u32 unk0xf4;
  u32 unk0xf8;
  s32 unk0x100;
  u32 unk0x104;
  u32 unk0x108;
  u32 unk0x10c;
  /* 0x110 */ s32 volume;
  s32 unk0x114;
  s32 unk0x118;
  s32 unk0x11c;
  int unk0x120;
  int unk0x124;
  /* 0x128 */ int positioned;
  /* 0x12c */ Vec3w trans;
  /* 0x138 */ s32 fo_min;
  /* 0x13c */ s32 fo_max;
  /* 0x140 */ s32 fo_curve;
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
