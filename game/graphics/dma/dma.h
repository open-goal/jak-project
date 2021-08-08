#pragma once

/*!
 * @file dma.h
 * Unpacking utils for PS2 DMA, VIF, and GIF data.
 */

#include <string>
#include <cstring>
#include "common/util/assert.h"
#include "common/common_types.h"

struct DmaTag {
  enum class Kind : u8 {
    REFE = 0,
    CNT = 1,
    NEXT = 2,
    REF = 3,
    REFS = 4,
    CALL = 5,
    RET = 6,
    END = 7
  };

  DmaTag(u64 value) {
    spr = (value >> 63);
    addr = (value >> 32) & 0x7fffffff;
    qwc = value & 0xfff;
    kind = Kind((value >> 28) & 0b111);
  }

  u16 qwc = 0;
  u32 addr = 0;
  bool spr = false;
  Kind kind;

  std::string print();
};

struct VifCode {
  enum class Kind : u8 {
    NOP = 0b0,
    STCYCL = 0b1,
    OFFSET = 0b10,
    BASE = 0b11,
    ITOP = 0b100,
    STMOD = 0b101,
    MSK3PATH = 0b110,
    MARK = 0b111,
    FLUSHE = 0b10000,
    FLUSH = 0b10001,
    FLUSHA = 0b10011,
    MSCAL = 0b10100,
    MSCNT = 0b10111,
    MSCALF = 0b10101,
    STMASK = 0b100000,
    STROW = 0b110000,
    STCOL = 0b110001,
    MPG = 0b1001010,
    DIRECT = 0b1010000,
    DIRECTHL = 0b1010001,
    UNPACK_MASK = 0b1100000  // unpack is a bunch of commands.
  };

  VifCode(u32 value) {
    interrupt = (value) & (1 << 31);
    kind = (Kind)((value >> 24) & 0b111'1111);
    num = (value >> 16) & 0xff;
    immediate = value & 0xffff;
  }

  bool interrupt = false;
  Kind kind;
  u16 num;
  u16 immediate;

  std::string print();
};
