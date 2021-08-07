#pragma once

/*!
 * @file dma.h
 * Unpacking utils for PS2 DMA, VIF, and GIF data.
 */

#include <string>
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