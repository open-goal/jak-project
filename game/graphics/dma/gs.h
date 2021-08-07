#pragma once

#include "game/graphics/dma/dma.h"

struct GifTag {
  enum class Format : u8 { PACKED = 0, REGLIST = 1, IMAGE = 2, DISABLE = 3 };

  enum class RegisterDescriptor : u8 {
    PRIM = 0,
    RGBAQ = 1,
    ST = 2,
    UV = 3,
    XYZF2 = 4,
    XYZ2 = 5,
    TEX0_1 = 6,
    TEX0_2 = 7,
    CLAMP_1 = 8,
    CLAMP_2 = 9,
    FOG = 10,
    // no 11
    XYZF3 = 12,
    XYZ3 = 13,
    AD = 14,
    NOP = 15,
  };

  u32 nloop() const {
    return data[0] & 0x7f;  // 15 bits
  }

  bool eop() const { return data[0] & (1ull << 15); }
  bool pre() const { return data[0] & (1ull << 46); }
  u32 prim() const { return (data[0] >> 47) & 0b111'1111'1111; }
  Format flg() const { return (Format)((data[0] >> 58) & 0b11); }

  u32 nreg() const {
    u32 result = (data[0] >> 60) & 0b1111;
    if (!result) {
      return 16;
    } else {
      return result;
    }
  }

  RegisterDescriptor reg(u32 idx) const {
    return (RegisterDescriptor)((data[1] >> (4 * idx)) & 0b1111);
  }

  std::string print() const;

  GifTag(const u8* ptr) { memcpy(data, ptr, 16); }

  u64 data[2];
};

std::string reg_descriptor_name(GifTag::RegisterDescriptor reg);

enum class GsRegisterAddress : u8 {
  PRIM = 0,
  RGBAQ = 1,
  ST = 2,
  UV = 3,
  XYZF2 = 4,
  XYZ2 = 5,
  TEX0_1 = 6,
  TEX0_2 = 7,
  CLAMP_1 = 8,
  CLAMP_2 = 9,
  FOG = 0xa,
  XYZF3 = 0xc,
  XYZ3 = 0xd,
  TEX1_1 = 0x14,
  TEX1_2 = 0x15,
  TEX2_1 = 0x16,
  TEX2_2 = 0x17,
  XYOFFSET_1 = 0x18,
  XYOFFSET_2 = 0x19,
  PRMODECONT = 0x1a,
  PRMODE = 0x1b,
  TEXCLUT = 0x1c,
  SCANMSK = 0x22,
  MIPTBP1_1 = 0x34,
  MIPTBP1_2 = 0x35,
  MIPTBP2_1 = 0x36,
  MIPTBP2_2 = 0x37,
  TEXA = 0x3b,
  FOGCOL = 0x3d,
  TEXFLUSH = 0x3f,
  SCISSOR_1 = 0x40,
  SCISSOR_2 = 0x41,
  ALPHA_1 = 0x42,
  ALPHA_2 = 0x43,
  DIMX = 0x44,
  DTHE = 0x45,
  COLCLAMP = 0x46,
  TEST_1 = 0x47,
  TEST_2 = 0x48,
  PABE = 0x49,
  FBA_1 = 0x4a,
  FBA_2 = 0x4b,
  FRAME_1 = 0x4c,
  FRAME_2 = 0x4d,
  ZBUF_1 = 0x4e,
  ZBUF_2 = 0x4f,
  BITBLTBUF = 0x50,
  TRXPOS = 0x51,
  TRXREG = 0x52,
  TRXDIR = 0x53,
  HWREG = 0x54,
  SIGNAL = 0x60,
  FINISH = 0x61,
  LABEL = 0x62
};

enum class TextureFormat { PSMZ32, PSMZ24, PSMZ16, PSMZ16S };

std::string register_address_name(GsRegisterAddress reg);

struct GsZbuf {
  GsZbuf(u64 val) : data(val) {}

  u32 zbp() const { return data & 0b1'1111'1111; }

  TextureFormat psm() const {
    u32 psm_field = (data >> 24) & 0b1111;
    switch (psm_field) {
      case 0b0000:
        return TextureFormat::PSMZ32;
      case 0b0001:
        return TextureFormat::PSMZ24;
      case 0b0010:
        return TextureFormat::PSMZ16;
      case 0b1010:
        return TextureFormat::PSMZ16S;
      default:
        assert(false);
    }
  }

  bool zmsk() const { return data & (1ull << 32); }

  u64 data;
};

struct GsTest {
  GsTest() = default;
  GsTest(u64 val) : data(val) {}

  bool alpha_test_enable() const { return data & 1; }

  enum class AlphaTest : u8 {
    NEVER = 0,
    ALWAYS = 1,
    LESS = 2,
    LEQUAL = 3,
    EQUAL = 4,
    GEQUAL = 5,
    GREATER = 6,
    NOTEQUAL = 7
  };

  AlphaTest alpha_test() const { return (AlphaTest)((data >> 1) & 0b111); }

  u8 aref() const { return (data >> 4); }

  enum class AlphaFail : u8 { KEEP = 0, FB_ONLY = 1, ZB_ONLY = 2, RGB_ONLY = 3 };

  AlphaFail afail() const { return (AlphaFail)((data >> 12) & 0b11); }

  bool date() const { return data & (1 << 14); }

  bool datm() const { return data & (1 << 15); }

  bool zte() const { return data & (1 << 16); }

  enum class ZTest : u8 { NEVER = 0, ALWAYS = 1, GEQUAL = 2, GREATER = 3 };

  ZTest ztest() const { return (ZTest)((data >> 17) & 0b11); }

  std::string print() const;

  u64 data = 0;

  bool operator==(const GsTest& other) const { return data == other.data; }
  bool operator!=(const GsTest& other) const { return data != other.data; }
};

struct GsAlpha {
  GsAlpha() = default;
  GsAlpha(u64 val) : data(val) {}

  enum class BlendMode {
    SOURCE = 0,
    DEST = 1,           // frame buffer
    ZERO_OR_FIXED = 2,  // 0 for a, b, d, fixed for c
    INVALID = 3
  };

  BlendMode a_mode() const { return (BlendMode)(data & 0b11); }
  BlendMode b_mode() const { return (BlendMode)((data >> 2) & 0b11); }
  BlendMode c_mode() const { return (BlendMode)((data >> 4) & 0b11); }
  BlendMode d_mode() const { return (BlendMode)((data >> 6) & 0b11); }
  u8 fix() const { return (data >> 32); }

  std::string print() const;

  u64 data = 0;
  bool operator==(const GsAlpha& other) const { return data == other.data; }
  bool operator!=(const GsAlpha& other) const { return data != other.data; }
};

struct GsPrim {
  GsPrim() = default;
  GsPrim(u64 val) : data(val & 0b111'1111'1111) {}

  enum class Kind {
    POINT = 0,
    LINE = 1,
    LINE_STRIP = 2,
    TRI = 3,
    TRI_STRIP = 4,
    TRI_FAN = 5,
    SPRITE = 6,
    PRIM_7 = 7
  };

  Kind kind() const { return (Kind)(data & 0b111); }

  bool gouraud() const {  // iip
    return data & (1 << 3);
  }

  bool tme() const { return data & (1 << 4); }
  bool fge() const { return data & (1 << 5); }
  bool abe() const { return data & (1 << 6); }
  bool aa1() const { return data & (1 << 7); }
  bool fst() const { return data & (1 << 8); }
  bool ctxt() const { return data & (1 << 9); }
  bool fix() const { return data & (1 << 10); }

  u64 data = 0;

  bool operator==(const GsPrim& other) const { return data == other.data; }
  bool operator!=(const GsPrim& other) const { return data != other.data; }
};