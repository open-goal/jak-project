#pragma once

#include "common/dma/dma.h"

/*!
 * @file gs.h
 * PS2 GS/GIF hardware types
 */

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
    return data[0] & 0x7fff;  // 15 bits
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
  GifTag() = default;

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
std::string register_address_name(u32 reg);

struct GsZbuf {
  GsZbuf(u64 val) : data(val) {}
  GsZbuf() = default;

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
        ASSERT(false);
    }
  }

  bool zmsk() const { return data & (1ull << 32); }

  u64 data;
};

struct GsScissor {
  GsScissor(u64 val) : data(val) {}
  u32 x0() const { return data & 0b11111111111; }
  u32 x1() const { return (data >> 16) & 0b11111111111; }
  u32 y0() const { return (data >> 32) & 0b11111111111; }
  u32 y1() const { return (data >> 48) & 0b11111111111; }
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

  std::string print() const;
};

struct GsTex0 {
  GsTex0() = default;
  GsTex0(u64 val) : data(val) {}

  u32 tbp0() const { return data & 0b11'1111'1111'1111; }
  u32 tbw() const { return (data >> 14) & 0b111111; }
  enum class PSM {
    PSMCT32 = 0,
    PSMCT24 = 1,
    PSMCT16 = 2,
    PSMCT16S = 0b1010,
    PSMT8 = 0b10011,
    PSMT4 = 0b10100,
    PSMT8H = 0b011011,
    PSMT4HL = 0b100100,
    PSMT4HH = 0b101100,
    PSMZ32 = 0b110000,
    PSMZ24 = 0b110001,
    PSMZ16 = 0b110010,
    PSMZ16S = 0b111010
  };
  PSM psm() const { return (PSM)((data >> 20) & 0b111111); }
  u32 tw() const { return (data >> 26) & 0b1111; }
  u32 th() const { return (data >> 30) & 0b1111; }
  enum class TextureFunction : u8 {
    MODULATE = 0,
    DECAL = 1,
    HIGHLIGHT = 2,
    HIGHLIGHT2 = 3,
  };
  u32 tcc() const { return ((data >> 34) & 0b1); }
  TextureFunction tfx() const { return (TextureFunction)((data >> 35) & 0b11); }
  u32 cbp() const { return (data >> 37) & 0b11'1111'1111'1111; }
  u32 cpsm() const { return (data >> 51) & 0b1111; }
  u32 csm() const { return (data >> 55) & 1; }

  std::string print() const;

  bool operator==(const GsTex0& other) const { return data == other.data; }
  bool operator!=(const GsTex0& other) const { return data != other.data; }

  u64 data = 0;
};

struct GsTex1 {  // tex1_1 and tex1_2
  GsTex1() = default;
  GsTex1(u64 val) : data(val) {}

  bool lcm() const {
    // 1 = fixed
    return data & 1;
  }

  u32 mxl() const { return (data >> 2) & 0b111; }
  bool mmag() const { return data & (1 << 5); }
  u32 mmin() const { return (data >> 6) & 0b111; }
  bool mtba() const { return data & (1 << 9); }
  u32 l() const { return (data >> 19) & 0b11; }
  u32 k() const { return (data >> 32) & 0b1111'1111'1111; }

  std::string print() const;

  u64 data = 0;
};

struct GsTexa {
  GsTexa() = default;
  GsTexa(u64 val) : data(val) {}

  u8 ta0() const { return data; }
  bool aem() const { return data & (1 << 15); }
  u8 ta1() const { return (data >> 32); }

  std::string print() const;

  u64 data = 0;
};

struct GsFrame {
  enum class PSM {
    PSMCT32 = 0,
    PSMCT24 = 1,
    PSMCT16 = 2,
    PSMCT16S = 0b1010,
    PSMT8 = 0b10011,
    PSMT4 = 0b10100,
    PSMT8H = 0b011011,
    PSMT4HL = 0b100100,
    PSMT4HH = 0b101100,
    PSMZ32 = 0b110000,
    PSMZ24 = 0b110001,
    PSMZ16 = 0b110010,
    PSMZ16S = 0b111010
  };

  GsFrame() = default;
  GsFrame(u64 val) : data(val) {}

  // Frame buffer base pointer (word address / 2048)
  u32 fbp() const { return (data & 0b1'1111'1111); }
  // Frame buffer width (pixels / 64)
  u32 fbw() const { return ((data >> 16) & 0b11'1111); }
  // Frame buffer pixel storage format
  PSM psm() const { return (PSM)((data >> 24) & 0b11'1111); }
  // Frame buffer drawing mask
  u32 fbmsk() const { return ((data >> 32) & 0xFFFF'FFFF); }

  std::string print() const;

  u64 data = 0;
};

struct GsXYOffset {
  GsXYOffset() = default;
  GsXYOffset(u64 val) : data(val) {}

  u32 ofx() const { return data & 0xFFFF; }
  u32 ofy() const { return (data >> 32) & 0xFFFF; }

  std::string print() const;

  u64 data = 0;
};

// not including the giftag
struct AdGifData {
  u64 tex0_data;
  u64 tex0_addr;
  u64 tex1_data;
  u64 tex1_addr;
  u64 mip_data;
  u64 mip_addr;
  u64 clamp_data;  // can also be zbuf!!
  u64 clamp_addr;
  u64 alpha_data;
  u64 alpha_addr;

  bool is_normal_adgif() const {
    return (u8)tex0_addr == (u32)GsRegisterAddress::TEX0_1 &&
           (u8)tex1_addr == (u32)GsRegisterAddress::TEX1_1 &&
           (u8)mip_addr == (u32)GsRegisterAddress::MIPTBP1_1 &&
           (u8)clamp_addr == (u32)GsRegisterAddress::CLAMP_1 &&
           ((u8)alpha_addr == (u32)GsRegisterAddress::ALPHA_1 ||
            (u8)alpha_addr == (u32)GsRegisterAddress::MIPTBP2_1);
  }
};

static_assert(sizeof(AdGifData) == 5 * 16);

// this represents all of the drawing state, stored as an integer.
// it can also represent "invalid".
class DrawMode {
 public:
  enum class AlphaBlend {
    DISABLED = 0,
    SRC_DST_SRC_DST = 1,
    SRC_0_SRC_DST = 2,
    SRC_0_FIX_DST = 3,    // fix = 128
    SRC_DST_FIX_DST = 4,  // fix = 64
    ZERO_SRC_SRC_DST = 5,
    SRC_SRC_SRC_SRC = 6,
    SRC_0_DST_DST = 7  // Note: requires color_mult tricks
  };

  enum class AlphaTest {
    NEVER = 0,
    ALWAYS = 1,
    GEQUAL = 2,
  };

  bool get_depth_write_enable() const { return m_val & 0b1; }
  void enable_depth_write() { m_val = m_val | 0b1; }
  void disable_depth_write() { m_val = m_val & ~(0b1); }
  void set_depth_write_enable(bool x) {
    if (x) {
      enable_depth_write();
    } else {
      disable_depth_write();
    }
  }

  GsTest::ZTest get_depth_test() const { return (GsTest::ZTest)((m_val >> 1) & 0b11); }
  void set_depth_test(GsTest::ZTest dt) { m_val = (m_val & ~(0b110)) | ((u32)(dt) << 1); }

  AlphaBlend get_alpha_blend() const { return (AlphaBlend)((m_val >> 24) & 0b111); }
  void set_alpha_blend(AlphaBlend ab) { m_val = (m_val & ~(0b111 << 24)) | ((u32)(ab) << 24); }

  u8 get_aref() const { return m_val >> 8; }
  void set_aref(u8 val) { m_val = (m_val & ~(0xff00)) | (val << 8); }

  AlphaTest get_alpha_test() const { return (AlphaTest)((m_val >> 16) & 0b11); }
  void set_alpha_test(AlphaTest ab) { m_val = (m_val & ~(0b11 << 16)) | ((u32)(ab) << 16); }

  GsTest::AlphaFail get_alpha_fail() const { return (GsTest::AlphaFail)((m_val >> 21) & 0b11); }
  void set_alpha_fail(GsTest::AlphaFail ab) { m_val = (m_val & ~(0b11 << 21)) | ((u32)(ab) << 21); }

  bool is_invalid() const { return m_val == UINT32_MAX; }
  bool is_valid() const { return !is_invalid(); }
  void set_invalid() { m_val = UINT32_MAX; }

  bool get_clamp_s_enable() const { return m_val & (1 << 5); }
  void set_clamp_s_enable(bool en) {
    if (en) {
      enable_s_clamp();
    } else {
      disable_s_clamp();
    }
  }
  void enable_s_clamp() { m_val = m_val | (1 << 5); }
  void disable_s_clamp() { m_val = m_val & (~(1 << 5)); }

  bool get_filt_enable() const { return m_val & (1 << 6); }
  void enable_filt() { m_val = m_val | (1 << 6); }
  void disable_filt() { m_val = m_val & (~(1 << 6)); }
  void set_filt_enable(bool en) {
    if (en) {
      enable_filt();
    } else {
      disable_filt();
    }
  }

  bool get_tcc_enable() const { return m_val & (1 << 6); }
  void enable_tcc() { m_val = m_val | (1 << 7); }
  void disable_tcc() { m_val = m_val & (~(1 << 7)); }
  void set_tcc(bool en) {
    if (en) {
      enable_tcc();
    } else {
      disable_tcc();
    }
  }

  bool get_at_enable() const { return m_val & (1 << 18); }
  void enable_at() { m_val = m_val | (1 << 18); }
  void disable_at() { m_val = m_val & (~(1 << 18)); }
  void set_at(bool en) {
    if (en) {
      enable_at();
    } else {
      disable_at();
    }
  }

  bool get_zt_enable() const { return m_val & (1 << 19); }
  void enable_zt() { m_val = m_val | (1 << 19); }
  void disable_zt() { m_val = m_val & (~(1 << 19)); }
  void set_zt(bool en) {
    if (en) {
      enable_zt();
    } else {
      disable_zt();
    }
  }

  bool get_ab_enable() const { return m_val & (1 << 20); }
  void enable_ab() { m_val = m_val | (1 << 20); }
  void disable_ab() { m_val = m_val & (~(1 << 20)); }
  void set_ab(bool en) {
    if (en) {
      enable_ab();
    } else {
      disable_ab();
    }
  }

  bool get_clamp_t_enable() const { return m_val & (1 << 23); }
  void set_clamp_t_enable(bool en) {
    if (en) {
      enable_t_clamp();
    } else {
      disable_t_clamp();
    }
  }
  void enable_t_clamp() { m_val = m_val | (1 << 23); }
  void disable_t_clamp() { m_val = m_val & (~(1 << 23)); }

  bool get_decal() const { return !(m_val & (1 << 28)); }
  void enable_decal() { m_val = m_val & (~(1 << 28)); }
  void disable_decal() { m_val = m_val | (1 << 28); }
  void set_decal(bool en) {
    if (en) {
      enable_decal();
    } else {
      disable_decal();
    }
  }

  bool get_fog_enable() const { return m_val & (1 << 29); }
  void enable_fog() { m_val = m_val | (1 << 29); }
  void disable_fog() { m_val = m_val & (~(1 << 29)); }
  void set_fog(bool en) {
    if (en) {
      enable_fog();
    } else {
      disable_fog();
    }
  }

  u32& as_int() { return m_val; }
  const u32& as_int() const { return m_val; }

  bool operator==(const DrawMode& other) const { return m_val == other.m_val; }
  bool operator!=(const DrawMode& other) const { return m_val != other.m_val; }

  std::string to_string() const;

 private:
  // 0 - depth write enable
  // 1, 2 - test: never, always, gequal, greater
  // 3, 4 - free

  // 5 - clamp enable
  // 6 - filt enable
  // 7 - tcc enable
  // 8,9,10,11,12,14,14,15 - aref
  // 16, 17 - atest
  // 18 - ate
  // 19 - zte
  // 20 - abe
  // 21, 22 - afail
  // 23 t clamp
  // 24 - 27 alpha blend
  // 28 !decal
  // 29 fge
  u32 m_val = UINT32_MAX;
};
