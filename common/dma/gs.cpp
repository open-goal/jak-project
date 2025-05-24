#include "gs.h"

#include "common/util/Assert.h"

#include "fmt/core.h"
#include "fmt/format.h"

std::string reg_descriptor_name(GifTag::RegisterDescriptor reg) {
  switch (reg) {
    case GifTag::RegisterDescriptor::PRIM:
      return "PRIM";
    case GifTag::RegisterDescriptor::RGBAQ:
      return "RGBAQ";
    case GifTag::RegisterDescriptor::ST:
      return "ST";
    case GifTag::RegisterDescriptor::UV:
      return "UV";
    case GifTag::RegisterDescriptor::XYZF2:
      return "XYZF2";
    case GifTag::RegisterDescriptor::XYZ2:
      return "XYZ2";
    case GifTag::RegisterDescriptor::TEX0_1:
      return "TEX0_1";
    case GifTag::RegisterDescriptor::TEX0_2:
      return "TEX0_2";
    case GifTag::RegisterDescriptor::CLAMP_1:
      return "CLAMP_1";
    case GifTag::RegisterDescriptor::CLAMP_2:
      return "CLAMP_2";
    case GifTag::RegisterDescriptor::FOG:
      return "FOG";
    case GifTag::RegisterDescriptor::XYZF3:
      return "XYZF3";
    case GifTag::RegisterDescriptor::XYZ3:
      return "XYZ3";
    case GifTag::RegisterDescriptor::AD:
      return "A+D";
    case GifTag::RegisterDescriptor::NOP:
      return "NOP";

    default:
      ASSERT(false);
  }
}

std::string GifTag::print() const {
  std::string result;
  switch (flg()) {
    case Format::PACKED:
      result += "packed ";
      break;
    case Format::REGLIST:
      result += "reglist ";
      break;
    case Format::IMAGE:
      result += "image ";
      return result;
    case Format::DISABLE:
      result += "disable ";
      return result;
    default:
      ASSERT(false);
  }

  result += fmt::format("nloop: {} ", nloop());

  if (pre()) {
    result += fmt::format("prim: 0x{:x} ", prim());
  }

  if (eop()) {
    result += "eop ";
  }

  result += '\n';
  result += ' ';

  for (u32 i = 0; i < nreg(); i++) {
    result += reg_descriptor_name(reg(i));
    result += ' ';
  }
  result += "\n";
  return result;
}

std::string register_address_name(u32 reg) {
  return register_address_name(GsRegisterAddress(reg));
}

std::string register_address_name(GsRegisterAddress reg) {
  switch (reg) {
    case GsRegisterAddress::PRIM:
      return "PRIM";
    case GsRegisterAddress::RGBAQ:
      return "RGBAQ";
    case GsRegisterAddress::ST:
      return "ST";
    case GsRegisterAddress::UV:
      return "UV";
    case GsRegisterAddress::XYZF2:
      return "XYZF2";
    case GsRegisterAddress::XYZ2:
      return "XYZ2";
    case GsRegisterAddress::TEX0_1:
      return "TEX0_1";
    case GsRegisterAddress::TEX0_2:
      return "TEX0_2";
    case GsRegisterAddress::CLAMP_1:
      return "CLAMP_1";
    case GsRegisterAddress::CLAMP_2:
      return "CLAMP_2";
    case GsRegisterAddress::FOG:
      return "FOG";
    case GsRegisterAddress::XYZF3:
      return "XYZF3";
    case GsRegisterAddress::XYZ3:
      return "XYZ3";
    case GsRegisterAddress::TEX1_1:
      return "TEX1_1";
    case GsRegisterAddress::TEX1_2:
      return "TEX1_2";
    case GsRegisterAddress::TEX2_1:
      return "TEX2_1";
    case GsRegisterAddress::TEX2_2:
      return "TEX2_2";
    case GsRegisterAddress::XYOFFSET_1:
      return "XYOFFSET_1";
    case GsRegisterAddress::XYOFFSET_2:
      return "XYOFFSET_2";
    case GsRegisterAddress::PRMODECONT:
      return "PRMODECONT";
    case GsRegisterAddress::PRMODE:
      return "PRMODE";
    case GsRegisterAddress::TEXCLUT:
      return "TEXCLUT";
    case GsRegisterAddress::SCANMSK:
      return "SCANMSK";
    case GsRegisterAddress::MIPTBP1_1:
      return "MIPTBP1_1";
    case GsRegisterAddress::MIPTBP1_2:
      return "MIPTBP1_2";
    case GsRegisterAddress::MIPTBP2_1:
      return "MIPTBP2_1";
    case GsRegisterAddress::MIPTBP2_2:
      return "MIPTBP2_2";
    case GsRegisterAddress::TEXA:
      return "TEXA";
    case GsRegisterAddress::FOGCOL:
      return "FOGCOL";
    case GsRegisterAddress::TEXFLUSH:
      return "TEXFLUSH";
    case GsRegisterAddress::SCISSOR_1:
      return "SCISSOR_1";
    case GsRegisterAddress::SCISSOR_2:
      return "SCISSOR_2";
    case GsRegisterAddress::ALPHA_1:
      return "ALPHA_1";
    case GsRegisterAddress::ALPHA_2:
      return "ALPHA_2";
    case GsRegisterAddress::DIMX:
      return "DIMX";
    case GsRegisterAddress::DTHE:
      return "DTHE";
    case GsRegisterAddress::COLCLAMP:
      return "COLCLAMP";
    case GsRegisterAddress::TEST_1:
      return "TEST_1";
    case GsRegisterAddress::TEST_2:
      return "TEST_2";
    case GsRegisterAddress::PABE:
      return "PABE";
    case GsRegisterAddress::FBA_1:
      return "FBA_1";
    case GsRegisterAddress::FBA_2:
      return "FBA_2";
    case GsRegisterAddress::FRAME_1:
      return "FRAME_1";
    case GsRegisterAddress::FRAME_2:
      return "FRAME_2";
    case GsRegisterAddress::ZBUF_1:
      return "ZBUF_1";
    case GsRegisterAddress::ZBUF_2:
      return "ZBUF_2";
    case GsRegisterAddress::BITBLTBUF:
      return "BITBLTBUF";
    case GsRegisterAddress::TRXPOS:
      return "TRXPOS";
    case GsRegisterAddress::TRXREG:
      return "TRXREG";
    case GsRegisterAddress::TRXDIR:
      return "TRXDIR";
    case GsRegisterAddress::HWREG:
      return "HWREG";
    case GsRegisterAddress::SIGNAL:
      return "SIGNAL";
    case GsRegisterAddress::FINISH:
      return "FINISH";
    case GsRegisterAddress::LABEL:
      return "LABEL";
    default:
      ASSERT(false);
  }
}

std::string GsTest::print() const {
  std::string result;
  if (alpha_test_enable()) {
    result += "alpha-test: ";
    switch (alpha_test()) {
      case AlphaTest::NEVER:
        result += "NEVER ";
        break;
      case AlphaTest::ALWAYS:
        result += "ALWAYS ";
        break;
      case AlphaTest::LESS:
        result += "LESS ";
        break;
      case AlphaTest::LEQUAL:
        result += "LEQUAL ";
        break;
      case AlphaTest::EQUAL:
        result += "EQUAL ";
        break;
      case AlphaTest::GEQUAL:
        result += "GEQUAL ";
        break;
      case AlphaTest::GREATER:
        result += "GREATER ";
        break;
      case AlphaTest::NOTEQUAL:
        result += "NOTEQUAL ";
        break;
      default:
        ASSERT(false);
    }
    result += fmt::format("ref: 0x{:x} alpha-fail: ", aref());
    switch (afail()) {
      case AlphaFail::KEEP:
        result += "KEEP ";
        break;
      case AlphaFail::FB_ONLY:
        result += "FB_ONLY ";
        break;
      case AlphaFail::ZB_ONLY:
        result += "ZB_ONLY ";
        break;
      case AlphaFail::RGB_ONLY:
        result += "RGB_ONLY ";
        break;
      default:
        ASSERT(false);
    }
  }

  if (date()) {
    result += fmt::format("dest-alpha-mode {} ", (int)datm());
  }

  if (zte()) {
    result += fmt::format("ztest: ");
    switch (ztest()) {
      case ZTest::NEVER:
        result += "NEVER";
        break;
      case ZTest::ALWAYS:
        result += "ALWAYS";
        break;
      case ZTest::GEQUAL:
        result += "GEQUAL";
        break;
      case ZTest::GREATER:
        result += "GREATER";
        break;
      default:
        ASSERT(false);
    }
  }
  result += '\n';
  return result;
}

std::string GsAlpha::print() const {
  std::string result = "(";
  switch (a_mode()) {
    case BlendMode::SOURCE:
      result += "Cs ";
      break;
    case BlendMode::DEST:
      result += "Cd ";
      break;
    case BlendMode::ZERO_OR_FIXED:
      result += "0 ";
      break;
    default:
      ASSERT(false);
  }

  switch (b_mode()) {
    case BlendMode::SOURCE:
      result += "- Cs) * ";
      break;
    case BlendMode::DEST:
      result += "- Cd) * ";
      break;
    case BlendMode::ZERO_OR_FIXED:
      result += "- 0) * ";
      break;
    default:
      ASSERT(false);
  }

  switch (c_mode()) {
    case BlendMode::SOURCE:
      result += "As / 128.0";
      break;
    case BlendMode::DEST:
      result += "Ad / 128.0";
      break;
    case BlendMode::ZERO_OR_FIXED: {
      float div = fix();
      div /= 128.0;
      result += fmt::format("{:.4f}", div);
    } break;
    default:
      ASSERT(false);
  }

  switch (d_mode()) {
    case BlendMode::SOURCE:
      result += " + Cs";
      break;
    case BlendMode::DEST:
      result += " + Cd";
      break;
    case BlendMode::ZERO_OR_FIXED:
      break;
    default:
      ASSERT(false);
  }

  return result;
}

std::string GsTex1::print() const {
  return fmt::format("lcm: {} mxl: {} mmag: {} mmin: {} mtba: {} l: {} k: {}\n", lcm(), mxl(),
                     mmag(), mmin(), mtba(), l(), k());
}

std::string GsTexa::print() const {
  return fmt::format("ta0: {} aem: {} ta1: {}\n", ta0(), aem(), ta1());
}

std::string GsTex0::print() const {
  return fmt::format(
      "tbp0: {} tbw: {} psm: {} tw: {} th: {} tcc: {} tfx: {} cbp: {} cpsm: {} csm: {}\n", tbp0(),
      tbw(), fmt::underlying(psm()), tw(), th(), tcc(), fmt::underlying(tfx()), cbp(), cpsm(),
      csm());
}

std::string GsPrim::print() const {
  return fmt::format("0x{:x}, kind {}\n", data, fmt::underlying(kind()));
}

std::string GsFrame::print() const {
  return fmt::format("fbp: {} fbw: {} psm: {} fbmsk: {:x}\n", fbp(), fbw(), fmt::underlying(psm()),
                     fbmsk());
}

std::string GsXYOffset::print() const {
  return fmt::format("ofx: {} ofy: {}\n", ofx(), ofy());
}

std::string DrawMode::to_string() const {
  std::string result;
  result += fmt::format(" depth-write: {}\n", get_depth_write_enable());
  result += fmt::format(" depth-test: ");
  switch (get_depth_test()) {
    case GsTest::ZTest::NEVER:
      result += "never\n";
      break;
    case GsTest::ZTest::GEQUAL:
      result += "gequal\n";
      break;
    case GsTest::ZTest::ALWAYS:
      result += "always\n";
      break;
    case GsTest::ZTest::GREATER:
      result += "greater\n";
      break;
    default:
      ASSERT(false);
  }
  result += fmt::format(" alpha: ");
  switch (get_alpha_blend()) {
    case AlphaBlend::SRC_0_SRC_DST:
      result += "src, 0, src, dst\n";
      break;
    case AlphaBlend::SRC_DST_SRC_DST:
      result += "src, dst, src, dst\n";
      break;
    case AlphaBlend::DISABLED:
      result += "disabled\n";
      break;
    case AlphaBlend::SRC_DST_FIX_DST:
      result += "src, dst, fix, dst\n";
      break;
    case AlphaBlend::SRC_0_DST_DST:
      result += "src, 0, dst, dst\n";
      break;
    case AlphaBlend::SRC_SRC_SRC_SRC:
      result += "src, src, src, src\n";
      break;
    case AlphaBlend::ZERO_SRC_SRC_DST:
      result += "0, src, src, dst\n";
      break;
    case AlphaBlend::SRC_0_FIX_DST:
      result += "src, 0, fix, dst\n";
      break;
    default:
      ASSERT(false);
  }
  result += fmt::format(" clamp: s {} t {}\n", get_clamp_s_enable(), get_clamp_t_enable());
  result += fmt::format(" filt: {}\n", get_filt_enable());
  result += fmt::format(" tcc: {}\n", get_tcc_enable());
  result += fmt::format(" aref: {}\n", get_aref());
  result += fmt::format(" ate: {}\n", get_at_enable());
  result += fmt::format(" atst: ");
  switch (get_alpha_test()) {
    case AlphaTest::ALWAYS:
      result += "always\n";
      break;
    case AlphaTest::GEQUAL:
      result += "gequal\n";
      break;
    case AlphaTest::NEVER:
      result += "never\n";
      break;
    default:
      result += "invalid!\n";
      break;
  }
  result += fmt::format(" zte: {}\n", get_zt_enable());
  result += fmt::format(" abe: {}\n", get_ab_enable());
  result += fmt::format(" afail: ");
  switch (get_alpha_fail()) {
    case GsTest::AlphaFail::KEEP:
      result += "keep\n";
      break;
    case GsTest::AlphaFail::FB_ONLY:
      result += "fb-only\n";
      break;
    case GsTest::AlphaFail::RGB_ONLY:
      result += "rgb-only\n";
      break;
    case GsTest::AlphaFail::ZB_ONLY:
      result += "zb-only\n";
      break;
    default:
      ASSERT(false);
  }
  result += fmt::format(" fog: {}\n decal: {}\n", get_fog_enable(), get_decal());
  return result;
}
