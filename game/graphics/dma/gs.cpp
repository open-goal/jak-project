#include "gs.h"

#include "third-party/fmt/core.h"
#include "common/util/assert.h"

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
      assert(false);
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
      assert(false);
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
      assert(false);
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
        assert(false);
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
        assert(false);
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
        assert(false);
    }
  }
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
      assert(false);
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
      assert(false);
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
      assert(false);
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
      assert(false);
  }

  return result;
}