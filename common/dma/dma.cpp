#include "dma.h"

#include "common/util/Assert.h"

#include "third-party/fmt/core.h"

std::string DmaTag::print() {
  std::string result;
  const char* mode_names[8] = {"refe", "cnt", "next", "ref", "refs", "call", "ret", "end"};
  result += fmt::format("TAG: 0x{:08x} {:4s} qwc 0x{:04x}", addr, mode_names[(int)kind], qwc);
  if (spr) {
    result += " SPR";
  }
  result += "\n";
  return result;
}

std::string VifCode::print() {
  std::string result;

  switch (kind) {
    case Kind::NOP:
      result = "NOP";
      break;
    case Kind::STCYCL: {
      VifCodeStcycl stcycl(immediate);
      result = fmt::format("STCYCL cl: {} wl: {}", stcycl.cl, stcycl.wl);
    } break;
    case Kind::OFFSET:
      result = fmt::format("OFFSET 0x{:x}", immediate);
      break;
    case Kind::BASE:
      result = fmt::format("BASE 0x{:x}", immediate);
      break;
    case Kind::ITOP:
      result = "ITOP";
      break;
    case Kind::STMOD:
      result = fmt::format("STMOD 0b{:b}", immediate);
      break;
    case Kind::MSK3PATH:
      result = "MSK3PATH";
      break;
    case Kind::MARK:
      result = "MARK";
      break;
    case Kind::FLUSHE:
      result = "FLUSHE";
      break;
    case Kind::FLUSH:
      result = "FLUSH";
      break;
    case Kind::FLUSHA:
      result = "FLUSHA";
      break;
    case Kind::MSCAL:
      result = fmt::format("MSCAL 0x{:x}", immediate);
      break;
    case Kind::MSCNT:
      result = "MSCNT";
      break;
    case Kind::MSCALF:
      result = fmt::format("MSCALF 0x{:x}", immediate);
      break;
    case Kind::STMASK:
      result = "STMASK";
      break;
    case Kind::STROW:
      result = "STROW";
      break;
    case Kind::STCOL:
      result = "STCOL";
      break;
    case Kind::MPG:
      result = "MPG";
      break;
    case Kind::DIRECT:
      result = "DIRECT";
      break;
    case Kind::DIRECTHL:
      result = "DIRECTHL";
      break;
    case Kind::PC_PORT:
      result = "PC_PORT";
      break;
    case Kind::UNPACK_V4_8: {
      VifCodeUnpack up(*this);
      result = fmt::format("UNPACK-V4-8: {} addr: {} us: {} tops: {}", num, up.addr_qw,
                           up.is_unsigned, up.use_tops_flag);
      break;
    }

    case Kind::UNPACK_V4_16: {
      VifCodeUnpack up(*this);
      result = fmt::format("UNPACK-V4-16: {} addr: {} us: {} tops: {}", num, up.addr_qw,
                           up.is_unsigned, up.use_tops_flag);
      break;
    }

    case Kind::UNPACK_V4_32: {
      VifCodeUnpack up(*this);
      result = fmt::format("UNPACK-V4-32: {} addr: {} us: {} tops: {}", num, up.addr_qw,
                           up.is_unsigned, up.use_tops_flag);
      break;
    }

    case Kind::UNPACK_V3_32: {
      VifCodeUnpack up(*this);
      result = fmt::format("UNPACK-V3-32: {} addr: {} us: {} tops: {}", num, up.addr_qw,
                           up.is_unsigned, up.use_tops_flag);
      break;
    }

    case Kind::UNPACK_V2_16: {
      VifCodeUnpack up(*this);
      result = fmt::format("UNPACK-V2-16: {} addr: {} us: {} tops: {}", num, up.addr_qw,
                           up.is_unsigned, up.use_tops_flag);
      break;
    }

    default:
      result = "???";
      ASSERT_MSG(false, fmt::format("Unhandled vif code {}", (int)kind));
      break;
  }
  // TODO: the rest of the VIF code.

  return result;
}
