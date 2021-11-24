#include "dma_helpers.h"

#include "third-party/fmt/format.h"

/*!
 * Make sure that the DMA Transfer is a VIF unpack (copy data to VIF memory) with the given
 * setup. This is for a transfer with STCYCL followed by UNPACK.
 */
bool verify_unpack_with_stcycl(const DmaTransfer& transfer,
                               VifCode::Kind unpack_kind,
                               u16 cl,
                               u16 wl,
                               u32 qwc,
                               u32 addr,
                               bool usn,
                               bool flg) {
  if (transfer.size_bytes != qwc * 16) {
    fmt::print("verify_unpack: bad size {} vs {}\n", transfer.size_bytes, qwc * 16);
    return false;
  }

  if (transfer.vifcode0().kind != VifCode::Kind::STCYCL) {
    fmt::print("verify_unpack: bad vifcode 0\n");
    return false;
  }

  if (transfer.vifcode1().kind != unpack_kind) {
    fmt::print("verify_unpack: bad vifcode 1\n");
    return false;
  }

  VifCodeStcycl stcycl(transfer.vifcode0());
  VifCodeUnpack unpack(transfer.vifcode1());

  if (stcycl.cl != cl || stcycl.wl != wl) {
    fmt::print("verify_unpack: bad cl/wl {}/{} vs {}/{}\n", stcycl.cl, stcycl.wl, cl, wl);
    return false;
  }

  if (unpack.addr_qw != addr || unpack.use_tops_flag != flg || unpack.is_unsigned != usn) {
    fmt::print("verify_unpack: bad unpack {}/{}/{} vs {}/{}/{}", unpack.addr_qw,
               unpack.use_tops_flag, unpack.is_unsigned, addr, flg, usn);
    return false;
  }

  if (transfer.vifcode1().num != qwc) {
    fmt::print("verify_unpack: bad num {} vs {}\n", transfer.vifcode1().num, qwc);
    return false;
  }

  return true;
}

/*!
 * Verify the DMA transfer is a VIF unpack (with STCYCL tag).
 * Then, unpack the data to dst.
 */
void unpack_to_stcycl(void* dst,
                      const DmaTransfer& transfer,
                      VifCode::Kind unpack_kind,
                      u16 cl,
                      u16 wl,
                      u32 size_bytes,
                      u32 addr,
                      bool usn,
                      bool flg) {
  bool ok =
      verify_unpack_with_stcycl(transfer, unpack_kind, cl, wl, size_bytes / 16, addr, usn, flg);
  assert(ok);
  assert((size_bytes & 0xf) == 0);
  memcpy(dst, transfer.data, size_bytes);
}

/*!
 * Make sure that the DMA transfer is a VIF unpack with the given setup.
 * This is for when there's just an UNPACK.
 */
bool verify_unpack_no_stcycl(const DmaTransfer& transfer,
                             VifCode::Kind unpack_kind,
                             u32 qwc,
                             u32 addr,
                             bool usn,
                             bool flg) {
  if (transfer.size_bytes != qwc * 16) {
    fmt::print("verify_unpack: bad size {} vs {}\n", transfer.size_bytes, qwc * 16);
    return false;
  }

  if (transfer.vifcode0().kind != VifCode::Kind::NOP) {
    fmt::print("verify_unpack: bad vifcode 0\n");
    return false;
  }

  if (transfer.vifcode1().kind != unpack_kind) {
    fmt::print("verify_unpack: bad vifcode 1\n");
    return false;
  }

  VifCodeUnpack unpack(transfer.vifcode1());

  if (unpack.addr_qw != addr || unpack.use_tops_flag != flg || unpack.is_unsigned != usn) {
    fmt::print("verify_unpack: bad unpack {}/{}/{} vs {}/{}/{}", unpack.addr_qw,
               unpack.use_tops_flag, unpack.is_unsigned, addr, flg, usn);
    return false;
  }

  if (transfer.vifcode1().num != qwc) {
    fmt::print("verify_unpack: bad num {} vs {}\n", transfer.vifcode1().num, qwc);
    return false;
  }

  return true;
}

/*!
 * Verify the DMA transfer is a VIF unpack (with no STCYCL tag).
 * Then, unpack the data to dst.
 */
void unpack_to_no_stcycl(void* dst,
                         const DmaTransfer& transfer,
                         VifCode::Kind unpack_kind,
                         u32 size_bytes,
                         u32 addr,
                         bool usn,
                         bool flg) {
  bool ok = verify_unpack_no_stcycl(transfer, unpack_kind, size_bytes / 16, addr, usn, flg);
  assert(ok);
  assert((size_bytes & 0xf) == 0);
  memcpy(dst, transfer.data, size_bytes);
}

void verify_mscal(const DmaTransfer& transfer, int address) {
  assert(transfer.size_bytes == 0);
  assert(transfer.vif0() == 0);
  assert(transfer.vifcode1().kind == VifCode::Kind::MSCAL);
  assert(transfer.vifcode1().immediate == address);
}