#pragma once

#include "common/dma/dma_chain_read.h"

bool verify_unpack_with_stcycl(const DmaTransfer& transfer,
                               VifCode::Kind unpack_kind,
                               u16 cl,
                               u16 wl,
                               u32 qwc,
                               u32 addr,
                               bool usn,
                               bool flg);

/*!
 * Make sure that the DMA transfer is a VIF unpack with the given setup.
 * This is for when there's just an UNPACK.
 */
bool verify_unpack_no_stcycl(const DmaTransfer& transfer,
                             VifCode::Kind unpack_kind,
                             u32 qwc,
                             u32 addr,
                             bool usn,
                             bool flg);

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
                         bool flg);

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
                      bool flg);

void verify_mscal(const DmaTransfer& transfer, int address);