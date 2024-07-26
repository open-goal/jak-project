#include "spustreams.h"

#include <cstring>

#include "common/log/log.h"
#include "common/util/Assert.h"

#include "game/overlord/jak3/basefile.h"
#include "game/overlord/jak3/dma.h"
#include "game/overlord/jak3/iso.h"
#include "game/overlord/jak3/iso_queue.h"
#include "game/overlord/jak3/ssound.h"
#include "game/overlord/jak3/streamlist.h"
#include "game/overlord/jak3/vag.h"
#include "game/sce/iop.h"
#include "game/sound/sdshim.h"

namespace jak3 {
using namespace iop;
void jak3_overlord_init_globals_spustreams() {}

u32 bswap_read32(u32 param_1) {
  return param_1 >> 0x18 | ((int)param_1 >> 8 & 0xff00U) | (param_1 & 0xff00) << 8 |
         param_1 << 0x18;
}

void UpdateIsoBuffer(ISO_VAGCommand* cmd, int length) {
  ASSERT(cmd);
  ASSERT(cmd->m_pBaseFile);

  auto* file = cmd->m_pBaseFile;
  int transfer_size = cmd->xfer_size;

  // advance progress in file
  file->m_Buffer.m_pCurrentData += length;

  // this part is weird - we've advanced the current data pointer, and now we're updating the
  // size field in the buffer. But, we're potentially adjusting the size in between these.
  // This means that our m_pCurrentData + m_DataLength may go off the end of the buffer??
  int tranferred_already = -length;
  if (transfer_size < length) {
    tranferred_already = -transfer_size;
    length = transfer_size;
    // ASSERT_NOT_REACHED();  // for now...
  }
  cmd->xfer_size = transfer_size + tranferred_already;
  file->m_Buffer.AddData(-length);
  cmd->num_isobuffered_chunks = cmd->num_isobuffered_chunks + 1;
}

/*!
 * Callback for handling VAG data. This transfers data from the Page buffers to SPU memory.
 */
EIsoStatus ProcessVAGData(ISO_Hdr* _msg) {
  int spu_addr;
  u32 uVar1;
  int iVar2;
  u32 val;
  uint8_t* iop_mem;
  int* piVar4;
  int iVar5;
  int iVar6;
  ISO_VAGCommand* sibling;
  u32 val2;
  CBuffer* buffer;
  CBaseFile* file;
  int length;
  // int iVar9;
  // undefined4 local_28[2];
  bool got_chunks;

  ASSERT(_msg);
  auto* msg = (ISO_VAGCommand*)_msg;
  ASSERT(msg->m_pBaseFile);
  file = msg->m_pBaseFile;
  ASSERT(file->m_ProcessDataSemaphore != -1);

  // lock the semaphore
  // lg::error("--------------- proces vag!!! {}\n", msg->name);
  WaitSema(file->m_ProcessDataSemaphore);
  buffer = &file->m_Buffer;

  if (msg->unk_gvsp_flag) {
    lg::error("unk gvsp flag set in process data - no more needed??");
  }

  // reject if errored/stopped - in this case no more data is needed.
  if (msg->unk_gvsp_flag || msg->flags.stop || msg->flags.file_disappeared ||
      msg->flags.stereo_secondary) {
    goto exit;
  }

  // reject if error.
  if (msg->error != 0) {
    file->m_Buffer.m_nDataLength = 0;
    goto exit;
  }

  // reject if dma in progress - wait for that to finish first
  if (msg->safe_to_modify_dma == 0) {
    goto exit;
  }

  // determine the length we need to do an upload.
  sibling = msg->stereo_sibling;
  got_chunks = msg->num_isobuffered_chunks != 0;
  length = 0x2000;
  if (sibling) {
    length = 0x4000;
  }

  // reject if not enough data.
  if (got_chunks) {
    int desired_length = length;
    if (msg->xfer_size < length) {
      desired_length = msg->xfer_size;
    }
    if ((int)file->m_Buffer.m_nDataLength < desired_length) {
      lg::warn("ProcessVAG data is starved.");
      goto exit;
    }
  }

  // reject if no buffer
  if (!buffer->m_pPageList || !buffer->m_pPageList->m_pCurrentActivePage)
    goto exit;

  // update buffer so it handles page crossing
  file->CheckPageBoundary();
  CPage* page;

  // add ref count to the page.
  if (!buffer->m_pPageList || ((page = buffer->m_pPageList->m_pCurrentActivePage,
                                !page || (spu_addr = page->AddRef(), spu_addr < 1))))
    goto exit;

  // make sure the page has the right state. I think this should never really happen, but I guess
  // they are paranoid.
  if (page->input_state != CPage::State::READ_DONE) {
    int status = page->ReleaseRef();
    ASSERT(status >= 0);
    goto exit;
  }

  if (got_chunks) {  // if we've already started streaming, all way have to do is update.
    if (msg->num_isobuffered_chunks & 1u) {
      int vag_transfer_size = msg->xfer_size;
      if ((length < vag_transfer_size) || ((u32)msg->unk_spu_mem_offset < 0x4000)) {
        VAG_MarkLoopEnd(file->m_Buffer.m_pCurrentData, 0x2000);
        VAG_MarkLoopStart(file->m_Buffer.m_pCurrentData);
        if (sibling) {
          VAG_MarkLoopEnd(file->m_Buffer.m_pCurrentData, 0x4000);
          VAG_MarkLoopStart(file->m_Buffer.m_pCurrentData + 0x2000);
        }
      } else {
        spu_addr = 0x2010;
        if ((0x1f < (int)vag_transfer_size) &&
            (spu_addr = vag_transfer_size + 0x1ff0, sibling != (ISO_VAGCommand*)0x0)) {
          spu_addr = ((int)(vag_transfer_size + (vag_transfer_size >> 0x1f)) >> 1) + 0x1ff0;
        }
        msg->unk_spu_mem_offset = spu_addr;
        if (sibling != (ISO_VAGCommand*)0x0) {
          sibling->unk_spu_mem_offset = msg->unk_spu_mem_offset;
        }
      }
      // CpuSuspendIntr(local_28);
      iop_mem = file->m_Buffer.m_pCurrentData;
      spu_addr = msg->stream_sram + 0x2000;
    } else {
      int vag_transfer_size = msg->xfer_size;
      if ((length < (int)vag_transfer_size) || ((u32)msg->unk_spu_mem_offset < 0x4000)) {
        VAG_MarkLoopEnd(file->m_Buffer.m_pCurrentData, 0x2000);
        VAG_MarkLoopStart(file->m_Buffer.m_pCurrentData);
        if (sibling) {
          VAG_MarkLoopEnd(file->m_Buffer.m_pCurrentData, 0x4000);
          VAG_MarkLoopStart(file->m_Buffer.m_pCurrentData + 0x2000);
        }
      } else {
        spu_addr = 0x10;
        if ((0x1f < (int)vag_transfer_size) &&
            (spu_addr = vag_transfer_size - 0x10, sibling != (ISO_VAGCommand*)0x0)) {
          spu_addr = ((int)(vag_transfer_size + (vag_transfer_size >> 0x1f)) >> 1) + -0x10;
        }
        msg->unk_spu_mem_offset = spu_addr;
        if (sibling != (ISO_VAGCommand*)0x0) {
          sibling->unk_spu_mem_offset = msg->unk_spu_mem_offset;
        }
      }
      // CpuSuspendIntr(local_28);
      iop_mem = file->m_Buffer.m_pCurrentData;
      spu_addr = msg->stream_sram;
    }
    spu_addr = DMA_SendToSPUAndSync(iop_mem, 0x2000, spu_addr, msg, page);
    if (spu_addr != 0) {
      set_active_b(msg, 0);
      goto LAB_000106a0;
    }
  LAB_0001067c:
    //
    {
      int status = page->ReleaseRef();
      ASSERT(status >= 0);
    }

  } else {
    // we're starting the vag file! check the header:
    piVar4 = (int*)file->m_Buffer.m_pCurrentData;
    if ((*piVar4 != 0x70474156) && (*piVar4 != 0x56414770)) {
      lg::error("Invalid VAG data is 0x{:x}", *piVar4);
      ASSERT_NOT_REACHED();  // invalid data
      msg->error = 1;
      file->m_Buffer.m_nDataLength = 0;
      int status = page->ReleaseRef();
      ASSERT(status >= 0);
      goto exit;
    }

    // read the rate/xfer size.
    val = piVar4[4];
    msg->vag_file_rate = val;
    val2 = piVar4[3];
    msg->unk_gvsp_cntr = 0;
    msg->xfer_size = val2;
    if (*piVar4 == 0x70474156) {
      uVar1 = bswap_read32(val);
      msg->vag_file_rate = uVar1;
      uVar1 = bswap_read32(val2);
      msg->xfer_size = uVar1;
    }

    // lg::die("xfer size is {}", msg->xfer_size);

    // set sibling properties
    if (sibling) {
      spu_addr = msg->xfer_size;
      sibling->vag_file_rate = msg->vag_file_rate;
      sibling->xfer_size = spu_addr;
      sibling->unk_gvsp_cntr = 0;
    }

    spu_addr = msg->xfer_size;
    if (sibling != (ISO_VAGCommand*)0x0) {
      spu_addr = spu_addr >> 1;
    }
    uVar1 = spu_addr + 0x30;
    if ((uVar1 & 0x1fff) != 0) {
      uVar1 = (uVar1 - (uVar1 & 0x1fff)) + 0x2000;
    }
    if (sibling != (ISO_VAGCommand*)0x0) {
      uVar1 = uVar1 * 2;
    }
    uVar1 = (uVar1 + 0x7fff) >> 0xf;
    if (uVar1 == 0) {
      uVar1 = 1;
    }
    file->m_LengthPages = uVar1;
    iVar6 = msg->vag_file_rate;
    iVar5 = msg->xfer_size;
    spu_addr = iVar5 + 0x30;
    msg->unk_spu_mem_offset = 0x4000;
    msg->xfer_size = spu_addr;
    uVar1 = (u32)(iVar6 << 0xc) / 48000;
    msg->pitch1_file = uVar1;
    msg->pitch1 = uVar1;
    if (spu_addr < 0x2001) {
      iVar2 = 0x10;
      if (0x1f < spu_addr) {
        iVar2 = iVar5 + 0x20;
      }
      msg->unk_spu_mem_offset = iVar2;
      if (sibling != (ISO_VAGCommand*)0x0) {
        sibling->xfer_size = msg->xfer_size;
        sibling->unk_spu_mem_offset = msg->unk_spu_mem_offset;
        sibling->vag_file_rate = iVar6;
        sibling->pitch1 = msg->pitch1;
        sibling->pitch1_file = msg->pitch1_file;
        sibling->xfer_size = msg->xfer_size;
        sibling->unk_spu_mem_offset = msg->unk_spu_mem_offset;
        sibling->unk_gvsp_cntr = 0;
      }
    }
    // CpuSuspendIntr(local_28);
    spu_addr =
        DMA_SendToSPUAndSync(file->m_Buffer.m_pCurrentData, 0x2000, msg->stream_sram, msg, page);
    if (spu_addr == 0)
      goto LAB_0001067c;
    msg->position_for_ee = 0;
    msg->unk_gvsp_len = 0;
    if (sibling) {  // added.
      sibling->position_for_ee = 0;
    }
  LAB_000106a0:
    UpdateIsoBuffer(msg, length);
  }
  // CpuResumeIntr(local_28[0]);
exit:
  SignalSema(file->m_ProcessDataSemaphore);
  return EIsoStatus::OK_2;
}

u32 GetSpuRamAddress(ISO_VAGCommand* cmd) {
  return sceSdGetAddr(SD_VA_NAX | cmd->voice);

  u32 voice = cmd->voice;
  u32 current_addr = cmd->stream_sram;
  BlockUntilVoiceSafe(voice, 0xf00);
  u32 sce_addr = sceSdGetAddr(voice | 0x2240);
  // lg::info("sce addr {}", sce_addr);
  if ((sce_addr < current_addr) || (current_addr + 0x4040 <= sce_addr)) {
    ASSERT_NOT_REACHED();
    sce_addr = current_addr + 0x4040;
  }
  return sce_addr;
}

u32 GetVAGStreamPos(ISO_VAGCommand* cmd) {
  u32 uVar1;
  int iVar2;
  u32 uVar3;
  u32 uVar4;
  ISO_VAGCommand* sibling;
  u32 uVar5;
  // undefined4 local_20[2];

  sibling = cmd->stereo_sibling;
  if (cmd->id == 0) {
    cmd->position_for_ee = 0;
    if (sibling == (ISO_VAGCommand*)0x0) {
      return 0;
    }
    sibling->position_for_ee = 0;
    return 0;
  }
  if (cmd->flags.file_disappeared != 0) {
    cmd->position_for_ee = cmd->clockd;
    if (sibling == (ISO_VAGCommand*)0x0) {
      return 0;
    }
    sibling->position_for_ee = sibling->clockd;
    return 0;
  }
  if (((cmd->flags.running == 0) || (cmd->flags.saw_chunks1 == 0)) || (cmd->flags.paused != 0)) {
    cmd->position_for_ee = cmd->clocka;
    if (sibling == (ISO_VAGCommand*)0x0) {
      return 0;
    }
    sibling->position_for_ee = sibling->clocka;
    return 0;
  }
  if (cmd->flags.stereo_secondary != 0) {
    cmd->position_for_ee = cmd->clocka;
    return 0;
  }
  // CpuSuspendIntr(local_20);
  uVar1 = GetSpuRamAddress(cmd);
  uVar5 = 0;
  // lg::info("offset is {}, {} - {}\n", uVar1 - cmd->stream_sram, uVar1, cmd->stream_sram);
  uVar1 = uVar1 - cmd->stream_sram;
  if (sibling != (ISO_VAGCommand*)0x0) {
    uVar5 = GetSpuRamAddress(sibling);
    uVar5 = uVar5 - sibling->stream_sram;
  }
  // CpuResumeIntr(local_20[0]);
  if (sibling != (ISO_VAGCommand*)0x0) {
    if (((uVar1 < 0x4000) && (uVar5 < 0x4000)) &&
        ((cmd->flags.bit20 == 0 && (sibling->flags.bit20 == 0)))) {
      iVar2 = (int)((uVar1 - uVar5) * 0x40000) >> 0x12;
      if (iVar2 < 0) {
        iVar2 = -iVar2;
      }
      if (4 < iVar2) {
        PauseVAG(cmd);
        uVar1 = cmd->current_spu_address - cmd->stream_sram;
        uVar5 = sibling->current_spu_address - sibling->stream_sram;
        UnPauseVAG(cmd);
      }
    }
    if (sibling == (ISO_VAGCommand*)0x0)
      goto LAB_00011010;
    // CpuSuspendIntr(local_20);
    // lg::info("inner values are: {} {}", uVar1, uVar5);
    if ((0x4000 < uVar1) && (cmd->flags.bit20 == 0)) {
      cmd->flags.bit20 = 1;
      cmd->flags.bit21 = 0;
      cmd->flags.bit22 = 0;
      sibling->flags.bit20 = 1;
      sibling->flags.bit21 = 0;
      sibling->flags.bit22 = 0;
    }
    if (uVar5 < 0x4001) {
      if (uVar1 < 0x2000) {
        if (cmd->flags.bit21 == 0) {
          cmd->unk_gvsp_cntr = cmd->unk_gvsp_cntr + 1;
          cmd->flags.bit21 = 1;
          cmd->flags.bit22 = 0;
        LAB_000109b8:
          cmd->flags.bit20 = 0;
        }
      } else {
        if (cmd->flags.bit22 == 0) {
          cmd->unk_gvsp_cntr = cmd->unk_gvsp_cntr + 1;
          cmd->flags.bit22 = 1;
          cmd->flags.bit21 = 0;
          goto LAB_000109b8;
        }
      }
      if (uVar5 < 0x2000) {
        if (sibling->flags.bit21 == 0) {
          sibling->unk_gvsp_cntr = sibling->unk_gvsp_cntr + 1;
          sibling->flags.bit21 = 1;
          sibling->flags.bit22 = 0;
        LAB_00010a1c:
          sibling->flags.bit20 = 0;
        }
      } else {
        if (sibling->flags.bit22 == 0) {
          sibling->unk_gvsp_cntr = sibling->unk_gvsp_cntr + 1;
          sibling->flags.bit22 = 1;
          sibling->flags.bit21 = 0;
          goto LAB_00010a1c;
        }
      }
    } else {
      if (sibling->flags.bit20 == 0) {
        cmd->flags.bit20 = 1;
        cmd->flags.bit21 = 0;
        cmd->flags.bit22 = 0;
        sibling->flags.bit20 = 1;
        sibling->flags.bit21 = 0;
        sibling->flags.bit22 = 0;
      }
    }
    // lg::info("bits are {} {} {}\n", cmd->flags.bit20, cmd->flags.bit21, cmd->flags.bit22);
    // CpuResumeIntr(local_20[0]);
    if (cmd->unk_gvsp_flag != 0)
      goto switchD_00010a60_caseD_1;

    // lg::info("switching {}", cmd->unk_gvsp_state2);
    switch (cmd->unk_gvsp_state2) {
      case 0:
        if ((((cmd->flags.dma_complete_even_chunk_count == 0) || (cmd->flags.bit21 == 0)) ||
             (sibling->flags.dma_complete_even_chunk_count == 0)) ||
            (sibling->flags.bit21 == 0))
          goto switchD_00010a60_caseD_1;
        cmd->flags.dma_complete_even_chunk_count = 0;
        cmd->unk_gvsp_state2 = 2;
        sibling->flags.dma_complete_even_chunk_count = 0;
        sibling->unk_gvsp_state2 = 2;
      case 2:
        if ((cmd->flags.dma_complete_odd_chunk_count == 0) ||
            (sibling->flags.dma_complete_odd_chunk_count == 0)) {
          if ((cmd->flags.bit20 != 0) || (sibling->flags.bit20 != 0)) {
            uVar1 = 0x2000;
            uVar5 = 0x2000;
            cmd->flags.bit17 = 1;
            cmd->flags.bit16 = 0;
            cmd->unk_gvsp_state2 = 4;
            sibling->flags.bit17 = 1;
            sibling->unk_gvsp_state2 = 4;
          LAB_00010efc:
            sibling->flags.bit16 = 0;
          }
          goto switchD_00010a60_caseD_1;
        }
        if ((cmd->flags.bit20 == 0) && (sibling->flags.bit20 == 0)) {
          BlockUntilVoiceSafe(cmd->voice, 0xf00);
          BlockUntilVoiceSafe(sibling->voice, 0xf00);
          // CpuSuspendIntr(local_20);
          // lg::info("sax case 1 (stream)");
          sceSdSetAddr(cmd->voice | 0x2140, cmd->stream_sram + 0x2000);
          sceSdSetAddr(sibling->voice | 0x2140, sibling->stream_sram + 0x2000);
          iVar2 = 3;
          cmd->flags.bit15 = 1;
          cmd->flags.bit14 = 0;
          cmd->flags.bit13 = 0;
          sibling->flags.bit15 = 1;
          sibling->flags.bit14 = 0;
        LAB_00010d78:
          sibling->flags.bit13 = 0;
        LAB_00010e78:
          cmd->unk_gvsp_state2 = iVar2;
          sibling->unk_gvsp_state2 = iVar2;
          // CpuResumeIntr(local_20[0]);
          goto switchD_00010a60_caseD_1;
        }
      LAB_00010bc4:
        RestartVag(cmd, 1);
        uVar1 = 0x2000;
        iVar2 = 9;
        uVar5 = 0x2000;
        break;
      default:
        goto switchD_00010a60_caseD_1;
      case 3:
        //        lg::info("case 3 bits: ({} {}) ({} {})\n", cmd->flags.bit20, cmd->flags.bit22,
        //                 sibling->flags.bit20, sibling->flags.bit22);
        if ((cmd->flags.bit20 != 0) || (sibling->flags.bit20 != 0))
          goto LAB_00010bc4;
        if ((cmd->flags.bit22 == 0) || (sibling->flags.bit22 == 0))
          goto switchD_00010a60_caseD_1;
        BlockUntilVoiceSafe(cmd->voice, 0xf00);
        BlockUntilVoiceSafe(sibling->voice, 0xf00);
        // CpuSuspendIntr(local_20);
        // lg::info("sax case 2 (trap)");
        sceSdSetAddr(cmd->voice | 0x2140, cmd->trap_sram);
        sceSdSetAddr(sibling->voice | 0x2140, sibling->trap_sram);
        iVar2 = 5;
        cmd->flags.bit13 = 1;
        cmd->flags.bit14 = 0;
        cmd->flags.bit15 = 0;
        sibling->flags.bit13 = 1;
        sibling->flags.bit14 = 0;
        sibling->flags.bit15 = 0;
        cmd->flags.dma_complete_odd_chunk_count = 0;
        sibling->flags.dma_complete_odd_chunk_count = 0;
        goto LAB_00010e78;
      case 4:
        uVar1 = cmd->unk_gvsp_len;
        uVar5 = sibling->unk_gvsp_len;
        if ((cmd->flags.dma_complete_odd_chunk_count == 0) ||
            (sibling->flags.dma_complete_odd_chunk_count == 0))
          goto switchD_00010a60_caseD_1;
        RestartVag(cmd, 1);
        iVar2 = 9;
        break;
      case 5:
        if ((cmd->flags.dma_complete_even_chunk_count == 0) ||
            (sibling->flags.dma_complete_even_chunk_count == 0)) {
          if (cmd->flags.bit20 == 0)
            goto switchD_00010a60_caseD_1;
          cmd->flags.bit16 = 1;
          cmd->flags.bit17 = 0;
          cmd->unk_gvsp_state2 = 7;
          uVar1 = 0x4000;
          uVar5 = 0x4000;
          sibling->flags.bit16 = 1;
          sibling->unk_gvsp_state2 = 7;
          goto LAB_00010db0;
        }
        if ((cmd->flags.bit20 != 0) || (sibling->flags.bit20 != 0))
          goto LAB_00010dd8;
        BlockUntilVoiceSafe(cmd->voice, 0xf00);
        BlockUntilVoiceSafe(sibling->voice, 0xf00);
        // CpuSuspendIntr(local_20);
        // lg::info("sax case 3 (stream)");
        sceSdSetAddr(cmd->voice | 0x2140, cmd->stream_sram);
        sceSdSetAddr(sibling->voice | 0x2140, sibling->stream_sram);
        iVar2 = 6;
        cmd->flags.bit14 = 1;
        cmd->flags.bit15 = 0;
        cmd->flags.bit13 = 0;
        sibling->flags.bit14 = 1;
        sibling->flags.bit15 = 0;
        goto LAB_00010d78;
      case 6:
        if ((cmd->flags.bit20 == 0) && (sibling->flags.bit20 == 0)) {
          if ((cmd->flags.bit21 == 0) || (sibling->flags.bit21 == 0))
            goto switchD_00010a60_caseD_1;
          BlockUntilVoiceSafe(cmd->voice, 0xf00);
          BlockUntilVoiceSafe(sibling->voice, 0xf00);
          // CpuSuspendIntr(local_20);
          // lg::info("sax case 4 (trap)");
          sceSdSetAddr(cmd->voice | 0x2140, cmd->trap_sram);
          sceSdSetAddr(sibling->voice | 0x2140, sibling->trap_sram);
          cmd->flags.bit13 = 1;
          cmd->flags.bit14 = 0;
          cmd->flags.bit15 = 0;
          iVar2 = 2;
          sibling->flags.bit13 = 1;
          sibling->flags.bit14 = 0;
          sibling->flags.bit15 = 0;
          cmd->flags.dma_complete_even_chunk_count = 0;
          sibling->flags.dma_complete_even_chunk_count = 0;
          goto LAB_00010e78;
        }
      LAB_00010dd8:
        RestartVag(cmd, 0);
        uVar1 = 0x4000;
        iVar2 = 8;
        uVar5 = 0x4000;
        break;
      case 7:
        uVar1 = cmd->unk_gvsp_len;
        uVar5 = uVar1;
        if ((cmd->flags.dma_complete_even_chunk_count == 0) ||
            (uVar5 = uVar1, sibling->flags.dma_complete_even_chunk_count == 0))
          goto switchD_00010a60_caseD_1;
        RestartVag(cmd, 0);
        iVar2 = 8;
        uVar5 = uVar1;
        break;
      case 8:
        if ((cmd->flags.bit21 == 0) || (sibling->flags.bit21 == 0)) {
          uVar1 = cmd->unk_gvsp_len;
          uVar5 = uVar1;
          goto switchD_00010a60_caseD_1;
        }
        cmd->unk_gvsp_state2 = 6;
        cmd->flags.bit16 = 0;
        sibling->unk_gvsp_state2 = 6;
        goto LAB_00010efc;
      case 9:
        if ((cmd->flags.bit22 == 0) || (sibling->flags.bit22 == 0)) {
          uVar1 = cmd->unk_gvsp_len;
          uVar5 = sibling->unk_gvsp_len;
          goto switchD_00010a60_caseD_1;
        }
        cmd->unk_gvsp_state2 = 3;
        cmd->flags.bit17 = 0;
        sibling->unk_gvsp_state2 = 3;
      LAB_00010db0:
        sibling->flags.bit17 = 0;
        goto switchD_00010a60_caseD_1;
    }
    cmd->unk_gvsp_state2 = iVar2;
    sibling->unk_gvsp_state2 = iVar2;
  switchD_00010a60_caseD_1:
    if (cmd->unk_gvsp_cntr == 0) {
      cmd->clockc = uVar1;
      sibling->clockc = uVar1;
    } else {
      iVar2 = sibling->unk_gvsp_cntr;
      cmd->clockc = uVar1 + cmd->unk_gvsp_cntr * 0x2000 + -0x2000;
      sibling->clockc = uVar5 + iVar2 * 0x2000 + -0x2000;
      if (0x2000 < uVar1) {
        cmd->clockc = cmd->clockc + -0x2000;
      }
      if (0x2000 < uVar5) {
        sibling->clockc = sibling->clockc + -0x2000;
      }
    }
    uVar4 = cmd->vag_file_rate;
    if (uVar4 == 0) {
      uVar3 = 0;
    } else {
      uVar3 = (u32)(cmd->clockc * 0x1c0) / uVar4;
      if (uVar4 == 0) {
        ASSERT_NOT_REACHED();
      }
    }
    iVar2 = sibling->clockc;
    uVar4 = sibling->vag_file_rate;
    cmd->unk_gvsp_len = uVar1;
    cmd->position_for_ee = uVar3 << 2;
    cmd->clocka = uVar3 << 2;
    if (uVar4 == 0) {
      uVar1 = 0;
    } else {
      uVar1 = (u32)(iVar2 * 0x1c0) / uVar4;
      if (uVar4 == 0) {
        ASSERT_NOT_REACHED();
      }
    }
    sibling->unk_gvsp_len = uVar5;
    sibling->position_for_ee = uVar1 << 2;
    sibling->clocka = uVar1 << 2;
    return 0;
  }
LAB_00011010:
  if (uVar1 < 0x4001) {
    if (uVar1 < 0x2000) {
      if (cmd->flags.bit21 == 0) {
        cmd->unk_gvsp_cntr = cmd->unk_gvsp_cntr + 1;
        cmd->flags.bit21 = 1;
        cmd->flags.bit22 = 0;
      LAB_00011098:
        cmd->flags.bit20 = 0;
      }
    } else {
      if (cmd->flags.bit22 == 0) {
        cmd->unk_gvsp_cntr = cmd->unk_gvsp_cntr + 1;
        cmd->flags.bit22 = 1;
        cmd->flags.bit21 = 0;
        goto LAB_00011098;
      }
    }
  } else {
    if (cmd->flags.bit20 == 0) {
      cmd->flags.bit20 = 1;
      cmd->flags.bit21 = 0;
      cmd->flags.bit22 = 0;
    }
  }
  if (cmd->unk_gvsp_flag != 0)
    goto switchD_000110d0_caseD_1;
  switch (cmd->unk_gvsp_state2) {
    case 0:
      if ((cmd->flags.dma_complete_even_chunk_count == 0) || (cmd->flags.bit21 == 0))
        goto switchD_000110d0_caseD_1;
      cmd->unk_gvsp_state2 = 2;
      cmd->flags.dma_complete_even_chunk_count = 0;
    switchD_000110d0_caseD_2:
      if (cmd->flags.dma_complete_odd_chunk_count == 0) {
        if (cmd->flags.bit20 != 0) {
          uVar1 = 0x2000;
          iVar2 = 4;
          cmd->flags.bit17 = 1;
        LAB_00011368:
          cmd->unk_gvsp_state2 = iVar2;
          cmd->flags.bit16 = 0;
        }
      } else {
        if (cmd->flags.bit20 == 0) {
          BlockUntilVoiceSafe(cmd->voice, 0xf00);
          // CpuSuspendIntr(local_20);
          sceSdSetAddr(cmd->voice | 0x2140, cmd->stream_sram + 0x2000);
          cmd->flags.bit15 = 1;
          cmd->unk_gvsp_state2 = 3;
          cmd->flags.bit14 = 0;
        LAB_00011284:
          cmd->flags.bit13 = 0;
        LAB_00011324:;
          // CpuResumeIntr(local_20[0]);
        } else {
        LAB_00011188:
          RestartVag(cmd, 1);
          uVar1 = 0x2000;
        LAB_0001120c:
          iVar2 = 9;
        LAB_00011350:
          cmd->unk_gvsp_state2 = iVar2;
        }
      }
    switchD_000110d0_caseD_1:
      if (cmd->unk_gvsp_cntr == 0) {
        cmd->clockc = uVar1;
      } else {
        iVar2 = uVar1 + cmd->unk_gvsp_cntr * 0x2000;
        cmd->clockc = iVar2 + -0x2000;
        if (0x2000 < uVar1) {
          cmd->clockc = iVar2 + -0x4000;
        }
      }
      uVar5 = cmd->vag_file_rate;
      if (uVar5 == 0) {
        uVar4 = 0;
      } else {
        uVar4 = (u32)(cmd->clockc * 0x1c0) / uVar5;
        if (uVar5 == 0) {
          // trap(0x1c00);
          ASSERT_NOT_REACHED();
        }
      }
      cmd->unk_gvsp_len = uVar1;
      cmd->position_for_ee = uVar4 << 2;
      cmd->clocka = uVar4 << 2;
      return 0;
    default:
      goto switchD_000110d0_caseD_1;
    case 2:
      goto switchD_000110d0_caseD_2;
    case 3:
      if (cmd->flags.bit20 != 0)
        goto LAB_00011188;
      if (cmd->flags.bit22 == 0)
        goto switchD_000110d0_caseD_1;
      BlockUntilVoiceSafe(cmd->voice, 0xf00);
      // CpuSuspendIntr(local_20);
      sceSdSetAddr(cmd->voice | 0x2140, cmd->trap_sram);
      cmd->flags.bit13 = 1;
      cmd->unk_gvsp_state2 = 5;
      cmd->flags.bit14 = 0;
      cmd->flags.bit15 = 0;
      cmd->flags.dma_complete_odd_chunk_count = 0;
      goto LAB_00011324;
    case 4:
      uVar1 = cmd->unk_gvsp_len;
      if (cmd->flags.dma_complete_odd_chunk_count == 0)
        goto switchD_000110d0_caseD_1;
      RestartVag(cmd, 1);
      goto LAB_0001120c;
    case 5:
      if (cmd->flags.dma_complete_even_chunk_count == 0) {
        if (cmd->flags.bit20 == 0)
          goto switchD_000110d0_caseD_1;
        uVar1 = 0x4000;
        cmd->flags.bit16 = 1;
        iVar2 = 7;
        goto LAB_000112a0;
      }
      if (cmd->flags.bit20 != 0)
        goto LAB_000112bc;
      BlockUntilVoiceSafe(cmd->voice, 0xf00);
      // CpuSuspendIntr(local_20);
      sceSdSetAddr(cmd->voice | 0x2140, cmd->stream_sram);
      cmd->flags.bit14 = 1;
      cmd->unk_gvsp_state2 = 6;
      cmd->flags.bit15 = 0;
      goto LAB_00011284;
    case 6:
      if (cmd->flags.bit20 == 0) {
        if (cmd->flags.bit21 == 0)
          goto switchD_000110d0_caseD_1;
        BlockUntilVoiceSafe(cmd->voice, 0xf00);
        // CpuSuspendIntr(local_20);
        sceSdSetAddr(cmd->voice | 0x2140, cmd->trap_sram);
        cmd->flags.bit13 = 1;
        cmd->unk_gvsp_state2 = 2;
        cmd->flags.bit14 = 0;
        cmd->flags.bit15 = 0;
        cmd->flags.dma_complete_even_chunk_count = 0;
        goto LAB_00011324;
      }
    LAB_000112bc:
      RestartVag(cmd, 0);
      uVar1 = 0x4000;
      goto LAB_0001134c;
    case 7:
      uVar1 = cmd->unk_gvsp_len;
      if (cmd->flags.dma_complete_even_chunk_count == 0)
        goto switchD_000110d0_caseD_1;
      RestartVag(cmd, 0);
    LAB_0001134c:
      iVar2 = 8;
      goto LAB_00011350;
    case 8:
      iVar2 = 6;
      if (cmd->flags.bit21 == 0) {
      LAB_00011374:
        uVar1 = cmd->unk_gvsp_len;
        goto switchD_000110d0_caseD_1;
      }
      goto LAB_00011368;
    case 9:
      iVar2 = 3;
      if (cmd->flags.bit22 == 0)
        goto LAB_00011374;
    LAB_000112a0:
      cmd->unk_gvsp_state2 = iVar2;
      cmd->flags.bit17 = 0;
      goto switchD_000110d0_caseD_1;
  }
}

u32 CheckVAGStreamProgress(ISO_VAGCommand* cmd) {
  u32 uVar1;
  u32 last_offset_in_stream_sram;
  ISO_VAGCommand* pIVar3;

  if (cmd->flags.file_disappeared == 0) {
    if (cmd->flags.stereo_secondary != 0) {
      return 1;
    }
    if (cmd->error != 0) {
      return 0;
    }
    if ((cmd->flags.bit20 != 0) && (cmd->unk_gvsp_flag != 0)) {
      return 0;
    }
    if (cmd->flags.saw_chunks1 == 0) {
      return 1;
    }
    if (cmd->flags.paused != 0) {
      return 1;
    }
    uVar1 = cmd->unk_spu_mem_offset;
    pIVar3 = cmd->stereo_sibling;
    last_offset_in_stream_sram = cmd->unk_gvsp_len;
    if ((uVar1 < 0x4000) && (((last_offset_in_stream_sram < 0x2001 && (uVar1 < 0x2001)) ||
                              ((0x1fff < last_offset_in_stream_sram && (0x1fff < uVar1)))))) {
      if (uVar1 <= (last_offset_in_stream_sram & 0xfffffff0)) {
        return 0;
      }
      // CpuSuspendIntr(local_18);
      if ((cmd->unk_gvsp_flag == 0) &&
          (last_offset_in_stream_sram < (u32)cmd->unk_spu_mem_offset)) {
        BlockUntilVoiceSafe(cmd->voice, 0xf00);
        sceSdSetAddr(cmd->voice | 0x2140, cmd->stream_sram + cmd->unk_spu_mem_offset);
        cmd->unk_gvsp_flag = 1;
        if (pIVar3 != (ISO_VAGCommand*)0x0) {
          BlockUntilVoiceSafe(pIVar3->voice, 0xf00);
          sceSdSetAddr(pIVar3->voice | 0x2140, pIVar3->stream_sram + cmd->unk_spu_mem_offset);
          pIVar3->unk_gvsp_flag = 1;
        }
      }
      set_active_a(cmd, 0);
      set_active_b(cmd, 0);
      // CpuResumeIntr(local_18[0]);
      return 1;
    }
    if (cmd->flags.saw_chunks1 == 0) {
      return 1;
    }
    if (cmd->active_b != 0) {
      return 1;
    }
    if (cmd->safe_to_modify_dma == 0) {
      return 1;
    }
    if (cmd->unk_gvsp_flag != 0) {
      return 1;
    }
    if (last_offset_in_stream_sram < 0x2000) {
      uVar1 = cmd->num_isobuffered_chunks;
    } else {
      uVar1 = cmd->num_isobuffered_chunks ^ 1;
    }
    if ((uVar1 & 1) == 0) {
      return 1;
    }

    set_active_b(cmd, 1);
  }
  return 1;
}

void StopVagStream(ISO_VAGCommand* cmd) {
  ISO_VAGCommand* sibling;
  VagStreamData vsd;
  //  undefined auStack72 [36];
  //  int local_24;
  //  int local_20;
  //  undefined4 local_18 [2];

  // CpuSuspendIntr(local_18);
  sibling = cmd->stereo_sibling;
  cmd->flags.saw_chunks1 = 0;
  if (sibling != (ISO_VAGCommand*)0x0) {
    sibling->flags.saw_chunks1 = 0;
  }
  if ((cmd->music_flag == 0) && (cmd->maybe_sound_handler != 0)) {
    PauseVAG(cmd);
    strncpy(vsd.name, cmd->name, 0x30);
    vsd.id = cmd->id;
    // RemoveVagStreamFromList(&vsd, &g_PluginStreamsList);
    RemoveVagStreamFromList(&vsd, &g_EEPlayList);
    //    local_20 = cmd->plugin_id;
    //    local_24 = cmd->id;
    //    RemoveLfoStreamFromList(auStack72,&g_LfoStreamsList);
  } else {
    PauseVAG(cmd);
    cmd->flags.stop = 1;
    if (sibling != (ISO_VAGCommand*)0x0) {
      PauseVAG(sibling);
      cmd->flags.stop = 1;
    }
  }
  // CpuResumeIntr(local_18[0]);
}

void ProcessStreamData() {
  EIsoStatus iVar2;
  CBuffer* pCVar3;
  ISO_VAGCommand* msg;
  int iVar5;
  ISO_VAGCommand** ppIVar6;

  WaitSema(g_nPriQueueSema);
  if (gPriStack[1].count < 8) {
    iVar5 = gPriStack[1].count + -1;
    if (-1 < iVar5) {
      ppIVar6 = (ISO_VAGCommand**)gPriStack[0].cmds + iVar5;
      do {
        msg = ppIVar6[9];
        if (msg != (ISO_VAGCommand*)0x0) {
          //          lg::warn("process stream data for {}, {} {} {}\n", msg->name,
          //                   (msg->status == EIsoStatus::OK_2), (msg->active_b != 0),
          //                   (msg->active_c != 0));
          if (((msg->status == EIsoStatus::OK_2) && (msg->active_b != 0)) && (msg->active_c != 0)) {
            auto* file = msg->m_pBaseFile;
            pCVar3 = &file->m_Buffer;
            if (!file) {
              pCVar3 = (CBuffer*)0x0;
            }
            if (((pCVar3 != (CBuffer*)0x0) &&
                 (pCVar3->m_eBufferType != CBuffer::BufferType::EBT_FREE)) &&
                (msg->callback == ProcessVAGData)) {
              iVar2 = ProcessVAGData(msg);
              msg->status = iVar2;
              if ((iVar2 != EIsoStatus::OK_2) && (msg->safe_to_modify_dma != 0)) {
                ReleaseMessage(msg);
              }
            }
          }
          if (msg->status != EIsoStatus::OK_2) {
            ReleaseMessage(msg);
          }
        }
        iVar5 = iVar5 + -1;
        ppIVar6 = ppIVar6 + -1;
      } while (-1 < iVar5);
    }
  }
  SignalSema(g_nPriQueueSema);
}

void CheckVagStreamsProgress() {
  int now;
  int iVar1;
  bool* flags;
  int iVar2;
  u32* times;
  ISO_VAGCommand* cmd;

  if ((g_bVagCmdsInitialized != 0) && (g_bSoundEnable != 0)) {
    now = GetSystemTimeLow();
    times = voice_key_times;
    flags = voice_key_flags;
    iVar2 = 0x2f;
    do {
      iVar2 = iVar2 + -1;
      if ((*flags != 0) && (0x17ff < (u32)(*times - now))) {
        *flags = 0;
      }
      flags = flags + 1;
      times = times + 1;
    } while (-1 < iVar2);
    if ((g_bRecentlyKeyedVoice != 0) && (0x17ff < (u32)(g_nTimeOfLastVoiceKey - now))) {
      g_bRecentlyKeyedVoice = 0;
    }
    iVar2 = 5;
    ProcessStreamData();
    // CpuSuspendIntr(local_18);
    cmd = g_aVagCmds;
    do {
      if (((cmd->flags.saw_chunks1 != 0) ||
           ((cmd->flags.running != 0 && (cmd->flags.file_disappeared != 0)))) ||
          ((cmd->active_b != 0 && (cmd->id != 0)))) {
        iVar1 = CheckVAGStreamProgress(cmd);
        if (iVar1 == 0) {
          if (cmd->flags.stereo_secondary == 0) {
            StopVagStream(cmd);
          }
        } else {
          GetVAGStreamPos(cmd);
        }
      }
      iVar2 = iVar2 + -1;
      cmd = cmd + 1;
    } while (-1 < iVar2);
    // CpuResumeIntr(local_18[0]);
  }
}

void BlockUntilAllVoicesSafe() {
  int now;
  int last_time;

  last_time = g_nTimeOfLastVoiceKey;
  if (g_bRecentlyKeyedVoice != 0) {
    do {
      now = GetSystemTimeLow();
    } while ((u32)(now - last_time) < 0x900);
  }
}

void BlockUntilVoiceSafe(int voice, u32 delay) {
  int iVar1;
  int iVar2;

  if (voice_key_flags[voice] != 0) {
    iVar2 = voice_key_times[voice];
    do {
      iVar1 = GetSystemTimeLow();
    } while ((u32)(iVar1 - iVar2) < delay);
  }
}

void MarkVoiceKeyedOnOff(int voice, u32 time) {
  g_nTimeOfLastVoiceKey = time;
  voice_key_times[voice] = time;
  voice_key_flags[voice] = 1;
  g_bRecentlyKeyedVoice = 1;
}

}  // namespace jak3