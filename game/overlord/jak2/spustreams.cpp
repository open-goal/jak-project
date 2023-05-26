#include "spustreams.h"

#include "common/common_types.h"
#include "common/util/Assert.h"

#include "game/overlord/jak2/dma.h"
#include "game/overlord/jak2/iso.h"
#include "game/overlord/jak2/iso_queue.h"
#include "game/overlord/jak2/streamlist.h"
#include "game/overlord/jak2/vag.h"
#include "game/sce/iop.h"
#include "game/sound/sdshim.h"

using namespace iop;

namespace jak2 {

void ProcessStreamData();
void StopVagStream(VagCmd* cmd, int suspend_irq);

s32 StreamsThread = 0;
void spusstreams_init_globals() {
  StreamsThread = 0;
}

int ProcessVAGData(CmdHeader* param_1_in, Buffer* param_2) {
  VagCmd* param_1 = (VagCmd*)param_1_in;
  int iVar1;
  int iVar2;
  u32 uVar3;
  int iVar4;
  u32 uVar5;
  int* piVar6;
  VagCmd* pRVar7;
  // undefined4 local_18[2];

  if (param_1->status_bytes[BYTE6] != '\0') {
    // printf("ProcessVAG didn't want the data: byte 6 is set\n");
    return -1;
  }
  if (param_1->status_bytes[BYTE11] != '\0') {
    // printf("ProcessVAG didn't want the data: byte 11 is set\n");
    return -1;
  }
  if (param_1->unk_260 != 0) {
    // printf("ProcessVAG didn't want the data: unk_260 is set, indicating INVALID data.\n");
    param_2->decompressed_size = 0;
    return -1;
  }
  if (!param_1->safe_to_change_dma_fields) {
    return -1;
  }
  // CpuSuspendIntr(local_18);
  CheckForIsoPageBoundaryCrossing(param_2);
  // added this check
  if (!param_2->page) {
    // printf("ProcessVAG didn't want the data: the buffer has no page (added check)\n");
    return -1;
  }

  iVar2 = (int)param_2->page->state;
  if ((iVar2 != 6) && (iVar2 != 4)) {
    // printf("ProcessVAG didn't want the data: the buffer isn't full.\n");
    goto LAB_0000fecc;
  }

  param_2->page->state = PageState::SIX;
  pRVar7 = param_1->stereo_sibling;
  iVar2 = 0x2000;
  if (pRVar7 != 0x0) {
    iVar2 = 0x4000;
  }
  uVar3 = param_1->num_processed_chunks;
  if (uVar3 == 0) {
    piVar6 = (int*)param_2->decomp_buffer;
    if ((*piVar6 != 0x70474156) && (*piVar6 != 0x56414770)) {
      param_1->unk_260 = 1;
      param_2->decompressed_size = 0;
      goto LAB_0000fecc;
    }
    param_1->sample_rate = piVar6[4];
    iVar2 = piVar6[3];
    param_1->unk_204 = 0;
    param_1->xfer_size = iVar2;
    if (*piVar6 == 0x70474156) {
      uVar3 = param_1->sample_rate;
      uVar5 = param_1->xfer_size;
      param_1->sample_rate =
          uVar3 >> 0x18 | ((int)uVar3 >> 8 & 0xff00U) | (uVar3 & 0xff00) << 8 | uVar3 << 0x18;
      param_1->xfer_size =
          uVar5 >> 0x18 | ((int)uVar5 >> 8 & 0xff00U) | (uVar5 & 0xff00) << 8 | uVar5 << 0x18;
    }
    if (pRVar7 != 0x0) {
      pRVar7->sample_rate = piVar6[4];
      iVar2 = piVar6[3];
      pRVar7->unk_204 = 0;
      pRVar7->xfer_size = iVar2;
    }
    iVar4 = param_1->xfer_size;
    iVar2 = iVar4 + 0x30;
    param_1->unk_264 = 0x4000;
    param_1->xfer_size = iVar2;
    param_1->pitch1 = (u32)(param_1->sample_rate << 0xc) / 48000;
    if ((iVar2 < 0x2001) && (0x3fff < (u32)param_1->unk_264)) {
      iVar1 = 0x10;
      if (0x1f < iVar2) {
        iVar1 = iVar4 + 0x20;
      }
      param_1->unk_264 = iVar1;
      if (pRVar7 != 0x0) {
        pRVar7->sample_rate = param_1->sample_rate;
        iVar2 = param_1->xfer_size;
        pRVar7->unk_204 = 0;
        pRVar7->xfer_size = iVar2;
        pRVar7->unk_264 = param_1->unk_264;
        pRVar7->pitch1 = param_1->pitch1;
        pRVar7->xfer_size = param_1->xfer_size;
        pRVar7->unk_264 = param_1->unk_264;
      }
    }
    iVar2 = DMA_SendToSPUAndSync(param_2->decomp_buffer, 0x2000, param_1->spu_stream_dma_mem_addr,
                                 param_1, 0);
    if (iVar2 == 0)
      goto LAB_0000fecc;
    param_1->unk_196 = 0;
    param_1->unk_200 = 0;
    if (pRVar7) {  // added
      pRVar7->unk_200 = 0;
    }
  LAB_0000fbdc:
    iVar2 = 0x2000;
    if (pRVar7 != 0x0) {
      iVar2 = 0x4000;
    }
    param_2->decomp_buffer = param_2->decomp_buffer + iVar2;
    iVar4 = param_1->xfer_size - iVar2;
    if (iVar2 < param_1->xfer_size)
      goto LAB_0000fe98;
    param_1->xfer_size = 0;
  LAB_0000feb4:
    param_2->decompressed_size = 0;
  } else {
    if (uVar3 == 1) {
      iVar4 = param_1->xfer_size;
      if ((iVar2 < iVar4) || ((u32)param_1->unk_264 < 0x4000)) {
        VAG_MarkLoopEnd((int8_t*)param_2->decomp_buffer, 0x2000);
        VAG_MarkLoopStart((int8_t*)param_2->decomp_buffer);
        if (pRVar7 != 0x0) {
          VAG_MarkLoopEnd((int8_t*)param_2->decomp_buffer, 0x4000);
          VAG_MarkLoopStart((int8_t*)(param_2->decomp_buffer + 0x2000));
        }
      } else {
        iVar2 = 0x2010;
        if (0x1f < iVar4) {
          if (pRVar7 == 0x0) {
            iVar2 = iVar4 + 0x1ff0;
          } else {
            iVar2 = iVar4 / 2 + 0x1ff0;
          }
        }
        param_1->unk_264 = iVar2;
        if (pRVar7 != 0x0) {
          pRVar7->unk_264 = param_1->unk_264;
        }
      }
      iVar2 = DMA_SendToSPUAndSync(param_2->decomp_buffer, 0x2000,
                                   param_1->spu_stream_dma_mem_addr + 0x2000, param_1, 0);
      if (iVar2 == 0)
        goto LAB_0000fecc;
      (param_1->header).ready_for_data = 0;
      goto LAB_0000fbdc;
    }
    if ((uVar3 & 1) != 0) {
      iVar4 = param_1->xfer_size;
      if ((iVar2 < iVar4) || ((u32)param_1->unk_264 < 0x4000)) {
        VAG_MarkLoopEnd((int8_t*)param_2->decomp_buffer, 0x2000);
        VAG_MarkLoopStart((int8_t*)param_2->decomp_buffer);
        if (pRVar7 != 0x0) {
          VAG_MarkLoopEnd((int8_t*)param_2->decomp_buffer, 0x4000);
          VAG_MarkLoopStart((int8_t*)(param_2->decomp_buffer + 0x2000));
        }
      } else {
        iVar2 = 0x2010;
        if (0x1f < iVar4) {
          if (pRVar7 == 0x0) {
            iVar2 = iVar4 + 0x1ff0;
          } else {
            iVar2 = iVar4 / 2 + 0x1ff0;
          }
        }
        param_1->unk_264 = iVar2;
        if (pRVar7 != 0x0) {
          pRVar7->unk_264 = param_1->unk_264;
        }
      }
      iVar2 = DMA_SendToSPUAndSync(param_2->decomp_buffer, 0x2000,
                                   param_1->spu_stream_dma_mem_addr + 0x2000, param_1, 0);
      if (iVar2 == 0)
        goto LAB_0000fecc;
      (param_1->header).ready_for_data = 0;
      goto LAB_0000fbdc;
    }
    iVar4 = param_1->xfer_size;
    if ((iVar2 < iVar4) || ((u32)param_1->unk_264 < 0x4000)) {
      VAG_MarkLoopEnd((int8_t*)param_2->decomp_buffer, 0x2000);
      VAG_MarkLoopStart((int8_t*)param_2->decomp_buffer);
      if (pRVar7 != 0x0) {
        VAG_MarkLoopEnd((int8_t*)param_2->decomp_buffer, 0x4000);
        VAG_MarkLoopStart((int8_t*)(param_2->decomp_buffer + 0x2000));
      }
    } else {
      iVar2 = 0x10;
      if (0x1f < iVar4) {
        if (pRVar7 == 0x0) {
          iVar2 = iVar4 + -0x10;
        } else {
          iVar2 = iVar4 / 2 + -0x10;
        }
      }
      param_1->unk_264 = iVar2;
      if (pRVar7 != 0x0) {
        pRVar7->unk_264 = param_1->unk_264;
      }
    }
    iVar2 = DMA_SendToSPUAndSync(param_2->decomp_buffer, 0x2000, param_1->spu_stream_dma_mem_addr,
                                 param_1, 0);
    if (iVar2 == 0)
      goto LAB_0000fecc;
    (param_1->header).ready_for_data = 0;
    iVar2 = 0x2000;
    if (pRVar7 != 0x0) {
      iVar2 = 0x4000;
    }
    param_2->decomp_buffer = param_2->decomp_buffer + iVar2;
    iVar4 = param_1->xfer_size - iVar2;
    if (param_1->xfer_size <= iVar2) {
      param_1->xfer_size = 0;
      goto LAB_0000feb4;
    }
  LAB_0000fe98:
    param_1->xfer_size = iVar4;
    param_2->decompressed_size = param_2->decompressed_size - iVar2;
  }

  param_1->num_processed_chunks++;

LAB_0000fecc:
  // CpuResumeIntr(local_18[0]);
  return -1;
}

int GetVAGStreamPos(VagCmd* param_1) {
  u32 primary_dma_offset;
  int iVar6;
  VagCmd* pRVar7;
  u32 secondary_dma_offset;
  u32 uVar9;
  u32 uVar10;

  pRVar7 = param_1->stereo_sibling;
  if (param_1->id == 0) {
    param_1->unk_200 = 0;
    if (pRVar7 == 0x0) {
      return 0;
    }
    pRVar7->unk_200 = 0;
    return 0;
  }
  if (param_1->byte6 != '\0') {
    param_1->unk_200 = param_1->unk_192;
    if (pRVar7 == 0x0) {
      return 0;
    }
    pRVar7->unk_200 = pRVar7->unk_192;
    return 0;
  }
  if (((param_1->byte4 == '\0') || (param_1->sb_playing == '\0')) || (param_1->sb_paused != '\0')) {
    param_1->unk_200 = param_1->unk_180;
    if (pRVar7 == 0x0) {
      return 0;
    }
    pRVar7->unk_200 = pRVar7->unk_180;
    return 0;
  }

  // this is inheriting what was calculated from the primary, if we're the "Stereo" second stream.
  if (param_1->byte11 != '\0') {
    param_1->unk_200 = param_1->unk_180;
    return 0;
  }
  // CpuSuspendIntr(local_30);
  primary_dma_offset = GetSpuRamAddress(param_1);
  primary_dma_offset = primary_dma_offset - param_1->spu_stream_dma_mem_addr;
  if (pRVar7 == 0x0) {
    secondary_dma_offset = 0;
  } else {
    secondary_dma_offset = GetSpuRamAddress(pRVar7);
    secondary_dma_offset = secondary_dma_offset - pRVar7->spu_stream_dma_mem_addr;
  }
  // CpuResumeIntr(local_30[0]);
  if (pRVar7 != 0x0) {
    if ((((primary_dma_offset < 0x4000) && (secondary_dma_offset < 0x4000)) &&
         (param_1->byte20 == '\0')) &&
        (pRVar7->byte20 == '\0')) {
      iVar6 = (int)((primary_dma_offset - secondary_dma_offset) * 0x40000) >> 0x12;
      if (iVar6 < 0) {
        iVar6 = -iVar6;
      }
      if (4 < iVar6) {
        PauseVAG(param_1, 1);
        primary_dma_offset = param_1->spu_addr_to_start_playing - param_1->spu_stream_dma_mem_addr;
        secondary_dma_offset = pRVar7->spu_addr_to_start_playing - pRVar7->spu_stream_dma_mem_addr;
        UnPauseVAG(param_1, 1);
      }
    }
    if (pRVar7 == 0x0)
      goto LAB_00010860;
    // CpuSuspendIntr(local_30);
    if ((0x4000 < primary_dma_offset) && (param_1->byte20 == '\0')) {
      param_1->byte20 = '\x01';
      param_1->byte21 = '\0';
      param_1->byte22 = '\0';
      pRVar7->byte20 = '\x01';
      pRVar7->byte21 = '\0';
      pRVar7->byte22 = '\0';
    }

    if (secondary_dma_offset < 0x4001) {
      if (primary_dma_offset < 0x2000) {
        if (param_1->byte21 == '\0') {
          iVar6 = param_1->unk_204;
          param_1->byte21 = '\x01';
          param_1->byte22 = '\0';
        LAB_00010234:
          param_1->byte20 = '\0';
          param_1->unk_204 = iVar6 + 1;
        }
      } else if (param_1->byte22 == '\0') {
        iVar6 = param_1->unk_204;
        param_1->byte22 = '\x01';
        param_1->byte21 = '\0';
        goto LAB_00010234;
      }

      if (secondary_dma_offset < 0x2000) {
        if (pRVar7->byte21 == '\0') {
          iVar6 = pRVar7->unk_204;
          pRVar7->byte21 = '\x01';
          pRVar7->byte22 = '\0';
        LAB_00010288:
          pRVar7->byte20 = '\0';
          pRVar7->unk_204 = iVar6 + 1;
        }
      } else if (pRVar7->byte22 == '\0') {
        iVar6 = pRVar7->unk_204;
        pRVar7->byte22 = '\x01';
        pRVar7->byte21 = '\0';
        goto LAB_00010288;
      } else {
      }
    } else if (pRVar7->byte20 == '\0') {
      param_1->byte20 = '\x01';
      param_1->byte21 = '\0';
      param_1->byte22 = '\0';
      pRVar7->byte20 = '\x01';
      pRVar7->byte21 = '\0';
      pRVar7->byte22 = '\0';
    }
    // CpuResumeIntr(local_30[0]);
    switch (param_1->unk_236) {
      case 0:
        if ((((param_1->sb_odd_buffer_dma_complete == '\0') || (param_1->byte21 == '\0')) ||
             (pRVar7->sb_odd_buffer_dma_complete == '\0')) ||
            (pRVar7->byte21 == '\0'))
          goto switchD_000102c4_caseD_1;
        param_1->sb_odd_buffer_dma_complete = '\0';
        pRVar7->sb_odd_buffer_dma_complete = '\0';
        param_1->unk_236 = 2;
        pRVar7->unk_236 = 2;
      case 2:
        if ((param_1->sb_even_buffer_dma_complete == '\0') ||
            (pRVar7->sb_even_buffer_dma_complete == '\0')) {
          if ((param_1->byte20 == '\0') && (pRVar7->byte20 == '\0'))
            goto switchD_000102c4_caseD_1;
          primary_dma_offset = 0x2000;
          secondary_dma_offset = 0x2000;
          param_1->byte17 = '\x01';
          param_1->byte16 = '\0';
          pRVar7->byte17 = '\x01';
          iVar6 = 4;
        LAB_00010744:
          pRVar7->byte16 = '\0';
        } else {
          if ((param_1->byte20 == '\0') && (pRVar7->byte20 == '\0')) {
            // CpuSuspendIntr(local_30);
            sceSdSetAddr(*(u16*)&param_1->voice | 0x2140,
                         param_1->spu_stream_dma_mem_addr + 0x2000);
            sceSdSetAddr(*(u16*)&pRVar7->voice | 0x2140, pRVar7->spu_stream_dma_mem_addr + 0x2000);
            param_1->byte15 = '\x01';
            param_1->byte14 = '\0';
            param_1->byte13 = '\0';
            pRVar7->byte15 = '\x01';
            pRVar7->byte14 = '\0';
            pRVar7->byte13 = '\0';
            iVar6 = 3;
          LAB_000106d4:
            param_1->unk_236 = iVar6;
            pRVar7->unk_236 = iVar6;
            // CpuResumeIntr(local_30[0]);
            goto switchD_000102c4_caseD_1;
          }
          primary_dma_offset = 0x2000;
          secondary_dma_offset = 0x2000;
          RestartVag(param_1, 1, 1);
          iVar6 = 9;
        }
        break;
      default:
        goto switchD_000102c4_caseD_1;
      case 3:
        if ((param_1->byte20 != '\0') || (pRVar7->byte20 != '\0')) {
          primary_dma_offset = 0x2000;
          secondary_dma_offset = 0x2000;
          RestartVag(param_1, 1, 1);
          iVar6 = 9;
          break;
        }
        if ((param_1->byte22 == '\0') || (pRVar7->byte22 == '\0'))
          goto switchD_000102c4_caseD_1;
        // CpuSuspendIntr(local_30);
        sceSdSetAddr(*(u16*)&param_1->voice | 0x2140, param_1->spu_trap_mem_addr);
        sceSdSetAddr(*(u16*)&pRVar7->voice | 0x2140, pRVar7->spu_trap_mem_addr);
        param_1->byte13 = '\x01';
        param_1->byte14 = '\0';
        param_1->byte15 = '\0';
        pRVar7->byte13 = '\x01';
        pRVar7->byte14 = '\0';
        pRVar7->byte15 = '\0';
        param_1->sb_even_buffer_dma_complete = '\0';
        pRVar7->sb_even_buffer_dma_complete = '\0';
        iVar6 = 5;
        goto LAB_000106d4;
      case 4:
        primary_dma_offset = param_1->unk_196;
        secondary_dma_offset = pRVar7->unk_196;
        if ((param_1->sb_even_buffer_dma_complete == '\0') ||
            (pRVar7->sb_even_buffer_dma_complete == '\0'))
          goto switchD_000102c4_caseD_1;
        RestartVag(param_1, 1, 1);
        iVar6 = 9;
        break;
      case 5:
        if ((param_1->sb_odd_buffer_dma_complete == '\0') ||
            (pRVar7->sb_odd_buffer_dma_complete == '\0')) {
          if (param_1->byte20 == '\0')
            goto switchD_000102c4_caseD_1;
          primary_dma_offset = 0x4000;
          secondary_dma_offset = 0x4000;
          param_1->byte16 = '\x01';
          param_1->byte17 = '\0';
          pRVar7->byte16 = '\x01';
          iVar6 = 7;
          pRVar7->byte17 = '\0';
        } else {
          if ((param_1->byte20 == '\0') && (pRVar7->byte20 == '\0')) {
            // CpuSuspendIntr(local_30);
            sceSdSetAddr(*(u16*)&param_1->voice | 0x2140, param_1->spu_stream_dma_mem_addr);
            sceSdSetAddr(*(u16*)&pRVar7->voice | 0x2140, pRVar7->spu_stream_dma_mem_addr);
            param_1->byte14 = '\x01';
            param_1->byte15 = '\0';
            param_1->byte13 = '\0';
            pRVar7->byte14 = '\x01';
            pRVar7->byte15 = '\0';
            pRVar7->byte13 = '\0';
            iVar6 = 6;
            goto LAB_000106d4;
          }
          primary_dma_offset = 0x4000;
          secondary_dma_offset = 0x4000;
          RestartVag(param_1, 0, 1);
          iVar6 = 8;
        }
        break;
      case 6:
        if ((param_1->byte20 == '\0') && (pRVar7->byte20 == '\0')) {
          if ((param_1->byte21 == '\0') || (pRVar7->byte21 == '\0'))
            goto switchD_000102c4_caseD_1;
          // CpuSuspendIntr(local_30);
          sceSdSetAddr(*(u16*)&param_1->voice | 0x2140, param_1->spu_trap_mem_addr);
          sceSdSetAddr(*(u16*)&pRVar7->voice | 0x2140, pRVar7->spu_trap_mem_addr);
          param_1->byte13 = '\x01';
          param_1->byte14 = '\0';
          param_1->byte15 = '\0';
          pRVar7->byte13 = '\x01';
          pRVar7->byte14 = '\0';
          pRVar7->byte15 = '\0';
          param_1->sb_odd_buffer_dma_complete = '\0';
          pRVar7->sb_odd_buffer_dma_complete = '\0';
          iVar6 = 2;
          goto LAB_000106d4;
        }
        primary_dma_offset = 0x4000;
        secondary_dma_offset = 0x4000;
        RestartVag(param_1, 0, 1);
        iVar6 = 8;
        break;
      case 7:
        secondary_dma_offset = param_1->unk_196;
        primary_dma_offset = secondary_dma_offset;
        if ((param_1->sb_odd_buffer_dma_complete == '\0') ||
            (pRVar7->sb_odd_buffer_dma_complete == '\0'))
          goto switchD_000102c4_caseD_1;
        RestartVag(param_1, 0, 1);
        iVar6 = 8;
        break;
      case 8:
        if ((param_1->byte21 == '\0') || (iVar6 = 6, pRVar7->byte21 == '\0')) {
          secondary_dma_offset = param_1->unk_196;
          primary_dma_offset = secondary_dma_offset;
          goto switchD_000102c4_caseD_1;
        }
        param_1->byte16 = '\0';
        goto LAB_00010744;
      case 9:
        if ((param_1->byte22 == '\0') || (iVar6 = 3, pRVar7->byte22 == '\0')) {
          secondary_dma_offset = pRVar7->unk_196;
          primary_dma_offset = param_1->unk_196;
          goto switchD_000102c4_caseD_1;
        }
        param_1->byte17 = '\0';
        pRVar7->byte17 = '\0';
    }
    param_1->unk_236 = iVar6;
    pRVar7->unk_236 = iVar6;
  switchD_000102c4_caseD_1:
    if (param_1->unk_204 == 0) {
      param_1->unk_188 = primary_dma_offset;
      pRVar7->unk_188 = primary_dma_offset;
    } else {
      param_1->unk_188 = primary_dma_offset + (param_1->unk_204 + -1) * 0x2000;
      pRVar7->unk_188 = secondary_dma_offset + (pRVar7->unk_204 + -1) * 0x2000;
      if (0x2000 < primary_dma_offset) {
        param_1->unk_188 = param_1->unk_188 + -0x2000;
      }
      if (0x2000 < secondary_dma_offset) {
        pRVar7->unk_188 = pRVar7->unk_188 + -0x2000;
      }
    }
    uVar9 = param_1->sample_rate;
    if (uVar9 == 0) {
      uVar10 = 0;
    } else {
      uVar10 = (u32)(param_1->unk_188 * 0x1c0) / uVar9;
      if (uVar9 == 0) {
        // trap(0x1c00);
        ASSERT_NOT_REACHED();
      }
    }
    param_1->unk_180 = uVar10 << 2;
    param_1->unk_200 = uVar10 << 2;
    param_1->unk_196 = primary_dma_offset;
    uVar9 = pRVar7->sample_rate;
    if (uVar9 == 0) {
      uVar10 = 0;
    } else {
      uVar10 = (u32)(pRVar7->unk_188 * 0x1c0) / uVar9;
      if (uVar9 == 0) {
        // trap(0x1c00);
        ASSERT_NOT_REACHED();
      }
    }
    pRVar7->unk_180 = uVar10 << 2;
    pRVar7->unk_200 = uVar10 << 2;
    pRVar7->unk_196 = secondary_dma_offset;
    return 0;
  }
LAB_00010860:
  if (primary_dma_offset < 0x4001) {
    if (primary_dma_offset < 0x2000) {
      if (param_1->byte21 == '\0') {
        iVar6 = param_1->unk_204;
        param_1->byte21 = '\x01';
        param_1->byte22 = '\0';
      LAB_000108cc:
        param_1->byte20 = '\0';
        param_1->unk_204 = iVar6 + 1;
      }
    } else if (param_1->byte22 == '\0') {
      iVar6 = param_1->unk_204;
      param_1->byte22 = '\x01';
      param_1->byte21 = '\0';
      goto LAB_000108cc;
    }
  } else if (param_1->byte20 == '\0') {
    param_1->byte20 = '\x01';
    param_1->byte21 = '\0';
    param_1->byte22 = '\0';
  }
  switch (param_1->unk_236) {
    case 0:
      if ((param_1->sb_odd_buffer_dma_complete == '\0') || (param_1->byte21 == '\0'))
        goto switchD_000108fc_caseD_1;
      param_1->sb_odd_buffer_dma_complete = '\0';
      param_1->unk_236 = 2;
    case 2:
      if (param_1->sb_even_buffer_dma_complete == '\0') {
        if (param_1->byte20 != '\0') {
          primary_dma_offset = 0x2000;
          param_1->byte17 = '\x01';
          iVar6 = 4;
        LAB_00010b7c:
          param_1->byte16 = '\0';
          param_1->unk_236 = iVar6;
        }
      } else if (param_1->byte20 == '\0') {
        // CpuSuspendIntr(local_30);
        sceSdSetAddr(*(u16*)&param_1->voice | 0x2140, param_1->spu_stream_dma_mem_addr + 0x2000);
        param_1->byte15 = '\x01';
        param_1->byte14 = '\0';
        param_1->byte13 = '\0';
        iVar6 = 3;
      LAB_00010b30:
        param_1->unk_236 = iVar6;
        // CpuResumeIntr(local_30[0]);
      } else {
        primary_dma_offset = 0x2000;
      LAB_00010a1c:
        RestartVag(param_1, 1, 1);
        param_1->unk_236 = 9;
      }
    default:
      goto switchD_000108fc_caseD_1;
    case 3:
      if (param_1->byte20 != '\0') {
        primary_dma_offset = 0x2000;
        goto LAB_00010a1c;
      }
      if (param_1->byte22 == '\0')
        goto switchD_000108fc_caseD_1;
      // CpuSuspendIntr(local_30);
      sceSdSetAddr(*(u16*)&param_1->voice | 0x2140, param_1->spu_trap_mem_addr);
      param_1->byte13 = '\x01';
      param_1->byte14 = '\0';
      param_1->byte15 = '\0';
      param_1->sb_even_buffer_dma_complete = '\0';
      iVar6 = 5;
      goto LAB_00010b30;
    case 4:
      primary_dma_offset = param_1->unk_196;
      if (param_1->sb_even_buffer_dma_complete == '\0')
        goto switchD_000108fc_caseD_1;
      goto LAB_00010a1c;
    case 5:
      if (param_1->sb_odd_buffer_dma_complete == '\0') {
        if (param_1->byte20 == '\0')
          goto switchD_000108fc_caseD_1;
        primary_dma_offset = 0x4000;
        param_1->byte16 = '\x01';
        iVar6 = 7;
        param_1->byte17 = '\0';
        goto LAB_00010acc;
      }
      if (param_1->byte20 == '\0') {
        // CpuSuspendIntr(local_30);
        sceSdSetAddr(*(u16*)&param_1->voice | 0x2140, param_1->spu_stream_dma_mem_addr);
        param_1->byte14 = '\x01';
        param_1->byte15 = '\0';
        param_1->byte13 = '\0';
        iVar6 = 6;
        goto LAB_00010b30;
      }
      primary_dma_offset = 0x4000;
      break;
    case 6:
      if (param_1->byte20 == '\0') {
        if (param_1->byte21 == '\0')
          goto switchD_000108fc_caseD_1;
        // CpuSuspendIntr(local_30);
        sceSdSetAddr(*(u16*)&param_1->voice | 0x2140, param_1->spu_trap_mem_addr);
        param_1->byte13 = '\x01';
        param_1->byte14 = '\0';
        param_1->byte15 = '\0';
        param_1->sb_odd_buffer_dma_complete = '\0';
        iVar6 = 2;
        goto LAB_00010b30;
      }
      primary_dma_offset = 0x4000;
      break;
    case 7:
      primary_dma_offset = param_1->unk_196;
      if (param_1->sb_odd_buffer_dma_complete == '\0')
        goto switchD_000108fc_caseD_1;
      break;
    case 8:
      iVar6 = 6;
      if (param_1->byte21 == '\0') {
      LAB_00010b88:
        primary_dma_offset = param_1->unk_196;
        goto switchD_000108fc_caseD_1;
      }
      goto LAB_00010b7c;
    case 9:
      iVar6 = 3;
      if (param_1->byte22 == '\0')
        goto LAB_00010b88;
      param_1->byte17 = '\0';
    LAB_00010acc:
      param_1->unk_236 = iVar6;
      goto switchD_000108fc_caseD_1;
  }
  RestartVag(param_1, 0, 1);
  param_1->unk_236 = 8;
switchD_000108fc_caseD_1:
  if (param_1->unk_204 == 0) {
    param_1->unk_188 = primary_dma_offset;
  } else {
    iVar6 = primary_dma_offset + (param_1->unk_204 + -1) * 0x2000;
    param_1->unk_188 = iVar6;
    if (0x2000 < primary_dma_offset) {
      param_1->unk_188 = iVar6 + -0x2000;
    }
  }
  secondary_dma_offset = param_1->sample_rate;
  if (secondary_dma_offset == 0) {
    uVar9 = 0;
  } else {
    uVar9 = (u32)(param_1->unk_188 * 0x1c0) / secondary_dma_offset;
    if (secondary_dma_offset == 0) {
      // trap(0x1c00);
      ASSERT_NOT_REACHED();
    }
  }
  param_1->unk_180 = uVar9 << 2;
  param_1->unk_200 = uVar9 << 2;
  param_1->unk_196 = primary_dma_offset;
  return 0;
}

int CheckVAGStreamProgress(VagCmd* param_1) {
  int uVar1;
  u32 uVar2;
  u32 uVar3;
  VagCmd* pRVar4;
  // undefined4 local_18 [2];

  if (param_1->byte11 != '\0') {
    return 1;
  }
  if (param_1->unk_260 != 0) {
    return 0;
  }
  if (param_1->sb_playing == '\0') {
    return 1;
  }
  if (param_1->sb_paused != '\0') {
    return 1;
  }
  uVar2 = param_1->unk_264;
  pRVar4 = param_1->stereo_sibling;
  uVar3 = param_1->unk_196;
  if (uVar2 < 0x4000) {
    if ((0x2000 < uVar3) || (0x2000 < uVar2)) {
      if ((uVar3 < 0x2000) || (uVar2 < 0x2000))
        goto LAB_00010d58;
      uVar2 = param_1->unk_264;
    }
    uVar1 = 0;
    if ((uVar3 & 0xfffffff0) < uVar2) {
      // CpuSuspendIntr(local_18);
      if ((param_1->unk_268 == 0) && (uVar3 < (u32)param_1->unk_264)) {
        sceSdSetAddr(*(u16*)&param_1->voice | 0x2140,
                     param_1->spu_stream_dma_mem_addr + param_1->unk_264);
        param_1->unk_268 = 1;
        if (pRVar4 != 0x0) {
          sceSdSetAddr(*(u16*)&pRVar4->voice | 0x2140,
                       pRVar4->spu_stream_dma_mem_addr + param_1->unk_264);
          pRVar4->unk_268 = 1;
        }
      }
      (param_1->header).ready_for_data = 0;
      // CpuResumeIntr(local_18[0]);
      uVar1 = 1;
    }
  } else {
  LAB_00010d58:
    uVar1 = 1;
    if ((((param_1->sb_playing != '\0') && (uVar1 = 1, (param_1->header).ready_for_data == 0)) &&
         param_1->safe_to_change_dma_fields) &&
        (uVar1 = 1, param_1->unk_268 == 0)) {
      if (uVar3 < 0x2000) {
        uVar1 = 1;
        if ((param_1->num_processed_chunks & 1U) != 0) {
          (param_1->header).ready_for_data = 1;
        }
      } else {
        uVar1 = 1;
        if ((param_1->num_processed_chunks & 1U) == 0) {
          (param_1->header).ready_for_data = 1;
        }
      }
    }
  }
  return uVar1;
}

u32 CheckVagStreamsProgress() {
  while (true) {
    ProcessStreamData();

    for (auto& cmd : VagCmds) {
      if (cmd.sb_playing || (cmd.byte4 && cmd.byte6) ||
          (cmd.header.ready_for_data == 1 && cmd.id)) {
        if (CheckVAGStreamProgress(&cmd)) {
          GetVAGStreamPos(&cmd);
        } else {
          StopVagStream(&cmd, 1);
        }
      }
    };

    if (ActiveVagStreams < 1) {
      SleepThread();
    } else {
      DelayThread(1000);
    }
  };

  return 0;
}

void StopVagStream(VagCmd* param_1, int param_2) {
  VagCmd* cmd;
  VagStrListNode VStack184;
  LfoListNode LStack80;
  // undefined4 local_20 [2];

  if (param_2 == 1) {
    // CpuSuspendIntr(local_20);
  }
  cmd = param_1->stereo_sibling;
  param_1->sb_playing = '\0';
  if (cmd != 0x0) {
    cmd->sb_playing = '\0';
  }
  if (param_1->unk_136 == 0) {
    PauseVAG(param_1, 0);
    param_1->byte9 = '\x01';
    if (cmd != 0x0) {
      PauseVAG(cmd, 0);
      param_1->byte9 = '\x01';
    }
  } else {
    PauseVAG(param_1, 0);
    strncpy(VStack184.name, param_1->name, 0x30);
    VStack184.id = param_1->id;
    RemoveVagStreamFromList(&VStack184, &PluginStreamsList);
    RemoveVagStreamFromList(&VStack184, &EEPlayList);
    LStack80.id = param_1->id;
    LStack80.plugin_id = param_1->plugin_id;
    RemoveLfoStreamFromList(&LStack80, &LfoList);
  }
  if (param_2 == 1) {
    // CpuResumeIntr(local_20[0]);
  }
}

void InitSpuStreamsThread() {
  ThreadParam local_20;

  local_20.attr = 0x2000000;
  local_20.entry = CheckVagStreamsProgress;
  local_20.initPriority = 0x32;
  local_20.stackSize = 0x800;
  local_20.option = 0;
  StreamsThread = CreateThread(&local_20);
  if (StreamsThread < 1) {
    printf("IOP: ======================================================================\n");
    printf("IOP: spustreams InitSpuStreamsThread: Cannot create streams thread\n");
    printf("IOP: ======================================================================\n");
    ASSERT_NOT_REACHED();
  }
  StartThread(StreamsThread, 0);
}

void WakeSpuStreamsUp() {
  iWakeupThread(StreamsThread);
}

u32 GetSpuRamAddress(VagCmd* param_1) {
  // this is simplified a lot from the original.
  // they seem to sanity check if the value is reasonable or not, but the sanity check can fail
  // if the overlord thread isn't keeping up.
  // as far as I can tell, it's totally fine to discard these checks because our sceSdGetAddr
  // works perfectly.
  return sceSdGetAddr((param_1->voice & 0xffffU) | 0x2240);
}

u32 bswap(u32 param_1) {
  return param_1 >> 0x18 | ((int)param_1 >> 8 & 0xff00U) | (param_1 & 0xff00) << 8 |
         param_1 << 0x18;
}

void ProcessStreamData() {
  int iVar1;
  VagCmd* pRVar2;
  Buffer* pBVar3;
  int iVar4;
  CmdHeader** ppCVar5;

  iVar4 = gPriStack[3].count + -1;
  if ((gPriStack[3].count < 8) && (-1 < iVar4)) {
    ppCVar5 = gPriStack[3].entries + gPriStack[3].count + -1;
    do {
      pRVar2 = (VagCmd*)*ppCVar5;
      if (pRVar2 != 0x0) {
        if ((pRVar2->header).status == -1) {
          if ((((pRVar2->header).ready_for_data != 0) &&
               (pBVar3 = (pRVar2->header).callback_buffer, pBVar3 != (Buffer*)0x0)) &&
              ((pRVar2->header).callback == ProcessVAGData)) {
            iVar1 = ProcessVAGData(&pRVar2->header, pBVar3);
            (pRVar2->header).status = iVar1;
            if ((pBVar3->decompressed_size == 0) && pRVar2->safe_to_change_dma_fields) {
              (pRVar2->header).callback_buffer = pBVar3->next;
              FreeBuffer(pBVar3, 1);
            }
            if ((pRVar2->header).status == -1)
              goto LAB_0001151c;
            if (pRVar2->safe_to_change_dma_fields) {
              ReleaseMessage((CmdHeader*)pRVar2, 1);
            }
          }
          if ((pRVar2->header).status == -1)
            goto LAB_0001151c;
        }
        ReleaseMessage((CmdHeader*)pRVar2, 1);
      }
    LAB_0001151c:
      iVar4 = iVar4 + -1;
      ppCVar5 = ppCVar5 + -1;
    } while (-1 < iVar4);
  }
}

}  // namespace jak2
