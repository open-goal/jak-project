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
    param_1->unk_248 = piVar6[4];
    iVar2 = piVar6[3];
    param_1->unk_204 = 0;
    param_1->xfer_size = iVar2;
    if (*piVar6 == 0x70474156) {
      uVar3 = param_1->unk_248;
      uVar5 = param_1->xfer_size;
      param_1->unk_248 =
          uVar3 >> 0x18 | ((int)uVar3 >> 8 & 0xff00U) | (uVar3 & 0xff00) << 8 | uVar3 << 0x18;
      param_1->xfer_size =
          uVar5 >> 0x18 | ((int)uVar5 >> 8 & 0xff00U) | (uVar5 & 0xff00) << 8 | uVar5 << 0x18;
    }
    if (pRVar7 != 0x0) {
      pRVar7->unk_248 = piVar6[4];
      iVar2 = piVar6[3];
      pRVar7->unk_204 = 0;
      pRVar7->xfer_size = iVar2;
    }
    iVar4 = param_1->xfer_size;
    iVar2 = iVar4 + 0x30;
    param_1->unk_264 = 0x4000;
    param_1->xfer_size = iVar2;
    param_1->pitch1 = (u32)(param_1->unk_248 << 0xc) / 48000;
    if ((iVar2 < 0x2001) && (0x3fff < (u32)param_1->unk_264)) {
      iVar1 = 0x10;
      if (0x1f < iVar2) {
        iVar1 = iVar4 + 0x20;
      }
      param_1->unk_264 = iVar1;
      if (pRVar7 != 0x0) {
        pRVar7->unk_248 = param_1->unk_248;
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
      (param_1->header).unk_24 = 0;
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
      (param_1->header).unk_24 = 0;
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
    (param_1->header).unk_24 = 0;
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
  bool bVar1;
  u32 uVar2;
  u32 uVar3;
  u32 uVar4;
  u32 uVar5;
  int iVar6;
  VagCmd* pRVar7;
  u32 uVar8;
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
  if (param_1->byte11 != '\0') {
    param_1->unk_200 = param_1->unk_180;
    return 0;
  }
  // CpuSuspendIntr(local_30);
  uVar9 = param_1->spu_stream_dma_mem_addr;
  uVar8 = (param_1->voice & 0xffffU) | 0x2240;
  do {
    uVar10 = 0;
    do {
      uVar2 = sceSdGetAddr(uVar8);
      uVar3 = sceSdGetAddr(uVar8);
      uVar4 = sceSdGetAddr(uVar8);
      if ((uVar2 == uVar3) ||
          ((uVar3 != uVar4 && (bVar1 = uVar2 == uVar4, uVar4 = uVar10, bVar1)))) {
        uVar4 = uVar2;
      }
      uVar10 = uVar4;
    } while (uVar4 == 0);
  } while ((uVar4 < uVar9) || (uVar9 + 0x4040 <= uVar4));
  uVar4 = uVar4 - param_1->spu_stream_dma_mem_addr;
  if (pRVar7 == 0x0) {
    uVar8 = 0;
  } else {
    uVar10 = pRVar7->spu_stream_dma_mem_addr;
    uVar9 = (pRVar7->voice & 0xffffU) | 0x2240;
    do {
      uVar2 = 0;
      do {
        uVar3 = sceSdGetAddr(uVar9);
        uVar5 = sceSdGetAddr(uVar9);
        uVar8 = sceSdGetAddr(uVar9);
        if ((uVar3 == uVar5) ||
            ((uVar5 != uVar8 && (bVar1 = uVar3 == uVar8, uVar8 = uVar2, bVar1)))) {
          uVar8 = uVar3;
        }
        uVar2 = uVar8;
      } while (uVar8 == 0);
    } while ((uVar8 < uVar10) || (uVar10 + 0x4040 <= uVar8));
    uVar8 = uVar8 - pRVar7->spu_stream_dma_mem_addr;
  }
  // CpuResumeIntr(local_30[0]);
  if (pRVar7 != 0x0) {
    if ((((uVar4 < 0x4000) && (uVar8 < 0x4000)) && (param_1->byte20 == '\0')) &&
        (pRVar7->byte20 == '\0')) {
      iVar6 = (int)((uVar4 - uVar8) * 0x40000) >> 0x12;
      if (iVar6 < 0) {
        iVar6 = -iVar6;
      }
      if (4 < iVar6) {
        PauseVAG(param_1, 1);
        uVar4 = param_1->spu_addr_to_start_playing - param_1->spu_stream_dma_mem_addr;
        uVar8 = pRVar7->spu_addr_to_start_playing - pRVar7->spu_stream_dma_mem_addr;
        UnPauseVAG(param_1, 1);
      }
    }
    if (pRVar7 == 0x0)
      goto LAB_00010860;
    // CpuSuspendIntr(local_30);
    if ((0x4000 < uVar4) && (param_1->byte20 == '\0')) {
      param_1->byte20 = '\x01';
      param_1->byte21 = '\0';
      param_1->byte22 = '\0';
      pRVar7->byte20 = '\x01';
      pRVar7->byte21 = '\0';
      pRVar7->byte22 = '\0';
    }
    if (uVar8 < 0x4001) {
      if (uVar4 < 0x2000) {
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
      if (uVar8 < 0x2000) {
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
          uVar4 = 0x2000;
          uVar8 = 0x2000;
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
          uVar4 = 0x2000;
          uVar8 = 0x2000;
          RestartVag(param_1, 1, 1);
          iVar6 = 9;
        }
        break;
      default:
        goto switchD_000102c4_caseD_1;
      case 3:
        if ((param_1->byte20 != '\0') || (pRVar7->byte20 != '\0')) {
          uVar4 = 0x2000;
          uVar8 = 0x2000;
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
        uVar4 = param_1->unk_196;
        uVar8 = pRVar7->unk_196;
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
          uVar4 = 0x4000;
          uVar8 = 0x4000;
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
          uVar4 = 0x4000;
          uVar8 = 0x4000;
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
        uVar4 = 0x4000;
        uVar8 = 0x4000;
        RestartVag(param_1, 0, 1);
        iVar6 = 8;
        break;
      case 7:
        uVar8 = param_1->unk_196;
        uVar4 = uVar8;
        if ((param_1->sb_odd_buffer_dma_complete == '\0') ||
            (pRVar7->sb_odd_buffer_dma_complete == '\0'))
          goto switchD_000102c4_caseD_1;
        RestartVag(param_1, 0, 1);
        iVar6 = 8;
        break;
      case 8:
        if ((param_1->byte21 == '\0') || (iVar6 = 6, pRVar7->byte21 == '\0')) {
          uVar8 = param_1->unk_196;
          uVar4 = uVar8;
          goto switchD_000102c4_caseD_1;
        }
        param_1->byte16 = '\0';
        goto LAB_00010744;
      case 9:
        if ((param_1->byte22 == '\0') || (iVar6 = 3, pRVar7->byte22 == '\0')) {
          uVar8 = pRVar7->unk_196;
          uVar4 = param_1->unk_196;
          goto switchD_000102c4_caseD_1;
        }
        param_1->byte17 = '\0';
        pRVar7->byte17 = '\0';
    }
    param_1->unk_236 = iVar6;
    pRVar7->unk_236 = iVar6;
  switchD_000102c4_caseD_1:
    if (param_1->unk_204 == 0) {
      param_1->unk_188 = uVar4;
      pRVar7->unk_188 = uVar4;
    } else {
      param_1->unk_188 = uVar4 + (param_1->unk_204 + -1) * 0x2000;
      pRVar7->unk_188 = uVar8 + (pRVar7->unk_204 + -1) * 0x2000;
      if (0x2000 < uVar4) {
        param_1->unk_188 = param_1->unk_188 + -0x2000;
      }
      if (0x2000 < uVar8) {
        pRVar7->unk_188 = pRVar7->unk_188 + -0x2000;
      }
    }
    uVar9 = param_1->unk_248;
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
    param_1->unk_196 = uVar4;
    uVar9 = pRVar7->unk_248;
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
    pRVar7->unk_196 = uVar8;
    return 0;
  }
LAB_00010860:
  if (uVar4 < 0x4001) {
    if (uVar4 < 0x2000) {
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
          uVar4 = 0x2000;
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
        uVar4 = 0x2000;
      LAB_00010a1c:
        RestartVag(param_1, 1, 1);
        param_1->unk_236 = 9;
      }
    default:
      goto switchD_000108fc_caseD_1;
    case 3:
      if (param_1->byte20 != '\0') {
        uVar4 = 0x2000;
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
      uVar4 = param_1->unk_196;
      if (param_1->sb_even_buffer_dma_complete == '\0')
        goto switchD_000108fc_caseD_1;
      goto LAB_00010a1c;
    case 5:
      if (param_1->sb_odd_buffer_dma_complete == '\0') {
        if (param_1->byte20 == '\0')
          goto switchD_000108fc_caseD_1;
        uVar4 = 0x4000;
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
      uVar4 = 0x4000;
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
      uVar4 = 0x4000;
      break;
    case 7:
      uVar4 = param_1->unk_196;
      if (param_1->sb_odd_buffer_dma_complete == '\0')
        goto switchD_000108fc_caseD_1;
      break;
    case 8:
      iVar6 = 6;
      if (param_1->byte21 == '\0') {
      LAB_00010b88:
        uVar4 = param_1->unk_196;
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
    param_1->unk_188 = uVar4;
  } else {
    iVar6 = uVar4 + (param_1->unk_204 + -1) * 0x2000;
    param_1->unk_188 = iVar6;
    if (0x2000 < uVar4) {
      param_1->unk_188 = iVar6 + -0x2000;
    }
  }
  uVar8 = param_1->unk_248;
  if (uVar8 == 0) {
    uVar9 = 0;
  } else {
    uVar9 = (u32)(param_1->unk_188 * 0x1c0) / uVar8;
    if (uVar8 == 0) {
      // trap(0x1c00);
      ASSERT_NOT_REACHED();
    }
  }
  param_1->unk_180 = uVar9 << 2;
  param_1->unk_200 = uVar9 << 2;
  param_1->unk_196 = uVar4;
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
      (param_1->header).unk_24 = 0;
      // CpuResumeIntr(local_18[0]);
      uVar1 = 1;
    }
  } else {
  LAB_00010d58:
    uVar1 = 1;
    if ((((param_1->sb_playing != '\0') && (uVar1 = 1, (param_1->header).unk_24 == 0)) &&
         param_1->safe_to_change_dma_fields) &&
        (uVar1 = 1, param_1->unk_268 == 0)) {
      if (uVar3 < 0x2000) {
        uVar1 = 1;
        if ((param_1->num_processed_chunks & 1U) != 0) {
          (param_1->header).unk_24 = 1;
        }
      } else {
        uVar1 = 1;
        if ((param_1->num_processed_chunks & 1U) == 0) {
          (param_1->header).unk_24 = 1;
        }
      }
    }
  }
  return uVar1;
}

u32 CheckVagStreamsProgress() {
  int iVar1;
  VagCmd* pRVar2;
  // int8_t* piVar3;
  Buffer* pBVar4;
  VagCmd* cmd;
  int iVar5;
  CmdHeader** ppCVar6;
  VagStrListNode VStack200;
  LfoListNode LStack96;
  // undefined4 local_30 [2];

  do {
    if (gPriStack[3].count < 8) {
      iVar5 = gPriStack[3].count + -1;
      if (-1 < iVar5) {
        ppCVar6 = gPriStack[3].entries + gPriStack[3].count + -1;
        do {
          pRVar2 = (VagCmd*)*ppCVar6;
          if (pRVar2 != 0x0) {
            if ((pRVar2->header).status == -1) {
              if ((((pRVar2->header).unk_24 != 0) &&
                   (pBVar4 = (pRVar2->header).callback_buffer, pBVar4 != (Buffer*)0x0)) &&
                  ((pRVar2->header).callback == ProcessVAGData)) {
                iVar1 = ProcessVAGData(&pRVar2->header, pBVar4);
                (pRVar2->header).status = iVar1;
                if ((pBVar4->decompressed_size == 0) && pRVar2->safe_to_change_dma_fields == 1) {
                  (pRVar2->header).callback_buffer = pBVar4->next;
                  FreeBuffer(pBVar4, 1);
                }
                if ((pRVar2->header).status == -1)
                  goto LAB_00010f30;
                if (pRVar2->safe_to_change_dma_fields) {
                  ReleaseMessage((CmdHeader*)pRVar2, 1);
                }
              }
              if ((pRVar2->header).status == -1)
                goto LAB_00010f30;
            }
            ReleaseMessage((CmdHeader*)pRVar2, 1);
          }
        LAB_00010f30:
          iVar5 = iVar5 + -1;
          ppCVar6 = ppCVar6 + -1;
        } while (-1 < iVar5);
      }
    }
    pRVar2 = VagCmds;
    iVar5 = 0;
    // piVar3 = &VagCmds[0].byte9;
    auto* cmd_iter = VagCmds;
    do {
      if (((cmd_iter->sb_playing != '\0') ||
           ((cmd_iter->byte4 != '\0' && (cmd_iter->byte6 != '\0')))) ||
          ((cmd_iter->header.unk_24 == 1 && (cmd_iter->id != 0)))) {
        iVar1 = CheckVAGStreamProgress(pRVar2);
        if (iVar1 == 0) {
          if (cmd_iter->byte11 == '\0') {
            // CpuSuspendIntr(local_30);
            cmd = cmd_iter->stereo_sibling;
            // piVar3[-8] = '\0';
            cmd_iter->sb_playing = 0;
            if (cmd != 0x0) {
              cmd->sb_playing = '\0';
            }
            if (cmd_iter->unk_136 == 0) {
              PauseVAG(pRVar2, 0);
              // *piVar3 = '\x01';
              cmd_iter->byte9 = 1;
              if (cmd != 0x0) {
                PauseVAG(cmd, 0);
                // *piVar3 = '\x01';
                cmd_iter->byte9 = 1;
              }
            } else {
              PauseVAG(pRVar2, 0);
              strncpy(VStack200.name, pRVar2->name, 0x30);
              VStack200.id = cmd_iter->id;
              RemoveVagStreamFromList(&VStack200, &PluginStreamsList);
              RemoveVagStreamFromList(&VStack200, &EEPlayList);
              LStack96.id = cmd_iter->id;
              LStack96.plugin_id = cmd_iter->plugin_id;
              RemoveLfoStreamFromList(&LStack96, &LfoList);
            }
            // CpuResumeIntr(local_30[0]);
          }
        } else {
          GetVAGStreamPos(pRVar2);
        }
      }
      pRVar2 = pRVar2 + 1;
      // piVar3 = piVar3 + 0x144;
      cmd_iter++;
      iVar5 = iVar5 + 1;
    } while (iVar5 < 4);
    if (ActiveVagStreams < 1) {
      SleepThread();
    } else {
      DelayThread(1000);
    }
  } while (true);
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
  bool bVar1;
  u32 uVar2;
  u32 uVar3;
  u32 uVar4;
  u32 uVar5;
  u32 uVar6;
  u32 uVar7;

  uVar7 = param_1->spu_stream_dma_mem_addr;
  uVar6 = (param_1->voice & 0xffffU) | 0x2240;
  do {
    uVar5 = 0;
    do {
      uVar2 = sceSdGetAddr(uVar6);
      uVar3 = sceSdGetAddr(uVar6);
      uVar4 = sceSdGetAddr(uVar6);
      // printf("got nax: %d\n", uVar3);
      if ((uVar2 == uVar3) ||
          ((uVar3 != uVar4 && (bVar1 = uVar2 == uVar4, uVar4 = uVar5, bVar1)))) {
        uVar4 = uVar2;
      }
      uVar5 = uVar4;
    } while (uVar4 == 0);
  } while ((uVar4 < uVar7) || (uVar7 + 0x4040 <= uVar4));
  return uVar4;
}

u32 bswap(u32 param_1) {
  return param_1 >> 0x18 | ((int)param_1 >> 8 & 0xff00U) | (param_1 & 0xff00) << 8 |
         param_1 << 0x18;
}

void ProcessStreamData(void) {
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
          if ((((pRVar2->header).unk_24 != 0) &&
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
