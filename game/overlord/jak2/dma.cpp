#include "dma.h"

#include "game/overlord/jak2/vag.h"
#include "game/sound/sdshim.h"
#include "game/sound/sndshim.h"

namespace jak2 {

s32 SpuDmaStatus = 0;
VagCmd* DmaVagCmd;
VagCmd* DmaStereoVagCmd;
int pending_dma = 0;
constexpr int kDmaDelay = 10;

void dma_init_globals() {
  SpuDmaStatus = 0;
  DmaVagCmd = 0;
  DmaStereoVagCmd = 0;
}

int SpuDmaIntr(int, void*) {
  //  int* piVar1;
  //  int* piVar2;
  //  RealVagCmd* pRVar3;
  //  RealVagCmd* pRVar4;
  //  undefined2 uVar5;
  //  u16 uVar6;
  //  uint uVar7;
  //  uint8_t* puVar8;
  //  int iVar9;
  //  short chan;

  printf("dma\n");
  if (SpuDmaStatus != 1) {
    return 0;
  }
  if (!DmaVagCmd) {
    goto cleanup;
  }

  int uVar7;
  int uVar6;

  if (!DmaStereoVagCmd) {
    if ((DmaVagCmd->num_processed_chunks & 1U) == 0) {
      DmaVagCmd->status_bytes[BYTE19] = 1;
    } else {
      DmaVagCmd->status_bytes[BYTE18] = 1;
    }
  } else {
    if (DmaStereoVagCmd->xfer_size != 0) {
      s16 chan;
      u8* iop_ptr;
      int spu_addr;
      if ((DmaStereoVagCmd->num_processed_chunks & 1U) == 0) {
        chan = DmaVagCmd->dma_chan;
        iop_ptr = DmaStereoVagCmd->dma_iop_mem_ptr;
        spu_addr = DmaStereoVagCmd->spu_stream_dma_mem_addr;
      } else {
        chan = DmaVagCmd->dma_chan;
        iop_ptr = DmaStereoVagCmd->dma_iop_mem_ptr;
        spu_addr = DmaStereoVagCmd->spu_stream_dma_mem_addr + 0x2000;
      }

      int old_xfer = DmaStereoVagCmd->xfer_size;
      DmaStereoVagCmd->xfer_size = 0;
      DmaStereoVagCmd->dma_iop_mem_ptr = nullptr;
      pending_dma = kDmaDelay;
      printf("stereo chain case: %d (side %d, size %d, dest %d)\n", DmaStereoVagCmd->xfer_size,
             DmaStereoVagCmd->num_processed_chunks & 1U, old_xfer, spu_addr);
      sceSdVoiceTrans((int)chan, 0, iop_ptr, spu_addr, old_xfer);
      return 0;
    }
    if ((DmaVagCmd->num_processed_chunks & 1U) == 0) {
      DmaVagCmd->status_bytes[BYTE19] = 1;
      DmaStereoVagCmd->status_bytes[BYTE19] = 1;
    } else {
      DmaVagCmd->status_bytes[BYTE18] = 1;
      DmaStereoVagCmd->status_bytes[BYTE18] = 1;
    }
  }

  if (DmaVagCmd->num_processed_chunks == 1) {
    printf("dma interrupt starting the stream...\n");
    int pitch = CalculateVAGPitch(DmaVagCmd->pitch1, DmaVagCmd->unk_256_pitch2);
    if (!DmaStereoVagCmd) {
      DmaVagCmd->spu_addr_to_start_playing = 0;
      sceSdSetAddr(((s16)DmaVagCmd->voice) | 0x2040, DmaVagCmd->spu_stream_dma_mem_addr + 0x30);
      printf("-------------- start adrs\n");
      sceSdSetParam(*(u16*)&DmaVagCmd->voice | 0x300, 0xf);
      sceSdSetParam(*(u16*)&DmaVagCmd->voice | 0x400, 0x1fc0);
      sceSdSetParam(*(u16*)&DmaVagCmd->voice | 0x200, pitch);
      uVar7 = 1 << (DmaVagCmd->voice >> 1 & 0x1fU);
      sceSdkey_on_jak2_voice(DmaVagCmd->voice);
    } else {
      DmaVagCmd->spu_addr_to_start_playing = 0;
      DmaStereoVagCmd->spu_addr_to_start_playing = 0;
      printf("-------------- start adrs (stereo)\n");

      sceSdSetAddr(*(u16*)&DmaVagCmd->voice | 0x2040, DmaVagCmd->spu_stream_dma_mem_addr + 0x30);
      sceSdSetAddr(*(u16*)&DmaStereoVagCmd->voice | 0x2040,
                   DmaStereoVagCmd->spu_stream_dma_mem_addr + 0x30);
      sceSdSetParam(*(u16*)&DmaVagCmd->voice | 0x300, 0xf);
      sceSdSetParam(*(u16*)&DmaStereoVagCmd->voice | 0x300, 0xf);
      sceSdSetParam(*(u16*)&DmaVagCmd->voice | 0x400, 0x1fc0);
      sceSdSetParam(*(u16*)&DmaStereoVagCmd->voice | 0x400, 0x1fc0);
      sceSdSetParam(*(u16*)&DmaVagCmd->voice | 0x200, pitch);
      sceSdSetParam(*(u16*)&DmaStereoVagCmd->voice | 0x200, pitch);
      uVar7 = 1 << (DmaVagCmd->voice >> 1 & 0x1fU) | 1 << (DmaStereoVagCmd->voice >> 1 & 0x1fU);

      sceSdkey_on_jak2_voice(DmaVagCmd->voice);
      sceSdkey_on_jak2_voice(DmaStereoVagCmd->voice);
    }
    uVar6 = *(u16*)&DmaVagCmd->voice & 1 | 0x1500;
  LAB_00003eb0:;
    // sceSdSetSwitch(uVar6, uVar7);
  } else if (DmaVagCmd->num_processed_chunks == 2) {
    DmaVagCmd->status_bytes[BYTE1] = 1;
    if (DmaStereoVagCmd) {
      DmaStereoVagCmd->status_bytes[BYTE1] = 1;
    }
    if (DmaVagCmd->status_bytes[PAUSED]) {
      if (!DmaStereoVagCmd) {
        sceSdSetParam(*(u16*)&DmaVagCmd->voice | 0x200, 0);
        uVar7 = 1 << (DmaVagCmd->voice >> 1 & 0x1fU);
      } else {
        sceSdSetParam(*(u16*)&DmaStereoVagCmd->voice | 0x200, 0);
        sceSdSetParam(*(u16*)&DmaVagCmd->voice | 0x200, 0);
        uVar7 = 1 << (DmaVagCmd->voice >> 1 & 0x1fU) | 1 << (DmaStereoVagCmd->voice >> 1 & 0x1fU);
      }
      uVar6 = *(u16*)&DmaVagCmd->voice & 1 | 0x1600;
      // goto LAB_00003eb0;
      // sceSdSetSwitch(uVar6, uVar7);
      sceSdkey_off_jak2_voice(DmaVagCmd->voice);
      goto hack;
    }
    DmaVagCmd->status_bytes[PAUSED] = 1;
    UnPauseVAG(DmaVagCmd, 0);
  hack:;
  }

  // now that we're done, mark it as not in use and remove from DmaVagCmd.
  DmaVagCmd->safe_to_change_dma_fields = 1;
  if (DmaStereoVagCmd) {
    DmaStereoVagCmd->safe_to_change_dma_fields = 1;
  }
  DmaVagCmd = nullptr;
  DmaStereoVagCmd = nullptr;

cleanup:
  //  sceSdSetTransIntrHandler(param_1, nullptr, nullptr);
  //  if (-1 < param_1) {
  //    snd_FreeSPUDMA(param_1);
  //  }
  SpuDmaStatus = 0;
  return 0;
}

void spu_dma_hack() {
  if (pending_dma) {
    pending_dma--;
    if (!pending_dma) {
      SpuDmaIntr(0, nullptr);
    }
  }
}

bool DMA_SendToSPUAndSync(uint8_t* iop_mem,
                          int size_one_side,
                          int spu_addr,
                          VagCmd* cmd,
                          int disable_intr) {
  printf("spudma\n");
  // if (disable_intr == 1) {
  // CpuSuspendIntr(local_28);
  //}
  int chan;
  int ret;
  if ((SpuDmaStatus == 0) && (chan = snd_GetFreeSPUDMA(), chan != -1)) {
    DmaVagCmd = cmd;
    if (cmd) {
      auto* sibling = cmd->stereo_sibling;
      // mark as in-use by dma.
      cmd->safe_to_change_dma_fields = 0;
      DmaStereoVagCmd = sibling;
      if (sibling) {
        sibling->dma_iop_mem_ptr = iop_mem + size_one_side;
        sibling->xfer_size = size_one_side;
        sibling->num_processed_chunks = cmd->num_processed_chunks;
        cmd->dma_chan = chan;
      }
    }
    SpuDmaStatus = 1;

    // note: I've bypassed the way the dma interrupts work for jak 2.
    // here, the interrupt handler set is removed, and we instead set a pending_dma flag.
    // the actual interrupt will be run from iso.cpp, in the isothread loop.
    // sceSdSetTransIntrHandler(chan, SpuDmaIntr, 0);
    pending_dma = kDmaDelay;  // added
    printf("DMA starting from SendToSPUAndSync to %d (sz %d)\n", spu_addr, size_one_side);
    int sz = sceSdVoiceTrans((int)(short)chan, 0, iop_mem, spu_addr, size_one_side);
    // if (disable_intr == 1) {
    // CpuResumeIntr(local_28[0]);
    //}
    ret = (size_one_side + 0x3fU & 0xffffffc0) <= sz;
  } else {
    ret = false;
    // if (disable_intr == 1) {
    // CpuResumeIntr(local_28[0]);
    ret = false;
    //}
  }
  return ret;
}

void DmaCancelThisVagCmd(VagCmd* param_1) {
  if (DmaVagCmd == param_1) {
    sceSdSetTransIntrHandler(DmaVagCmd->dma_chan, nullptr, nullptr);
    if (-1 < DmaVagCmd->dma_chan) {
      snd_FreeSPUDMA(DmaVagCmd->dma_chan);
    }
    DmaVagCmd = nullptr;
    DmaStereoVagCmd = nullptr;
    SpuDmaStatus = 0;
  }
}

}  // namespace jak2