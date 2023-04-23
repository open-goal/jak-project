#include "dma.h"

#include "game/overlord/jak2/vag.h"
#include "game/sound/sdshim.h"
#include "game/sound/sndshim.h"

namespace jak2 {

s32 SpuDmaStatus = 0;
VagCmd* DmaVagCmd;
VagCmd* DmaStereoVagCmd;
int pending_dma = 0;

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
  //  ushort uVar6;
  //  uint uVar7;
  //  uint8_t* puVar8;
  //  int iVar9;
  //  short chan;

  if (SpuDmaStatus != 1) {
    return 0;
  }
  if (!DmaVagCmd) {
    goto cleanup;
  }

  int uVar7;
  int uVar6;

  if (!DmaStereoVagCmd) {
    if ((DmaVagCmd->unk_240_flag0 & 1U) == 0) {
      DmaVagCmd->status_bytes[BYTE19] = 1;
    } else {
      DmaVagCmd->status_bytes[BYTE18] = 1;
    }
  } else {
    if (DmaStereoVagCmd->xfer_size != 0) {
      s16 chan;
      u8* iop_ptr;
      int spu_addr;
      if ((DmaStereoVagCmd->unk_240_flag0 & 1U) == 0) {
        chan = DmaVagCmd->chan;
        iop_ptr = DmaStereoVagCmd->dma_iop_mem_ptr;
        spu_addr = DmaStereoVagCmd->spu_stream_mem_addr;
      } else {
        chan = DmaVagCmd->chan;
        iop_ptr = DmaStereoVagCmd->dma_iop_mem_ptr;
        spu_addr = DmaStereoVagCmd->spu_stream_mem_addr + 0x2000;
      }

      int old_xfer = DmaStereoVagCmd->xfer_size;
      DmaStereoVagCmd->xfer_size = 0;
      DmaStereoVagCmd->dma_iop_mem_ptr = nullptr;
      pending_dma = 1;
      sceSdVoiceTrans((int)chan, 0, iop_ptr, spu_addr, old_xfer);
      return 0;
    }
    if ((DmaVagCmd->unk_240_flag0 & 1U) == 0) {
      DmaVagCmd->status_bytes[BYTE19] = 1;
      DmaStereoVagCmd->status_bytes[BYTE19] = 1;
    } else {
      DmaVagCmd->status_bytes[BYTE18] = 1;
      DmaStereoVagCmd->status_bytes[BYTE18] = 1;
    }
  }
  if (DmaVagCmd->unk_240_flag0 == 1) {
    int pitch = CalculateVAGPitch(DmaVagCmd->pitch1, DmaVagCmd->unk_256_pitch2);
    if (!DmaStereoVagCmd) {
      DmaVagCmd->unk_64 = 0;
      sceSdSetAddr(((s16)DmaVagCmd->voice) | 0x2040, DmaVagCmd->spu_stream_mem_addr + 0x30);
      printf("-------------- start adrs\n");
      sceSdSetParam(*(ushort*)&DmaVagCmd->voice | 0x300, 0xf);
      sceSdSetParam(*(ushort*)&DmaVagCmd->voice | 0x400, 0x1fc0);
      sceSdSetParam(*(ushort*)&DmaVagCmd->voice | 0x200, pitch);
      uVar7 = 1 << (DmaVagCmd->voice >> 1 & 0x1fU);
    } else {
      DmaVagCmd->unk_64 = 0;
      DmaStereoVagCmd->unk_64 = 0;
      printf("-------------- start adrs (stereo)\n");

      sceSdSetAddr(*(ushort*)&DmaVagCmd->voice | 0x2040, DmaVagCmd->spu_stream_mem_addr + 0x30);
      sceSdSetAddr(*(ushort*)&DmaStereoVagCmd->voice | 0x2040,
                   DmaStereoVagCmd->spu_stream_mem_addr + 0x30);
      sceSdSetParam(*(ushort*)&DmaVagCmd->voice | 0x300, 0xf);
      sceSdSetParam(*(ushort*)&DmaStereoVagCmd->voice | 0x300, 0xf);
      sceSdSetParam(*(ushort*)&DmaVagCmd->voice | 0x400, 0x1fc0);
      sceSdSetParam(*(ushort*)&DmaStereoVagCmd->voice | 0x400, 0x1fc0);
      sceSdSetParam(*(ushort*)&DmaVagCmd->voice | 0x200, pitch);
      sceSdSetParam(*(ushort*)&DmaStereoVagCmd->voice | 0x200, pitch);
      uVar7 = 1 << (DmaVagCmd->voice >> 1 & 0x1fU) | 1 << (DmaStereoVagCmd->voice >> 1 & 0x1fU);
    }
    uVar6 = *(ushort*)&DmaVagCmd->voice & 1 | 0x1500;
  LAB_00003eb0:
    sceSdSetSwitch(uVar6, uVar7);
  } else if (DmaVagCmd->unk_240_flag0 == 2) {
    DmaVagCmd->status_bytes[BYTE1] = 1;
    if (DmaStereoVagCmd) {
      DmaStereoVagCmd->status_bytes[BYTE1] = 1;
    }
    if (DmaVagCmd->status_bytes[PAUSED]) {
      if (!DmaStereoVagCmd) {
        sceSdSetParam(*(ushort*)&DmaVagCmd->voice | 0x200, 0);
        uVar7 = 1 << (DmaVagCmd->voice >> 1 & 0x1fU);
      } else {
        sceSdSetParam(*(ushort*)&DmaStereoVagCmd->voice | 0x200, 0);
        sceSdSetParam(*(ushort*)&DmaVagCmd->voice | 0x200, 0);
        uVar7 = 1 << (DmaVagCmd->voice >> 1 & 0x1fU) | 1 << (DmaStereoVagCmd->voice >> 1 & 0x1fU);
      }
      uVar6 = *(ushort*)&DmaVagCmd->voice & 1 | 0x1600;
      // goto LAB_00003eb0;
      sceSdSetSwitch(uVar6, uVar7);
      goto hack;
    }
    DmaVagCmd->status_bytes[PAUSED] = 1;
    UnPauseVAG(DmaVagCmd, 0);
  hack:;
  }

  printf("restoring unk60\n");
  DmaVagCmd->unk_60 = 1;
  if (DmaStereoVagCmd) {
    DmaStereoVagCmd->unk_60 = 1;
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
    SpuDmaIntr(0, nullptr);
    pending_dma = 0;
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
      printf("setting unk60 to 0...\n");
      cmd->unk_60 = 0;
      DmaStereoVagCmd = sibling;
      if (sibling) {
        sibling->dma_iop_mem_ptr = iop_mem + size_one_side;
        sibling->xfer_size = size_one_side;
        sibling->unk_240_flag0 = cmd->unk_240_flag0;
        cmd->chan = chan;
      }
    }
    SpuDmaStatus = 1;
    sceSdSetTransIntrHandler(chan, SpuDmaIntr, 0);
    pending_dma = 1;
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
    sceSdSetTransIntrHandler(DmaVagCmd->chan, nullptr, nullptr);
    if (-1 < DmaVagCmd->chan) {
      snd_FreeSPUDMA(DmaVagCmd->chan);
    }
    DmaVagCmd = nullptr;
    DmaStereoVagCmd = nullptr;
    SpuDmaStatus = 0;
  }
}

}  // namespace jak2