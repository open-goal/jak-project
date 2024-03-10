#include "dma.h"

#include "common/log/log.h"
#include "common/util/string_util.h"

#include "game/overlord/jak2/ssound.h"
#include "game/overlord/jak2/vag.h"
#include "game/sound/sdshim.h"
#include "game/sound/sndshim.h"

namespace jak2 {

// This file has SPU DMA functions. Unlike Jak 1, they run SPU DMA in the background, and it doesn't
// work correctly if we assume instant DMA. We end up running the DMA finished interrupt handler
// in the loop of ISO thread - this makes sure that DMA appears to finish pretty quickly, and before
// any more loading stuff happens (so loads will never "wait" for the fake simulated dma).

s32 SpuDmaStatus = 0;     //! set to 1 when SPU DMA is in progress
VagCmd* DmaVagCmd;        //! VagCmd currently doing SPU DMA
VagCmd* DmaStereoVagCmd;  //! VagCmd for the stereo sibling SPU DMA
int pending_dma = 0;      //! PC-port addition to indicate that "dma is running"

constexpr int kDmaDelay = 10;

void dma_init_globals() {
  SpuDmaStatus = 0;
  DmaVagCmd = nullptr;
  DmaStereoVagCmd = nullptr;
  pending_dma = 0;
}

/*!
 * Interrupt handler for DMA completion.
 */
int SpuDmaIntr(int, void*) {
  if (SpuDmaStatus != 1) {
    return 0;
  }

  if (!DmaVagCmd) {
    goto cleanup;
  }

  if (!DmaStereoVagCmd) {
    // not stereo mode, just set status bits
    if ((DmaVagCmd->num_processed_chunks & 1U) == 0) {
      DmaVagCmd->sb_even_buffer_dma_complete = 1;
    } else {
      DmaVagCmd->sb_odd_buffer_dma_complete = 1;
    }
  } else {
    // in stereo mode, we need to do two DMA transfers. The first one starts the second stereo one.
    if (DmaStereoVagCmd->xfer_size != 0) {
      // pick the appropriate double buffer
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

      // clear the pending transfer, so we don't try to start it again
      int old_xfer = DmaStereoVagCmd->xfer_size;
      DmaStereoVagCmd->xfer_size = 0;
      DmaStereoVagCmd->dma_iop_mem_ptr = nullptr;

      // start the transfer!
      pending_dma = kDmaDelay;
      // TODO - temporary hack, `channel` being set to -1 is what causes the lifeseed cutscene crash
      // narrow down why/where -1 is coming from
      // - https://github.com/open-goal/jak-project/issues/2988
      if (chan < 0) {
        lg::error("SND-ERROR: channel was invalid: {} for command: {}", chan,
                  DmaStereoVagCmd->name);
        return -1;
      }
      sceSdVoiceTrans((int)chan, 0, iop_ptr, spu_addr, old_xfer);
      // and return. This will get called again on completion
      return 0;
    }

    // if we made it here, both transfers are done, so just toggle the buffers.
    if ((DmaVagCmd->num_processed_chunks & 1U) == 0) {
      DmaVagCmd->sb_even_buffer_dma_complete = 1;
      DmaStereoVagCmd->sb_even_buffer_dma_complete = 1;
    } else {
      DmaVagCmd->sb_odd_buffer_dma_complete = 1;
      DmaStereoVagCmd->sb_odd_buffer_dma_complete = 1;
    }
  }

  // if we just finished the first upload, start playing! I'm not entirely sure if this is playing
  // sound yet, or if we're just looping and waiting for the second upload to actually start.
  // (we don't set the appropriate volumes here, so this is probably not the real playback)
  if (DmaVagCmd->num_processed_chunks == 1) {
    int pitch = CalculateVAGPitch(DmaVagCmd->pitch1, DmaVagCmd->unk_256_pitch2);
    if (!DmaStereoVagCmd) {
      DmaVagCmd->spu_addr_to_start_playing = 0;
      sceSdSetAddr(SD_VA_SSA | DmaVagCmd->voice, DmaVagCmd->spu_stream_dma_mem_addr + 0x30);
      sceSdSetParam(SD_VP_ADSR1 | DmaVagCmd->voice, 0xf);
      sceSdSetParam(SD_VP_ADSR2 | DmaVagCmd->voice, 0x1fc0);
      sceSdSetParam(SD_VP_PITCH | DmaVagCmd->voice, pitch);

      sceSdSetSwitch(SD_S_KON | (DmaVagCmd->voice & 1), VOICE_BIT(DmaVagCmd->voice));
    } else {
      // same for stereo, but we start both voices.
      DmaVagCmd->spu_addr_to_start_playing = 0;
      DmaStereoVagCmd->spu_addr_to_start_playing = 0;

      sceSdSetAddr(SD_VA_SSA | DmaVagCmd->voice, DmaVagCmd->spu_stream_dma_mem_addr + 0x30);
      sceSdSetAddr(SD_VA_SSA | DmaStereoVagCmd->voice,
                   DmaStereoVagCmd->spu_stream_dma_mem_addr + 0x30);
      sceSdSetParam(SD_VP_ADSR1 | DmaVagCmd->voice, 0xf);
      sceSdSetParam(SD_VP_ADSR1 | DmaStereoVagCmd->voice, 0xf);
      sceSdSetParam(SD_VP_ADSR2 | DmaVagCmd->voice, 0x1fc0);
      sceSdSetParam(SD_VP_ADSR2 | DmaStereoVagCmd->voice, 0x1fc0);
      sceSdSetParam(SD_VP_PITCH | DmaVagCmd->voice, pitch);
      sceSdSetParam(SD_VP_PITCH | DmaStereoVagCmd->voice, pitch);

      sceSdSetSwitch(SD_S_KON | (DmaVagCmd->voice & 1),
                     VOICE_BIT(DmaVagCmd->voice) | VOICE_BIT(DmaStereoVagCmd->voice));
    }
  } else if (DmaVagCmd->num_processed_chunks == 2) {
    // on the second chunk's DMA finish, start playing by unpausing.

    // set playing flag
    DmaVagCmd->sb_playing = 1;
    if (DmaStereoVagCmd) {
      DmaStereoVagCmd->sb_playing = 1;
    }

    // if we paused since the first, kill the voice.
    if (DmaVagCmd->sb_paused) {
      if (!DmaStereoVagCmd) {
        sceSdSetParam(SD_VP_PITCH | DmaVagCmd->voice, 0);
      } else {
        sceSdSetParam(SD_VP_PITCH | DmaStereoVagCmd->voice, 0);
        sceSdSetParam(SD_VP_PITCH | DmaVagCmd->voice, 0);
        sceSdSetSwitch(SD_S_KOFF | (DmaVagCmd->voice & 1), VOICE_BIT(DmaStereoVagCmd->voice));
      }
      sceSdSetSwitch(SD_S_KOFF | (DmaVagCmd->voice & 1), VOICE_BIT(DmaVagCmd->voice));
      goto hack;
    }

    // mark as paused manually and unpause to start playback.
    printf("start playing from dma unpause\n");
    DmaVagCmd->sb_paused = 1;
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

/*!
 * Call any pending DMA transfer complete interrupt handlers.
 */
void spu_dma_hack() {
  if (pending_dma) {
    pending_dma--;
    if (!pending_dma) {
      SpuDmaIntr(0, nullptr);
    }
  }
}

/*!
 * Despite the name, this does not sync and just starts dma.
 */
bool DMA_SendToSPUAndSync(uint8_t* iop_mem,
                          int size_one_side,
                          int spu_addr,
                          VagCmd* cmd,
                          int /*disable_intr*/) {
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
    int sz = sceSdVoiceTrans((int)(short)chan, 0, iop_mem, spu_addr, size_one_side);
    // if (disable_intr == 1) {
    // CpuResumeIntr(local_28[0]);
    //}
    ret = int(size_one_side + 0x3fU & 0xffffffc0) <= sz;
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
