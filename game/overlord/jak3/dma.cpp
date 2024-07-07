#include "dma.h"

#include "common/log/log.h"
#include "common/util/Assert.h"

#include "game/overlord/jak3/basefile.h"
#include "game/overlord/jak3/overlord.h"
#include "game/overlord/jak3/vag.h"
#include "game/sce/iop.h"
#include "game/sound/sdshim.h"
#include "game/sound/sndshim.h"

#define VOICE_BIT(voice) (1 << ((voice) >> 1))

namespace jak3 {
using namespace iop;
namespace {

// most recent call to voice_trans_wrapper's arguments
u32 g_voiceTransMode = 0;
u32 g_voiceTransSize = 0;
s16 g_voiceTransChannel = 0;
const void* g_voiceTransAddr = nullptr;
u32 g_voiceTransSpuAddr = 0;

// if we've started a transfer recently
bool g_voiceTransRunning = false;
// when that transfer was started
u32 g_voiceTransTime = 0;

// despite the name, this is really an indicator that the SPU streaming system is waiting
// for a SPU interrupt on completion.
bool g_bSpuDmaBusy = false;
int g_nSpuDmaChannel = 0;

ISO_VAGCommand* g_pDmaVagCmd = nullptr;
ISO_VAGCommand* g_pDmaStereoVagCmd = nullptr;

int g_nSpuDmaChunks = 0;

std::array<DmaQueueEntry, 16> g_aSpuDmaQueue;
int g_nSpuDmaQueueHead = 0;
int g_nSpuDmaQueueTail = 0;
int g_nSpuDmaQueueCount = 0;

struct DmaInterruptHandlerHack {
  s32 chan = 0;
  sceSdTransIntrHandler cb = nullptr;
  void* data;
  int countdown = 0;
} g_DmaInterruptHack;

}  // namespace

void jak3_overlord_init_globals_dma() {
  g_voiceTransMode = 0;
  g_voiceTransSize = 0;
  g_voiceTransChannel = 0;
  g_voiceTransAddr = nullptr;
  g_voiceTransSpuAddr = 0;
  g_voiceTransRunning = false;
  g_voiceTransTime = 0;
  g_bSpuDmaBusy = false;
  g_nSpuDmaChannel = 0;
  g_pDmaVagCmd = nullptr;
  g_pDmaStereoVagCmd = nullptr;
  g_nSpuDmaChunks = 0;
  g_aSpuDmaQueue = {};
  g_nSpuDmaQueueHead = 0;
  g_nSpuDmaQueueCount = 0;
  g_nSpuDmaQueueTail = 0;
  g_DmaInterruptHack = {};
}

// The DMA callback hack below is used to defer dma completion "interrupts" until the next run
// of the ISO Thread. This avoids re-entry type problems where the original design would set off
// a dma transfer in the completion handler of the previous transfer, and expect a few instructions
// to run after.

void uninstall_dma_intr() {
  g_DmaInterruptHack = {};
}

void set_dma_intr_handler_hack(s32 chan, sceSdTransIntrHandler cb, void* data) {
  ASSERT(!g_DmaInterruptHack.cb);
  g_DmaInterruptHack.chan = chan;
  g_DmaInterruptHack.cb = cb;
  g_DmaInterruptHack.data = data;
  g_DmaInterruptHack.countdown = 10;
}

int SPUDmaIntr(int channel, void* userdata);

void dma_intr_hack() {
  if (g_DmaInterruptHack.countdown) {
    g_DmaInterruptHack.countdown--;
    if (g_DmaInterruptHack.countdown == 0) {
      int chan = g_DmaInterruptHack.chan;
      void* data = g_DmaInterruptHack.data;
      g_DmaInterruptHack = {};
      SPUDmaIntr(chan, data);
    }
  }
}

/*!
 * This function is used to set up a DMA transfer to SPU DMA.
 *
 * This wrapper was added very close to the end of Jak 3's development.
 *
 * I believe it basically checks for dma transfers that are somehow "dropped", and retries them.
 * Since I don't think our IOP framework will ever do this, we have an assert if the dropped logic
 * ever goes off.
 */
int voice_trans_wrapper(s16 chan, u32 mode, const void* iop_addr, u32 spu_addr, u32 size) {
  // remember the transfer settings. If there's a transfer in progress, so we can't start here,
  // we'll use these to start the transfer later.
  g_voiceTransMode = mode;
  g_voiceTransSize = size;
  g_voiceTransChannel = chan;
  g_voiceTransAddr = iop_addr;
  g_voiceTransSpuAddr = spu_addr;

  if (g_voiceTransRunning) {
    // I claim this should never happen, and this is their workaround for a bug.
    ASSERT_NOT_REACHED();
    return -0xd2;  // busy
  } else {
    g_voiceTransRunning = true;
    g_voiceTransTime = GetSystemTimeLow();
    return sceSdVoiceTrans(chan, mode, iop_addr, spu_addr, size);
  }
}

u32 read_rate_calc(u32 pitch) {
  u64 pitch1 = (pitch >> 3);
  u64 mult_result = pitch1 * 0x2492'4925ull;
  return mult_result >> 32;
}

/*!
 * The worst function of all time - the SPU DMA completion interrupt.
 */
int SPUDmaIntr(int channel, void* userdata) {
  ovrld_log(LogCategory::SPU_DMA_STR, "SPUDmaIntr enter! {} 0x{:x}", channel, (u64)userdata);

  if (!g_bSpuDmaBusy) {
    // we got an interrupt, but weren't expecting it, or no longer have the need for the data.
    ovrld_log(LogCategory::SPU_DMA_STR, "SPUDmaIntr exit - not busy");
    return 0;
  }

  if (channel != g_nSpuDmaChannel) {
    // interrupt was for the wrong channel, somehow.
    ovrld_log(LogCategory::SPU_DMA_STR, "SPUDmaIntr exit - not our channel ??");
    return 0;
  }

  // since we're in the completion handler, we know that there is no voice trans (SPU DMA) running.
  g_voiceTransRunning = false;

  // This next block will handle updating the playback command that triggered this dma:
  if (g_pDmaVagCmd) {
    ovrld_log(LogCategory::SPU_DMA_STR, "SPUDma for cmd {}", g_pDmaVagCmd->name);
    if (!g_pDmaStereoVagCmd) {
      // non-stereo audio

      // set a flag to indicate even/odd number of chunks have been dma'd
      if ((g_nSpuDmaChunks & 1) == 0) {
        g_pDmaVagCmd->flags.dma_complete_even_chunk_count = 1;
      } else {
        g_pDmaVagCmd->flags.dma_complete_odd_chunk_count = 1;
      }
    } else {
      // stereo audio. This requires two uploads, one for left/right audio. If we've finished the
      // first, start the second one here:
      if (g_pDmaStereoVagCmd->xfer_size) {
        // parameters for second upload
        int chan = g_pDmaVagCmd->dma_chan;
        const u8* iop_addr = g_pDmaStereoVagCmd->dma_iop_mem_ptr;
        int size = g_pDmaStereoVagCmd->xfer_size;

        // SPU addr - toggle the buffer based on stereo side:
        // TODO: better explanation of why this picks the correct buffer.
        int spu_addr;
        if ((g_nSpuDmaChunks & 1) == 0) {
          spu_addr = g_pDmaStereoVagCmd->stream_sram;
        } else {
          spu_addr = g_pDmaStereoVagCmd->stream_sram + 0x2000;
        }

        // these lines reordered to possibly support immediate dma completion callback??

        // clear flag so we know not to transfer the next part
        g_pDmaStereoVagCmd->xfer_size = 0;
        g_pDmaStereoVagCmd->dma_iop_mem_ptr = nullptr;

        // start next transfer
        ovrld_log(LogCategory::SPU_DMA_STR, "SPUDmaIntr starting stereo sibling transfer");
        set_dma_intr_handler_hack(g_nSpuDmaChannel, SPUDmaIntr, userdata);
        voice_trans_wrapper(chan, 0, iop_addr, spu_addr, size);
        return 0;
      }

      // second stereo upload completed - update double-buffering flags
      if ((g_nSpuDmaChunks & 1) == 0) {
        g_pDmaVagCmd->flags.dma_complete_even_chunk_count = 1;
        g_pDmaStereoVagCmd->flags.dma_complete_even_chunk_count = 1;
      } else {
        g_pDmaVagCmd->flags.dma_complete_odd_chunk_count = 1;
        g_pDmaStereoVagCmd->flags.dma_complete_odd_chunk_count = 1;
      }
    }

    // if this is the first chunk, we'll start the actual audio here:
    // lg::warn("----------> interrupt with chunks {}\n", g_nSpuDmaChunks);
    ovrld_log(LogCategory::SPU_DMA_STR, "SPUDmaIntr chunks count {}", g_nSpuDmaChunks);

    if (g_nSpuDmaChunks == 0) {
      // compute pitch/playback rate
      int pitch = CalculateVAGPitch(g_pDmaVagCmd->pitch1, g_pDmaVagCmd->pitch_cmd);
      ASSERT(pitch == (pitch & 0xffff));

      // inform the ISO system how fast we're reading
      if (g_pDmaVagCmd->m_pBaseFile) {
        // unlike actual playback, this is done with the pitch1 value from the file itself - so if
        // we speed up/slow down stuff in debug, it won't change streaming modes
        const int pitch_from_file =
            CalculateVAGPitch(g_pDmaVagCmd->pitch1_file, g_pDmaVagCmd->pitch_cmd);
        int rate = g_pDmaStereoVagCmd ? pitch_from_file * 0x2ee : pitch_from_file * 0x177;
        g_pDmaVagCmd->m_pBaseFile->m_ReadRate = read_rate_calc(rate);
      }

      // start!
      u32 voice_mask = 0;
      if (!g_pDmaStereoVagCmd) {
        // forget any previous spu address
        g_pDmaVagCmd->current_spu_address = 0;

        static_assert(SD_VA_SSA == 0x2040);
        static_assert(SD_S_KOFF == 0x1600);
        static_assert(SD_S_KON == 0x1500);

        static_assert(SD_VP_ADSR1 == 0x300);
        static_assert(SD_VP_ADSR2 == 0x400);
        static_assert(SD_VP_PITCH == 0x200);

        // before touching SPU2 hardware, wait for voice safety:
        BlockUntilVoiceSafe(g_pDmaVagCmd->voice, 0x900);

        // set address and ADSR settings
        sceSdSetAddr(g_pDmaVagCmd->voice | SD_VA_SSA, g_pDmaVagCmd->stream_sram + 0x30);
        sceSdSetParam(g_pDmaVagCmd->voice | SD_VP_ADSR1, 0xff);
        sceSdSetParam(g_pDmaVagCmd->voice | SD_VP_ADSR2, 0x1fc0);
        if (g_pDmaVagCmd->flags.paused) {
          pitch = 0;
        }
        sceSdSetParam(g_pDmaVagCmd->voice | SD_VP_PITCH, pitch);
        voice_mask = VOICE_BIT(g_pDmaVagCmd->voice);
      } else {
        // forget any previous spu address
        g_pDmaVagCmd->current_spu_address = 0;
        g_pDmaStereoVagCmd->current_spu_address = 0;

        // wait for voices to be safe to adjust
        BlockUntilVoiceSafe(g_pDmaVagCmd->voice, 0x900);
        BlockUntilVoiceSafe(g_pDmaStereoVagCmd->voice, 0x900);

        // set voice params
        sceSdSetAddr(g_pDmaVagCmd->voice | SD_VA_SSA, g_pDmaVagCmd->stream_sram + 0x30);
        sceSdSetAddr(g_pDmaStereoVagCmd->voice | SD_VA_SSA, g_pDmaStereoVagCmd->stream_sram + 0x30);
        sceSdSetParam(g_pDmaVagCmd->voice | SD_VP_ADSR1, 0xff);
        sceSdSetParam(g_pDmaStereoVagCmd->voice | SD_VP_ADSR1, 0xff);
        sceSdSetParam(g_pDmaVagCmd->voice | SD_VP_ADSR2, 0x1fc0);
        sceSdSetParam(g_pDmaStereoVagCmd->voice | SD_VP_ADSR2, 0x1fc0);
        if (g_pDmaVagCmd->flags.paused) {
          pitch = 0;
        }
        sceSdSetParam(g_pDmaVagCmd->voice | SD_VP_PITCH, pitch);
        sceSdSetParam(g_pDmaStereoVagCmd->voice | SD_VP_PITCH, pitch);
        voice_mask = VOICE_BIT(g_pDmaVagCmd->voice) | VOICE_BIT(g_pDmaStereoVagCmd->voice);
      }

      // do key-on or key-off
      if (g_pDmaVagCmd->flags.paused) {
        ovrld_log(LogCategory::SPU_DMA_STR, "SPUDmaIntr chunks 0, key off");
        BlockUntilAllVoicesSafe();
        sceSdSetSwitch(SD_S_KOFF | (g_pDmaVagCmd->voice & 1), voice_mask);
      } else {
        ovrld_log(LogCategory::SPU_DMA_STR, "SPUDmaIntr chunks 0, key on");
        BlockUntilAllVoicesSafe();
        sceSdSetSwitch(SD_S_KON | (g_pDmaVagCmd->voice & 1), voice_mask);
      }

      // remember the time of the key-on/off. This is used to avoid sending voice commands
      // quickly, which somehow confuses the sound hardware.
      auto sys_time = GetSystemTimeLow();
      MarkVoiceKeyedOnOff(g_pDmaVagCmd->voice, sys_time);
      if (g_pDmaStereoVagCmd) {
        MarkVoiceKeyedOnOff(g_pDmaStereoVagCmd->voice, sys_time);
      }
    } else if (g_nSpuDmaChunks == 1) {
      g_pDmaVagCmd->flags.saw_chunks1 = 1;
      if (g_pDmaStereoVagCmd) {
        g_pDmaStereoVagCmd->flags.saw_chunks1 = 1;
      }

      if (g_pDmaVagCmd->flags.paused) {
        ovrld_log(LogCategory::SPU_DMA_STR, "SPUDmaIntr chunks 1, pausing");
        u32 voice_mask = 0;
        if (!g_pDmaStereoVagCmd) {
          // pause by setting pitches to 0
          sceSdSetParam(g_pDmaVagCmd->voice | SD_VP_PITCH, 0);
          BlockUntilVoiceSafe(VOICE_BIT(g_pDmaVagCmd->voice), 0x900);
          voice_mask = VOICE_BIT(g_pDmaVagCmd->voice);
        } else {
          sceSdSetParam(g_pDmaStereoVagCmd->voice | SD_VP_PITCH, 0);
          sceSdSetParam(g_pDmaVagCmd->voice | SD_VP_PITCH, 0);
          BlockUntilVoiceSafe(VOICE_BIT(g_pDmaVagCmd->voice), 0x900);
          BlockUntilVoiceSafe(VOICE_BIT(g_pDmaStereoVagCmd->voice), 0x900);
          voice_mask = VOICE_BIT(g_pDmaVagCmd->voice) | VOICE_BIT(g_pDmaStereoVagCmd->voice);
        }

        // switch off
        BlockUntilAllVoicesSafe();
        sceSdSetSwitch(SD_S_KOFF | (g_pDmaVagCmd->voice & 1), voice_mask);
        auto sys_time = GetSystemTimeLow();
        MarkVoiceKeyedOnOff(g_pDmaVagCmd->voice, sys_time);
        if (g_pDmaStereoVagCmd) {
          MarkVoiceKeyedOnOff(g_pDmaStereoVagCmd->voice, sys_time);
        }
      } else {
        ovrld_log(LogCategory::SPU_DMA_STR, "SPUDmaIntr chunks 1, unpausing by call to UnPauseVAG");
        g_pDmaVagCmd->flags.paused = 1;
        UnPauseVAG(g_pDmaVagCmd);
      }
    }

    // now that we've processed the command from this interrupt, mark it as safe to modify
    g_pDmaVagCmd->safe_to_modify_dma = 1;
    if (g_pDmaStereoVagCmd) {
      g_pDmaStereoVagCmd->safe_to_modify_dma = 1;
    }
    // and forget it!
    g_pDmaVagCmd = nullptr;
    g_pDmaStereoVagCmd = nullptr;
    ovrld_log(LogCategory::SPU_DMA_STR, "SPUDmaIntr dma handling of VAG cmd is complete");
  }

  // release ref on this page. (interestingly, not a dma ref...)
  if (userdata) {
    CPage* page = (CPage*)userdata;
    int ret = page->ReleaseRef();
    ASSERT(ret >= 0);
  }

  // now - see if we have another queued dma transfer
  ASSERT(g_nSpuDmaQueueCount >= 0);
  if (g_nSpuDmaQueueCount == 0) {
    ovrld_log(LogCategory::SPU_DMA_STR, "SPUDmaIntr dma queue is empty, disabling interrupt");
    // we're done!
    // set_dma_intr_handler_hack(channel, nullptr, nullptr);
    uninstall_dma_intr();
    //  if (-1 < channel) {
    //    snd_FreeSPUDMA(channel);
    //  }
    g_bSpuDmaBusy = false;
  } else {
    ovrld_log(LogCategory::SPU_DMA_STR,
              "SPUDmaIntr dma queue is not empty, preparing to run {} ({} pending)",
              g_nSpuDmaQueueHead, g_nSpuDmaQueueCount);
    // nope, more dma to run
    auto* next_xfer = &g_aSpuDmaQueue[g_nSpuDmaQueueHead];

    // set up the next interrupt handler
    set_dma_intr_handler_hack(channel, SPUDmaIntr, next_xfer->user_data);

    // args for the dma transfer
    int next_chan = channel;
    int next_mode = 0;
    const void* next_iop = next_xfer->iop_mem;
    u32 next_spu = next_xfer->spu_addr;
    u32 next_length = next_xfer->length;

    // load up the commands to handle
    g_pDmaVagCmd = next_xfer->command;
    g_pDmaStereoVagCmd = nullptr;
    if (g_pDmaVagCmd) {
      g_pDmaStereoVagCmd = g_pDmaVagCmd->stereo_sibling;
    }
    g_nSpuDmaChunks = next_xfer->num_isobuffered_chunks;

    // advance the queue!
    g_nSpuDmaQueueCount = g_nSpuDmaQueueCount + -1;
    g_nSpuDmaQueueHead = g_nSpuDmaQueueHead + 1;
    if (0xf < g_nSpuDmaQueueHead) {
      g_nSpuDmaQueueHead = 0;
    }

    // start the next one!
    // set_dma_intr_handler_hack(g_nSpuDmaChannel, SPUDmaIntr, userdata);
    voice_trans_wrapper(next_chan, next_mode, next_iop, next_spu, next_length);
  }

  ovrld_log(LogCategory::SPU_DMA_STR, "SPUDmaIntr exit - end of function");
  return 0;
}

/*!
 * Start DMA to EE.
 */
void DMA_SendToEE(void* ee_dest,
                  const void* iop_src,
                  u32 length,
                  void callback(void*),
                  void* callback_arg) {
  ASSERT(iop_src);
  ASSERT(ee_dest);
  ASSERT(((uintptr_t)iop_src & 3) == 0);
  ASSERT(((uintptr_t)ee_dest & 0xf) == 0);
  ASSERT(length < 0xffff0);
  sceSifDmaData cmd;  // DMA settings

  // setup command
  cmd.mode = 0;
  cmd.data = iop_src;
  cmd.addr = ee_dest;
  cmd.size = length;

  // instant DMA
  // ovrld_log(LogCategory::EE_DMA, "DMA_SendToEE: 0x{:x}, size {}", (u64)ee_dest, length);
  sceSifSetDma(&cmd, 1);

  // for now, we'll do the callback here, but I bet it will cause problems
  if (callback) {
    callback(callback_arg);
  }
}

/*!
 * Start DMA transfer to SPU. Despite the name, this does not actually "sync" - the transfer will
 * be ongoing. If there is an ongoing transfer when this is called, the transfer will be queued.
 */
int DMA_SendToSPUAndSync(const u8* iop_mem,
                         int length,
                         int spu_addr,
                         ISO_VAGCommand* cmd,
                         void* user_data) {
  // CpuSuspendIntr(local_28);
  int ret = 1;
  bool defer = false;

  ovrld_log(LogCategory::SPU_DMA_STR,
            "DMA to SPU requested for {}, {} bytes to 0x{:x}, currently busy? {}",
            cmd ? cmd->name : "NO-CMD", length, spu_addr, g_bSpuDmaBusy);
  if (g_bSpuDmaBusy == 0) {
    // not busy, we can actually start dma now.
    g_nSpuDmaChannel = snd_GetFreeSPUDMA();
    if (g_nSpuDmaChannel == -1) {
      return 0;
    }
    // set globals for DMA processing
    if (cmd) {
      g_nSpuDmaChunks = cmd->num_isobuffered_chunks;
      g_pDmaStereoVagCmd = cmd->stereo_sibling;
      g_pDmaVagCmd = cmd;
    }

  } else {
    // busy, need to queue the dma
    ASSERT(g_nSpuDmaQueueCount <= (int)g_aSpuDmaQueue.size());

    // set values:
    g_aSpuDmaQueue[g_nSpuDmaQueueTail].length = length;
    g_aSpuDmaQueue[g_nSpuDmaQueueTail].spu_addr = spu_addr;
    g_aSpuDmaQueue[g_nSpuDmaQueueTail].user_data = user_data;
    g_aSpuDmaQueue[g_nSpuDmaQueueTail].num_isobuffered_chunks =
        cmd ? cmd->num_isobuffered_chunks : 0;
    g_aSpuDmaQueue[g_nSpuDmaQueueTail].command = cmd;
    g_aSpuDmaQueue[g_nSpuDmaQueueTail].iop_mem = iop_mem;
    g_nSpuDmaQueueCount = g_nSpuDmaQueueCount + 1;
    g_nSpuDmaQueueTail = g_nSpuDmaQueueTail + 1;
    if (0xf < g_nSpuDmaQueueTail) {
      g_nSpuDmaQueueTail = 0;
    }
    defer = true;
  }

  // set up the stereo command
  if (cmd) {
    cmd->safe_to_modify_dma = 0;
    auto* stereo = cmd->stereo_sibling;
    if (stereo) {
      stereo->num_isobuffered_chunks = cmd->num_isobuffered_chunks;
      stereo->dma_iop_mem_ptr = iop_mem + length;
      cmd->dma_chan = g_nSpuDmaChannel;
      stereo->xfer_size = length;
    }
  }

  // kick off dma, if we decided not to queue.
  if (!defer) {
    g_bSpuDmaBusy = true;
    set_dma_intr_handler_hack(g_nSpuDmaChannel, SPUDmaIntr, user_data);
    voice_trans_wrapper(g_nSpuDmaChannel, 0, iop_mem, spu_addr, length);
  }
  return ret;
}

/*!
 * Run a dma transfer that was delayed or dropped.
 */
void RunDeferredVoiceTrans() {
  // only if there's a currently happening transfer.
  if (g_voiceTransRunning) {
    if (GetSystemTimeLow() - g_voiceTransTime > 0x384000) {
      ovrld_log(LogCategory::WARN, "DeferredVoiceTrans has detected hung dma... expect problems.");
      // original game also check sceSdVoiceTransStatus here, we'll possibly need to mess with this
      // if we delay dma completion interrupts...
      g_voiceTransRunning = false;
      voice_trans_wrapper(g_voiceTransChannel, g_voiceTransMode, g_voiceTransAddr,
                          g_voiceTransSpuAddr, g_voiceTransSize);
    }
  }
}
}  // namespace jak3