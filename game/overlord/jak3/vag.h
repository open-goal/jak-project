#pragma once

#include "game/overlord/jak3/isocommon.h"
namespace jak3 {
void jak3_overlord_init_globals_vag();

struct ISO_VAGCommand : ISO_Hdr {
  ISOFileDef* vag_file_def = nullptr;        // 44
  VagDirEntry* vag_dir_entry = nullptr;      // 48
  ISO_VAGCommand* stereo_sibling = nullptr;  // 52

  // pointer to IOP memory to DMA to SPU. Points to the data for the next new transfer.
  const u8* dma_iop_mem_ptr = nullptr;  // 56

  // the DMA channel to upload to for sceCdVoiceTrans
  int dma_chan = 0;  // 60

  // if not set, a pending dma interrupt will want to modify this command.
  int safe_to_modify_dma = 0;  // 64

  u32 current_spu_address = 0;  // 68

  char name[48];  // 72
  char overflow[16];

  // SPU address of the next chunk to fill
  // for stereo mode, there's a 0x2000 offset between the left and right audio. This stream_sram
  // doesn't include that offset.
  u32 stream_sram;  // 124

  // spu voice for playback
  int voice;  // 132

  int maybe_sound_handler = 0;  // 140

  int oog = 0;             // 180
  int some_pan_thing = 0;  // 184
  int clocka = 0;          // 188
  int clockb = 0;          // 192
  int clockc = 0;          // 196
  int clockd = 0;          // 200

  // if we need to do a second SPU DMA for stereo's second channel, the size of that transfer
  int xfer_size;

  int pitch1;       // pitch to use for playback, possibly overwritten
  int pitch1_file;  // pitch to use for playback, from the file itself (sample rate)
  int pitch_cmd;    // pitch mod command (?)

  int num_isobuffered_chunks = 0;

  struct {
    // byte 216
    u8 saw_chunks1 = 0;  // 217

    // will start the voice, but with a pitch of 0.
    u8 paused = 0;  // 218

    u8 running = 0;

    u8 clocks_set = 0;  // 221, set by SetVagClock if it succeeds.

    u8 file_disappeared = 0;  // 222, set if SetVagClock notices that bBaseFile is gone!

    // set if we are the non-main stereo command.
    u8 stereo_secondary = 0; // 227

    // set if SPU DMA has completed for an even or odd number of chunks of non-stereo audio.
    u8 dma_complete_even_chunk_count = 0;  // 234
    u8 dma_complete_odd_chunk_count = 0;   // 235

  } flags;

  int play_volume = 0;  // 284
  int id = 0;           // 288
  int plugin_id = 0;    // 292

  int art_flag = 0;    // 300
  int music_flag = 0;  // 304

  int movie_flag = 0;  // 340
};

int CalculateVAGPitch(int a, int b);
void BlockUntilVoiceSafe(int, int);
void BlockUntilAllVoicesSafe();
void MarkVoiceKeyedOnOff(int voice, u32 systime);
void UnPauseVAG(ISO_VAGCommand* cmd);
ISO_VAGCommand* FindMusicStreamName(const char* name);
ISO_VAGCommand* SmartAllocMusicVagCommand(const ISO_VAGCommand* user_command, int flag);
void InitVAGCmd(ISO_VAGCommand* cmd, int flag);
}  // namespace jak3