#pragma once

/*!
 * @file isocommon.h
 * Common ISO utilities.
 */

#include <string>

#include "common/common_types.h"
#include "common/link_types.h"

#include "game/common/overlord_common.h"
#include "game/overlord/common/isocommon.h"
#include "game/overlord/jak1/ssound.h"

struct VagDirEntry;
struct SoundBank;

namespace jak1 {
constexpr int PRI_STACK_LENGTH = 4;  // number of queued commands per priority
constexpr int N_PRIORITIES = 4;      // number of priorities

constexpr int BUFFER_PAGE_SIZE = 0xc000;      // size in bytes of normal read buffer
constexpr int STR_BUFFER_DATA_SIZE = 0x6000;  // size in bytes of vag read buffer

constexpr int LOAD_SOUND_BANK = 0x300;      // Command to load a sound bank
constexpr int LOAD_MUSIC = 0x380;           // Command to load music
constexpr int QUEUE_VAG_STREAM = 0x400;     // Command to load a vag stream
constexpr int PLAY_VAG_STREAM = 0x401;      // Command to play a vag stream
constexpr int STOP_VAG_STREAM = 0x402;      // Command to stop a vag stream
constexpr int PAUSE_VAG_STREAM = 0x403;     // Command to pause a vag stream
constexpr int CONTINUE_VAG_STREAM = 0x404;  // Command to continue a vag stream
constexpr int SET_VAG_VOLUME = 0x405;       // Command to set the volume of vag playback
constexpr int SET_DIALOG_VOLUME = 0x406;    // Command to set the volume of vag playback
/*!
 * Command to do something.
 */
struct VagCommand : public IsoMessage {
  FileRecord* file;
  VagDirEntry* vag;
  u32 buffer_number;
  u32 data_left;
  u32 started;
  u32 paused;
  u32 sample_rate;
  u32 stop;
  s32 end_point;
  u32 unk2;
  s32 volume;
  u32 sound_id;
  u32 priority;
  u32 positioned;
  Vec3w trans;
};

struct SoundBankLoadCommand : public IsoMessage {
  char bank_name[16];
  SoundBank* bank;
};

struct MusicLoadCommand : public IsoMessage {
  char music_name[16];
  s32* music_handle;
};

/*!
 * Priority Stack entry.
 */
struct PriStackEntry {
  IsoMessage* cmds[PRI_STACK_LENGTH];   // cmds at this priority
  std::string names[PRI_STACK_LENGTH];  // my addition for debug
  uint32_t n;                           // how many in this priority?

  void reset();
};

}  // namespace jak1