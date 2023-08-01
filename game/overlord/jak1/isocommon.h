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

struct SoundBank;

namespace jak1 {
struct VagDirEntry;

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
 * Record for an open file.
 */
struct LoadStackEntry {
  FileRecord* fr;
  uint32_t location;  // sectors.
  FILE* fp;
};

/*!
 * API to access files. There are debug modes + reading from an ISO filesystem.
 */
struct IsoFs {
  int (*init)();                                            // 0
  FileRecord* (*find)(const char*);                         // 4
  FileRecord* (*find_in)(const char*);                      // 8
  uint32_t (*get_length)(FileRecord*);                      // c
  LoadStackEntry* (*open)(FileRecord*, int32_t);            // 10
  LoadStackEntry* (*open_wad)(FileRecord*, int32_t);        // 14
  void (*close)(LoadStackEntry*);                           // 18
  uint32_t (*begin_read)(LoadStackEntry*, void*, int32_t);  // 1c
  uint32_t (*sync_read)();                                  // 20
  uint32_t (*load_sound_bank)(char*, SoundBank*);           // 24
  uint32_t (*load_music)(char*, s32*);
  void (*poll_drive)();
};

struct IsoMessage;

/*!
 * Header for a ISO data buffer.
 */
struct IsoBufferHeader {
  void* data;          // 0
  uint32_t data_size;  // 1
  uint32_t buffer_size;
  void* next;

  // follows the header.
  u8* get_data() { return ((u8*)this) + sizeof(IsoBufferHeader); }
};

//! Callback function for data loads.
typedef u32 (*iso_callback_func)(IsoMessage* cmd, IsoBufferHeader* buffer);

/*!
 * Command, common parent.
 */
struct IsoMessage {
  uint32_t field_0x0;                   // 0x00
  uint32_t field_0x4;                   // 0x04
  uint32_t cmd_id;                      // 0x08
  uint32_t status;                      // 0x0c
  s32 messagebox_to_reply;              // 0x10
  s32 thread_id;                        // 0x14
  uint32_t ready_for_data;              // 0x18
  IsoBufferHeader* callback_buffer;     // 0x1c
  iso_callback_func callback_function;  // 0x20
  LoadStackEntry* fd;                   // 0x24
};

/*!
 * Command to load a single file.
 */
struct IsoCommandLoadSingle : public IsoMessage {
  FileRecord* file_record;  // 0x28
  u8* dest_addr;            // 0x2c
  s32 length;               // 0x30
  s32 length_to_copy;       // 0x34
  u32 offset;               // 0x38
  u8* dst_ptr;              // 0x3c
  s32 bytes_done;           // 0x40
};

/*!
 * DGO Load State Machine states.
 */
enum class DgoState {
  Init = 0,
  Read_Header = 1,
  Finish_Obj = 2,
  Read_Last_Obj = 3,
  Read_Obj_Header = 4,
  Read_Obj_data = 5,
  Finish_Dgo = 6,
  Finish_Obj_NoDoubleBuffer = 7,  // jak 2 only
};

/*!
 * Command to load a DGO.
 */
struct DgoCommand : public IsoMessage {
  FileRecord* fr;      // 0x28, DGO file that's open
  u8* buffer1;         // 0x2c, first EE buffer
  u8* buffer2;         // 0x30, second EE buffer
  u8* buffer_heaptop;  // 0x34, top of the heap

  DgoHeader dgo_header;    // 0x38, current DGO's header
  ObjectHeader objHeader;  // 0x78, current obj's header

  u8* ee_destination_buffer;  // 0xb8, where we are currently loading to on ee
  u32 bytes_processed;        // 0xbc, how many bytes processed in the current state
  u32 objects_loaded;         // 0xc0, completed object count
  DgoState dgo_state;         // 0xc4, state machine state
  u32 finished_first_obj;     // 0xc8, have we finished loading the first object?
  u32 buffer_toggle;          // 0xcc, which buffer to load into (top, buffer1, buffer2)
  u8* selectedBuffer;         // 0xd0, most recently completed load destination
  u32 want_abort;             // 0xd4, should we quit?
};

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
