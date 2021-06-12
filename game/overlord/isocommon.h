#pragma once

/*!
 * @file isocommon.h
 * Common ISO utilities.
 */

#ifndef JAK_V2_ISOCOMMON_H
#define JAK_V2_ISOCOMMON_H

#include <string>
#include "common/common_types.h"
#include "common/link_types.h"
#include "game/common/overlord_common.h"

constexpr int PRI_STACK_LENGTH = 4;  // number of queued commands per priority
constexpr int N_PRIORITIES = 4;      // number of priorities

constexpr u32 CMD_STATUS_READ_ERR = 8;              // read encountered a problem or was canceled.
constexpr u32 CMD_STATUS_NULL_CB = 7;               // status returned if you don't set a callback
constexpr u32 CMD_STATUS_FAILED_TO_OPEN = 6;        // status if file couldn't be opened
constexpr u32 CMD_STATUS_FAILED_TO_QUEUE = 2;       // status if we couldn't be queued
constexpr u32 CMD_STATUS_IN_PROGRESS = 0xffffffff;  // status if command is running and healthy
constexpr u32 CMD_STATUS_DONE = 0;                  // status if command is done.

constexpr int BUFFER_PAGE_SIZE = 0xc000;      // size in bytes of normal read buffer
constexpr int STR_BUFFER_DATA_SIZE = 0x6000;  // size in bytes of vag read buffer

constexpr int LOAD_TO_EE_CMD_ID = 0x100;         // command to load file to ee
constexpr int LOAD_TO_IOP_CMD_ID = 0x101;        // command to load to iop
constexpr int LOAD_TO_EE_OFFSET_CMD_ID = 0x102;  // command to load file to ee with offset.
constexpr int LOAD_DGO_CMD_ID = 0x200;           // command to load DGO
constexpr int LOAD_SOUND_BANK = 0x300;           // Command to load a sound bank

constexpr int MAX_ISO_FILES = 350;  // maximum files on FS
constexpr int MAX_OPEN_FILES = 16;  // maximum number of open files at a time.

/*!
 * Record for file. There is one for each file in the FS, and pointers to each FileRecord act as
 * an identifier.
 * The location/size can't be counted on to be anything meaningful as it depends on the IsoFs
 * implementation being used.
 */
struct FileRecord {
  char name[12];
  uint32_t location;
  uint32_t size;
};

/*!
 * Record for an open file.
 */
struct LoadStackEntry {
  FileRecord* fr;
  uint32_t location;  // sectors.
};

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

struct IsoMessage;
struct LoadStackEntry;

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
 * Command to do something.
 */
struct VagCommand : public IsoMessage {
  u32 field_0x30;
  u32 field_0x34;
  u32 field_0x38;
  u32 field_0x3c;
  u32 field_0x40;
  u32 field_0x44;
  u32 field_0x48;
  u32 field_0x4c;
  // 0x6c max
};

struct SoundBank;

struct SoundBankLoadCommand : public IsoMessage {
  char bank_name[16];
  SoundBank* bank;
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
  Finish_Dgo = 6
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
 * Priority Stack entry.
 */
struct PriStackEntry {
  IsoMessage* cmds[PRI_STACK_LENGTH];   // cmds at this priority
  std::string names[PRI_STACK_LENGTH];  // my addition for debug
  uint32_t n;                           // how many in this priority?

  void reset();
};

/*!
 * API to access files. There are debug modes + reading from an ISO filesystem.
 */
struct IsoFs {
  int (*init)(u8*);                                         // 0
  FileRecord* (*find)(const char*);                         // 4
  FileRecord* (*find_in)(const char*);                      // 8
  uint32_t (*get_length)(FileRecord*);                      // c
  LoadStackEntry* (*open)(FileRecord*, int32_t);            // 10
  LoadStackEntry* (*open_wad)(FileRecord*, int32_t);        // 14
  void (*close)(LoadStackEntry*);                           // 18
  uint32_t (*begin_read)(LoadStackEntry*, void*, int32_t);  // 1c
  uint32_t (*sync_read)();                                  // 20
  uint32_t (*load_sound_bank)(char*, void*);                // 24
  uint32_t (*load_music)(char*, void*);
  void (*poll_drive)();
};

extern IsoFs* isofs;
extern s32 iso_mbx;

void MakeISOName(char* dst, const char* src);
void ISONameFromAnimationName(char* dst, const char* src);

#endif  // JAK_V2_ISOCOMMON_H
