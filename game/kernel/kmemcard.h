#pragma once

/*!
 * @file kmemcard.h
 * Memory card interface. Very messy code.
 */

#include "common/common_types.h"
#include "kmachine.h"

void kmemcard_init_globals();

constexpr s32 SAVE_SIZE = 0x2b3;  // likely different by versions!

enum class MemoryCardState : u32 { UNKNOWN = 0, KNOWN = 1, OPEN = 2, FORMATTED = 3 };

// cached in ee memory so we can preview.
struct MemoryCardFile {
  u32 present;
  u32 pad1;
  u32 pad2;
  u8 data[64];
};

// type of the mc field.
struct MemoryCard {
  MemoryCardState state;
  u32 handle;
  u32 formatted;
  u32 inited;
  u32 last_file;
  u32 mem_size;
  MemoryCardFile files[4];
};

enum class MemoryCardOperationKind : u32 {
  NO_OP = 0,
  FORMAT = 1,       // (handle, unused), (slot, type, free, format)
  UNFORMAT = 2,     // (handle, unused), (slot)
  CREATE_FILE = 3,  // (handle, unused)
  SAVE = 4,
  LOAD = 5,
};

enum class McStatusCode : u32 {
  BUSY = 0,
  OK = 1,
  BAD_HANDLE = 2,
  FORMAT_FAILED = 3,
  INTERNAL_ERROR = 4,
  WRITE_ERROR = 5,
  READ_ERROR = 6,
  NEW_GAME = 7,
  NO_MEMORY = 8,
  NO_CARD = 9,
  NO_LAST = 10,
  NO_FORMAT = 11,
  NO_FILE = 12,
  NO_SAVE = 13,
  NO_SPACE = 14,
  BAD_VERSION = 15,
  NO_PROCESS = 16,
  NO_AUTO_SAVE = 17
};

struct MemoryCardOperation {
  MemoryCardOperationKind operation;
  uint32_t param;
  uint32_t param2;
  McStatusCode result;
  uint32_t counter;
  Ptr<u8> data_ptr;
  Ptr<u8> data_ptr2;
};

struct mc_file_info {
  u32 present;
  u8 data[64];
};

struct mc_slot_info {
  u32 handle;
  u32 known;
  u32 formatted;
  u32 initted;
  s32 last_file;
  u32 mem_required;
  u32 mem_actual;
  mc_file_info files[4];
};

void MC_set_language(s32 lang);
void MC_run();
u64 MC_format(s32 card_idx);
u64 MC_unformat(s32 card_idx);
u64 MC_createfile(s32 param, Ptr<u8> data);
u64 MC_save(s32 card_idx, s32 file_idx, Ptr<u8> save_data, Ptr<u8> save_summary_data);
u64 MC_load(s32 card_idx, s32 file_idx, Ptr<u8> data);
void MC_makefile(s32 port, s32 size);
void MC_get_status(s32 slot, Ptr<mc_slot_info> info);
u32 MC_check_result();