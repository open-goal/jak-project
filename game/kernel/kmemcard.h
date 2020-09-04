/*!
 * @file kmemcard.h
 * Memory card interfaces. Very messy code.
 */

#ifndef JAK_KMEMCARD_H
#define JAK_KMEMCARD_H

#include "common/common_types.h"
#include "kmachine.h"

void kmemcard_init_globals();

constexpr s32 SAVE_SIZE = 0x2b3;  // likely different by versions!

enum MemoryCardOperationKind {
  NO_OP = 0,
  FORMAT = 1,
  UNFORMAT = 2,
  CREATE_FILE = 3,
  SAVE = 4,
  LOAD = 5,
};

struct MemoryCardOperation {
  uint32_t operation;
  uint32_t param;
  uint32_t param2;
  uint32_t result;
  uint32_t f_10;
  Ptr<u8> data_ptr;
  Ptr<u8> data_ptr2;
};

struct mc_file_info {
  u32 present;
  u8 data[64];
};

struct mc_file_info_2 {
  u32 present;
  u32 pad1;
  u32 pad2;
  u8 data[64];
};

struct mc_slot_info {
  u32 handle;
  u32 known;
  u32 formatted;
  u32 initted;
  u32 last_file;
  u32 mem_required;
  u32 mem_actual;
  mc_file_info files[4];
};

struct mc_info {
  s32 p0;
  s32 handle;
  s32 inited;
  s32 mem_actual;
  s32 last_file;
  mc_file_info_2 files[4];
};

s32 new_mc_handle();
u32 mc_checksum(Ptr<u8> data, s32 size);
u32 handle_to_slot(s32 p1, s32 p2);
void MC_run();
void MC_set_language(s32 lang);
u64 MC_format(s32 param);
u64 MC_unformat(s32 param);
u64 MC_createfile(s32 param, Ptr<u8> data);
u64 MC_save(s32 param, s32 param2, Ptr<u8> data, Ptr<u8> data2);
u64 MC_load(s32 param, s32 param2, Ptr<u8> data);
void MC_makefile(s32 port, s32 size);
u32 MC_check_result();
void MC_get_status(s32 slot, Ptr<mc_slot_info> info);

#endif  // JAK_KMEMCARD_H
