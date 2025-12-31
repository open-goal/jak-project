#pragma once

#include "common/common_types.h"

#include "game/kernel/common/Ptr.h"
#include "game/kernel/common/kmalloc.h"

constexpr char FOLDER_PREFIX[] = "";

void fileio_init_globals();
char* strend(char* str);
void kstrcpy(char* dst, const char* src);
char* basename_goal(char* s);
void kstrcpyup(char* dst, const char* src);
void kstrcat(char* dest, const char* src);
char* kstrinsert(char* str, char pad, s32 count);
void kstrncat(char* dest, const char* src, s32 count);
Ptr<u8> FileLoad(char* name, Ptr<kheapinfo> heap, Ptr<u8> memory, u32 malloc_flags, s32* size_out);

extern char buffer_633[512];

// GOAL File Types
enum GoalFileType {
  LISTENER_TO_KERNEL_FILE_TYPE = 1,
  KERNEL_TO_LISTENER_FILE_TYPE = 2,
  CODE_FILE_TYPE = 3,
  GAMEPAD_FILE_TYPE = 4,
  LISTENER_TO_KERNEL_LOCK_FILE_TYPE = 5,
  KERNEL_TO_LISTENER_LOCK_FILE_TYPE = 6,
  IOP_MODULE_FILE_TYPE = 8,
  DATA_FILE_TYPE = 0x20,  // called FINAL in jak2
  TX_PAGE_FILE_TYPE = 0x21,
  JA_FILE_TYPE = 0x22,
  JG_FILE_TYPE = 0x23,
  MA_FILE_TYPE = 0x24,
  MG_FILE_TYPE = 0x25,
  TG_FILE_TYPE = 0x26,
  LEVEL_FILE_TYPE = 0x27,
  ART_GROUP_FILE_TYPE = 0x30,
  VS_FILE_TYPE = 0x31,
  TX_FILE_TYPE = 0x32,
  VS_BIN_FILE_TYPE = 0x33,
  DGO_TXT_FILE_TYPE = 0x34,
  LEVEL_WITH_EXTENSION_FILE_TYPE = 0x35,
  DATA_DGO_FILE_TYPE = 0x36,
  GAME_DGO_FILE_TYPE = 0x37,
  DATA_CGO_FILE_TYPE = 0x38,
  GAME_CGO_FILE_TYPE = 0x39,
  CNT_FILE_TYPE = 0x3a,
  RES_FILE_TYPE = 0x3b,
  SND_BNK_FILE_TYPE = 0x3c,
  MUSIC_BNK_FILE_TYPE = 0x3d,
  VAG_FILE_TYPE = 0x3e,
  MISC_FILE_TYPE = 0x3f,  // jak2 only
  MAP_FILE_TYPE = 0x40,   // jak2 only
  CL_FILE_TYPE = 0x41,    // jak 3 cloth animation
  REFPLANT_FILE_TYPE = 0x301,
  // added this, allows access directly to out/iso from fileio.
  ISO_FILE_TYPE = 0x302
};