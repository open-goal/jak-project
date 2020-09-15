#pragma once

/*!
 * @file fileio.h
 * GOAL Low-Level File I/O and String Utilities
 */

#ifndef RUNTIME_FILEIO_H
#define RUNTIME_FILEIO_H

#include "common/common_types.h"
#include "Ptr.h"
#include "kmalloc.h"

// GOAL File Types
enum GoalFileType {
  LISTENER_TO_KERNEL_FILE_TYPE = 1,
  KERNEL_TO_LISTENER_FILE_TYPE = 2,
  CODE_FILE_TYPE = 3,
  GAMEPAD_FILE_TYPE = 4,
  LISTENER_TO_KERNEL_LOCK_FILE_TYPE = 5,
  KERNEL_TO_LISTENER_LOCK_FILE_TYPE = 6,
  IOP_MODULE_FILE_TYPE = 8,
  DATA_FILE_TYPE = 0x20,
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
  REFPLANT_FILE_TYPE = 0x301,
};

constexpr char FOLDER_PREFIX[] = "";

constexpr u32 ART_FILE_VERSION = 6;
constexpr u32 LEVEL_FILE_VERSION = 30;
constexpr u32 DGO_FILE_VERSION = 1;
constexpr u32 RES_FILE_VERSION = 1;
constexpr u32 TX_PAGE_VERSION = 7;

char* strend(char* str);
u32 ReadHufWord(u8** loc_ptr);
void kstrcpy(char* dst, const char* src);
void kstrcpyup(char* dst, const char* src);
void kstrcat(char* dest, const char* src);
void kstrncat(char* dest, const char* src, s32 count);
char* kstrinsert(char* str, char pad, s32 count);
char* basename_goal(char* s);
char* DecodeFileName(const char* name);
char* MakeFileName(int type, const char* name, int new_string);
u32 FileExists(const char* name);
void FileDelete(const char* name);
void FileCopy(const char* a, const char* b);
s32 FileLength(char* filename);
Ptr<u8> FileLoad(char* name, Ptr<kheapinfo> heap, Ptr<u8> memory, u32 malloc_flags, s32* size_out);
s32 FileSave(char* name, u8* data, s32 size);
void fileio_init_globals();

#endif  // RUNTIME_FILEIO_H
