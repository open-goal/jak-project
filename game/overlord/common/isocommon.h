#pragma once

#include "common/common_types.h"
#include "common/link_types.h"

// was 350 in jak 1, 641 in jak 2, no reason not to go higher
constexpr int MAX_ISO_FILES = 999;  // maximum files on FS
// was 16 in jak 1, 32 in jak 2. no reason not to increase for jak 1.
constexpr int MAX_OPEN_FILES = 32;  // maximum number of open files at a time.

constexpr u32 CMD_STATUS_READ_ERR = 8;              // read encountered a problem or was canceled.
constexpr u32 CMD_STATUS_NULL_CB = 7;               // status returned if you don't set a callback
constexpr u32 CMD_STATUS_FAILED_TO_OPEN = 6;        // status if file couldn't be opened
constexpr u32 CMD_STATUS_FAILED_TO_QUEUE = 2;       // status if we couldn't be queued
constexpr u32 CMD_STATUS_IN_PROGRESS = 0xffffffff;  // status if command is running and healthy
constexpr u32 CMD_STATUS_DONE = 0;                  // status if command is done.

constexpr int LOAD_TO_EE_CMD_ID = 0x100;         // command to load file to ee
constexpr int LOAD_TO_IOP_CMD_ID = 0x101;        // command to load to iop
constexpr int LOAD_TO_EE_OFFSET_CMD_ID = 0x102;  // command to load file to ee with offset.

constexpr int LOAD_DGO_CMD_ID = 0x200;  // command to load DGO

struct SoundBank;

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

void MakeISOName(char* dst, const char* src);
