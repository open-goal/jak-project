#pragma once

#include "common/common_types.h"
#include "game/common/overlord_common.h"

constexpr int STR_RPC_ID = 0xdeb5;
constexpr int STR_RPC_CHANNEL = 4;

struct RPC_Str_Cmd {
  u16 rsvd;       // 0, seems unused
  u16 result;     // 2, return code. see STR_RPC_RESULT_XXX
  u32 ee_addr;    // 4, GOAL address to load to.
  s32 chunk_id;   // 8, chunk ID for chunked file. Use -1 to load a non-chunked file, which gets the
                  // whole file.
  u32 length;     // 12, length that was actually loaded
  char name[64];  // file name
};

constexpr int STR_RPC_RESULT_ERROR = 1;
constexpr int STR_RPC_RESULT_DONE = 0;

// maximum number of chunks in a chunked file.
constexpr int SECTOR_TABLE_SIZE = 64;

// the header of a chunked file
struct StrFileHeader {
  u32 sectors[SECTOR_TABLE_SIZE];  // start of chunk, in sectors. including this sector.
  u32 sizes[SECTOR_TABLE_SIZE];    // size of chunk, in bytes. always an integer number of sectors
};

// the first sector of a chunked file.
struct StrFileHeaderSector : StrFileHeader {
  u32 pad[512 - 128];  // all zero
};

static_assert(sizeof(StrFileHeader) == 0x200, "Sector header size");
static_assert(sizeof(StrFileHeaderSector) == SECTOR_SIZE, "Sector header size");