#pragma once

#include "common/common_types.h"
#include "common/versions/versions.h"

#include "game/common/overlord_common.h"

// TODO: jak 3 stub
constexpr PerGameVersion<int> STR_RPC_ID(0xdeb5, 0xfab4, 0x0);
constexpr int STR_RPC_CHANNEL = 4;

/*
(deftype load-chunk-msg (structure)
  ((rsvd     uint16     :offset-assert 0)
   (result   load-msg-result     :offset-assert 2)
   (address  pointer    :offset-assert 4)
   (section  uint32     :offset-assert 8)
   (maxlen   uint32     :offset-assert 12)
   (id       uint32     :offset 4)
   (basename uint8 48 :offset-assert 16)
   )
  :method-count-assert 9
  :size-assert         #x40
  :flag-assert         #x900000040
  )
*/
struct RPC_Str_Cmd_Jak1 {
  u16 rsvd;       // 0, seems unused
  u16 result;     // 2, return code. see STR_RPC_RESULT_XXX
  u32 ee_addr;    // 4, GOAL address to load to.
  s32 chunk_id;   // 8, chunk ID for chunked file. Use -1 to load a non-chunked file, which gets the
                  // whole file.
  u32 length;     // 12, length that was actually loaded
  char name[64];  // file name
};

/*
(deftype load-chunk-msg (structure)
  ((rsvd     uint16                    :offset-assert   0)
   (result   load-msg-result           :offset-assert   2)
   (address  pointer                   :offset-assert   4)
   (section  uint32                    :offset-assert   8)
   (maxlen   uint32                    :offset-assert  12)
   (dummy    uint32            4       :offset-assert  16)
   (basename sound-stream-name :inline :offset-assert  32)
   )
  :method-count-assert 9
  :size-assert         #x50
  :flag-assert         #x900000050
  )
*/
struct RPC_Str_Cmd_Jak2 {
  u16 rsvd;
  u16 result;  // 2
  u32 address;
  s32 section;  // 8
  u32 maxlen;
  u32 dummy[4];
  char basename[48];  // 32
};

struct RPC_Play_Cmd_Jak1 {
  u16 rsvd;
  u16 result;
  u32 address;
  u32 section;
  u32 maxlen;
  char name[48];
};

struct SoundStreamName {
  char chars[48];
};

struct RPC_Play_Cmd_Jak2 {
  u16 rsvd;
  u16 result;
  u32 address;
  u32 section;
  u32 maxlen;
  u32 id[4];
  SoundStreamName names[4];
  u32 pad[8];
};
static_assert(sizeof(RPC_Play_Cmd_Jak2) == 256);

/*
 *
(deftype play-chunk-msg (structure)
  ((rsvd     uint16                      :offset-assert   0)
   (result   uint16                      :offset-assert   2)
   (address  pointer                     :offset-assert   4)
   (section  uint32                      :offset-assert   8)
   (maxlen   uint32                      :offset-assert  12)
   (id       uint32            4         :offset-assert  16)
   (basename sound-stream-name 4 :inline :offset-assert  32)
   )
  :method-count-assert 9
  :size-assert         #xe0
  :flag-assert         #x9000000e0
  )
 */

constexpr int STR_RPC_RESULT_ERROR = 1;
constexpr int STR_RPC_RESULT_DONE = 0;

// maximum number of chunks in a chunked file.
constexpr int SECTOR_TABLE_SIZE = 64;

// the header of a chunked file
struct StrFileHeaderJ1 {
  u32 sectors[SECTOR_TABLE_SIZE];  // start of chunk, in sectors. including this sector.
  u32 sizes[SECTOR_TABLE_SIZE];    // size of chunk, in bytes. always an integer number of sectors
};

// the first sector of a chunked file.
struct StrFileHeaderSector : StrFileHeaderJ1 {
  u32 pad[512 - 128];  // all zero
};

static_assert(sizeof(StrFileHeaderJ1) == 0x200, "Sector header size");
static_assert(sizeof(StrFileHeaderSector) == SECTOR_SIZE, "Sector header size");

constexpr int SECTOR_TABLE_SIZE_J2 = 512;

struct StrFileHeaderJ2 {
  u32 sectors[SECTOR_TABLE_SIZE_J2];  // start of chunk, in sectors. including this sector.
  u32 sizes[SECTOR_TABLE_SIZE_J2];  // size of chunk, in bytes. always an integer number of sectors
};

static_assert(sizeof(StrFileHeaderJ2) == 0x1000, "Sector header size");
