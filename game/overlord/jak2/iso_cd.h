#pragma once

#include "common/common_types.h"

#include "game/overlord/common/isocommon.h"
#include "game/overlord/jak2/pages.h"

namespace jak2 {
void iso_cd_init_globals();

struct LargeBuffer {
  PageList* page_list;
  Page* allocated_pages;
  Page* page_before_midway_page;
  Page* mid_way_page;
  Page* unk_page_end2;
  Page* current_page;
  int unk3;
  int first_done_flag;
  // ??
  int done_flag_2;
  int maybe_post_first_done;
  int unk4;
  // ??
  int chunk_cnt_1;
  int chunk_cnt_2;
  u8* some_buffer;
  u8* current_buffer_base_ptr;
  u32 blzo_chunk_size;
  int blzo_buffer_size_bytes;
  u32 incoming_block_size;
  u8* ptr;      // pointer to data to be processed by DecompressBlock
  u8* end_ptr;  // end of data to be processed by DecompressBlock
  int init0_2;
};

struct LoadStackEntry {
  FileRecord* fr;
  int cd_offset;  // location in cd (sector). In OpenGOAL, it's just relative to the start of the
                  // file.
  int uses_blzo;
  int read_bytes;
  u32 size_after_decompression;
};

struct Buffer {
  u8* decomp_buffer;
  int decompressed_size;
  // ????
  PageList* plist;
  Page* page;
};

enum class OpenMode { MODE0, MODE1, KNOWN_NOT_BLZO };

}  // namespace jak2