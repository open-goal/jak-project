#pragma once

#include "common/common_types.h"

#include "game/overlord/common/isocommon.h"
#include "game/overlord/jak2/pages.h"

namespace jak2 {
void iso_init_globals();
u32 ISOThread();

struct LoadStackEntry {
  FileRecord* fr;
  int cd_offset;  // location in cd (sector). In OpenGOAL, it's just relative to the start of the
  // file.
  int uses_blzo;
  int read_bytes;
  u32 size_after_decompression;
};

enum class OpenMode { MODE0, MODE1, KNOWN_NOT_BLZO };

struct Buffer {
  u8* decomp_buffer;
  int decompressed_size;
  Buffer* next;
  int unk_12;
  int data_buffer_idx;
  int use_mode;
  PageList* plist;
  int num_pages;
  int unk_32;
  int free_pages;
  Page* page;
  int unk_44;
};

struct IsoFs {
  int (*init)();                                        // 0
  FileRecord* (*find)(const char*);                     // 4
  FileRecord* (*find_in)(const char*);                  // 8
  uint32_t (*get_length)(FileRecord*);                  // c
  LoadStackEntry* (*open)(FileRecord*, int, OpenMode);  // 10
  LoadStackEntry* (*open_wad)(FileRecord*, int32_t);    // 14
  void (*close)(LoadStackEntry*);                       // 18
  int (*page_begin_read)(LoadStackEntry*, Buffer*);     // 1c
  // uint32_t (*sync_read)();                                // 20
  uint32_t (*load_sound_bank)(char*, SoundBank*);  // 24
  uint32_t (*load_music)(char*, u32*);
  // void (*poll_drive)();
};

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

extern IsoFs* isofs;
extern LargeBuffer* SpLargeBuffer;

struct CmdHeader {
  int cmd_kind;
  int status;
  int mbx_to_reply;
  int thread_id;
  int unk_24;                            // 24 (init to 1)
  Buffer* callback_buffer;               // 28
  int (*callback)(CmdHeader*, Buffer*);  // 32
  LoadStackEntry* lse;                   // 36
};

struct CmdLoadSingleIop {
  CmdHeader header;
  FileRecord* file_record;
  u8* dest_addr;
  int length;
  int length_to_copy;
};

int NullCallback(CmdHeader* cmd, Buffer* buff);

}  // namespace jak2