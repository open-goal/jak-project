#pragma once

#include "common/common_types.h"

#include "game/overlord/common/isocommon.h"
#include "game/overlord/jak2/pages.h"
#include "game/sce/iop.h"

namespace jak2 {
void iso_init_globals();
u32 ISOThread();
extern u32 IsoThreadCounter;

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
  u8* unk_12;
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
  uint32_t (*sync_read)();                              // 20
  uint32_t (*load_sound_bank)(char*, SoundBank*);       // 24
  uint32_t (*load_music)(char*, s32*);
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
  iop::MsgPacket pkt;
  int cmd_kind;
  int status;
  int mbx_to_reply;
  int thread_id;
  int ready_for_data;                    // 24 (init to 1)
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
  int offset;
  u8* ptr;
  int unk_64;
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
  Finish_Dgo = 6,
  Finish_Obj_NoDoubleBuffer = 7,  // jak 2 only
};

struct CmdDgo {
  CmdHeader header;
  FileRecord* fr;      // 0x28, DGO file that's open
  u8* buffer1;         // 0x2c, first EE buffer
  u8* buffer2;         // 0x30, second EE buffer
  u8* buffer_heaptop;  // 0x34, top of the heap

  DgoHeader dgo_header;     // 0x38, current DGO's header
  ObjectHeader obj_header;  // 0x78, current obj's header

  u8* ee_dest_buffer;         // 0xb8, where we are currently loading to on ee
  u32 bytes_processed;        // 0xbc, how many bytes processed in the current state
  u32 objects_loaded;         // 0xc0, completed object count
  DgoState dgo_state;         // 0xc4, state machine state
  u32 finished_first_object;  // 0xc8, have we finished loading the first object?
  u32 buffer_toggle;          // 0xcc, which buffer to load into (top, buffer1, buffer2)
  u8* selected_buffer;        // 0xd0, most recently completed load destination
  u32 want_abort;             // 0xd4, should we quit?
};

struct CmdLoadSoundBank {
  CmdHeader header;
  char bank_name[16];
  SoundBank* bank;
};

struct CmdLoadMusic {
  CmdHeader header;
  char name[16];
  s32* handle;
};

struct VagDirEntry {
  char name[8];
  u32 offset;
  u32 flag;
};

struct VagCmd;

int NullCallback(CmdHeader* cmd, Buffer* buff);
u32 InitISOFS();
void IsoStopVagStream(VagCmd* param_1, int param_2);
void ProcessMessageData();
void IsoPlayVagStream(VagCmd* param_1, int param_2);
VagDirEntry* FindVAGFile(const char* name);
void IsoQueueVagStream(VagCmd* cmd, int param_2);

static constexpr int VAG_COUNT = 2728;
struct VagDir {
  u32 count;
  VagDirEntry vag[VAG_COUNT];
};
extern VagDir gVagDir;
extern s32 iso_mbx;

}  // namespace jak2
