#include "iso_cd.h"

#include <cstdio>
#include <cstring>

#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"

#include "game/common/overlord_common.h"
#include "game/overlord/common/fake_iso.h"
#include "game/overlord/common/isocommon.h"
#include "game/overlord/common/sbank.h"
#include "game/overlord/jak2/iso_queue.h"
#include "game/sce/iop.h"
#include "game/sound/sndshim.h"

using namespace iop;

namespace jak2 {

/// Page being written by CD reads
Page* ReadPagesCurrentPage = nullptr;

/// Semaphore that blocks on a CD read
s32 DvdSema = -1;

s32 BlzoSema = -1;

/// Callback function installed before using the iso_cd.cpp system
void (*PreviousCallBack)(int) = nullptr;

/// Flag to indicate that we should stop reading.
s32 ReadPagesCancelRead = 0;

/// The number of pages to read for the current read.
s32 ReadPagesNumToRead = 0;

/// The actual buffer in IOP memory to write to for the current read.
void* ReadPagesCurrentBuffer = nullptr;

/// The sector of the read in progress
s32 ReadPagesCurrentSector = 0;

/// How many sectors should be done per page (and call to sceCdRead)
s32 ReadPagesSectorsPerPage = 0;

PageList* ReadPagesPagePool = nullptr;

s32 SubBufferToRead = 0;
// -1 is set after some stuff.
// 5 or 6 needed for IsoCdPagesCallback to fire
// 5 for non-blzo read
// 6 for blzo read
// 9 is backed up by insta-reads (it happens during CD_WaitReturn)
// 8 for insta-reads itself

/// Flag that can be set to 1 once reading pages is done.
s32* ReadPagesDoneFlag = nullptr;

/// LoadStackEntry for currently reading file
static LoadStackEntry* sReadInfo;

s32 StopPluginStreams = 0;

s32 SubBuffersFlags = 0;

void* sMode = nullptr;

static LoadStackEntry sLoadStack[MAX_OPEN_FILES];  //! List of all files that are "open"

IsoFs iso_cd;

int FS_Init();
LoadStackEntry* FS_Open(FileRecord* file_record, int offset, OpenMode mode);
LoadStackEntry* FS_OpenWad(FileRecord* fr, int offset);
void FS_Close(LoadStackEntry* lse);
int FS_PageBeginRead(LoadStackEntry* lse, Buffer* buffer);
uint32_t FS_LoadSoundBank(char* name, SoundBank* buffer);
uint32_t FS_LoadMusic(char* name, snd::BankHandle* buffer);
u32 FS_SyncRead();

struct FakeCd {
  int offset_into_file = 0;
  FILE* fp = nullptr;
  void (*callback)(int) = nullptr;
  FileRecord* last_fr = nullptr;
} gFakeCd;

void iso_cd_init_globals() {
  ReadPagesCurrentPage = nullptr;
  DvdSema = -1;
  BlzoSema = -1;
  PreviousCallBack = nullptr;
  ReadPagesCancelRead = 0;
  SubBufferToRead = 0;
  ReadPagesNumToRead = 0;
  ReadPagesCurrentBuffer = nullptr;
  ReadPagesCurrentSector = 0;
  ReadPagesSectorsPerPage = 0;
  ReadPagesDoneFlag = nullptr;
  ReadPagesPagePool = nullptr;
  SpLargeBuffer = nullptr;
  SubBuffersFlags = 0;
  StopPluginStreams = 0;
  memset(sLoadStack, 0, sizeof(sLoadStack));
  sReadInfo = nullptr;

  iso_cd.init = FS_Init;
  iso_cd.find = FS_Find;
  iso_cd.find_in = FS_FindIN;
  iso_cd.get_length = FS_GetLength;
  iso_cd.open = FS_Open;
  iso_cd.open_wad = FS_OpenWad;
  iso_cd.close = FS_Close;
  iso_cd.page_begin_read = FS_PageBeginRead;
  iso_cd.load_sound_bank = FS_LoadSoundBank;
  iso_cd.load_music = FS_LoadMusic;
  iso_cd.sync_read = FS_SyncRead;
  gFakeCd.last_fr = nullptr;
}

static FILE* open_fr(FileRecord* fr, s32 thread_to_wake) {
  const char* path = get_file_path(fr);
  FILE* fp = file_util::open_file(path, "rb");
  iop::iWakeupThread(thread_to_wake);

  return fp;
}

///////////////////////////
// Sony Fake CD Functions
///////////////////////////

auto sceCdCallback(void (*callback)(int)) {
  auto ret = gFakeCd.callback;
  gFakeCd.callback = callback;
  return ret;
}

int sceCdRead(int lsn, int num_sectors, void* dest, void* mode) {
  (void)mode;
  auto do_read = [lsn, num_sectors, dest](s32 thid) {
    // printf("sceCdRead %d, %d -> %p\n", lsn, num_sectors, dest);
    ASSERT(gFakeCd.fp);
    if (fseek(gFakeCd.fp, lsn * SECTOR_SIZE, SEEK_SET)) {
      ASSERT_MSG(false, "Failed to fseek");
    }
    if (fread(dest, num_sectors * SECTOR_SIZE, 1, gFakeCd.fp) < 0) {
      printf("dest is %p, num_sectors %d, lsn %d\n", dest, num_sectors, lsn);
      printf("err: %s\n", strerror(errno));
      ASSERT_MSG(false, "Failed to fread");
    }
    ASSERT(gFakeCd.callback);

    iWakeupThread(thid);
  };

  auto future = thpool.submit(do_read, GetThreadId());
  SleepThread();
  future.get();

  return 1;
}

void do_cd_callback() {
  if (gFakeCd.callback) {
    gFakeCd.callback(1);
  }
}

////////////////////////
// Overlord Functions
////////////////////////

/*!
 * Handle a completed read. Called by Sony CD system.
 */
void IsoCdPagesCallback(int done) {
  if (!ReadPagesCurrentPage) {
    // somehow there is nothing left to read. I guess the read is no longer desired.
    printf("-- ReadPagesCurrentPage was mysteriously null in IsoCdPagesCallback\n");

    // allow stuff waiting on the dvd semaphore
    SignalSema(DvdSema);  // was iSignalSema

    // restore old CD callback
    sceCdCallback(PreviousCallBack);
  } else if (done == 1) {
    if (ReadPagesCancelRead == 0) {
      // read is still in progress.

      // ???
      if ((SubBufferToRead != 5) && (SubBufferToRead != 6)) {
        // not a FS_BeginPagedRead read, so don't use this callback. seems bad if this is reached.
        return;
      }

      // ???
      if (ReadPagesCurrentPage->state == PageState::ALLOCATED_EMPTY) {
        ReadPagesCurrentPage->state = PageState::ALLOCATED_FILLED;
      }

      // advance to the next page.
      ReadPagesCurrentPage = ReadPagesCurrentPage->next;
      ReadPagesNumToRead = ReadPagesNumToRead + -1;

      // kick off next read if we can.
      if (ReadPagesCurrentPage && ReadPagesNumToRead > 0) {
        ReadPagesCurrentBuffer = ReadPagesCurrentPage->buffer;
        ReadPagesCurrentSector = ReadPagesCurrentSector + ReadPagesSectorsPerPage;

        // read!
        int cd_ret = sceCdRead(ReadPagesCurrentSector, ReadPagesSectorsPerPage,
                               ReadPagesCurrentBuffer, sMode);
        if (cd_ret != 0) {
          return;
        }

        // no need for CdReturnThread - just checks for removed CD.
        // iWakeupThread(CdReturnThread);
        return;
      }

      // otherwise, we cannot read. Hopefully we read everything the user requested
      // added this check here
      if (ReadPagesNumToRead) {
        printf("---- IsoCdPagesCallback wants to keep reading, but ran out of pages!\n");
        ASSERT_NOT_REACHED();
      }

      ReadPagesPagePool = 0;
      ReadPagesCurrentPage = nullptr;
      ReadPagesCurrentSector = 0;
      if (ReadPagesDoneFlag == nullptr) {
        ReadPagesDoneFlag = nullptr;
      } else {
        *ReadPagesDoneFlag = 1;
      }
    } else {  // we were cancelled.
      ReadPagesPagePool = 0;
      ReadPagesCurrentPage = nullptr;
      ReadPagesCurrentSector = 0;
      ReadPagesDoneFlag = nullptr;
      ReadPagesCancelRead = 0;
    }

    // read is done.
    SubBufferToRead = -1;
    sceCdCallback(PreviousCallBack);
    SignalSema(DvdSema);  // was iSignalSema.
  }
}

// ReadDirectory : skipped
// DecodeDUP : skipped
// LoadMusicTweaks : can use common
// LoadDiscID : skipped

/*!
 * Initialize the iso_cd filesystem.
 */
int FS_Init() {
  // added: initialize fake_iso. This will allow us to call fake_iso functions in the implementation
  // of jak 2 CD reading code.
  // this will also take care of loading music tweaks
  fake_iso_FS_Init();

  // removed checks related to checking DVD/CD type.

  SemaParam sema_param;
  sema_param.attr = 0;
  sema_param.init_count = 1;
  sema_param.max_count = 1;
  sema_param.option = 0;
  DvdSema = CreateSema(&sema_param);
  if (DvdSema < 0) {
    printf("IOP: ======================================================================\n");
    printf("IOP: iso_cd FS_Init: Can\'t create DVD semaphore\n");
    printf("IOP: ======================================================================\n");
    ASSERT_NOT_REACHED();
  }

  // removed creation of CD Return thread.
  // this only checks for the CD being removed and put back in.

  sema_param.attr = 1;
  sema_param.init_count = 1;
  sema_param.max_count = 1;
  sema_param.option = 0;
  BlzoSema = CreateSema(&sema_param);
  if (BlzoSema < 0) {
    printf("IOP: ======================================================================\n");
    printf("IOP: iso_cd FS_Init: Can\'t create BLZO semaphore\n");
    printf("IOP: ======================================================================\n");
    ASSERT_NOT_REACHED();
  }

  // init the LargeBuffer.
  SubBufferToRead = -1;
  SubBuffersFlags = 0;
  SpLargeBuffer->chunk_cnt_1 = 8;
  SpLargeBuffer->chunk_cnt_2 = 8;
  SpLargeBuffer->blzo_buffer_size_bytes = 0x40000;
  SpLargeBuffer->current_buffer_base_ptr = nullptr;
  SpLargeBuffer->page_list = SpMemoryBuffers;
  SpLargeBuffer->blzo_chunk_size = 0x8000;
  SpLargeBuffer->incoming_block_size = 0;
  SpLargeBuffer->ptr = nullptr;
  SpLargeBuffer->init0_2 = 0;
  SpLargeBuffer->end_ptr = (SpLargeBuffer->current_buffer_base_ptr + 0x40000);  // ?

  return 0;
}

LoadStackEntry* FS_Open(FileRecord* file_record, int offset, OpenMode mode) {
  // CpuSuspendIntr();
  // first, find a LoadStackEntry*
  LoadStackEntry* load_stack_entry = nullptr;
  for (auto& entry : sLoadStack) {
    if (!entry.fr) {
      load_stack_entry = &entry;
      load_stack_entry->fr = file_record;
      break;
    }
  }
  // CpuResumeIntr(local_28[0]);

  if (!load_stack_entry) {
    printf("IOP: ======================================================================\n");
    printf("IOP: iso_cd FS_Open: stack full\n");
    printf("IOP: ======================================================================\n");
    ASSERT_NOT_REACHED();
  }

  u32 first_four_bytes_of_file = 0;
  u32 next_four_bytes_of_file = 0;

  switch (mode) {
    case OpenMode::MODE0:
    case OpenMode::MODE1:
      // we'd read 1 sector now to check the header. this would populate first_four_bytes_of_file.
      // in OpenGOAL we don't use BLZO, so just put
      first_four_bytes_of_file = 0;
      break;
    case OpenMode::KNOWN_NOT_BLZO:
      // like it was in the original
      first_four_bytes_of_file = 0;
      break;
    default:
      ASSERT_NOT_REACHED();
  }

  if (first_four_bytes_of_file == 0x426c5a6f) {  // blzo magic.
    load_stack_entry->uses_blzo = 1;
    if (PollSema(BlzoSema) != 0) {
      load_stack_entry->size_after_decompression = 0;
      file_record->size = 0;
      printf("======================================================================\n");
      // printf("IOP: iso_cd FS_Open: blzo file %s cant get blzo semaphore\n", file_record);
      printf("======================================================================\n");
      ASSERT_NOT_REACHED();
    }
    // remember the size
    load_stack_entry->size_after_decompression = next_four_bytes_of_file;
    // also modify the file record!!
    file_record->size = next_four_bytes_of_file;

    // allocate a list of pages to hold the data.
    Page* blzo_pages =
        AllocPagesBytes(SpLargeBuffer->page_list, SpLargeBuffer->blzo_buffer_size_bytes);
    SpLargeBuffer->allocated_pages = blzo_pages;

    auto page_size = SpLargeBuffer->page_list->page_size;

    // make sure we have enough pages.
    if ((u32)blzo_pages->free_pages <
        ((SpLargeBuffer->blzo_buffer_size_bytes + page_size) - 1) / page_size) {
      printf("IOP: ======================================================================\n");
      page_size = SpLargeBuffer->page_list->page_size;
      printf("IOP: iso_cd FS_Open: need %d pages got %d\n",
             ((SpLargeBuffer->blzo_buffer_size_bytes + page_size) - 1) / page_size,
             SpLargeBuffer->allocated_pages->free_pages);
      printf("IOP: ======================================================================\n");

      if (0 < SpLargeBuffer->allocated_pages->free_pages) {
        FreePagesList(SpLargeBuffer->page_list, SpLargeBuffer->allocated_pages);
      }
      StopPluginStreams = 1;
      SignalSema(BlzoSema);
      load_stack_entry->size_after_decompression = 0;
      load_stack_entry->fr = nullptr;
      return nullptr;
    }
    auto* page_iter = SpLargeBuffer->allocated_pages;
    SpLargeBuffer->current_page = blzo_pages;
    SpLargeBuffer->mid_way_page = page_iter->end_page_first_only;
    SpLargeBuffer->unk_page_end2 = page_iter->end_page_first_only;

    // seek page-iter to the midway page.
    int midway_page_idx = page_iter->free_pages / 2;
    int i = 0;
    if (0 < midway_page_idx) {
      do {
        page_iter = page_iter->next;
        i = i + 1;
      } while (i < midway_page_idx);
    }
    SpLargeBuffer->mid_way_page = page_iter;

    blzo_pages = page_iter->prev;
    SpLargeBuffer->first_done_flag = 0;
    SpLargeBuffer->done_flag_2 = 0;
    SpLargeBuffer->unk3 = 0;
    SpLargeBuffer->maybe_post_first_done = 0;
    SpLargeBuffer->unk4 = 0;
    SpLargeBuffer->ptr = 0;
    SpLargeBuffer->page_before_midway_page = blzo_pages;
    SpLargeBuffer->some_buffer = nullptr;
    SpLargeBuffer->current_buffer_base_ptr = SpLargeBuffer->allocated_pages->buffer;
  } else {
    load_stack_entry->uses_blzo = 0;
    // load_stack_entry->size_after_decompression = file_record->size;
    load_stack_entry->size_after_decompression = FS_GetLength(file_record);
  }
  load_stack_entry->read_bytes = 0;
  // load_stack_entry->cd_offset = file_record->location;
  load_stack_entry->cd_offset = 0;
  if (offset != -1) {
    load_stack_entry->cd_offset += offset;
  }
  return load_stack_entry;
}

int DecompressBlock(u8* /*input*/, u8* output) {
  int size_out = -1;
  int decomp_return_code = -1;

  // Note: in here SpLargeBuffer's ptr points to the start of the new data
  // and Page's ptr points to the end.

  // see if we should use decompress or not.
  // in some cases, compressing the data makes it bigger.
  // in this case, the data on DVD contains a CHUNK_SIZE block of uncompressed data
  // and the _incoming_block_size is set to the larger-than-CHUNK_SIZE size of a compressed block.
  if (SpLargeBuffer->blzo_chunk_size < SpLargeBuffer->incoming_block_size) {
    // the case where compressing doesn't help. Copy data from the current page to output.
    FromPagesCopy(SpLargeBuffer->current_page, SpLargeBuffer->ptr, output,
                  SpLargeBuffer->blzo_chunk_size);
    // output size is exactly max size
    size_out = SpLargeBuffer->blzo_chunk_size;
    // no error possible in this case.
    decomp_return_code = 0;
    // adjust to the actual size of data processed.
    SpLargeBuffer->incoming_block_size = size_out;
  } else {
    // otherwise actually decompress.
    // this reads incoming_block_size data, writes decompressed to output, and stores the number
    // of bytes output in size_out.
    ASSERT_NOT_REACHED();
    // decomp_return_code = lzo1x_decompress(input, SpLargeBuffer->incoming_block_size, output,
    // &size_out, 0);
  }

  // align to 4 bytes so our next read is still 4 byte aligned.
  auto aligned_incoming_size = SpLargeBuffer->incoming_block_size + 3U & 0xfffffffc;
  SpLargeBuffer->incoming_block_size = aligned_incoming_size;

  // seek the pointer in SpLargeBuffer simply by adding the size for now...
  // this might run on the page...
  auto* end_of_incoming = SpLargeBuffer->ptr + aligned_incoming_size;
  SpLargeBuffer->ptr = end_of_incoming;

  // now check to see if we need to advance the page
  Page* page = SpLargeBuffer->current_page;
  auto* page_ptr = page->ptr;

  if (page_ptr < end_of_incoming) {  // did our pointer seek go past the end of this page?
    // if so, seek the page!
    page = page->next;
    if (page) {
      SpLargeBuffer->current_page = page;
      // carry over... not sure what the -1 is.
      SpLargeBuffer->ptr = end_of_incoming - page_ptr + page->buffer - 1;
    }
  }

  // check to see if there is additional data read, after this block.
  if (SpLargeBuffer->ptr < SpLargeBuffer->end_ptr) {
    // there is!
    // we can determine the next block's size!

    // read it
    memcpy(&SpLargeBuffer->incoming_block_size, SpLargeBuffer->ptr, 4);
    end_of_incoming = SpLargeBuffer->ptr + 4;
    SpLargeBuffer->ptr = end_of_incoming;
    page_ptr = SpLargeBuffer->current_page->ptr;

    // check for crossing page boundary by reading this...
    if (page_ptr < end_of_incoming) {
      page = SpLargeBuffer->current_page->next;
      if (page) {
        SpLargeBuffer->current_page = page;
        SpLargeBuffer->ptr = end_of_incoming - page_ptr + page->buffer - 1;
      }
    }

    // check for insane value
    if (70000 < SpLargeBuffer->incoming_block_size) {
      printf("IOP: ======================================================================\n");
      printf("IOP: iso_cd DecompressBlock: next compressed block length too big page %d\n",
             SpLargeBuffer->current_page->maybe_page_id);
      printf("IOP: ======================================================================\n");
      ASSERT_NOT_REACHED();
    }
  } else {
    // no data read. I guess we set incoming block size to 0, for now.
    SpLargeBuffer->incoming_block_size = 0;
  }
  if (decomp_return_code != 0) {
    ASSERT_NOT_REACHED();
  }
  return size_out;
}

int FS_PageBeginRead(LoadStackEntry* lse, Buffer* buffer) {
  //  bool bVar1;
  //  undefined4 uVar3;
  //  int iVar4;
  //  int *piVar5;
  //  int iVar6;
  //  Page *pPVar7;
  //  uint8_t *puVar8;
  //  int iVar9;
  //  uint uVar10;
  //  uint uVar11;
  //  PageList *plist;
  //  Page *pages;
  //  undefined4 local_40;
  //  undefined4 local_3c;
  //  undefined4 local_38;
  //  int local_34;
  //  undefined4 local_30 [2];

  // _buffer = buffer->decomp_buffer;
  int sector = lse->cd_offset;
  // _sectors = 0;
  sReadInfo = lse;
  // _sector = uVar10;
  // _real_sector = uVar10;
  if (lse->uses_blzo == 1) {
    ASSERT_NOT_REACHED_MSG("no blzo for u");
  } else {
    if ((lse->size_after_decompression != 0) &&
        (lse->size_after_decompression <= (u32)lse->read_bytes)) {
      // _sectors = 0;
      // read past the end already.
      // not entirely sure why this is "in progress"...
      return CMD_STATUS_IN_PROGRESS;
    }

    // these pages will hold the data.
    auto* plist = buffer->plist;
    auto* pages = buffer->page;

    // wait for drive
    if (PollSema(DvdSema) == KE_SEMA_ZERO) {
      WaitSema(DvdSema);
    }

    // wait for pending read to finish, I guess...
    if (-1 < SubBufferToRead) {
      while (-1 < SubBufferToRead) {
        DelayThread(100);
      }
    }

    // set SubBufferToRead to 5, to indicate a non-blzo FS read.
    // iVar4 = CpuSuspendIntr(local_30);
    SubBufferToRead = 5;
    /*
    if (iVar4 != -0x66) {
      CpuResumeIntr(local_30[0]);
    }
     */
    int page_size = plist->page_size;
    if (page_size < 0) {  // usual page size mystery.
      page_size = page_size + 0x7ff;
    }

    // init globals for the callback
    ReadPagesCurrentBuffer = (void*)pages->buffer;
    ReadPagesSectorsPerPage = page_size >> 0xb;
    ReadPagesCancelRead = 0;
    ReadPagesPagePool = plist;
    ReadPagesCurrentPage = pages;
    ReadPagesCurrentSector = sector;
    int local_34 = 0;  // super sketchy...
    ReadPagesDoneFlag = &local_34;
    int npages = pages->free_pages;
    ReadPagesNumToRead = npages;

    // mark all as allocated and empty.
    pages->state = PageState::ALLOCATED_EMPTY;
    auto* next = pages->next;
    while ((next && (npages = npages + -1, 0 < npages))) {
      pages = pages->next;
      next = pages->next;
      pages->state = PageState::ALLOCATED_EMPTY;
    }

    // start a read!
    if (gFakeCd.last_fr != lse->fr) {
      auto future = thpool.submit(open_fr, lse->fr, GetThreadId());
      SleepThread();
      FILE* fp = future.get();
      if (!fp) {
        lg::error("[OVERLORD] fake iso could not open the file \"{}\"", get_file_path(lse->fr));
      } else {
        // printf("PAGE READING %s\n", path);
      }

      ASSERT(fp);

      if (gFakeCd.fp) {
        fclose(gFakeCd.fp);
      }
      gFakeCd.fp = fp;
      gFakeCd.last_fr = lse->fr;
    }

    while (true) {
      PreviousCallBack = nullptr;
      PreviousCallBack = sceCdCallback(IsoCdPagesCallback);
      int ret =
          sceCdRead(ReadPagesCurrentSector, ReadPagesSectorsPerPage, ReadPagesCurrentBuffer, sMode);
      if (ret)
        break;
      sceCdCallback(PreviousCallBack);
      // FS_PollDrive();
    }

    // wait for read to finish
    while (-1 < SubBufferToRead) {
      // DelayThread(1000);
      do_cd_callback();  // added, to make progress. TODO remove sleep avoe.
    }

    // update stats.
    int bytes_read = buffer->page->free_pages * buffer->plist->page_size;
    lse->read_bytes = lse->read_bytes + bytes_read;
    buffer->decompressed_size = bytes_read;
    lse->cd_offset += bytes_read >> 0xb;
  }

  // not sure why we use "in progress" here, maybe the meaning changed.
  return CMD_STATUS_IN_PROGRESS;
}

uint32_t FS_LoadSoundBank(char* name, SoundBank* buffer) {
  SoundBank* bank = (SoundBank*)buffer;
  FileRecord* file = nullptr;
  char namebuf[16];
  char isoname[16];
  snd::BankHandle handle;

  strncpy(namebuf, name, 12);
  namebuf[8] = 0;
  strcat(namebuf, ".sbk");

  MakeISOName(isoname, namebuf);
  file = FS_FindIN(isoname);
  if (!file) {
    return CMD_STATUS_FAILED_TO_OPEN;
  }

  handle = snd_BankLoadEx(get_file_path(file), 0, bank->spu_loc, bank->spu_size);
  snd_ResolveBankXREFS();
  bank->bank_handle = handle;

  return CMD_STATUS_DONE;
}

uint32_t FS_LoadMusic(char* name, snd::BankHandle* bank_handle) {
  FileRecord* file = nullptr;
  char namebuf[16];
  char isoname[16];
  snd::BankHandle handle;

  strncpy(namebuf, name, 12);
  namebuf[8] = 0;
  strcat(namebuf, ".mus");

  MakeISOName(isoname, namebuf);

  file = FS_FindIN(isoname);
  if (!file) {
    return CMD_STATUS_FAILED_TO_OPEN;
  }

  handle = snd_BankLoadEx(get_file_path(file), 0, 0xcfcc0, 0x61a80);
  snd_ResolveBankXREFS();
  *bank_handle = handle;

  return CMD_STATUS_DONE;
}

// CD_WaitReturn: don't need it
// FS_Find: common
// FS_FindIN: common
// FS_GetLength: common

LoadStackEntry* FS_OpenWad(FileRecord* fr, int offset) {
  // CpuSuspendIntr();
  LoadStackEntry* selected = nullptr;
  for (uint32_t i = 0; i < MAX_OPEN_FILES; i++) {
    if (!sLoadStack[i].fr) {
      selected = sLoadStack + i;
      selected->fr = fr;
      break;
    }
  }
  // CpuResumeIntr(local_20[0]);
  if (!selected) {
    printf("======================================================================\n");
    printf("IOP: iso_cd FS_OpenWad: stack full\n");
    printf("======================================================================\n");
    ASSERT_NOT_REACHED();
  }

  selected->uses_blzo = 0;
  selected->size_after_decompression = 0;
  selected->read_bytes = 0;
  // selected->cd_offset = fr->location + offset;
  selected->cd_offset = offset;
  return selected;
}

void FS_Close(LoadStackEntry* lse) {
  if (lse->uses_blzo == 1) {
    SpLargeBuffer->ptr = nullptr;
    SpLargeBuffer->current_buffer_base_ptr = nullptr;
    SpLargeBuffer->allocated_pages = FreePagesList(SpMemoryBuffers, SpLargeBuffer->allocated_pages);
    SignalSema(BlzoSema);
    if (StopPluginStreams == 1) {
      StopPluginStreams = 0;
    }
  }
  if (lse == sReadInfo) {
    sReadInfo = (LoadStackEntry*)0x0;
    ReadPagesCancelRead = 1;
  }
  lse->fr = nullptr;
}

u32 FS_SyncRead() {
  if (sReadInfo) {
    sReadInfo = nullptr;
    return CMD_STATUS_IN_PROGRESS;
  } else {
    return CMD_STATUS_READ_ERR;
  }
}

// FS_StoreSoundBankInIOP: stub
// FS_LoadSoundBankFromIOP: stub
// FS_LoadSoundBankFromEE: stub

// FS_PollDrive: not needed.

// CdReturn: not needed thread

// DoCdReadPages: inlined

// CheckPagesReady: inline/not used

}  // namespace jak2
