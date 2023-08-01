#include "fake_iso.h"

#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"

#include "game/overlord/common/fake_iso.h"
#include "game/overlord/common/overlord.h"
#include "game/overlord/common/soundcommon.h"
#include "game/overlord/jak1/isocommon.h"
#include "game/sound/sndshim.h"

namespace jak1 {
IsoFs fake_iso;
LoadStackEntry* FS_Open(FileRecord* fr, int32_t offset);
uint32_t FS_BeginRead(LoadStackEntry* fd, void* buffer, int32_t len);
LoadStackEntry* FS_OpenWad(FileRecord* fr, int32_t offset);
uint32_t FS_LoadSoundBank(char* name, SoundBank* bank);
void FS_PollDrive();
uint32_t FS_SyncRead();
uint32_t FS_LoadMusic(char* name, s32* bank_handle);
void FS_Close(LoadStackEntry* fd);
static LoadStackEntry sLoadStack[MAX_OPEN_FILES];  //! List of all files that are "open"
static LoadStackEntry* sReadInfo;                  // LoadStackEntry for currently reading file

void fake_iso_init_globals() {
  // init API struct
  fake_iso.init = fake_iso_FS_Init;
  fake_iso.find = FS_Find;
  fake_iso.find_in = FS_FindIN;
  fake_iso.get_length = FS_GetLength;
  fake_iso.open = FS_Open;
  fake_iso.open_wad = FS_OpenWad;
  fake_iso.close = FS_Close;
  fake_iso.begin_read = FS_BeginRead;
  fake_iso.sync_read = FS_SyncRead;
  fake_iso.poll_drive = FS_PollDrive;
  fake_iso.load_sound_bank = FS_LoadSoundBank;
  fake_iso.load_music = FS_LoadMusic;

  memset(sLoadStack, 0, sizeof(sLoadStack));
  sReadInfo = nullptr;
}

static FILE* open_fr(FileRecord* fr, s32 thread_to_wake) {
  const char* path = get_file_path(fr);
  FILE* fp = file_util::open_file(path, "rb");
  if (!fp) {
    lg::error("[OVERLORD] fake iso could not open the file \"{}\"", path);
  }

  iop::iWakeupThread(thread_to_wake);

  return fp;
}

/*!
 * Open a file by putting it on the load stack.
 * Set the offset to 0 or -1 if you do not want to have an offset.
 * This is an ISO FS API Function
 */
LoadStackEntry* FS_Open(FileRecord* fr, int32_t offset) {
  lg::debug("[OVERLORD] FS Open {}", fr->name);
  LoadStackEntry* selected = nullptr;
  // find first unused spot on load stack.
  for (uint32_t i = 0; i < MAX_OPEN_FILES; i++) {
    if (!sLoadStack[i].fr) {
      selected = sLoadStack + i;
      selected->fr = fr;
      selected->location = 0;

      if (offset != -1) {
        selected->location += offset;
      }

      auto future = thpool.submit(open_fr, fr, iop::GetThreadId());
      iop::SleepThread();
      selected->fp = future.get();

      return selected;
    }
  }
  lg::warn("[OVERLORD] Failed to FS Open {}", fr->name);
  ExitIOP();
  return nullptr;
}

/*!
 * Open a file by putting it on the load stack.
 * Like Open, but allows an offset of -1 to be applied.
 * This is an ISO FS API Function
 */
LoadStackEntry* FS_OpenWad(FileRecord* fr, int32_t offset) {
  lg::debug("[OVERLORD] FS_OpenWad {}", fr->name);
  LoadStackEntry* selected = nullptr;
  for (uint32_t i = 0; i < MAX_OPEN_FILES; i++) {
    if (!sLoadStack[i].fr) {
      selected = sLoadStack + i;
      selected->fr = fr;
      selected->location = offset;

      auto future = thpool.submit(open_fr, fr, iop::GetThreadId());
      iop::SleepThread();
      selected->fp = future.get();

      return selected;
    }
  }
  lg::warn("[OVERLORD] Failed to FS_OpenWad {}", fr->name);
  ExitIOP();
  return nullptr;
}

/*!
 * Close an open file.
 * This is an ISO FS API Function
 */
void FS_Close(LoadStackEntry* fd) {
  lg::debug("[OVERLORD] FS_Close {} @ {}/{}", fd->fr->name, fd->fr->location, fd->location);

  // close the FD
  fd->fr = nullptr;
  fclose(fd->fp);
  if (fd == sReadInfo) {
    sReadInfo = nullptr;
  }
}

void fs_read(LoadStackEntry* fd, void* buffer, int32_t len, s32 thread_to_wake) {
  int32_t real_size = len;
  if (len < 0) {
    // not sure what this is about...
    lg::warn("[OVERLORD ISO CD] Negative length warning!");
    real_size = len + 0x7ff;
  }
  u32 sectors = real_size / SECTOR_SIZE;
  real_size = sectors * SECTOR_SIZE;
  u32 offset_into_file = SECTOR_SIZE * fd->location;

  ASSERT(fd->fp);
  fseek(fd->fp, 0, SEEK_END);
  uint32_t file_len = ftell(fd->fp);
  rewind(fd->fp);

  if (offset_into_file < file_len) {
    if (offset_into_file) {
      fseek(fd->fp, offset_into_file, SEEK_SET);
    }

    if (offset_into_file + real_size > file_len) {
      real_size = (file_len - offset_into_file);
    }

    if (fread(buffer, real_size, 1, fd->fp) != 1) {
      ASSERT(false);
    }
  }

  if (len < 0) {
    len = len + 0x7ff;
  }

  fd->location += (len / SECTOR_SIZE);
  sReadInfo = fd;

  iop::iWakeupThread(thread_to_wake);
}

/*!
 * Begin reading!  Returns FS_READ_OK on success (always)
 * This is an ISO FS API Function
 *
 * Idea: do the fopen in FS_Open and keep the file open?  It would be faster.
 */
uint32_t FS_BeginRead(LoadStackEntry* fd, void* buffer, int32_t len) {
  ASSERT(fd->fr->location < fake_iso_entry_count);

  auto future = thpool.submit(fs_read, fd, buffer, len, iop::GetThreadId());
  iop::SleepThread();
  future.get();

  return CMD_STATUS_IN_PROGRESS;
}

/*!
 * Block until read completes.
 */
uint32_t FS_SyncRead() {
  // FS_BeginRead is blocking, so this is useless.
  if (sReadInfo) {
    sReadInfo = nullptr;
    return CMD_STATUS_IN_PROGRESS;
  } else {
    return CMD_STATUS_READ_ERR;
  }
}

/*!
 * Poll drive
 */
void FS_PollDrive() {}

uint32_t FS_LoadMusic(char* name, s32* bank_handle) {
  char namebuf[16];
  strcpy(namebuf, name);
  namebuf[8] = 0;
  strcat(namebuf, ".mus");
  auto file = FS_Find(namebuf);
  if (!file)
    return CMD_STATUS_FAILED_TO_OPEN;

  *bank_handle = snd_BankLoadEx(get_file_path(file), 0, 0, 0);
  snd_ResolveBankXREFS();

  return 0;
}

uint32_t FS_LoadSoundBank(char* name, SoundBank* bank) {
  char namebuf[16];

  int offset = 10 * 2048;
  if (bank->sound_count == 101) {
    offset = 1 * 2048;
  }

  strcpy(namebuf, name);
  namebuf[8] = 0;
  strcat(namebuf, ".sbk");

  auto file = FS_Find(namebuf);
  if (!file) {
    file = FS_Find("empty1.sbk");
    if (!file)  // Might have no files when running tests.
      return 0;
  }

  auto fp = file_util::open_file(get_file_path(file), "rb");
  fread(bank, offset, 1, fp);
  fclose(fp);

  s32 handle = snd_BankLoadEx(get_file_path(file), offset, 0, 0);
  snd_ResolveBankXREFS();
  PrintBankInfo(bank);
  bank->bank_handle = handle;

  return 0;
}
}  // namespace jak1
