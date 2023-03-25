/*!
 * @file fake_iso.cpp
 * This provides an implementation of IsoFs for reading a "fake iso".
 * A "fake iso" is just a map file which maps 8.3 ISO file names to files in the source folder.
 * This way we don't need to actually create an ISO.
 *
 * The game has this compilation unit, but there is nothing in it. Probably it is removed to save
 * IOP memory and was only included on TOOL-only builds.  So this is my interpretation of how it
 * should work.
 */

#if defined(__GNUC__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#elif defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif

#include "fake_iso.h"

#include <cstring>

#include "isocommon.h"
#include "overlord.h"

#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"

#include "game/overlord/sbank.h"
#include "game/overlord/soundcommon.h"
#include "game/overlord/srpc.h"
#include "game/runtime.h"
#include "game/sce/iop.h"
#include "game/sound/sndshim.h"

using namespace iop;

IsoFs fake_iso;

/*!
 * Map from iso file name to file path in the src folder.
 */
struct FakeIsoEntry {
  char iso_name[16];
  char file_path[128];
};

static LoadStackEntry sLoadStack[MAX_OPEN_FILES];  //! List of all files that are "open"
FakeIsoEntry fake_iso_entries[MAX_ISO_FILES];      //! List of all known files
static FileRecord sFiles[MAX_ISO_FILES];           //! List of "FileRecords" for IsoFs API consumers
u32 fake_iso_entry_count;                          //! Total count of fake iso files
static LoadStackEntry* sReadInfo;                  // LoadStackEntry for currently reading file

static int FS_Init(u8* buffer);
static FileRecord* FS_Find(const char* name);
static FileRecord* FS_FindIN(const char* iso_name);
static uint32_t FS_GetLength(FileRecord* fr);
static LoadStackEntry* FS_Open(FileRecord* fr, int32_t offset);
static LoadStackEntry* FS_OpenWad(FileRecord* fr, int32_t offset);
static void FS_Close(LoadStackEntry* fd);
static uint32_t FS_BeginRead(LoadStackEntry* fd, void* buffer, int32_t len);
static uint32_t FS_SyncRead();
static uint32_t FS_LoadSoundBank(char*, void*);
static uint32_t FS_LoadMusic(char*, void*);

static uint32_t FS_LoadSoundBank2(char*, void*);
static uint32_t FS_LoadMusic2(char*, void*);

static void FS_PollDrive();
static void LoadMusicTweaks();

void fake_iso_init_globals() {
  // init file lists
  memset(fake_iso_entries, 0, sizeof(fake_iso_entries));
  memset(sFiles, 0, sizeof(sFiles));
  memset(sLoadStack, 0, sizeof(sLoadStack));
  fake_iso_entry_count = 0;

  // init API struct
  fake_iso.init = FS_Init;
  fake_iso.find = FS_Find;
  fake_iso.find_in = FS_FindIN;
  fake_iso.get_length = FS_GetLength;
  fake_iso.open = FS_Open;
  fake_iso.open_wad = FS_OpenWad;
  fake_iso.close = FS_Close;
  fake_iso.begin_read = FS_BeginRead;
  fake_iso.sync_read = FS_SyncRead;
  fake_iso.poll_drive = FS_PollDrive;

  if (g_game_version == GameVersion::Jak1) {
    fake_iso.load_sound_bank = FS_LoadSoundBank;
    fake_iso.load_music = FS_LoadMusic;
  } else {
    fake_iso.load_sound_bank = FS_LoadSoundBank2;
    fake_iso.load_music = FS_LoadMusic2;
  }

  sReadInfo = nullptr;
}

/*!
 * Initialize the file system.
 */
int FS_Init(u8* buffer) {
  (void)buffer;

  for (const auto& f : fs::directory_iterator(file_util::get_jak_project_dir() / "out" /
                                              game_version_names[g_game_version] / "iso")) {
    if (f.is_regular_file()) {
      ASSERT(fake_iso_entry_count < MAX_ISO_FILES);
      FakeIsoEntry* e = &fake_iso_entries[fake_iso_entry_count];
      std::string file_name = f.path().filename().string();
      ASSERT(file_name.length() < 16);  // should be 8.3.
      strcpy(e->iso_name, file_name.c_str());
      strcpy(e->file_path,
             fmt::format("out/{}/iso/{}", game_version_names[g_game_version], file_name).c_str());
      fake_iso_entry_count++;
    }
  }

  for (u32 i = 0; i < fake_iso_entry_count; i++) {
    MakeISOName(sFiles[i].name, fake_iso_entries[i].iso_name);
    // we don't figure out the size yet.
    // this is so you can change the file without restarting the game.
    sFiles[i].size = -1;
    // repurpose "location" as the index.
    sFiles[i].location = i;
  }

  LoadMusicTweaks();

  return 0;
}

/*!
 * Find a file on the disc and return a FileRecord.
 * Find using a "normal" 8.3 name.
 * This is an ISO FS API Function
 */
FileRecord* FS_Find(const char* name) {
  char name_buff[16];
  MakeISOName(name_buff, name);
  return FS_FindIN(name_buff);
}

/*!
 * Find a file on the disc.  Uses the "ISO name" of the file, which is different from the normal 8.3
 * name.  This can be generated with MakeISOFile.
 * This is an ISO FS API Function.
 */
FileRecord* FS_FindIN(const char* iso_name) {
  const uint32_t* buff = (const uint32_t*)iso_name;
  uint32_t count = 0;
  while (count < fake_iso_entry_count) {
    const uint32_t* ref = (uint32_t*)sFiles[count].name;
    if (ref[0] == buff[0] && ref[1] == buff[1] && ref[2] == buff[2]) {
      return sFiles + count;
    }
    count++;
  }
  printf("[FAKEISO] failed to find %s\n", iso_name);
  return nullptr;
}

/*!
 * Build a full file path for a FileRecord.
 */
static const char* get_file_path(FileRecord* fr) {
  ASSERT(fr->location < fake_iso_entry_count);
  static char path_buffer[1024];
  strcpy(path_buffer, file_util::get_jak_project_dir().string().c_str());
  strcat(path_buffer, "/");
  strcat(path_buffer, fake_iso_entries[fr->location].file_path);
  return path_buffer;
}

/*!
 * Determine the length of a file. This isn't very fast, but nobody checks file sizes extremely
 * quickly. This is an ISO FS API Function
 */
uint32_t FS_GetLength(FileRecord* fr) {
  const char* path = get_file_path(fr);
  file_util::assert_file_exists(path, "fake_iso FS_GetLength");
  FILE* fp = file_util::open_file(path, "rb");
  ASSERT(fp);
  fseek(fp, 0, SEEK_END);
  uint32_t len = ftell(fp);
  rewind(fp);
  fclose(fp);
  return len;
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
  if (fd == sReadInfo) {
    sReadInfo = nullptr;
  }
}

/*!
 * Begin reading!  Returns FS_READ_OK on success (always)
 * This is an ISO FS API Function
 *
 * Idea: do the fopen in FS_Open and keep the file open?  It would be faster.
 */
uint32_t FS_BeginRead(LoadStackEntry* fd, void* buffer, int32_t len) {
  ASSERT(fd->fr->location < fake_iso_entry_count);

  int32_t real_size = len;
  if (len < 0) {
    // not sure what this is about...
    lg::warn("[OVERLORD ISO CD] Negative length warning!");
    real_size = len + 0x7ff;
  }

  u32 sectors = real_size / SECTOR_SIZE;
  real_size = sectors * SECTOR_SIZE;
  u32 offset_into_file = SECTOR_SIZE * fd->location;

  const char* path = get_file_path(fd->fr);
  FILE* fp = file_util::open_file(path, "rb");
  if (!fp) {
    lg::error("[OVERLORD] fake iso could not open the file \"{}\"", path);
  }
  ASSERT(fp);
  fseek(fp, 0, SEEK_END);
  uint32_t file_len = ftell(fp);
  rewind(fp);

  if (offset_into_file < file_len) {
    if (offset_into_file) {
      fseek(fp, offset_into_file, SEEK_SET);
    }

    if (offset_into_file + real_size > file_len) {
      real_size = (file_len - offset_into_file);
    }

    if (fread(buffer, real_size, 1, fp) != 1) {
      ASSERT(false);
    }
  }

  if (len < 0) {
    len = len + 0x7ff;
  }

  fd->location += (len / SECTOR_SIZE);
  sReadInfo = fd;

  fclose(fp);

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

uint32_t FS_LoadMusic(char* name, void* buffer) {
  s32* bank_handle = (s32*)buffer;
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

uint32_t FS_LoadSoundBank(char* name, void* buffer) {
  SoundBank* bank = (SoundBank*)buffer;
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
  fread(buffer, offset, 1, fp);
  fclose(fp);

  s32 handle = snd_BankLoadEx(get_file_path(file), offset, 0, 0);
  snd_ResolveBankXREFS();
  PrintBankInfo(bank);
  bank->bank_handle = handle;

  return 0;
}

uint32_t FS_LoadMusic2(char* name, void* buffer) {
  FileRecord* file = nullptr;
  u32* bank_handle = (u32*)buffer;
  char namebuf[16];
  char isoname[16];
  u32 handle;

  strncpy(namebuf, name, 12);
  namebuf[8] = 0;
  strcat(namebuf, ".mus");

  MakeISOName(isoname, namebuf);

  file = FS_FindIN(isoname);
  if (!file) {
    return 6;
  }

  handle = snd_BankLoadEx(get_file_path(file), 0, 0xcfcc0, 0x61a80);
  snd_ResolveBankXREFS();
  *bank_handle = handle;

  return 0;
}

uint32_t FS_LoadSoundBank2(char* name, void* buffer) {
  SoundBank* bank = (SoundBank*)buffer;
  FileRecord* file = nullptr;
  char namebuf[16];
  char isoname[16];
  u32 handle;

  strncpy(namebuf, name, 12);
  namebuf[8] = 0;
  strcat(namebuf, ".sbk");

  MakeISOName(isoname, namebuf);
  file = FS_FindIN(isoname);
  if (!file) {
    return 6;
  }

  handle = snd_BankLoadEx(get_file_path(file), 0, bank->spu_loc, bank->spu_size);
  snd_ResolveBankXREFS();
  bank->bank_handle = handle;

  return 0;
}

void LoadMusicTweaks() {
  char tweakname[16];
  MakeISOName(tweakname, "TWEAKVAL.MUS");
  auto file = FS_FindIN(tweakname);
  if (file) {
    auto fp = file_util::open_file(get_file_path(file), "rb");
    fread(&gMusicTweakInfo, sizeof(gMusicTweakInfo), 1, fp);
    fclose(fp);
  } else {
    gMusicTweakInfo.TweakCount = 0;
  }
}

#if defined(__GNUC__)
#pragma GCC diagnostic pop
#elif defined(__clang__)
#pragma clang diagnostic pop
#endif
