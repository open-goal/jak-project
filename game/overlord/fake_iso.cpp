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

#include <cstring>
#include <cassert>
#include "fake_iso.h"
#include "game/sce/iop.h"
#include "isocommon.h"
#include "overlord.h"
#include "common/util/FileUtil.h"
#include "third-party/spdlog/include/spdlog/spdlog.h"

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
static bool read_in_progress;                      //! Does the ISO Thread think we're reading?

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
static void FS_PollDrive();

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
  fake_iso.load_sound_bank = FS_LoadSoundBank;
  fake_iso.load_music = FS_LoadMusic;
  fake_iso.poll_drive = FS_PollDrive;

  read_in_progress = false;
}

/*!
 * Initialize the file system.
 */
int FS_Init(u8* buffer) {
  (void)buffer;

  // get path to next/data/fake_iso.txt, the map file.
  char fakeiso_path[512];
  strcpy(fakeiso_path, file_util::get_file_path({"game", "fake_iso.txt"}).c_str());

  // open the map.
  FILE* fp = fopen(fakeiso_path, "r");
  assert(fp);
  fseek(fp, 0, SEEK_END);
  size_t len = ftell(fp);
  rewind(fp);
  char* fakeiso = (char*)malloc(len + 1);
  if (fread(fakeiso, len, 1, fp) != 1) {
#ifdef __linux__
    assert(false);
#endif
  }
  fakeiso[len] = '\0';

  // loop over lines
  char* ptr = fakeiso;
  while (*ptr) {
    // newlines
    while (*ptr && *ptr == '\n')
      ptr++;

    // comment line
    if (*ptr == ';') {
      while (*ptr && (*ptr != '\n')) {
        ptr++;
      }
      continue;
    }

    // entry line
    assert(fake_iso_entry_count < MAX_ISO_FILES);
    FakeIsoEntry* e = &fake_iso_entries[fake_iso_entry_count];
    int i = 0;
    while (*ptr && (*ptr != ' ') && i < 16) {
      e->iso_name[i] = *ptr;
      ptr++;
      i++;
    }

    while (*ptr == ' ') {
      ptr++;
    }

    i = 0;
    while (*ptr && (*ptr != '\n') && (*ptr != ' ') && i < 128) {
      e->file_path[i] = *ptr;
      ptr++;
      i++;
    }
    e->file_path[i] = 0;
    fake_iso_entry_count++;
  }

  for (u32 i = 0; i < fake_iso_entry_count; i++) {
    MakeISOName(sFiles[i].name, fake_iso_entries[i].iso_name);
    // we don't figure out the size yet.
    // this is so you can change the file without restarting the game.
    sFiles[i].size = -1;
    // repurpose "location" as the index.
    sFiles[i].location = i;
  }

  free(fakeiso);

  // TODO load tweak music.

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
  assert(false);
  return nullptr;
}

/*!
 * Build a full file path for a FileRecord.
 */
static const char* get_file_path(FileRecord* fr) {
  assert(fr->location < fake_iso_entry_count);
  static char path_buffer[1024];
  strcpy(path_buffer, file_util::get_project_path().c_str());
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
  FILE* fp = fopen(path, "rb");
  assert(fp);
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
  //printf("[OVERLORD] FS Open %s\n", fr->name);  // Added
  spdlog::info("[OVERLORD] FS Open {}", fr->name);
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
  //printf("[OVERLORD ISO CD] Failed to FS_Open %s\n", fr->name);
  spdlog::warn("[OVERLORD] Failed to FS Open {}", fr->name);
  ExitIOP();
  return nullptr;
}

/*!
 * Open a file by putting it on the load stack.
 * Like Open, but allows an offset of -1 to be applied.
 * This is an ISO FS API Function
 */
LoadStackEntry* FS_OpenWad(FileRecord* fr, int32_t offset) {
  //printf("[OVERLORD] FS Open %s\n", fr->name);  // Added
  spdlog::info("[OVERLORD] FS_OpenWad {}", fr->name);
  LoadStackEntry* selected = nullptr;
  for (uint32_t i = 0; i < MAX_OPEN_FILES; i++) {
    if (!sLoadStack[i].fr) {
      selected = sLoadStack + i;
      selected->fr = fr;
      selected->location = offset;
      return selected;
    }
  }
  //printf("[OVERLORD ISO CD] Failed to FS_OpenWad %s\n", fr->name);
  spdlog::warn("[OVERLORD] Failed to FS_OpenWad {}", fr->name);
  ExitIOP();
  return nullptr;
}

/*!
 * Close an open file.
 * This is an ISO FS API Function
 */
void FS_Close(LoadStackEntry* fd) {
  //printf("[OVERLORD] FS Close %s\n", fd->fr->name);
  spdlog::info("[OVERLORD] FS_Close {}", fd->fr->name);

  // close the FD
  fd->fr = nullptr;
  read_in_progress = false;
}

/*!
 * Begin reading!  Returns FS_READ_OK on success (always)
 * This is an ISO FS API Function
 *
 * Idea: do the fopen in FS_Open and keep the file open?  It would be faster.
 */
uint32_t FS_BeginRead(LoadStackEntry* fd, void* buffer, int32_t len) {
  assert(fd->fr->location < fake_iso_entry_count);

  int32_t real_size = len;
  if (len < 0) {
    // not sure what this is about...
    //printf("[OVERLORD ISO CD] negative length warning!\n");
    spdlog::warn("[OVERLORD ISO CD] Negative length warning!");
    real_size = len + 0x7ff;
  }

  u32 sectors = real_size / SECTOR_SIZE;
  real_size = sectors * SECTOR_SIZE;
  u32 offset_into_file = SECTOR_SIZE * fd->location;

  const char* path = get_file_path(fd->fr);
  FILE* fp = fopen(path, "rb");
  assert(fp);
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
      assert(false);
    }
  }

  if (len < 0) {
    len = len + 0x7ff;
  }

  fd->location += (len / SECTOR_SIZE);
  read_in_progress = true;

  return CMD_STATUS_IN_PROGRESS;
}

/*!
 * Block until read completes.
 */
uint32_t FS_SyncRead() {
  // FS_BeginRead is blocking, so this is useless.
  if (read_in_progress) {
    read_in_progress = false;
    return CMD_STATUS_IN_PROGRESS;
  } else {
    return CMD_STATUS_READ_ERR;
  }
}

/*!
 * Poll drive
 */
void FS_PollDrive() {}

// TODO FS_LoadMusic
uint32_t FS_LoadMusic(char* name, void* buffer) {
  (void)name;
  (void)buffer;
  assert(false);
  return 0;
}

// TODO FS_LoadSoundBank
uint32_t FS_LoadSoundBank(char* name, void* buffer) {
  (void)name;
  (void)buffer;
  assert(false);
  return 0;
}
