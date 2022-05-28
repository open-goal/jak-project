/*!
 * @file iso_cd.cpp
 * IsoFs API for accessing the CD/DVD drive.
 */

#include <cstring>
#include "game/sce/iop.h"
#include "game/sce/stubs.h"
#include "iso_cd.h"
#include "isocommon.h"
#include "overlord.h"
#include "soundcommon.h"
#include "srpc.h"
#include "common/log/log.h"

// iso_cd is an implementation of the IsoFs API for loading files from a CD/DVD with an ISO and/or
// DUP filesystem.
// The DUP filesystem is a custom Naughty Dog filesystem which attempts to hide
// files. The DUP filesystem also stores all files twice on the disk and will try reading from the
// other copy if it reading the first copy encounters errors.  The DUP filesystem is unused.

using namespace iop;
typedef int (*mmode_func)(int);

// Drive State
// sector to read from (for DUP files, sector of the first copy of the file)
u32 _sector;
// number of sectors to read
u32 _sectors;
// number of retries in the current read
u32 _retries;
// buffer to read into
void* _buffer;
// set to 0 or 1 to indicate if the first or second copy of DUP files should be used.
uint32_t _dupseg;
// the actual sector to read from (differs from _sector when reading second copy of DUP file)
uint32_t _real_sector;
// set 1 if the current read was continuous from the previous read (didn't require a seek)
uint32_t _continuous;
// time when the current read was started
SysClock _starttime;
// time when the current read has ended
SysClock _endtime;

// Globals
u32 gDirtyCd;          // set when we're waiting on a read which has errors
u32 gNoCD;             // set when we believe the game disc has been removed.
static u32 sNumFiles;  // number of files (includes both ISO and DUP files)
static u32 sArea1;     // Sector where the first copy of DUP files live.
static u32 sAreaDiff;  // Sectors in between the first and second copy of files.

u32 pirated;                                // do we think the game is pirated?
mmode_func cdmmode = nullptr;               // function to call to set the expected media (CD/DVD)
static sceCdRMode sNominalMode;             // drive settings for "nominal" reading
static sceCdRMode sStreamMode;              // drive settings for "streaming" reading
static sceCdRMode* sMode;                   // pointer to currently selected read mode
static LoadStackEntry* sReadInfo;           // LoadStackEntry for currently reading file
static u8* sSecBuffer[3];                   // Buffers for a single sector
u32 add_files;                              // Should we add files we discover to the sFiles list?
static FileRecord sFiles[MAX_ISO_FILES];    // Info for all files on the disc
u32 CD_ID_SectorNum;                        // Sector of the DISK.ID file
s32 CD_ID_Sector[SECTOR_SIZE / 4];          // Contents of the DISK.ID file
s32 CD_ID_SectorSum;                        // Sum of the CD_ID_SECTOR array
LoadStackEntry sLoadStack[MAX_OPEN_FILES];  // List of all files that are "open"
static u32 sound_bank_loads;                // might be a static variable in a function?
IsoFs iso_cd_;                              // IsoFs function pointers

constexpr int TIME_SIZE = 16;  // how many samples for read timing
s32 _times[TIME_SIZE];         // read timing data
s32 _timesix;
s32 _tsamps[2];
s32 _tkps[2];

s32 gLastSpeed;
s32 gDiskSpeed[2];
s32 gDupSeg;

u32 ReadU32(u8* buffer);
u32 ReadSectorsNow(uint32_t sector, uint32_t len, void* buffer);
u32 ReadDirectory(uint32_t sector, uint32_t size, uint32_t secBufID);
void DecodeDUP(u8* buffer);
void LoadMusicTweaks(u8* buffer);
void LoadDiscID();
u32 CheckDiscID();
void SetRealSector();
void CD_WaitReturn();

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

void iso_cd_init_globals() {
  _sector = 0;
  _sectors = 0;
  _retries = 0;
  gDirtyCd = 0;
  gNoCD = 0;
  _dupseg = 0;
  _real_sector = 0;
  _continuous = 0;
  _buffer = nullptr;
  memset(&_starttime, 0, sizeof(SysClock));
  memset(&_endtime, 0, sizeof(SysClock));

  sNumFiles = 0;
  sArea1 = 0;
  sAreaDiff = 0;

  pirated = 0;
  cdmmode = nullptr;

  sNominalMode.trycount = 0;
  sNominalMode.spindlctrl = 1;
  sNominalMode.datapattern = 0;
  sNominalMode.pad = 0;

  sStreamMode.trycount = 0xf;
  sStreamMode.spindlctrl = 0;
  sStreamMode.datapattern = 0;
  sStreamMode.pad = 0;

  sMode = &sStreamMode;
  sReadInfo = nullptr;

  memset(sSecBuffer, 0, sizeof(sSecBuffer));
  add_files = 0;
  memset(sFiles, 0, sizeof(sFiles));

  CD_ID_SectorNum = 0;
  memset(CD_ID_Sector, 0, sizeof(CD_ID_Sector));
  CD_ID_SectorSum = 0;
  memset(sLoadStack, 0, sizeof(sLoadStack));
  sound_bank_loads = 0;

  iso_cd_.init = FS_Init;
  iso_cd_.find = FS_Find;
  iso_cd_.find_in = FS_FindIN;
  iso_cd_.get_length = FS_GetLength;
  iso_cd_.open = FS_Open;
  iso_cd_.open_wad = FS_OpenWad;
  iso_cd_.close = FS_Close;
  iso_cd_.begin_read = FS_BeginRead;
  iso_cd_.sync_read = FS_SyncRead;
  iso_cd_.load_sound_bank = FS_LoadSoundBank;
  iso_cd_.load_music = FS_LoadMusic;
  iso_cd_.poll_drive = FS_PollDrive;

  memset(_times, 0, sizeof(_times));
  memset(_tsamps, 0, sizeof(_tsamps));
  memset(_tkps, 0, sizeof(_tkps));
  _timesix = 0;

  memset(gDiskSpeed, 0, sizeof(gDiskSpeed));
  gLastSpeed = 0;
  gDupSeg = 0;
}

/*!
 * Read unaligned uint32_t from buffer.  ISO file systems may have 32-bit values that aren't word
 * aligned. DONE, EXACT
 */
uint32_t ReadU32(u8* data) {
  return (uint32_t)data[0] + (((uint32_t)data[1]) * 0x100) + (((uint32_t)data[2]) * 0x10000) +
         (((uint32_t)data[3]) * 0x1000000);
}

/*!
 * Read from disc, immediately (blocking), into a local buffer.  Will retry if needed, setting
 * gDirtyCd. This does not use the DUP file system or drive state, so this should not be used
 * outside of initialization. Will clear gDirtyCd on successful read. Returns 1 on success, 0 on
 * sceCdRead failure, or otherwise retries forever until sceCdGetError is OK.
 * The length is in terms of sectors.
 * DONE, EXACT
 */
u32 ReadSectorsNow(uint32_t sector, uint32_t len, void* buffer) {
  // reset the sector state to break any continuous reads in progress
  _sector = 0;

  // retry loop for read
  while (true) {
    // Start async read from DVD...
    if (sceCdRead(sector, len, buffer, sMode) == 0) {
      // if this fails, it indicates catastrophic failure of the DVD drive, so give up immediately.
      return 0;
    }

    // Wait for read to finish (0x00 is blocking)
    sceCdSync(0);

    // check for error
    if (sceCdGetError() == 0) {
      // no error, we are good!
      break;
    }
    // we got an error.  Try again, and set a dirty flag so the EE knows we're having trouble
    gDirtyCd = 1;
  }

  // success! clear the dirty flag and return!
  gDirtyCd = 0;
  return 1;
}

/*!
 * Read ISO file systems directory tree, adding files to the filerecord table if add_files is set.
 * Regardless of add_files is not set, it will be set for folders under NAUGHTY.DOG.
 * This feature is used on demos with multiple games and Jak is in the NAUGHTY.DOG folder.
 * Recursively walks the tree.
 * There is a stack of single sector buffers used to recursively read the directories.
 * Returns 1 on success and 0 on failure.
 * Running out of sector buffers because of too many nested folders is considered success?
 * DONE
 */
u32 ReadDirectory(uint32_t sector, uint32_t size, uint32_t secBufID) {
  if (secBufID < 3) {
    // grab our buffer from the stack
    u8* buffer = sSecBuffer[secBufID];

    uint32_t lsector = sector;
    int32_t lsize = size;

    // loop over sector reads
    while (lsize > 0) {
      // ISO low-level read
      if (!ReadSectorsNow(lsector, 1, buffer)) {
        lg::info("[OVERLORD ISO CD] Failed to read sector in ReadDirectory!");
        return 0;
      }
      u8* lbuffer = buffer;

      // loop over stuff in the sector
      while ((*lbuffer != 0) && (lbuffer < buffer + SECTOR_SIZE)) {
        u8 dir_record_size = *lbuffer;
        if ((lbuffer[0x21] != 0) && (lbuffer[0x21] != 1)) {  // skip over whatever these things are

          uint32_t extent = ReadU32(lbuffer + 2);
          uint32_t dir_size = ReadU32(lbuffer + 10);
          uint32_t name_len = lbuffer[0x20];

          bool is_directory = true;
          if ((lbuffer[0x1f + name_len] == ';') && (lbuffer[0x20 + name_len] == '1')) {
            is_directory = false;
          }

          if (is_directory) {
            if (!add_files) {
              // don't add file by default, but add files if we recurse in the NAUGHTY.DOG folder
              if (!memcmp(lbuffer + 0x21, "NAUGHTY.DOG", 0xb)) {
                add_files = true;
                ReadDirectory(extent, dir_size, secBufID + 1);
                add_files = false;
              }
            } else {
              // otherwise just recurse
              ReadDirectory(extent, dir_size, secBufID + 1);
            }
          } else {
            if (sNumFiles == MAX_ISO_FILES) {
              lg::info("[OVERLORD ISO CD] There are too many files on the disc!");
              return 0;
            }

            if (add_files) {
              lbuffer[0x1f + name_len] = 0;  // null terminate the name
              MakeISOName(sFiles[sNumFiles].name, (char*)(lbuffer + 0x21));
              sFiles[sNumFiles].location = extent;
              sFiles[sNumFiles].size = dir_size;
              sNumFiles++;
            }
          }
        }
        lbuffer += dir_record_size;
      }
      lsector++;
      lsize -= 0x800;
    }
  } else {
    lg::info("[OVERLORD ISO CD] ReadDirectory ran out of sector buffers!");
  }
  return 1;
}

struct DupIndexEntry {
  // 20 bytes
  char name[12];
  u32 location;
  u32 size;
};

/*!
 * DUP code.  The DUP files aren't on the disc, so this doesn't do anything.
 * This would allow for hidden files that aren't in the standard ISO format, presumably to make
 * pirating harder? The DUP files store the location of these hidden files. Also it support having
 * two copies of some files.  There are two areas, both of which are identical. But it was never
 * used.
 */
void DecodeDUP(u8* buffer) {
  (void)buffer;

  // set sArea1 to point to an impossibly large sector - if DUP initialization fails this means no
  // file will be in the DUP zones.
  sArea1 = 0x7fffffff;

  char iso_name[16];
  // all three of these will fail.
  MakeISOName(iso_name, "Z1INDEX.DUP");
  FileRecord* index_file = FS_FindIN(iso_name);
  MakeISOName(iso_name, "Z3AREA1.DUP");
  FileRecord* area1_file = FS_FindIN(iso_name);
  MakeISOName(iso_name, "Z5AREA2.DUP");
  FileRecord* area2_file = FS_FindIN(iso_name);

  // Note - this reads 4 sectors, but the buffer only has enough room for 3 sectors.
  // So this code would likely cause a crash if it was run.
  // Maybe this is why it was removed?
  // Or maybe there used to be 4 init buffers, but one was removed once they gave up on DUP?
  if (index_file && area1_file && area2_file && ReadSectorsNow(index_file->location, 4, buffer)) {
    sArea1 = area1_file->location;                            // marks start of 1st zone
    sAreaDiff = area2_file->location - area1_file->location;  // difference between zones

    // make sure we have enough room to store all entries
    if (sNumFiles + *(s32*)(buffer) <= MAX_ISO_FILES) {
      // read entries
      DupIndexEntry* dup_entries = (DupIndexEntry*)(((u8*)buffer) + 4);
      for (int i = 0; i < *(s32*)(buffer); i++) {
        *(s32*)(&sFiles[sNumFiles].name) = *(s32*)(&dup_entries[i].name);
        *(s32*)(&sFiles[sNumFiles].name + 4) = *(s32*)(&dup_entries[i].name + 4);
        *(s32*)(&sFiles[sNumFiles].name + 8) = *(s32*)(&dup_entries[i].name + 8);
        sFiles[sNumFiles].size = dup_entries[i].size;
        sFiles[sNumFiles].location = dup_entries[i].location;
        sNumFiles++;
      }
    }
  }
}

/*!
 * Load the TWEAKVAL.MUS file into the gMusicTweakInfo file.
 * Only works if the file is less than 1 sector long.
 * If loading fails, writes a 0 to the first 32-bits of gMusicTweakInfo
 * @param buffer a sector buffer which will be used
 */
void LoadMusicTweaks(u8* buffer) {
  char iso_name[16];
  MakeISOName(iso_name, "TWEAKVAL.MUS");
  FileRecord* fr = FS_FindIN(iso_name);
  if (!fr || !ReadSectorsNow(fr->location, 1, buffer)) {
    gMusicTweakInfo.TweakCount = 0;
    lg::warn("[OVERLORD ISO CD] Failed to load music tweaks!");
  } else {
    memcpy((void*)&gMusicTweakInfo, buffer, sizeof(MusicTweaks));
  }
}

/*!
 * Load the DISK ID file and compute the sum.
 * This is used as a checksum to make sure the disc is correct.
 * A literal sum is not a great checksum
 * Also, this function name and the file on the disc itself spell dis{c,k} differently.
 *
 * If there is no DISK_ID.DIZ file, uses whatever is stored at 0x400 instead.
 */
void LoadDiscID() {
  char iso_name[16];
  MakeISOName(iso_name, "DISK_ID.DIZ");
  FileRecord* fr = FS_FindIN(iso_name);
  if (!fr) {
    lg::warn(
        "[OVERLORD ISO CD] LoadDiscID failed to find DISK_ID.DIZ, using sector 0x400 instead!");
    CD_ID_SectorNum = 0x400;
  } else {
    CD_ID_SectorNum = fr->location;
  }

  ReadSectorsNow(CD_ID_SectorNum, 1, &CD_ID_Sector);
  CD_ID_SectorSum = 0;
  for (uint32_t i = 0; i < SECTOR_SIZE / 4; i++) {
    CD_ID_SectorSum += CD_ID_Sector[i];
  }
  lg::info("[OVERLORD] DISK_ID.DIZ OK 0x{:x}\n", CD_ID_SectorSum);
}

/*!
 * Verify that the DISK ID file has not changed. Returns 1 if it is good.
 */
u32 CheckDiskID() {
  if (ReadSectorsNow(CD_ID_SectorNum, 1, CD_ID_Sector) == 0) {
    // failed to read CD ID data
    return 0;
  }

  int sum = 0;
  for (uint32_t i = 0; i < SECTOR_SIZE / 4; i++) {
    sum += CD_ID_Sector[i];
  }
  return sum == CD_ID_SectorSum;
}

/*!
 * Set _real_sector in preparation for a read, based on the requested _sector.
 * This has logic for a system which has a double copy of some data on the disc and can pick between
 * two different copies. This selection is done with the dupseg flag.
 */
void SetRealSector() {
  // if we are below sArea1, it's not a duplicated file, so ignore the dupseg flag and read directly
  if (_sector < sArea1 || _dupseg == 0) {
    _real_sector = _sector;
  } else {
    // it's a duplicated file, and duplicate read is enabled, so get the area 2 sector.
    _real_sector = _sector + sAreaDiff;
    lg::warn("[OVERLORD] Warning, adjusting real sector in SetRealSector");
  }

  // we suspect the game is pirated, load the wrong sector.
  if (pirated) {
    _real_sector += 3;
    lg::warn("Pirated!");
  }
}

/*!
 * Initialize the ISO CD system and builds file record table.
 * This is an ISO_FS API Function.
 * Also loads music tweaks/DISK ID
 * @param buffer : a buffer larger enough to hold 3 sectors
 *                 this buffer can be freed immediately this returns
 * Return 0 on success.
 */
int FS_Init(u8* buffer) {
  // determine disk type
  int disk_type = SCECdDETCT;
  while (disk_type = sceCdGetDiskType(), disk_type == SCECdDETCT) {
    // This SleepThread will cause the Overlord initialization to lock up. It's called with an
    // argument of 10000, but SleepThread accepts no arguments. Probably they meant to call
    // DelayThread. It ends up working because the drive already knows the disk type at this point.
    SleepThread();
  }

  // what is this. it's crazy. why?
  if (disk_type <= SCECdPS2DVD || disk_type < SCECdCDDA || disk_type <= SCECdDVDV ||
      disk_type != SCECdIllegalMedia) {
    // we are actually using the CD drive, so set the mmode function to the SCE function.
    // This is called in FS_LoadMusic.  If you call this with the wrong media type, it locks up.
    // I guess this is an attempt at making convoluted anti-piracy code so it's harder to find
    // calls to sceCdMmode with static analysis.  But they left in debug symbols and the variable
    // is called "cdmmode", which is not a very sneaky way to hide it!  (At least on the EE it's
    // called aybabtu and is a GOAL symbol which is way harder to figure out.)  Also it seems like
    // the primary mode of piracy they were concerned with is somebody swapping a DVD with a CD?
    cdmmode = sceCdMmode;

    // verify the disc is a DVD.
    sceCdMmode(SCECdDVD);

    // set up sector buffers used for initialization reads.
    for (int i = 0; i < 3; i++) {
      sSecBuffer[i] = buffer + i * SECTOR_SIZE;
    }

    // read primary volume descriptor into buffer
    if (!ReadSectorsNow(0x10, 1, sSecBuffer[0])) {
      lg::warn("[OVERLORD ISO CD] Failed to read primary volume descriptor");
      return 1;
    }

    // check volume descriptor identifier
    if (memcmp(sSecBuffer[0] + 1, "CD001", 5)) {
      lg::warn("[OVERLORD ISO CD] Got the wrong volume descriptor identifier");
      char* cptr = (char*)sSecBuffer[0] + 1;
      printf("%c%c%c%c%c\n", cptr[0], cptr[1], cptr[2], cptr[3], cptr[4]);
      return 1;
    }

    // read path table into buffer
    uint32_t path_table_sector = ReadU32(sSecBuffer[0] + 0x8c);

    if (!ReadSectorsNow(path_table_sector, 1, sSecBuffer[0])) {
      lg::warn("[OVERLORD ISO CD] Failed to read path");
      return 1;
    }

    // read path table's extent into buffer
    uint32_t path_table_extent = ReadU32(sSecBuffer[0] + 2);

    if (!ReadSectorsNow(path_table_extent, 1, sSecBuffer[0])) {
      lg::warn("[OVERLORD ISO CD] Failed to read path table extent");
    }

    // read root directory
    add_files = true;
    uint32_t dir_size = ReadU32(sSecBuffer[0] + 10);
    if (!ReadDirectory(path_table_extent, dir_size, 0)) {
      lg::warn("[OVERLORD ISO CD] Failed to ReadDirectory");
      return 1;
    }

    // load filesystem stuff
    DecodeDUP(sSecBuffer[0]);
    LoadMusicTweaks(sSecBuffer[0]);
    LoadDiscID();

    // there's some sort of weird loop here over all file that does nothing.
    // my guess is its some commented out print thing?

    // empty load stack
    for (int i = 0; i < MAX_OPEN_FILES; i++) {
      sLoadStack[i].fr = nullptr;
    }

    // kill sector buffers
    for (int i = 0; i < 3; i++) {
      sSecBuffer[i] = nullptr;
    }
    return 0;
  } else {
    lg::warn("[OVERLORD ISO CD] Bad Media Type!");
    return 1;
  }
}

/*!
 * Find a file on the disc and return a FileRecord.
 * This is an ISO FS API Function
 */
FileRecord* FS_Find(const char* name) {
  char name_buff[16];
  MakeISOName(name_buff, name);
  return FS_FindIN(name_buff);
}

/*!
 * Find a file on the disc.  Uses the ISO name of the file.
 * This can be generated with MakeISOFile
 * This is an ISO FS API Function
 * There is a weird anti-piracy thing in here to prevent people from making copies with less than
 * 1 GB of data?  I guess you could remove the audio in languages you don't care about and put in
 * on a CD, and this would block this from happening.
 */
FileRecord* FS_FindIN(const char* iso_name) {
  const uint32_t* buff = (const uint32_t*)iso_name;
  for (;;) {            // this loop will spin forever if you have < 1 GB of files
    uint32_t size = 0;  // total sum of file sizes
    uint32_t count = 0;
    while (count < sNumFiles) {
      const uint32_t* ref = (uint32_t*)sFiles[count].name;
      if (ref[0] == buff[0] && ref[1] == buff[1] && ref[2] == buff[2]) {
        return sFiles + count;
      }
      size += sFiles[count].size;
      count++;
    }
    // if we get here, we haven't found the file, we should return 0 to indicate we don't have it
    // however, if we haven't found 1 GB of files after searching the whole thing
    // we assume that we've pirated the game and should continue looping
    // Note that the game attempts to load DUP files which will fails and will hit this condition.
    buff +=
        3;  // to make this look less suspicious, lets increment buff. also will crash eventually.
    if (0x3fffffff < size) {
      return nullptr;  // we got 1 GB of files, okay to return
    }

    // we didn't get 1 GB of files, you're a pirate.
    lg::warn("Pirated!");
  }
}

/*!
 * Determine the length of a file.
 * This is an ISO FS API Function
 */
uint32_t FS_GetLength(FileRecord* fr) {
  return fr->size;
}

/*!
 * Open a file by putting it on the load stack.
 * Set the offset to 0 or -1 if you do not want to have an offset.
 * This is an ISO FS API Function
 */
LoadStackEntry* FS_Open(FileRecord* fr, int32_t offset) {
  lg::info("[OVERLORD] FS Open {}", fr->name);
  LoadStackEntry* selected = nullptr;
  // find first unused spot on load stack.
  for (uint32_t i = 0; i < MAX_OPEN_FILES; i++) {
    if (!sLoadStack[i].fr) {
      selected = sLoadStack + i;
      selected->fr = fr;
      selected->location = fr->location;
      if (offset != -1) {
        selected->location += offset;
      }
      return selected;
    }
  }
  lg::warn("[OVERLORD ISO CD] Failed to FS_Open {}", fr->name);
  ExitIOP();
  return nullptr;
}

/*!
 * Open a file by putting it on the load stack.
 * Like Open, but allows an offset of -1 to be applied.
 * This is an ISO FS API Function
 */
LoadStackEntry* FS_OpenWad(FileRecord* fr, int32_t offset) {
  printf("[OVERLORD] FS Open %s\n", fr->name);
  LoadStackEntry* selected = nullptr;
  for (uint32_t i = 0; i < MAX_OPEN_FILES; i++) {
    if (!sLoadStack[i].fr) {
      selected = sLoadStack + i;
      selected->fr = fr;
      selected->location = fr->location + offset;
      return selected;
    }
  }
  lg::warn("[OVERLORD ISO CD] Failed to use FS_OpenWad {}", fr->name);
  ExitIOP();
  return nullptr;
}

/*!
 * Close an open file.
 * This is an ISO FS API Function
 */
void FS_Close(LoadStackEntry* fd) {
  lg::info("[OVERLORD] FS Close {}", fd->fr->name);
  if (fd == sReadInfo) {
    // the file is currently being read, so lets try to finish out the read, if possible.
    int count = 0;

    // the non-blocking sync, so we don't get stuck here on a catastrophic error.
    while (sceCdSync(1)) {
      DelayThread(1000);  // wait 1 ms and allow other stuff to run.
      count++;
      if (count == 1000) {  // waited too long to close this file
        sceCdBreak();       // interrupt the read
        break;
      }
    }
    sReadInfo = nullptr;
  }

  // close the FD
  fd->fr = nullptr;
}

/*!
 * Begin reading!  Returns FS_READ_OK on success (always)
 * This is an ISO FS API Function
 */
uint32_t FS_BeginRead(LoadStackEntry* fd, void* buffer, int32_t len) {
  // set the reading state:
  // I guess continuous stream buffer reads don't count as continuous?
  _continuous = (len == BUFFER_PAGE_SIZE) && (fd->location == (_sector + _sectors));
  _sector = fd->location;
  int32_t real_size = len;
  if (len < 0) {
    // not sure what this is about...
    lg::warn("[OVERLORD ISO CD] Negative length warning!");
    real_size = len + 0x7ff;
  }
  _sectors = real_size >> 11;
  _retries = 0;
  _buffer = buffer;
  GetSystemTime(&_starttime);

  // compute _real_sector
  SetRealSector();

  while (!sceCdRead(_real_sector, _sectors, _buffer, sMode)) {
    // error starting the read.  this is bad and possibly indicates somebody took the CD out.
    // lets wait for the CD to be ready again...
    CD_WaitReturn();
    _retries++;
    if (_sector >= sArea1) {
      // the original file we tried to read is duplicated...
      // so lets try reading the other copy of it!
      _dupseg = 1 - _dupseg;
      _continuous = 0;  // mark as noncontinuous read
      SetRealSector();  // recompute!
    }
  }

  // ??? this is strangely set up.
  if (len < 0) {
    len = len + 0x7ff;
  }

  fd->location += (len >> 0xb);

  // set sReadInfo to point to the current read.
  sReadInfo = fd;
  return CMD_STATUS_IN_PROGRESS;
}

/*!
 * Wait for current read to complete!
 * This is an ISO FS API Function
 * @return
 */
uint32_t FS_SyncRead() {
  // make sure a read is actually in progress
  if (!sReadInfo) {
    return CMD_STATUS_READ_ERR;
  }

  // remember when we start doing a SyncRead.
  SysClock now;
  GetSystemTime(&now);

  // block and wait for completion
  sceCdSync(0);
  // remember when we sync.
  GetSystemTime(&_endtime);

  // Loop to check if read succeed and start an additional read if not.
  while (sceCdGetError()) {
    // no, it didn't, lets retry
    _retries++;
    // toggle dupseg
    if (_sector >= sArea1) {
      _dupseg = 1 - _dupseg;
      _continuous = 0;
      SetRealSector();
    }

    // try until a read starts...
    while (!sceCdRead(_real_sector, _sectors, _buffer, sMode)) {
      // read start failed, possibly CD is removed
      CD_WaitReturn();
      // retry!
      _retries++;
      // toggle dupseg if possible
      if (_sector >= sArea1) {
        _dupseg = 1 - _dupseg;
        _continuous = 0;
        SetRealSector();
      }
    }

    // read has started
    // set dirty cd to indicate we had trouble
    gDirtyCd = 1;
    // wait for read to finish...
    sceCdSync(0);
    // if the read/sync fails, the loop will go again.
  }

  // Read complete! Mark CD as not dirty and clear active read!
  gDirtyCd = 0;
  sReadInfo = nullptr;

  // Optionally do some timing checks
  // (note that these never run because we don't have DUP files)
  // continuous read with no failures from dup zone
  // more than half the time spent in FS_SyncRead
  // Basically it tries to learn about which segment does worse in "too slow" reads
  // by averaging all historical too slow reads.  Once the other is winning by a certain amount
  // it will swap. It will also swap if there isn't enough samples on one.
  if (_retries == 0 && _continuous && _sectors >= sArea1 &&
      (_endtime.hi - _starttime.hi) / 2 < (_endtime.hi - now.hi)) {
    // record the time
    _times[_timesix++] = _endtime.hi - _starttime.hi;

    // if we filled the time buffer
    if (_timesix == TIME_SIZE) {
      // compute total time
      s32 total_time = 0;
      for (s32 i = 0; i < TIME_SIZE; i++) {
        total_time += _times[i];
      }

      // determine read speed
      gLastSpeed = 0x69780000 / (total_time >> 4);  // todo - work out this constant

      // add to average kps for this seg
      if (_tsamps[_dupseg] < 0x40000) {
        _tkps[_dupseg] = _tkps[_dupseg] + gLastSpeed;
        _tsamps[_dupseg] = _tsamps[_dupseg];
      }

      // average speed of this segment
      gDiskSpeed[_dupseg] = _tkps[_dupseg] / _tsamps[_dupseg];

      _timesix = 0;

      if (_tsamps[0] < 8) {
        // not much information about segment 0
        if (_dupseg) {
          // and we aren't reading segment 0...
          // so let's read segment 0
          _dupseg = 0;
          _sector = 0;
        }
      } else {
        // got enough info about segment 0.
        if (_tsamps[1] < 8) {
          // not enough information about segment 1
          if (!_dupseg) {
            // and not reading, so lets read it.
            _dupseg = 1;
            _sector = 0;
          }
        } else {
          // enough info about both.
          if ((_tkps[1] / _tsamps[1] + 0x32) < (_tkps[0] / _tsamps[0])) {
            // section 0 wins by at least 0x32, lets use it if we aren't already
            if (_dupseg) {
              _dupseg = 0;
              _sector = 0;
            }
          } else if ((_tkps[0] / _tsamps[0] + 0x32) < (_tkps[1] / _tsamps[1])) {
            if (!_dupseg) {
              _dupseg = 1;
              _sector = 0;
            }
          }
        }
      }
      // set our current decision in a global for the EE to read.
      gDupSeg = _dupseg;
    }
  }
  return CMD_STATUS_IN_PROGRESS;
}

/*!
 * Load a SoundBank now. Doesn't do any fancy read stuff.
 */
uint32_t FS_LoadSoundBank(char* name, void* buffer) {
  char full_name[32];  // may actually be 8, but lets be safe

  // ??? todo this is probably a field of the buffer.
  u32 header_size;
  if (*(s32*)(((u8*)buffer) + 0x14) == 0x65) {
    header_size = 1;
  } else {
    header_size = 10;
  }

  if (strlen(name) > 16) {
    printf("[OVERLORD ISO CD] FS_LoadSoundBank has an invalid name!\n");
  }

  // append .sbk
  strcpy(full_name, name);
  strcat(full_name, ".sbk");

  FileRecord* fr = FS_Find(full_name);
  if (!fr) {
    printf("[OVERLORD ISO CD] FS_LoadSoundBank cannot find bank %s, loading empty instead.\n",
           full_name);
    fr = FS_Find("empty1.sbk");
  }

  // hack to do a read now (the Sound Bank loads bypass all the other fancy loading stuff evidently)
  _sector = fr->location;
  SetRealSector();

  // loop until we read header successfully.
  // don't set retries or dirty cd
  while (!ReadSectorsNow(_real_sector, header_size, buffer)) {
    // ReadSectorsNow will only return if the read fails to start.  in this case we assume the disc
    // was removed:
    CD_WaitReturn();
    // we don't increment retries...
    if (_sector >= sArea1) {
      _dupseg = 1 - _dupseg;
      _continuous = 0;
      SetRealSector();
    }
  }

  // now have the sound library do a load.
  // (this time we set dirty cd if it fails, but no retries)
  auto load_status = snd_BankLoadByLoc(_real_sector + header_size, 0);
  while (!load_status && snd_GetLastLoadError() < 0x100) {
    CD_WaitReturn();
    if (_sector >= sArea1) {
      _dupseg = 1 - _dupseg;
      _continuous = 0;
      SetRealSector();
    }
    load_status = snd_BankLoadByLoc(_real_sector + header_size, 0);
    if (!load_status) {
      gDirtyCd = 1;
    }
  }
  gDirtyCd = 0;

  // pirate check sometimes
  sound_bank_loads++;
  if ((sound_bank_loads & 7) == 0) {
    pirated = 1;
    // check that one file is past sector 0x80000 (approx 1 GB)
    for (u32 i = 0; i < sNumFiles; i++) {
      if (sFiles[i].location + (sFiles[i].size >> 11) > 0x80000) {
        pirated = 0;
      }
    }
  }

  snd_ResolveBankXREFS();
  PrintBankInfo((SoundBank*)buffer);
  _sector = 0;
  // ??? todo this is probably a field of the buffer.
  *(s32*)(((u8*)buffer) + 0x10) = load_status;
  return 0;
}

/*!
 * Load a music file. Load now, doesn't do fancy reading stuff.
 */
uint32_t FS_LoadMusic(char* name, void* buffer) {
  char full_name[32];  // may actually be 8, but lets be safe
  if (strlen(name) > 16) {
    printf("[OVERLORD ISO CD] FS_LoadMusic has an invalid name!\n");
  }

  // append .mus
  strcpy(full_name, name);
  strcat(full_name, ".mus");

  FileRecord* fr = FS_Find(full_name);
  if (!fr) {
    printf("[OVERLORD ISO CD] FS_LoadMusic cannot find bank %s.\n", full_name);
    return 6;
  }

  _sector = fr->location;
  SetRealSector();
  // another "piracy" check to make sure the media is the correct type...
  (*cdmmode)(SCECdDVD);

  // now have the sound library do a load.
  auto load_status = snd_BankLoadByLoc(_real_sector, 0);
  // TODO magic constant 0x100
  while (!load_status && snd_GetLastLoadError() < 0x100) {
    CD_WaitReturn();
    if (_sector >= sArea1) {
      _dupseg = 1 - _dupseg;
      _continuous = 0;
      SetRealSector();
    }
    load_status = snd_BankLoadByLoc(_real_sector, 0);
    if (!load_status) {
      gDirtyCd = 1;
    }
  }
  gDirtyCd = 0;
  snd_ResolveBankXREFS();
  _sector = 0;
  *(s32*)buffer = load_status;
  return 0;
}

/*!
 * Make sure the drive is happy.
 * NOTE - only call this when the drive should have nothing to do!
 */
void FS_PollDrive() {
  if (sceCdDiskReady(1) == SCECdNotReady) {  // non-blocking
    CD_WaitReturn();
  }
}

/*!
 * Wait for the game CD/DVD to be put back in the playstation.
 * Only call this if you think the CD/DVD has been removed, as requires a seek.
 */
void CD_WaitReturn() {
  gNoCD = 1;
  do {
    while (sceCdDiskReady(1) == SCECdNotReady) {
    }
  } while (!CheckDiskID());
  gNoCD = 0;
}
