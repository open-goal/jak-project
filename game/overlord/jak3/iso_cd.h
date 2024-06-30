#pragma once

#include <string>

#include "game/overlord/jak3/basefile.h"
#include "game/overlord/jak3/basefilesystem.h"
#include "game/overlord/jak3/dvd_driver.h"
#include "game/overlord/jak3/isocommon.h"

namespace jak3 {
void jak3_overlord_init_globals_iso_cd();

CBaseFileSystem* get_file_system();

extern VagDir g_VagDir;
extern MusicTweaks gMusicTweakInfo;

struct CISOCDFile : public CBaseFile {
  CISOCDFile();
  CISOCDFile(const ISOFileDef* filedef, s32 process_data_semaphore);
  int m_nLoaded = 0;  // bytes loaded so far
  int m_nSector = 0;  // next sector to read.
  int m_nLength = 0;  // bytes that we want to load. 0 for the whole file
  CDescriptor m_Descriptor;

  EIsoStatus BeginRead() override;

  void ReadPages(int sector,
                 CPage* destination_page,
                 int num_pages,
                 char* done_flag_ptr,
                 bool flag);
  EIsoStatus SyncRead() override;
  void Close() override;
  int RecoverPages(int num_pages) override;
  int GetSector() override;

  void ReadPagesCallback(Block* block, int error);
};

struct CISOCDFileSystem : public CBaseFileSystem {
  CISOCDFileSystem() = default;
  int Init() override;
  ISOFileDef* Find(const char* name) override;
  ISOFileDef* FindIN(const ISOName* name) override;
  int GetLength(const ISOFileDef* file) override;
  CBaseFile* Open(const ISOFileDef* file_def, int sector_offset, int file_kind) override;
  CBaseFile* OpenWAD(const ISOFileDef* file_def, int page_offset) override;
  VagDirEntry* FindVAGFile(const char* name) override;

  void DvdDriverCallback(int a);
  void ReadDirectory();
  void LoadMusicTweaks();
  CISOCDFile* AllocateFile(const ISOFileDef* file);

  //   int m_drive_ready_event_flag = -1;
};
}  // namespace jak3