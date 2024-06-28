#pragma once

namespace jak3 {
void jak3_overlord_init_globals_basefilesystem();

struct ISOFileDef;
struct ISOName;
struct CBaseFile;
struct VagDirEntry;

struct CBaseFileSystem {
  CBaseFileSystem();

  // semaphores for processing open files
  int m_Sema[16];

  // todo
  virtual void Init() = 0;
  // polldrive
  virtual ISOFileDef* Find(const char* name) = 0;
  virtual ISOFileDef* FindIN(const ISOName* name) = 0;
  virtual int GetLength(const ISOFileDef* file) = 0;
  virtual CBaseFile* Open(const ISOFileDef* file_def, int file_kind, int sector_offset);
  virtual CBaseFile* OpenWAD(const ISOFileDef* file_def, int page_offset) = 0;
  virtual VagDirEntry* FindVAGFile(const char* name) = 0;
};
}  // namespace jak3