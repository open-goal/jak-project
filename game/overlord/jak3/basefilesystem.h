#pragma once

namespace jak3 {
void jak3_overlord_init_globals_basefilesystem();

struct ISOFileDef;
struct ISOName;
struct CBaseFile;
struct VagDirEntry;

constexpr int kMaxOpenFiles = 16;

/*!
 * Base class for "FileSystem", which supports finding and opening files.
 * The only implementation we have is CISOCDFileSystem
 */
struct CBaseFileSystem {
  CBaseFileSystem();
  virtual ~CBaseFileSystem() = default;

  // semaphores for processing open files
  int m_Sema[kMaxOpenFiles];

  virtual int Init() = 0;
  // polldrive
  virtual ISOFileDef* Find(const char* name) = 0;
  virtual ISOFileDef* FindIN(const ISOName* name) = 0;
  virtual int GetLength(const ISOFileDef* file) = 0;
  virtual CBaseFile* Open(const ISOFileDef* file_def, int sector_offset, int file_kind) = 0;
  virtual CBaseFile* OpenWAD(const ISOFileDef* file_def, int page_offset) = 0;
  virtual VagDirEntry* FindVAGFile(const char* name) = 0;
};
}  // namespace jak3