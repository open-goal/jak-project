#pragma once

#include "common/common_types.h"

#include "game/overlord/jak3/isocommon.h"
#include "game/overlord/jak3/pagemanager.h"

namespace jak3 {
void jak3_overlord_init_globals_basefile();

struct CPageList;
struct ISOFileDef;
struct ISO_Hdr;

constexpr int kDefaultBufferPageCount = 4;

/*!
 * Base class for a file that the ISO system is processing.
 * This represents an "open" file, and contains references to the buffer holding this file's data
 */
struct CBaseFile {
  CBaseFile();
  CBaseFile(const ISOFileDef* file, int semaphore);
  virtual ~CBaseFile() = default;

  uint8_t* CheckPageBoundary();
  int InitBuffer(CBuffer::BufferType type, ISO_Hdr* msg);
  CPageList* AllocPages();
  void TerminateBuffer();
  void FreePages();

  // buffer that stores some contents of this file
  CBuffer m_Buffer;

  // the number of pages that were allocated for reading this file.
  u32 m_nNumPages;

  // Metadata about the file
  const ISOFileDef* m_FileDef;

  // The compression format used on the file
  enum class Kind {
    UNKNOWN = 0,
    NORMAL = 1,
    LZO_COMPRESSED = 2,
  } m_FileKind = Kind::UNKNOWN;

  EIsoStatus m_Status = EIsoStatus::NONE_0;

  // The expected read rate for streaming, used to prioritize CD reads. Can be 0 if unknown/not
  // applicable.
  int m_ReadRate = 0;

  // Number of sectors that we should read in total, decided based on the file size and request from
  // user when they opened this file.
  int m_LengthPages = 0;  // really, in pages...

  // The current offset. (todo: is this for data we read, processed?)
  int m_PageOffset = 0;

  // Semaphore that we should wait on before handing new data to the process callback.
  // Set to -1 if there is no semaphore.
  // (this is a bit of hack, only used for VAG streaming).
  int m_ProcessDataSemaphore = 0;

  // virtual methods
  virtual EIsoStatus BeginRead() = 0;
  virtual EIsoStatus SyncRead() = 0;
  virtual void Close() = 0;
  virtual int RecoverPages(int num_pages) = 0;
  virtual int GetSector() = 0;
  // ??
  // ??
  // ??
};
}  // namespace jak3