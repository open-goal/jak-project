#include "iso_cd.h"

#include <vector>

#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"

#include "game/overlord/jak3/iso.h"
#include "game/overlord/jak3/isocommon.h"
#include "game/overlord/jak3/overlord.h"
#include "game/overlord/jak3/spustreams.h"
#include "game/sce/iop.h"

using namespace iop;

namespace jak3 {
VagDir g_VagDir;
MusicTweaks gMusicTweakInfo;
CISOCDFile g_CISOCDFiles[kMaxOpenFiles];

namespace {
CISOCDFile* g_pReadInfo = nullptr;
std::vector<ISOFileDef> g_FileDefs;
std::unique_ptr<CISOCDFileSystem> g_ISOCDFileSystem;
}  // namespace

void jak3_overlord_init_globals_iso_cd() {
  g_pReadInfo = nullptr;
  for (auto& f : g_CISOCDFiles) {
    f = CISOCDFile();
  }
  g_FileDefs.clear();
  g_VagDir = {};
  g_ISOCDFileSystem = std::make_unique<CISOCDFileSystem>();
  gMusicTweakInfo = {};
}

CBaseFileSystem* get_file_system() {
  return g_ISOCDFileSystem.get();
}

CISOCDFile::CISOCDFile() {
  m_nSector = -1;
  m_nLoaded = 0;
  m_nLength = 0;
}

namespace {
void ReadPagesCallbackF(CISOCDFile* file, Block* block, s32 error) {
  file->ReadPagesCallback(block, error);
}
}  // namespace

CISOCDFile::CISOCDFile(const jak3::ISOFileDef* filedef, s32 process_data_semaphore)
    : CBaseFile(filedef, process_data_semaphore) {
  m_nSector = -1;
  m_nLoaded = 0;
  m_nLength = 0;
  m_Descriptor.m_File = this;
  m_Descriptor.m_Callback = ReadPagesCallbackF;
}

/*!
 * Call ReadPages to read data from this file into its PageList. I believe that the read
 * is finished after this function returns. Note that this returns COMPLETE enum in many cases, both
 * if the read succeeds, or if the read is not attempted for some reasons.
 */
EIsoStatus CISOCDFile::BeginRead() {
  ASSERT(m_Buffer.m_pPageList);
  ASSERT(m_Buffer.m_eBufferType != CBuffer::BufferType::EBT_FREE);

  if (m_Status == EIsoStatus::NONE_0) {
    return EIsoStatus::ERROR_b;
  }

  if (m_Status == EIsoStatus::OK_2) {
    return EIsoStatus::OK_2;
  }

  ASSERT(m_Status == EIsoStatus::IDLE_1);

  auto* plist = m_Buffer.m_pPageList;

  // how many pages are in our list, but not filled?
  int num_pages_desired = plist->m_nNumPages - plist->m_nNumActivePages;

  if (!num_pages_desired) {
    // no space to read
    return EIsoStatus::OK_2;
  }

  // convert pages to active, indicating that a read will attempt to fill them:
  auto* first_page = plist->AddActivePages(num_pages_desired);
  if (!first_page) {
    lg::warn("Failed to add {} active pages", num_pages_desired);
  }

  // convert buffer type - ??
  switch (m_Buffer.m_eBufferType) {
    case CBuffer::BufferType::EBT_FREE:
      ASSERT_NOT_REACHED();
    case CBuffer::BufferType::NORMAL:
      m_Buffer.m_eBufferType = CBuffer::BufferType::REQUEST_NORMAL;
      break;
    case CBuffer::BufferType::VAG:
      m_Buffer.m_eBufferType = CBuffer::BufferType::REQUEST_VAG;
      break;
    case CBuffer::BufferType::REQUEST_NORMAL:
    case CBuffer::BufferType::REQUEST_VAG:
      break;
  }

  m_Status = EIsoStatus::OK_2;

  // remember this as our currently reading file
  g_pReadInfo = this;
  if (m_FileKind != CBaseFile::Kind::LZO_COMPRESSED) {
    if (m_nLength == 0 || m_nLoaded < m_nLength) {
      ovrld_log(LogCategory::PAGING, "Calling ReadPages: sector {}, pages {}", m_nSector,
                num_pages_desired);
      ReadPages(m_nSector, first_page, num_pages_desired, nullptr, true);
      int bytes = 0x8000 * num_pages_desired;
      m_nLoaded += bytes;
      m_Buffer.AddData(bytes);
      m_nSector += bytes >> 0xb;
    }
  } else {
    ASSERT_NOT_REACHED();
  }

  return EIsoStatus::OK_2;
}

/*!
 * Called by ?? to indicate that the read is done.
 */
EIsoStatus CISOCDFile::SyncRead() {
  if (m_Status != EIsoStatus::IDLE_1 && g_pReadInfo) {
    if (m_Status == EIsoStatus::OK_2) {
      m_Status = EIsoStatus::IDLE_1;
    }
    g_pReadInfo = nullptr;
    return EIsoStatus::OK_2;
  }
  return EIsoStatus::ERROR_b;
}

/*!
 * As soon as possible, stop ongoing reads and free buffers.
 */
void CISOCDFile::Close() {
  // cancel ongoing reading in the driver
  get_driver()->CancelRead(&m_Descriptor);

  ASSERT(m_FileKind != CBaseFile::Kind::LZO_COMPRESSED);  // unsupported in pc
  if (this == g_pReadInfo) {
    g_pReadInfo = nullptr;
  }

  if (m_Buffer.m_eBufferType != CBuffer::BufferType::EBT_FREE) {
    TerminateBuffer();
  }

  // reset self
  *this = CISOCDFile();
}

/*!
 * In the case where we have run out of page memory, take pages that we have loaded, but aren't yet
 * using, and discard them back to the pool.
 */
int CISOCDFile::RecoverPages(int num_pages_desired) {
  // we only allow ourselves to recover pages for VAG streaming.
  if (!m_Buffer.m_pIsoCmd || m_Buffer.m_pIsoCmd->callback != ProcessVAGData) {
    return 0;
  }

  // lock semaphore for processing
  ASSERT(m_ProcessDataSemaphore != -1);
  WaitSema(m_ProcessDataSemaphore);
  auto* plist = m_Buffer.m_pPageList;
  int num_removed = 0;
  if (plist) {
    int pages_to_ask_for = plist->m_nNumUnsteppedPages;
    if (pages_to_ask_for > 1) {
      // don't ask for more than user asked for
      if (pages_to_ask_for > num_pages_desired) {
        pages_to_ask_for = num_pages_desired;
      }
      num_removed = plist->RemoveActivePages(pages_to_ask_for);
      if (num_removed) {
        m_nSector -= 0x10 * num_removed;
        m_nLoaded -= 0x8000 * num_removed;
        // suspend intr
        ASSERT(m_Buffer.m_nDataLength >= 0);
        if (m_Buffer.m_nDataLength > 0) {
          m_Buffer.AddData(-num_removed * 0x8000);
        }
        // resume intr
        ASSERT(m_nLoaded >= 0);
      }
    }
  }

  // move our reading pointer back.
  m_PageOffset -= num_removed;
  // unlock processing
  SignalSema(m_ProcessDataSemaphore);
  return num_removed;
}

/*!
 * Get the next sector to read.
 */
int CISOCDFile::GetSector() {
  return m_nSector;
}

// WaitForLZOPages - not ported.

void CISOCDFile::ReadPages(int sector,
                           jak3::CPage* destination_page,
                           int num_pages,
                           char* done_flag_ptr,
                           bool sleep_until_done) {
  ASSERT(destination_page);
  ASSERT(num_pages >= 1);

  constexpr int kMaxPages = 24;
  // I _think_ this might have been an alloca...
  BlockParams params_array[kMaxPages];
  ASSERT(num_pages <= kMaxPages);

  // iVar2 = -((num_pages + -1) * 0x14 + 0x1bU & 0xfffffff8);
  // params = (BlockParams*)((int)local_30 + iVar2);
  BlockParams* params = params_array;

  // increment our progress in the file - the distributed update of the various progress integers
  // is somewhat confusing, especially since failures here don't seem to reset it properly.
  m_PageOffset += num_pages;

  // Set up block params for each page to read
  CPage* page = destination_page;
  int pg_remaining = num_pages;
  do {
    page->input_state = CPage::State::READING;
    ClearEventFlag(get_page_manager()->m_CCache.m_PagesFilledEventFlag, ~page->mask);

    params->destination = page->m_pPageMemStart;
    params->num_sectors = 0x10;
    params->sector_num = sector;
    params->file_def = m_FileDef;
    params->page = page;

    params->flag = nullptr;
    if (pg_remaining == 1) {             // on the last page...
      params->flag = (char*)0xffffffff;  // flag this
      if (!sleep_until_done) {
        params->flag = done_flag_ptr;
      }
    }
    ovrld_log(LogCategory::PAGING, "[paging] building block for driver: 0x{:x}, {}, {}, flag: {}",
              (u64)params->destination, params->sector_num, params->file_def->name.data,
              (u64)params->flag);
    page = page->m_pNextPage;
    pg_remaining = pg_remaining + -1;
    sector += 0x10;
    params++;
  } while (page && pg_remaining > 0);

  if (pg_remaining == 0) {
    // we set up block params for all the requested pages.
    int pages_actually_read = -1;

    ovrld_log(LogCategory::PAGING, "[paging] Submitting {} blocks to driver.\n", num_pages);
    int status = get_driver()->ReadMultiple(&m_Descriptor, &pages_actually_read, params_array,
                                            num_pages, true);
    if ((status == 0) && pages_actually_read == num_pages) {
      if (!sleep_until_done) {
        return;
      }
      // put us to sleep...
      ovrld_log(LogCategory::PAGING, "[paging] Sleeping, waiting for driver to read {}",
                pages_actually_read);
      SleepThread();
      ovrld_log(LogCategory::PAGING, "[paging] Driver woke us up!");
      return;
    }
    ASSERT_NOT_REACHED();  // unexpected failure to issue read
  } else {
    // ran out of pages....
    ASSERT_NOT_REACHED();  // this was just a warning.. but I think it shouldn't happen.
    if (destination_page) {
      do {
        destination_page->input_state = CPage::State::ACTIVE;
        ClearEventFlag(get_page_manager()->m_CCache.m_PagesFilledEventFlag,
                       ~destination_page->mask);
        destination_page = destination_page->m_pNextPage;
      } while (destination_page);
    }
  }

  ASSERT_NOT_REACHED();
  // only get here is we failed.... set the done flag.
  if (sleep_until_done == 0 && done_flag_ptr) {
    *done_flag_ptr = 1;
  }
}

/*!
 * Callback run by the dvd driver when a page is finished reading.
 */
void CISOCDFile::ReadPagesCallback(jak3::Block* block, int error) {
  if (error == 0) {
    ASSERT(block->params.page->m_nAllocState == 1);
    // flag page as done
    block->params.page->input_state = CPage::State::READ_DONE;
    // set flag
    SetEventFlag(get_page_manager()->m_CCache.m_PagesFilledEventFlag, block->params.page->mask);

    // if this block has a notification, process it:
    if (block->params.flag == (char*)0xffffffff) {  // indicates we should wake caller
      // we assume the caller is the iso thread
      WakeupThread(g_nISOThreadID);
    } else if (block->params.flag) {
      // in this case, it's a pointer.
      *block->params.flag = 1;
    }
  }
}

// DecompressBlock - not ported

/*!
 * Initialize the file system - find all files and set up their definitions.
 */
int CISOCDFileSystem::Init() {
  // drive ready event flag - not ported
  // get disc type - not ported

  // this is mostly not needed... except for some vag pausing nonsense :(
  get_driver()->SetDriverCallback([&](int a) { DvdDriverCallback(a); });

  // skipped a bunch of lzo pages crap
  ReadDirectory();
  LoadMusicTweaks();
  // skip load Disc ID

  // should already be done in the constructor...
  for (auto& f : g_CISOCDFiles) {
    f.m_FileDef = nullptr;
  }
  return 0;
}

// PollDrive - not ported

/*!
 * Find a file definition by name.
 */
ISOFileDef* CISOCDFileSystem::Find(const char* name) {
  ISOName iname;
  file_util::MakeISOName(iname.data, name);
  return FindIN(&iname);
}

/*!
 * Find a file definition by its "ISO Name", a 12-byte name.
 */
ISOFileDef* CISOCDFileSystem::FindIN(const jak3::ISOName* name) {
  for (auto& def : g_FileDefs) {
    if (def.name == *name) {
      return &def;
    }
  }
  return nullptr;
}

/*!
 * Get the length of a file, in bytes.
 */
int CISOCDFileSystem::GetLength(const jak3::ISOFileDef* file) {
  // return file->length;
  lg::info("getlength");
  file_util::assert_file_exists(file->full_path.c_str(), "CISOCDFileSystem GetLength");
  FILE* fp = file_util::open_file(file->full_path.c_str(), "rb");
  ASSERT(fp);
  fseek(fp, 0, SEEK_END);
  uint32_t len = ftell(fp);
  fclose(fp);
  return len;
}

/*!
 * Open a file for reading.
 */
CBaseFile* CISOCDFileSystem::Open(const jak3::ISOFileDef* file_def,
                                  int sector_offset,
                                  int file_kind) {
  auto* file = AllocateFile(file_def);
  ASSERT(file);

  // file kind must be known to be non-compressed (1). (TODO: remove arg)
  ASSERT(file_kind == 1);

  file->m_FileKind = CBaseFile::Kind::NORMAL;
  file->m_nLength = file_def->length;
  file->m_LengthPages = (0x7fff + file->m_nLength) >> 0xf;
  if (file->m_LengthPages < 1) {
    ASSERT_NOT_REACHED();
  }
  file->m_nLoaded = 0;
  file->m_PageOffset = 0;

  // in the original game, this was the sector of the start of the file
  // we just make it 0, since we don't build up a whole iso file.
  file->m_nSector = 0;
  if (sector_offset != -1) {
    file->m_nSector += sector_offset;
  }
  return file;
}

/*!
 * Open a WAD file for reading, given the offset of the data to read, in pages.
 */
CBaseFile* CISOCDFileSystem::OpenWAD(const jak3::ISOFileDef* file_def, int page_offset) {
  auto* file = AllocateFile(file_def);
  ASSERT(file);

  file->m_LengthPages = 1;  // this is not really true..
  file->m_FileKind = CBaseFile::Kind::NORMAL;
  file->m_PageOffset = 0;
  file->m_nSector = page_offset * 0x10;
  file->m_nLoaded = 0;
  file->m_nLength = 0;
  return file;
}

/*!
 * Locate the entry for a VAG file in the VAG directory.
 */
VagDirEntry* CISOCDFileSystem::FindVAGFile(const char* name) {
  u32 packed_name[2];
  PackVAGFileName(packed_name, name);
  for (int i = 0; i < g_VagDir.num_entries; i++) {
    auto& entry = g_VagDir.entries[i];
    if (packed_name[0] == entry.words[0] && packed_name[1] == (entry.words[1] & 0x3ff)) {
      return &entry;
    }
  }
  return nullptr;
}

/*!
 * Get a CISOCDFile* for a newly opened file.
 */
CISOCDFile* CISOCDFileSystem::AllocateFile(const jak3::ISOFileDef* file_def) {
  for (int i = 0; i < kMaxOpenFiles; i++) {
    auto* file = &g_CISOCDFiles[i];
    if (!file->m_FileDef) {
      *file = CISOCDFile(file_def, m_Sema[i]);
      return file;
    }
  }
  ASSERT_NOT_REACHED();
}

/*!
 * Callback from the DVD driver itself into the filesystem. This was originally used for notifying
 * when the tray is opened or closed.
 */
void CISOCDFileSystem::DvdDriverCallback(int) {
  // the only callbacks that do anything are tray open/close, which we don't care about
  ASSERT_NOT_REACHED();
}

// CheckDiscID - not ported
// LoadDiscID - not ported
// ReadSectorsNow - not ported

/*!
 * Find all the files on the disc and set up their information. This is modified for the PC port to
 * just search for files in the appropriate out folder.
 */
void CISOCDFileSystem::ReadDirectory() {
  for (const auto& f :
       fs::directory_iterator(file_util::get_jak_project_dir() / "out" / "jak3" / "iso")) {
    if (f.is_regular_file()) {
      auto& e = g_FileDefs.emplace_back();
      std::string file_name = f.path().filename().string();
      ASSERT(file_name.length() < 16);  // should be 8.3.
      MakeISOName(&e.name, file_name.c_str());
      e.full_path =
          fmt::format("{}/out/jak3/iso/{}", file_util::get_jak_project_dir().string(), file_name);
      auto* fp = file_util::open_file(e.full_path, "rb");
      ASSERT(fp);
      fseek(fp, 0, SEEK_END);
      e.length = ftell(fp);
      fclose(fp);
    }
  }
}

/*!
 * Load the "Music Tweaks" file, which contains a volume setting per music track.
 */
void CISOCDFileSystem::LoadMusicTweaks() {
  ISOName tweakname;
  MakeISOName(&tweakname, "TWEAKVAL.MUS");
  auto file = g_ISOCDFileSystem->FindIN(&tweakname);
  if (file) {
    auto fp = file_util::open_file(file->full_path, "rb");
    ASSERT(fp);
    ASSERT(file->length <= sizeof(gMusicTweakInfo));
    auto ret = fread(&gMusicTweakInfo, file->length, 1, fp);
    ASSERT(ret == 1);
    fclose(fp);
  } else {
    lg::warn("Failed to open music tweak file.");
    gMusicTweakInfo.TweakCount = 0;
  }
}

// Crc32 - not ported
// ReadU32 - not ported

}  // namespace jak3