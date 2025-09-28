#include "basefile.h"

#include "common/log/log.h"
#include "common/util/Assert.h"

#include "game/overlord/jak3/overlord.h"
#include "game/overlord/jak3/pagemanager.h"
#include "game/overlord/jak3/vag.h"

namespace jak3 {
void jak3_overlord_init_globals_basefile() {}

/*!
 * Construct a CBaseFile in an unused state.
 */
CBaseFile::CBaseFile() {
  m_Buffer.m_pCurrentData = nullptr;
  m_Buffer.m_pCurrentPageStart = nullptr;
  m_Buffer.m_nMinNumPages = 1;
  m_Buffer.m_nMaxNumPages = kDefaultBufferPageCount;
  m_Buffer.m_nDataLength = 0;
  m_Buffer.m_pPageList = nullptr;
  m_Buffer.m_pIsoCmd = nullptr;
  m_Buffer.m_eBufferType = CBuffer::BufferType::EBT_FREE;

  m_ProcessDataSemaphore = -1;
  m_FileDef = nullptr;
  m_FileKind = Kind::UNKNOWN;
  m_Status = EIsoStatus::NONE_0;
  m_ReadRate = 0;
  m_LengthPages = 0;
  m_PageOffset = 0;
  m_nNumPages = kDefaultBufferPageCount;
}

/*!
 * Construct a CBaseFile for a given file, but keep it in the "idle" state, with no buffer
 * allocated.
 */
CBaseFile::CBaseFile(const jak3::ISOFileDef* file, int semaphore) {
  m_Buffer.m_pCurrentData = nullptr;
  m_Buffer.m_nMaxNumPages = kDefaultBufferPageCount;
  m_Buffer.m_nDataLength = 0;
  m_Buffer.m_nMinNumPages = 1;
  m_Buffer.m_pPageList = nullptr;
  m_Buffer.m_pIsoCmd = nullptr;
  m_Buffer.m_pCurrentPageStart = nullptr;
  m_Buffer.m_eBufferType = CBuffer::BufferType::EBT_FREE;

  m_nNumPages = kDefaultBufferPageCount;
  m_FileDef = file;
  m_ProcessDataSemaphore = semaphore;
  m_FileKind = Kind::UNKNOWN;
  m_Status = EIsoStatus::IDLE_1;
  m_ReadRate = 0;
  m_LengthPages = 0;
  m_PageOffset = 0;
}

/*!
 * Update our buffer to handle crossing page boundaries, return pointer to next data.
 */
uint8_t* CBaseFile::CheckPageBoundary() {
  // Can't check page boundary if the buffer is not allocated.
  ASSERT(m_Buffer.m_eBufferType != CBuffer::BufferType::EBT_FREE);

  // Can't check page boundary if there is no manager
  ASSERT(m_Buffer.m_pPageList);

  CPageList* page_list = m_Buffer.m_pPageList;
  CPage* page = page_list->m_pCurrentActivePage;

  // can only return data if there's an active page
  if (!page || page_list->m_nNumActivePages <= 0) {
    return nullptr;
  }

  // buffer doesn't know the current page, just reset it to the start of the active page.
  if (m_Buffer.m_pCurrentPageStart == nullptr) {
    m_Buffer.m_pCurrentData = page->m_pPageMemStart;
    m_Buffer.m_pCurrentPageStart = page->m_pPageMemStart;
  } else {
    uint8_t* end_ptr = page->m_pPageMemEnd;
    // check if our data pointer crossed the page boundary
    if (end_ptr <= m_Buffer.m_pCurrentData) {
      // it did!
      uint8_t* past_boundary_ptr = m_Buffer.m_pCurrentData;

      // get the next active page
      CPage* next_page = page_list->StepActivePage();
      if (!next_page) {
        // no more active pages, no data to process
        m_Buffer.m_pCurrentPageStart = nullptr;
        m_Buffer.m_pCurrentData = nullptr;
        ovrld_log(LogCategory::PAGING, "File {} ran out of pages in CheckPageBoundary",
                  m_FileDef->name.data);
      } else {
        // this is a little weird, but if we went past the end of the previous page, we actually
        // start at an offset into the next page - perhaps the user could know that pages are
        // consecutive in memory?
        uint8_t* new_page_mem = next_page->m_pPageMemStart;
        m_Buffer.m_pCurrentData = new_page_mem + (end_ptr + 1 - past_boundary_ptr);
        m_Buffer.m_pCurrentPageStart = new_page_mem;
        ovrld_log(LogCategory::PAGING, "File {} advanced to next page (wrapped {} bytes)",
                  m_FileDef->name.data, (end_ptr + 1 - past_boundary_ptr));
      }
    }
  }
  return m_Buffer.m_pCurrentData;
}

/*!
 * Allocate pages and set up the CBuffer for the given ISO Msg and type.
 */
int CBaseFile::InitBuffer(CBuffer::BufferType type, jak3::ISO_Hdr* msg) {
  ASSERT(msg);
  m_Buffer.m_pCurrentData = nullptr;
  m_Buffer.m_pPageList = nullptr;
  m_Buffer.m_pIsoCmd = msg;
  m_Buffer.m_pCurrentPageStart = nullptr;
  m_Buffer.m_nDataLength = 0;
  m_Buffer.m_eBufferType = CBuffer::BufferType::EBT_FREE;
  m_Buffer.m_nMinNumPages = 1;
  m_Buffer.m_nMaxNumPages = kDefaultBufferPageCount;
  m_nNumPages = 4;

  // adjust size based on the request buffer type
  switch (type) {
    case CBuffer::BufferType::REQUEST_NORMAL: {  // 3
      m_Buffer.m_pCurrentData = nullptr;
      m_Buffer.m_nMinNumPages = 1;
      m_Buffer.m_nMaxNumPages = 4;
      m_nNumPages = 4;
      m_Buffer.m_nDataLength = 0;
      m_Buffer.m_pCurrentPageStart = nullptr;
      m_Buffer.m_eBufferType = CBuffer::BufferType::NORMAL;  // 1

      // and the iso msg request.
      switch (msg->msg_type) {
        case ISO_Hdr::MsgType::LOAD_EE:
        case ISO_Hdr::MsgType::LOAD_IOP:
        case ISO_Hdr::MsgType::LOAD_EE_CHUNK:
          m_Buffer.m_nMinNumPages = 1;
          m_nNumPages = 4;
          m_Buffer.m_nMaxNumPages = 4;
          break;
        case ISO_Hdr::MsgType::LOAD_SOUNDBANK:
          m_Buffer.m_nMinNumPages = 1;
          m_Buffer.m_nMaxNumPages = 2;
          break;
        default:
          break;
      }

    } break;
    case CBuffer::BufferType::REQUEST_VAG: {
      m_Buffer.m_pCurrentData = nullptr;
      m_Buffer.m_eBufferType = CBuffer::BufferType::VAG;
      m_Buffer.m_nMinNumPages = 1;
      m_nNumPages = 0x10;
      m_Buffer.m_nDataLength = 0;
      m_Buffer.m_pCurrentPageStart = nullptr;
      m_Buffer.m_nMaxNumPages = 0x10;
    } break;
    default:
      ASSERT_NOT_REACHED();  // bad buffer type
  }

  ovrld_log(LogCategory::PAGING, "File {} initializing buffer ({} pages {} min {} max)",
            m_FileDef->name.data, m_nNumPages, m_Buffer.m_nMinNumPages, m_Buffer.m_nMaxNumPages);

  // Actual allocation of page data
  CPageList* page_list = AllocPages();
  if (page_list) {
    // set up the current pointers of the buffer.
    m_Buffer.m_pCurrentPageStart = page_list->m_pFirstPage->m_pPageMemStart;
    m_Buffer.m_pCurrentData = m_Buffer.m_pCurrentPageStart;
    return 1;
  } else {
    ovrld_log(LogCategory::WARN, "File {} failed to allocate a page list.", m_FileDef->name.data);
    // if it failed, terminate the buffer
    TerminateBuffer();
    return 0;
  }
}

/*!
 * Free page memory, clear event flags for cpages.
 */
void CBaseFile::TerminateBuffer() {
  ovrld_log(LogCategory::PAGING, "File {} terminating buffer.", m_FileDef->name.data);

  auto buffer_type = m_Buffer.m_eBufferType;
  if (buffer_type != CBuffer::BufferType::EBT_FREE) {
    // clean up our pages
    auto* page_list = m_Buffer.m_pPageList;
    if (page_list) {
      // stop try to load
      page_list->CancelActivePages();
    }

    switch (buffer_type) {
      case CBuffer::BufferType::EBT_FREE:
      case CBuffer::BufferType::NORMAL:
      case CBuffer::BufferType::VAG:
      case CBuffer::BufferType::REQUEST_NORMAL:
      case CBuffer::BufferType::REQUEST_VAG:
        // return memory
        FreePages();

        // reset our buffer
        m_Buffer.m_nDataLength = 0;
        m_Buffer.m_pCurrentPageStart = 0;
        m_Buffer.m_eBufferType = CBuffer::BufferType::EBT_FREE;

        // reset our command
        if (buffer_type != CBuffer::BufferType::EBT_FREE) {
          ASSERT(m_Buffer.m_pIsoCmd);
          m_Buffer.m_pIsoCmd = nullptr;
        }
        break;
      default:
        ASSERT_NOT_REACHED();  // Invalid buffer type
    }
  } else {
    ASSERT_NOT_REACHED();  // Buffer already terminated, shouldn't happen again.
  }
}

/*!
 * Allocate CPage and CPageList as needed to reach the target number of pages.
 */
CPageList* CBaseFile::AllocPages() {
  // should have picked a buffer type
  ASSERT(m_Buffer.m_eBufferType != CBuffer::BufferType::EBT_FREE);

  if (m_Buffer.m_eBufferType == CBuffer::BufferType::EBT_FREE) {
    return nullptr;
  }

  // We might have a PageList or not
  auto* old_plist = m_Buffer.m_pPageList;
  int old_unstepped_pages = 0;
  bool have_plist = old_plist != nullptr;
  CPageList* ret_plist = nullptr;

  // if we do have a pagelist, we'll reuse it and any unstepped pages.
  if (have_plist) {
    old_unstepped_pages = old_plist->m_nNumUnsteppedPages;
    ret_plist = old_plist;
  }

  int page_alloc_count = 0;

  // to increase unstepped pages to the target m_nNumPages, we need to allocate this many more.
  page_alloc_count = m_nNumPages - old_unstepped_pages;
  // lg::warn("page counts in AllocPages: {} {}", m_nNumPages, old_unstepped_pages);
  if (old_plist) {
    // lg::warn("     {} {}", old_plist->m_nNumPages, old_plist->m_nNumActivePages);
  }

  if (old_unstepped_pages < (int)m_nNumPages) {
    // alloc count will be positive.

    // reduce allocation count to at most the number of free pages
    if (get_page_manager()->m_CCache.m_nNumFreePages < page_alloc_count) {
      page_alloc_count = get_page_manager()->m_CCache.m_nNumFreePages;
    }

  } else {
    // alloc count would be negative, don't allocate
    page_alloc_count = 0;
  }

  switch (m_Buffer.m_eBufferType) {
    case CBuffer::BufferType::NORMAL:
    case CBuffer::BufferType::REQUEST_NORMAL: {
      // don't do anything special
    } break;
    case CBuffer::BufferType::VAG:
    case CBuffer::BufferType::REQUEST_VAG: {
      // for VAG commands, we don't bother buffering more than the DMA transfer size
      int dma_xfer_size = ((ISO_VAGCommand*)m_Buffer.m_pIsoCmd)->xfer_size;
      if (dma_xfer_size) {
        ASSERT(dma_xfer_size > 0);
        int limit = (dma_xfer_size + 0x7fff) >> 0xf;
        if (page_alloc_count > limit) {
          ovrld_log(LogCategory::WARN, "File {} wants {} pages, but VAG DMA sizes limit us to ",
                    m_FileDef->name.data, page_alloc_count, limit);
          lg::info("page count dma limit {} -> {}\n", page_alloc_count, limit);
          page_alloc_count = limit;
        }
      }
    } break;
    default:
      ASSERT_NOT_REACHED();  // bad buffer type.
  }

  if (page_alloc_count == 0) {
    return ret_plist;
  }

  ovrld_log(LogCategory::PAGING, "File {} wants {} more pages in AllocPages.", m_FileDef->name.data,
            page_alloc_count);
  if (have_plist) {
    ret_plist = get_page_manager()->GrowPageList(m_Buffer.m_pPageList, page_alloc_count);
  } else {
    ret_plist = get_page_manager()->AllocPageList(page_alloc_count, 0);
  }

  if (ret_plist) {
    m_Buffer.m_pPageList = ret_plist;
    if (!have_plist) {
      m_Buffer.m_pCurrentData = ret_plist->m_pFirstPage->m_pPageMemStart;
    }
  } else {
    ASSERT_NOT_REACHED();  // might be ok.
  }
  return ret_plist;
}

/*!
 * Free all pages and the page list.
 */
void CBaseFile::FreePages() {
  ASSERT(m_Buffer.m_eBufferType != CBuffer::BufferType::EBT_FREE);
  if (m_Buffer.m_pPageList) {
    get_page_manager()->FreePageList(m_Buffer.m_pPageList);
  }
  m_Buffer.m_pPageList = nullptr;
}

}  // namespace jak3