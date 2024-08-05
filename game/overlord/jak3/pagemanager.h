#pragma once

#include "common/common_types.h"

namespace jak3 {
void jak3_overlord_init_globals_pagemanager();

/*!
 * CPages Overview
 *
 * Data is read from the DVD driver into CPages. The CPages are then given to consumers.
 * Each file that's opened has an associated CPageList.
 *
 * The CPageList is a linked list of pages. Within this linked list, there is a section of "active
 * pages" which have been filled by the DVD driver. There is also the "current active page", which
 * is the page that the user will read from next. Note that pages before the current active page
 * and still be referenced by the user, but they should keep a nonzero reference count so the CPage
 * is not Garbage Collected.
 *
 * By default, each active page will have a ref count of 1.
 *
 * The CBuffer object owned by CBaseFile uses memory managed by this CPage system.
 *
 * The pages are preallocated by the CPageManager, which gives out pages as needed.
 */

struct CPage;
struct CPageList;
struct ISO_Hdr;

constexpr int kPageSize = 0x8000;
constexpr int kPageStride = 0x8010;

/*!
 * A CBuffer stores file data in pages, and tracks the progress through the page data as it is fed
 * into the ISO data handling system.
 */
struct CBuffer {
  // The pages in this buffer are managed by a PageList
  CPageList* m_pPageList = nullptr;

  // The ISO command that requested us to load this data
  ISO_Hdr* m_pIsoCmd = nullptr;

  // Current progress through the data (todo: load or process?)
  uint8_t* m_pCurrentData = nullptr;

  // First address in the current page
  uint8_t* m_pCurrentPageStart = nullptr;

  // todo
  int m_nDataLength = 0;

  enum class BufferType {
    // this is a really confusing enum...
    EBT_FREE = 0,        // there is no buffer allocate
    NORMAL = 1,          // a buffer sized for non-VAG stream operations (several possible sizes)
    VAG = 2,             // a buffer size for VAG streams (larger than normal sizes)
    REQUEST_NORMAL = 3,  // argument passed to InitBuffer to get a normal buffer
    REQUEST_VAG = 4,     // argument passed to InitBuffer to get a VAG size buffer
  } m_eBufferType = BufferType::EBT_FREE;

  // Try to have at least this many pages filled
  int m_nMinNumPages = 0;

  // Don't fill more than this number of pages
  int m_nMaxNumPages = 0;

  void AddData(int len);
  void AdvanceCurrentData(int len);
};

/*!
 * List of pages for a file.
 */
struct CPageList {
  CPageList() = default;  // ???

  // list of all pages
  CPage* m_pFirstPage = nullptr;
  CPage* m_pLastPage = nullptr;

  // list of active pages. This is part of the all page list
  CPage* m_pFirstActivePage = nullptr;
  CPage* m_pLastActivePage = nullptr;

  // page that the application is currently reading from
  CPage* m_pCurrentActivePage = nullptr;

  // total number of CPage, including both active/inactive
  int m_nNumPages = 0;

  // number of cpages in the active page list
  int m_nNumActivePages = 0;

  // number of CPages remaining for the user, including the current active page
  int m_nNumUnsteppedPages = 0;

  // Reference counters to know if this data is still needed or not.
  int m_nPageRefCnt = 0;
  int m_nDmaRefCnt = 0;

  enum class AllocState {
    EPLAS_FREE = 0,
    EPLAS_ALLOCATED = 1,
    FREE_PENDING = 2,  // FreePageList called, but no
  } m_nAllocState = AllocState::EPLAS_FREE;

  CPage* StepActivePage();
  CPage* AddActivePages(int num_pages);
  int RemoveActivePages(int num_pages);
  void CancelActivePages();
  void GarbageCollect();
};

/*!
 * A single page, pointing to a contiguous buffer of file data.
 */
struct CPage {
  CPage(uint8_t* page_mem_start, uint8_t* page_mem_end, int page_idx);
  CPage() = default;  // ???
  CPage* m_pNextPage = nullptr;
  CPage* m_pPrevPage = nullptr;
  CPageList* m_pPageList = nullptr;
  int m_nPageRefCount = 0;
  int m_nDmaRefCount = 0;
  int m_nAllocState = 0;
  enum class State {
    UNMAKRED = 0,
    ACTIVE = 1,
    READING = 2,
    READ_DONE = 3
  } input_state = State::UNMAKRED;
  uint8_t* m_pPageMemStart = nullptr;
  uint8_t* m_pPageMemEnd = nullptr;
  u32 m_nPageIdx = 0;
  u32 mask = 0;

  int AddRef();
  int ReleaseRef();
  int AddDmaRef();
  int ReleaseDmaRef();
  void FromPagesCopy(uint8_t* in, uint8_t* dest, s32 size);
};

/*!
 * CCache contains the actual CPage/CPageList objects to be given out to files.
 */
struct CCache {
  static constexpr int kNumPages = 29;
  static constexpr int kNumPageLists = 29;
  static constexpr int kAllPagesMask = (1 << kNumPages) - 1;

  CCache();
  void Initialize();
  void* m_paCache = nullptr;
  CPageList m_PageLists[kNumPageLists];
  CPage m_Pages[kNumPages];
  int m_nNumFreePages = 0;
  u32 m_nAllocatedMask;
  u32 m_nPagelistAllocatedMask;
  int m_PagesFilledEventFlag;
  int m_PendingMask;
};

struct CPageManager {
  CCache m_CCache;

  CPageList* GrowPageList(CPageList* in, int page_count);
  CPageList* AllocPageList(int count, bool flag);
  CPageList* AllocPageListBytes(int bytes, bool flag);
  void FreePageList(CPageList* list);
  void WaitForPagesFilled(u32 mask);
  void Initialize();
  int RecoverPages(int k);
  void GarbageCollect();
  void GarbageCollectPageList(CPageList* list);
};

CPageManager* get_page_manager();
}  // namespace jak3