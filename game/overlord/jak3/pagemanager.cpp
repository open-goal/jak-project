#include "pagemanager.h"

#include <cstdlib>
#include <cstring>

#include "common/log/log.h"
#include "common/util/Assert.h"

#include "game/overlord/jak3/overlord.h"
#include "game/sce/iop.h"

using namespace iop;

namespace jak3 {
namespace {
std::unique_ptr<CPageManager> g_manager;
}
void jak3_overlord_init_globals_pagemanager() {
  g_manager = std::make_unique<CPageManager>();
}

CPageManager* get_page_manager() {
  return g_manager.get();
}

/*!
 * Add the given number of pages to the active list, returning the first in the active list.
 * This should be called before writing to the pages.
 * This grows the active list from the end and adds a reference count.
 */
CPage* CPageList::AddActivePages(int num_pages) {
  ASSERT(m_nAllocState == AllocState::EPLAS_ALLOCATED);
  ASSERT(num_pages > 0);  // game warned on this, might be ok to just return null

  if (m_nNumPages < num_pages + m_nNumActivePages) {
    // the original game just gave you no pages, but that seems sus, so abort for now.
    ASSERT_NOT_REACHED();
  }

  // pick the page to convert from non-active to active
  CPage* first_to_convert;
  if (m_pLastActivePage) {
    // if we have active pages, go to the page after that
    first_to_convert = m_pLastActivePage->m_pNextPage;
  } else {
    // otherwise, start at the beginning of the page list.
    first_to_convert = m_pFirstPage;
  }

  if (first_to_convert) {
    // the first active page should stay the same if we have one
    CPage* new_first_active = m_pFirstActivePage;
    if (!new_first_active) {
      // but if we don't, it'll be the first we convert.
      new_first_active = first_to_convert;
    }

    int page_idx = 0;                      // how many we've done
    CPage* last_converted = nullptr;       // last one that was finished
    CPage* to_convert = first_to_convert;  // next one to convert
    if (0 < num_pages) {
      // loop over pages and convert them!!
      do {
        ASSERT(to_convert->input_state == CPage::State::UNMAKRED);
        last_converted = to_convert;
        last_converted->input_state = CPage::State::ACTIVE;
        ClearEventFlag(get_page_manager()->m_CCache.m_PagesFilledEventFlag, ~last_converted->mask);
        int status = last_converted->AddRef();
        ASSERT(status >= 1);
        page_idx = page_idx + 1;
        if (!last_converted->m_pNextPage)
          break;
        to_convert = last_converted->m_pNextPage;
      } while (page_idx < num_pages);
    }
    if (page_idx == num_pages) {
      // success! all converted.
      // CpuSuspendIntr(local_28);
      // update the active range and count
      m_pFirstActivePage = new_first_active;
      m_pLastActivePage = last_converted;
      m_nNumActivePages = m_nNumActivePages + page_idx;
      m_nNumUnsteppedPages = m_nNumUnsteppedPages + page_idx;
      if (!m_pCurrentActivePage) {
        m_pCurrentActivePage = first_to_convert;
      }
      // CpuResumeIntr(local_28[0]);
      return first_to_convert;
    } else {
      // darn, didn't have enough. undo what we did.
      // Not really sure that we need to clear the even flag again.
      while (0 < page_idx) {
        new_first_active->input_state = CPage::State::UNMAKRED;
        ClearEventFlag(get_page_manager()->m_CCache.m_PagesFilledEventFlag,
                       ~new_first_active->mask);
        new_first_active->ReleaseRef();
      }
      return nullptr;
    }
  }
  return nullptr;
}

/*!
 * Try to remove count pages from the active list, starting at the end and working back.
 * This will remove buffered that the user has not yet read. The data will have to be read from the
 * DVD again, so this should only be used when more free pages are absolutely needed.
 *
 * This may remove fewer than requested. It will not remove the current active page, and always
 * leaves at least 1 active page to avoid removing a page that's currently being used.
 */
int CPageList::RemoveActivePages(int count) {
  // lg::error("Remove Active Pages {}", count);
  int num_removed = 0;
  ASSERT(m_nAllocState == AllocState::EPLAS_ALLOCATED);

  // CpuSuspendIntr(local_28);
  num_removed = 0;

  // only attempt if we have more than 1 active
  if (count > 0 && m_nNumActivePages > 1) {
    CPage* last_active = m_pLastActivePage;
    CPage* current_active = m_pCurrentActivePage;

    // I'm not sure what the last->next = current check is actually doing.
    // this check is added - I have no idea how this could happen, or why the game avoids removing
    // pages in this case. Hopefully we don't hit it.
    if (last_active) {
      ASSERT(last_active->m_pNextPage != current_active);
    }

    if (current_active && last_active && last_active->m_pNextPage != current_active) {
      // assume we remove them all except 1.
      num_removed = m_nNumActivePages + -1;
      // but if that's more than we asked for, reduce.
      if (count < num_removed) {
        num_removed = count;
      }

      // iterate backward to find the first to remove, stopping if we reach current_active, or we
      // exceed the count to remove.
      int num_pages_back = 0;
      CPage* iter = last_active;
      if (last_active != current_active && 0 < num_removed) {
        do {
          iter = iter->m_pPrevPage;
          num_pages_back = num_pages_back + 1;
          if (!iter) {
            ASSERT_NOT_REACHED();
          }
        } while (iter != current_active && num_pages_back < num_removed);

        if (0 < num_pages_back) {
          // now, iterate forward and convert to inactive.
          // go one forward, since the last loop terminated on the page after the last one we want.
          CPage* fwd_iter = iter->m_pNextPage;

          // the last one that stays active is one after that
          m_pLastActivePage = fwd_iter->m_pPrevPage;
          m_nNumActivePages = m_nNumActivePages - num_pages_back;
          m_nNumUnsteppedPages = m_nNumUnsteppedPages - num_pages_back;

          // the end of the conversion loop
          CPage* end_iter = last_active->m_pNextPage;
          num_removed = num_pages_back;
          while (fwd_iter && fwd_iter != end_iter) {
            fwd_iter->input_state = CPage::State::UNMAKRED;
            ClearEventFlag(get_page_manager()->m_CCache.m_PagesFilledEventFlag, ~fwd_iter->mask);
            fwd_iter->ReleaseRef();
            fwd_iter = fwd_iter->m_pNextPage;
          }
        } else {
          // in this case, I think we return the wrong number of removed pages.
          ASSERT_NOT_REACHED();
        }
      }
    }
  }
  // CpuResumeIntr(local_28[0]);
  return num_removed;
}

/*!
 * Remove all active pages.
 */
void CPageList::CancelActivePages() {
  ASSERT(m_nAllocState == AllocState::EPLAS_ALLOCATED);
  // CpuSuspendIntr(local_18);
  CPage* last_active = m_pLastActivePage;
  CPage* iter = m_pCurrentActivePage;
  // note: keep the last active page pointing at the right point in the ring to allocate.
  m_pLastActivePage = m_pCurrentActivePage;
  m_pFirstActivePage = nullptr;
  m_pCurrentActivePage = nullptr;
  m_nNumActivePages = 0;
  m_nNumUnsteppedPages = 0;
  if (iter && last_active && last_active->m_pNextPage != iter) {
    CPage* end = last_active->m_pNextPage;
    do {
      if (iter == end)
        break;
      iter->input_state = CPage::State::UNMAKRED;
      ClearEventFlag(get_page_manager()->m_CCache.m_PagesFilledEventFlag, ~iter->mask);
      iter->ReleaseRef();
      iter = iter->m_pNextPage;
    } while (iter);
  }
  // CpuResumeIntr(local_18[0]);
}

/*!
 * Step the current active page forward. This will release the reference count added when the page
 * became active. If no other references were added, this page may be Garbage Collected at any time.
 */
CPage* CPageList::StepActivePage() {
  ASSERT(m_nAllocState == AllocState::EPLAS_ALLOCATED);

  CPage* new_current_active = nullptr;
  // CpuSuspendIntr(local_18);
  auto* current_active = m_pCurrentActivePage;
  if (current_active && m_pLastActivePage && m_pLastActivePage->m_pNextPage != current_active) {
    ASSERT(m_nNumActivePages > 0);
    m_nNumUnsteppedPages = m_nNumUnsteppedPages + -1;
    current_active->ReleaseRef();
    current_active->input_state = CPage::State::UNMAKRED;
    ClearEventFlag(get_page_manager()->m_CCache.m_PagesFilledEventFlag, ~current_active->mask);
    new_current_active = current_active->m_pNextPage;
    ASSERT(new_current_active != m_pCurrentActivePage);
    m_pCurrentActivePage = new_current_active;
  } else {
    ASSERT_NOT_REACHED();  // step past end of active warning, seems bad.
  }
  // CpuResumeIntr(local_18[0]);
  return new_current_active;
}

/*!
 * Remove pages that are no longer needed. They will be sent back to the Page Manager.
 */
void CPageList::GarbageCollect() {
  ovrld_log(LogCategory::PAGING, "[paging] Garbage collecting, currently have {} pages, {} active",
            m_nNumPages, m_nNumActivePages);

  //  for (auto* p = m_pFirstPage; p; p = p->m_pNextPage) {
  //    ovrld_log(LogCategory::PAGING,
  //              "page 0x{:x}, first active? {} last active? {} current active? {} last? {}",
  //              p->m_nPageIdx, p == m_pFirstActivePage, p == m_pLastActivePage,
  //              p == m_pCurrentActivePage, p == m_pLastPage);
  //  }
  // trim pages at the front. Anything unreferenced before the current active page is ok to clean.
  CPage* page = m_pFirstPage;
  if (page && page != m_pCurrentActivePage) {
    ASSERT(page->m_nAllocState == 1);  // pages in CPageLists should always be allocated.

    while (page->m_nPageRefCount == 0 && page->m_nDmaRefCount == 0) {  // only unref'd pages.
      ASSERT(page->m_nAllocState == 1);                                // prior to active.
      CPage* next_page = page->m_pNextPage;
      // CpuSuspendIntr(&local_18);
      m_nNumPages = m_nNumPages + -1;
      // pop page from our normal list
      m_pFirstPage = next_page;
      if (m_pLastPage == page) {
        m_pLastPage = nullptr;
        // sanity check - we just killed our last page from the list, count should be 0
        ASSERT(m_nNumPages == 0);
      }

      // maintain active list too
      if (m_pFirstActivePage == page) {
        m_nNumActivePages = m_nNumActivePages + -1;
        if (page == m_pLastActivePage) {
          // since we're getting rid of this page from our list, don't keep a ref to it.
          m_pFirstActivePage = nullptr;
          m_pLastActivePage = nullptr;
          ASSERT(m_nNumActivePages == 0);  // sanity check count.
        } else {
          m_pFirstActivePage = next_page;
        }
      }
      if (m_pLastActivePage == page) {
        m_pLastActivePage = nullptr;
      }
      if (next_page) {
        next_page->m_pPrevPage = nullptr;
      }
      // CpuResumeIntr(local_18);

      // now clean the page itself
      ovrld_log(LogCategory::PAGING, "[paging] GC took page 0x{:x} (fwd)", page->m_nPageIdx);
      page->m_pPageList = nullptr;
      page->m_pPrevPage = nullptr;
      page->m_pNextPage = nullptr;
      page->m_nAllocState = 0;
      page->input_state = CPage::State::UNMAKRED;
      ClearEventFlag(get_page_manager()->m_CCache.m_PagesFilledEventFlag, ~page->mask);
      int page_idx = page - get_page_manager()->m_CCache.m_Pages;
      if (page_idx < 0x1d) {
        get_page_manager()->m_CCache.m_nAllocatedMask &= ~(1 << (page_idx & 0x1f));
      } else {
        ASSERT_NOT_REACHED();  // idk
      }
      get_page_manager()->m_CCache.m_nNumFreePages++;
      if (!next_page) {
        break;
      }
      page = next_page;
      if (page == m_pCurrentActivePage) {
        break;
      }
    }
  }

  // now, start at the end and work backward. We'll stop once we reach the last active page, since
  // we expect everything before that to have a nonzero ref count.
  page = m_pLastPage;
  if (page && page != m_pLastActivePage && page != m_pCurrentActivePage) {
    ASSERT(page->m_nAllocState == 1);
    while ((page->m_nPageRefCount == 0 && (page->m_nDmaRefCount == 0))) {
      ovrld_log(LogCategory::PAGING, "[paging] GC took page 0x{:x} (bwd)", page->m_nPageIdx);
      ASSERT(page->m_nAllocState == 1);
      CPage* prev = page->m_pPrevPage;
      // CpuSuspendIntr(&local_14);
      m_nNumPages = m_nNumPages + -1;
      m_pLastPage = prev;
      if (m_pFirstPage == page) {
        m_pFirstPage = nullptr;
        ASSERT(m_nNumPages == 0);
      }
      if (prev != (CPage*)0x0) {
        prev->m_pNextPage = (CPage*)0x0;
      }
      // CpuResumeIntr(local_14);
      page->m_pPageList = nullptr;
      page->m_pPrevPage = nullptr;
      page->m_pNextPage = nullptr;
      page->m_nAllocState = 0;
      page->input_state = CPage::State::UNMAKRED;
      ClearEventFlag(get_page_manager()->m_CCache.m_PagesFilledEventFlag, ~page->mask);
      int page_idx = page - get_page_manager()->m_CCache.m_Pages;
      if (page_idx < 0x1d) {
        get_page_manager()->m_CCache.m_nAllocatedMask &= ~(1 << (page_idx & 0x1f));
      } else {
        ASSERT_NOT_REACHED();
      }
      get_page_manager()->m_CCache.m_nNumFreePages++;

      if (!prev) {
        return;
      }
      if (prev == m_pLastActivePage) {
        return;
      }
      page = prev;
      if (prev == m_pCurrentActivePage) {
        return;
      }
    }
  }

  ovrld_log(LogCategory::PAGING,
            "[paging] Done Garbage collecting, currently have {} pages, {} active in 0x{:x}",
            m_nNumPages, m_nNumActivePages, (u64)this);
}

/*!
 * Wait on the pages indicated by the mask.
 */
void CPageManager::WaitForPagesFilled(u32 mask) {
  if ((mask & m_CCache.m_PendingMask) != 0) {
    WaitEventFlag(m_CCache.m_PagesFilledEventFlag, mask, 0);
  } else {
    // waiting for something that's not pending... might be ok. was a warning.
    ASSERT_NOT_REACHED();
  }
}

CPage::CPage(uint8_t* page_mem_start, uint8_t* page_mem_end, int page_idx) {
  m_pNextPage = nullptr;
  mask = 1 << (page_idx & 0x1f);
  m_pPrevPage = nullptr;
  m_pPageMemStart = page_mem_start;
  m_pPageList = nullptr;
  m_pPageMemEnd = page_mem_end;
  m_nPageRefCount = 0;
  m_nPageIdx = page_idx;
  m_nDmaRefCount = 0;
  m_nAllocState = 0;
  input_state = State::UNMAKRED;
}

/*!
 * Add one to this CPage's reference count, preventing it from being garbage collected
 */
int CPage::AddRef() {
  // CpuSuspendIntr(local_18);
  auto* page_list = m_pPageList;
  int ret = -1;
  ASSERT(page_list);
  ASSERT(m_nAllocState == 1);
  if (m_nAllocState == 1 && page_list) {
    page_list->m_nPageRefCnt = page_list->m_nPageRefCnt + 1;
    m_nPageRefCount = m_nPageRefCount + 1;
    ret = m_nPageRefCount;
  }
  // CpuResumeIntr(local_18[0]);
  return ret;
}

/*!
 * Subtract one from this CPage's reference count.
 */
int CPage::ReleaseRef() {
  // CpuSuspendIntr(local_18);
  auto* page_list = m_pPageList;
  int ret = -1;
  ASSERT(page_list);
  ASSERT(m_nAllocState == 1);
  if (m_nAllocState == 1 && page_list) {
    page_list->m_nPageRefCnt = page_list->m_nPageRefCnt - 1;
    m_nPageRefCount = m_nPageRefCount - 1;
    ret = m_nPageRefCount;
    ASSERT(ret >= 0);
  }
  // CpuResumeIntr(local_18[0]);
  return ret;
}

/*!
 * Add one to the DMA reference count of this page
 */
int CPage::AddDmaRef() {
  // CpuSuspendIntr(local_18);
  auto* page_list = m_pPageList;
  int ret = -1;
  ASSERT(page_list);
  ASSERT(m_nAllocState == 1);
  if (m_nAllocState == 1 && page_list) {
    page_list->m_nDmaRefCnt = page_list->m_nDmaRefCnt + 1;
    m_nDmaRefCount = m_nDmaRefCount + 1;
    ret = m_nPageRefCount;
  }
  // CpuResumeIntr(local_18[0]);
  return ret;
}

int CPage::ReleaseDmaRef() {
  // CpuSuspendIntr(local_18);
  auto* page_list = m_pPageList;
  int ret = -1;
  ASSERT(page_list);
  ASSERT(m_nAllocState == 1);
  if (m_nAllocState == 1 && page_list) {
    page_list->m_nDmaRefCnt = page_list->m_nDmaRefCnt - 1;
    m_nDmaRefCount = m_nDmaRefCount - 1;
    ret = m_nPageRefCount;
    ASSERT(ret >= 0);
  }
  // CpuResumeIntr(local_18[0]);
  return ret;
}

/*!
 * Copy data from this page to destination. This works with sizes that are greater than the page
 * size, and will look at future pages. However, it does not actually advance progress in the page.
 */
void CPage::FromPagesCopy(uint8_t* in, uint8_t* dest, s32 size) {
  ASSERT(in);
  ASSERT(dest);
  ASSERT(size >= 0);
  CPage* page = this;
  if (0 < size) {
    while (true) {
      s32 input_page_left = (page->m_pPageMemEnd - in) + 1;
      ASSERT(input_page_left >= 0);
      if (size < input_page_left)
        break;
      size -= input_page_left;
      memcpy(dest, in, input_page_left);
      dest += input_page_left;
      if (size == 0) {
        return;
      }
      ASSERT(size > 0);
      ASSERT(page->m_pNextPage);
      page = page->m_pNextPage;
      in = page->m_pPageMemStart;
    }
    memcpy(dest, in, size);
  }
}

CCache::CCache() {
  m_PendingMask = kAllPagesMask;
  m_PagesFilledEventFlag = -1;
  m_nNumFreePages = 0;
  m_nAllocatedMask = 0;
  m_nPagelistAllocatedMask = 0;
}

void CCache::Initialize() {
  static_assert(0xe81d0 == kPageStride * kNumPages);
  m_paCache = AllocSysMemory(0, kPageStride * kNumPages, nullptr);
  ASSERT(m_paCache);
  m_nNumFreePages = kNumPages;

  // initialize pageslists
  for (auto& page_list : m_PageLists) {
    page_list.m_pFirstActivePage = nullptr;
    page_list.m_pLastActivePage = nullptr;
    page_list.m_pCurrentActivePage = nullptr;

    page_list.m_pFirstPage = nullptr;
    page_list.m_pLastPage = nullptr;
    page_list.m_nNumActivePages = 0;
    page_list.m_nNumUnsteppedPages = 0;
    page_list.m_nPageRefCnt = 0;
    page_list.m_nDmaRefCnt = 0;
    page_list.m_nAllocState = CPageList::AllocState::EPLAS_FREE;
  }

  u8* mem = (u8*)m_paCache;
  for (int i = 0; i < kNumPages; i++) {
    m_Pages[i] = CPage(mem, mem + kPageSize - 1, i);
    // interestingly, the stride is a bit longer.
    mem += kPageStride;
  }

  m_nAllocatedMask = 0;
  m_nPagelistAllocatedMask = 0;

  EventFlagParam param;
  param.attr = 2;
  param.option = 0;
  param.init_pattern = 0;
  m_PagesFilledEventFlag = CreateEventFlag(&param);  // TODO args here
  ASSERT(m_PagesFilledEventFlag >= 0);
}

/*!
 * Increase the length by the given amount. This is used for the DVD reading side to inform the
 * consumer that there is more data available.
 */
void CBuffer::AddData(int len) {
  // suspend interrupts
  m_nDataLength += len;
  // resume interrupts
}

/*!
 * Advance the current point in the buffer. This is used by the consume to mark forward progress.
 */
void CBuffer::AdvanceCurrentData(int len) {
  // suspend interrupts
  m_nDataLength -= len;
  m_pCurrentData += len;
  // resume interrupts
}

/*!
 * Set up pages.
 */
void CPageManager::Initialize() {
  m_CCache.Initialize();
}

CPageList* CPageManager::AllocPageListBytes(int bytes, bool flag) {
  return AllocPageList((bytes + kPageSize - 1) / kPageSize, flag);
}

s32 alloc_bitmask(u32* mask, u32 length, u32 start = 0) {
  for (u32 i = start; i < length; i++) {
    if ((*mask & (1 << i)) == 0) {
      // it's free!
      (*mask) |= (1 << i);
      return i;
    }
  }
  return -1;
}

/*!
 * Allocate a PageList with the given number of pages.
 */
CPageList* CPageManager::AllocPageList(int count, bool consecutive_pages) {
  ASSERT(count > 0);
  ASSERT(count <= CCache::kNumPages);

  if (count > m_CCache.m_nNumFreePages) {
    // if we're out of pages, use RecoverPages to discard pages that we've already read, but
    // nobody is using yet.  We'll be able to read them from the DVD again.
    lg::warn("Recovering pages - {} requested in AllocPageList, but only {} available", count,
             m_CCache.m_nNumFreePages);
    RecoverPages(count);
    ASSERT(m_CCache.m_nNumFreePages >= count);
  }

  // next, find a pagelist. the original game had some fancy bit magic here, but this is simpler
  int plist_idx = alloc_bitmask(&m_CCache.m_nPagelistAllocatedMask, CCache::kNumPageLists);
  ASSERT(plist_idx >= 0);
  CPageList* plist = &m_CCache.m_PageLists[plist_idx];

  // Fill this array with allocated pages
  CPage* pages[CCache::kNumPages];
  int pages_allocated = 0;
  int last_page_allocated = -1;
  int next_page_to_check = 0;

  while (pages_allocated < count) {
    if (next_page_to_check >= CCache::kNumPages) {
      break;
    }
    // grab the next page
    int page_idx = alloc_bitmask(&m_CCache.m_nAllocatedMask, CCache::kNumPages, next_page_to_check);
    ASSERT(page_idx >= 0);

    // start after this page on the next search
    next_page_to_check = page_idx + 1;

    pages[pages_allocated] = &m_CCache.m_Pages[page_idx];
    pages_allocated++;

    // if we asked for consecutive pages, but didn't get them, we need to rewind our progress.
    // but, we shouldn't rewind next_page_to_check!
    if (consecutive_pages && last_page_allocated != -1 && last_page_allocated + 1 != page_idx) {
      page_idx = -1;
      while (pages_allocated) {
        pages_allocated--;

        u32 i = pages[pages_allocated] - m_CCache.m_Pages;
        ASSERT(i >= 0 && i < CCache::kNumPages);
        m_CCache.m_nAllocatedMask &= ~(1 << i);
      }
    }

    last_page_allocated = page_idx;
  }

  if (pages_allocated != count) {
    // allocation failed
    ASSERT_NOT_REACHED();
  }

  m_CCache.m_nNumFreePages -= count;
  ASSERT(m_CCache.m_nNumFreePages >= 0);

  // zero everything
  plist->m_pFirstPage = nullptr;
  plist->m_pLastPage = nullptr;
  plist->m_pFirstActivePage = nullptr;
  plist->m_pLastActivePage = nullptr;
  plist->m_pCurrentActivePage = nullptr;
  plist->m_nNumPages = 0;
  plist->m_nNumActivePages = 0;
  plist->m_nNumUnsteppedPages = 0;
  ASSERT(plist->m_nPageRefCnt == 0);
  ASSERT(plist->m_nDmaRefCnt == 0);

  plist->m_nAllocState = CPageList::AllocState::EPLAS_ALLOCATED;

  // set up the pages
  CPage* prev = nullptr;
  CPage* page = nullptr;
  for (int i = 0; i < pages_allocated; i++) {
    page = pages[i];
    page->m_pPageList = plist;
    page->m_pPrevPage = prev;
    if (prev) {
      prev->m_pNextPage = page;
    }
    page->m_nAllocState = 1;
    page->input_state = CPage::State::UNMAKRED;
    ClearEventFlag(get_page_manager()->m_CCache.m_PagesFilledEventFlag, ~page->mask);
    prev = page;
  }
  page->m_pNextPage = nullptr;
  plist->m_nNumPages = count;
  plist->m_pLastPage = page;
  plist->m_pFirstPage = pages[0];
  return plist;
}

/*!
 * Allocate page_count more pages, add them to the end of the list. This can be used to get more
 * pages for the DVD driver to write to.
 */
CPageList* CPageManager::GrowPageList(jak3::CPageList* in, int page_count) {
  ASSERT(in);
  ASSERT(in->m_nAllocState == CPageList::AllocState::EPLAS_ALLOCATED);
  ASSERT(page_count >= 0);

  if (page_count > m_CCache.m_nNumFreePages) {
    // if we're out of pages, use RecoverPages to discard pages that we've already read, but
    // nobody is using yet.  We'll be able to read them from the DVD again.
    lg::warn("Recovering pages - {} requested in AllocPageList, but only {} available", page_count,
             m_CCache.m_nNumFreePages);
    RecoverPages(page_count);
    ASSERT(m_CCache.m_nNumFreePages >= page_count);
  }

  CPage* pages[CCache::kNumPages];
  int next_page_to_check = 0;
  int pages_allocated = 0;
  while (pages_allocated != page_count) {
    if (next_page_to_check >= CCache::kNumPages) {
      break;
    }
    int page_idx = alloc_bitmask(&m_CCache.m_nAllocatedMask, CCache::kNumPages, next_page_to_check);
    ASSERT(page_idx >= 0);  // otherwise need to handle this better
    next_page_to_check = page_idx + 1;
    pages[pages_allocated] = &m_CCache.m_Pages[page_idx];
    pages_allocated++;
  }

  if (pages_allocated != page_count) {
    ASSERT_NOT_REACHED();
  }

  m_CCache.m_nNumFreePages -= pages_allocated;

  // set up the pages
  CPage* prev = nullptr;
  CPage* page = nullptr;
  for (int i = 0; i < pages_allocated; i++) {
    page = pages[i];
    ASSERT(page);
    ASSERT(page->m_nAllocState == 0);
    page->m_pPageList = in;
    page->m_pPrevPage = prev;
    if (prev) {
      prev->m_pNextPage = page;
    }
    page->m_nAllocState = 1;
    page->input_state = CPage::State::UNMAKRED;
    ClearEventFlag(get_page_manager()->m_CCache.m_PagesFilledEventFlag, ~page->mask);
    prev = page;
  }
  page->m_pNextPage = nullptr;

  // suspendintr
  if (in->m_nNumPages == 0) {
    ASSERT(!in->m_pFirstPage);
    ASSERT(!in->m_pLastPage);
    ASSERT(!in->m_pFirstActivePage);
    ASSERT(!in->m_pLastActivePage);
    ASSERT(!in->m_pCurrentActivePage);
    in->m_pFirstPage = pages[0];
  } else {
    auto* old_end = in->m_pLastPage;
    ASSERT(!old_end->m_pNextPage);
    old_end->m_pNextPage = pages[0];
    pages[0]->m_pPrevPage = old_end;
  }
  in->m_pLastPage = page;
  in->m_nNumPages += pages_allocated;
  // resume intr
  return in;
}

/*!
 * Return a PageList and its pages to the CCache. If the PageList is still referenced, the freeing
 * will be deferred until GC.
 */
void CPageManager::FreePageList(jak3::CPageList* list) {
  ASSERT(list);
  ASSERT(list->m_nAllocState != CPageList::AllocState::EPLAS_FREE);
  // suspend itr
  list->m_nAllocState = CPageList::AllocState::FREE_PENDING;
  // resume intr

  if (list->m_nDmaRefCnt || list->m_nPageRefCnt) {
    lg::warn("Skipping free since list is referenced!");
    return;
  }

  // loop over pages, clearing them.
  CPage* page = list->m_pFirstPage;
  int pages_count = 0;
  int pages[CCache::kNumPages];

  while (page) {
    u32 page_slot = page - m_CCache.m_Pages;
    ASSERT(page_slot < CCache::kNumPages);
    pages[pages_count] = page_slot;
    pages_count++;
    auto* next = page->m_pNextPage;
    page->m_pPageList = nullptr;
    page->m_pPrevPage = nullptr;
    page->m_pNextPage = nullptr;
    page->m_nAllocState = 0;
    page->input_state = CPage::State::UNMAKRED;
    ASSERT(!page->m_nPageRefCount);
    ASSERT(!page->m_nDmaRefCount);
    ClearEventFlag(get_page_manager()->m_CCache.m_PagesFilledEventFlag, ~page->mask);
    page = next;
  }

  // clear list
  if (pages_count != list->m_nNumPages) {
    lg::die("Paging error: found {} pages out of {} in FreePageList", pages_count,
            list->m_nNumPages);
  }
  ASSERT(pages_count == list->m_nNumPages);
  list->m_pFirstActivePage = nullptr;
  list->m_nNumPages = 0;
  list->m_pLastActivePage = nullptr;
  list->m_pCurrentActivePage = nullptr;
  list->m_nNumActivePages = 0;
  list->m_nNumUnsteppedPages = 0;
  list->m_nAllocState = CPageList::AllocState::EPLAS_FREE;
  m_CCache.m_nNumFreePages += pages_count;
  ASSERT(m_CCache.m_nNumFreePages <= CCache::kNumPages);
  list->m_pFirstPage = nullptr;
  list->m_pLastPage = nullptr;
  // unset bits in allocated mask
  while (0 < pages_count) {
    pages_count--;
    m_CCache.m_nAllocatedMask &= ~(1 << pages[pages_count]);
  }
  // unset bit for the pagelist itself
  u32 plist_idx = list - m_CCache.m_PageLists;
  ASSERT(plist_idx >= 0 && plist_idx < CCache::kNumPageLists);
  m_CCache.m_nPagelistAllocatedMask &= ~(1 << plist_idx);
}

int CPageManager::RecoverPages(int) {
  ASSERT_NOT_REACHED();  // TODO, this looks at pristack
}

/*!
 * Call GarbageCollect on all allocate CPageLists.
 */
void CPageManager::GarbageCollect() {
  for (u32 i = 0; i < CCache::kNumPageLists; i++) {
    if (m_CCache.m_nPagelistAllocatedMask & (1 << i)) {
      GarbageCollectPageList(&m_CCache.m_PageLists[i]);
    }
  }
}

/*!
 * Do garbage collection of pages and page lists.
 */
void CPageManager::GarbageCollectPageList(jak3::CPageList* list) {
  ASSERT(list);
  list->GarbageCollect();
  // if we tried to free the list in the past, but failed, try to do it again now.
  if (list->m_nAllocState == CPageList::AllocState::FREE_PENDING) {
    FreePageList(list);
  }
}

}  // namespace jak3