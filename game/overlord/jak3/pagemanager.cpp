#include "pagemanager.h"

#include <cstring>

#include "common/util/Assert.h"

#include "game/sce/iop.h"

namespace jak3 {
using namespace iop;

static constexpr u32 PAGE_SIZE = 0x8010;
static constexpr u32 NUM_PAGES = 29;

CPageManager::CPage::CPage(u8* start, u8* end, int page_id) {
  m_pNextPage = nullptr;
  m_uPageBit = 1 << (page_id & 0x1f);
  m_nPageState = 0;
  m_pData = start;
  m_pDataEnd = end;
  m_nPageRefCount = 0;
  m_nPageID = page_id;
  m_nPageDmaRefCount = 0;
  m_nAllocState = 0;
  m_nPageState = 0;
}

int CPageManager::CPage::AddRef() {
  int state, ret = -1;

  CpuSuspendIntr(&state);

  if (m_nAllocState == 1 && m_pPageList != nullptr) {
    m_pPageList->m_nListRefCount++;
    m_nPageRefCount++;
    ret = m_nPageRefCount;
  }

  CpuResumeIntr(state);

  return ret;
}

int CPageManager::CPage::ReleaseRef() {
  int state, ret = -1;

  CpuSuspendIntr(&state);

  if (m_nAllocState == 1 && m_pPageList != nullptr) {
    m_pPageList->m_nListRefCount--;
    m_nPageRefCount--;
    ret = m_nPageRefCount;
  }

  CpuResumeIntr(state);

  return ret;
}

int CPageManager::CPage::AddDmaRef() {
  int state, ret = -1;

  CpuSuspendIntr(&state);

  if (m_nAllocState == 1 && m_pPageList != nullptr) {
    m_pPageList->m_nListDmaRefCount++;
    m_nPageDmaRefCount++;
    ret = m_nPageDmaRefCount;
  }

  CpuResumeIntr(state);

  return ret;
}

int CPageManager::CPage::ReleaseDmaRef() {
  int state, ret = -1;

  CpuSuspendIntr(&state);

  if (m_nAllocState == 1 && m_pPageList != nullptr) {
    m_pPageList->m_nListDmaRefCount--;
    m_nPageDmaRefCount--;
    ret = m_nPageDmaRefCount;
  }

  CpuResumeIntr(state);

  return ret;
}

void CPageManager::CPage::FromPagesCopy(u8* pInPageData, u8* pDest, int nNumBytes) {
  int nInputPageLengthLeft;
  CPage* page = this;

  if (nNumBytes <= 0) {
    return;
  }

  for (;;) {
    nInputPageLengthLeft = (uintptr_t)page->m_pDataEnd + (1 - (uintptr_t)pInPageData);
    if (nInputPageLengthLeft <= nNumBytes) {
      break;
    }

    if (nNumBytes <= 0) {
      nNumBytes -= nInputPageLengthLeft;
      return;
    }

    ASSERT(m_pNextPage != nullptr);

    page = page->m_pNextPage;
    pInPageData = page->m_pData;
  }

  memcpy(pDest, pInPageData, nNumBytes);
}

CPageManager::CCache::CCache() {
  m_paCache = nullptr;
  m_uAllocatedPageMap = 0;
  m_Unk = (1 << NUM_PAGES) - 1;
}

int CPageManager::CCache::Initialize() {
  m_paCache = AllocSysMemory(0, PAGE_SIZE * NUM_PAGES, nullptr);
  if (!m_paCache) {
    return 1;
  }

  m_nNumFree = 29;

  for (auto& pl : m_aPageLists) {
    pl = {};
  }

  int page_idx = 0;
  u8* page_start = (u8*)m_paCache;
  for (auto& page : m_aPages) {
    page = CPage(page_start, page_start + 0x7fff, page_idx);

    page_start += PAGE_SIZE;
    page_idx++;
  }

  return 0;
}

void CPageManager::Initialize() {
  m_Cache.Initialize();
}

int CPageManager::TryFreePages(int nPages) {
  return 0;
}

}  // namespace jak3
