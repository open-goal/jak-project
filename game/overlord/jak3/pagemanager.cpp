#include "pagemanager.h"

#include "game/sce/iop.h"

namespace jak3 {
using namespace iop;

static constexpr u32 PAGE_SIZE = 0x8010;
static constexpr u32 NUM_PAGES = 29;

static inline int ctz(u32 x) {
  // Isolate lowest bit and subtract 1
  x = (x & -x) - 1;

  // popcount
  x -= (x >> 1) & 0x55555555;
  x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
  x = (x + (x >> 4)) & 0x0f0f0f0f;
  x += (x >> 8);
  x += (x >> 16);
  x &= 0x3f;

  return static_cast<int>(x);
}

CPageManager::CPage::CPage(u8* start, u8* end, int page_id) {
  m_pNextPage = nullptr;
  m_uPageBit = 1 << (page_id & 0x1f);
  m_nUnk = 0;
  m_pData = start;
  m_pEnd = end;
  m_nRefCount = 0;
  m_nPageID = page_id;
  m_nDmaRefCount = 0;
  m_nAllocState = 0;
  m_nUnk2 = 0;
}

int CPageManager::CPage::AddRef() {
  int state, ret = -1;

  CpuSuspendIntr(&state);

  if (m_nAllocState == 1 && m_pPageList != nullptr) {
    m_pPageList->m_nRefCount++;
    m_nRefCount++;
    ret = m_nRefCount;
  }

  CpuResumeIntr(state);

  return ret;
}

int CPageManager::CPage::ReleaseRef() {
  int state, ret = -1;

  CpuSuspendIntr(&state);

  if (m_nAllocState == 1 && m_pPageList != nullptr) {
    m_pPageList->m_nRefCount--;
    m_nRefCount--;
    ret = m_nRefCount;
  }

  CpuResumeIntr(state);

  return ret;
}

int CPageManager::CPage::AddDmaRef() {
  int state, ret = -1;

  CpuSuspendIntr(&state);

  if (m_nAllocState == 1 && m_pPageList != nullptr) {
    m_pPageList->m_nDmaRefCount++;
    m_nDmaRefCount++;
    ret = m_nDmaRefCount;
  }

  CpuResumeIntr(state);

  return ret;
}

int CPageManager::CPage::ReleaseDmaRef() {
  int state, ret = -1;

  CpuSuspendIntr(&state);

  if (m_nAllocState == 1 && m_pPageList != nullptr) {
    m_pPageList->m_nDmaRefCount--;
    m_nDmaRefCount--;
    ret = m_nDmaRefCount;
  }

  CpuResumeIntr(state);

  return ret;
}

void CPageManager::CPage::FromPagesCopy(u8* pInPageData, u8* pDest, int nNumBytes) {
  if (nNumBytes <= 0) {
    return;
  }

}

void CPageManager::Initialize() {
  m_Cache.Initialize();
}

CPageManager::CCache::CCache() {
  m_paCache = nullptr;
  m_uAllocatedPageMap = 0;
  m_uFreePageMap = (1 << NUM_PAGES) - 1;
}

int CPageManager::CCache::Initialize() {
  m_paCache = AllocSysMemory(0, PAGE_SIZE * NUM_PAGES, nullptr);
  if (!m_paCache) {
    return 1;
  }

  m_nNumFree = 29;

  /* TODO FIXME */
  for (auto& pl : m_aPageLists) {
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

}  // namespace jak3
