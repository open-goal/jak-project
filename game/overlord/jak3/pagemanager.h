#pragma once

#include "common/common_types.h"

namespace jak3 {
class CPageManager {
 public:
  class CPageList;

  enum class AllocState {
    EPLAS_FREE,
    EPLAS_ALLOCATED,
  };

  class CPage {
   public:
    CPage() = default;
    CPage(u8*, u8*, int);
    int AddRef();
    int ReleaseRef();
    int AddDmaRef();
    int ReleaseDmaRef();
    void FromPagesCopy(u8* pInPageData, u8* pDest, int nNumBytes);
    u8* GetStart() const;
    u8* GetEnd() const;
    bool IsReferenced() { return m_nPageRefCount != 0 || m_nPageDmaRefCount != 0; }
    bool IsFilled() const;  // Exists as string in july version, not sure on impl
    bool IsAllocated() const;
    void SetPageInputState(int state);  // enum
    int GetPageCacheIndex() const;
    CPage* GetNextPage() const;
    CPage* GetPrevPage() const;

   private:
    CPage* m_pNextPage;
    CPage* m_pPrevPage;
    CPageList* m_pPageList;
    int m_nPageRefCount;
    int m_nPageDmaRefCount;
    int m_nAllocState;
    u32 m_nPageState;  // 3: filled otherwise unk?
    u8* m_pData;
    u8* m_pDataEnd;
    int m_nPageID;
    u32 m_uPageBit;
  };

  class CPageList {
    friend class CPage;

   public:
    int AddActivePages(int);
    int CancelActivePages();
    CPage* StepActivePage();
    void GarbageCollect();
    CPage* GetCurrentActivePage() const;
    int GetNumRawPages() const;
    int GetFirstRawPage() const;
    int GetNumUnsteppedPages() const;
    bool IsReferenced() { return m_nListRefCount != 0 || m_nListDmaRefCount != 0; }
    int GetNumActivePages() const;

   private:
    CPage* m_pFirstPage;
    CPage* m_pLastPage;
    CPage* m_pFirstACtivePage;
    CPage* m_pLastActivePage;
    CPage* m_pCurrentActivePage;
    int m_nNumPages;
    int m_nNumActivePages;
    int m_nNumUnsteppedPages;
    int m_nListRefCount;
    int m_nListDmaRefCount;
    int m_nAllocState;
  };

  void Initialize();
  CPageList* AllocPageListBytes(int nBytes, char unk);
  CPageList* AllocPageList(int nPages, char unk);
  CPageList* GrowPageList(CPageList* list, char nPages);
  int FreePageList(CPageList* list);
  int TryFreePages(int nUnk);
  void GarbageCollect();

 private:
  class CCache {
   public:
    CCache();
    int Initialize();

    void* m_paCache;
    CPageList m_aPageLists[29];
    CPage m_aPages[29];
    int m_nNumFree;
    u32 m_uAllocatedPageMap;
    u32 m_uAllocatedListMap;
    int m_nEventFlag;
    u32 m_Unk;
  };

  CCache m_Cache;
};

}  // namespace jak3
