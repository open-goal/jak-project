#pragma once

#include "common/common_types.h"

namespace jak3 {
class CPageManager {
 public:
  class CPageList;

  class CPage {
   public:
    CPage() = default;
    CPage(u8*, u8*, int);
    int AddRef();
    int ReleaseRef();
    int AddDmaRef();
    int ReleaseDmaRef();
    void FromPagesCopy(u8* pInPageData, u8* pDest, int nNumBytes);

   private:
    CPage* m_pNextPage;
    u32 m_uPageBit;
    u32 m_nUnk;
    CPageList* m_pPageList;
    u8* m_pData;
    u8* m_pEnd;  // maybe
    u32 m_nRefCount;
    u32 m_nDmaRefCount;
    int m_nPageID;
    int m_nAllocState;
    int m_nUnk2;
  };

  class CPageList {
    int AddActivePages(int);
    int CancelActivePages();
    CPage* StepActivePage();
    void GarbageCollect();
  };

  void Initialize();
  CPageList* AllocPageListBytes(int nBytes, char unk);
  CPageList* AllocPageList(int nPages, char unk);
  CPageList* GrowPageList(CPageList* list, char nPages);
  int FreePageList(CPageList* list);
  int Unk(int nUnk);
  void GarbageCollect();

 private:
  class CCache {
   public:
    CCache();
    int Initialize();

   private:
    int m_nEventFlag;
    void* m_paCache;
    CPageList m_aPageLists[29];
    CPage m_aPages[29];
    u32 m_uAllocatedPageMap;
    u32 m_uFreePageMap;
    int m_nNumFree;
  };
  CCache m_Cache;
};

}  // namespace jak3
