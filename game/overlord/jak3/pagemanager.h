#pragma once

#include "common/common_types.h"

namespace jak3 {
class CPageManager {
 public:
  class CPage {
   public:
    CPage(u8*, u8*, int);
    int AddRef();
    int ReleaseRef();
    int AddDmaRef();
    int ReleaseDmaRef();
    void FromPagesCopy(u8* pInPageData, u8* pDest, int nNumBytes);
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
    int Initialize();
  };
  CCache m_Cache;
};

}  // namespace jak3
