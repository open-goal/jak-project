#pragma once

#include <functional>

#include "common/common_types.h"

namespace jak3 {
void jak3_overlord_init_globals_dvd_driver();

struct CDescriptor;
struct BlockParams;
struct CPage;

struct Block {
  CPage* page;
  char* done_flag;
};

class CDvdDriver {
 public:
  // todo CTOR
  void Initialize();
  void CancelRead(CDescriptor* descriptor);
  int ReadMultiple(CDescriptor* descriptor,
                   int* pages_read_out,
                   BlockParams* params,
                   int num_pages,
                   bool flag);
  void SetDriverCallback(std::function<void(int)> f);
};

// replacement for g_DvdDriver
CDvdDriver* get_driver();

struct CISOCDFile;
struct CDescriptor {
  CISOCDFile* m_File = nullptr;
  void (*m_Callback)(CISOCDFile*, Block*, s32);
};
}  // namespace jak3