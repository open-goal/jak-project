#pragma once

#include "game/sce/iop.h"

namespace jak3 {

struct ISOBuffer {
  void AdjustDataLength(int);
  void AdvanceCurrentData(int);
};

struct ISO_Hdr {
  iop::MsgPacket msg;

  void SetActive();
  void SetUnk1();
  void SetUnk2();
};

struct ISO_Msg : ISO_Hdr {};

struct ISO_LoadDGO : ISO_Msg {};

int InitISOFS(const char* fs_mode, const char* loading_sceeen);
}  // namespace jak3
