#pragma once

#include "game/overlord/jak3/basefile.h"
#include "game/sce/iop.h"

namespace jak3 {

struct ISO_Hdr {
  iop::MsgPacket m_Pkt;
  int m_nStatus;
  bool m_bActive;
  bool unk1;
  bool unk2;

  void SetActive();
  void SetUnk1();
  void SetUnk2();
};

struct ISO_Msg : ISO_Hdr {
  u32 cmd_kind;
  int mbx_to_reply;
  int thread_id;
  void* callback;
  CBaseFile* file;
};

struct ISO_LoadDGO : ISO_Msg {
  ISOFileDef* fd;
};

int InitISOFS(const char* fs_mode, const char* loading_sceeen);
}  // namespace jak3
