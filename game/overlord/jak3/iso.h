#pragma once

#include "game/overlord/jak3/basefile.h"
#include "game/sce/iop.h"

namespace jak3 {
extern int g_nISOMbx;

struct ISO_Hdr {
  iop::MsgPacket m_Pkt;
  int m_nStatus;
  bool m_bActive;
  bool unk1;
  bool unk2;

  void SetActive(bool b);
  void SetUnk1(bool b);
  void SetUnk2(bool b);
};

struct ISO_Msg : ISO_Hdr {
  u32 msg_kind;
  int mbx_to_reply;
  int thread_id;
  int (*callback)(ISO_Msg*);
  CBaseFile* file;
};

struct ISO_LoadDGO : ISO_Msg {
  ISOFileDef* fd;
};

struct ISO_LoadSingle : ISO_Msg {
  const ISOFileDef* fd;
  void* address;
  u32 length;
  u32 length_to_copy;
  u32 offset;
};

int InitISOFS(const char* fs_mode, const char* loading_sceeen);
const ISOFileDef* FindIsoFile(const char* name);

}  // namespace jak3
