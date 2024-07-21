#pragma once

#include "game/overlord/jak3/basefile.h"
#include "game/sce/iop.h"

namespace jak3 {

constexpr int LOAD_TO_EE_CMD_ID = 0x100;         // command to load file to ee
constexpr int LOAD_TO_IOP_CMD_ID = 0x101;        // command to load to iop
constexpr int LOAD_TO_EE_OFFSET_CMD_ID = 0x102;  // command to load file to ee with offset.
constexpr int LOAD_TO_989SND = 0x103;
constexpr int PAUSE_VAG_STREAM = 0x403;     // Command to pause a vag stream
constexpr int CONTINUE_VAG_STREAM = 0x404;  // Command to continue a vag stream
constexpr int SET_DIALOG_VOLUME = 0x406;    // Command to set the volume of vag playback

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
  int (*callback)(ISO_Hdr*);
  CBaseFile* file;
  u32 priority;
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
  char* filename;
  u32 unk40;
  void* unk44;
  u32 unk48;
  u32 unk4c;
};

int InitISOFS(const char* fs_mode, const char* loading_sceeen);
const ISOFileDef* FindIsoFile(const char* name);

}  // namespace jak3
