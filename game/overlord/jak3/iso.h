#pragma once
#include "common/link_types.h"

#include "game/overlord/jak3/isocommon.h"

namespace jak3 {
struct ISOFileDef;
void jak3_overlord_init_globals_iso();
void InitISOFS();

const ISOFileDef* FindISOFile(const char*);
struct ISO_VAGCommand;
struct VagStreamData;
struct RPC_Dgo_Cmd;

struct ISO_DGOCommand : public ISO_Hdr {
  u8* buffer1 = nullptr;
  u8* buffer2 = nullptr;
  u8* buffer_top = nullptr;
  DgoHeader dgo_header;          //
  ObjectHeader obj_header;       //
  u8* ee_dest_buffer = nullptr;  // 192
  int bytes_processed = 0;       // 196
  int objects_loaded = 0;        // 200
  enum class State {
    INIT = 0,
    READ_DGO_HEADER = 1,
    FINISH_OBJ = 2,
    READ_LAST_OBJ = 3,
    READ_OBJ_HEADER = 4,
    READ_OBJ_DATA = 5,
    FINISH_DGO = 6,
    FINISH_OBJ_SINGLE_BUFFER = 7,
  } state = State::INIT;              // 204
  int finished_first_object = 0;      // 208
  int buffer_toggle = 0;              // 212
  u8* selected_buffer = nullptr;      // 216
  int selected_id = 0;                // 220
  int last_id = 0;                    // 224
  int acked_cancel_id = 0;            // 228
  u8 nosync_cancel_pending_flag = 0;  // 232
  int request_cancel_id = 0;          // 236
  u8 nosync_cancel_ack = 0;           // 240
  int sync_sent_count = 0;            // 244

  // the number of times we looked in the iso thread's sync mbox
  // I think just used for debugging/asserts.
  int sync_mbox_wait_count = 0;  // 248

  int sync_ret_count = 0;  // 252
  int want_abort = 0;      // 256
};

enum class CopyKind {
  EE = 0,
  IOP = 1,
  SBK = 2,
};

void set_active_a(ISO_Hdr* cmd, int val);
void set_active_b(ISO_Hdr* cmd, int val);
void set_active_c(ISO_Hdr* cmd, int val);
void IsoStopVagStream(ISO_VAGCommand* cmd);
void IsoPlayVagStream(ISO_VAGCommand* user_cmd);
EIsoStatus NullCallback(ISO_Hdr* msg);
EIsoStatus CopyDataToIOP(ISO_Hdr* msg);
EIsoStatus CopyDataSbkLoad(ISO_Hdr* msg);
EIsoStatus CopyDataToEE(ISO_Hdr* msg);
EIsoStatus RunDGOStateMachine(ISO_Hdr* msg);
void QueueVAGStream(VagStreamData* cmd);
void CopyDataDmaCallback(void*);
void* RPC_DGO(unsigned int fno, void* msg_ptr, int);
void LoadDGO(RPC_Dgo_Cmd* cmd);
void CancelDGO(RPC_Dgo_Cmd* cmd);
void LoadNextDGO(RPC_Dgo_Cmd* cmd);
EIsoStatus CopyData(ISO_LoadCommon* msg, CopyKind kind);
void CancelDGONoSync(int id);
void IsoPlayMusicStream(ISO_VAGCommand* user_cmd);
void IsoQueueVagStream(ISO_VAGCommand* user_cmd);
extern int g_nISOThreadID;
extern int g_nISOMbx;
extern bool g_bMusicPause;
extern int g_nMusicSemaphore;
extern char g_szTargetMusicName[0x30];
extern int g_nActiveMusicStreams;
extern bool g_bVagCmdsInitialized;
extern bool g_bMusicIsPaused;
extern bool g_bAnotherMusicPauseFlag;
extern int g_nMusicFade;
extern int g_nMusicTweak;
extern int g_nMusicFadeDir;
}  // namespace jak3