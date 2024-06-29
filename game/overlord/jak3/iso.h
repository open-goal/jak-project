#pragma once
#include "game/overlord/jak3/isocommon.h"

namespace jak3 {
struct ISOFileDef;
void jak3_overlord_init_globals_iso();
void InitISOFS();

const ISOFileDef* FindISOFile(const char*);
struct ISO_VAGCommand;

struct ISO_DGOCommand : public ISO_Hdr {
  int selected_id = 0;                // 220
  int last_id = 0;                    // 224
  int acked_cancel_id = 0;            // 228
  u8 nosync_cancel_pending_flag = 0;  // 232
  int request_cancel_id = 0;          // 236
  u8 nosync_cancel_ack = 0;           // 240
  int sync_sent_count = 0;            // 244
  int sync_ret_count = 0;             // 252

  // the number of times we looked in the iso thread's sync mbox
  // I think just used for debugging/asserts.
  int sync_mbox_wait_count = 0;  // 248
};

void set_active_a(ISO_VAGCommand* cmd, int val);
void set_active_b(ISO_VAGCommand* cmd, int val);
void set_active_c(ISO_VAGCommand* cmd, int val);
void IsoStopVagStream(ISO_VAGCommand* cmd);


extern int g_nISOThreadID;
}  // namespace jak3