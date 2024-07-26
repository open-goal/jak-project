#pragma once

#include "common/common_types.h"

namespace jak3 {
void jak3_overlord_init_globals_iso_queue();
struct ISO_Hdr;
struct ISO_VAGCommand;
void ReleaseMessage(ISO_Hdr* msg);
void FreeVagCmd(ISO_VAGCommand* msg);
int QueueMessage(ISO_Hdr* msg, int pri);
int UnqueueMessage(ISO_Hdr* msg);
void ReturnMessage(ISO_Hdr* msg);
void InitBuffers();
bool DgoCmdWaiting();
ISO_Hdr* GetMessage();
int ProcessMessageData(ISO_Hdr* msg);
void FreeVAGCommand(ISO_VAGCommand* msg);
ISO_VAGCommand* GetVAGCommand();

struct PriStackEntry {
  ISO_Hdr* cmds[8];
  int count = 0;
};

extern u32 g_auTrapSRAM[6];
extern u32 g_auStrmSRAM[6];
extern s32 g_nPriQueueSema;
extern PriStackEntry gPriStack[2];
}  // namespace jak3