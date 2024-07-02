#pragma once

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
ISO_Hdr* GetMessage();
int ProcessMessageData(ISO_Hdr* msg);
void FreeVAGCommand(ISO_Hdr* msg);

struct PriStackEntry {
  ISO_Hdr* cmds[8];
  int count = 0;
};
}  // namespace jak3