#pragma once

namespace jak3 {
void jak3_overlord_init_globals_iso_queue();
struct ISO_Hdr;
struct ISO_VAGCommand;
void ReleaseMessage(ISO_Hdr* msg);
void FreeVagCmd(ISO_VAGCommand* msg);
int QueueMessage(ISO_Hdr* msg, int);
int UnqueueMessage(ISO_Hdr* msg);
int ReturnMessage(ISO_Hdr* msg);
void InitBuffers();
}  // namespace jak3