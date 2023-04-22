#pragma once
#include "common/common_types.h"

#include "game/overlord/jak2/pages.h"
#include "game/overlord/jak2/iso.h"
namespace jak2 {
extern uint8_t* ScratchPadMemory;
void iso_queue_init_globals();
extern PageList* SpMemoryBuffers;
void ReleaseMessage(CmdHeader* param_1, int param_2);
int QueueMessage(CmdHeader* param_1, int param_2, const char* param_3, int param_4);
void DisplayQueue();
}  // namespace jak2