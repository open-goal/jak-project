#pragma once
#include "common/common_types.h"

#include "game/overlord/jak2/pages.h"
namespace jak2 {
extern uint8_t* ScratchPadMemory;
void iso_queue_init_globals();
extern PageList* SpMemoryBuffers;
}  // namespace jak2