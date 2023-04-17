#pragma once

#include "isocommon.h"

#include "common/common_types.h"

#include "game/overlord/common/isocommon.h"

namespace jak1 {
void iso_queue_init_globals();
void InitBuffers();
IsoBufferHeader* AllocateBuffer(uint32_t size);
void FreeBuffer(IsoBufferHeader* buffer);
u32 QueueMessage(IsoMessage* cmd, int32_t priority, const char* name);
void UnqueueMessage(IsoMessage* cmd);
IsoMessage* GetMessage();
void ProcessMessageData();
void ReturnMessage(IsoMessage* cmd);
IsoBufferHeader* TryAllocateBuffer(uint32_t size);

VagCommand* GetVAGCommand();
void FreeVAGCommand(VagCommand* cmd);
void ReleaseMessage(IsoMessage* cmd);
}  // namespace jak1