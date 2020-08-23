#ifndef JAK_V2_ISO_QUEUE_H
#define JAK_V2_ISO_QUEUE_H


#include "common/common_types.h"
#include "isocommon.h"



void iso_queue_init_globals();
void InitBuffers();
IsoBufferHeader* AllocateBuffer(uint32_t size);
void FreeBuffer(IsoBufferHeader *buffer);
u32 QueueMessage(IsoMessage *cmd, int32_t priority, const char *name);
void UnqueueMessage(IsoMessage *cmd);
IsoMessage* GetMessage();
void ProcessMessageData();
void ReturnMessage(IsoMessage *cmd);


#endif //JAK_V2_ISO_QUEUE_H
