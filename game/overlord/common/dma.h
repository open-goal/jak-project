#pragma once

#include "common/common_types.h"

void DMA_SendToEE(void* data, u32 size, void* dest);
void DMA_Sync();
