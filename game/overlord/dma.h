#pragma once

/*!
 * @file dma.h
 * DMA Related functions for Overlord.
 * This code is not great.
 */

#ifndef JAK_V2_DMA_H
#define JAK_V2_DMA_H

#include "common/common_types.h"

void DMA_Sync();
void DMA_SendToEE(void* data, u32 size, void* dest);
void dma_init_globals();

#endif  // JAK_V2_DMA_H
