#pragma once

#define SCE_PAD_DMA_BUFFER_SIZE 0x100

namespace ee {
int scePadPortOpen(int port, int slot, void* data);
}