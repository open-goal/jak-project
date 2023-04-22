#include "iso_queue.h"

#include <cstring>

#include "game/overlord/jak2/iso.h"
#include "game/overlord/jak2/vag.h"
#include "game/sound/sndshim.h"

namespace jak2 {

/// The data structure containing all memory pages.
PageList* SpMemoryBuffers = nullptr;
u8* ScratchPadMemory = nullptr;
constexpr int N_BUFFERS = 3 + 11;  // ??
static Buffer sBuffer[N_BUFFERS];
static Buffer* sFreeBuffer = nullptr;
static Buffer* sFreeStrBuffer = nullptr;
int BuffersAlloc = 0;
int StrBuffersAlloc = 0;

void iso_queue_init_globals() {
  memset(sBuffer, 0, sizeof(sBuffer));
  ScratchPadMemory = nullptr;
  SpMemoryBuffers = nullptr;
  sFreeBuffer = nullptr;
  sFreeStrBuffer = nullptr;
  BuffersAlloc = 0;
  StrBuffersAlloc = 0;
}

void InitBuffers() {
  SpMemoryBuffers = (PageList*)ScratchPadMemory;
  ScratchPadMemory += sizeof(PageList);
  InitPagedMemory(SpMemoryBuffers, 0x12, 0x8000);
  Buffer* next_buffer = sBuffer;
  for (int i = 0; i < 3; i++) {
    next_buffer++;
    sBuffer[i].next = next_buffer;
    sBuffer[i].decomp_buffer = nullptr;
    sBuffer[i].decompressed_size = 0;
    sBuffer[i].unk_12 = 0;
    sBuffer[i].data_buffer_idx = -1;
    sBuffer[i].use_mode = 1;
    sBuffer[i].plist = SpMemoryBuffers;
    sBuffer[i].num_pages = 1;
    sBuffer[i].unk_32 = 0;
    sBuffer[i].free_pages = 0;
    sBuffer[i].page = nullptr;
    sBuffer[i].unk_44 = 0;
  };
  sBuffer[2].next = nullptr;

  next_buffer = sBuffer + 4;
  sFreeBuffer = sBuffer;
  BuffersAlloc = 0;
  for (int i = 0; i < 8; i++) {
    sBuffer[i + 3].next = next_buffer;
    next_buffer++;
    sBuffer[i + 3].decomp_buffer = nullptr;
    sBuffer[i + 3].decompressed_size = 0;
    sBuffer[i + 3].unk_12 = 0;
    sBuffer[i + 3].data_buffer_idx = -1;
    sBuffer[i + 3].use_mode = 2;
    sBuffer[i + 3].plist = SpMemoryBuffers;
    sBuffer[i + 3].num_pages = 1;
    sBuffer[i + 3].unk_32 = 2;
    sBuffer[i + 3].free_pages = 0;
    sBuffer[i + 3].page = nullptr;
    sBuffer[i + 3].unk_44 = 0;
  };
  sBuffer[10].next = nullptr;
  sFreeStrBuffer = sBuffer + 3;
  StreamSRAM[0] = 0x5040;
  StrBuffersAlloc = 0;
  TrapSRAM[0] = 0x9040;
  snd_SRAMMarkUsed(0x5040, 0x4040);
  StreamSRAM[1] = 0x9080;
  TrapSRAM[1] = 0xd080;
  snd_SRAMMarkUsed(0x9080, 0x4040);
  StreamSRAM[2] = 0xd0c0;
  TrapSRAM[2] = 0x110c0;
  snd_SRAMMarkUsed(0xd0c0, 0x4040);
  StreamSRAM[3] = 0x11100;
  TrapSRAM[3] = 0x15100;
  snd_SRAMMarkUsed(0x11100, 0x4040);


  iVar6 = 0;
  iVar7 = 0;
  do {
    while (true) {
      iVar3 = DMA_SendToSPUAndSync(&VAG_SilentLoop, 0x30, *(int*)((int)TrapSRAM + iVar7), 0, 1);
      if (iVar3 != 0)
        break;
      DelayThread(1000);
    }
    iVar6 = iVar6 + 1;
    iVar7 = iVar6 * 4;
  } while (iVar6 < 4);
  local_20.attr = 1;
  local_20.init_count = 1;
  local_20.max_count = 1;
  local_20.option = 0;
  sSema = CreateSema(&local_20);
  if (sSema < 0) {
    Kprintf("IOP: ======================================================================\n");
    Kprintf("IOP: iso_queue InitBuffers: Can\'t create semaphore\n");
    Kprintf("IOP: ======================================================================\n");
    do {
      /* WARNING: Do nothing block with infinite loop */
    } while (true);
  }
}
}  // namespace jak2
