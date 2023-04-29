#include "iso_queue.h"

#include <cstring>

#include "common/log/log.h"
#include "common/util/Assert.h"

#include "game/overlord/common/iso.h"
#include "game/overlord/jak2/dma.h"
#include "game/overlord/jak2/iso.h"
#include "game/overlord/jak2/spustreams.h"
#include "game/overlord/jak2/vag.h"
#include "game/sce/iop.h"
#include "game/sound/sndshim.h"

using namespace iop;

namespace jak2 {

/// The data structure containing all memory pages.
PageList* SpMemoryBuffers = nullptr;
u8* ScratchPadMemory = nullptr;
constexpr int N_BUFFERS = 3 + 11;  // ??
static Buffer sBuffer[N_BUFFERS];
static Buffer* sFreeBuffer = nullptr;
static Buffer* sFreeStrBuffer = nullptr;
u32 BuffersAlloc = 0;
u32 StrBuffersAlloc = 0;
u32 AllocdBuffersCount = 0;
u32 NextBuffer = 0;
u32 AllocdStrBuffersCount = 0;
u32 NextStrBuffer = 0;
int sSema = 0;
int vag_cmd_cnt = 0;
VagCmd vag_cmds[16];
u32 vag_cmd_used = 0;
u32 max_vag_cmd_cnt = 0;

PriStackEntry gPriStack[N_PRIORITIES];
std::string gPriEntryNames[N_PRIORITIES][PRI_STACK_LENGTH];  // my addition for debug

void ReturnMessage(CmdHeader* param_1);
void FreeVAGCommand(VagCmd* param_1);

void iso_queue_init_globals() {
  memset(sBuffer, 0, sizeof(sBuffer));
  memset(gPriStack, 0, sizeof(gPriStack));
  memset(vag_cmds, 0, sizeof(vag_cmds));

  ScratchPadMemory = nullptr;
  SpMemoryBuffers = nullptr;
  sFreeBuffer = nullptr;
  sFreeStrBuffer = nullptr;
  BuffersAlloc = 0;
  StrBuffersAlloc = 0;
  AllocdBuffersCount = 0;
  NextBuffer = 0;
  sSema = 0;
  vag_cmd_cnt = 0;
  vag_cmd_used = 0;
  max_vag_cmd_cnt = 0;
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

  for (int i = 0; i < 4; i++) {
    if (DMA_SendToSPUAndSync(VAG_SilentLoop, 0x30, TrapSRAM[i], 0, 1)) {
      ASSERT_NOT_REACHED();
      break;
    }
    DelayThread(1000);
  }

  SemaParam param;
  param.attr = 1;
  param.init_count = 1;
  param.max_count = 1;
  param.option = 0;
  sSema = CreateSema(&param);
  if (sSema < 0) {
    printf("IOP: ======================================================================\n");
    printf("IOP: iso_queue InitBuffers: Can\'t create semaphore\n");
    printf("IOP: ======================================================================\n");
    ASSERT_NOT_REACHED();
  }
}

u32 AllocDataBuffer(u32* param_1, u32 param_2) {
  bool bVar1;
  u32 uVar2;
  u32 uVar3;
  int iVar4;
  u32 uVar5;

  uVar5 = 0xffffffff;
  bVar1 = false;
  uVar3 = uVar5;
  if (param_1 == &BuffersAlloc) {
    if (AllocdBuffersCount < param_2) {
      if ((BuffersAlloc & 1 << (NextBuffer & 0x1f)) == 0) {
        bVar1 = true;
      } else {
        iVar4 = 0;
        if (0 < (int)param_2) {
          do {
            NextBuffer = NextBuffer + 1;
            if (param_2 <= NextBuffer) {
              NextBuffer = 0;
            }
            iVar4 = iVar4 + 1;
            if ((BuffersAlloc & 1 << (NextBuffer & 0x1f)) == 0) {
              bVar1 = true;
              iVar4 = param_2 + 1;
            }
          } while (iVar4 < (int)param_2);
        }
      }
      uVar5 = NextBuffer;
      if (bVar1) {
        BuffersAlloc = BuffersAlloc | 1 << (NextBuffer & 0x1f);
        AllocdBuffersCount = AllocdBuffersCount + 1;
        NextBuffer = NextBuffer + 1;
        uVar3 = uVar5;
        if (param_2 <= NextBuffer) {
          NextBuffer = 0;
        }
      }
    }
  } else {
    uVar3 = 0xffffffff;
    if ((param_1 == &StrBuffersAlloc) && (uVar3 = uVar5, AllocdStrBuffersCount < param_2)) {
      if ((StrBuffersAlloc & 1 << (NextStrBuffer & 0x1f)) == 0) {
        bVar1 = true;
      } else {
        iVar4 = 0;
        if (0 < (int)param_2) {
          do {
            NextStrBuffer = NextStrBuffer + 1;
            if (param_2 <= NextStrBuffer) {
              NextStrBuffer = 0;
            }
            iVar4 = iVar4 + 1;
            if ((StrBuffersAlloc & 1 << (NextStrBuffer & 0x1f)) == 0) {
              bVar1 = true;
              iVar4 = param_2 + 1;
            }
          } while (iVar4 < (int)param_2);
        }
      }
      uVar2 = NextStrBuffer;
      if (bVar1) {
        StrBuffersAlloc = StrBuffersAlloc | 1 << (NextStrBuffer & 0x1f);
        AllocdStrBuffersCount = AllocdStrBuffersCount + 1;
        NextStrBuffer = NextStrBuffer + 1;
        uVar3 = uVar2;
        if (param_2 <= NextStrBuffer) {
          NextStrBuffer = 0;
        }
      }
    }
  }
  return uVar3;
}

Buffer* AllocateBuffer(int param_1, VagCmd* param_2, int /*param_3*/) {
  PageList** ppPVar1;
  int* piVar2;
  int iVar3;
  int iVar4;
  int iVar5;
  Buffer* pBVar6;
  Buffer* pBVar7;
  Page* pPVar8;

  // if (param_3 == 1) {
  // CpuSuspendIntr(local_28);
  //}
  pBVar6 = (Buffer*)0x0;
  pPVar8 = (Page*)0x0;
  if (param_1 == 1) {
    if ((sFreeBuffer != (Buffer*)0x0) &&
        (iVar3 = AllocDataBuffer(&BuffersAlloc, 3), pBVar7 = sFreeBuffer, -1 < iVar3)) {
      ppPVar1 = &sFreeBuffer->plist;
      piVar2 = &sFreeBuffer->num_pages;
      sFreeBuffer->data_buffer_idx = iVar3;
      sFreeBuffer->use_mode = 1;
      sFreeBuffer = sFreeBuffer->next;
      pPVar8 = (Page*)AllocPages(*ppPVar1, *piVar2);
      if (pPVar8 != (Page*)0x0) {
        pBVar7->page = pPVar8;
        pBVar7->free_pages = pPVar8->free_pages;
        pBVar7->decomp_buffer = (uint8_t*)pPVar8->buffer;
      }
      goto LAB_00006a0c;
    }
  LAB_00006a28:
    pBVar7 = pBVar6;
    if (pPVar8 != (Page*)0x0)
      goto LAB_00006a44;
  } else {
    if (((param_1 != 2) || (sFreeStrBuffer == (Buffer*)0x0)) ||
        (iVar3 = AllocDataBuffer(&StrBuffersAlloc, 8), pBVar7 = sFreeStrBuffer, iVar3 < 0))
      goto LAB_00006a28;
    sFreeStrBuffer->data_buffer_idx = iVar3;
    sFreeStrBuffer->use_mode = 2;
    pBVar6 = (param_2->header).callback_buffer;
    pPVar8 = (Page*)0x0;
    if (param_2->xfer_size == 0) {
      iVar5 = sFreeStrBuffer->num_pages;
    } else {
      iVar4 = sFreeStrBuffer->plist->page_size;
      iVar3 = param_2->xfer_size + iVar4 + -1;
      if (iVar4 == 0) {
        ASSERT_NOT_REACHED();
        // trap(0x1c00);
      }
      if (pBVar6 == (Buffer*)0x0) {
        iVar5 = 0;
      } else {
        iVar5 = -pBVar6->page->free_pages;
      }
      iVar5 = iVar3 / iVar4 + iVar5;
      if (iVar5 < 0) {
        iVar5 = 0;
      }
    }
    pBVar6 = sFreeStrBuffer->next;
    if (iVar5 != 0) {
      if ((param_2->status_bytes[BYTE10] == '\0') || (param_2->unk_232 == '\0')) {
        if (param_2->status_bytes[BYTE4] == '\0') {
          iVar3 = sFreeStrBuffer->num_pages;
        } else {
          iVar3 = sFreeStrBuffer->unk_32;
        }
        if (iVar5 < iVar3) {
          iVar3 = iVar5;
        }
      } else {
        iVar3 = iVar5;
        if (3 < iVar5) {
          iVar3 = 3;
        }
      }
      ppPVar1 = &sFreeStrBuffer->plist;
      sFreeStrBuffer = sFreeStrBuffer->next;
      pPVar8 = (Page*)AllocPages(*ppPVar1, iVar3);
      pBVar6 = sFreeStrBuffer;
    }
    sFreeStrBuffer = pBVar6;
    if (pPVar8 != (Page*)0x0) {
      pBVar7->page = pPVar8;
      pBVar7->free_pages = pPVar8->free_pages;
      pBVar7->decomp_buffer = (uint8_t*)pPVar8->buffer;
    }
  LAB_00006a0c:
    if (pPVar8 != (Page*)0x0) {
      pBVar7->decompressed_size = 0;
      pBVar7->next = (Buffer*)0x0;
      pBVar7->unk_12 = pPVar8->buffer;
      pBVar6 = pBVar7;
      goto LAB_00006a28;
    }
  }
  if (pBVar7) {
    FreeBuffer(pBVar7, 0);
    pBVar7 = (Buffer*)0x0;
  }
LAB_00006a44:
  // if (param_3 == 1) {
  // CpuResumeIntr(local_28[0]);
  //}
  return pBVar7;
}

void FreeBuffer(Buffer* param_1, int /*param_2*/) {
  Buffer* pBVar1;
  Page* pPVar2;
  int iVar3;
  u32 uVar4;
  // if (param_2 == 1) {
  // CpuSuspendIntr(local_18);
  //}
  iVar3 = param_1->use_mode;
  if (iVar3 == 1) {
    if ((BuffersAlloc & 1 << (param_1->data_buffer_idx & 0x1fU)) != 0) {
      pPVar2 = FreePagesList(param_1->plist, param_1->page);
      pBVar1 = sFreeBuffer;
      uVar4 = param_1->data_buffer_idx;
      param_1->page = pPVar2;
      param_1->data_buffer_idx = -1;
      param_1->decompressed_size = 0;
      param_1->unk_12 = 0;
      sFreeBuffer = param_1;
      param_1->use_mode = 0;
      param_1->next = pBVar1;
      BuffersAlloc = BuffersAlloc & ~(1 << (uVar4 & 0x1f));
      AllocdBuffersCount = AllocdBuffersCount + -1;
    }
  } else if (((1 < iVar3) && (iVar3 == 2)) &&
             ((StrBuffersAlloc & 1 << (param_1->data_buffer_idx & 0x1fU)) != 0)) {
    pPVar2 = FreePagesList(param_1->plist, param_1->page);
    param_1->page = pPVar2;
    param_1->decompressed_size = 0;
    param_1->unk_12 = 0;
    pBVar1 = param_1;
    param_1->next = sFreeStrBuffer;
    sFreeStrBuffer = pBVar1;
    StrBuffersAlloc = StrBuffersAlloc & ~(1 << (param_1->data_buffer_idx & 0x1fU));
    AllocdStrBuffersCount = AllocdStrBuffersCount + -1;
    param_1->data_buffer_idx = -1;
    param_1->use_mode = 0;
  }
  // if (param_2 == 1) {
  // CpuResumeIntr(local_18[0]);
  //}
}

void ReleaseMessage(CmdHeader* param_1, int param_2) {
  Buffer* pBVar1;
  int iVar2;
  PriStackEntry* pPVar3;
  int iVar4;
  int iVar5;
  PriStackEntry* pPVar6;

  pBVar1 = param_1->callback_buffer;
  while (pBVar1 != (Buffer*)0x0) {
    pBVar1 = param_1->callback_buffer;
    param_1->callback_buffer = pBVar1->next;
    FreeBuffer(pBVar1, param_2);
    pBVar1 = param_1->callback_buffer;
  }
  if (param_1->lse != (LoadStackEntry*)0x0) {
    // (*(code*)isofs->close)();
    isofs->close(param_1->lse);
  }
  // if (param_2 == 1) {
  // CpuSuspendIntr(local_18);
  //}
  iVar5 = 0;
  pPVar6 = gPriStack;
LAB_00006d0c:
  iVar4 = 0;
  pPVar3 = pPVar6;
  if (pPVar6->count < 1)
    goto LAB_00006da0;
  do {
    if (pPVar3->entries[0] == param_1)
      break;
    iVar4 = iVar4 + 1;
    pPVar3 = (PriStackEntry*)(pPVar3->entries + 1);
  } while (iVar4 < pPVar6->count);
  iVar2 = pPVar6->count + -1;
  if (pPVar6->count <= iVar4)
    goto LAB_00006da0;
  pPVar6->count = iVar2;
  if (iVar4 < iVar2) {
    do {
      iVar5 = iVar4 + 1;
      pPVar6->entries[iVar4] = pPVar6->entries[iVar4 + 1];
      iVar4 = iVar5;
    } while (iVar5 < pPVar6->count);
  }
  if (param_2 != 1) {
    return;
  }
  goto LAB_00006dbc;
LAB_00006da0:
  iVar5 = iVar5 + 1;
  pPVar6 = pPVar6 + 1;
  if (3 < iVar5) {
    if (param_2 == 1) {
    LAB_00006dbc:;
      // CpuResumeIntr(local_18[0]);
    }
    return;
  }
  goto LAB_00006d0c;
}

void DisplayQueue() {
  for (int pri = 0; pri < N_PRIORITIES; pri++) {
    for (int cmd = 0; cmd < (int)gPriStack[pri].count; cmd++) {
      lg::debug("  PRI {} elt {} {} @ #x{:X}", pri, cmd, gPriEntryNames[pri][cmd],
                (u64)gPriStack[pri].entries[cmd]);
    }
  }
}

int QueueMessage(CmdHeader* param_1, int param_2, const char* param_3, int param_4) {
  int uVar1;
  // undefined4 local_20[2];

  if (param_4 == 1) {
    // CpuSuspendIntr(local_20);
  }
  if (gPriStack[param_2].count == 8) {
    param_1->status = 2;
    // CpuResumeIntr(local_20[0]);
    ReturnMessage(param_1);
    uVar1 = 0;
  } else {
    gPriStack[param_2].entries[gPriStack[param_2].count] = param_1;
    gPriEntryNames[param_2][gPriStack[param_2].count] = param_3;

    gPriStack[param_2].count = gPriStack[param_2].count + 1;
    if (param_4 == 1) {
      // CpuResumeIntr(local_20[0]);
    }
    uVar1 = 1;
  }
  return uVar1;
}

void UnqueueMessage(CmdHeader* param_1, int param_2) {
  int iVar1;
  PriStackEntry* pPVar2;
  int iVar3;
  PriStackEntry* pPVar4;
  int iVar5;

  if (param_2 == 1) {
    // CpuSuspendIntr(local_18);
  }
  iVar5 = 0;
  pPVar4 = gPriStack;
LAB_00007088:
  iVar3 = 0;
  pPVar2 = pPVar4;
  if (pPVar4->count < 1)
    goto LAB_0000711c;
  do {
    if (pPVar2->entries[0] == param_1)
      break;
    iVar3 = iVar3 + 1;
    pPVar2 = (PriStackEntry*)(pPVar2->entries + 1);
  } while (iVar3 < pPVar4->count);
  iVar1 = pPVar4->count + -1;
  if (pPVar4->count <= iVar3)
    goto LAB_0000711c;
  pPVar4->count = iVar1;
  if (iVar3 < iVar1) {
    do {
      iVar5 = iVar3 + 1;
      pPVar4->entries[iVar3] = pPVar4->entries[iVar3 + 1];
      iVar3 = iVar5;
    } while (iVar5 < pPVar4->count);
  }
  if (param_2 != 1) {
    return;
  }
  goto LAB_00007138;
LAB_0000711c:
  iVar5 = iVar5 + 1;
  pPVar4 = pPVar4 + 1;
  if (3 < iVar5) {
    if (param_2 == 1) {
    LAB_00007138:;
      // CpuResumeIntr(local_18[0]);
    }
    return;
  }
  goto LAB_00007088;
}

CmdHeader* GetMessage() {
  CmdHeader* pCVar1;
  int iVar2;
  CmdHeader** ppCVar3;
  PriStackEntry* iVar4;
  int iVar5;

  iVar5 = 3;
  iVar4 = gPriStack + 3;
  do {
    iVar2 = iVar4->count + -1;
    if (-1 < iVar2) {
      ppCVar3 = iVar4->entries + iVar4->count + -1;
      do {
        pCVar1 = *ppCVar3;
        if ((((pCVar1->lse != (LoadStackEntry*)0x0) && (pCVar1->status == -1)) &&
             (pCVar1->unk_24 != 0)) &&
            ((pCVar1->callback_buffer == (Buffer*)0x0 ||
              (pCVar1->callback_buffer->next == (Buffer*)0x0)))) {
          return pCVar1;
        }
        iVar2 = iVar2 + -1;
        ppCVar3 = ppCVar3 + -1;
      } while (-1 < iVar2);
    }
    iVar5 = iVar5 + -1;
    iVar4 = iVar4 + -1;
    if (iVar5 < 0) {
      return (CmdHeader*)0x0;
    }
  } while (true);
}

void ProcessMessageData() {
  int iVar1;
  CmdHeader* pCVar2;
  Buffer* pBVar3;
  int iVar4;
  CmdHeader** ppCVar5;
  PriStackEntry* iVar6;
  int iVar7;

  iVar7 = 2;
  iVar6 = gPriStack + 2;
  do {
    iVar4 = iVar6->count + -1;
    if (-1 < iVar4) {
      ppCVar5 = iVar6->entries + iVar6->count + -1;
      do {
        pCVar2 = *ppCVar5;
        if ((pCVar2 != (CmdHeader*)0x0) && (pCVar2->unk_24 != 0)) {
          iVar1 = pCVar2->status;
          if (iVar1 == -1) {
            pBVar3 = pCVar2->callback_buffer;
            if (pBVar3 != (Buffer*)0x0) {
              if (pCVar2->callback != ProcessVAGData) {
                iVar1 = (pCVar2->callback)(pCVar2, pBVar3);
                pCVar2->status = iVar1;
                if (pBVar3->decompressed_size == 0) {
                  pCVar2->callback_buffer = pBVar3->next;
                  FreeBuffer(pBVar3, 1);
                }
              }
              iVar1 = pCVar2->status;
            }
            if (iVar1 == -1)
              goto LAB_00007308;
          }

          ReleaseMessage(pCVar2, 1);
          ReturnMessage(pCVar2);
          iVar6 = iVar6 + 1;
          iVar7 = iVar7 + 1;
          break;
        }
      LAB_00007308:
        iVar4 = iVar4 + -1;
        ppCVar5 = ppCVar5 + -1;
      } while (-1 < iVar4);
    }
    iVar7 = iVar7 + -1;
    iVar6 = iVar6 + -1;
    if (iVar7 < 0) {
      return;
    }
  } while (true);
}

void ReturnMessage(CmdHeader* param_1) {
  if (param_1->mbx_to_reply == 0) {
    if (param_1->thread_id == 0) {
      FreeVAGCommand((VagCmd*)param_1);
    } else {
      WakeupThread(param_1->thread_id);
    }
  } else {
    SendMbx(param_1->mbx_to_reply, param_1);
  }
}

VagCmd* GetVAGCommand() {
  int iVar1;
  u32 uVar2;
  VagCmd* pRVar3;

  do {
    while (vag_cmd_cnt == 0x1f) {
      DelayThread(100);
    }
    do {
      iVar1 = WaitSema(sSema);
      uVar2 = 0;
      pRVar3 = vag_cmds;
    } while (iVar1 != 0);
    do {
      if (((int)vag_cmd_used >> (uVar2 & 0x1f) & 1U) == 0) {
        vag_cmd_used = vag_cmd_used | 1 << (uVar2 & 0x1f);
        vag_cmd_cnt = vag_cmd_cnt + 1;
        if ((int)max_vag_cmd_cnt < vag_cmd_cnt) {
          max_vag_cmd_cnt = vag_cmd_cnt;
        }
        SignalSema(sSema);
        return pRVar3;
      }
      uVar2 = uVar2 + 1;
      pRVar3 = pRVar3 + 1;
    } while ((int)uVar2 < 0x1f);
    SignalSema(sSema);
  } while (true);
}

void FreeVAGCommand(VagCmd* param_1) {
  int iVar1;
  u32 uVar2;

  // uVar2 = (param_1 + -0x17e50) * 0x781948b1 >> 2;
  // kinda sus
  uVar2 = (param_1 - vag_cmds) / sizeof(VagCmd);
  if ((uVar2 < 0x1f) && (((int)vag_cmd_used >> (uVar2 & 0x1f) & 1U) != 0)) {
    do {
      iVar1 = WaitSema(sSema);
    } while (iVar1 != 0);
    vag_cmd_used = vag_cmd_used & ~(1 << (uVar2 & 0x1f));
    vag_cmd_cnt = vag_cmd_cnt + -1;
    SignalSema(sSema);
  }
}

uint8_t* CheckForIsoPageBoundaryCrossing(Buffer* param_1) {
  Page* new_page;
  uint8_t* iVar1;
  uint8_t* our_ptr;
  uint8_t* page_end;

  our_ptr = param_1->decomp_buffer;
  page_end = (uint8_t*)param_1->page->ptr;
  if (page_end <= our_ptr) {
    new_page = StepTopPage(param_1->plist, param_1->page);
    param_1->page = new_page;
    if (new_page != (Page*)0x0) {
      iVar1 = new_page->buffer;
      param_1->unk_12 = iVar1;
      param_1->decomp_buffer = page_end + (iVar1 - (our_ptr + -1));
    }
  }
  return param_1->decomp_buffer;
}

void FreeDataBuffer(u32* param_1, u32 param_2) {
  if (param_1 == &BuffersAlloc) {
    BuffersAlloc = BuffersAlloc & ~(1 << (param_2 & 0x1f));
    AllocdBuffersCount = AllocdBuffersCount + -1;
  } else if (param_1 == &StrBuffersAlloc) {
    AllocdStrBuffersCount = AllocdStrBuffersCount + -1;
    StrBuffersAlloc = StrBuffersAlloc & ~(1 << (param_2 & 0x1f));
  }
}

}  // namespace jak2
