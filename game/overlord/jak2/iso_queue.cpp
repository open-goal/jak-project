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
constexpr int N_BUFFERS = 3;
constexpr int N_STR_BUFFERS = 8;
static Buffer sBuffer[N_BUFFERS];
static Buffer sStrBuffer[N_STR_BUFFERS];
static Buffer* sFreeBuffer = nullptr;
static Buffer* sFreeStrBuffer = nullptr;
u32 BuffersAlloc = 0;
u32 StrBuffersAlloc = 0;
u32 AllocdBuffersCount = 0;
u32 NextBuffer = 0;
u32 AllocdStrBuffersCount = 0;
u32 NextStrBuffer = 0;
int sSema = 0;

// note that these are different vag commands from the VagCmds array.
// These are used for the return values of GetVagCommand, and are used for queued commands.
// The VagCmds array is used for commands that are actually running.
u32 vag_cmd_cnt = 0;
VagCmd vag_cmds[32];
u32 vag_cmd_used = 0;
u32 max_vag_cmd_cnt = 0;

PriStackEntry gPriStack[N_PRIORITIES];
std::string gPriEntryNames[N_PRIORITIES][PRI_STACK_LENGTH];  // my addition for debug

void ReturnMessage(CmdHeader* param_1);
void FreeVAGCommand(VagCmd* param_1);
void FreeDataBuffer(u32* param_1, u32 param_2);

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
  for (int i = 0; i < N_BUFFERS; i++) {
    sBuffer[i].next = &sBuffer[i + 1];
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
  sBuffer[N_BUFFERS - 1].next = nullptr;
  sFreeBuffer = sBuffer;
  BuffersAlloc = 0;

  for (int i = 0; i < N_STR_BUFFERS; i++) {
    sStrBuffer[i].next = &sStrBuffer[i + 1];
    sStrBuffer[i].decomp_buffer = nullptr;
    sStrBuffer[i].decompressed_size = 0;
    sStrBuffer[i].unk_12 = 0;
    sStrBuffer[i].data_buffer_idx = -1;
    sStrBuffer[i].use_mode = 2;
    sStrBuffer[i].plist = SpMemoryBuffers;
    sStrBuffer[i].num_pages = 1;
    sStrBuffer[i].unk_32 = 2;
    sStrBuffer[i].free_pages = 0;
    sStrBuffer[i].page = nullptr;
    sStrBuffer[i].unk_44 = 0;
  };
  sStrBuffer[N_STR_BUFFERS - 1].next = nullptr;
  sFreeStrBuffer = sStrBuffer;
  StrBuffersAlloc = 0;

  StreamSRAM[0] = 0x5040;
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

void FreeBuffer(Buffer* buf, int /*param_2*/) {
  // if (param_2 == 1) {
  // CpuSuspendIntr(local_18);
  //}
  if (buf->use_mode == 1) {
    if ((BuffersAlloc & 1 << (buf->data_buffer_idx & 0x1fU)) != 0) {
      buf->page = FreePagesList(buf->plist, buf->page);
      FreeDataBuffer(&BuffersAlloc, buf->data_buffer_idx);
      buf->next = sFreeBuffer;
      buf->decompressed_size = 0;
      buf->unk_12 = 0;
      sFreeBuffer = buf;
      buf->use_mode = 0;
      buf->data_buffer_idx = -1;
    }
  } else if (buf->use_mode == 2) {
    if ((StrBuffersAlloc & 1 << (buf->data_buffer_idx & 0x1fU)) != 0) {
      buf->page = FreePagesList(buf->plist, buf->page);
      FreeDataBuffer(&StrBuffersAlloc, buf->data_buffer_idx);
      buf->next = sFreeStrBuffer;
      buf->decompressed_size = 0;
      buf->unk_12 = 0;
      sFreeStrBuffer = buf;
      buf->use_mode = 0;
      buf->data_buffer_idx = -1;
    }
  }
  // if (param_2 == 1) {
  // CpuResumeIntr(local_18[0]);
  //}
}

void ReleaseMessage(CmdHeader* cmd, int suspend_irq) {
  while (cmd->callback_buffer != nullptr) {
    auto cb_buf = cmd->callback_buffer;
    cmd->callback_buffer = cb_buf->next;
    FreeBuffer(cb_buf, suspend_irq);
  }

  if (cmd->lse != nullptr) {
    isofs->close(cmd->lse);
  }

  UnqueueMessage(cmd, suspend_irq);
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

void UnqueueMessage(CmdHeader* cmd, int suspend_irq) {
  if (suspend_irq == 1) {
    //  CpuSuspendIntr(&stat);
  }

  for (int pri = 0; pri < N_PRIORITIES; pri++) {
    auto pse = &gPriStack[pri];

    for (int idx = 0; idx < pse->count; idx++) {
      if (pse->entries[idx] == cmd) {
        pse->count--;
        while (idx < pse->count) {
          pse->entries[idx] = pse->entries[idx + 1];
          idx++;
        }

        goto exit;
      }
    }
  }

exit:
  if (suspend_irq == 1) {
    //  CpuResumeIntr(stat);
  }
}

CmdHeader* GetMessage() {
  for (int pri = (N_PRIORITIES - 1); pri >= 0; pri--) {
    auto pse = &gPriStack[pri];
    int idx = pse->count;

    for (idx = idx - 1; idx >= 0; idx--) {
      auto cmd = pse->entries[idx];

      if (cmd->lse && (u32)cmd->status == CMD_STATUS_IN_PROGRESS && cmd->ready_for_data) {
        if (cmd->callback_buffer == nullptr) {
          return cmd;
        }
        if (cmd->callback_buffer->next == nullptr) {
          return cmd;
        }
      }
    }
  }

  return nullptr;
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
        if ((pCVar2 != (CmdHeader*)0x0) && (pCVar2->ready_for_data != 0)) {
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
  while (true) {
    while (vag_cmd_cnt == 31) {
      DelayThread(100);
    }

    while (WaitSema(sSema))
      ;

    for (int i = 0; i < 31; i++) {
      // scan bits for free entry
      if (((vag_cmd_used >> i) & 1) == 0) {
        vag_cmd_used |= 1 << i;

        vag_cmd_cnt++;
        if (max_vag_cmd_cnt < vag_cmd_cnt) {
          max_vag_cmd_cnt = vag_cmd_cnt;
        }

        SignalSema(sSema);
        return &vag_cmds[i];
      }
    }

    SignalSema(sSema);
  }
}

void FreeVAGCommand(VagCmd* cmd) {
  u32 vag_idx;

  // get array index
  vag_idx = (cmd - vag_cmds) / sizeof(VagCmd);

  if ((vag_idx < 0x1f) && ((vag_cmd_used >> vag_idx) & 1U) != 0) {
    while (WaitSema(sSema))
      ;

    vag_cmd_used &= ~(1 << vag_idx);
    --vag_cmd_cnt;

    SignalSema(sSema);
  }
}

u8* CheckForIsoPageBoundaryCrossing(Buffer* buf) {
  Page* new_page;
  u8* new_buffer;
  u8* our_ptr;
  u8* page_end;

  our_ptr = buf->decomp_buffer;
  page_end = buf->page->ptr;

  if (page_end <= our_ptr) {
    new_page = StepTopPage(buf->plist, buf->page);
    buf->page = new_page;
    if (new_page != nullptr) {
      new_buffer = new_page->buffer;
      buf->unk_12 = new_buffer;
      buf->decomp_buffer = page_end + (new_buffer - (our_ptr + -1));
    }
  }

  return buf->decomp_buffer;
}

void FreeDataBuffer(u32* param_1, u32 buffer_idx) {
  if (param_1 == &BuffersAlloc) {
    BuffersAlloc &= ~(1 << (buffer_idx & 0x1f));
    --AllocdBuffersCount;
  } else if (param_1 == &StrBuffersAlloc) {
    StrBuffersAlloc &= ~(1 << (buffer_idx & 0x1f));
    --AllocdStrBuffersCount;
  }
}

}  // namespace jak2
