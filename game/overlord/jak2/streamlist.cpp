#include "streamlist.h"

#include <cstdio>
#include <cstring>

#include "game/overlord/jak2/vag.h"
#include "game/sce/iop.h"

using namespace iop;

namespace jak2 {

List PluginStreamsList;
List LfoList;
List EEPlayList;
List RequestedStreamsList;
List NewStreamsList;
List EEStreamsList;

void init_globals_streamlist() {
  memset(&PluginStreamsList, 0, sizeof(PluginStreamsList));
  memset(&LfoList, 0, sizeof(LfoList));
  memset(&EEPlayList, 0, sizeof(EEPlayList));
  memset(&RequestedStreamsList, 0, sizeof(RequestedStreamsList));
  memset(&NewStreamsList, 0, sizeof(NewStreamsList));
  memset(&EEStreamsList, 0, sizeof(EEStreamsList));
}

// TODO
void CheckLfoList(void*) {}
void RemoveLfoStreamFromList(void*, void*) {}

VagStrListNode* InsertVagStreamInList(VagStrListNode* param_1, List* param_2) {
  int iVar1;
  u32 uVar2;
  u32 uVar3;
  VagStrListNode* pLVar3;
  VagStrListNode* pVVar4;
  VagStrListNode* pVVar5;
  VagStrListNode* pVVar6;

  uVar2 = 0;
  uVar3 = param_2->elt_count;
  pLVar3 = (VagStrListNode*)param_2->next;
  pVVar5 = (VagStrListNode*)0x0;
  if (uVar3 != 0) {
    do {
      if (pLVar3->id == 0) {
        uVar2 = uVar3;
        pVVar5 = pLVar3;
      }
      pLVar3 = (VagStrListNode*)(pLVar3->list).next;
      uVar2 = uVar2 + 1;
    } while (uVar2 < uVar3);
    pLVar3 = (VagStrListNode*)param_2->next;
  }
  pVVar6 = (VagStrListNode*)0x0;
  if (pVVar5 == (VagStrListNode*)0x0) {
    printf("IOP: ======================================================================\n");
    printf("IOP: streamlist InsertVagStreamInList: no free spot in list %s\n", param_2->name);
    printf("IOP: ======================================================================\n");
  } else {
    uVar2 = 0;
    if (uVar3 != 0) {
      do {
        pVVar4 = pLVar3;
        uVar2 = uVar2 + 1;
        if (pVVar4->prio < param_1->prio) {
          param_2->maybe_any_in_use = 1;
          (pVVar5->list).in_use = 1;
          strncpy(pVVar5->name, param_1->name, 0x30);
          pVVar5->id = param_1->id;
          pVVar5->unk_68 = param_1->unk_68;
          pVVar5->unk_76 = param_1->unk_76;
          pVVar5->unk_80 = param_1->unk_80;
          pVVar5->prio = param_1->prio;
          iVar1 = param_1->sound_handler;
          pVVar5->unk_92 = 0;
          pVVar5->sound_handler = iVar1;
          pVVar5->vol_multiplier = param_1->vol_multiplier;
          pVVar5->unk_100 = param_1->unk_100;
          if (pVVar6 == (VagStrListNode*)0x0) {
            if (pVVar5 == pVVar4) {
              return pVVar5;
            }
            ((pVVar5->list).next)->prev = (pVVar5->list).prev;
            ((pVVar5->list).prev)->next = (pVVar5->list).next;
            (pVVar4->list).prev = &pVVar5->list;
            (pVVar5->list).next = &pVVar4->list;
            (pVVar5->list).prev = (ListNode*)0x0;
            param_2->next = &pVVar5->list;
            return pVVar5;
          }
          if (pVVar5 == pVVar4) {
            return pVVar5;
          }
          ((pVVar5->list).prev)->next = (pVVar5->list).next;
          ((pVVar5->list).next)->prev = (pVVar5->list).prev;
          (pVVar5->list).next = (pVVar6->list).next;
          (pVVar6->list).next = &pVVar5->list;
          (pVVar4->list).prev = &pVVar5->list;
          (pVVar5->list).prev = &pVVar6->list;
          return pVVar5;
        }
        pLVar3 = (VagStrListNode*)(pVVar4->list).next;
        pVVar6 = pVVar4;
      } while (uVar2 < uVar3);
    }
  }
  return pVVar5;
}

void QueueNewStreamsFromList(List* list) {
  int iVar1;
  u32 uVar2;
  VagCmd* pRVar3;
  // undefined4 *puVar4;
  VagStrListNode* pVVar5;
  VagStrListNode* pvVar6;
  u32 uVar6;
  u32 uVar7;

  SetVagStreamsNotScanned();
  iVar1 = NewStreamsList.elt_count;
  uVar6 = 0;
  uVar7 = 0;
  if (NewStreamsList.elt_count != 0) {
    // puVar4 = (undefined4 *)((int)NewStreamsList.buffer + 8);
    pvVar6 = (VagStrListNode*)NewStreamsList.buffer;
    do {
      strncpy(pvVar6->name, "free", 0x30);
      // puVar4[0xe] = 0;
      pvVar6->id = 0;
      // puVar4[0x10] = 0;
      pvVar6->sound_handler = 0;
      // puVar4[0x13] = 0;
      pvVar6->prio = 0;
      // puVar4[0x11] = 0;
      pvVar6->unk_76 = 0;
      // puVar4[0x12] = 0;
      pvVar6->unk_80 = 0;
      // puVar4[0x14] = 0;
      pvVar6->unk_88 = 0;
      // puVar4[0x15] = 0;
      pvVar6->unk_92 = 0;
      // puVar4[0x16] = 0;
      pvVar6->vol_multiplier = 0;
      // puVar4[0x17] = 0;
      pvVar6->unk_100 = 0;
      // *puVar4 = 0;
      pvVar6->list.in_use = 0;
      // puVar4 = puVar4 + 0x1a;
      uVar6 = uVar6 + 1;
      pvVar6 = pvVar6 + 1;
    } while (uVar6 < (u32)iVar1);
  }
  uVar6 = 0;
  NewStreamsList.maybe_any_in_use = 0;
  do {
    do {
      pVVar5 = (VagStrListNode*)0x0;
      if (uVar7 < (u32)list->elt_count) {
        pVVar5 = (VagStrListNode*)list->next;
        for (uVar2 = uVar7; uVar2 != 0; uVar2 = uVar2 - 1) {
          pVVar5 = (VagStrListNode*)(pVVar5->list).next;
        }
      }
      uVar7 = uVar7 + 1;
      if (pVVar5 == (VagStrListNode*)0x0) {
        uVar6 = 4;
        goto LAB_0000ef9c;
      }
    } while (pVVar5->id == 0);
    pRVar3 = FindThisVagStream(pVVar5->name, pVVar5->id);
    if (pRVar3 == 0x0) {
      pRVar3 = FindThisVagStream(pVVar5->name, pVVar5->id);
      if (pRVar3 != 0x0)
        goto LAB_0000ef9c;
      InsertVagStreamInList(pVVar5, &NewStreamsList);
      uVar6 = uVar6 + 1;
    } else {
      pRVar3->sb_scanned = '\x01';
      if (pRVar3->stereo_sibling != 0x0) {
        pRVar3->stereo_sibling->sb_scanned = '\x01';
      }
      if (pVVar5->prio == pRVar3->priority) {
      LAB_0000ef9c:
        uVar6 = uVar6 + 1;
      } else {
        SetNewVagCmdPri(pRVar3, pVVar5->prio, 1);
        uVar6 = uVar6 + 1;
      }
    }
    if (3 < uVar6) {
      return;
    }
  } while (true);
}

void CheckPlayList(List* param_1) {
  VagCmd* pRVar1;
  int iVar2;
  VagStrListNode* pLVar3;
  VagStrListNode* pVVar3;
  VagStrListNode* pVVar4;
  u32 uVar5;
  u32 uVar6;
  VagStrListNode* pLVar7;
  int iVar7;

  iVar7 = param_1->elt_count;
  pLVar7 = (VagStrListNode*)param_1->next;
joined_r0x0000f00c:
  do {
    while (true) {
      if (iVar7 == 0) {
        return;
      }
      iVar7 = iVar7 + -1;
      if (pLVar7->id != 0)
        break;
      pLVar7 = (VagStrListNode*)(pLVar7->list).next;
    }
    pRVar1 = FindThisVagStream(pLVar7->name, pLVar7->id);
  } while (pRVar1 == 0x0);
  uVar5 = 0;
  if (pRVar1->byte4 == '\0')
    goto code_r0x0000f058;
  uVar6 = param_1->elt_count;
  pVVar3 = (VagStrListNode*)param_1->next;
  pVVar4 = (VagStrListNode*)0x0;
  if (uVar6 != 0) {
    do {
      if ((pVVar3->id == pLVar7->id) &&
          (iVar2 = strncmp(pVVar3->name, pLVar7->name, 0x30), iVar2 == 0)) {
        pVVar4 = pVVar3;
        uVar5 = uVar6;
      }
      pVVar3 = (VagStrListNode*)(pVVar3->list).next;
      uVar5 = uVar5 + 1;
    } while (uVar5 < uVar6);
  }
  goto LAB_0000f144;
code_r0x0000f058:
  if (((pRVar1->sb_playing != '\0') || (pRVar1->byte6 != '\0')) && (pRVar1->byte23 == '\0')) {
    IsoPlayVagStream(pRVar1, 1);
    uVar5 = 0;
    uVar6 = param_1->elt_count;
    pLVar3 = (VagStrListNode*)param_1->next;
    pVVar4 = (VagStrListNode*)0x0;
    if (uVar6 != 0) {
      do {
        if ((pLVar3->id == pLVar7->id) &&
            (iVar2 = strncmp(pLVar3->name, pLVar7->name, 0x30), iVar2 == 0)) {
          pVVar4 = pLVar3;
          uVar5 = uVar6;
        }
        pLVar3 = (VagStrListNode*)(pLVar3->list).next;
        uVar5 = uVar5 + 1;
      } while (uVar5 < uVar6);
    }
  LAB_0000f144:
    if (pVVar4 != (VagStrListNode*)0x0) {
      (pVVar4->list).in_use = 0;
      strncpy(pVVar4->name, "free", 0x30);
      pVVar4->id = 0;
      pVVar4->sound_handler = 0;
      pVVar4->prio = 0;
      pVVar4->unk_76 = 0;
      pVVar4->unk_80 = 0;
      pVVar4->unk_88 = 0;
      pVVar4->unk_92 = 0;
      pVVar4->vol_multiplier = 0;
      pVVar4->unk_100 = 0;
      param_1->maybe_any_in_use = 1;
    }
  }
  goto joined_r0x0000f00c;
}

u32 StreamListThread() {
  int iVar1;
  int iVar2;
  VagStrListNode* pVVar3;
  // undefined4* puVar4;
  VagStrListNode* pVVar5;
  VagStrListNode* pLVar6;
  VagStrListNode* pvVar6;
  u32 uVar7;
  u32 uVar8;
  VagStrListNode* pLVar10;
  u32 uVar9;

  do {
    do {
      SleepThread();
    } while (RequestedStreamsList.unk2_init0 != 0);
    uVar8 = 0;
    WaitSema(RequestedStreamsList.sema);
    iVar1 = RequestedStreamsList.elt_count;
    if (RequestedStreamsList.elt_count != 0) {
      // puVar4 = (undefined4*)((int)RequestedStreamsList.buffer + 8);
      pvVar6 = (VagStrListNode*)RequestedStreamsList.buffer;
      do {
        strncpy(pvVar6->name, "free", 0x30);
        // puVar4[0xe] = 0;
        pvVar6->id = 0;
        // puVar4[0x10] = 0;
        pvVar6->sound_handler = 0;
        // puVar4[0x13] = 0;
        pvVar6->prio = 0;
        // puVar4[0x11] = 0;
        pvVar6->unk_76 = 0;
        // puVar4[0x12] = 0;
        pvVar6->unk_80 = 0;
        // puVar4[0x14] = 0;
        pvVar6->unk_88 = 0;
        // puVar4[0x15] = 0;
        pvVar6->unk_92 = 0;
        // puVar4[0x16] = 0;
        pvVar6->vol_multiplier = 0;
        // puVar4[0x17] = 0;
        pvVar6->unk_100 = 0;
        // *puVar4 = 0;
        pvVar6->list.in_use = 0;
        // puVar4 = puVar4 + 0x1a;
        uVar8 = uVar8 + 1;
        pvVar6++;
        // pvVar6 = (void*)((int)pvVar6 + 0x68);
      } while (uVar8 < (u32)iVar1);
    }
    uVar8 = 0;
    RequestedStreamsList.maybe_any_in_use = 0;
    uVar9 = 0;
    WaitSema(PluginStreamsList.sema);
  LAB_0000f2c4:
    do {
      iVar1 = RequestedStreamsList.elt_count;
      pVVar3 = (VagStrListNode*)0x0;
      uVar7 = uVar8;
      pVVar5 = (VagStrListNode*)PluginStreamsList.next;
      if (uVar8 < (u32)PluginStreamsList.elt_count) {
        for (; pVVar3 = pVVar5, uVar7 != 0; uVar7 = uVar7 - 1) {
          pVVar5 = (VagStrListNode*)(pVVar3->list).next;
        }
      }
      uVar8 = uVar8 + 1;
      if (pVVar3 != (VagStrListNode*)0x0) {
        if (pVVar3->id == 0)
          goto LAB_0000f2c4;
        uVar7 = 0;
        pLVar10 = (VagStrListNode*)0x0;
        pVVar5 = (VagStrListNode*)RequestedStreamsList.next;
        if (RequestedStreamsList.elt_count != 0) {
          do {
            if ((pVVar5->id == pVVar3->id) &&
                (iVar2 = strncmp(pVVar5->name, pVVar3->name, 0x30), iVar2 == 0)) {
              uVar7 = iVar1;
              pLVar10 = pVVar5;
            }
            pVVar5 = (VagStrListNode*)(pVVar5->list).next;
            uVar7 = uVar7 + 1;
          } while (uVar7 < (u32)iVar1);
        }
        if (pLVar10 == (VagStrListNode*)0x0) {
          InsertVagStreamInList(pVVar3, &RequestedStreamsList);
        }
      }
      uVar9 = uVar9 + 1;
    } while (uVar9 < 4);
    uVar8 = 0;
    SignalSema(PluginStreamsList.sema);
    uVar9 = 0;
    WaitSema(EEStreamsList.sema);
  LAB_0000f3c0:
    do {
      iVar1 = RequestedStreamsList.elt_count;
      pVVar3 = (VagStrListNode*)0x0;
      uVar7 = uVar8;
      pVVar5 = (VagStrListNode*)EEStreamsList.next;
      if (uVar8 < (u32)EEStreamsList.elt_count) {
        for (; pVVar3 = pVVar5, uVar7 != 0; uVar7 = uVar7 - 1) {
          pVVar5 = (VagStrListNode*)(pVVar3->list).next;
        }
      }
      uVar8 = uVar8 + 1;
      if (pVVar3 != (VagStrListNode*)0x0) {
        if (pVVar3->id == 0)
          goto LAB_0000f3c0;
        uVar7 = 0;
        pVVar5 = (VagStrListNode*)0x0;
        pLVar6 = (VagStrListNode*)RequestedStreamsList.next;
        if (RequestedStreamsList.elt_count != 0) {
          do {
            if ((pLVar6->id == pVVar3->id) &&
                (iVar2 = strncmp(pLVar6->name, pVVar3->name, 0x30), iVar2 == 0)) {
              uVar7 = iVar1;
              pVVar5 = pLVar6;
            }
            pLVar6 = (VagStrListNode*)(pLVar6->list).next;
            uVar7 = uVar7 + 1;
          } while (uVar7 < (u32)iVar1);
        }
        if (pVVar5 == (VagStrListNode*)0x0) {
          InsertVagStreamInList(pVVar3, &RequestedStreamsList);
        }
      }
      uVar9 = uVar9 + 1;
    } while (uVar9 < 4);
    SignalSema(EEStreamsList.sema);
    RequestedStreamsList.unk2_init0 = 1;
    SignalSema(RequestedStreamsList.sema);
    WaitSema(EEPlayList.sema);
    CheckPlayList(&EEPlayList);
    SignalSema(EEPlayList.sema);
    WaitSema(LfoList.sema);
    CheckLfoList(&LfoList);
    SignalSema(LfoList.sema);
  } while (true);
  return 0;
}

bool InitVagStreamList(List* param_1, u32 param_2, const char* param_3) {
  // uint8_t* piVar1;
  u32 uVar1;
  VagStrListNode* pLVar3;

  strncpy(param_1->name, param_3, 8);
  InitList(param_1, param_2, sizeof(VagStrListNode));
  pLVar3 = (VagStrListNode*)param_1->next;
  uVar1 = 0;
  if (param_2 != 0) {
    // piVar1 = (uint8_t*)&pLVar3->unk_100;
    do {
      // *(undefined4*)(piVar1 + -0x5c) = 0;
      pLVar3->list.in_use = 0;
      strncpy(pLVar3->name, "free", 0x30);
      // *(undefined4*)(piVar1 + -0x24) = 0;
      pLVar3->id = 0;
      // *(undefined4*)(piVar1 + -0x1c) = 0;
      pLVar3->sound_handler = 0;
      // *(undefined4*)(piVar1 + -0x10) = 0;
      pLVar3->prio = 0;
      // *(undefined4*)(piVar1 + -0x18) = 0;
      pLVar3->unk_76 = 0;
      // *(undefined4*)(piVar1 + -0x14) = 0;
      pLVar3->unk_80 = 0;
      // *(undefined4*)(piVar1 + -0xc) = 0;
      pLVar3->unk_88 = 0;
      // *(undefined4*)(piVar1 + -8) = 0;
      pLVar3->unk_92 = 0;
      // *(undefined4*)(piVar1 + -4) = 0;
      pLVar3->vol_multiplier = 0;
      // *(undefined4*)piVar1 = 0;
      pLVar3->unk_100 = 0;
      // piVar1 = piVar1 + 0x68;
      uVar1 = uVar1 + 1;
      pLVar3 = pLVar3 + 1;
    } while (uVar1 < param_2);
  }
  return param_1->buffer != (void*)0x0;
}

VagStrListNode* FindVagStreamInList(VagStrListNode* param_1, List* param_2) {
  int iVar1;
  VagStrListNode* pLVar2;
  u32 uVar2;
  u32 uVar3;
  VagStrListNode* pVVar4;

  uVar2 = 0;
  uVar3 = param_2->elt_count;
  pLVar2 = (VagStrListNode*)param_2->next;
  pVVar4 = (VagStrListNode*)0x0;
  if (uVar3 != 0) {
    do {
      if ((pLVar2->id == param_1->id) &&
          (iVar1 = strncmp(pLVar2->name, param_1->name, 0x30), iVar1 == 0)) {
        uVar2 = uVar3;
        pVVar4 = pLVar2;
      }
      pLVar2 = (VagStrListNode*)(pLVar2->list).next;
      uVar2 = uVar2 + 1;
    } while (uVar2 < uVar3);
  }
  return pVVar4;
}

ListNode* GetVagStreamInList(u32 param_1, List* param_2) {
  ListNode* pLVar1;

  pLVar1 = (ListNode*)0x0;
  if ((param_1 < (u32)param_2->elt_count) && (pLVar1 = param_2->next, param_1 != 0)) {
    do {
      pLVar1 = pLVar1->next;
      param_1 = param_1 - 1;
    } while (param_1 != 0);
  }
  return pLVar1;
}

void RemoveVagStreamFromList(VagStrListNode* param_1, List* param_2) {
  int iVar1;
  VagStrListNode* pLVar2;
  VagStrListNode* pVVar2;
  u32 uVar3;
  u32 uVar4;

  uVar3 = 0;
  uVar4 = param_2->elt_count;
  pLVar2 = (VagStrListNode*)param_2->next;
  pVVar2 = (VagStrListNode*)0x0;
  if (uVar4 != 0) {
    do {
      if ((pLVar2->id == param_1->id) &&
          (iVar1 = strncmp(pLVar2->name, param_1->name, 0x30), iVar1 == 0)) {
        pVVar2 = pLVar2;
        uVar3 = uVar4;
      }
      pLVar2 = (VagStrListNode*)(pLVar2->list).next;
      uVar3 = uVar3 + 1;
    } while (uVar3 < uVar4);
  }
  if (pVVar2 != (VagStrListNode*)0x0) {
    (pVVar2->list).in_use = 0;
    strncpy(pVVar2->name, "free", 0x30);
    pVVar2->id = 0;
    pVVar2->sound_handler = 0;
    pVVar2->prio = 0;
    pVVar2->unk_76 = 0;
    pVVar2->unk_80 = 0;
    pVVar2->unk_88 = 0;
    pVVar2->unk_92 = 0;
    pVVar2->vol_multiplier = 0;
    pVVar2->unk_100 = 0;
    param_2->maybe_any_in_use = 1;
  }
}

void EmptyVagStreamList(List* param_1) {
  // undefined4 *puVar1;
  VagStrListNode* pvVar2;
  u32 uVar3;
  u32 uVar4;

  uVar4 = param_1->elt_count;
  pvVar2 = (VagStrListNode*)param_1->buffer;
  uVar3 = 0;
  if (uVar4 != 0) {
    // puVar1 = (undefined4 *)((int)pvVar2 + 8);
    do {
      strncpy(pvVar2->name, "free", 0x30);
      //      puVar1[0xe] = 0;
      //      puVar1[0x10] = 0;
      //      puVar1[0x13] = 0;
      //      puVar1[0x11] = 0;
      //      puVar1[0x12] = 0;
      //      puVar1[0x14] = 0;
      //      puVar1[0x15] = 0;
      //      puVar1[0x16] = 0;
      //      puVar1[0x17] = 0;
      //      *puVar1 = 0;
      // puVar1 = puVar1 + 0x1a;

      pvVar2->id = 0;
      pvVar2->sound_handler = 0;
      pvVar2->prio = 0;
      pvVar2->unk_76 = 0;
      pvVar2->unk_80 = 0;
      pvVar2->unk_88 = 0;
      pvVar2->unk_92 = 0;
      pvVar2->vol_multiplier = 0;
      pvVar2->unk_100 = 0;

      uVar3 = uVar3 + 1;
      pvVar2++;
    } while (uVar3 < uVar4);
  }
  param_1->maybe_any_in_use = 1;
}

void MergeVagStreamLists(List* param_1, List* param_2) {
  u32 uVar1;
  int iVar2;
  VagStrListNode* pVVar3;
  VagStrListNode* pLVar4;
  u32 uVar4;
  VagStrListNode* pVVar5;
  u32 uVar6;
  u32 uVar7;

  uVar6 = 0;
  uVar7 = 0;
  do {
    do {
      pVVar3 = (VagStrListNode*)0x0;
      if (uVar6 < (u32)param_1->elt_count) {
        pVVar3 = (VagStrListNode*)param_1->next;
        for (uVar1 = uVar6; uVar1 != 0; uVar1 = uVar1 - 1) {
          pVVar3 = (VagStrListNode*)(pVVar3->list).next;
        }
      }
      uVar6 = uVar6 + 1;
      if (pVVar3 == (VagStrListNode*)0x0)
        goto LAB_0000f930;
    } while (pVVar3->id == 0);
    uVar4 = param_2->elt_count;
    uVar1 = 0;
    pLVar4 = (VagStrListNode*)param_2->next;
    pVVar5 = (VagStrListNode*)0x0;
    if (uVar4 != 0) {
      do {
        if ((pLVar4->id == pVVar3->id) &&
            (iVar2 = strncmp(pLVar4->name, pVVar3->name, 0x30), iVar2 == 0)) {
          uVar1 = uVar4;
          pVVar5 = pLVar4;
        }
        pLVar4 = (VagStrListNode*)(pLVar4->list).next;
        uVar1 = uVar1 + 1;
      } while (uVar1 < uVar4);
    }
    if (pVVar5 == (VagStrListNode*)0x0) {
      InsertVagStreamInList(pVVar3, param_2);
    }
  LAB_0000f930:
    uVar7 = uVar7 + 1;
    if (3 < uVar7) {
      return;
    }
  } while (true);
}

}  // namespace jak2
