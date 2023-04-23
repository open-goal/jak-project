#include "list.h"

#include <cstdio>

#include "common/util/Assert.h"

#include "game/sce/iop.h"

using namespace iop;
namespace jak2 {

bool InitList(List* head, u32 elt_count, int elt_size) {
  ListNode* buf_ptr;
  int iVar1;
  ListNode** ppLVar2;
  u32 elt_idx;
  SemaParam local_20;

  head->elt_count = elt_count;
  buf_ptr = (ListNode*)AllocSysMemory(0, elt_count * elt_size, 0);
  head->buffer = (u8*)buf_ptr;
  if (!buf_ptr) {
    printf("IOP: ======================================================================\n");
    printf("IOP: list InitList: no memory for list %s\n", head->name);
    printf("IOP: ======================================================================\n");
    ASSERT_NOT_REACHED();
  }
  elt_idx = 0;
  head->next = buf_ptr;
  // suspicious pointer math ahead.
  if (elt_count != 0) {
    ppLVar2 = &buf_ptr->prev;
    do {
      ppLVar2[1] = (ListNode*)0x0;
      if (elt_idx < elt_count - 1) {
        buf_ptr->next = (ListNode*)((u8*)&buf_ptr->next + elt_size);
      } else {
        buf_ptr->next = (ListNode*)0x0;
      }
      if (elt_idx == 0) {
        *ppLVar2 = (ListNode*)0x0;
      } else {
        *ppLVar2 = (ListNode*)((u8*)buf_ptr - elt_size);
      }
      ppLVar2 = (ListNode**)((u8*)ppLVar2 + elt_size);
      elt_idx = elt_idx + 1;
      buf_ptr = (ListNode*)((u8*)&buf_ptr->next + elt_size);
    } while (elt_idx < elt_count);
  }
  head->maybe_any_in_use = 0;
  head->unk2_init0 = 0;
  local_20.attr = 1;
  local_20.init_count = 1;
  local_20.max_count = 1;
  local_20.option = 0;
  iVar1 = CreateSema(&local_20);
  head->sema = iVar1;
  if (iVar1 < 0) {
    printf("IOP: ======================================================================\n");
    printf("IOP: list InitList: can\'t create semaphore for list %s\n", head->name);
    printf("IOP: ======================================================================\n");
    ASSERT_NOT_REACHED();
  }
  return head->buffer != (void*)0x0;
}

ListNode* AddToCircularList(List* param_1) {
  int iVar1;
  ListNode* pLVar2;

  iVar1 = param_1->elt_count;
  pLVar2 = param_1->next;
  while (iVar1 != 0) {
    iVar1 = iVar1 + -1;
    if (pLVar2->in_use != 1)
      goto LAB_00011c14;
    pLVar2 = pLVar2->next;
  }
  if (pLVar2->in_use == 1) {
    pLVar2 = param_1->next->next;
    param_1->next = pLVar2;
    pLVar2 = pLVar2->prev;
  }
LAB_00011c14:
  param_1->maybe_any_in_use = 1;
  pLVar2->in_use = 1;
  return pLVar2;
}

void MakeCircularList(List* lst) {
  ListNode* pLVar1;
  ListNode* pLVar2;
  int iVar3;
  ListNode* pLVar4;

  pLVar4 = lst->next;
  iVar3 = lst->elt_count;
  pLVar1 = pLVar4->next;
  pLVar2 = pLVar4;
  while (true) {
    if (pLVar1 == (ListNode*)0x0) {
      if (iVar3 != 0) {
        pLVar2->next = pLVar4;
        pLVar4->prev = pLVar2;
      }
      return;
    }
    if (iVar3 == 0)
      break;
    pLVar2 = pLVar2->next;
    pLVar1 = pLVar2->next;
    iVar3 = iVar3 + -1;
  }
}

void BreakCircularList(List* param_1) {
  ListNode* pLVar1;
  ListNode* pLVar2;

  pLVar2 = param_1->next;
  pLVar1 = pLVar2->prev;
  if (pLVar1) {
    pLVar1->next = nullptr;
    pLVar2->prev = nullptr;
  }
}
}  // namespace jak2
