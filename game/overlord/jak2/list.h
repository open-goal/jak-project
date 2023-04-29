#pragma once

#include "common/common_types.h"
namespace jak2 {

struct ListNode;

struct List {
  char name[8];
  int sema;              // 12
  int maybe_any_in_use;  // 16
  int elt_count;         // 20
  int unk2_init0;        // 24

  ListNode* next;
  u8* buffer;
};

struct ListNode {
  ListNode* next;
  ListNode* prev;
  int in_use;
};

bool InitList(List* head, u32 elt_count, int elt_size);
}  // namespace jak2