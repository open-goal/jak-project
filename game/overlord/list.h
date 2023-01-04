#ifndef LIST_H_
#define LIST_H_

#include "common/common_types.h"

struct ListElement {
  ListElement *next, *prev;
};

struct List {
  char name[8];
  s32 sema;
  u32 elements;
  int unk0x10;
  int unk0x18;
  ListElement* head;
  u8* buf;
};

void InitList(List* list, u32 elements, u32 elm_size);

#endif  // LIST_H_
