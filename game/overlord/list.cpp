#include "list.h"

#include <cstdio>

#include "game/sce/iop.h"

using namespace iop;

void InitList(List* list, u32 elements, u32 elm_size) {
  list->elements = elements;
  list->buf = (u8*)AllocSysMemory(SMEM_Low, elements * elm_size, nullptr);
  if (!list->buf) {
    printf("IOP: ======================================================================\n");
    printf("IOP: list InitList: no memory for list %s\n", list->name);
    printf("IOP: ======================================================================\n");
    while (true)
      ;
  }

  list->head = (ListElement*)list->buf;

  ListElement* elm = list->head;

  if (elements) {
    for (int i = 0; i < elements; i++) {
      elm->unk0x8 = 0;

      if (i >= elements - 1) {
        elm->next = nullptr;
      } else {
        elm->next = (ListElement*)((uintptr_t)elm + elm_size);
      }

      if (i > 0) {
        elm->prev = (ListElement*)((uintptr_t)elm - elm_size);
      } else {
        elm->prev = nullptr;
      }

      elm = (ListElement*)((uintptr_t)elm + elm_size);
    }
  }

  list->unk0x10 = 0;
  list->unk0x18 = 0;

  SemaParam sema;
  sema.attr = 1;
  sema.init_count = 1;
  sema.max_count = 1;
  sema.option = 0;
  list->sema = CreateSema(&sema);
  if (list->sema < 0) {
    printf("IOP: ======================================================================\n");
    printf("IOP: list InitList: can't create semaphore for list %s\n", list->name);
    printf("IOP: ======================================================================\n");
    while (true)
      ;
  }
}
