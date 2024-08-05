#include "list.h"

#include "common/util/Assert.h"

#include "game/overlord/jak3/vag.h"
#include "game/sce/iop.h"

namespace jak3 {
using namespace iop;
void jak3_overlord_init_globals_list() {}

void InitList(List* list, int count, int element_size) {
  VagStreamData* iter;
  int iVar1;
  SemaParam sema_params;

  list->count = count;
  iter = (VagStreamData*)AllocSysMemory(0, count * element_size, 0);
  ASSERT(iter);
  ASSERT(element_size == sizeof(VagStreamData));
  list->buffer = (u8*)iter;
  if (iter != (VagStreamData*)0x0) {
    list->next = iter;
    iVar1 = 0;
    if (0 < count) {
      do {
        iter->in_use = 0;
        if (iVar1 < count + -1) {
          iter->next = (VagStreamData*)((u8*)iter + element_size);
        } else {
          iter->next = nullptr;
        }
        if (iVar1 == 0) {
          iter->prev = nullptr;
        } else {
          iter->prev = (VagStreamData*)((u8*)iter - element_size);
        }
        iVar1 = iVar1 + 1;
        iter = (VagStreamData*)((u8*)iter + element_size);
      } while (iVar1 < count);
    }
    list->unk_flag = 0;
    list->pending_data = 0;
    sema_params.max_count = 1;
    sema_params.attr = 1;
    sema_params.init_count = 1;
    sema_params.option = 0;
    iVar1 = CreateSema(&sema_params);
    list->sema = iVar1;
    ASSERT(list->sema >= 0);
  }
}

}  // namespace jak3