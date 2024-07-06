#pragma once

#include "common/common_types.h"

namespace jak3 {
void jak3_overlord_init_globals_list();

struct VagStreamData;

struct List {
  char name[8];
  int sema = 0;
  int unk_flag = 0;  // set when there's a free node??
  int count = 0;
  int pending_data = 0;
  VagStreamData* next = nullptr;
  u8* buffer;
};

void InitList(List* list, int num_elements, int element_size);
}  // namespace jak3