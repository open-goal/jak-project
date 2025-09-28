#pragma once

#include "common/common_types.h"

namespace jak3 {
void jak3_overlord_init_globals_list();

struct VagStreamData;

/*!
 * The List system is a linked list used to track requested and playing streams.
 * Originally, it supported multiple element types.
 * One of those types is related to plugin streams which are not supported in PC.
 * So, this is just hard-coded to use VagStreamData as the element type.
 */
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