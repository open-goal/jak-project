#pragma once

namespace jak3 {
void jak3_overlord_init_globals_list();

struct VagStreamData;

struct List {
  int sema = 0;
  int pending_data = 0;
  VagStreamData* next = nullptr;
};
}  // namespace jak3