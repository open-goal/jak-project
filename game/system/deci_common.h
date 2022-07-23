#pragma once

#include "common/common_types.h"

struct Deci2Driver {
  u16 protocol = 0;
  void* opt = nullptr;
  void (*handler)(s32 event, s32 param, void* opt) = nullptr;
  u8 id = 0;
  bool active = false;
  void* recv_buffer = nullptr;
  int recv_size = 0;
  int available_to_receive = 0;
  char pending_send = 0;
};

// handler event values
#define DECI2_READ 1
#define DECI2_READDONE 2
#define DECI2_WRITE 3
#define DECI2_WRITEDONE 4
#define DECI2_CHSTATUS 5
