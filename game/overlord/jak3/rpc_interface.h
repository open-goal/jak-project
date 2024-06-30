#pragma once

#include "common/common_types.h"

namespace jak3 {

enum RpcId {
  LoadToEE = 0xfab2,
  DGO = 0xfab3,
};

// RAMDISK RPC (renamed to LoadToEE for jak 3, kinda)
struct RpcLoadToEEMsg {
  u32 unk;
  u32 addr;
  u32 unk2;
  u32 length;
  char name[16];
};
static_assert(sizeof(RpcLoadToEEMsg) == 32);

enum LoadToEEFno {
  LOAD_FILE = 4,
};

struct RPC_Dgo_Cmd {
  uint16_t rsvd;
  uint16_t status;
  uint32_t buffer1;
  uint32_t buffer2;
  uint32_t buffer_heap_top;
  char name[16];
  uint16_t cgo_id;
  uint8_t pad[30];
};
static_assert(sizeof(RPC_Dgo_Cmd) == 0x40);

enum DgoFno {
  LOAD = 0,
  LOAD_NEXT = 1,
  CANCEL = 2,
};
}  // namespace jak3