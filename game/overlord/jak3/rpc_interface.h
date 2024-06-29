#pragma once

#include "common/common_types.h"

namespace jak3 {

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

enum RpcId {
  LoadToEE = 0xfab2,
};

}  // namespace jak3