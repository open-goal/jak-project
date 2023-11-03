#pragma once

/*!
 * @file dgo_rpc_types.h
 * Types used for the DGO Remote Procedure Call between the EE and the IOP
 */

#include "common/common_types.h"
#include "common/versions/versions.h"

// TODO: jak 3 stub
constexpr PerGameVersion<int> DGO_RPC_ID(0xdeb4, 0xfab3, 0x0);
constexpr int DGO_RPC_CHANNEL = 3;
constexpr int DGO_RPC_LOAD_FNO = 0;
constexpr int DGO_RPC_LOAD_NEXT_FNO = 1;
constexpr int DGO_RPC_CANCEL_FNO = 2;
constexpr int DGO_RPC_RESULT_INIT = 666;
constexpr int DGO_RPC_RESULT_ABORTED = 3;
constexpr int DGO_RPC_RESULT_MORE = 2;
constexpr int DGO_RPC_RESULT_ERROR = 1;
constexpr int DGO_RPC_RESULT_DONE = 0;

struct RPC_Dgo_Cmd {
  uint16_t rsvd;
  uint16_t result;
  uint32_t buffer1;
  uint32_t buffer2;
  uint32_t buffer_heap_top;
  char name[16];
};
