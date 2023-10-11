#pragma once

/*!
 * @file loader_rpc_types.h
 * Types used for the Loader Remote Procedure Call between the EE and the IOP
 */

#include "common/versions/versions.h"

// TODO: jak 3 stub
constexpr PerGameVersion<int> LOADER_RPC_ID(0xdeb2, 0xfab1, 0x0);
constexpr int LOADER_RPC_CHANNEL = 1;
