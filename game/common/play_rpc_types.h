#pragma once

/*!
 * @file play_rpc_types.h
 * Types used for the play Remote Procedure Call between the EE and the IOP.
 * Note that PLAY and PLAYER are different.
 */
#include "common/versions/versions.h"

// TODO: jak 3 stub
constexpr PerGameVersion<int> PLAY_RPC_ID(0xdeb6, 0xfab5, 0x0);
constexpr int PLAY_RPC_CHANNEL = 5;
