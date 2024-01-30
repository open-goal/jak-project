#pragma once

/*!
 * @file player_rpc_types.h
 * Types used for the player Remote Procedure Call between the EE and the IOP.
 * Note that PLAY and PLAYER are different.
 */
#include "common/versions/versions.h"

constexpr PerGameVersion<int> PLAYER_RPC_ID(0xdeb1, 0xfab0, 0xfab0);
constexpr int PLAYER_RPC_CHANNEL = 0;
