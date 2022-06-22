#pragma once

/*!
 * @file versions.h
 * Version numbers for GOAL Language, Kernel, etc...
 */

#include "common/common_types.h"

namespace versions {
// language version (OpenGOAL)
constexpr s32 GOAL_VERSION_MAJOR = 0;
constexpr s32 GOAL_VERSION_MINOR = 9;

constexpr int DECOMPILER_VERSION = 4;

namespace jak1 {
// these versions are from the game
constexpr u32 ART_FILE_VERSION = 6;
constexpr u32 LEVEL_FILE_VERSION = 30;
constexpr u32 DGO_FILE_VERSION = 1;
constexpr u32 RES_FILE_VERSION = 1;
constexpr u32 TX_PAGE_VERSION = 7;
}  // namespace jak1

namespace jak2 {
constexpr u32 ART_FILE_VERSION = 7;
}

}  // namespace versions

// GOAL kernel version (OpenGOAL changes this version from the game's version)
constexpr int KERNEL_VERSION_MAJOR = 2;
constexpr int KERNEL_VERSION_MINOR = 0;

// OVERLORD version returned by an RPC
constexpr int IRX_VERSION_MAJOR = 2;
constexpr int IRX_VERSION_MINOR = 0;

enum class GameVersion { Jak1 = 1, Jak2 = 2 };