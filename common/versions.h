#pragma once

/*!
 * @file versions.h
 * Version numbers for GOAL Language, Kernel, etc...
 */

#ifndef JAK1_VERSIONS_H
#define JAK1_VERSIONS_H

#include "common/common_types.h"

namespace versions {
// language version (OpenGOAL)
constexpr s32 GOAL_VERSION_MAJOR = 0;
constexpr s32 GOAL_VERSION_MINOR = 4;

// these versions are from the game
constexpr u32 ART_FILE_VERSION = 6;
constexpr u32 LEVEL_FILE_VERSION = 30;
constexpr u32 DGO_FILE_VERSION = 1;
constexpr u32 RES_FILE_VERSION = 1;
constexpr u32 TX_PAGE_VERSION = 7;
}  // namespace versions

// GOAL kernel version (OpenGOAL changes this version from the game's version)
constexpr int KERNEL_VERSION_MAJOR = 2;
constexpr int KERNEL_VERSION_MINOR = 0;

#endif  // JAK1_VERSIONS_H
