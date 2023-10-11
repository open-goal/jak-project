#pragma once

/*!
 * @file versions.h
 * Version numbers for GOAL Language, Kernel, etc...
 */

#include <string>
#include <vector>

#include "common/common_types.h"

namespace versions {
// language version (OpenGOAL)
constexpr s32 GOAL_VERSION_MAJOR = 1;
constexpr s32 GOAL_VERSION_MINOR = 0;

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
constexpr u32 LEVEL_FILE_VERSION = 36;
constexpr u32 DGO_FILE_VERSION = 1;
constexpr u32 TX_PAGE_VERSION = 8;
}  // namespace jak2

namespace jak3 {
constexpr u32 ART_FILE_VERSION = 8;
constexpr u32 LEVEL_FILE_VERSION = 36;
constexpr u32 TX_PAGE_VERSION = 8;
}  // namespace jak3

}  // namespace versions

// GOAL kernel version (OpenGOAL changes this version from the game's version)
constexpr int KERNEL_VERSION_MAJOR = 2;
constexpr int KERNEL_VERSION_MINOR = 0;

// OVERLORD version returned by an RPC
constexpr int IRX_VERSION_MAJOR = 2;
constexpr int IRX_VERSION_MINOR = 0;

enum class GameVersion { Jak1 = 1, Jak2 = 2, Jak3 = 3 };

// TODO: most usages of this are currently stubs for jak 3
template <typename T>
struct PerGameVersion {
  constexpr PerGameVersion(T jak1, T jak2, T jak3) : data{jak1, jak2, jak3} {}
  constexpr const T& operator[](GameVersion v) const { return data[(int)v - 1]; }
  T data[3];
};

constexpr PerGameVersion<const char*> game_version_names = {"jak1", "jak2", "jak3"};

GameVersion game_name_to_version(const std::string& name);
bool valid_game_version(const std::string& name);
std::string version_to_game_name(GameVersion v);
std::string version_to_game_name_external(GameVersion v);
std::vector<std::string> valid_game_version_names();

std::string build_revision();
