#pragma once

#include <cstddef>

#include "common/common_types.h"

namespace jak3 {

struct Vec3 {
  s32 x, y, z;
};

enum class EIsoStatus { Unk };

int start_overlord_wrapper(int argc, const char* const* argv, bool* signal);
void Panic();
char* strncpyz(char* dst, const char* src, size_t sz);

void InitSound();

int VBlank_Initialize();
}  // namespace jak3
