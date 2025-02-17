#pragma once

#include <string>

#include "common/common_types.h"

inline u64 fnv64(const void* data, u64 len) {
  u64 ret = 0xcbf29ce484222325;
  const auto* ptr = (const u8*)data;
  for (u64 i = 0; i < len; i++) {
    ret = 1099511628211 * (((u64)*ptr) ^ ret);
    ptr++;
  }
  return ret;
}

inline u64 fnv64(const std::string& str) {
  return fnv64(str.data(), str.length());
}