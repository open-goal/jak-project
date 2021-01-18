#pragma once

#include <string>
#include <unordered_map>
#include "common/common_types.h"
#include "common/type_system/TypeSpec.h"

struct GoalEnum {
  TypeSpec base_type;
  bool is_bitfield = false;
  std::unordered_map<std::string, s64> entries;

  bool operator==(const GoalEnum& other) const;
  bool operator!=(const GoalEnum& other) const;
};