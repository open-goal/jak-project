/*!
 * @file GoalEnum.h
 * Implementation of GOAL Enums
 */

#ifndef JAK_V2_GOALENUM_H
#define JAK_V2_GOALENUM_H

#include "GoalType.h"

struct GoalEnum {
  TypeSpec ts;
  bool is_bitfield;
  std::unordered_map<std::string, int64_t> entries;

  bool operator==(const GoalEnum& other);
  bool operator!=(const GoalEnum& other);
};

#endif  // JAK_V2_GOALENUM_H
