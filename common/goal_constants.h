#ifndef JAK_GOAL_CONSTANTS_H
#define JAK_GOAL_CONSTANTS_H

constexpr int POINTER_SIZE = 4;
constexpr int BASIC_OFFSET = 4;
constexpr int STRUCTURE_ALIGNMENT = 16;

enum class RegKind {
  GPR_64,
  FLOAT,
  INT_128,
  FLOAT_4X,
  INVALID
};

#endif  // JAK_GOAL_CONSTANTS_H
