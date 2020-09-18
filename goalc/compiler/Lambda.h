#pragma once

#ifndef JAK_LAMBDA_H
#define JAK_LAMBDA_H

#include "common/goos/Object.h"

// note - we cannot easily reuse the GOOS argument system because GOAL's is slightly different.
// there's no rest or keyword support.
struct GoalArg {
  std::string name;
  TypeSpec type;
};

struct Lambda {
  std::string debug_name;
  std::vector<GoalArg> params;
  goos::Object body;
};

#endif  // JAK_LAMBDA_H
