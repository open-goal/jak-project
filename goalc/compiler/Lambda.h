#pragma once

#include "common/goos/Object.h"
#include "common/type_system/TypeSpec.h"

// note - we cannot easily reuse the GOOS argument system because GOAL's is slightly different.
// there's no rest or keyword support.
struct GoalArg {
  std::string name;  // todo intern
  TypeSpec type;
};

struct Lambda {
  std::string debug_name;
  std::vector<GoalArg> params;
  goos::Object body;
};

struct InlineableFunction {
  Lambda lambda;
  TypeSpec type;
  bool inline_by_default = false;
};
