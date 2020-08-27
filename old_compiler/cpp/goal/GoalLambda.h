#ifndef JAK_GOALLAMBDA_H
#define JAK_GOALLAMBDA_H

#include <string>
#include "goos/Object.h"
#include "GoalType.h"

enum LambdaKind { FUNCTION, METHOD, BEHAVIOR };

struct GoalLambdaParam {
  std::string name;
  Object default_value;
  TypeSpec type;

  bool has_default = false;
  bool is_keyword = false;

  GoalLambdaParam(const std::string& param_name, TypeSpec type_spec) {
    name = param_name;
    type = type_spec;
    has_default = false;
    is_keyword = (param_name.at(0) == ':');
  }

  GoalLambdaParam() = default;

  std::string print();
};

class GoalLambda {
 public:
  Object body;
  // std::shared_ptr<FunctionEnv> env;
  std::vector<GoalLambdaParam> params;
  std::string name;

  std::string big_print();
};

#endif  // JAK_GOALLAMBDA_H
