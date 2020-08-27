#include "GoalLambda.h"

std::string GoalLambdaParam::print() {
  return "(" + name + " " + type.print() + (has_default ? ("  " + default_value.print()) : "") +
         ")";
}

std::string GoalLambda::big_print() {
  std::string result = "--lambda--\n";
  result += "name: " + name;
  result += "\n";

  result += "Body:\n" + body.print() + "\n";
  //  result += "Env: " + env->print() + "\n";
  //  result += "IR:\n";
  //  for(auto& ir : env->code) {
  //    result += " " + ir->print() + "\n";
  //  }
  result += "Params:\n";
  for (auto& p : params) {
    result += " " + p.print() + "\n";
  }

  return result;
}