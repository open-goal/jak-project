#ifndef JAK_LABEL_H
#define JAK_LABEL_H

class FunctionEnv;

struct Label {
  Label() = default;
  Label(FunctionEnv* _func, int _idx = -1) : func(_func), idx(_idx) {}
  FunctionEnv* func;
  int idx;
  std::string print() { return "LABEL-" + std::to_string(idx); }
};

#endif  // JAK_LABEL_H
