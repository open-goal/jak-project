#pragma once

#ifndef JAK_LABEL_H
#define JAK_LABEL_H

class FunctionEnv;
struct Label {
  Label() = default;
  Label(FunctionEnv* f, int _idx = -1) : func(f), idx(_idx) {}
  FunctionEnv* func = nullptr;
  int idx = -1;
  std::string print() const { return "label-" + std::to_string(idx); }
};

#endif  // JAK_LABEL_H
