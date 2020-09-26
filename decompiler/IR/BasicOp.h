#pragma once

#include <string>
#include "decompiler/Disasm/InstructionMatching.h"

struct BasicOpArg {
  BasicOpArg(Register _r, int _i) : reg(_r), i(_i) {}
  Register reg;
  int i;

  std::string print() const { return reg.to_charp(); }
};

class BasicOp {
 public:
  virtual std::string print() const = 0;
  virtual bool conversion_succeeded() const;
};

class FailedBasicOp : public BasicOp {
 public:
  std::string print() const override;
  bool conversion_succeeded() const override;
};

class RegRegMove : public BasicOp {
 public:
  BasicOpArg dst, src;
  enum Kind { GPR64_GPR64 } kind;

  RegRegMove(BasicOpArg _dst, BasicOpArg _src, Kind _kind);
  std::string print() const override;
};