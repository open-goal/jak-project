#include <cassert>
#include "BasicOp.h"

bool BasicOp::conversion_succeeded() const {
  return true;
}

std::string FailedBasicOp::print() const {
  return "Failed to convert";
}

bool FailedBasicOp::conversion_succeeded() const {
  return false;
}

RegRegMove::RegRegMove(BasicOpArg _dst, BasicOpArg _src, Kind _kind)
    : dst(_dst), src(_src), kind(_kind) {}

std::string RegRegMove::print() const {
  switch (kind) {
    case GPR64_GPR64:
      return "(m64! " + dst.print() + " " + src.print() + ")";
    default:
      assert(false);
  }
}