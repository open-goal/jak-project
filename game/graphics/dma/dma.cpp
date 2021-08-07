#include "dma.h"

#include "third-party/fmt/core.h"

std::string DmaTag::print() {
  std::string result;
  const char* mode_names[8] = {"refe", "cnt", "next", "ref", "refs", "call", "ret", "end"};
  result += fmt::format("TAG: 0x{:08x} {:4s} qwc 0x{:04x}", addr, mode_names[(int)kind], qwc);
  if (spr) {
    result += " SPR";
  }
  result += "\n";
  return result;
}
