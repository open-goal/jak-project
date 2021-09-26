#pragma once

#include <unordered_map>
#include <vector>
#include <string>
#include <cstring>

#include "game/kernel/Ptr.h"
#include "common/common_types.h"
#include "common/util/assert.h"

namespace Mips2C {

class LinkedFunctionTable {
 public:
  void reg(const std::string& name, u64 (*exec)(void*), u32 goal_stack_size);
  u32 get(const std::string& name);

 private:
  struct Func {
    u64 (*c_func)(void*);
    Ptr<u8> goal_trampoline;
  };
  std::unordered_map<std::string, Func> m_executes;
};

extern std::unordered_map<std::string, std::vector<void (*)()>> gMips2CLinkCallbacks;
extern LinkedFunctionTable gLinkedFunctionTable;

struct Rng {
  Rng() { init(); }
  float R = 0.;

  u32 R_u32() {
    u32 result;
    memcpy(&result, &R, 4);
    return result;
  }

  float from23_bits(float in) {
    u32 val;
    memcpy(&val, &in, 4);
    val = 0x3F800000 | (0x0007FFFFF & val);
    memcpy(&in, &val, 4);
    return in;
  }

  float from23_bits(u32 val) {
    val = 0x3F800000 | (0x0007FFFFF & val);
    float out;
    memcpy(&out, &val, 4);
    return out;
  }

  void init(float rinit = 1.418091058731079f) { R = from23_bits(rinit); }

  void advance() {
    u32 r32 = R_u32();
    u32 x = 1 & (r32 >> 4);
    u32 y = 1 & (r32 >> 22);
    r32 <<= 1;
    r32 = r32 ^ x ^ y;
    R = from23_bits(r32);
  }

  void rxor(u32 in) { R = from23_bits(in ^ R_u32()); }
};

extern Rng gRng;
}  // namespace Mips2C