#ifndef JAK_COMMON_H
#define JAK_COMMON_H

enum class IntegerMathKind {
  ADD_64,
  SUB_64,
  IMUL_32,
  IDIV_32,
  SHLV_64,
  SARV_64,
  SHRV_64,
  SHL_64,
  SAR_64,
  SHR_64,
  IMOD_32,
  OR_64,
  AND_64,
  XOR_64,
  NOT_64
};

namespace emitter {
enum class RegKind : u8 { GPR, XMM, INVALID };
}

#endif  // JAK_COMMON_H
