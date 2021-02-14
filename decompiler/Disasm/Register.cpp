/*!
 * @file Register.cpp
 * Representation of an EE register.
 */

#include "Register.h"
#include <cassert>
#include <stdexcept>

namespace decompiler {
namespace Reg {
// register which may hold GOAL local variables

// clang-format off
const bool allowed_local_gprs[Reg::MAX_GPR] = {
    false /*R0*/, false /*AT*/, true /*V0*/,  true /*V1*/,
    true /*A0*/,  true /*A1*/,  true /*A2*/,  true /*A3*/,
    true /*T0*/,  true /*T1*/,  true /*T2*/,  true /*T3*/,
    true /*T4*/,  true /*T5*/,  true /*T6*/,  true /*T7*/,
    true /*S0*/,  true /*S1*/,  true /*S2*/,  true /*S3*/,
    true /*S4*/,  true /*S5*/,  false /*S6*/, false /*S7*/,
    true /*T8*/,  true /*T9*/,  false /*K0*/, false /*K1*/,
    true /*GP*/,  true /*SP*/,  false /*FP*/, false /*RA*/
};
// clang-format on
}  // namespace Reg

////////////////////////////
// Register Name Constants
////////////////////////////

const static char* gpr_names[32] = {
    "r0", "at", "v0", "v1", "a0", "a1", "a2", "a3", "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7",
    "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "t8", "t9", "k0", "k1", "gp", "sp", "fp", "ra"};

const static char* fpr_names[32] = {"f0",  "f1",  "f2",  "f3",  "f4",  "f5",  "f6",  "f7",
                                    "f8",  "f9",  "f10", "f11", "f12", "f13", "f14", "f15",
                                    "f16", "f17", "f18", "f19", "f20", "f21", "f22", "f23",
                                    "f24", "f25", "f26", "f27", "f28", "f29", "f30", "f31"};

const static char* cop0_names[32] = {
    "Index",     "Random",    "EntryLo0", "EntryLo1",  "Context",   "PageMask",  "Wired",
    "INVALID7",  "BadVAddr",  "Count",    "EntryHi",   "Compare",   "Status",    "Cause",
    "EPC",       "PRId",      "Config",   "INVALID17", "INVALID18", "INVALID19", "INVALID20",
    "INVALID21", "INVALID22", "BadPAddr", "Debug",     "Perf",      "INVALID26", "INVALID27",
    "TagLo",     "TagHi",     "ErrorEPR", "INVALID31"};

const static char* vf_names[32] = {"vf0",  "vf1",  "vf2",  "vf3",  "vf4",  "vf5",  "vf6",  "vf7",
                                   "vf8",  "vf9",  "vf10", "vf11", "vf12", "vf13", "vf14", "vf15",
                                   "vf16", "vf17", "vf18", "vf19", "vf20", "vf21", "vf22", "vf23",
                                   "vf24", "vf25", "vf26", "vf27", "vf28", "vf29", "vf30", "vf31"};

const static char* vi_names[32] = {
    "vi0",      "vi1",      "vi2",      "vi3",      "vi4",   "vi5",      "vi6",       "vi7",
    "vi8",      "vi9",      "vi10",     "vi11",     "vi12",  "vi13",     "vi14",      "vi15",
    "Status",   "MAC",      "Clipping", "INVALID3", "vi_R",  "vi_I",     "vi_Q",      "INVALID7",
    "INVALID8", "INVALID9", "TPC",      "CMSAR0",   "FBRST", "VPU-STAT", "INVALID14", "CMSAR1"};

const static char* pcr_names[2] = {"pcr0", "pcr1"};

const static char* cop2_macro_special[2] = {"Q", "ACC"};

/////////////////////////////
// Register Names Conversion
/////////////////////////////

namespace {
const char* gpr_to_charp(Reg::Gpr gpr) {
  assert(gpr < 32);
  return gpr_names[gpr];
}

const char* fpr_to_charp(uint32_t fpr) {
  assert(fpr < 32);
  return fpr_names[fpr];
}

const char* cop0_to_charp(Reg::Cop0 cpr) {
  assert(cpr < 32);
  return cop0_names[cpr];
}

const char* vf_to_charp(uint32_t vf) {
  assert(vf < 32);
  return vf_names[vf];
}

const char* vi_to_charp(uint32_t vi) {
  assert(vi < 32);
  return vi_names[vi];
}

const char* pcr_to_charp(uint32_t pcr) {
  assert(pcr < 2);
  return pcr_names[pcr];
}

const char* cop2_macro_special_to_charp(uint32_t reg) {
  assert(reg < 2);
  return cop2_macro_special[reg];
}
}  // namespace

/////////////////////////////
// Register Class
/////////////////////////////
// A register is stored as a 16-bit integer, with the top 8 bits indicating the "kind" and the lower
// 8 bits representing the register id within that kind.  If the integer is -1, it is a special
// "invalid" register used to represent an uninitialized Register.

// Note: VI / COP2 are separate "kinds" of registers, each with 16 registers.
// It might make sense to make this a single "kind" instead?

/*!
 * Create a register. The kind and num must both be valid.
 */
Register::Register(Reg::RegisterKind kind, uint32_t num) {
  id = (kind << 8) | num;

  // check range:
  switch (kind) {
    case Reg::GPR:
    case Reg::FPR:
    case Reg::VF:
    case Reg::COP0:
    case Reg::VI:
      assert(num < 32);
      break;
    case Reg::PCR:
    case Reg::COP2_MACRO_SPECIAL:
      assert(num < 2);
      break;
    default:
      assert(false);
  }
}

Register::Register(const std::string& name) {
  // first try gprs,
  for (int i = 0; i < Reg::MAX_GPR; i++) {
    if (name == gpr_names[i]) {
      id = (Reg::GPR << 8) | i;
      return;
    }
  }

  // next fprs
  for (int i = 0; i < 32; i++) {
    if (name == fpr_names[i]) {
      id = (Reg::FPR << 8) | i;
      return;
    }
  }

  throw std::runtime_error("Unknown register name: " + name);
}

/*!
 * Convert to string. The register must be valid.
 */
const char* Register::to_charp() const {
  switch (get_kind()) {
    case Reg::GPR:
      return gpr_to_charp(get_gpr());
    case Reg::FPR:
      return fpr_to_charp(get_fpr());
    case Reg::VF:
      return vf_to_charp(get_vf());
    case Reg::VI:
      return vi_to_charp(get_vi());
    case Reg::COP0:
      return cop0_to_charp(get_cop0());
    case Reg::PCR:
      return pcr_to_charp(get_pcr());
    case Reg::COP2_MACRO_SPECIAL:
      return cop2_macro_special_to_charp(get_cop2_macro_special());
    default:
      throw std::runtime_error("Unsupported Register");
  }
}

/*!
 * Convert to string. The register must be valid.
 */
std::string Register::to_string() const {
  return {to_charp()};
}

/*!
 * Get the register kind.
 */
Reg::RegisterKind Register::get_kind() const {
  uint16_t kind = id >> 8;
  assert(kind < Reg::MAX_KIND);
  return (Reg::RegisterKind)kind;
}

/*!
 * Get the GPR number. Must be a GPR.
 */
Reg::Gpr Register::get_gpr() const {
  assert(get_kind() == Reg::GPR);
  uint16_t kind = id & 0xff;
  assert(kind < Reg::MAX_GPR);
  return (Reg::Gpr)(kind);
}

/*!
 * Get the FPR number. Must be an FPR.
 */
uint32_t Register::get_fpr() const {
  assert(get_kind() == Reg::FPR);
  uint16_t kind = id & 0xff;
  assert(kind < 32);
  return kind;
}

/*!
 * Get the VF number. Must be a VF.
 */
uint32_t Register::get_vf() const {
  assert(get_kind() == Reg::VF);
  uint16_t kind = id & 0xff;
  assert(kind < 32);
  return kind;
}

/*!
 * Get the VI number. Must be a VI.
 */
uint32_t Register::get_vi() const {
  assert(get_kind() == Reg::VI);
  uint16_t kind = id & 0xff;
  assert(kind < 32);
  return kind;
}

/*!
 * Get the COP0 number. Must be a COP0.
 */
Reg::Cop0 Register::get_cop0() const {
  assert(get_kind() == Reg::COP0);
  uint16_t kind = id & 0xff;
  assert(kind < Reg::MAX_COP0);
  return (Reg::Cop0)(kind);
}

/*!
 * Get the PCR number. Must be a PCR.
 */
uint32_t Register::get_pcr() const {
  assert(get_kind() == Reg::PCR);
  uint16_t kind = id & 0xff;
  assert(kind < 2);
  return kind;
}

Reg::Cop2MacroSpecial Register::get_cop2_macro_special() const {
  assert(get_kind() == Reg::COP2_MACRO_SPECIAL);
  uint16_t k = id & 0xff;
  assert(k < 2);
  return (Reg::Cop2MacroSpecial)k;
}

bool Register::operator==(const Register& other) const {
  return id == other.id;
}

bool Register::operator!=(const Register& other) const {
  return id != other.id;
}
}  // namespace decompiler