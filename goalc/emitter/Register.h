/*!
 * @file Register.h
 * Representation of an x86-64 Register.
 */

#ifndef JAK_REGISTER_H
#define JAK_REGISTER_H

#include <cassert>
#include <functional>
#include <array>
#include "common/common_types.h"

namespace emitter {

// registers by name
enum X86_REG : u8 {
  RAX,  // return, temp
  RCX,  // arg 3
  RDX,  // arg 2
  RBX,  // X saved

  RSP,  // stack pointer
  RBP,  // X base pointer (like fp)
  RSI,  // arg 1
  RDI,  // arg 0

  R8,   // arg 4
  R9,   // arg 5, saved
  R10,  // arg 6, saved (arg in GOAL only)
  R11,  // arg 7, saved (arg in GOAL only)
  R12,  // X saved - pp register (like s6)
  R13,  // X saved - function call register (like t9)
  R14,  // X saved - offset (added in GOAL x86)
  R15,  // X saved - st (like s7)
  XMM0,
  XMM1,
  XMM2,
  XMM3,
  XMM4,
  XMM5,
  XMM6,
  XMM7,
  XMM8,
  XMM9,
  XMM10,
  XMM11,
  XMM12,
  XMM13,
  XMM14,
  XMM15
};

constexpr int N_REGS = 32;
static_assert(N_REGS - 1 == XMM15, "bad register count");

class Register {
 public:
  Register() = default;

  // intentionally not explicit so we can use X86_REGs in place of Registers
  Register(int id) : m_id(id) {}

  bool is_xmm() { return m_id >= XMM0 && m_id <= XMM15; }

  bool is_gpr() { return m_id >= RAX && m_id <= R15; }

  int hw_id() {
    if (is_xmm()) {
      return m_id - XMM0;
    } else if (is_gpr()) {
      return m_id - RAX;
    } else {
      assert(false);
    }
    return 0xff;
  }

  struct hash {
    auto operator()(const Register& x) const { return std::hash<u8>()(x.m_id); }
  };

  bool operator==(const Register& x) const {
    return m_id == x.m_id;
  }

  bool operator!=(const Register& x) const {
    return m_id != x.m_id;
  }

 private:
  u8 m_id = 0xff;
};



class RegisterInfo {
  struct Info {
    int argument_id = -1; // -1 if not argument
    bool saved = false; // does the callee save it?
    bool special = false; // is it a special GOAL register?
    std::string name;
  };

  const Info& get_info(Register r);

  int get_arg_reg_count();
  Register get_arg_reg(int id);

  int get_saved_reg_count();
  Register get_saved_reg(int id);

  Register get_process_reg();
  Register get_st_reg();
  Register get_offset_reg();
  Register get_ret_reg();
};


}  // namespace emitter

#endif  // JAK_REGISTER_H
