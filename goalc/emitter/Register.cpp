#include "Register.h"
#include <stdexcept>

namespace emitter {
RegisterInfo RegisterInfo::make_register_info() {
  RegisterInfo info;

  info.m_info[RAX] = {-1, false, false, "rax"};  // temp
  info.m_info[RCX] = {3, false, false, "rcx"};   // temp
  info.m_info[RDX] = {2, false, false, "rdx"};   // temp
  info.m_info[RBX] = {-1, true, false, "rbx"};   //
  info.m_info[RSP] = {-1, false, true, "rsp"};
  info.m_info[RBP] = {-1, true, false, "rbp"};
  info.m_info[RSI] = {1, false, false, "rsi"};
  info.m_info[RDI] = {0, false, false, "rdi"};

  info.m_info[R8] = {4, false, false, "r8"};
  info.m_info[R9] = {5, false, false, "r9"};
  info.m_info[R10] = {6, true, false, "r10"};
  info.m_info[R11] = {7, true, false, "r11"};
  info.m_info[R12] = {-1, true, false, "r12"};
  info.m_info[R13] = {-1, false, true, "r13"};  // pp?
  info.m_info[R14] = {-1, false, true, "r14"};  // st?
  info.m_info[R15] = {-1, false, true, "r15"};  // offset.

  info.m_info[XMM0] = {-1, false, false, "xmm0"};
  info.m_info[XMM1] = {-1, false, false, "xmm1"};
  info.m_info[XMM2] = {-1, false, false, "xmm2"};
  info.m_info[XMM3] = {-1, false, false, "xmm3"};
  info.m_info[XMM4] = {-1, false, false, "xmm4"};
  info.m_info[XMM5] = {-1, false, false, "xmm5"};
  info.m_info[XMM6] = {-1, false, false, "xmm6"};
  info.m_info[XMM7] = {-1, false, false, "xmm7"};
  info.m_info[XMM8] = {-1, true, false, "xmm8"};
  info.m_info[XMM9] = {-1, true, false, "xmm9"};
  info.m_info[XMM10] = {-1, true, false, "xmm10"};
  info.m_info[XMM11] = {-1, true, false, "xmm11"};
  info.m_info[XMM12] = {-1, true, false, "xmm12"};
  info.m_info[XMM13] = {-1, true, false, "xmm13"};
  info.m_info[XMM14] = {-1, true, false, "xmm14"};
  info.m_info[XMM15] = {-1, true, false, "xmm15"};

  info.m_arg_regs = std::array<Register, N_ARGS>({RDI, RSI, RDX, RCX, R8, R9, R10, R11});
  info.m_saved_gprs = std::array<Register, N_SAVED_GPRS>({RBX, RBP, R10, R11, R12});
  info.m_saved_xmms =
      std::array<Register, N_SAVED_XMMS>({XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15});

  for (size_t i = 0; i < N_SAVED_GPRS; i++) {
    info.m_saved_all[i] = info.m_saved_gprs[i];
  }
  for (size_t i = 0; i < N_SAVED_XMMS; i++) {
    info.m_saved_all[i + N_SAVED_GPRS] = info.m_saved_xmms[i];
  }

  // todo - experiment with better orders for allocation.
  info.m_gpr_alloc_order = {RAX, RCX, RDX, RBX, RBP, RSI, RDI, R8, R9, R10, R11};  // arbitrary
  info.m_xmm_alloc_order = {XMM0, XMM1, XMM2,  XMM3,  XMM4,  XMM5,  XMM6, XMM7,
                            XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14};

  // these should only be temp registers!
  info.m_gpr_temp_only_alloc_order = {RAX, RCX, RDX, RSI, RDI, R8, R9};
  info.m_xmm_temp_only_alloc_order = {XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7};

  info.m_gpr_spill_temp_alloc_order = {RAX, RCX, RDX, RBX, RBP, RSI,
                                       RDI, R8,  R9,  R10, R11, R12};  // arbitrary
  info.m_xmm_spill_temp_alloc_order = {XMM0, XMM1, XMM2,  XMM3,  XMM4,  XMM5,  XMM6,  XMM7,
                                       XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15};
  return info;
}

RegisterInfo gRegInfo = RegisterInfo::make_register_info();

std::string to_string(RegKind kind) {
  switch (kind) {
    case RegKind::GPR:
      return "gpr";
    case RegKind::XMM:
      return "xmm";
    default:
      throw std::runtime_error("Unsupported RegKind");
  }
}

std::string Register::print() const {
  return gRegInfo.get_info(*this).name;
}

}  // namespace emitter