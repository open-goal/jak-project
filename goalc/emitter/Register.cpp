#include "Register.h"

#include <stdexcept>

namespace emitter {
RegisterInfo RegisterInfo::make_register_info() {
  RegisterInfo info;

  info.m_info[RAX] = {false, false, "rax"};    // return, temp
  info.m_info[RCX] = {false, false, "rcx"};    // gpr arg 3, temp
  info.m_info[RDX] = {false, false, "rdx"};    // gpr arg 2, temp
  info.m_info[RBX] = {true, false, "rbx", 8};  // saved
  info.m_info[RSP] = {false, true, "rsp", 8};  // stack pointer
  info.m_info[RBP] = {true, false, "rbp", 8};  // saved
  info.m_info[RSI] = {false, false, "rsi"};    // gpr arg 1, temp
  info.m_info[RDI] = {false, false, "rdi"};    // gpr arg 0, temp

  info.m_info[R8] = {false, false, "r8"};      // gpr arg 4, temp
  info.m_info[R9] = {false, false, "r9"};      // gpr arg 5, temp
  info.m_info[R10] = {true, false, "r10", 8};  // gpr arg 6, saved
  info.m_info[R11] = {true, false, "r11", 8};  // gpr arg 7, saved
  info.m_info[R12] = {true, false, "r12", 8};  // saved
  info.m_info[R13] = {false, true, "r13", 8};  // pp
  info.m_info[R14] = {false, true, "r14", 8};  // st
  info.m_info[R15] = {false, true, "r15", 8};  // offset.

  info.m_info[XMM0] = {false, false, "xmm0"};
  info.m_info[XMM1] = {false, false, "xmm1"};
  info.m_info[XMM2] = {false, false, "xmm2"};
  info.m_info[XMM3] = {false, false, "xmm3"};
  info.m_info[XMM4] = {false, false, "xmm4"};
  info.m_info[XMM5] = {false, false, "xmm5"};
  info.m_info[XMM6] = {false, false, "xmm6"};
  info.m_info[XMM7] = {false, false, "xmm7"};
  info.m_info[XMM8] = {true, false, "xmm8", 16};
  info.m_info[XMM9] = {true, false, "xmm9", 16};
  info.m_info[XMM10] = {true, false, "xmm10", 16};
  info.m_info[XMM11] = {true, false, "xmm11", 16};
  info.m_info[XMM12] = {true, false, "xmm12", 16};
  info.m_info[XMM13] = {true, false, "xmm13", 16};
  info.m_info[XMM14] = {true, false, "xmm14", 16};
  info.m_info[XMM15] = {true, false, "xmm15", 16};
  for (int i = XMM15 + 1; i < N_REGS; i++) {
    info.m_info[i] = {false, true, ""};
  }

  info.m_gpr_arg_regs = std::array<Register, N_ARGS>({RDI, RSI, RDX, RCX, R8, R9, R10, R11});
  // skip xmm0 so it can be used for return.
  info.m_xmm_arg_regs =
      std::array<Register, N_ARGS>({XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8});
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
  info.m_gpr_alloc_order = {RAX, RCX, RDX, RBX, RBP, RSI, RDI, R8, R9, R10};  // arbitrary
  info.m_xmm_alloc_order = {XMM0, XMM1, XMM2, XMM3,  XMM4,  XMM5,  XMM6,
                            XMM7, XMM8, XMM9, XMM10, XMM11, XMM12, XMM13};

  // these should only be temp registers!
  info.m_gpr_temp_only_alloc_order = {RAX, RCX, RDX, RSI, RDI, R8, R9};
  info.m_xmm_temp_only_alloc_order = {XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7};

  info.m_gpr_spill_temp_alloc_order = {RAX, RCX, RDX, RBX, RBP, RSI,
                                       RDI, R8,  R9,  R10, R11, R12};  // arbitrary
  info.m_xmm_spill_temp_alloc_order = {XMM0, XMM1, XMM2,  XMM3,  XMM4,  XMM5,  XMM6,  XMM7,
                                       XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15};

  info.m_process_reg = R13;
  info.m_st_reg = R14;
  info.m_offset_reg = R15;
  // x86 uses the offset register as its executable base.
  info.m_exec_base_reg = R15;
  info.m_stack_reg = RSP;
  info.m_gpr_ret_reg = RAX;
  info.m_xmm_ret_reg = XMM0;
  return info;
}

/*!
 * ARM64 register layout for AAPCS64 and GOAL.
 */
RegisterInfo RegisterInfo::make_register_info_arm64() {
  RegisterInfo info;

  // x0 through x15 are caller-saved
  // x8 also carries the indirect result pointer
  const char* temp_names[16] = {"x0", "x1", "x2",  "x3",  "x4",  "x5",  "x6",  "x7",
                                "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15"};
  for (int i = 0; i <= X15; i++) {
    info.m_info[i] = {false, false, temp_names[i]};
  }

  // x16 and x17 are scratch registers
  // x18 is reserved by the platform ABI
  info.m_info[X16] = {false, true, "x16"};
  info.m_info[X17] = {false, true, "x17"};
  info.m_info[X18] = {false, true, "x18"};

  info.m_info[X19] = {true, false, "x19", 8};  // saved
  info.m_info[X20] = {false, true, "x20", 8};  // pp
  info.m_info[X21] = {false, true, "x21", 8};  // st
  info.m_info[X22] = {false, true, "x22", 8};  // offset
  info.m_info[X23] = {true, false, "x23", 8};  // saved
  info.m_info[X24] = {true, false, "x24", 8};
  info.m_info[X25] = {true, false, "x25", 8};
  info.m_info[X26] = {true, false, "x26", 8};
  // x27 carries the executable EE memory view.
  info.m_info[X27] = {false, true, "x27", 8};  // exec base
  info.m_info[X28] = {false, true, "x28", 8};  // reserved
  info.m_info[X29] = {false, true, "x29", 8};  // frame pointer
  info.m_info[X30] = {false, true, "x30"};     // link register
  info.m_info[SP] = {false, true, "sp", 8};

  // v8 through v15 can hold scalar values across calls
  for (int i = 0; i < 32; i++) {
    bool saved = (i >= 8 && i <= 15);
    u8 preserved_bytes = saved ? 4 : 0;
    info.m_info[V0 + i] = {saved, false, "v" + std::to_string(i), preserved_bytes};
  }
  // keep v16 free for lane helpers
  info.m_info[V16] = {false, true, "v16"};

  info.m_gpr_arg_regs = std::array<Register, N_ARGS>({X0, X1, X2, X3, X4, X5, X6, X7});
  // skip v0 so it stays free for vector returns
  info.m_xmm_arg_regs = std::array<Register, N_ARGS>({V1, V2, V3, V4, V5, V6, V7, V8});
  info.m_saved_gprs = std::array<Register, N_SAVED_GPRS>({X19, X23, X24, X25, X26});
  info.m_saved_xmms = std::array<Register, N_SAVED_XMMS>({V8, V9, V10, V11, V12, V13, V14, V15});

  for (size_t i = 0; i < N_SAVED_GPRS; i++) {
    info.m_saved_all[i] = info.m_saved_gprs[i];
  }
  for (size_t i = 0; i < N_SAVED_XMMS; i++) {
    info.m_saved_all[i + N_SAVED_GPRS] = info.m_saved_xmms[i];
  }

  info.m_gpr_alloc_order = {X0,  X1,  X2,  X3,  X4,  X5,  X6,  X7,  X8,  X9, X10,
                            X11, X12, X13, X14, X15, X19, X23, X24, X25, X26};
  info.m_xmm_alloc_order = {V0, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15};

  info.m_gpr_temp_only_alloc_order = {X0, X1, X2,  X3,  X4,  X5,  X6,  X7,
                                      X8, X9, X10, X11, X12, X13, X14, X15};
  info.m_xmm_temp_only_alloc_order = {V0, V1, V2, V3, V4, V5, V6, V7};

  info.m_gpr_spill_temp_alloc_order = {X0,  X1,  X2,  X3,  X4,  X5,  X6,  X7,  X8,  X9, X10,
                                       X11, X12, X13, X14, X15, X19, X23, X24, X25, X26};
  info.m_xmm_spill_temp_alloc_order = {V0, V1, V2,  V3,  V4,  V5,  V6,  V7,
                                       V8, V9, V10, V11, V12, V13, V14, V15};

  info.m_process_reg = X20;
  info.m_st_reg = X21;
  info.m_offset_reg = X22;
  info.m_exec_base_reg = X27;
  info.m_stack_reg = SP;
  // scalar returns use x0, vector returns use v0
  info.m_gpr_ret_reg = X0;
  info.m_xmm_ret_reg = V0;
  return info;
}

bool RegisterInfo::is_preserved_across_call(Register reg, RegClass reg_class) const {
  u8 value_bytes;
  switch (reg_class) {
    case RegClass::FLOAT:
      value_bytes = 4;
      break;
    case RegClass::GPR_64:
      value_bytes = 8;
      break;
    case RegClass::INT_128:
    case RegClass::VECTOR_FLOAT:
      value_bytes = 16;
      break;
    default:
      ASSERT(false);
      return false;
  }
  return get_info(reg).call_preserved_bytes >= value_bytes;
}

RegisterInfo gRegInfo = RegisterInfo::make_register_info();
RegisterInfo gRegInfoArm64 = RegisterInfo::make_register_info_arm64();

const RegisterInfo& reg_info(InstructionSet instr_set) {
  switch (instr_set) {
    case InstructionSet::X86:
      return gRegInfo;
    case InstructionSet::ARM64:
      return gRegInfoArm64;
    default:
      throw std::runtime_error("reg_info: unsupported instruction set");
  }
}

std::string to_string(HWRegKind kind) {
  switch (kind) {
    case HWRegKind::GPR:
      return "gpr";
    case HWRegKind::XMM:
      return "xmm";
    default:
      throw std::runtime_error("Unsupported HWRegKind");
  }
}

HWRegKind reg_class_to_hw(RegClass reg_class) {
  switch (reg_class) {
    case RegClass::VECTOR_FLOAT:
    case RegClass::FLOAT:
    case RegClass::INT_128:
      return HWRegKind::XMM;
    case RegClass::GPR_64:
      return HWRegKind::GPR;
    default:
      ASSERT(false);
      return HWRegKind::INVALID;
  }
}

std::string Register::print(InstructionSet instr_set) const {
  return reg_info(instr_set).get_info(*this).name;
}

}  // namespace emitter
