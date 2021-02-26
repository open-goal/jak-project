#include "OpenGoalMapping.h"

// TODO - having a way to handle named args here might be nice.
// TODO - env->emit_ir<IR_LoadConstOffset>(dest, 0, src->to_gpr(env), info, color); in compile_asm_lvf
namespace decompiler {
const std::map<InstructionKind, OpenGOALAsm::Function> MIPS_ASM_TO_OPEN_GOAL_FUNCS = {
    // TODO - load/store instructions?
    {InstructionKind::VMOVE, {"TODO", {}, {}}},

    // Load and Store
    {InstructionKind::LQC2, {".lvf", {}, {OpenGOALAsm::InstructionModifiers::OFFSET}}},
    {InstructionKind::QMFC2, {".mov", {}, {}}},
    {InstructionKind::SQC2, {".svf", {}, {OpenGOALAsm::InstructionModifiers::OFFSET}}},

    // Redundant ops, NOP and WAIT
    {InstructionKind::VNOP, {".nop.vf", {}, {}}},
    {InstructionKind::VWAITQ, {".wait.vf", {}, {}}},

    // Max / Min
    {InstructionKind::VMAX, {".max.vf", {}, {OpenGOALAsm::InstructionModifiers::DEST_MASK}}},
    {InstructionKind::VMAX_BC,
     {".max.{}.vf",
      {},
      {OpenGOALAsm::InstructionModifiers::BROADCAST,
       OpenGOALAsm::InstructionModifiers::DEST_MASK}}},
    {InstructionKind::VMINI, {".min.vf", {}, {OpenGOALAsm::InstructionModifiers::DEST_MASK}}},
    {InstructionKind::VMINI_BC,
     {".min.{}.vf",
      {},
      {OpenGOALAsm::InstructionModifiers::BROADCAST,
       OpenGOALAsm::InstructionModifiers::DEST_MASK}}},

    // Addition / Addition with ACC
    {InstructionKind::VADD, {".add.vf", {}, {OpenGOALAsm::InstructionModifiers::DEST_MASK}}},
    {InstructionKind::VADD_BC,
     {".add.{}.vf",
      {},
      {OpenGOALAsm::InstructionModifiers::BROADCAST,
       OpenGOALAsm::InstructionModifiers::DEST_MASK}}},
    {InstructionKind::VADDA,
     {".add.vf",
      {OpenGOALAsm::AdditionalVURegisters::ACC},
      {OpenGOALAsm::InstructionModifiers::DEST_MASK}}},
    {InstructionKind::VADDA_BC,
     {".add.{}.vf",
      {OpenGOALAsm::AdditionalVURegisters::ACC},
      {OpenGOALAsm::InstructionModifiers::BROADCAST,
       OpenGOALAsm::InstructionModifiers::DEST_MASK}}},

    // Subtraction
    {InstructionKind::VSUB, {".sub.vf", {}, {OpenGOALAsm::InstructionModifiers::DEST_MASK}}},
    {InstructionKind::VSUB_BC,
     {".sub.{}.vf",
      {},
      {OpenGOALAsm::InstructionModifiers::BROADCAST,
       OpenGOALAsm::InstructionModifiers::DEST_MASK}}},
    // TODO - missing VSUBA?

    // Multiplication / Multiplication with ACC
    {InstructionKind::VMUL, {".mul.vf", {}, {OpenGOALAsm::InstructionModifiers::DEST_MASK}}},
    {InstructionKind::VMUL_BC,
     {".mul.{}.vf",
      {},
      {OpenGOALAsm::InstructionModifiers::BROADCAST,
       OpenGOALAsm::InstructionModifiers::DEST_MASK}}},
    {InstructionKind::VMULA,
     {".mul.vf",
      {OpenGOALAsm::AdditionalVURegisters::ACC},
      {OpenGOALAsm::InstructionModifiers::DEST_MASK}}},
    {InstructionKind::VMULA_BC,
     {".mul.{}.vf",
      {OpenGOALAsm::AdditionalVURegisters::ACC},
      {OpenGOALAsm::InstructionModifiers::BROADCAST,
       OpenGOALAsm::InstructionModifiers::DEST_MASK}}},

    // Add or Subtract with the resulting product / use the ACC
    {InstructionKind::VMADD, {".add.mul.vf", {}, {OpenGOALAsm::InstructionModifiers::DEST_MASK}}},
    {InstructionKind::VMADD_BC,
     {".add.mul.{}.vf",
      {},
      {OpenGOALAsm::InstructionModifiers::BROADCAST,
       OpenGOALAsm::InstructionModifiers::DEST_MASK}}},
    {InstructionKind::VMADDA,
     {".add.mul.vf",
      {OpenGOALAsm::AdditionalVURegisters::ACC},
      {OpenGOALAsm::InstructionModifiers::DEST_MASK}}},
    {InstructionKind::VMADDA_BC,
     {".add.mul.{}.vf",
      {OpenGOALAsm::AdditionalVURegisters::ACC},
      {OpenGOALAsm::InstructionModifiers::BROADCAST,
       OpenGOALAsm::InstructionModifiers::DEST_MASK}}},
    {InstructionKind::VMSUB, {".sub.mul.vf", {}, {OpenGOALAsm::InstructionModifiers::DEST_MASK}}},
    {InstructionKind::VMSUB_BC,
     {".sub.mul.{}.vf",
      {},
      {OpenGOALAsm::InstructionModifiers::BROADCAST,
       OpenGOALAsm::InstructionModifiers::DEST_MASK}}},
    // TODO - missing a VMSUBA?
    {InstructionKind::VMSUBA_BC,
     {".sub.mul.{}.vf",
      {OpenGOALAsm::AdditionalVURegisters::ACC},
      {OpenGOALAsm::InstructionModifiers::BROADCAST,
       OpenGOALAsm::InstructionModifiers::DEST_MASK}}},

    // Absolute value
    {InstructionKind::VABS, {".abs.vf", {}, {OpenGOALAsm::InstructionModifiers::DEST_MASK}}},

    // Outer-product
    // TODO - currently it's assumed these groups of instructions will be replaced with 1
    {InstructionKind::VOPMULA, {".outer.product.vf", {}, {}}},
    {InstructionKind::VOPMSUB, {".outer.product.vf", {}, {}}},

    // Division
    {InstructionKind::VDIV,
     {".div.vf",
      {OpenGOALAsm::AdditionalVURegisters::Q_DST},
      {OpenGOALAsm::InstructionModifiers::FTF, OpenGOALAsm::InstructionModifiers::FSF}}},

    // Square-root
    {InstructionKind::VSQRT,
     {".sqrt.vf",
      {OpenGOALAsm::AdditionalVURegisters::Q_DST},
      {OpenGOALAsm::InstructionModifiers::FTF}}},

    // Operations using the result of division
    {InstructionKind::VADDQ,
     {".add.vf",
      {OpenGOALAsm::AdditionalVURegisters::Q_SRC},
      {OpenGOALAsm::InstructionModifiers::DEST_MASK}}},
    {InstructionKind::VSUBQ,
     {".sub.vf",
      {OpenGOALAsm::AdditionalVURegisters::Q_SRC},
      {OpenGOALAsm::InstructionModifiers::DEST_MASK}}},
    {InstructionKind::VMULQ,
     {".mul.vf",
      {OpenGOALAsm::AdditionalVURegisters::Q_SRC},
      {OpenGOALAsm::InstructionModifiers::DEST_MASK}}},
    {InstructionKind::VMULAQ,
     {".mul.vf",
      {OpenGOALAsm::AdditionalVURegisters::Q_SRC},
      {OpenGOALAsm::InstructionModifiers::DEST_MASK}}},
    // TODO - missing a vmaddq?
    {InstructionKind::VMSUBQ,
     {".sub.mul.vf",
      {OpenGOALAsm::AdditionalVURegisters::Q_SRC},
      {OpenGOALAsm::InstructionModifiers::DEST_MASK}}},

    //// Random number generation
    //{InstructionKind::VRGET, "not-implemented"},
    //{InstructionKind::VRSQRT, "not-implemented"},
    //{InstructionKind::VRXOR, "not-implemented"},
    //{InstructionKind::VRNEXT, "not-implemented"},

    //// VU Integer operations
    //{InstructionKind::VMTIR, "not-implemented"},
    //{InstructionKind::VIAND, "not-implemented"},
    //{InstructionKind::VIADDI, "not-implemented"},

    //// Load/store from VU memory
    //{InstructionKind::VLQI, "not-implemented"},
    //{InstructionKind::VSQI, "not-implemented"},

    //// Fixed point conversions
    //{InstructionKind::VFTOI0, "not-implemented"},
    //{InstructionKind::VFTOI4, "not-implemented"},
    //{InstructionKind::VFTOI12, "not-implemented"},
    //{InstructionKind::VITOF0, "not-implemented"},
    //{InstructionKind::VITOF12, "not-implemented"},
    //{InstructionKind::VITOF15, "not-implemented"},

    //// Status Checks
    //{InstructionKind::VCLIP, "not-implemented"},
};
OpenGOALAsm::OpenGOALAsm(InstructionKind kind) {
  if (MIPS_ASM_TO_OPEN_GOAL_FUNCS.count(kind) == 0) {
    valid = false;
  } else {
    func = MIPS_ASM_TO_OPEN_GOAL_FUNCS.at(kind);
  }
}
}  // namespace decompiler
