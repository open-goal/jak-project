#pragma once

#ifndef JAK_IR_H
#define JAK_IR_H

#include <string>
#include "CodeGenerator.h"
#include "goalc/regalloc/allocate.h"
#include "Val.h"
#include "goalc/emitter/ObjectGenerator.h"

class IR {
 public:
  virtual std::string print() = 0;
  virtual RegAllocInstr to_rai() = 0;
  virtual void do_codegen(emitter::ObjectGenerator* gen,
                          const AllocationResult& allocs,
                          emitter::IR_Record irec) = 0;
  virtual void add_constraints(std::vector<IRegConstraint>* constraints, int my_id) {
    (void)constraints;
    (void)my_id;
  }
};

// class IR_Set : public IR {
// public:
//  std::string print() override;
//  RegAllocInstr to_rai() override;
//};

class IR_Return : public IR {
 public:
  IR_Return(const RegVal* return_reg, const RegVal* value);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void add_constraints(std::vector<IRegConstraint>* constraints, int my_id) override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;
  const RegVal* value() { return m_value; }

 protected:
  const RegVal* m_return_reg = nullptr;
  const RegVal* m_value = nullptr;
};

class IR_LoadConstant64 : public IR {
 public:
  IR_LoadConstant64(const RegVal* dest, u64 value);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;

 protected:
  const RegVal* m_dest = nullptr;
  u64 m_value = 0;
};

class IR_LoadSymbolPointer : public IR {
 public:
  IR_LoadSymbolPointer(const RegVal* dest, std::string name);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;

 protected:
  const RegVal* m_dest = nullptr;
  std::string m_name;
};

class IR_SetSymbolValue : public IR {
 public:
  IR_SetSymbolValue(const SymbolVal* dest, const RegVal* src);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;

 protected:
  const SymbolVal* m_dest = nullptr;
  const RegVal* m_src = nullptr;
};

class IR_GetSymbolValue : public IR {
 public:
  IR_GetSymbolValue(const RegVal* dest, const SymbolVal* src, bool sext);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;

 protected:
  const RegVal* m_dest = nullptr;
  const SymbolVal* m_src = nullptr;
  bool m_sext = false;
};

class IR_RegSet : public IR {
 public:
  IR_RegSet(const RegVal* dest, const RegVal* src);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;

 protected:
  const RegVal* m_dest = nullptr;
  const RegVal* m_src = nullptr;
};

class IR_GotoLabel : public IR {
 public:
  IR_GotoLabel();
  void resolve(const Label* dest);
  explicit IR_GotoLabel(const Label* dest);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;

 protected:
  const Label* m_dest = nullptr;
  bool m_resolved = false;
};

class IR_FunctionCall : public IR {
 public:
  IR_FunctionCall(const RegVal* func, const RegVal* ret, std::vector<RegVal*> args);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;
  void add_constraints(std::vector<IRegConstraint>* constraints, int my_id) override;

 protected:
  const RegVal* m_func = nullptr;
  const RegVal* m_ret = nullptr;
  std::vector<RegVal*> m_args;
};

class IR_StaticVarAddr : public IR {
 public:
  IR_StaticVarAddr(const RegVal* dest, const StaticObject* src);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;

 protected:
  const RegVal* m_dest = nullptr;
  const StaticObject* m_src = nullptr;
};

class IR_StaticVarLoad : public IR {
 public:
  IR_StaticVarLoad(const RegVal* dest, const StaticObject* src);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;

 protected:
  const RegVal* m_dest = nullptr;
  const StaticObject* m_src = nullptr;
};

class IR_FunctionAddr : public IR {
 public:
  IR_FunctionAddr(const RegVal* dest, FunctionEnv* src);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;

 protected:
  const RegVal* m_dest = nullptr;
  FunctionEnv* m_src = nullptr;
};

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

class IR_IntegerMath : public IR {
 public:
  IR_IntegerMath(IntegerMathKind kind, RegVal* dest, RegVal* arg);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;
  IntegerMathKind get_kind() const { return m_kind; }

 protected:
  IntegerMathKind m_kind;
  RegVal* m_dest;
  RegVal* m_arg;
};

enum class FloatMathKind { DIV_SS, MUL_SS, ADD_SS, SUB_SS };

class IR_FloatMath : public IR {
 public:
  IR_FloatMath(FloatMathKind kind, RegVal* dest, RegVal* arg);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;
  FloatMathKind get_kind() const { return m_kind; }

 protected:
  FloatMathKind m_kind;
  RegVal* m_dest;
  RegVal* m_arg;
};

enum class ConditionKind { NOT_EQUAL, EQUAL, LEQ, LT, GT, GEQ, INVALID_CONDITION };

struct Condition {
  ConditionKind kind = ConditionKind::INVALID_CONDITION;
  RegVal* a = nullptr;
  RegVal* b = nullptr;
  bool is_signed = false;
  bool is_float = false;
  RegAllocInstr to_rai();
  std::string print() const;
};

class IR_ConditionalBranch : public IR {
 public:
  IR_ConditionalBranch(const Condition& condition, Label _label);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;
  void mark_as_resolved() { m_resolved = true; }

  Condition condition;
  Label label;

 private:
  bool m_resolved = false;
};

class IR_LoadConstOffset : public IR {
 public:
  IR_LoadConstOffset(const RegVal* dest, int offset, const RegVal* base, MemLoadInfo info);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;

 private:
  const RegVal* m_dest = nullptr;
  int m_offset = 0;
  const RegVal* m_base = nullptr;
  MemLoadInfo m_info;
};

class IR_StoreConstOffset : public IR {
 public:
  IR_StoreConstOffset(const RegVal* value, int offset, const RegVal* base, int size);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;

 private:
  const RegVal* m_value = nullptr;
  int m_offset = 0;
  const RegVal* m_base = nullptr;
  int m_size = 0;
};

class IR_Null : public IR {
 public:
  IR_Null() = default;
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;
};

class IR_FunctionStart : public IR {
 public:
  IR_FunctionStart(std::vector<RegVal*> args);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;

 private:
  std::vector<RegVal*> m_args;
};

class IR_FloatToInt : public IR {
 public:
  IR_FloatToInt(const RegVal* dest, const RegVal* src);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;

 private:
  const RegVal* m_dest = nullptr;
  const RegVal* m_src = nullptr;
};

class IR_IntToFloat : public IR {
 public:
  IR_IntToFloat(const RegVal* dest, const RegVal* src);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;

 private:
  const RegVal* m_dest = nullptr;
  const RegVal* m_src = nullptr;
};

class IR_GetStackAddr : public IR {
 public:
  IR_GetStackAddr(const RegVal* dest, int slot);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;

 private:
  const RegVal* m_dest = nullptr;
  int m_slot = -1;
};

#endif  // JAK_IR_H
