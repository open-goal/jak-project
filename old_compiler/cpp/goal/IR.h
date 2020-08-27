#ifndef JAK_IR_H
#define JAK_IR_H

#include <string>
#include <cassert>
#include "regalloc/RegAllocInstr.h"
#include "GoalPlace.h"
#include "Label.h"

enum IR_Kind {
  SET_SYMBOL_VALUE,
  GET_SYMBOL_VALUE,
  LOAD_INTEGER,
  STATIC_VAR_ADDR,
  STATIC_VAR_32,
  RETURN,
  GOTO_LABEL,
  SET,
  FUNCTION_CALL,
  FUNC_ADDR,
  FUNCTION_BEGIN,
  INTEGER_MATH,
  FLOAT_MATH,
  GET_SYMBOL_OBJ,
  CONDITIONAL_BRANCH,
  XMM_TO_GPR,
  LOAD_CONST_OFFSET,
  STORE_CONST_OFFSET,
  GET_RETURN_ADDRESS_POINTER,
  FLOAT_TO_INT,
  INT_TO_FLOAT,
  ASM,
  IR_NULL
};

class IR {
 public:
  virtual std::string print() = 0;
  virtual RegAllocInstr to_rai() = 0;
  virtual void add_constraints_to_program(std::vector<RegConstraint>& constraints, int my_id) {
    (void)constraints;
    (void)my_id;
  }
  IR_Kind kind;
};

class IR_SetSymbolValue : public IR {
 public:
  IR_SetSymbolValue() { kind = SET_SYMBOL_VALUE; }

  std::string print() { return "SET-SYM-VAL " + dest->print() + " TO " + value->print(); }

  RegAllocInstr to_rai();

  std::shared_ptr<Place> value;
  std::shared_ptr<SymbolPlace> dest;
};

class IR_GetSymbolValue : public IR {
 public:
  IR_GetSymbolValue() { kind = GET_SYMBOL_VALUE; }

  std::string print();
  RegAllocInstr to_rai();

  bool sext = false;

  std::shared_ptr<Place> dest;
  std::shared_ptr<SymbolPlace> symbol;
};

class IR_GetSymbolObj : public IR {
 public:
  IR_GetSymbolObj() { kind = GET_SYMBOL_OBJ; }

  IR_GetSymbolObj(std::shared_ptr<Place> _dest, std::shared_ptr<SymbolPlace> _sym)
      : dest(_dest), sym(_sym) {
    kind = GET_SYMBOL_OBJ;
  }

  std::string print();
  RegAllocInstr to_rai();

  std::shared_ptr<Place> dest;
  std::shared_ptr<SymbolPlace> sym;
};

// always a variable
class IR_Set : public IR {
 public:
  IR_Set() { kind = SET; }

  IR_Set(std::shared_ptr<Place> _dst, std::shared_ptr<Place> _src) : dest(_dst), src(_src) {
    kind = SET;
  }
  std::string print() { return "SET " + dest->print() + " TO " + src->print(); }
  RegAllocInstr to_rai();
  std::shared_ptr<Place> dest;
  std::shared_ptr<Place> src;
};

class IR_FloatToInt : public IR {
 public:
  IR_FloatToInt() { kind = FLOAT_TO_INT; }

  IR_FloatToInt(std::shared_ptr<Place> _dst, std::shared_ptr<Place> _src) : dest(_dst), src(_src) {
    kind = FLOAT_TO_INT;
  }

  std::string print() override { return "F2I " + dest->print() + " " + src->print(); }
  RegAllocInstr to_rai() override;
  std::shared_ptr<Place> dest;
  std::shared_ptr<Place> src;
};

class IR_IntToFloat : public IR {
 public:
  IR_IntToFloat() { kind = INT_TO_FLOAT; }

  IR_IntToFloat(std::shared_ptr<Place> _dst, std::shared_ptr<Place> _src) : dest(_dst), src(_src) {
    kind = INT_TO_FLOAT;
  }

  std::string print() override { return "I2F " + dest->print() + " " + src->print(); }
  RegAllocInstr to_rai() override;
  std::shared_ptr<Place> dest;
  std::shared_ptr<Place> src;
};

class IR_Goto_Label : public IR {
 public:
  IR_Goto_Label() { kind = GOTO_LABEL; }
  std::string print();
  RegAllocInstr to_rai();
  std::shared_ptr<Label> label;
  bool resolved = false;
};

class IR_LoadInteger : public IR {
 public:
  IR_LoadInteger() { kind = LOAD_INTEGER; }

  std::string print();
  RegAllocInstr to_rai();

  union {
    uint64_t us_value;
    int64_t s_value;
  };

  bool is_signed;
  uint8_t size;

  std::shared_ptr<Place> value;
};

class IR_StaticVarAddr : public IR {
 public:
  IR_StaticVarAddr() { kind = STATIC_VAR_ADDR; }
  std::string print();
  RegAllocInstr to_rai();

  std::shared_ptr<Place> dest;
  std::shared_ptr<Place> src;
};

class IR_StaticVar32 : public IR {
 public:
  IR_StaticVar32() { kind = STATIC_VAR_32; }
  std::string print();
  RegAllocInstr to_rai();

  std::shared_ptr<Place> dest;
  std::shared_ptr<Place> src;
};

class IR_FunctionAddr : public IR {
 public:
  IR_FunctionAddr() { kind = FUNC_ADDR; }

  std::string print();
  RegAllocInstr to_rai();
  std::shared_ptr<Place> dest;
  std::shared_ptr<Place> src;
};

class IR_Return : public IR {
 public:
  IR_Return(std::shared_ptr<Place> v, std::shared_ptr<Place> dst) : value(v), dest(dst) {
    kind = RETURN;
  }
  std::string print();
  RegAllocInstr to_rai();
  std::shared_ptr<Place> value, dest;

  void add_constraints_to_program(std::vector<RegConstraint>& constraints, int my_id) override {
    RegConstraint c;
    if (std::dynamic_pointer_cast<NonePlace>(dest)) {
      return;
    }
    assert(dest->is_register());
    c.var_id = dest->get_assignment().id;
    c.instr_id = my_id;
    c.ass.kind = REGISTER;
    c.ass.reg_id = RAX;
    constraints.push_back(c);
  }
};

class IR_FunctionBegin : public IR {
 public:
  IR_FunctionBegin(std::shared_ptr<LambdaPlace> l) : lambda(l) { kind = FUNCTION_BEGIN; }

  std::string print();
  RegAllocInstr to_rai();
  int nargs = -1;
  std::shared_ptr<LambdaPlace> lambda;
};

class IR_FunctionCall : public IR {
 public:
  IR_FunctionCall(std::shared_ptr<Place> func_call_reg,
                  std::shared_ptr<Place> func_in_reg,
                  std::shared_ptr<Place> dest_reg,
                  const std::vector<std::shared_ptr<Place>>& arg_regs) {
    kind = FUNCTION_CALL;
    func_call = func_call_reg;
    func_in = func_in_reg;
    dest = dest_reg;
    args = arg_regs;
  }

  std::string print();
  RegAllocInstr to_rai();
  void add_constraints_to_program(std::vector<RegConstraint>& constraints, int my_id) override;
  std::shared_ptr<Place> func_in, func_call;
  std::shared_ptr<Place> dest;
  std::vector<std::shared_ptr<Place>> args;
};

enum IntegerMathKind {
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
  IR_IntegerMath(IntegerMathKind _math_kind, std::shared_ptr<Place> _dest, uint8_t _sa)
      : math_kind(_math_kind), d(std::move(_dest)), sa(_sa) {
    kind = INTEGER_MATH;
  }

  IR_IntegerMath(IntegerMathKind k, std::shared_ptr<Place> dest, std::shared_ptr<Place> arg0) {
    d = dest;
    a0 = arg0;
    kind = INTEGER_MATH;
    math_kind = k;
  }

  std::string print();
  RegAllocInstr to_rai();

  IntegerMathKind math_kind;
  std::shared_ptr<Place> d, a0;
  uint8_t sa = -1;
};

enum FloatMathKind { MUL_SS, DIV_SS, SUB_SS, ADD_SS };

class IR_FloatMath : public IR {
 public:
  IR_FloatMath(FloatMathKind k, std::shared_ptr<Place> dest, std::shared_ptr<Place> arg0) {
    d = dest;
    a0 = arg0;
    kind = FLOAT_MATH;
    math_kind = k;
  }

  std::string print();
  RegAllocInstr to_rai();
  FloatMathKind math_kind;
  std::shared_ptr<Place> d, a0;
};

enum ConditionKind { NOT_EQUAL_64, EQUAL_64, LEQ_64, LT_64, GT_64, GEQ_64, INVALID_CONDITION };

struct GoalCondition {
  ConditionKind kind = INVALID_CONDITION;
  std::shared_ptr<Place> a, b;
  bool is_signed = false;
  bool is_float = false;
  RegAllocInstr to_rai();
  std::string print();
};

class IR_ConditionalBranch : public IR {
 public:
  IR_ConditionalBranch() { kind = CONDITIONAL_BRANCH; }

  std::string print();
  RegAllocInstr to_rai();

  GoalCondition cond;
  std::shared_ptr<Label> label = nullptr;
  bool resolved = false;
};

// class IR_Xmm2Gpr : public IR {
// public:
//  IR_Xmm2Gpr() {
//    kind = XMM_TO_GPR;
//  }
//
//  std::string print();
//  RegAllocInstr to_rai();
//
//  std::shared_ptr<Place> dst;
//  std::shared_ptr<Place> src;
//};

class IR_LoadConstOffset : public IR {
 public:
  IR_LoadConstOffset() { kind = LOAD_CONST_OFFSET; }

  IR_LoadConstOffset(std::shared_ptr<Place> _dst,
                     std::shared_ptr<Place> _src,
                     int32_t _offset,
                     int32_t _size,
                     bool _signed)
      : dst(_dst), src(_src), offset(_offset), size(_size), is_signed(_signed) {
    kind = LOAD_CONST_OFFSET;
  }

  std::string print();
  RegAllocInstr to_rai();
  std::shared_ptr<Place> dst;
  std::shared_ptr<Place> src;
  int32_t offset;
  int32_t size;
  bool is_signed;
};

class IR_StoreConstOffset : public IR {
 public:
  IR_StoreConstOffset() { kind = STORE_CONST_OFFSET; }

  IR_StoreConstOffset(std::shared_ptr<Place> _mem,
                      std::shared_ptr<Place> _val,
                      int32_t _offset,
                      int32_t _size,
                      bool _signed)
      : mem(_mem), val(_val), offset(_offset), size(_size), is_signed(_signed) {
    kind = STORE_CONST_OFFSET;
  }

  std::string print();
  RegAllocInstr to_rai();
  std::shared_ptr<Place> mem;
  std::shared_ptr<Place> val;
  int32_t offset;
  int32_t size;
  bool is_signed;
};

class IR_GetReturnAddressPointer : public IR {
 public:
  IR_GetReturnAddressPointer(std::shared_ptr<Place> _dest) : dest(_dest) {
    kind = GET_RETURN_ADDRESS_POINTER;
  }

  std::string print();
  RegAllocInstr to_rai();
  std::shared_ptr<Place> dest;
};

class IR_Asm : public IR {
 public:
  enum AsmKind { RET, RET_REGISTER, MOVE_GPR_U64, PUSH, POP, JMP, SUB, CALL_GOAL_STACK_SUB_8 };

  IR_Asm(AsmKind _kind, std::vector<std::shared_ptr<Place>>& _args) : asm_kind(_kind), args(_args) {
    kind = ASM;
  }

  std::string print();
  RegAllocInstr to_rai();
  AsmKind asm_kind;
  std::vector<std::shared_ptr<Place>> args;
};

class IR_Null : public IR {
 public:
  IR_Null() { kind = IR_NULL; }

  std::string print() { return "NULL"; }

  RegAllocInstr to_rai() { return {}; }
};

struct UnresolvedGoto {
  IR_Goto_Label* ir;
  std::string label_name;
};

struct UnresolvedConditionalGoto {
  IR_ConditionalBranch* ir;
  std::string label_name;
};

#endif  // JAK_IR_H
