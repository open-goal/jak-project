#ifndef JAK_IR_H
#define JAK_IR_H

#include <cassert>
#include <utility>
#include <memory>
#include <unordered_map>
#include "decompiler/Disasm/Register.h"
#include "common/type_system/TypeSpec.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "decompiler/util/TP_Type.h"

class LinkedObjectFile;
class DecompilerTypeSystem;

namespace goos {
class Object;
}

class IR {
 public:
  virtual goos::Object to_form(const LinkedObjectFile& file) const = 0;
  std::vector<std::shared_ptr<IR>> get_all_ir(LinkedObjectFile& file) const;
  std::string print(const LinkedObjectFile& file) const;
  virtual void get_children(std::vector<std::shared_ptr<IR>>* output) const = 0;
  bool is_basic_op = false;
  virtual TP_Type get_expression_type(const TypeState& input,
                                      const LinkedObjectFile& file,
                                      DecompilerTypeSystem& dts);
  virtual ~IR() = default;
};

class IR_Atomic : public virtual IR {
 public:
  std::vector<Register> read_regs, write_regs, clobber_regs;
  bool reg_info_set = false;

  TypeState end_types;  // types at the end of this instruction
  std::vector<std::string> warnings;
  void warn(const std::string& str) { warnings.emplace_back(str); }

  virtual void propagate_types(const TypeState& input,
                               const LinkedObjectFile& file,
                               DecompilerTypeSystem& dts);
  std::string print_with_types(const TypeState& init_types, const LinkedObjectFile& file) const;
  std::string print_with_reguse(const LinkedObjectFile& file) const;
};

class IR_Failed : public virtual IR {
 public:
  IR_Failed() = default;
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
};

class IR_Failed_Atomic : public IR_Failed, public IR_Atomic {
 public:
  IR_Failed_Atomic() = default;
};

class IR_Register : public virtual IR {
 public:
  IR_Register(Register _reg, int _instr_idx) : reg(_reg), instr_idx(_instr_idx) {}
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
  Register reg;
  int instr_idx = -1;
  TP_Type get_expression_type(const TypeState& input,
                              const LinkedObjectFile& file,
                              DecompilerTypeSystem& dts) override;
};

class IR_Set : public virtual IR {
 public:
  enum Kind {
    REG_64,
    LOAD,
    STORE,
    SYM_LOAD,
    SYM_STORE,
    FPR_TO_GPR64,
    GPR_TO_FPR,
    REG_FLT,
    REG_I128
  } kind;
  IR_Set(Kind _kind, std::shared_ptr<IR> _dst, std::shared_ptr<IR> _src)
      : kind(_kind), dst(std::move(_dst)), src(std::move(_src)) {}
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
  std::shared_ptr<IR> dst, src;
  std::shared_ptr<IR> clobber = nullptr;
};

// todo
class IR_Set_Atomic : public IR_Set, public IR_Atomic {
 public:
  IR_Set_Atomic(IR_Set::Kind _kind, std::shared_ptr<IR> _dst, std::shared_ptr<IR> _src)
      : IR_Set(_kind, std::move(_dst), std::move(_src)) {}

  template <typename T>
  void update_reginfo_self(int n_dest, int n_src, int n_clobber);
  void update_reginfo_regreg();
  void propagate_types(const TypeState& input,
                       const LinkedObjectFile& file,
                       DecompilerTypeSystem& dts) override;
};

class IR_IntMath2;
template <>
void IR_Set_Atomic::update_reginfo_self<IR_IntMath2>(int n_dest, int n_src, int n_clobber);

class IR_Store : public virtual IR_Set {
 public:
  enum Kind { INTEGER, FLOAT } kind;
  IR_Store(Kind _kind, std::shared_ptr<IR> _dst, std::shared_ptr<IR> _src, int _size)
      : IR_Set(IR_Set::STORE, std::move(_dst), std::move(_src)), kind(_kind), size(_size) {}
  int size;
  goos::Object to_form(const LinkedObjectFile& file) const override;
};

/*!
 * Note, IR_Store_Atomic does not appear as a IR_Set_Atomic.
 * This is to avoid the "diamond problem".
 */
class IR_Store_Atomic : public IR_Set_Atomic {
 public:
  enum Kind { INTEGER, FLOAT } kind;
  IR_Store_Atomic(Kind _kind, std::shared_ptr<IR> _dst, std::shared_ptr<IR> _src, int _size)
      : IR_Set_Atomic(IR_Set::STORE, std::move(_dst), std::move(_src)), kind(_kind), size(_size) {}
  int size;
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void update_reginfo_self(int n_dest, int n_src, int n_clobber);
  void propagate_types(const TypeState& input,
                       const LinkedObjectFile& file,
                       DecompilerTypeSystem& dts) override;
};

class IR_Symbol : public virtual IR {
 public:
  explicit IR_Symbol(std::string _name) : name(std::move(_name)) {}
  std::string name;
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
  TP_Type get_expression_type(const TypeState& input,
                              const LinkedObjectFile& file,
                              DecompilerTypeSystem& dts) override;
};

class IR_SymbolValue : public virtual IR {
 public:
  explicit IR_SymbolValue(std::string _name) : name(std::move(_name)) {}
  std::string name;
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
  TP_Type get_expression_type(const TypeState& input,
                              const LinkedObjectFile& file,
                              DecompilerTypeSystem& dts) override;
};

class IR_EmptyPair : public virtual IR {
 public:
  explicit IR_EmptyPair() = default;
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
  TP_Type get_expression_type(const TypeState& input,
                              const LinkedObjectFile& file,
                              DecompilerTypeSystem& dts) override;
};

class IR_StaticAddress : public virtual IR {
 public:
  explicit IR_StaticAddress(int _label_id) : label_id(_label_id) {}
  int label_id = -1;
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
  TP_Type get_expression_type(const TypeState& input,
                              const LinkedObjectFile& file,
                              DecompilerTypeSystem& dts) override;
};

class IR_Load : public virtual IR {
 public:
  enum Kind { UNSIGNED, SIGNED, FLOAT } kind;

  IR_Load(Kind _kind, int _size, std::shared_ptr<IR> _location)
      : kind(_kind), size(_size), location(std::move(_location)) {}
  int size;
  std::shared_ptr<IR> location;
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
  TP_Type get_expression_type(const TypeState& input,
                              const LinkedObjectFile& file,
                              DecompilerTypeSystem& dts) override;
};

class IR_FloatMath2 : public virtual IR {
 public:
  enum Kind { DIV, MUL, ADD, SUB, MIN, MAX } kind;
  IR_FloatMath2(Kind _kind, std::shared_ptr<IR> _arg0, std::shared_ptr<IR> _arg1)
      : kind(_kind), arg0(std::move(_arg0)), arg1(std::move(_arg1)) {}
  std::shared_ptr<IR> arg0, arg1;
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
  TP_Type get_expression_type(const TypeState& input,
                              const LinkedObjectFile& file,
                              DecompilerTypeSystem& dts) override;
};

class IR_FloatMath1 : public virtual IR {
 public:
  enum Kind { FLOAT_TO_INT, INT_TO_FLOAT, ABS, NEG, SQRT } kind;
  IR_FloatMath1(Kind _kind, std::shared_ptr<IR> _arg) : kind(_kind), arg(std::move(_arg)) {}
  std::shared_ptr<IR> arg;
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
  TP_Type get_expression_type(const TypeState& input,
                              const LinkedObjectFile& file,
                              DecompilerTypeSystem& dts) override;
};

class IR_IntMath2 : public virtual IR {
 public:
  enum Kind {
    ADD,
    SUB,
    MUL_SIGNED,
    DIV_SIGNED,
    MOD_SIGNED,
    DIV_UNSIGNED,
    MOD_UNSIGNED,
    OR,
    AND,
    NOR,
    XOR,
    LEFT_SHIFT,
    RIGHT_SHIFT_ARITH,
    RIGHT_SHIFT_LOGIC,
    MUL_UNSIGNED,
    MIN_SIGNED,
    MAX_SIGNED
  } kind;
  IR_IntMath2(Kind _kind, std::shared_ptr<IR> _arg0, std::shared_ptr<IR> _arg1)
      : kind(_kind), arg0(std::move(_arg0)), arg1(std::move(_arg1)) {}
  std::shared_ptr<IR> arg0, arg1;
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
  TP_Type get_expression_type(const TypeState& input,
                              const LinkedObjectFile& file,
                              DecompilerTypeSystem& dts) override;
};

class IR_IntMath1 : public virtual IR {
 public:
  enum Kind { NOT, ABS, NEG } kind;
  IR_IntMath1(Kind _kind, std::shared_ptr<IR> _arg) : kind(_kind), arg(std::move(_arg)) {}
  std::shared_ptr<IR> arg;
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
  TP_Type get_expression_type(const TypeState& input,
                              const LinkedObjectFile& file,
                              DecompilerTypeSystem& dts) override;
};

class IR_Call : public virtual IR {
 public:
  IR_Call() = default;
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
};

// todo
class IR_Call_Atomic : public virtual IR_Call, public IR_Atomic {
 public:
  IR_Call_Atomic() = default;
  void propagate_types(const TypeState& input,
                       const LinkedObjectFile& file,
                       DecompilerTypeSystem& dts) override;
};

class IR_IntegerConstant : public virtual IR {
 public:
  int64_t value;
  explicit IR_IntegerConstant(int64_t _value) : value(_value) {}
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
  TP_Type get_expression_type(const TypeState& input,
                              const LinkedObjectFile& file,
                              DecompilerTypeSystem& dts) override;
};

struct BranchDelay {
  enum Kind {
    NOP,
    SET_REG_FALSE,
    SET_REG_TRUE,
    SET_REG_REG,
    SET_BINTEGER,
    SET_PAIR,
    DSLLV,
    NEGATE,
    UNKNOWN
  } kind;
  std::shared_ptr<IR> destination = nullptr, source = nullptr, source2 = nullptr;
  explicit BranchDelay(Kind _kind) : kind(_kind) {}
  goos::Object to_form(const LinkedObjectFile& file) const;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const;

  std::vector<Register> read_regs;
  std::vector<Register> write_regs;
  std::vector<Register> clobber_regs;

  void type_prop(TypeState& output, const LinkedObjectFile& file, DecompilerTypeSystem& dts);
};

struct Condition {
  enum Kind {
    NOT_EQUAL,
    EQUAL,
    LESS_THAN_SIGNED,
    GREATER_THAN_SIGNED,
    LEQ_SIGNED,
    GEQ_SIGNED,
    GREATER_THAN_ZERO_SIGNED,
    LEQ_ZERO_SIGNED,
    LESS_THAN_ZERO,
    GEQ_ZERO_SIGNED,
    LESS_THAN_UNSIGNED,
    GREATER_THAN_UNSIGNED,
    LEQ_UNSIGNED,
    GEQ_UNSIGNED,
    ZERO,
    NONZERO,
    FALSE,
    TRUTHY,
    ALWAYS,
    NEVER,
    FLOAT_EQUAL,
    FLOAT_NOT_EQUAL,
    FLOAT_LESS_THAN,
    FLOAT_GEQ,
    FLOAT_LEQ,
    FLOAT_GREATER_THAN,
  } kind;

  Condition(Kind _kind,
            std::shared_ptr<IR> _src0,
            std::shared_ptr<IR> _src1,
            std::shared_ptr<IR> _clobber)
      : kind(_kind), src0(std::move(_src0)), src1(std::move(_src1)), clobber(std::move(_clobber)) {
    int nargs = num_args();
    if (nargs == 2) {
      assert(src0 && src1);
    } else if (nargs == 1) {
      assert(src0 && !src1);
    } else if (nargs == 0) {
      assert(!src0 && !src1);
    }
  }

  int num_args() const;
  goos::Object to_form(const LinkedObjectFile& file) const;
  std::shared_ptr<IR> src0, src1, clobber;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const;
  void invert();
};

class IR_Branch : public virtual IR {
 public:
  IR_Branch(Condition _condition, int _dest_label_idx, BranchDelay _branch_delay, bool _likely)
      : condition(std::move(_condition)),
        dest_label_idx(_dest_label_idx),
        branch_delay(std::move(_branch_delay)),
        likely(_likely) {}

  Condition condition;
  int dest_label_idx;
  BranchDelay branch_delay;
  bool likely;

  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
};

// todo
class IR_Branch_Atomic : public virtual IR_Branch, public IR_Atomic {
 public:
  IR_Branch_Atomic(Condition _condition,
                   int _dest_label_idx,
                   BranchDelay _branch_delay,
                   bool _likely)
      : IR_Branch(std::move(_condition), _dest_label_idx, std::move(_branch_delay), _likely) {}
  // note - counts only for the condition.
  void update_reginfo_self(int n_dst, int n_src, int n_clobber);
  void propagate_types(const TypeState& input,
                       const LinkedObjectFile& file,
                       DecompilerTypeSystem& dts) override;
};

class IR_Compare : public virtual IR {
 public:
  explicit IR_Compare(Condition _condition) : condition(std::move(_condition)) {}

  Condition condition;

  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
  TP_Type get_expression_type(const TypeState& input,
                              const LinkedObjectFile& file,
                              DecompilerTypeSystem& dts) override;
};

class IR_Nop : public virtual IR {
 public:
  IR_Nop() = default;
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
};

class IR_Nop_Atomic : public IR_Nop, public IR_Atomic {
 public:
  IR_Nop_Atomic() = default;
  void propagate_types(const TypeState& input,
                       const LinkedObjectFile& file,
                       DecompilerTypeSystem& dts) override;
};

class IR_Suspend : public virtual IR, public IR_Atomic {
 public:
  IR_Suspend() = default;
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
};

class IR_Breakpoint_Atomic : public virtual IR_Atomic {
 public:
  IR_Breakpoint_Atomic() = default;
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
  void propagate_types(const TypeState& input,
                       const LinkedObjectFile& file,
                       DecompilerTypeSystem& dts) override;
};

class IR_Begin : public virtual IR {
 public:
  IR_Begin() = default;
  explicit IR_Begin(const std::vector<std::shared_ptr<IR>>& _forms) : forms(std::move(_forms)) {}
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
  std::vector<std::shared_ptr<IR>> forms;
};

class IR_WhileLoop : public virtual IR {
 public:
  IR_WhileLoop(std::shared_ptr<IR> _condition, std::shared_ptr<IR> _body)
      : condition(std::move(_condition)), body(std::move(_body)) {}
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
  std::shared_ptr<IR> condition, body;
  bool cleaned = false;
};

class IR_UntilLoop : public virtual IR {
 public:
  IR_UntilLoop(std::shared_ptr<IR> _condition, std::shared_ptr<IR> _body)
      : condition(std::move(_condition)), body(std::move(_body)) {}
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
  std::shared_ptr<IR> condition, body;
};

class IR_CondWithElse : public virtual IR {
 public:
  struct Entry {
    std::shared_ptr<IR> condition = nullptr;
    std::shared_ptr<IR> body = nullptr;
    bool cleaned = false;
  };
  std::vector<Entry> entries;
  std::shared_ptr<IR> else_ir;
  IR_CondWithElse(std::vector<Entry> _entries, std::shared_ptr<IR> _else_ir)
      : entries(std::move(_entries)), else_ir(std::move(_else_ir)) {}
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
};

// this one doesn't have an else statement. Will return false if none of the cases are taken.
class IR_Cond : public virtual IR {
 public:
  struct Entry {
    std::shared_ptr<IR> condition = nullptr;
    std::shared_ptr<IR> body = nullptr;
    std::shared_ptr<IR> false_destination = nullptr;
    bool cleaned = false;
  };
  std::vector<Entry> entries;
  explicit IR_Cond(std::vector<Entry> _entries) : entries(std::move(_entries)) {}
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
};

// this will work on pairs, bintegers, or basics
class IR_GetRuntimeType : public virtual IR {
 public:
  std::shared_ptr<IR> object, clobber;
  IR_GetRuntimeType(std::shared_ptr<IR> _object, std::shared_ptr<IR> _clobber)
      : object(std::move(_object)), clobber(std::move(_clobber)) {}
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
};

class IR_ShortCircuit : public virtual IR {
 public:
  struct Entry {
    std::shared_ptr<IR> condition = nullptr;
    std::shared_ptr<IR> output = nullptr;  // where the delay slot writes to.
    bool cleaned = false;
  };

  enum Kind { UNKNOWN, AND, OR } kind = UNKNOWN;

  std::shared_ptr<IR> final_result = nullptr;  // the register that the final result goes in.

  std::vector<Entry> entries;
  explicit IR_ShortCircuit(std::vector<Entry> _entries) : entries(std::move(_entries)) {}
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
};

class IR_Ash : public virtual IR {
 public:
  std::shared_ptr<IR> shift_amount, value, clobber;
  bool is_signed = true;
  IR_Ash(std::shared_ptr<IR> _shift_amount,
         std::shared_ptr<IR> _value,
         std::shared_ptr<IR> _clobber,
         bool _is_signed)
      : shift_amount(std::move(_shift_amount)),
        value(std::move(_value)),
        clobber(std::move(_clobber)),
        is_signed(_is_signed) {}
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
};

class IR_AsmOp : public virtual IR {
 public:
  std::shared_ptr<IR> dst = nullptr;
  std::shared_ptr<IR> src0 = nullptr;
  std::shared_ptr<IR> src1 = nullptr;
  std::shared_ptr<IR> src2 = nullptr;
  std::string name;
  IR_AsmOp(std::string _name) : name(std::move(_name)) {}
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
};

class IR_AsmOp_Atomic : public virtual IR_AsmOp, public IR_Atomic {
 public:
  IR_AsmOp_Atomic(std::string _name) : IR_AsmOp(std::move(_name)) {}
  void set_reg_info();
  void propagate_types(const TypeState& input,
                       const LinkedObjectFile& file,
                       DecompilerTypeSystem& dts) override;
};

class IR_CMoveF : public virtual IR {
 public:
  std::shared_ptr<IR> src = nullptr;
  bool on_zero = false;
  explicit IR_CMoveF(std::shared_ptr<IR> _src, bool _on_zero)
      : src(std::move(_src)), on_zero(_on_zero) {}
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
};

class IR_AsmReg : public virtual IR {
 public:
  enum Kind { VU_Q, VU_ACC } kind;
  explicit IR_AsmReg(Kind _kind) : kind(_kind) {}
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
};

class IR_Return : public virtual IR {
 public:
  std::shared_ptr<IR> return_code;
  std::shared_ptr<IR> dead_code;
  IR_Return(std::shared_ptr<IR> _return_code, std::shared_ptr<IR> _dead_code)
      : return_code(std::move(_return_code)), dead_code(std::move(_dead_code)) {}
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
};

class IR_Break : public virtual IR {
 public:
  std::shared_ptr<IR> return_code;
  std::shared_ptr<IR> dead_code;
  IR_Break(std::shared_ptr<IR> _return_code, std::shared_ptr<IR> _dead_code)
      : return_code(std::move(_return_code)), dead_code(std::move(_dead_code)) {}
  goos::Object to_form(const LinkedObjectFile& file) const override;
  void get_children(std::vector<std::shared_ptr<IR>>* output) const override;
};

#endif  // JAK_IR_H
