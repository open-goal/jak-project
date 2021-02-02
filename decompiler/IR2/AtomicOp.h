#pragma once

#include <string>
#include <optional>
#include <cassert>
#include <utility>
#include "common/goos/Object.h"
#include "decompiler/Disasm/Register.h"
#include "decompiler/Disasm/Instruction.h"
#include "decompiler/IR2/IR2_common.h"
#include "Env.h"

namespace decompiler {
class FormElement;
class ConditionElement;
class FormPool;
class DecompilerTypeSystem;

/*!
 * An atomic operation represents a single operation from the point of view of the IR2 system.
 * Each IR2 op is one or more instructions.
 * Each function can be represented as a list of AtomicOps. These are stored in exactly the same
 * order as the instructions appear.
 *
 * The AtomicOps use SimpleAtom and SimpleExpression. These are extremely limited versions of
 * the full IR2 expression system, but are much easier to work with because they are less general
 * and can't be nested infinitely. They also have features specific to the AtomicOp system that are
 * not required for full expressions. The full expression system will later convert these into the
 * more complicated expressions.
 *
 * The types of AtomicOp are:
 * ConditionalMoveFalseOp
 * CallOp
 * SpecialOp
 * BranchOp
 * LoadVarOp
 * StoreOp
 * SetVarConditionOp
 * AsmOp
 * SetVarExprOp
 * FunctionEndOp
 */
class AtomicOp {
 public:
  explicit AtomicOp(int my_idx);
  std::string to_string(const std::vector<DecompilerLabel>& labels, const Env& env) const;
  std::string to_string(const Env& env) const;
  std::string reg_type_info_as_string(const TypeState& init_types,
                                      const TypeState& end_types) const;
  virtual goos::Object to_form(const std::vector<DecompilerLabel>& labels,
                               const Env& env) const = 0;
  virtual bool operator==(const AtomicOp& other) const = 0;
  bool operator!=(const AtomicOp& other) const;

  // determine if this is a GOAL "sequence point".
  // non-sequence point instructions may be out of order from the point of view of the expression
  // stack.
  virtual bool is_sequence_point() const = 0;

  // get the variable being set by this operation. Only call this if is_variable_set returns true.
  virtual Variable get_set_destination() const = 0;

  // convert me to an expression. If I'm a set!, this will produce a (set! x y), which may be
  // undesirable when expression stacking.
  virtual FormElement* get_as_form(FormPool& pool, const Env& env) const = 0;

  // figure out what registers are read and written in this AtomicOp and update read_regs,
  // write_regs, and clobber_regs.  It's expected that these have duplicates if a register appears
  // in the original instructions multiple times. Ex: "and v0, v1, v1" would end up putting v1 in
  // read twice.
  virtual void update_register_info() = 0;

  virtual void collect_vars(VariableSet& vars) const = 0;

  TypeState propagate_types(const TypeState& input, const Env& env, DecompilerTypeSystem& dts);

  int op_id() const { return m_my_idx; }
  const std::vector<Register>& read_regs() const { return m_read_regs; }
  const std::vector<Register>& write_regs() const { return m_write_regs; }
  const std::vector<Register>& clobber_regs() const { return m_clobber_regs; }
  void add_clobber_reg(Register r) { m_clobber_regs.push_back(r); }
  void clear_register_info() {
    m_read_regs.clear();
    m_write_regs.clear();
    m_clobber_regs.clear();
  }

  virtual ~AtomicOp() = default;

 protected:
  int m_my_idx = -1;

  // given the input types of all registers, figure out the output types.
  virtual TypeState propagate_types_internal(const TypeState& input,
                                             const Env& env,
                                             DecompilerTypeSystem& dts) = 0;
  void clobber_temps();

  // the register values that are read (at the start of this op)
  std::vector<Register> m_read_regs;
  // the registers that have actual values written into them (at the end of this op)
  std::vector<Register> m_write_regs;
  // the registers which have junk written into them.
  std::vector<Register> m_clobber_regs;
};

class SimpleExpression;

/*!
 * The has a value. In some cases it can be set.
 */
class SimpleAtom {
 public:
  enum class Kind : u8 {
    VARIABLE,
    INTEGER_CONSTANT,
    SYMBOL_PTR,
    SYMBOL_VAL,
    EMPTY_LIST,
    STATIC_ADDRESS,
    INVALID
  };

  SimpleAtom() = default;
  static SimpleAtom make_var(const Variable& var);
  static SimpleAtom make_sym_ptr(const std::string& name);
  static SimpleAtom make_sym_val(const std::string& name);
  static SimpleAtom make_empty_list();
  static SimpleAtom make_int_constant(s64 value);
  static SimpleAtom make_static_address(int static_label_id);
  goos::Object to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const;
  void collect_vars(VariableSet& vars) const;

  bool is_var() const { return m_kind == Kind::VARIABLE; }
  const Variable& var() const {
    assert(is_var());
    return m_variable;
  }
  s64 get_int() const {
    assert(is_int());
    return m_int;
  }
  bool is_int() const { return m_kind == Kind::INTEGER_CONSTANT; };
  bool is_sym_ptr() const { return m_kind == Kind::SYMBOL_PTR; };
  bool is_sym_val() const { return m_kind == Kind::SYMBOL_VAL; };
  bool is_empty_list() const { return m_kind == Kind::EMPTY_LIST; };
  bool is_static_addr() const { return m_kind == Kind::STATIC_ADDRESS; };
  bool operator==(const SimpleAtom& other) const;
  bool operator!=(const SimpleAtom& other) const { return !((*this) == other); }
  void get_regs(std::vector<Register>* out) const;
  SimpleExpression as_expr() const;
  TP_Type get_type(const TypeState& input, const Env& env, const DecompilerTypeSystem& dts) const;
  const std::string& get_str() const {
    assert(is_sym_ptr() || is_sym_val());
    return m_string;
  }

 private:
  Kind m_kind = Kind::INVALID;
  std::string m_string;  // for symbol ptr and symbol val
  s64 m_int = -1;        // for integer constant and static address label id
  Variable m_variable;
};

/*!
 * A "simple expression" can be used within an AtomicOp.
 * AtomicOps are often made up of very few instructions, so these expressions are quite simple and
 * can't nest. There is an "operation" and some arguments.  There are no side effects of a
 * SimpleExpression. The side effects will be captured by the AtomicOp.
 *
 * Note - there is an expression kind called identity which takes one argument and uses that
 * argument as an expression.
 */
class SimpleExpression {
 public:
  enum class Kind : u8 {
    INVALID,
    IDENTITY,
    DIV_S,
    MUL_S,
    ADD_S,
    SUB_S,
    MIN_S,
    MAX_S,
    FLOAT_TO_INT,
    INT_TO_FLOAT,
    ABS_S,
    NEG_S,
    SQRT_S,
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
    LOGNOT,
    NEG,
    GPR_TO_FPR,
    FPR_TO_GPR,
    MIN_SIGNED,
    MAX_SIGNED,
    MIN_UNSIGNED,
    MAX_UNSIGNED
  };

  // how many arguments?
  int args() const { return n_args; }
  const SimpleAtom& get_arg(int idx) const {
    assert(idx < args());
    return m_args[idx];
  }
  Kind kind() const { return m_kind; }
  SimpleExpression() = default;
  SimpleExpression(Kind kind, const SimpleAtom& arg0);
  SimpleExpression(Kind kind, const SimpleAtom& arg0, const SimpleAtom& arg1);
  goos::Object to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const;
  bool operator==(const SimpleExpression& other) const;
  bool is_identity() const { return m_kind == Kind::IDENTITY; }
  void get_regs(std::vector<Register>* out) const;
  TP_Type get_type(const TypeState& input, const Env& env, const DecompilerTypeSystem& dts) const;
  TP_Type get_type_int2(const TypeState& input,
                        const Env& env,
                        const DecompilerTypeSystem& dts) const;
  TP_Type get_type_int1(const TypeState& input,
                        const Env& env,
                        const DecompilerTypeSystem& dts) const;
  void collect_vars(VariableSet& vars) const;

 private:
  Kind m_kind = Kind::INVALID;
  SimpleAtom m_args[2];
  s8 n_args = -1;
};

int get_simple_expression_arg_count(SimpleExpression::Kind kind);

/*!
 * Set a variable equal to a Simple Expression
 */
class SetVarOp : public AtomicOp {
 public:
  SetVarOp(const Variable& dst, SimpleExpression src, int my_idx)
      : AtomicOp(my_idx), m_dst(dst), m_src(std::move(src)) {
    assert(my_idx == dst.idx());
  }
  virtual goos::Object to_form(const std::vector<DecompilerLabel>& labels,
                               const Env& env) const override;
  bool operator==(const AtomicOp& other) const override;
  bool is_sequence_point() const override;
  Variable get_set_destination() const override;
  FormElement* get_as_form(FormPool& pool, const Env& env) const override;
  void update_register_info() override;
  TypeState propagate_types_internal(const TypeState& input,
                                     const Env& env,
                                     DecompilerTypeSystem& dts) override;
  void collect_vars(VariableSet& vars) const override;

 private:
  Variable m_dst;
  SimpleExpression m_src;
};

/*!
 * An AsmOp represents a single inline assembly instruction. This is used when the BasicOpBuilder
 * pass decides that an instruction could not have been generated from high-level GOAL code, and
 * instead must be due to inline assembly.
 *
 * Each AsmOp stores the instruction it uses, as well as "Variable"s for each register used.
 */
class AsmOp : public AtomicOp {
 public:
  AsmOp(Instruction instr, int my_idx);
  goos::Object to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const override;
  bool operator==(const AtomicOp& other) const override;
  bool is_sequence_point() const override;
  Variable get_set_destination() const override;
  FormElement* get_as_form(FormPool& pool, const Env& env) const override;
  void update_register_info() override;
  TypeState propagate_types_internal(const TypeState& input,
                                     const Env& env,
                                     DecompilerTypeSystem& dts) override;
  void collect_vars(VariableSet& vars) const override;

 private:
  Instruction m_instr;
  std::optional<Variable> m_dst;
  std::optional<Variable> m_src[3];
};

/*!
 * A condition represents something that can generate a 0 or 1 based on a check or comparison.
 * This can be used as a branch condition in BranchOp
 * This can be used as a condition in an SetVarConditionOp, which sets a variable to a GOAL boolean.
 * Sometimes a SetVarConditionOp gets spread across many many instructions, in which case it is
 * not correctly detected here.
 */
class IR2_Condition {
 public:
  enum class Kind {
    NOT_EQUAL,
    EQUAL,
    LESS_THAN_SIGNED,
    GREATER_THAN_SIGNED,
    LEQ_SIGNED,
    GEQ_SIGNED,
    GREATER_THAN_ZERO_SIGNED,
    GREATER_THAN_ZERO_UNSIGNED,
    LEQ_ZERO_SIGNED,
    LEQ_ZERO_UNSIGNED,
    LESS_THAN_ZERO_SIGNED,
    GEQ_ZERO_SIGNED,
    LESS_THAN_ZERO_UNSIGNED,
    GEQ_ZERO_UNSIGNED,
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
    IS_PAIR,
    IS_NOT_PAIR,
    INVALID
  };

  IR2_Condition() = default;
  explicit IR2_Condition(Kind kind);
  IR2_Condition(Kind kind, const SimpleAtom& src0);
  IR2_Condition(Kind kind, const SimpleAtom& src0, const SimpleAtom& src1);

  void invert();
  bool operator==(const IR2_Condition& other) const;
  bool operator!=(const IR2_Condition& other) const { return !((*this) == other); }
  goos::Object to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const;
  void get_regs(std::vector<Register>* out) const;
  Kind kind() const { return m_kind; }
  const SimpleAtom& src(int i) const { return m_src[i]; }
  ConditionElement* get_as_form(FormPool& pool, const Env& env, int my_idx) const;
  void collect_vars(VariableSet& vars) const;

 private:
  Kind m_kind = Kind::INVALID;
  SimpleAtom m_src[2];
};

std::string get_condition_kind_name(IR2_Condition::Kind kind);
int get_condition_num_args(IR2_Condition::Kind kind);
IR2_Condition::Kind get_condition_opposite(IR2_Condition::Kind kind);

/*!
 * Set a variable to a GOAL boolean, based off of a condition.
 */
class SetVarConditionOp : public AtomicOp {
 public:
  SetVarConditionOp(Variable dst, IR2_Condition condition, int my_idx);
  goos::Object to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const override;
  bool operator==(const AtomicOp& other) const override;
  bool is_sequence_point() const override;
  Variable get_set_destination() const override;
  FormElement* get_as_form(FormPool& pool, const Env& env) const override;
  void update_register_info() override;
  void invert() { m_condition.invert(); }
  TypeState propagate_types_internal(const TypeState& input,
                                     const Env& env,
                                     DecompilerTypeSystem& dts) override;
  void collect_vars(VariableSet& vars) const override;

 private:
  Variable m_dst;
  IR2_Condition m_condition;
};

/*!
 * Store an Atom into a memory location.
 * Note - this is _not_ considered a set! form because you are not setting the value of a
 * register which can be expression-compacted.
 */
class StoreOp : public AtomicOp {
 public:
  StoreOp(int size, bool is_float, SimpleExpression addr, SimpleAtom value, int my_idx);
  goos::Object to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const override;
  bool operator==(const AtomicOp& other) const override;
  bool is_sequence_point() const override;
  Variable get_set_destination() const override;
  FormElement* get_as_form(FormPool& pool, const Env& env) const override;
  void update_register_info() override;
  TypeState propagate_types_internal(const TypeState& input,
                                     const Env& env,
                                     DecompilerTypeSystem& dts) override;
  void collect_vars(VariableSet& vars) const override;
  const SimpleExpression& addr() const { return m_addr; }
  const SimpleAtom& value() const { return m_value; }

 private:
  int m_size;
  bool m_is_float;
  SimpleExpression m_addr;
  SimpleAtom m_value;
};

/*!
 * Load a value into a variable.
 * This is treated as a set! form.
 */
class LoadVarOp : public AtomicOp {
 public:
  enum class Kind { UNSIGNED, SIGNED, FLOAT };
  LoadVarOp(Kind kind, int size, Variable dst, SimpleExpression src, int my_idx);
  goos::Object to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const override;
  bool operator==(const AtomicOp& other) const override;
  bool is_sequence_point() const override;
  Variable get_set_destination() const override;
  FormElement* get_as_form(FormPool& pool, const Env& env) const override;
  void update_register_info() override;
  TypeState propagate_types_internal(const TypeState& input,
                                     const Env& env,
                                     DecompilerTypeSystem& dts) override;
  TP_Type get_src_type(const TypeState& input, const Env& env, DecompilerTypeSystem& dts) const;
  void collect_vars(VariableSet& vars) const override;

 private:
  Kind m_kind;
  int m_size = -1;
  Variable m_dst;
  SimpleExpression m_src;
};

/*!
 * This represents one of the possible instructions that can go in a branch delay slot.
 * These will be "absorbed" into higher level structures, but for the purpose of printing AtomicOps,
 * it will be nice to have these print like expressions.
 *
 * These are always part of the branch op.
 */
class IR2_BranchDelay {
 public:
  enum class Kind {
    NOP,
    SET_REG_FALSE,
    SET_REG_TRUE,
    SET_REG_REG,
    SET_BINTEGER,
    SET_PAIR,
    DSLLV,
    NEGATE,
    UNKNOWN
  };

  explicit IR2_BranchDelay(Kind kind);
  IR2_BranchDelay(Kind kind, Variable var0);
  IR2_BranchDelay(Kind kind, Variable var0, Variable var1);
  IR2_BranchDelay(Kind kind, Variable var0, Variable var1, Variable var2);
  goos::Object to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const;
  bool operator==(const IR2_BranchDelay& other) const;
  void get_regs(std::vector<Register>* write, std::vector<Register>* read) const;
  bool is_known() const { return m_kind != Kind::UNKNOWN; }
  TypeState propagate_types(const TypeState& input,
                            const Env& env,
                            DecompilerTypeSystem& dts) const;
  void collect_vars(VariableSet& vars) const;
  Kind kind() const { return m_kind; }
  const Variable& var(int idx) const {
    assert(idx < 3);
    assert(m_var[idx].has_value());
    return m_var[idx].value();
  }

 private:
  std::optional<Variable> m_var[3];
  Kind m_kind = Kind::UNKNOWN;
};

/*!
 * This represents a combination of a condition + a branch + the branch delay slot.
 * This is considered as a single operation.
 */
class BranchOp : public AtomicOp {
 public:
  BranchOp(bool likely,
           IR2_Condition condition,
           int label,
           IR2_BranchDelay branch_delay,
           int my_idx);
  goos::Object to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const override;
  bool operator==(const AtomicOp& other) const override;
  bool is_sequence_point() const override;
  Variable get_set_destination() const override;
  FormElement* get_as_form(FormPool& pool, const Env& env) const override;
  void update_register_info() override;
  TypeState propagate_types_internal(const TypeState& input,
                                     const Env& env,
                                     DecompilerTypeSystem& dts) override;
  void collect_vars(VariableSet& vars) const override;
  const IR2_BranchDelay& branch_delay() const { return m_branch_delay; }
  const IR2_Condition& condition() const { return m_condition; }
  ConditionElement* get_condition_as_form(FormPool& pool, const Env& env) const;
  bool likely() const { return m_likely; }

 private:
  bool m_likely = false;
  IR2_Condition m_condition;
  int m_label = -1;
  IR2_BranchDelay m_branch_delay;
};

/*!
 * A "special" op has no arguments.
 * NOP, BREAK, SUSPEND,
 */
class SpecialOp : public AtomicOp {
 public:
  enum class Kind {
    NOP,
    BREAK,
    CRASH,
    SUSPEND,
  };

  SpecialOp(Kind kind, int my_idx);
  goos::Object to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const override;
  bool operator==(const AtomicOp& other) const override;
  bool is_sequence_point() const override;
  Variable get_set_destination() const override;
  FormElement* get_as_form(FormPool& pool, const Env& env) const override;
  void update_register_info() override;
  TypeState propagate_types_internal(const TypeState& input,
                                     const Env& env,
                                     DecompilerTypeSystem& dts) override;
  void collect_vars(VariableSet& vars) const override;
  Kind kind() const { return m_kind; }

 private:
  Kind m_kind;
};

/*!
 * Represents a function call.
 * This has so many special cases and exceptions that it is separate from SpecialOp.
 */
class CallOp : public AtomicOp {
 public:
  explicit CallOp(int my_idx);
  goos::Object to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const override;
  bool operator==(const AtomicOp& other) const override;
  bool is_sequence_point() const override;
  Variable get_set_destination() const override;
  FormElement* get_as_form(FormPool& pool, const Env& env) const override;
  void update_register_info() override;
  TypeState propagate_types_internal(const TypeState& input,
                                     const Env& env,
                                     DecompilerTypeSystem& dts) override;
  void collect_vars(VariableSet& vars) const override;
  const std::vector<Variable>& arg_vars() const { return m_arg_vars; }
  Variable function_var() const { return m_function_var; }

 protected:
  TypeSpec m_call_type;
  bool m_call_type_set = false;

  std::vector<Variable> m_arg_vars;
  Variable m_function_var;
  Variable m_return_var;
};

/*!
 * Unfortunately the original GOAL compiler does something weird when compiling (zero? x) or (not
 * (zero? x)) when the result needs to be stored in a GOAL boolean (not in a branch condition). It
 * first does a (set! result #t), then (possibly) a bunch of code to evaluate x, then does a
 * conditional move (movn/movz).  As a result, we can't recognize this as a Condition in the
 * AtomicOp pass. Instead we'll recognize it as a (set! result #t) .... (cmove result flag) where
 * flag is checked to be 0 or not.  It's weird because all of the other similar cases get this
 * right.
 *
 * Note - this isn't considered a variable set.  It's "conditional set" so it needs to be
 * handled separately. Unfortunately.
 */
class ConditionalMoveFalseOp : public AtomicOp {
 public:
  ConditionalMoveFalseOp(Variable dst, Variable src, bool on_zero, int my_idx);
  goos::Object to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const override;
  bool operator==(const AtomicOp& other) const override;
  bool is_sequence_point() const override;
  Variable get_set_destination() const override;
  FormElement* get_as_form(FormPool& pool, const Env& env) const override;
  void update_register_info() override;
  TypeState propagate_types_internal(const TypeState& input,
                                     const Env& env,
                                     DecompilerTypeSystem& dts) override;
  void collect_vars(VariableSet& vars) const override;

 private:
  Variable m_dst, m_src;
  bool m_on_zero;
};

struct IR2_RegOffset {
  Register reg;
  Variable var;
  int offset;
};

/*!
 * An extra operation inserted at the very end of a function.
 * It "reads" the return register V0.
 * During type analysis, call "mark_function_as_no_return_value" to update the register info if
 * we learn that this function does not return a value.
 */
class FunctionEndOp : public AtomicOp {
 public:
  explicit FunctionEndOp(int my_idx);
  virtual goos::Object to_form(const std::vector<DecompilerLabel>& labels,
                               const Env& env) const override;
  bool operator==(const AtomicOp& other) const override;
  bool is_sequence_point() const override;
  Variable get_set_destination() const override;
  FormElement* get_as_form(FormPool& pool, const Env& env) const override;
  void update_register_info() override;
  TypeState propagate_types_internal(const TypeState& input,
                                     const Env& env,
                                     DecompilerTypeSystem& dts) override;
  void collect_vars(VariableSet& vars) const override;
  void mark_function_as_no_return_value();
  const Variable& return_var() const {
    assert(m_function_has_return_value);
    return m_return_reg;
  }

 private:
  bool m_function_has_return_value = true;
  Variable m_return_reg;
};

bool get_as_reg_offset(const SimpleExpression& expr, IR2_RegOffset* out);
}  // namespace decompiler