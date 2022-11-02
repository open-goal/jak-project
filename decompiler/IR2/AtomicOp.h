#pragma once

#include <optional>
#include <string>
#include <utility>

#include "Env.h"

#include "common/goos/Object.h"
#include "common/util/Assert.h"

#include "decompiler/Disasm/Instruction.h"
#include "decompiler/Disasm/Register.h"
#include "decompiler/IR2/IR2_common.h"
#include "decompiler/util/MatchParam.h"

namespace decompiler {
class FormElement;
class ConditionElement;
class FormPool;
class DecompilerTypeSystem;
namespace types2 {
struct Instruction;
struct TypeState;
struct TypePropExtras;
}  // namespace types2

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
  virtual RegisterAccess get_set_destination() const = 0;

  // convert me to an expression. If I'm a set!, this will produce a (set! x y), which may be
  // undesirable when expression stacking.
  virtual FormElement* get_as_form(FormPool& pool, const Env& env) const = 0;

  // figure out what registers are read and written in this AtomicOp and update read_regs,
  // write_regs, and clobber_regs.  It's expected that these have duplicates if a register appears
  // in the original instructions multiple times. Ex: "and v0, v1, v1" would end up putting v1 in
  // read twice.
  virtual void update_register_info() = 0;

  virtual void collect_vars(RegAccessSet& vars) const = 0;

  TypeState propagate_types(const TypeState& input, const Env& env, DecompilerTypeSystem& dts);

  virtual void propagate_types2(types2::Instruction& instr,
                                const Env& env,
                                types2::TypeState& input_types,
                                DecompilerTypeSystem& dts,
                                types2::TypePropExtras& extras) = 0;

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
    SYMBOL_VAL_PTR,
    EMPTY_LIST,
    STATIC_ADDRESS,
    INVALID
  };

  SimpleAtom() = default;
  static SimpleAtom make_var(const RegisterAccess& var);
  static SimpleAtom make_sym_ptr(const std::string& name);
  static SimpleAtom make_sym_val(const std::string& name);
  static SimpleAtom make_sym_val_ptr(const std::string& name);
  static SimpleAtom make_empty_list();
  static SimpleAtom make_int_constant(s64 value);
  static SimpleAtom make_static_address(int static_label_id);
  goos::Object to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const;
  goos::Object to_form(const Env& env) const;
  std::string to_string(const Env& env) const;
  void collect_vars(RegAccessSet& vars) const;

  bool is_var() const { return m_kind == Kind::VARIABLE; }
  bool is_label() const { return m_kind == Kind::STATIC_ADDRESS; }
  const RegisterAccess& var() const {
    ASSERT(is_var());
    return m_variable;
  }

  int label() const {
    ASSERT(is_label());
    return m_int;
  }

  s64 get_int() const {
    ASSERT(is_int());
    return m_int;
  }
  bool is_int() const { return m_kind == Kind::INTEGER_CONSTANT; };
  bool is_int(s64 integer) const { return is_int() && get_int() == integer; }
  bool is_sym_ptr() const { return m_kind == Kind::SYMBOL_PTR; };
  bool is_sym_ptr(const std::string& str) const {
    return m_kind == Kind::SYMBOL_PTR && m_string == str;
  }
  bool is_sym_val() const { return m_kind == Kind::SYMBOL_VAL; };
  bool is_sym_val(const std::string& str) const {
    return m_kind == Kind::SYMBOL_VAL && m_string == str;
  }
  bool is_sym_val_ptr() const { return m_kind == Kind::SYMBOL_VAL_PTR; };
  bool is_empty_list() const { return m_kind == Kind::EMPTY_LIST; };
  bool is_static_addr() const { return m_kind == Kind::STATIC_ADDRESS; };
  Kind get_kind() const { return m_kind; }

  bool operator==(const SimpleAtom& other) const;
  bool operator!=(const SimpleAtom& other) const { return !((*this) == other); }
  void get_regs(std::vector<Register>* out) const;
  SimpleExpression as_expr() const;
  TP_Type get_type(const TypeState& input, const Env& env, const DecompilerTypeSystem& dts) const;
  const std::string& get_str() const {
    ASSERT(is_sym_ptr() || is_sym_val() || is_sym_val_ptr());
    return m_string;
  }
  void mark_as_float();
  bool is_integer_promoted_to_float() const;
  float get_integer_promoted_to_float() const;

 private:
  Kind m_kind = Kind::INVALID;
  std::string m_string;  // for symbol ptr and symbol val
  s64 m_int = -1;        // for integer constant and static address label id
  RegisterAccess m_variable;
  bool m_display_int_as_float = false;
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
    MAX_UNSIGNED,
    PCPYLD,
    VECTOR_PLUS,
    VECTOR_MINUS,
    VECTOR_FLOAT_PRODUCT,
    VECTOR_CROSS,
    SUBU_L32_S7,  // use SUBU X, src0, s7 to check if lower 32-bits are s7.
    VECTOR_3_DOT,
    VECTOR_4_DOT,
    VECTOR_LENGTH,            // jak 2 only.
    VECTOR_PLUS_FLOAT_TIMES,  // jak 2 only.
    SET_ON_LESS_THAN,
    SET_ON_LESS_THAN_IMM
  };

  // how many arguments?
  int args() const { return n_args; }
  const SimpleAtom& get_arg(int idx) const {
    ASSERT(idx < args());
    return m_args[idx];
  }

  SimpleAtom& get_arg(int idx) {
    ASSERT(idx < args());
    return m_args[idx];
  }
  Kind kind() const { return m_kind; }
  SimpleExpression() = default;
  SimpleExpression(Kind kind, const SimpleAtom& arg0);
  SimpleExpression(Kind kind, const SimpleAtom& arg0, const SimpleAtom& arg1);
  SimpleExpression(Kind kind,
                   const SimpleAtom& arg0,
                   const SimpleAtom& arg1,
                   const SimpleAtom& arg2);
  SimpleExpression(Kind kind,
                   const SimpleAtom& arg0,
                   const SimpleAtom& arg1,
                   const SimpleAtom& arg2,
                   const SimpleAtom& arg3);
  goos::Object to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const;
  std::string to_string(const Env& env) const;
  bool operator==(const SimpleExpression& other) const;
  bool is_identity() const { return m_kind == Kind::IDENTITY; }
  bool is_var() const { return is_identity() && get_arg(0).is_var(); }
  const RegisterAccess& var() const {
    ASSERT(is_var());
    return get_arg(0).var();
  }
  void get_regs(std::vector<Register>* out) const;
  TP_Type get_type(const TypeState& input, const Env& env, const DecompilerTypeSystem& dts) const;
  TP_Type get_type_int2(const TypeState& input,
                        const Env& env,
                        const DecompilerTypeSystem& dts) const;
  TP_Type get_type_int1(const TypeState& input,
                        const Env& env,
                        const DecompilerTypeSystem& dts) const;
  void collect_vars(RegAccessSet& vars) const;

 private:
  Kind m_kind = Kind::INVALID;
  SimpleAtom m_args[4];
  s8 n_args = -1;
};

int get_simple_expression_arg_count(SimpleExpression::Kind kind);

/*!
 * Set a variable equal to a Simple Expression
 */
class SetVarOp : public AtomicOp {
 public:
  SetVarOp(const RegisterAccess& dst, SimpleExpression src, int my_idx)
      : AtomicOp(my_idx), m_dst(dst), m_src(std::move(src)) {
    ASSERT(my_idx == dst.idx());
  }
  virtual goos::Object to_form(const std::vector<DecompilerLabel>& labels,
                               const Env& env) const override;
  bool operator==(const AtomicOp& other) const override;
  bool is_sequence_point() const override;
  RegisterAccess get_set_destination() const override;
  FormElement* get_as_form(FormPool& pool, const Env& env) const override;
  void update_register_info() override;
  TypeState propagate_types_internal(const TypeState& input,
                                     const Env& env,
                                     DecompilerTypeSystem& dts) override;
  void propagate_types2(types2::Instruction& instr,
                        const Env& env,
                        types2::TypeState& input_types,
                        DecompilerTypeSystem& dts,
                        types2::TypePropExtras& extras) override;
  void collect_vars(RegAccessSet& vars) const override;
  const RegisterAccess& dst() const { return m_dst; }
  const SimpleExpression& src() const { return m_src; }

 private:
  RegisterAccess m_dst;
  SimpleExpression m_src;
  std::optional<TypeSpec> m_source_type;
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
  goos::Object to_open_goal_form(const std::vector<DecompilerLabel>& labels, const Env& env) const;
  bool operator==(const AtomicOp& other) const override;
  bool is_sequence_point() const override;
  RegisterAccess get_set_destination() const override;
  FormElement* get_as_form(FormPool& pool, const Env& env) const override;
  void update_register_info() override;
  TypeState propagate_types_internal(const TypeState& input,
                                     const Env& env,
                                     DecompilerTypeSystem& dts) override;
  void propagate_types2(types2::Instruction& instr,
                        const Env& env,
                        types2::TypeState& input_types,
                        DecompilerTypeSystem& dts,
                        types2::TypePropExtras& extras) override;
  void collect_vars(RegAccessSet& vars) const override;
  const Instruction& instruction() const { return m_instr; }
  const std::optional<RegisterAccess> dst() const { return m_dst; }
  const std::optional<RegisterAccess> src(int i) const {
    ASSERT(i < 4);
    return m_src[i];
  }

 private:
  Instruction m_instr;
  std::optional<RegisterAccess> m_dst;
  std::optional<RegisterAccess> m_src[4];
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
  void collect_vars(RegAccessSet& vars) const;
  void make_flipped() { m_flipped_eval = true; }
  bool flipped() const { return m_flipped_eval; }

 private:
  Kind m_kind = Kind::INVALID;
  SimpleAtom m_src[2];
  bool m_flipped_eval = false;
};

std::string get_condition_kind_name(IR2_Condition::Kind kind);
int get_condition_num_args(IR2_Condition::Kind kind);
IR2_Condition::Kind get_condition_opposite(IR2_Condition::Kind kind);
bool condition_uses_float(IR2_Condition::Kind kind);

/*!
 * Set a variable to a GOAL boolean, based off of a condition.
 */
class SetVarConditionOp : public AtomicOp {
 public:
  SetVarConditionOp(RegisterAccess dst, IR2_Condition condition, int my_idx);
  goos::Object to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const override;
  bool operator==(const AtomicOp& other) const override;
  bool is_sequence_point() const override;
  RegisterAccess get_set_destination() const override;
  FormElement* get_as_form(FormPool& pool, const Env& env) const override;
  void update_register_info() override;
  void invert() { m_condition.invert(); }
  TypeState propagate_types_internal(const TypeState& input,
                                     const Env& env,
                                     DecompilerTypeSystem& dts) override;
  void propagate_types2(types2::Instruction& instr,
                        const Env& env,
                        types2::TypeState& input_types,
                        DecompilerTypeSystem& dts,
                        types2::TypePropExtras& extras) override;
  void collect_vars(RegAccessSet& vars) const override;

 private:
  RegisterAccess m_dst;
  IR2_Condition m_condition;
};

/*!
 * Store an Atom into a memory location.
 * Note - this is _not_ considered a set! form because you are not setting the value of a
 * register which can be expression-compacted.
 */
class StoreOp : public AtomicOp {
 public:
  enum class Kind { INTEGER, FLOAT, VECTOR_FLOAT, INVALID };
  StoreOp(int size, Kind kind, SimpleExpression addr, SimpleAtom value, int my_idx);
  goos::Object to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const override;
  bool operator==(const AtomicOp& other) const override;
  bool is_sequence_point() const override;
  RegisterAccess get_set_destination() const override;
  FormElement* get_as_form(FormPool& pool, const Env& env) const override;
  FormElement* get_vf_store_as_form(FormPool& pool, const Env& env) const;
  void update_register_info() override;
  TypeState propagate_types_internal(const TypeState& input,
                                     const Env& env,
                                     DecompilerTypeSystem& dts) override;
  void propagate_types2(types2::Instruction& instr,
                        const Env& env,
                        types2::TypeState& input_types,
                        DecompilerTypeSystem& dts,
                        types2::TypePropExtras& extras) override;
  void collect_vars(RegAccessSet& vars) const override;
  const SimpleExpression& addr() const { return m_addr; }
  const SimpleAtom& value() const { return m_value; }
  Kind kind() const { return m_kind; }
  int store_size() const { return m_size; }

 private:
  int m_size;
  Kind m_kind = Kind::INVALID;
  SimpleExpression m_addr;
  SimpleAtom m_value;
};

/*!
 * Load a value into a variable.
 * This is treated as a set! form.
 */
class LoadVarOp : public AtomicOp {
 public:
  enum class Kind { UNSIGNED, SIGNED, FLOAT, VECTOR_FLOAT, INVALID };
  LoadVarOp(Kind kind, int size, RegisterAccess dst, SimpleExpression src, int my_idx);
  goos::Object to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const override;
  bool operator==(const AtomicOp& other) const override;
  bool is_sequence_point() const override;
  RegisterAccess get_set_destination() const override;
  FormElement* get_as_form(FormPool& pool, const Env& env) const override;
  Form* get_load_src(FormPool& pool, const Env& env) const;
  void update_register_info() override;
  TypeState propagate_types_internal(const TypeState& input,
                                     const Env& env,
                                     DecompilerTypeSystem& dts) override;
  void propagate_types2(types2::Instruction& instr,
                        const Env& env,
                        types2::TypeState& input_types,
                        DecompilerTypeSystem& dts,
                        types2::TypePropExtras& extras) override;
  TP_Type get_src_type(const TypeState& input, const Env& env, DecompilerTypeSystem& dts) const;
  void collect_vars(RegAccessSet& vars) const override;
  const SimpleExpression& src() const { return m_src; }
  Kind kind() const { return m_kind; }
  int size() const { return m_size; }

 private:
  Kind m_kind;
  int m_size = -1;
  RegisterAccess m_dst;
  SimpleExpression m_src;
  std::optional<TypeSpec> m_type;
};

std::string load_kind_to_string(LoadVarOp::Kind kind);
FormElement* make_label_load(int label_idx,
                             const Env& env,
                             FormPool& pool,
                             int load_size,
                             LoadVarOp::Kind load_kind);

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
    NO_DELAY,
    UNKNOWN
  };

  explicit IR2_BranchDelay(Kind kind);
  IR2_BranchDelay(Kind kind, RegisterAccess var0);
  IR2_BranchDelay(Kind kind, RegisterAccess var0, RegisterAccess var1);
  IR2_BranchDelay(Kind kind, RegisterAccess var0, RegisterAccess var1, RegisterAccess var2);
  goos::Object to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const;
  bool operator==(const IR2_BranchDelay& other) const;
  void get_regs(std::vector<Register>* write, std::vector<Register>* read) const;
  bool is_known() const { return m_kind != Kind::UNKNOWN; }
  TypeState propagate_types(const TypeState& input,
                            const Env& env,
                            DecompilerTypeSystem& dts) const;
  void collect_vars(RegAccessSet& vars) const;
  Kind kind() const { return m_kind; }
  const RegisterAccess& var(int idx) const {
    ASSERT(idx < 3);
    ASSERT(m_var[idx].has_value());
    return m_var[idx].value();
  }

 private:
  std::optional<RegisterAccess> m_var[3];
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
  RegisterAccess get_set_destination() const override;
  FormElement* get_as_form(FormPool& pool, const Env& env) const override;
  void update_register_info() override;
  TypeState propagate_types_internal(const TypeState& input,
                                     const Env& env,
                                     DecompilerTypeSystem& dts) override;
  void propagate_types2(types2::Instruction& instr,
                        const Env& env,
                        types2::TypeState& input_types,
                        DecompilerTypeSystem& dts,
                        types2::TypePropExtras& extras) override;
  void collect_vars(RegAccessSet& vars) const override;
  const IR2_BranchDelay& branch_delay() const { return m_branch_delay; }
  const IR2_Condition& condition() const { return m_condition; }
  ConditionElement* get_condition_as_form(FormPool& pool, const Env& env) const;
  bool likely() const { return m_likely; }
  int label_id() const { return m_label; }

 private:
  bool m_likely = false;
  IR2_Condition m_condition;
  int m_label = -1;
  IR2_BranchDelay m_branch_delay;
};

/*!
 * This represents an unknown branch instruction that we think was generated from inline assembly
 */
class AsmBranchOp : public AtomicOp {
 public:
  AsmBranchOp(bool likely, IR2_Condition condition, int label, AtomicOp* branch_delay, int my_idx);

  AsmBranchOp(bool likely,
              IR2_Condition condition,
              int label,
              std::shared_ptr<AtomicOp> branch_delay,
              int my_idx);

  goos::Object to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const override;
  bool operator==(const AtomicOp& other) const override;
  bool is_sequence_point() const override;
  RegisterAccess get_set_destination() const override;
  FormElement* get_as_form(FormPool& pool, const Env& env) const override;
  void update_register_info() override;
  TypeState propagate_types_internal(const TypeState& input,
                                     const Env& env,
                                     DecompilerTypeSystem& dts) override;
  void propagate_types2(types2::Instruction& instr,
                        const Env& env,
                        types2::TypeState& input_types,
                        DecompilerTypeSystem& dts,
                        types2::TypePropExtras& extras) override;
  void collect_vars(RegAccessSet& vars) const override;
  bool is_likely() const { return m_likely; }
  const IR2_Condition& condition() const { return m_condition; }
  int label_id() const { return m_label; }
  AtomicOp* branch_delay() { return m_branch_delay; }

 private:
  bool m_likely = false;
  IR2_Condition m_condition;
  int m_label = -1;
  AtomicOp* m_branch_delay;
  std::shared_ptr<AtomicOp> m_branch_delay_sp;
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
  RegisterAccess get_set_destination() const override;
  FormElement* get_as_form(FormPool& pool, const Env& env) const override;
  void update_register_info() override;
  TypeState propagate_types_internal(const TypeState& input,
                                     const Env& env,
                                     DecompilerTypeSystem& dts) override;
  void propagate_types2(types2::Instruction& instr,
                        const Env& env,
                        types2::TypeState& input_types,
                        DecompilerTypeSystem& dts,
                        types2::TypePropExtras& extras) override;
  void collect_vars(RegAccessSet& vars) const override;
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
  RegisterAccess get_set_destination() const override;
  FormElement* get_as_form(FormPool& pool, const Env& env) const override;
  void update_register_info() override;
  TypeState propagate_types_internal(const TypeState& input,
                                     const Env& env,
                                     DecompilerTypeSystem& dts) override;
  void propagate_types2(types2::Instruction& instr,
                        const Env& env,
                        types2::TypeState& input_types,
                        DecompilerTypeSystem& dts,
                        types2::TypePropExtras& extras) override;
  void collect_vars(RegAccessSet& vars) const override;
  const std::vector<RegisterAccess>& arg_vars() const { return m_arg_vars; }
  RegisterAccess function_var() const { return m_function_var; }

 protected:
  TypeSpec m_call_type;
  bool m_call_type_set = false;

  std::vector<RegisterAccess> m_arg_vars;
  RegisterAccess m_function_var;
  RegisterAccess m_return_var;
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
  ConditionalMoveFalseOp(RegisterAccess dst,
                         RegisterAccess src,
                         RegisterAccess old_value,
                         bool on_zero,
                         int my_idx);
  goos::Object to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const override;
  bool operator==(const AtomicOp& other) const override;
  bool is_sequence_point() const override;
  RegisterAccess get_set_destination() const override;
  FormElement* get_as_form(FormPool& pool, const Env& env) const override;
  void update_register_info() override;
  TypeState propagate_types_internal(const TypeState& input,
                                     const Env& env,
                                     DecompilerTypeSystem& dts) override;
  void propagate_types2(types2::Instruction& instr,
                        const Env& env,
                        types2::TypeState& input_types,
                        DecompilerTypeSystem& dts,
                        types2::TypePropExtras& extras) override;
  void collect_vars(RegAccessSet& vars) const override;

 private:
  RegisterAccess m_dst, m_src, m_old_value;
  bool m_on_zero;
};

struct IR2_RegOffset {
  Register reg;
  RegisterAccess var;
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
  goos::Object to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const override;
  bool operator==(const AtomicOp& other) const override;
  bool is_sequence_point() const override;
  RegisterAccess get_set_destination() const override;
  FormElement* get_as_form(FormPool& pool, const Env& env) const override;
  void update_register_info() override;
  TypeState propagate_types_internal(const TypeState& input,
                                     const Env& env,
                                     DecompilerTypeSystem& dts) override;
  void propagate_types2(types2::Instruction& instr,
                        const Env& env,
                        types2::TypeState& input_types,
                        DecompilerTypeSystem& dts,
                        types2::TypePropExtras& extras) override;
  void collect_vars(RegAccessSet& vars) const override;
  void mark_function_as_no_return_value();
  const RegisterAccess& return_var() const {
    ASSERT(m_function_has_return_value);
    return m_return_reg;
  }

 private:
  bool m_function_has_return_value = true;
  RegisterAccess m_return_reg;
};

/*!
 * An operation to store a variable from the stack.
 */
class StackSpillStoreOp : public AtomicOp {
 public:
  StackSpillStoreOp(const SimpleAtom& value, int size, int offset, int my_idx);
  goos::Object to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const override;
  bool operator==(const AtomicOp& other) const override;
  bool is_sequence_point() const override;
  FormElement* get_as_form(FormPool& pool, const Env& env) const override;
  RegisterAccess get_set_destination() const override;
  void update_register_info() override;
  TypeState propagate_types_internal(const TypeState& input,
                                     const Env& env,
                                     DecompilerTypeSystem& dts) override;
  void propagate_types2(types2::Instruction& instr,
                        const Env& env,
                        types2::TypeState& input_types,
                        DecompilerTypeSystem& dts,
                        types2::TypePropExtras& extras) override;
  void collect_vars(RegAccessSet& vars) const override;
  int offset() const { return m_offset; }

 private:
  SimpleAtom m_value;
  int m_size;
  int m_offset;
};

/*!
 * An operation to load a variable from the stack.
 */
class StackSpillLoadOp : public AtomicOp {
 public:
  StackSpillLoadOp(RegisterAccess dst, int size, int offset, bool is_signed, int my_idx);
  goos::Object to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const override;
  bool operator==(const AtomicOp& other) const override;
  bool is_sequence_point() const override;
  FormElement* get_as_form(FormPool& pool, const Env& env) const override;
  RegisterAccess get_set_destination() const override;
  void update_register_info() override;
  TypeState propagate_types_internal(const TypeState& input,
                                     const Env& env,
                                     DecompilerTypeSystem& dts) override;
  void propagate_types2(types2::Instruction& instr,
                        const Env& env,
                        types2::TypeState& input_types,
                        DecompilerTypeSystem& dts,
                        types2::TypePropExtras& extras) override;
  void collect_vars(RegAccessSet& vars) const override;
  int offset() const { return m_offset; }

 private:
  RegisterAccess m_dst;
  int m_size;
  int m_offset;
  bool m_is_signed;
};

bool get_as_reg_offset(const SimpleExpression& expr, IR2_RegOffset* out);

bool is_op_2(AtomicOp* op,
             MatchParam<SimpleExpression::Kind> kind,
             MatchParam<Register> dst,
             MatchParam<Register> src0,
             Register* dst_out = nullptr,
             Register* src0_out = nullptr);
}  // namespace decompiler
