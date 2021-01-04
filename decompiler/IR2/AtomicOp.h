#pragma once

#include <string>
#include <optional>
#include <cassert>
#include "common/goos/Object.h"
#include "decompiler/Disasm/Register.h"
#include "decompiler/Disasm/Instruction.h"
#include "Env.h"

class LinkedObjectFile;
class Expr;

/*!
 * A "Variable" represents a register at a given instruction index.
 * The register can either be a GOAL local variable or a GOAL register used in inline assembly.
 * Because OpenGOAL's registers don't one-to-one map to GOAL registers, GOAL "inline assembly
 * registers" will become OpenGOAL variables, and are treated similarly to variables in
 * decompilation.
 *
 * In the earlier parts of decompilation, this just behaves like a register in all cases.
 * But in later parts registers can be mapped to real local variables with types. A variable can
 * look itself up in an environment to determine what "local variable" it is.
 *
 * Note: a variable is _not_ allowed to be R0, AT, S7, K0, K1, FP, or RA by default, as these
 * can never hold normal GOAL locals.  Inline assembly may use these, but you must set the allow_all
 * flag to true in the constructor of Variable to indicate this is what you really want.
 *
 * Note: access to the process pointer (s6) is handled as a variable. As a result, you may always
 * use s6 as a variable.
 */
class Variable {
 public:
  enum class Mode : u8 {
    READ,  // represents value of the variable at the beginning of the instruction
    WRITE  // represents value of the variable at the end of the instruction
  };

  Variable() = default;
  Variable(Mode mode, Register reg, int atomic_idx, bool allow_all = false);

  enum class Print {
    AS_REG,       // print as a PS2 register name
    FULL,         // print as a register name, plus an index, plus read or write
    AS_VARIABLE,  // print local variable name, error if impossible
    AUTOMATIC,    // print as variable, but if that's not possible print as reg.
  };

  std::string to_string(const Env* env, Print mode = Print::AUTOMATIC) const;

  bool operator==(const Variable& other) const;
  bool operator!=(const Variable& other) const;

 private:
  Mode m_mode = Mode::READ;  // do we represent a read or a write?
  Register m_reg;            // the EE register
  int m_atomic_idx = -1;     // the index in the function's list of AtomicOps
};

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
 */
class AtomicOp {
 public:
  explicit AtomicOp(int my_idx);
  virtual goos::Object to_form(const LinkedObjectFile* file, const Env* env) const = 0;
  virtual bool operator==(const AtomicOp& other) const = 0;
  bool operator!=(const AtomicOp& other) const;

  // determine if this is a (set! <var> thing) form. These will be handled differently in expression
  // building.
  virtual bool is_variable_set() const = 0;

  // determine if this is a GOAL "sequence point".
  // non-sequence point instructions may be out of order from the point of view of the expression
  // stack.
  virtual bool is_sequence_point() const = 0;

  // get the variable being set by this operation. Only call this if is_variable_set returns true.
  virtual Variable get_set_destination() const = 0;

  // get the value of the variable being set, as an expression. Only call this if is_variable_set
  // returns true.
  virtual std::unique_ptr<Expr> get_set_source_as_expr() const = 0;

  // convert me to an expression. If I'm a set!, this will produce a (set! x y), which may be
  // undesirable when expression stacking.
  virtual std::unique_ptr<Expr> get_as_expr() const = 0;

 private:
  int m_my_idx = -1;
};

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
  goos::Object to_form(const LinkedObjectFile* file, const Env* env) const;

  bool is_var() const { return m_kind == Kind::VARIABLE; }
  bool is_int() const { return m_kind == Kind::INTEGER_CONSTANT; };
  bool is_sym_ptr() const { return m_kind == Kind::SYMBOL_PTR; };
  bool is_sym_val() const { return m_kind == Kind::SYMBOL_VAL; };
  bool is_empty_list() const { return m_kind == Kind::EMPTY_LIST; };
  bool is_static_addr() const { return m_kind == Kind::STATIC_ADDRESS; };
  bool operator==(const SimpleAtom& other) const;
  bool operator!=(const SimpleAtom& other) const { return !((*this) == other); }

 private:
  Kind m_kind = Kind::INVALID;
  std::string m_string;  // for symbol ptr and symbol val
  s64 m_int = 0;         // for integer constant and static address label id
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
    NOT,
    NEG
  };

  // how many arguments?
  int args() const { return n_args; }
  const SimpleAtom& get_arg(int idx) const {
    assert(idx < args());
    return m_args[idx];
  }
  Kind kind() const { return m_kind; }

  SimpleExpression(Kind kind, const SimpleAtom& arg0);
  SimpleExpression(Kind kind, const SimpleAtom& arg0, const SimpleAtom& arg1);
  goos::Object to_form(const LinkedObjectFile* file, const Env* env) const;
  bool operator==(const SimpleExpression& other) const;

 private:
  Kind m_kind = Kind::INVALID;
  SimpleAtom m_args[2];
  s8 n_args = -1;
};

/*!
 * Set a variable equal to a Simple Expression
 */
class SetVarOp : public AtomicOp {
 public:
  SetVarOp(const Variable& dst, const SimpleExpression& src, int my_idx)
      : AtomicOp(my_idx), m_dst(dst), m_src(src) {}
  virtual goos::Object to_form(const LinkedObjectFile* file, const Env* env) const = 0;
  bool operator==(const AtomicOp& other) const override;
  bool is_variable_set() const override;
  bool is_sequence_point() const override;
  Variable get_set_destination() const override;
  std::unique_ptr<Expr> get_set_source_as_expr() const override;
  std::unique_ptr<Expr> get_as_expr() const override;

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
  AsmOp(const Instruction& instr, int my_idx);
  goos::Object to_form(const LinkedObjectFile* file, const Env* env) const override;
  bool operator==(const AtomicOp& other) const override;

 private:
  Instruction m_instr;
  bool m_has_dest = false;
  int m_n_src = -1;
  Variable m_dst;
  Variable m_src[3];
};

/*!
 * A condition represents something that can generate a 0 or 1 based on a check or comparison.
 * This can be used as a branch condition in BranchOp
 * This can be used as a condition in an SetVarConditionOp, which sets a variable to a GOAL boolean.
 * Sometimes a SetVarConditionOp gets spread across many many instructions, in which case it is
 * not correctly detected here.
 */
struct Condition {};

/*!
 *
 */
class SetVarConditonOp : public AtomicOp {
 public:
 private:
  Variable m_dst;
  Condition m_condition;
};

/*!
 * Store an Argument (or expression?)
 */
class StoreOp : public AtomicOp {
 public:
 private:
  SimpleExpression m_addr;
  SimpleExpression m_value;
};

/*!
 * Load a value into a variable
 */
class LoadVarOp : public AtomicOp {
 public:
 private:
};

/*!
 * A function call.
 */
class CallOp : public AtomicOp {};

struct BranchDelay {};

class BranchOp : public AtomicOp {};

/*!
 * NOP, BREAK, SUSPEND
 */
class SpecialOp : public AtomicOp {};

class ConditionalMoveOp : public AtomicOp {};