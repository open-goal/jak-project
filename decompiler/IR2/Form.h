#pragma once

#include <functional>
#include <memory>
#include <unordered_set>
#include <vector>

#include "common/goos/Object.h"
#include "common/math/Vector.h"
#include "common/type_system/TypeSystem.h"
#include "common/type_system/state.h"

#include "decompiler/Disasm/DecompilerLabel.h"
#include "decompiler/Disasm/Register.h"
#include "decompiler/IR2/AtomicOp.h"
#include "decompiler/IR2/LabelDB.h"
#include "decompiler/ObjectFile/LinkedWord.h"

namespace decompiler {
class Form;
class Env;
class FormStack;

/*!
 * A "FormElement" represents a single LISP form that's not a begin.
 * This is a abstract base class that all types of forms should be based on.
 */
class FormElement {
 public:
  Form* parent_form = nullptr;

  goos::Object to_form(const Env& env) const;
  virtual goos::Object to_form_internal(const Env& env) const = 0;
  virtual goos::Object to_form_as_condition_internal(const Env& env) const;
  virtual ~FormElement() = default;
  virtual void apply(const std::function<void(FormElement*)>& f) = 0;
  virtual void apply_form(const std::function<void(Form*)>& f) = 0;
  virtual bool is_sequence_point() const { return true; }
  virtual void collect_vars(RegAccessSet& vars, bool recursive) const = 0;
  virtual void get_modified_regs(RegSet& regs) const = 0;
  virtual bool active() const;

  // is this element reasonable to put directly in an if?
  // of course it's possible to put whatever you want, but it looks weird to do something like
  // (if (condition?)
  //   <some huge thing hundreds of lines long>
  //   <some other huge thing>
  //   )
  virtual bool allow_in_if() const { return true; }

  std::string to_string(const Env& env) const;
  bool has_side_effects();

  // push the result of this operation to the operation stack
  virtual void push_to_stack(const Env& env, FormPool& pool, FormStack& stack);
  virtual void update_from_stack(const Env& env,
                                 FormPool& pool,
                                 FormStack& stack,
                                 std::vector<FormElement*>* result,
                                 bool allow_side_effects);
  bool is_popped() const { return m_popped; }

  FormElement() = default;
  FormElement(const FormElement& other) = delete;
  FormElement& operator=(const FormElement& other) = delete;

  void mark_popped() {
    ASSERT(!m_popped);
    m_popped = true;
  }

 protected:
  friend class Form;
  bool m_popped = false;
};

/*!
 * A SimpleExpressionElement is a form which has the value of a SimpleExpression.
 * Like a SimpleExpression, it has no side effects.
 */
class SimpleExpressionElement : public FormElement {
 public:
  explicit SimpleExpressionElement(SimpleExpression expr, int my_idx);

  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  bool is_sequence_point() const override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  void get_modified_regs(RegSet& regs) const override;
  void update_from_stack_identity(const Env& env,
                                  FormPool& pool,
                                  FormStack& stack,
                                  std::vector<FormElement*>* result,
                                  bool allow_side_effects);
  void update_from_stack_gpr_to_fpr(const Env& env,
                                    FormPool& pool,
                                    FormStack& stack,
                                    std::vector<FormElement*>* result,
                                    bool allow_side_effects);
  void update_from_stack_fpr_to_gpr(const Env& env,
                                    FormPool& pool,
                                    FormStack& stack,
                                    std::vector<FormElement*>* result,
                                    bool allow_side_effects);
  void update_from_stack_div_s(const Env& env,
                               FormPool& pool,
                               FormStack& stack,
                               std::vector<FormElement*>* result,
                               bool allow_side_effects);
  void update_from_stack_float_2(const Env& env,
                                 FixedOperatorKind kind,
                                 FormPool& pool,
                                 FormStack& stack,
                                 std::vector<FormElement*>* result,
                                 bool allow_side_effects);
  void update_from_stack_float_2_nestable(const Env& env,
                                          FixedOperatorKind kind,
                                          FormPool& pool,
                                          FormStack& stack,
                                          std::vector<FormElement*>* result,
                                          bool allow_side_effects);
  void update_from_stack_float_1(const Env& env,
                                 FixedOperatorKind kind,
                                 FormPool& pool,
                                 FormStack& stack,
                                 std::vector<FormElement*>* result,
                                 bool allow_side_effects);
  void update_from_stack_si_1(const Env& env,
                              FixedOperatorKind kind,
                              FormPool& pool,
                              FormStack& stack,
                              std::vector<FormElement*>* result,
                              bool allow_side_effects);
  void update_from_stack_add_i(const Env& env,
                               FormPool& pool,
                               FormStack& stack,
                               std::vector<FormElement*>* result,
                               bool allow_side_effects);
  void update_from_stack_mult_si(const Env& env,
                                 FormPool& pool,
                                 FormStack& stack,
                                 std::vector<FormElement*>* result,
                                 bool allow_side_effects);
  void update_from_stack_lognot(const Env& env,
                                FormPool& pool,
                                FormStack& stack,
                                std::vector<FormElement*>* result,
                                bool allow_side_effects);
  void update_from_stack_force_si_2(const Env& env,
                                    FixedOperatorKind kind,
                                    FormPool& pool,
                                    FormStack& stack,
                                    std::vector<FormElement*>* result,
                                    bool allow_side_effects,
                                    bool reverse);
  void update_from_stack_force_ui_2(const Env& env,
                                    FixedOperatorKind kind,
                                    FormPool& pool,
                                    FormStack& stack,
                                    std::vector<FormElement*>* result,
                                    bool allow_side_effects);
  void update_from_stack_int_to_float(const Env& env,
                                      FormPool& pool,
                                      FormStack& stack,
                                      std::vector<FormElement*>* result,
                                      bool allow_side_effects);
  void update_from_stack_subu_l32_s7(const Env& env,
                                     FormPool& pool,
                                     FormStack& stack,
                                     std::vector<FormElement*>* result,
                                     bool allow_side_effects);
  void update_from_stack_float_to_int(const Env& env,
                                      FormPool& pool,
                                      FormStack& stack,
                                      std::vector<FormElement*>* result,
                                      bool allow_side_effects);
  void update_from_stack_copy_first_int_2(const Env& env,
                                          FixedOperatorKind kind,
                                          FormPool& pool,
                                          FormStack& stack,
                                          std::vector<FormElement*>* result,
                                          bool allow_side_effects);
  void update_from_stack_left_shift(const Env& env,
                                    FormPool& pool,
                                    FormStack& stack,
                                    std::vector<FormElement*>* result,
                                    bool allow_side_effects);
  void update_from_stack_right_shift_logic(const Env& env,
                                           FormPool& pool,
                                           FormStack& stack,
                                           std::vector<FormElement*>* result,
                                           bool allow_side_effects);
  void update_from_stack_right_shift_arith(const Env& env,
                                           FormPool& pool,
                                           FormStack& stack,
                                           std::vector<FormElement*>* result,
                                           bool allow_side_effects);
  FormElement* update_from_stack_logor_or_logand_helper(const Env& env,
                                                        FixedOperatorKind kind,
                                                        FormPool& pool,
                                                        FormStack& stack,
                                                        bool allow_side_effects);
  void update_from_stack_logor_or_logand(const Env& env,
                                         FixedOperatorKind kind,
                                         FormPool& pool,
                                         FormStack& stack,
                                         std::vector<FormElement*>* result,
                                         bool allow_side_effects);
  void update_from_stack_pcypld(const Env& env,
                                FormPool& pool,
                                FormStack& stack,
                                std::vector<FormElement*>* result,
                                bool allow_side_effects);
  void update_from_stack_vector_plus_minus_cross(FixedOperatorKind op_kind,
                                                 const Env& env,
                                                 FormPool& pool,
                                                 FormStack& stack,
                                                 std::vector<FormElement*>* result,
                                                 bool allow_side_effects);
  void update_from_stack_vector_float_product(const Env& env,
                                              FormPool& pool,
                                              FormStack& stack,
                                              std::vector<FormElement*>* result,
                                              bool allow_side_effects);
  void update_from_stack_vector_plus_float_times(const Env& env,
                                                 FormPool& pool,
                                                 FormStack& stack,
                                                 std::vector<FormElement*>* result,
                                                 bool allow_side_effects);
  void update_from_stack_vectors_in_common(FixedOperatorKind kind,
                                           const Env& env,
                                           FormPool& pool,
                                           FormStack& stack,
                                           std::vector<FormElement*>* result,
                                           bool allow_side_effects);

  const SimpleExpression& expr() const { return m_expr; }

 private:
  SimpleExpression m_expr;
  int m_my_idx;
};

/*!
 * Represents storing a value into memory.
 * This is only used as a placeholder if AtomicOpForm fails to convert it to something nicer.
 */
class StoreElement : public FormElement {
 public:
  explicit StoreElement(const StoreOp* op);

  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
  const StoreOp* op() const { return m_op; }

 private:
  // todo - we may eventually want to use a different representation for more
  // complicated store paths.
  const StoreOp* m_op;
};

/*!
 * Representing a value loaded from memory.  Not the destination.
 */
class LoadSourceElement : public FormElement {
 public:
  LoadSourceElement(Form* addr,
                    int size,
                    LoadVarOp::Kind kind,
                    const std::optional<IR2_RegOffset>& load_source_ro,
                    const TP_Type& ro_reg_type);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  int size() const { return m_size; }
  LoadVarOp::Kind kind() const { return m_kind; }
  const Form* location() const { return m_addr; }
  Form* location() { return m_addr; }
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  void get_modified_regs(RegSet& regs) const override;

 private:
  Form* m_addr = nullptr;
  int m_size = -1;
  LoadVarOp::Kind m_kind;
  std::optional<IR2_RegOffset> m_load_source_ro;
  TP_Type m_ro_reg_type;
};

/*!
 * Representing an indivisible thing, like an integer constant variable, etc.
 * Just a wrapper around SimpleAtom.
 */
class SimpleAtomElement : public FormElement {
 public:
  explicit SimpleAtomElement(const SimpleAtom& var, bool omit_var_cast = false);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  const SimpleAtom& atom() const { return m_atom; }
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;

 private:
  SimpleAtom m_atom;
  bool m_omit_var_cast;
};

/*!
 * Set a variable to a Form.  This is the set! form to be used for expression building.
 */
class SetVarElement : public FormElement {
 public:
  SetVarElement(const RegisterAccess& var,
                Form* value,
                bool is_sequence_point,
                TypeSpec src_type,
                const SetVarInfo& info = {});
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  bool is_sequence_point() const override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
  void get_modified_regs(RegSet& regs) const override;
  bool active() const override;

  const RegisterAccess& dst() const { return m_dst; }
  const Form* src() const { return m_src; }
  Form* src() { return m_src; }
  bool is_eliminated_coloring_move() const { return m_var_info.is_eliminated_coloring_move; }
  void eliminate_as_coloring_move() { m_var_info.is_eliminated_coloring_move = true; }

  bool is_dead_set() const { return m_var_info.is_dead_set; }
  void mark_as_dead_set() { m_var_info.is_dead_set = true; }

  bool is_dead_false_set() const { return m_var_info.is_dead_false; }
  void mark_as_dead_false() { m_var_info.is_dead_false = true; }

  const SetVarInfo& info() const { return m_var_info; }
  const TypeSpec src_type() const { return m_src_type; }

  std::optional<TypeSpec> required_cast(const Env& env) const;

 private:
  RegisterAccess m_dst;
  Form* m_src = nullptr;
  bool m_is_sequence_point = true;
  TypeSpec m_src_type;
  SetVarInfo m_var_info;
};

class StoreInSymbolElement : public FormElement {
 public:
  StoreInSymbolElement(std::string sym_name,
                       SimpleExpression value,
                       std::optional<TypeSpec> cast_for_set,
                       std::optional<TypeSpec> cast_for_define,
                       int my_idx);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
  void get_modified_regs(RegSet& regs) const override;

 private:
  std::string m_sym_name;
  SimpleExpression m_value;
  std::optional<TypeSpec> m_cast_for_set;
  std::optional<TypeSpec> m_cast_for_define;
  int m_my_idx = -1;
};

class StoreInPairElement : public FormElement {
 public:
  StoreInPairElement(bool is_car, RegisterAccess pair, SimpleExpression value, int my_idx);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
  void get_modified_regs(RegSet& regs) const override;

 private:
  bool m_is_car = false;
  RegisterAccess m_pair;
  SimpleExpression m_value;
  int m_my_idx = -1;
};

/*!
 * Like SetVar, but sets a form to another form.
 * This is intended to be used with stores.
 * NOTE: do not use this when SetVarElement could be used instead.
 */
class SetFormFormElement : public FormElement {
 public:
  SetFormFormElement(Form* dst,
                     Form* src,
                     std::optional<TypeSpec> cast_for_set = {},
                     std::optional<TypeSpec> cast_for_define = {});
  goos::Object to_form_internal(const Env& env) const override;
  goos::Object to_form_for_define(const Env& env,
                                  const std::optional<std::string>& docstring) const;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  bool is_sequence_point() const override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
  void get_modified_regs(RegSet& regs) const override;

  const Form* src() const { return m_src; }
  const Form* dst() const { return m_dst; }
  Form* src() { return m_src; }
  Form* dst() { return m_dst; }
  const std::optional<TypeSpec>& cast_for_set() const { return m_cast_for_set; }
  const std::optional<TypeSpec>& cast_for_define() const { return m_cast_for_define; }
  void set_cast_for_set(const std::optional<TypeSpec>& ts) { m_cast_for_set = ts; }
  void set_cast_for_define(const std::optional<TypeSpec>& ts) { m_cast_for_define = ts; }

 private:
  int m_real_push_count = 0;
  Form* m_dst = nullptr;
  Form* m_src = nullptr;
  std::optional<TypeSpec> m_cast_for_set, m_cast_for_define;
};

/*!
 * A wrapper around a single AtomicOp.
 * The "important" special AtomicOps have their own Form type, like FunctionCallElement.
 */
class AtomicOpElement : public FormElement {
 public:
  explicit AtomicOpElement(AtomicOp* op);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
  void get_modified_regs(RegSet& regs) const override;
  const AtomicOp* op() const { return m_op; }
  AtomicOp* op() { return m_op; }

 private:
  AtomicOp* m_op = nullptr;  // not const because of asm likely merging
};

class AsmBranchElement : public FormElement {
 public:
  AsmBranchElement(AsmBranchOp* branch_op, Form* branch_delay, bool likely);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
  void get_modified_regs(RegSet& regs) const override;

 private:
  AsmBranchOp* m_branch_op = nullptr;
  Form* m_branch_delay = nullptr;
  bool m_likely = false;
};

class TranslatedAsmBranch : public FormElement {
 public:
  TranslatedAsmBranch(Form* branch_condition, Form* branch_delay, int label_id, bool likely);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  // void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
  void get_modified_regs(RegSet& regs) const override;

 private:
  Form* m_branch_condition = nullptr;
  Form* m_branch_delay = nullptr;
  int m_label_id = -1;
  bool m_likely = false;
};

/*!
 * A wrapper around a single AsmOp
 */
class AsmOpElement : public FormElement {
 public:
  explicit AsmOpElement(const AsmOp* op);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
  void get_modified_regs(RegSet& regs) const override;
  const AsmOp* op() const { return m_op; }

 private:
  const AsmOp* m_op;
};

/*!
 * A wrapper around a single AsmOp, with OpenGOAL considerations
 */
class OpenGoalAsmOpElement : public FormElement {
 public:
  explicit OpenGoalAsmOpElement(const AsmOp* op);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void collect_vf_regs(RegSet& regs) const;
  void get_modified_regs(RegSet& regs) const override;
  const AsmOp* op() const { return m_op; }

 private:
  const AsmOp* m_op;
};

/*!
 * A "condition" like (< a b). This can be used as a boolean value directly: (set! a (< b c))
 * or it can be used as a branch condition: (if (< a b)). As a result, it implements both push
 * and update.
 *
 * In the first case, it can be either a conditional move or actually branching. GOAL seems to use
 * the branching when sometimes it could have used the conditional move, and for now, we don't
 * care about the difference.
 */
class ConditionElement : public FormElement {
 public:
  ConditionElement(IR2_Condition::Kind kind,
                   std::optional<SimpleAtom> src0,
                   std::optional<SimpleAtom> src1,
                   RegSet consumed,
                   bool flipped);
  goos::Object to_form_internal(const Env& env) const override;
  goos::Object to_form_as_condition_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  void get_modified_regs(RegSet& regs) const override;
  void invert();
  const RegSet& consume() const { return m_consumed; }

  FormElement* make_generic(const Env& env,
                            FormPool& pool,
                            const std::vector<Form*>& source_forms,
                            const std::vector<TypeSpec>& types);
  FormElement* make_zero_check_generic(const Env& env,
                                       FormPool& pool,
                                       const std::vector<Form*>& source_forms,
                                       const std::vector<TypeSpec>& types);
  FormElement* make_nonzero_check_generic(const Env& env,
                                          FormPool& pool,
                                          const std::vector<Form*>& source_forms,
                                          const std::vector<TypeSpec>& types);
  FormElement* make_equal_check_generic(const Env& env,
                                        FormPool& pool,
                                        const std::vector<Form*>& source_forms,
                                        const std::vector<TypeSpec>& types);
  FormElement* make_not_equal_check_generic(const Env& env,
                                            FormPool& pool,
                                            const std::vector<Form*>& source_forms,
                                            const std::vector<TypeSpec>& types);
  FormElement* make_less_than_zero_signed_check_generic(const Env& env,
                                                        FormPool& pool,
                                                        const std::vector<Form*>& source_forms,
                                                        const std::vector<TypeSpec>& types);
  FormElement* make_geq_zero_unsigned_check_generic(const Env& env,
                                                    FormPool& pool,
                                                    const std::vector<Form*>& source_forms,
                                                    const std::vector<TypeSpec>& types);
  FormElement* make_geq_zero_signed_check_generic(const Env& env,
                                                  FormPool& pool,
                                                  const std::vector<Form*>& source_forms,
                                                  const std::vector<TypeSpec>& types);
  bool allow_in_if() const override { return false; }

 private:
  IR2_Condition::Kind m_kind;
  std::optional<SimpleAtom> m_src[2];
  RegSet m_consumed;
  bool m_flipped;
};

/*!
 * Wrapper around an AtomicOp call.
 */
class FunctionCallElement : public FormElement {
 public:
  explicit FunctionCallElement(const CallOp* op);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
  void get_modified_regs(RegSet& regs) const override;

 private:
  const CallOp* m_op;
};

/*!
 * Wrapper around an AtomicOp branch.  These are inserted when directly converting blocks to Form,
 * but should be eliminated after the cfg_builder pass completes.
 */
class BranchElement : public FormElement {
 public:
  explicit BranchElement(const BranchOp* op);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
  const BranchOp* op() const { return m_op; }

 private:
  const BranchOp* m_op;
};

/*!
 * Represents a (return-from #f x) form, which immediately returns from the function.
 * This always has some "dead code" after it that can't be reached, which is the "dead_code".
 * We store the dead code because it may contain an unreachable jump to the next place that can
 * be stripped away in later analysis passes. Or they may have written code after the return.
 */
class ReturnElement : public FormElement {
 public:
  Form* return_code = nullptr;
  Form* dead_code = nullptr;
  ReturnElement(Form* _return_code, Form* _dead_code);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
  void get_modified_regs(RegSet& regs) const override;
  std::optional<TypeSpec> return_type;
};

/*!
 * Represents a (return-from Lxxx x) form, which returns from a block which ends before the end
 * of the function.  These are used pretty rarely. As a result, I'm not planning to allow these to
 * next within other expressions. This means that the following code:
 *
 * (set! x (block my-block
 *           (if (condition?)
 *               (return-from my-block 12))
 *         2))
 *
 * Would become
 *
 * (block my-block
 *   (when (condition?)
 *         (set! x 12)
 *         (return-from my-block none))
 *   (set! x 2)
 * )
 *
 * which seems fine to me.
 */
class BreakElement : public FormElement {
 public:
  Form* return_code = nullptr;
  Form* dead_code = nullptr;
  int lid = -1;
  BreakElement(Form* _return_code, Form* _dead_code, int _lid);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
};

/*!
 * Condition (cond, if, when, unless) which has an "else" case.
 * The condition of the first entry may contain too much and will need to be adjusted later.
 * Example:
 *
 * (set! x 10)
 * (if (something?) ... )
 *
 * might become
 * (if (begin (set! x 10) (something?)) ... )
 *
 * We want to wait until after expressions are built to move the extra stuff up to avoid splitting
 * up a complicated expression used as the condition.  But this should happen before variable
 * scoping.
 */
class CondWithElseElement : public FormElement {
 public:
  struct Entry {
    Form* condition = nullptr;
    Form* body = nullptr;
    bool cleaned = false;
  };
  std::vector<Entry> entries;
  Form* else_ir = nullptr;
  bool already_rewritten = false;
  CondWithElseElement(std::vector<Entry> _entries, Form* _else_ir);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
  void get_modified_regs(RegSet& regs) const override;
  bool allow_in_if() const override { return false; }
};

/*!
 * An empty element. This is used to fill the body of control forms with nothing in them.
 * For example, I believe that (cond ((x y) (else none))) will generate an else case with an
 * "empty" and looks different from (cond ((x y))).
 *
 * We _could_ simplify out the use of empty, but I think it's more "authentic" to leave them in, and
 * might give us more clues about how the code was originally written
 */
class EmptyElement : public FormElement {
 public:
  EmptyElement() = default;
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
};

/*!
 * Represents an OpenGOAL 'rlet' expressions
 */
class RLetElement : public FormElement {
 public:
  enum class RegClass { VF };

  explicit RLetElement(Form* _body, RegSet _regs);
  goos::Object to_form_internal(const Env& env) const override;
  goos::Object reg_list() const;
  bool needs_vf0_init() const;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  Form* body = nullptr;
  std::vector<decompiler::Register> sorted_regs;
};

/*!
 * Represents a GOAL while loop and more complicated loops which have the "while" format of checking
 * the condition before the first loop. This will not include infinite while loops.
 * Unlike CondWithElseElement, this will correctly identify the start and end of the condition.
 */
class WhileElement : public FormElement {
 public:
  WhileElement(Form* _condition, Form* _body);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
  void get_modified_regs(RegSet& regs) const override;
  bool allow_in_if() const override { return false; }
  Form* condition = nullptr;
  Form* body = nullptr;
  bool cleaned = false;
};

/*!
 * Represents a GOAL until loop and more complicated loops which use the "until" format of checking
 * the condition after the first iteration. Has the same limitation as CondWithElseElement for the
 * condition.
 */
class UntilElement : public FormElement {
 public:
  UntilElement(Form* _condition, Form* _body);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
  void get_modified_regs(RegSet& regs) const override;
  bool allow_in_if() const override { return false; }
  Form* condition = nullptr;
  Form* body = nullptr;
  std::optional<RegisterAccess> false_destination;  // used in jak 2, sometimes.
};

/*!
 * Represents a GOAL short-circuit expression, either AND or OR.
 * The first "element" in ShortCircuitElement may be too large, see the comment on
 * CondWithElseElement
 */
class ShortCircuitElement : public FormElement {
 public:
  struct Entry {
    Form* condition = nullptr;
    std::optional<SetVarOp> branch_delay;
    // in the case where there's no else, each delay slot will write #f to the "output" register.
    // this can be with an or <output>, s7, r0
    //    Form* output = nullptr; // todo, what? add to collect vars if we need it?
    bool is_output_trick = false;
    bool cleaned = false;
  };

  enum Kind { UNKNOWN, AND, OR } kind = UNKNOWN;

  RegisterAccess final_result;
  std::vector<Entry> entries;
  std::optional<bool> used_as_value = std::nullopt;
  bool already_rewritten = false;

  explicit ShortCircuitElement(std::vector<Entry> _entries);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  void get_modified_regs(RegSet& regs) const override;
};

/*!
 * Represents a GOAL cond/if/when/unless statement which does not have an explicit else case. The
 * compiler will then move #f into the result register in the delay slot. The first condition may be
 * too large at first, see CondWithElseElement
 */
class CondNoElseElement : public FormElement {
 public:
  struct Entry {
    Form* condition = nullptr;
    Form* body = nullptr;
    std::optional<RegisterAccess> false_destination;
    FormElement* original_condition_branch = nullptr;
    bool cleaned = false;
  };
  RegisterAccess final_destination;
  bool used_as_value = false;
  bool already_rewritten = false;
  std::vector<Entry> entries;
  explicit CondNoElseElement(std::vector<Entry> _entries);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
  void get_modified_regs(RegSet& regs) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  bool allow_in_if() const override { return false; }
};

class CaseElement : public FormElement {
 public:
  struct Entry {
    std::vector<Form*> vals;
    Form* body = nullptr;
  };

  CaseElement(Form* value, const std::vector<Entry>& entries, Form* else_body);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  bool allow_in_if() const override { return false; }

 private:
  Form* m_value = nullptr;
  std::vector<Entry> m_entries;
  Form* m_else_body = nullptr;  // may be nullptr, if no else.
};

/*!
 * Represents a (abs x) expression.
 */
class AbsElement : public FormElement {
 public:
  explicit AbsElement(RegisterAccess _source, RegSet _consumed);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  void get_modified_regs(RegSet& regs) const override;
  RegisterAccess source;
  RegSet consumed;
};

/*!
 * Represents an (ash x y) expression. There is also an "unsigned" version of this using logical
 * shifts. This only recognizes the fancy version where the shift amount isn't known at compile time
 * and the compiler emits code that branches depending on the sign of the shift amount.
 */
class AshElement : public FormElement {
 public:
  RegisterAccess shift_amount, value;
  std::optional<RegisterAccess> clobber;
  bool is_signed = true;
  RegSet consumed;
  AshElement(RegisterAccess _shift_amount,
             RegisterAccess _value,
             std::optional<RegisterAccess> _clobber,
             bool _is_signed,
             RegSet _consumed);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  void get_modified_regs(RegSet& regs) const override;
};

/*!
 * Represents a form which gets the runtime type of a boxed object. This is for the most general
 * "object" case where we check for pair, binteger, or basic and there's actually branching.
 */
class TypeOfElement : public FormElement {
 public:
  Form* value;
  TypeOfElement(Form* _value);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
};

/*!
 * Represents an unpaired cmove #f.  GOAL may emit code like
 * (set! x #t)
 * (... evaluate something)
 * (cmov x y #f)
 * where the stuff in between is potentially very large.
 * GOAL has no "condition move" keyword available to the programmer - this would only happen if when
 * doing something like (set! x (zero? y)), in the code for creating a GOAL boolean.
 *
 * Code like (if x (set! y z)) will branch, the compiler isn't smart enough to use movn/movz here.
 *
 * These cannot be compacted into a single form until expression building, so we leave these
 * placeholders in.
 *
 * Note - some conditionals put the (set! x #t) immediately before the cmove, but not all. Those
 * that do will be correctly recognized and will be a ConditionElement. zero! seems to be the most
 * common one that's split, and it happens reasonably often, so I will try to actually correct it.
 */
class ConditionalMoveFalseElement : public FormElement {
 public:
  RegisterAccess dest;
  RegisterAccess old_value;
  RegisterAccess source;
  bool on_zero = false;
  ConditionalMoveFalseElement(RegisterAccess _dest,
                              RegisterAccess _old_value,
                              RegisterAccess _source,
                              bool _on_zero);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
};

std::string fixed_operator_to_string(FixedOperatorKind kind);

/*!
 * A GenericOperator is the head of a GenericElement.
 * It is used for the final output.
 */
class GenericOperator {
 public:
  enum class Kind { FIXED_OPERATOR, CONDITION_OPERATOR, FUNCTION_EXPR, INVALID };

  static GenericOperator make_fixed(FixedOperatorKind kind);
  static GenericOperator make_function(Form* value);
  static GenericOperator make_compare(IR2_Condition::Kind kind);
  void collect_vars(RegAccessSet& vars, bool recursive) const;
  goos::Object to_form(const Env& env) const;
  void apply(const std::function<void(FormElement*)>& f);
  void apply_form(const std::function<void(Form*)>& f);
  bool operator==(const GenericOperator& other) const;
  bool operator!=(const GenericOperator& other) const;
  void get_modified_regs(RegSet& regs) const;
  Kind kind() const { return m_kind; }
  FixedOperatorKind fixed_kind() const {
    ASSERT(m_kind == Kind::FIXED_OPERATOR);
    return m_fixed_kind;
  }

  IR2_Condition::Kind condition_kind() const {
    ASSERT(m_kind == Kind::CONDITION_OPERATOR);
    return m_condition_kind;
  }

  bool is_func() const { return m_kind == Kind::FUNCTION_EXPR; }

  const Form* func() const {
    ASSERT(m_kind == Kind::FUNCTION_EXPR);
    return m_function;
  }

  Form* func() {
    ASSERT(m_kind == Kind::FUNCTION_EXPR);
    return m_function;
  }

  bool is_fixed(FixedOperatorKind kind) const {
    return m_kind == Kind::FIXED_OPERATOR && m_fixed_kind == kind;
  }

  bool is_fixed() const { return m_kind == Kind::FIXED_OPERATOR; }

 private:
  friend class GenericElement;
  Kind m_kind = Kind::INVALID;
  IR2_Condition::Kind m_condition_kind = IR2_Condition::Kind::INVALID;
  FixedOperatorKind m_fixed_kind = FixedOperatorKind::INVALID;
  Form* m_function = nullptr;
};

class GenericElement : public FormElement {
 public:
  explicit GenericElement(GenericOperator op);
  GenericElement(GenericOperator op, Form* arg);
  GenericElement(GenericOperator op, Form* arg0, Form* arg1);
  GenericElement(GenericOperator op, std::vector<Form*> forms);

  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  void get_modified_regs(RegSet& regs) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
  const GenericOperator& op() const { return m_head; }
  GenericOperator& op() { return m_head; }
  const std::vector<Form*>& elts() const { return m_elts; }
  std::vector<Form*>& elts() { return m_elts; }

 private:
  GenericOperator m_head;
  std::vector<Form*> m_elts;
};

class CastElement : public FormElement {
 public:
  explicit CastElement(TypeSpec type, Form* source, bool numeric = false);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  const TypeSpec& type() const { return m_type; }
  const Form* source() const { return m_source; }
  void set_type(const TypeSpec& ts) { m_type = ts; }
  Form* source() { return m_source; }
  bool numeric() const { return m_numeric; }

 private:
  TypeSpec m_type;
  Form* m_source = nullptr;
  bool m_numeric = false;  // if true, use the. otherwise the-as
};

class DynamicMethodAccess : public FormElement {
 public:
  explicit DynamicMethodAccess(RegisterAccess source);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  void get_modified_regs(RegSet& regs) const override;

 private:
  RegisterAccess m_source;
};

class GetMethodElement : public FormElement {
 public:
  GetMethodElement(Form* in, std::string name, bool is_object);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;

 private:
  Form* m_in = nullptr;
  std::string m_name;
  bool m_is_object = false;
};

class StringConstantElement : public FormElement {
 public:
  StringConstantElement(const std::string& value);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;

 private:
  std::string m_value;
};

class ConstantTokenElement : public FormElement {
 public:
  explicit ConstantTokenElement(const std::string& value);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  const std::string& value() const { return m_value; }

 private:
  std::string m_value;
};

class ConstantFloatElement : public FormElement {
 public:
  explicit ConstantFloatElement(float value);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  float value() const { return m_value; }

 private:
  float m_value;
};

class DerefToken {
 public:
  enum class Kind {
    INTEGER_CONSTANT,
    INTEGER_EXPRESSION,  // some form which evaluates to an integer index. Not offset, index.
    FIELD_NAME,
    EXPRESSION_PLACEHOLDER,
    INVALID
  };
  static DerefToken make_int_constant(s64 int_constant);
  static DerefToken make_int_expr(Form* expr);
  static DerefToken make_field_name(const std::string& name);
  static DerefToken make_expr_placeholder();

  void collect_vars(RegAccessSet& vars, bool recursive) const;
  goos::Object to_form(const Env& env) const;
  void apply(const std::function<void(FormElement*)>& f);
  void apply_form(const std::function<void(Form*)>& f);
  void get_modified_regs(RegSet& regs) const;

  bool is_field_name(const std::string& name) const {
    return m_kind == Kind::FIELD_NAME && m_name == name;
  }

  bool is_int() const { return m_kind == Kind::INTEGER_CONSTANT; }
  bool is_int(int x) const { return m_kind == Kind::INTEGER_CONSTANT && m_int_constant == x; }

  bool is_expr() const { return m_kind == Kind::INTEGER_EXPRESSION; }

  Kind kind() const { return m_kind; }
  const std::string& field_name() const {
    ASSERT(m_kind == Kind::FIELD_NAME);
    return m_name;
  }

  s64 int_constant() const { return m_int_constant; }

  Form* expr() {
    ASSERT(m_kind == Kind::INTEGER_EXPRESSION);
    return m_expr;
  }

 private:
  Kind m_kind = Kind::INVALID;
  s64 m_int_constant = -1;
  std::string m_name;
  Form* m_expr = nullptr;
};

DerefToken to_token(const FieldReverseLookupOutput::Token& in);

class DerefElement : public FormElement {
 public:
  DerefElement(Form* base, bool is_addr_of, DerefToken token);
  DerefElement(Form* base, bool is_addr_of, std::vector<DerefToken> tokens);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  void get_modified_regs(RegSet& regs) const override;

  void inline_nested();

  bool is_addr_of() const { return m_is_addr_of; }
  const Form* base() const { return m_base; }
  Form* base() { return m_base; }
  const std::vector<DerefToken>& tokens() const { return m_tokens; }
  std::vector<DerefToken>& tokens() { return m_tokens; }
  void set_base(Form* new_base);
  void set_addr_of(bool is_addr_of) { m_is_addr_of = is_addr_of; }

 private:
  ConstantTokenElement* try_as_art_const(const Env& env, FormPool& pool);
  GenericElement* try_as_curtime(FormPool& pool);

  Form* m_base = nullptr;
  bool m_is_addr_of = false;
  std::vector<DerefToken> m_tokens;
};

class ArrayFieldAccess : public FormElement {
 public:
  ArrayFieldAccess(RegisterAccess source,
                   const std::vector<DerefToken>& deref_tokens,
                   int expected_stride,
                   int constant_offset,
                   bool flipped);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  void get_modified_regs(RegSet& regs) const override;

  void update_with_val(Form* new_val,
                       const Env& env,
                       FormPool& pool,
                       std::vector<FormElement*>* result,
                       bool allow_side_effects);

  bool flipped() const { return m_flipped; }

 private:
  RegisterAccess m_source;
  std::vector<DerefToken> m_deref_tokens;
  int m_expected_stride = -1;
  int m_constant_offset = -1;
  bool m_flipped = false;
};

class StorePlainDeref : public FormElement {
 public:
  StorePlainDeref(Form* dst,
                  SimpleExpression expr,
                  int my_idx,
                  RegisterAccess base_var,
                  std::optional<TypeSpec> dst_cast_type,
                  std::optional<TypeSpec> src_cast_type,
                  int size);

  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
  int size() const { return m_size; }

 private:
  Form* m_dst = nullptr;
  SimpleExpression m_expr;
  int m_my_idx = -1;
  RegisterAccess m_base_var;
  std::optional<TypeSpec> m_dst_cast_type, m_src_cast_type;
  int m_size = -1;
};

class StoreArrayAccess : public FormElement {
 public:
  StoreArrayAccess(ArrayFieldAccess* dst,
                   SimpleExpression expr,
                   int my_idx,
                   RegisterAccess array_src,
                   std::optional<TypeSpec> src_cast_type);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;

 private:
  ArrayFieldAccess* m_dst = nullptr;
  SimpleExpression m_expr;
  int m_my_idx = -1;
  RegisterAccess m_base_var;
  std::optional<TypeSpec> m_src_cast_type;
};

/*!
 * This marks some static data that will be decompiled in a later pass.
 * This is done at the very end so that we can make sure all static references to lambdas work.
 */
class DecompiledDataElement : public FormElement {
 public:
  // DecompiledDataElement(goos::Object description);
  DecompiledDataElement(const DecompilerLabel& label,
                        const std::optional<LabelInfo>& label_info = {});
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  void do_decomp(const Env& env, const LinkedObjectFile* file);
  DecompilerLabel label() const { return m_label; }
  std::optional<LabelInfo> label_info() const { return m_label_info; }

 private:
  bool m_decompiled = false;
  goos::Object m_description;
  DecompilerLabel m_label;
  std::optional<LabelInfo> m_label_info;
};

class LetElement : public FormElement {
 public:
  LetElement(Form* body, bool star = false);
  void add_def(RegisterAccess dst, Form* value);

  void make_let_star();
  void clear_let_star();
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  Form* body() { return m_body; }
  const Form* body() const { return m_body; }
  void set_body(Form* new_body);
  bool allow_in_if() const override { return false; }

  struct Entry {
    RegisterAccess dest;
    Form* src = nullptr;
  };
  std::vector<Entry>& entries() { return m_entries; }
  const std::vector<Entry>& entries() const { return m_entries; }
  void add_entry(const Entry& e);
  bool is_star() const { return m_star; }

 private:
  Form* m_body = nullptr;
  std::vector<Entry> m_entries;
  bool m_star = false;
};

class CounterLoopElement : public FormElement {
 public:
  enum class Kind { DOTIMES, COUNTDOWN, INVALID };
  CounterLoopElement(Kind kind,
                     RegisterAccess var_init,
                     RegisterAccess var_check,
                     RegisterAccess var_inc,
                     Form* check_value,
                     Form* body);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  bool allow_in_if() const override { return false; }
  Kind kind() const { return m_kind; }
  Form* counter_value() const { return m_check_value; }
  Form* body() const { return m_body; }
  RegisterAccess var_init() const { return m_var_init; }
  RegisterAccess var_check() const { return m_var_check; }
  RegisterAccess var_inc() const { return m_var_inc; }

 private:
  RegisterAccess m_var_init, m_var_check, m_var_inc;
  Form* m_check_value = nullptr;
  Form* m_body = nullptr;
  Kind m_kind = Kind::INVALID;
};

class LambdaDefinitionElement : public FormElement {
 public:
  LambdaDefinitionElement(const goos::Object& def);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;

 private:
  goos::Object m_def;
};

class StackStructureDefElement : public FormElement {
 public:
  StackStructureDefElement(const StackStructureEntry& entry);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  const TypeSpec& type() const { return m_entry.ref_type; }
  const StackStructureEntry& entry() const { return m_entry; }

 private:
  StackStructureEntry m_entry;
};

class VectorFloatLoadStoreElement : public FormElement {
 public:
  VectorFloatLoadStoreElement(Register vf_reg, Form* location, bool is_load, int my_idx);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
  void collect_vf_regs(RegSet& regs) const;
  bool is_load() const { return m_is_load; }
  Register vf_reg() const { return m_vf_reg; }
  const std::optional<TypeSpec>& addr_type() const { return m_addr_type; }
  Form* location() const { return m_location; }
  int my_idx() const { return m_my_idx; }

 private:
  Register m_vf_reg;
  Form* m_location = nullptr;
  bool m_is_load = false;
  std::optional<TypeSpec> m_addr_type;
  int m_my_idx = -1;
};

class StackSpillStoreElement : public FormElement {
 public:
  StackSpillStoreElement(SimpleAtom value,
                         int size,
                         int stack_offset,
                         const std::optional<TypeSpec>& cast_type);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;
  const std::optional<TypeSpec>& cast_type() const { return m_cast_type; }

 private:
  SimpleAtom m_value;
  int m_size = -1;
  int m_stack_offset = -1;
  std::optional<TypeSpec> m_cast_type;
};

// the value from a stack load.
class StackSpillValueElement : public FormElement {
 public:
  StackSpillValueElement(int size, int stack_offset, bool is_signed);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;

 private:
  int m_size = -1;
  int m_stack_offset = -1;
  bool m_is_signed = false;
};

class MethodOfTypeElement : public FormElement {
 public:
  MethodOfTypeElement(RegisterAccess type_reg,
                      const TypeSpec& type_at_decompile,
                      const MethodInfo& method_info);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  void get_modified_regs(RegSet& regs) const override;

 private:
  RegisterAccess m_type_reg;
  TypeSpec m_type_at_decompile;
  MethodInfo m_method_info;
};

class LabelElement : public FormElement {
 public:
  LabelElement(int lid);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  void push_to_stack(const Env& env, FormPool& pool, FormStack& stack) override;

 private:
  int m_lid = -1;
};

class LabelDerefElement : public FormElement {
 public:
  LabelDerefElement(int lid, int size, LoadVarOp::Kind load_kind, RegisterAccess var);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;

 private:
  int m_lid = -1;
  int m_size = -1;
  LoadVarOp::Kind m_load_kind = LoadVarOp::Kind::INVALID;
  RegisterAccess m_var;
};

class GetSymbolStringPointer : public FormElement {
 public:
  GetSymbolStringPointer(Form* src);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  Form* src() { return m_src; }

 private:
  Form* m_src = nullptr;
};

class DefstateElement : public FormElement {
 public:
  struct Entry {
    StateHandler kind;
    Form* val = nullptr;
    bool is_behavior = false;
  };
  DefstateElement(const std::string& process_type,
                  const std::string& state_name,
                  const std::vector<Entry>& entries,
                  bool is_virtual,
                  bool is_override);

  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  void get_modified_regs(RegSet& regs) const override;
  std::vector<Entry>& entries() { return m_entries; }

 private:
  std::string m_process_type;
  std::string m_state_name;
  std::vector<Entry> m_entries;
  bool m_is_virtual = false;
  bool m_is_override = false;
};

class DefskelgroupElement : public FormElement {
 public:
  struct StaticInfo {
    std::string name;  // jak 2
    std::string art_group_name;
    math::Vector4f bounds;
    int max_lod;
    float longest_edge;
    s8 tex_level;
    s8 version;
    s8 shadow;
    s8 sort;
    s8 origin_joint_index;
    s8 shadow_joint_index;
    s8 light_index;
  };
  struct Entry {
    Form* mgeo = nullptr;
    Form* lod_dist = nullptr;
  };
  struct Info {
    Form* jgeo;
    Form* janim;
    std::vector<Entry> lods;
  };
  DefskelgroupElement(const std::string& name, const Info& info, const StaticInfo& data);

  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  void get_modified_regs(RegSet& regs) const override;

 private:
  std::string m_name;
  StaticInfo m_static_info;
  Info m_info;
};

class DefpartgroupElement : public FormElement {
 public:
  struct StaticInfo {
    u16 duration;
    u16 linger;
    u16 flags;
    std::string name;
    math::Vector4f bounds;
    // added in jak 2
    math::Vector3f rot;
    math::Vector3f scale;

    struct PartGroupItem {
      u32 part_id;
      float fade;
      float falloff;
      u16 flags;
      u16 period;
      u16 length;
      u16 offset;
      u32 hour_mask;
      u32 binding;
    };
    std::vector<PartGroupItem> elts;
  };
  DefpartgroupElement(const StaticInfo& data, int group_id);

  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  void get_modified_regs(RegSet& regs) const override;

  const std::string& name() const { return m_static_info.name; }

 private:
  StaticInfo m_static_info;
  int m_group_id;
};

class DefpartElement : public FormElement {
 public:
  struct StaticInfo {
    struct PartField {
      u16 field_id;
      u16 flags;
      std::vector<LinkedWord> data;
      goos::Object sound_spec;  // any static object actually
      goos::Object userdata;    // backup

      bool is_sp_end(GameVersion version) const {
        switch (version) {
          case GameVersion::Jak1:
            return field_id == 67;
          case GameVersion::Jak2:
            return field_id == 72;
          default:
            ASSERT_MSG(false, fmt::format("unknown version {} for is_sp_end"));
            return false;
        }
      }
    };
    std::vector<PartField> fields;
  };
  DefpartElement(const StaticInfo& data, int id);

  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  void get_modified_regs(RegSet& regs) const override;

 private:
  StaticInfo m_static_info;
  int m_id;
};

// for that macro
class WithDmaBufferAddBucketElement : public FormElement {
 public:
  WithDmaBufferAddBucketElement(RegisterAccess dma_buf,
                                Form* dma_buf_val,
                                Form* bucket,
                                Form* body);

  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  void get_modified_regs(RegSet& regs) const override;
  bool allow_in_if() const override { return false; }

 private:
  RegisterAccess m_dma_buf;
  Form* m_dma_buf_val;
  Form* m_bucket;
  Form* m_body;
};

class ResLumpMacroElement : public FormElement {
 public:
  enum class Kind { DATA, STRUCT, VALUE, INVALID };
  ResLumpMacroElement(Kind kind,
                      Form* lump_object,
                      Form* property_name,
                      Form* default_arg,
                      Form* tag_ptr,
                      Form* time,
                      const TypeSpec& result_type);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void update_from_stack(const Env& env,
                         FormPool& pool,
                         FormStack& stack,
                         std::vector<FormElement*>* result,
                         bool allow_side_effects) override;
  void get_modified_regs(RegSet& regs) const override;
  void apply_cast(const TypeSpec& new_type) { m_result_type = new_type; }

 private:
  Kind m_kind = Kind::INVALID;
  Form* m_lump_object = nullptr;
  Form* m_property_name = nullptr;
  Form* m_default_arg = nullptr;  // may be null
  Form* m_tag_ptr = nullptr;      // may be null
  Form* m_time = nullptr;         // may be null
  TypeSpec m_result_type;
};

/*!
 * A Form is a wrapper around one or more FormElements.
 * This is done for two reasons:
 *  - Easier to "inline" begins, prevents stupid nesting of begins.
 *  - Easier to manage ownership.
 */
class Form {
 public:
  Form() = default;
  Form(FormElement* parent, FormElement* single_child)
      : parent_element(parent), m_elements({single_child}) {
    single_child->parent_form = this;
  }

  Form(FormElement* parent, const std::vector<FormElement*>& sequence)
      : parent_element(parent), m_elements(sequence) {
    for (auto& x : sequence) {
      x->parent_form = this;
    }
  }

  FormElement* try_as_single_element() const {
    if (is_single_element()) {
      return m_elements.front();
    }
    return nullptr;
  }

  FormElement* try_as_single_active_element() const {
    int active_count = 0;
    FormElement* result = nullptr;
    for (auto& elt : m_elements) {
      if (elt->active()) {
        active_count++;
        result = elt;
      }
    }
    if (active_count == 1) {
      return result;
    } else {
      return nullptr;
    }
  }

  template <typename T>
  T* try_as_element() const {
    return dynamic_cast<T*>(try_as_single_element());
  }

  bool is_single_element() const { return m_elements.size() == 1; }
  bool is_single_active_element() const {
    int active_count = 0;
    for (auto& elt : m_elements) {
      if (elt->active()) {
        active_count++;
      }
    }
    return active_count == 1;
  }

  bool is_reasonable_for_if() const {
    int active_count = 0;
    for (auto& elt : m_elements) {
      if (elt->active()) {
        if (!elt->allow_in_if()) {
          return false;
        }
        active_count++;
      }
    }
    return active_count == 1;
  }

  FormElement* operator[](int idx) { return m_elements.at(idx); }
  FormElement*& at(int idx) { return m_elements.at(idx); }
  const FormElement* operator[](int idx) const { return m_elements.at(idx); }
  int size() const { return int(m_elements.size()); }
  FormElement* back() const {
    ASSERT(!m_elements.empty());
    return m_elements.back();
  }

  FormElement** back_ref() {
    ASSERT(!m_elements.empty());
    return &m_elements.back();
  }

  void pop_back() {
    ASSERT(!m_elements.empty());
    m_elements.pop_back();
  }

  const std::vector<FormElement*>& elts() const { return m_elements; }
  std::vector<FormElement*>& elts() { return m_elements; }
  void claim_all_children() {
    for (auto elt : elts()) {
      elt->parent_form = this;
    }
  }

  void push_back(FormElement* elt) {
    elt->parent_form = this;
    m_elements.push_back(elt);
  }

  void clear() { m_elements.clear(); }

  goos::Object to_form(const Env& env) const;
  goos::Object to_form_as_condition(const Env& env) const;
  std::string to_string(const Env& env) const;
  void inline_forms(std::vector<goos::Object>& forms, const Env& env) const;
  void apply(const std::function<void(FormElement*)>& f);
  void apply_form(const std::function<void(Form*)>& f);
  void collect_vars(RegAccessSet& vars, bool recursive) const;

  void update_children_from_stack(const Env& env,
                                  FormPool& pool,
                                  FormStack& stack,
                                  bool allow_side_effects);
  bool has_side_effects();
  void get_modified_regs(RegSet& regs) const;

  bool is_popped() const { return m_elements.at(0)->is_popped(); }

  void mark_popped() {
    for (auto x : m_elements) {
      x->mark_popped();
    }
  }

  FormElement* parent_element = nullptr;

 private:
  std::vector<FormElement*> m_elements;
};

class CfgVtx;

/*!
 * A FormPool is used to allocate forms and form elements.
 * It will clean up everything when it is destroyed.
 * As a result, you don't need to worry about deleting / referencing counting when manipulating
 * a Form graph.
 */
class FormPool {
 public:
  template <typename T, class... Args>
  T* alloc_element(Args&&... args) {
    auto elt = new T(std::forward<Args>(args)...);
    m_elements.emplace_back(elt);
    return elt;
  }

  template <typename T, class... Args>
  Form* alloc_single_element_form(FormElement* parent, Args&&... args) {
    auto elt = new T(std::forward<Args>(args)...);
    m_elements.emplace_back(elt);
    auto form = alloc_single_form(parent, elt);
    return form;
  }

  template <typename T, class... Args>
  Form* form(Args&&... args) {
    auto elt = new T(std::forward<Args>(args)...);
    m_elements.emplace_back(elt);
    auto form = alloc_single_form(nullptr, elt);
    return form;
  }

  Form* alloc_single_form(FormElement* parent, FormElement* elt) {
    auto form = new Form(parent, elt);
    m_forms.push_back(form);
    return form;
  }

  Form* alloc_sequence_form(FormElement* parent, const std::vector<FormElement*> sequence) {
    auto form = new Form(parent, sequence);
    m_forms.push_back(form);
    return form;
  }

  Form* acquire(std::unique_ptr<Form> form_ptr) {
    Form* form = form_ptr.release();
    m_forms.push_back(form);
    return form;
  }

  Form* alloc_empty_form() {
    Form* form = new Form;
    m_forms.push_back(form);
    return form;
  }

  Form* lookup_cached_conversion(const CfgVtx* vtx) const {
    auto it = m_vtx_to_form_cache.find(vtx);
    if (it == m_vtx_to_form_cache.end()) {
      return nullptr;
    }
    return it->second;
  }

  void cache_conversion(const CfgVtx* vtx, Form* form) {
    ASSERT(m_vtx_to_form_cache.find(vtx) == m_vtx_to_form_cache.end());
    m_vtx_to_form_cache[vtx] = form;
  }

  ~FormPool();

 private:
  std::vector<Form*> m_forms;
  std::vector<FormElement*> m_elements;
  std::unordered_map<const CfgVtx*, Form*> m_vtx_to_form_cache;
};

std::optional<SimpleAtom> form_element_as_atom(const FormElement* f);
std::optional<SimpleAtom> form_as_atom(const Form* f);
FormElement* make_cast_using_existing(FormElement* elt, const TypeSpec& type, FormPool& pool);
GenericElement* alloc_generic_token_op(const std::string& name,
                                       const std::vector<Form*>& args,
                                       FormPool& pool);
Form* alloc_var_form(const RegisterAccess& var, FormPool& pool);
Form* try_cast_simplify(Form* in,
                        const TypeSpec& new_type,
                        FormPool& pool,
                        const Env& env,
                        bool tc_pass = false);
}  // namespace decompiler
