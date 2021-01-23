#pragma once

#include <vector>
#include <unordered_set>
#include <memory>
#include <functional>
#include "decompiler/Disasm/Register.h"
#include "decompiler/IR2/AtomicOp.h"
#include "common/goos/Object.h"

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

  virtual goos::Object to_form(const Env& env) const = 0;
  virtual ~FormElement() = default;
  virtual void apply(const std::function<void(FormElement*)>& f) = 0;
  virtual void apply_form(const std::function<void(Form*)>& f) = 0;
  virtual bool is_sequence_point() const { return true; }
  virtual void collect_vars(VariableSet& vars) const = 0;
  std::string to_string(const Env& env) const;

  // push the result of this operation to the operation stack
  virtual void push_to_stack(const Env& env, FormStack& stack);

 protected:
  friend class Form;
};

/*!
 * A SimpleExpressionElement is a form which has the value of a SimpleExpression.
 * Like a SimpleExpression, it has no side effects.
 */
class SimpleExpressionElement : public FormElement {
 public:
  explicit SimpleExpressionElement(SimpleExpression expr);

  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  bool is_sequence_point() const override;
  void collect_vars(VariableSet& vars) const override;

  const SimpleExpression& expr() const { return m_expr; }

 private:
  SimpleExpression m_expr;
};

/*!
 * Represents storing a value into memory.
 * Because a value can be propagated "into" the source value, this will have to be special cased
 * in expression propagation.
 */
class StoreElement : public FormElement {
 public:
  explicit StoreElement(const StoreOp* op);

  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(VariableSet& vars) const override;

 private:
  // todo - we may eventually want to use a different representation for more
  // complicated store paths.
  const StoreOp* m_op;
};

/*!
 * Representing a value loaded from memory.
 * Unclear if this should have some common base with store?
 */
class LoadSourceElement : public FormElement {
 public:
  LoadSourceElement(Form* addr, int size, LoadVarOp::Kind kind);
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(VariableSet& vars) const override;
  int size() const { return m_size; }
  LoadVarOp::Kind kind() const { return m_kind; }
  const Form* location() const { return m_addr; }

 private:
  Form* m_addr = nullptr;
  int m_size = -1;
  LoadVarOp::Kind m_kind;
};

/*!
 * Representing an indivisible thing, like an integer constant variable, etc.
 * Just a wrapper around SimpleAtom.
 */
class SimpleAtomElement : public FormElement {
 public:
  explicit SimpleAtomElement(const SimpleAtom& var);
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(VariableSet& vars) const override;

 private:
  SimpleAtom m_atom;
};

/*!
 * Set a variable to a Form.  This is the set! form to be used for expression building.
 */
class SetVarElement : public FormElement {
 public:
  SetVarElement(const Variable& var, Form* value, bool is_sequence_point);
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  bool is_sequence_point() const override;
  void collect_vars(VariableSet& vars) const override;

  const Variable& dst() const { return m_dst; }
  const Form* src() const { return m_src; }

 private:
  Variable m_dst;
  Form* m_src = nullptr;
  bool m_is_sequence_point = true;
};

/*!
 * A wrapper around a single AtomicOp.
 * The "important" special AtomicOps have their own Form type, like FuncitonCallElement.
 */
class AtomicOpElement : public FormElement {
 public:
  explicit AtomicOpElement(const AtomicOp* op);
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(VariableSet& vars) const override;

 private:
  const AtomicOp* m_op;
};

/*!
 * A "condition" like (< a b). This can be used as a boolean value directly: (set! a (< b c))
 * or it can be used as a branch condition: (if (< a b)).
 *
 * In the first case, it can be either a conditional move or actually branching. GOAL seems to use
 * the branching when sometimes it could have used the conditional move, and for now, we don't
 * care about the difference.
 */
class ConditionElement : public FormElement {
 public:
  ConditionElement(IR2_Condition::Kind kind, Form* src0, Form* src1);
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(VariableSet& vars) const override;
  void invert();

 private:
  IR2_Condition::Kind m_kind;
  Form* m_src[2] = {nullptr, nullptr};
};

/*!
 * Wrapper around an AtomicOp call.
 */
class FunctionCallElement : public FormElement {
 public:
  explicit FunctionCallElement(const CallOp* op);
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(VariableSet& vars) const override;

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
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(VariableSet& vars) const override;
  const BranchOp* op() const { return m_op; }

 private:
  const BranchOp* m_op;
};

/*!
 * Represents a (return-from #f x) form, which immediately returns from the function.
 * This always has some "dead code" after it that can't be reached, which is the "dead_code".
 */
class ReturnElement : public FormElement {
 public:
  Form* return_code = nullptr;
  Form* dead_code = nullptr;
  ReturnElement(Form* _return_code, Form* _dead_code)
      : return_code(_return_code), dead_code(_dead_code) {}
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(VariableSet& vars) const override;
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
  BreakElement(Form* _return_code, Form* _dead_code)
      : return_code(_return_code), dead_code(_dead_code) {}
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(VariableSet& vars) const override;
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
  CondWithElseElement(std::vector<Entry> _entries, Form* _else_ir)
      : entries(std::move(_entries)), else_ir(_else_ir) {}
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(VariableSet& vars) const override;
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
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(VariableSet& vars) const override;
};

/*!
 * Represents a GOAL while loop and more complicated loops which have the "while" format of checking
 * the condition before the first loop. This will not include infinite while loops.
 * Unlike CondWithElseElement, this will correctly identify the start and end of the condition.
 */
class WhileElement : public FormElement {
 public:
  WhileElement(Form* _condition, Form* _body) : condition(_condition), body(_body) {}
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(VariableSet& vars) const override;
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
  UntilElement(Form* _condition, Form* _body) : condition(_condition), body(_body) {}
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(VariableSet& vars) const override;
  Form* condition = nullptr;
  Form* body = nullptr;
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
    // in the case where there's no else, each delay slot will write #f to the "output" register.
    // this can be with an or <output>, s7, r0
    //    Form* output = nullptr; // todo, what? add to collect vars if we need it?
    bool is_output_trick = false;
    bool cleaned = false;
  };

  enum Kind { UNKNOWN, AND, OR } kind = UNKNOWN;

  Variable final_result;
  std::vector<Entry> entries;
  std::optional<bool> used_as_value = std::nullopt;

  explicit ShortCircuitElement(std::vector<Entry> _entries) : entries(std::move(_entries)) {}
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(VariableSet& vars) const override;
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
    std::optional<Variable> false_destination;
    FormElement* original_condition_branch = nullptr;
    bool cleaned = false;
  };
  Register final_destination;
  bool used_as_value = false;
  std::vector<Entry> entries;
  explicit CondNoElseElement(std::vector<Entry> _entries) : entries(std::move(_entries)) {}
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(VariableSet& vars) const override;
};

/*!
 * Represents a (abs x) expression.
 */
class AbsElement : public FormElement {
 public:
  explicit AbsElement(Form* _source);
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(VariableSet& vars) const override;
  Form* source = nullptr;
};

/*!
 * Represents an (ash x y) expression. There is also an "unsigned" version of this using logical
 * shifts. This only recognizes the fancy version where the shift amount isn't known at compile time
 * and the compiler emits code that branches depending on the sign of the shift amount.
 */
class AshElement : public FormElement {
 public:
  Form* shift_amount = nullptr;
  Form* value = nullptr;
  std::optional<Variable> clobber;
  bool is_signed = true;
  AshElement(Form* _shift_amount, Form* _value, std::optional<Variable> _clobber, bool _is_signed);
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(VariableSet& vars) const override;
};

/*!
 * Represents a form which gets the runtime type of a boxed object. This is for the most general
 * "object" case where we check for pair, binteger, or basic and there's actually branching.
 */
class TypeOfElement : public FormElement {
 public:
  Form* value;
  std::optional<Variable> clobber;
  TypeOfElement(Form* _value, std::optional<Variable> _clobber);
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(VariableSet& vars) const override;
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
  Variable dest;
  Form* source = nullptr;
  bool on_zero = false;
  ConditionalMoveFalseElement(Variable _dest, Form* _source, bool _on_zero);
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(VariableSet& vars) const override;
};

///*!
// * A GenericOperator is the head of a GenericElement.
// * It is used for the final output.
// */
// class GenericOperator {
// public:
//  enum class Kind {
//    FIXED_FUNCTION_CALL,
//    VAR_FUNCTION_CALL,
//    FIXED_OPERATOR
//  };
//
// private:
//  // if we're a VAR_FUNCTION_CALL, this should contain the expression to get the function
//  Form* m_function_val;
//
//  //std::string
//
//};
//
// class GenericElement : public FormElement {
// public:
//  goos::Object to_form(const Env& env) const override;
//  void apply(const std::function<void(FormElement*)>& f) override;
//  void apply_form(const std::function<void(Form*)>& f) override;
//  void collect_vars(VariableSet& vars) const override;
// private:
//  GenericOperator m_head;
//  std::vector<Form*> m_elts;
//};

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
  bool is_single_element() const { return m_elements.size() == 1; }
  FormElement* operator[](int idx) { return m_elements.at(idx); }
  FormElement* at(int idx) { return m_elements.at(idx); }
  const FormElement* operator[](int idx) const { return m_elements.at(idx); }
  int size() const { return int(m_elements.size()); }
  FormElement* back() const {
    assert(!m_elements.empty());
    return m_elements.back();
  }

  FormElement** back_ref() {
    assert(!m_elements.empty());
    return &m_elements.back();
  }

  void pop_back() {
    assert(!m_elements.empty());
    m_elements.pop_back();
  }

  const std::vector<FormElement*>& elts() const { return m_elements; }
  std::vector<FormElement*>& elts() { return m_elements; }

  void push_back(FormElement* elt) {
    elt->parent_form = this;
    m_elements.push_back(elt);
  }

  void clear() { m_elements.clear(); }

  goos::Object to_form(const Env& env) const;
  std::string to_string(const Env& env) const;
  void inline_forms(std::vector<goos::Object>& forms, const Env& env) const;
  void apply(const std::function<void(FormElement*)>& f);
  void apply_form(const std::function<void(Form*)>& f);
  void collect_vars(VariableSet& vars) const;

  FormElement* parent_element = nullptr;

 private:
  std::vector<FormElement*> m_elements;
};

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

  ~FormPool();

 private:
  std::vector<Form*> m_forms;
  std::vector<FormElement*> m_elements;
};
}  // namespace decompiler
