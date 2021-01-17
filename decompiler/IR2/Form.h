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
  virtual bool is_sequence_point() const { return true; }

 protected:
  friend class Form;
};

/*!
 * A SimpleExpressionElement is a form which has the value of a SimpleExpression.
 * Like a SimpleExpression, it has no side effects.
 */
class SimpleExpressionElement : public FormElement {
 public:
  explicit SimpleExpressionElement(const SimpleExpression& expr);

  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  bool is_sequence_point() const override;

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

 private:
  Form* m_addr = nullptr;
  int m_size = -1;
  LoadVarOp::Kind m_kind;
};

class SimpleAtomElement : public FormElement {
 public:
  explicit SimpleAtomElement(const SimpleAtom& var);
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;

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
  bool is_sequence_point() const override;

 private:
  Variable m_dst;
  Form* m_src = nullptr;
  bool m_is_sequence_point = true;
};

class AtomicOpElement : public FormElement {
 public:
  explicit AtomicOpElement(const AtomicOp* op);
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;

 private:
  const AtomicOp* m_op;
};

class ConditionElement : public FormElement {
 public:
  ConditionElement(IR2_Condition::Kind kind, Form* src0, Form* src1);
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void invert();

 private:
  IR2_Condition::Kind m_kind;
  Form* m_src[2] = {nullptr, nullptr};
};

class FunctionCallElement : public FormElement {
 public:
  explicit FunctionCallElement(const CallOp* op);
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;

 private:
  const CallOp* m_op;
};

class BranchElement : public FormElement {
 public:
  explicit BranchElement(const BranchOp* op);
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  const BranchOp* op() const { return m_op; }

 private:
  const BranchOp* m_op;
};

class ReturnElement : public FormElement {
 public:
  Form* return_code = nullptr;
  Form* dead_code = nullptr;
  ReturnElement(Form* _return_code, Form* _dead_code)
      : return_code(_return_code), dead_code(_dead_code) {}
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
};

class BreakElement : public FormElement {
 public:
  Form* return_code = nullptr;
  Form* dead_code = nullptr;
  BreakElement(Form* _return_code, Form* _dead_code)
      : return_code(_return_code), dead_code(_dead_code) {}
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
};

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
};

class EmptyElement : public FormElement {
 public:
  EmptyElement() = default;
  goos::Object to_form(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
};

/*!
 * A Form is a wrapper around one or more FormElements.
 * This is done for two reasons:
 *  - Easier to "inline" begins, prevents stupid nesting of begins.
 *  - Easier to manage ownership.
 */
class Form {
 public:
  Form(FormElement* parent, FormElement* single_child)
      : parent_element(parent), m_elements({single_child}) {
    single_child->parent_form = this;
  }

  Form(FormElement* parent, const std::vector<FormElement*> sequence)
      : parent_element(parent), m_elements(sequence) {
    for (auto& x : sequence) {
      x->parent_form = this;
    }
  }

  bool is_single_element() const { return m_elements.size() == 1; }
  FormElement* operator[](int idx) { return m_elements.at(idx); }
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

  goos::Object to_form(const Env& env) const;
  void inline_forms(std::vector<goos::Object>& forms, const Env& env) const;
  void apply(const std::function<void(FormElement*)>& f) const;
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

  ~FormPool();

 private:
  std::vector<Form*> m_forms;
  std::vector<FormElement*> m_elements;
};
}  // namespace decompiler
