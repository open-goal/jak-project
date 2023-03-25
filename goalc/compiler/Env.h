#pragma once

/*!
 * @file Env.h
 * The Env tree. The stores all of the nested scopes/contexts during compilation and also
 * manages the memory for stuff generated during compiling.
 */

#include <memory>
#include <string>
#include <vector>

#include "Label.h"
#include "StaticObject.h"
#include "Val.h"

#include "common/goos/Object.h"
#include "common/type_system/TypeSpec.h"

#include "goalc/regalloc/allocator_interface.h"

class FileEnv;
class BlockEnv;
class FunctionEnv;
class SymbolMacroEnv;
class MacroExpandEnv;
class IR;

enum class EnvKind { FILE_ENV, FUNCTION_ENV, SYMBOL_MACRO_ENV, MACRO_EXPAND_ENV, OTHER_ENV };

/*!
 * Parent class for Env's
 */
class Env {
 public:
  explicit Env(EnvKind kind, Env* parent);
  virtual std::string print() = 0;
  void emit(const goos::Object& form, std::unique_ptr<IR> ir);
  virtual RegVal* make_ireg(const TypeSpec& ts, RegClass reg_class);
  virtual void constrain_reg(IRegConstraint constraint);  // todo, remove!
  virtual RegVal* lexical_lookup(goos::Object sym);
  virtual BlockEnv* find_block(const std::string& name);
  virtual std::unordered_map<std::string, Label>& get_label_map();
  RegVal* make_gpr(const TypeSpec& ts);
  RegVal* make_fpr(const TypeSpec& ts);
  RegVal* make_vfr(const TypeSpec& ts);
  virtual ~Env() = default;
  Env* parent() { return m_parent; }

  template <typename IR_Type, typename... Args>
  void emit_ir(const goos::Object& form, Args&&... args) {
    emit(form, std::make_unique<IR_Type>(std::forward<Args>(args)...));
  }

  FileEnv* file_env() { return m_lowest_envs.file_env; }
  FunctionEnv* function_env() { return m_lowest_envs.function_env; }
  SymbolMacroEnv* symbol_macro_env() { return m_lowest_envs.symbol_macro_env; }
  MacroExpandEnv* macro_expand_env() { return m_lowest_envs.macro_expand_env; }

 protected:
  EnvKind m_kind;
  Env* m_parent = nullptr;

  // cache of the lowest env of the given type, possibly including ourselves
  struct {
    FileEnv* file_env = nullptr;
    FunctionEnv* function_env = nullptr;
    SymbolMacroEnv* symbol_macro_env = nullptr;
    MacroExpandEnv* macro_expand_env = nullptr;
  } m_lowest_envs;
};

/*!
 * The top-level Env. Holds FileEnvs for all files.
 */
class GlobalEnv : public Env {
 public:
  GlobalEnv();
  std::string print() override;
  RegVal* make_ireg(const TypeSpec& ts, RegClass reg_class) override;
  void constrain_reg(IRegConstraint constraint) override;
  RegVal* lexical_lookup(goos::Object sym) override;
  BlockEnv* find_block(const std::string& name) override;
  ~GlobalEnv() = default;

  FileEnv* add_file(std::string name);
  // TODO - consider refactoring to use a Trie
  std::vector<std::string> list_files_with_prefix(const std::string& prefix);

 private:
  std::vector<std::unique_ptr<FileEnv>> m_files;
};

/*!
 * An Env for an entire file (or input to the REPL)
 */
class FileEnv : public Env {
 public:
  FileEnv(Env* parent, std::string name);
  std::string print() override;
  void add_function(std::unique_ptr<FunctionEnv> fe);
  void add_top_level_function(std::unique_ptr<FunctionEnv> fe);
  void add_static(std::unique_ptr<StaticObject> s);
  void debug_print_tl();
  const std::vector<std::unique_ptr<FunctionEnv>>& functions() { return m_functions; }
  const std::vector<std::unique_ptr<StaticObject>>& statics() { return m_statics; }
  std::string get_anon_function_name() {
    return "anon-function-" + std::to_string(m_anon_func_counter++);
  }
  const FunctionEnv& top_level_function() {
    ASSERT(m_top_level_func);
    return *m_top_level_func;
  }
  const std::string& name() { return m_name; }

  bool is_empty();
  ~FileEnv() = default;

  template <typename T, class... Args>
  T* alloc_val(Args&&... args) {
    std::unique_ptr<T> new_obj = std::make_unique<T>(std::forward<Args>(args)...);
    m_vals.push_back(std::move(new_obj));
    return (T*)m_vals.back().get();
  }

  int default_segment() const { return m_default_segment; }
  void set_nondebug_file() { m_default_segment = MAIN_SEGMENT; }
  void set_debug_file() { m_default_segment = DEBUG_SEGMENT; }
  bool is_debug_file() const { return default_segment() == DEBUG_SEGMENT; }

  void cleanup_after_codegen();

 protected:
  std::string m_name;
  std::vector<std::unique_ptr<FunctionEnv>> m_functions;
  std::vector<std::unique_ptr<StaticObject>> m_statics;
  int m_anon_func_counter = 0;
  std::vector<std::unique_ptr<Val>> m_vals;
  int m_default_segment = MAIN_SEGMENT;

  // statics
  FunctionEnv* m_top_level_func = nullptr;
};

/*!
 * An Env which manages the scope for (declare ...) statements.
 */
class DeclareEnv : public Env {
 public:
  explicit DeclareEnv(EnvKind kind, Env* parent) : Env(kind, parent) {}
  virtual std::string print() = 0;
  ~DeclareEnv() = default;

  struct Settings {
    bool is_set = false;             // has the user set these with a (declare)?
    bool inline_by_default = false;  // if a function, inline when possible?
    bool save_code = true;           // if a function, should we save the code?
    bool allow_inline = false;       // should we allow the user to use this an inline function
    bool print_asm = false;          // should we print out the asm for this function?
  } settings;
};

class IR_GotoLabel;

struct UnresolvedGoto {
  IR_GotoLabel* ir = nullptr;
  std::string label;
};

class IR_ConditionalBranch;

struct UnresolvedConditionalGoto {
  IR_ConditionalBranch* ir = nullptr;
  std::string label;
};

class FunctionEnv : public DeclareEnv {
 public:
  FunctionEnv(Env* parent, std::string name, const goos::Reader* reader);
  std::string print() override;
  std::unordered_map<std::string, Label>& get_label_map() override;
  void set_segment(int seg) { segment = seg; }
  void emit(const goos::Object& form, std::unique_ptr<IR> ir, Env* lowest_env);
  void finish();
  RegVal* make_ireg(const TypeSpec& ts, RegClass reg_class) override;
  const std::vector<std::unique_ptr<IR>>& code() const { return m_code; }
  const std::vector<goos::Object>& code_source() const { return m_code_debug_source; }
  int max_vars() const { return m_iregs.size(); }
  const std::vector<IRegConstraint>& constraints() { return m_constraints; }
  void constrain(const IRegConstraint& c) { m_constraints.push_back(c); }
  void set_allocations(AllocationResult&& result) { m_regalloc_result = std::move(result); }
  RegVal* lexical_lookup(goos::Object sym) override;
  const AllocationResult& alloc_result() { return m_regalloc_result; }
  bool needs_aligned_stack() const { return m_aligned_stack_required; }
  void require_aligned_stack() { m_aligned_stack_required = true; }
  Label* alloc_unnamed_label() {
    m_unnamed_labels.emplace_back(std::make_unique<Label>());
    return m_unnamed_labels.back().get();
  }
  const std::string& name() const { return m_name; }

  struct StackSpace {
    int start_slot;
    int slot_count;
  };
  StackSpace allocate_aligned_stack_space(int size_bytes, int align_bytes);
  StackVarAddrVal* allocate_aligned_stack_variable(const TypeSpec& ts,
                                                   int size_bytes,
                                                   int align_bytes);
  StackVarAddrVal* allocate_stack_singleton(const TypeSpec& ts, int size_bytes, int align_bytes);
  int stack_slots_used_for_stack_vars() const { return m_stack_var_slots_used; }

  int segment_for_static_data() {
    if (segment == TOP_LEVEL_SEGMENT) {
      return file_env()->default_segment();
    } else {
      return segment;
    }
  }

  int idx_in_file = -1;

  template <typename T, class... Args>
  T* alloc_val(Args&&... args) {
    std::unique_ptr<T> new_obj = std::make_unique<T>(std::forward<Args>(args)...);
    m_vals.push_back(std::move(new_obj));
    return (T*)m_vals.back().get();
  }

  template <typename T, class... Args>
  T* alloc_env(Args&&... args) {
    std::unique_ptr<T> new_obj = std::make_unique<T>(std::forward<Args>(args)...);
    m_envs.push_back(std::move(new_obj));
    return (T*)m_envs.back().get();
  }

  const std::vector<std::unique_ptr<RegVal>>& reg_vals() const { return m_iregs; }

  RegVal* push_reg_val(std::unique_ptr<RegVal> in);

  int segment = -1;
  std::string method_of_type_name = "#f";
  bool is_asm_func = false;
  bool asm_func_saved_regs = false;
  TypeSpec asm_func_return_type;
  std::vector<UnresolvedGoto> unresolved_gotos;
  std::vector<UnresolvedConditionalGoto> unresolved_cond_gotos;
  std::unordered_map<std::string, RegVal*> params;

 protected:
  void resolve_gotos();
  std::string m_name;
  std::vector<std::unique_ptr<IR>> m_code;
  std::vector<goos::Object> m_code_debug_source;

  std::vector<std::unique_ptr<RegVal>> m_iregs;
  std::vector<std::unique_ptr<Val>> m_vals;
  std::vector<std::unique_ptr<Env>> m_envs;
  std::vector<IRegConstraint> m_constraints;

  AllocationResult m_regalloc_result;

  bool m_aligned_stack_required = false;
  int m_stack_var_slots_used = 0;
  std::unordered_map<std::string, Label> m_labels;
  std::vector<std::unique_ptr<Label>> m_unnamed_labels;
  std::unordered_map<std::string, StackSpace> m_stack_singleton_slots;

  const goos::Reader* m_reader = nullptr;
};

class BlockEnv : public Env {
 public:
  BlockEnv(Env* parent, std::string name);
  std::string print() override;
  BlockEnv* find_block(const std::string& name) override;

  std::string name;
  Label end_label = nullptr;
  RegVal* return_value = nullptr;
  std::vector<TypeSpec> return_types;
};

class LexicalEnv : public DeclareEnv {
 public:
  explicit LexicalEnv(Env* parent) : DeclareEnv(EnvKind::OTHER_ENV, parent) {}
  RegVal* lexical_lookup(goos::Object sym) override;
  std::string print() override;
  std::unordered_map<std::string, RegVal*> vars;
};

class LabelEnv : public Env {
 public:
  explicit LabelEnv(Env* parent) : Env(EnvKind::OTHER_ENV, parent) {}
  std::string print() override { return "labelenv"; }
  std::unordered_map<std::string, Label>& get_label_map() override;
  BlockEnv* find_block(const std::string& name) override;

 protected:
  std::unordered_map<std::string, Label> m_labels;
};

class SymbolMacroEnv : public Env {
 public:
  explicit SymbolMacroEnv(Env* parent) : Env(EnvKind::SYMBOL_MACRO_ENV, parent) {}
  // key is goos symbols.
  std::unordered_map<goos::HeapObject*, goos::Object> macros;
  std::string print() override { return "symbol-macro-env"; }
};

class MacroExpandEnv : public Env {
 public:
  MacroExpandEnv(Env* parent,
                 const goos::HeapObject* macro_name,
                 const goos::Object& macro_body,
                 const goos::Object& macro_use)
      : Env(EnvKind::MACRO_EXPAND_ENV, parent),
        m_macro_name(macro_name),
        m_macro_body(macro_body),
        m_macro_use_location(macro_use) {
    MacroExpandEnv* parent_macro = nullptr;
    if (parent) {
      parent_macro = parent->macro_expand_env();
    }
    if (parent_macro) {
      m_root_form = parent_macro->m_root_form;
    } else {
      m_root_form = m_macro_use_location;
    }
  }

  std::string print() override { return "macro-env"; }

  const goos::HeapObject* name() const { return m_macro_name; }
  const goos::Object& macro_body() const { return m_macro_body; }
  const goos::Object& macro_use_location() const { return m_macro_use_location; }
  const goos::Object& root_form() const { return m_root_form; }

 private:
  const goos::HeapObject* m_macro_name = nullptr;
  goos::Object m_macro_body;
  goos::Object m_macro_use_location;
  goos::Object m_root_form;
};

template <typename T>
T* get_parent_env_of_type_slow(Env* in) {
  for (;;) {
    auto attempt = dynamic_cast<T*>(in);
    if (attempt)
      return attempt;
    if (dynamic_cast<GlobalEnv*>(in)) {
      return nullptr;
    }
    in = in->parent();
  }
}

inline Env::Env(EnvKind kind, Env* parent) : m_kind(kind), m_parent(parent) {
  if (m_kind == EnvKind::FILE_ENV) {
    m_lowest_envs.file_env = static_cast<FileEnv*>(this);
  } else {
    m_lowest_envs.file_env = m_parent ? m_parent->m_lowest_envs.file_env : nullptr;
  }

  if (m_kind == EnvKind::FUNCTION_ENV) {
    m_lowest_envs.function_env = static_cast<FunctionEnv*>(this);
  } else {
    m_lowest_envs.function_env = m_parent ? m_parent->m_lowest_envs.function_env : nullptr;
  }

  if (m_kind == EnvKind::SYMBOL_MACRO_ENV) {
    m_lowest_envs.symbol_macro_env = static_cast<SymbolMacroEnv*>(this);
  } else {
    m_lowest_envs.symbol_macro_env = m_parent ? m_parent->m_lowest_envs.symbol_macro_env : nullptr;
  }

  if (m_kind == EnvKind::MACRO_EXPAND_ENV) {
    m_lowest_envs.macro_expand_env = static_cast<MacroExpandEnv*>(this);
  } else {
    m_lowest_envs.macro_expand_env = m_parent ? m_parent->m_lowest_envs.macro_expand_env : nullptr;
  }
}
