#include <stdexcept>
#include "third-party/fmt/core.h"
#include "Env.h"
#include "IR.h"
#include "common/goos/Reader.h"

///////////////////
// Env
///////////////////

/*!
 * Allocate an IRegister with the given type.
 */
RegVal* Env::make_ireg(const TypeSpec& ts, RegClass reg_class) {
  return m_parent->make_ireg(ts, reg_class);
}

/*!
 * Apply a register constraint to the current function.
 */
void Env::constrain_reg(IRegConstraint constraint) {
  m_parent->constrain_reg(std::move(constraint));
}

/*!
 * Lookup the given symbol object as a lexical variable.
 */
RegVal* Env::lexical_lookup(goos::Object sym) {
  return m_parent->lexical_lookup(std::move(sym));
}

BlockEnv* Env::find_block(const std::string& name) {
  return m_parent->find_block(name);
}

RegVal* Env::make_gpr(const TypeSpec& ts) {
  return make_ireg(coerce_to_reg_type(ts), RegClass::GPR_64);
}

RegVal* Env::make_fpr(const TypeSpec& ts) {
  return make_ireg(coerce_to_reg_type(ts), RegClass::FLOAT);
}

RegVal* Env::make_vfr(const TypeSpec& ts) {
  return make_ireg(coerce_to_reg_type(ts), RegClass::VECTOR_FLOAT);
}

std::unordered_map<std::string, Label>& Env::get_label_map() {
  return parent()->get_label_map();
}

void Env::emit(const goos::Object& form, std::unique_ptr<IR> ir) {
  auto e = function_env();
  assert(e);
  e->emit(form, std::move(ir), this);
}

///////////////////
// GlobalEnv
///////////////////

// Because this is the top of the environment chain, all these end the parent calls and provide
// errors, or return that the items were not found.

GlobalEnv::GlobalEnv() : Env(EnvKind::OTHER_ENV, nullptr) {}

std::string GlobalEnv::print() {
  return "global-env";
}

/*!
 * Allocate an IRegister with the given type.
 */
RegVal* GlobalEnv::make_ireg(const TypeSpec& ts, RegClass reg_class) {
  (void)ts;
  (void)reg_class;
  throw std::runtime_error("cannot alloc reg in GlobalEnv");
}

/*!
 * Apply a register constraint to the current function.
 */
void GlobalEnv::constrain_reg(IRegConstraint constraint) {
  (void)constraint;
  throw std::runtime_error("cannot constrain reg in GlobalEnv");
}

/*!
 * Lookup the given symbol object as a lexical variable.
 */
RegVal* GlobalEnv::lexical_lookup(goos::Object sym) {
  (void)sym;
  return nullptr;
}

BlockEnv* GlobalEnv::find_block(const std::string& name) {
  (void)name;
  return nullptr;
}

FileEnv* GlobalEnv::add_file(std::string name) {
  m_files.push_back(std::make_unique<FileEnv>(this, std::move(name)));
  return m_files.back().get();
}

///////////////////
// BlockEnv
///////////////////

BlockEnv::BlockEnv(Env* parent, std::string _name)
    : Env(EnvKind::OTHER_ENV, parent), name(std::move(_name)) {}

std::string BlockEnv::print() {
  return "block-" + name;
}

BlockEnv* BlockEnv::find_block(const std::string& block) {
  if (name == block) {
    return this;
  } else {
    return parent()->find_block(block);
  }
}

///////////////////
// FileEnv
///////////////////

FileEnv::FileEnv(Env* parent, std::string name)
    : Env(EnvKind::FILE_ENV, parent), m_name(std::move(name)) {}

std::string FileEnv::print() {
  return "file-" + m_name;
}

void FileEnv::add_function(std::unique_ptr<FunctionEnv> fe) {
  assert(fe->idx_in_file == -1);
  fe->idx_in_file = m_functions.size();
  assert(!fe->name().empty());
  m_functions.push_back(std::move(fe));
}

void FileEnv::add_static(std::unique_ptr<StaticObject> s) {
  m_statics.push_back(std::move(s));
}

void FileEnv::add_top_level_function(std::unique_ptr<FunctionEnv> fe) {
  // todo, set FE as top level segment
  m_functions.push_back(std::move(fe));
  m_top_level_func = m_functions.back().get();
}

void FileEnv::debug_print_tl() {
  if (m_top_level_func) {
    for (auto& code : m_top_level_func->code()) {
      fmt::print("{}\n", code->print());
    }
  } else {
    printf("no top level function.\n");
  }
}

bool FileEnv::is_empty() {
  return m_functions.size() == 1 && m_functions.front().get() == m_top_level_func &&
         m_top_level_func->code().empty();
}
///////////////////
// FunctionEnv
///////////////////

FunctionEnv::FunctionEnv(Env* parent, std::string name, const goos::Reader* reader)
    : DeclareEnv(EnvKind::FUNCTION_ENV, parent), m_name(std::move(name)), m_reader(reader) {}

std::string FunctionEnv::print() {
  return "function-" + m_name;
}

void FunctionEnv::emit(const goos::Object& form, std::unique_ptr<IR> ir, Env* lowest_env) {
  ir->add_constraints(&m_constraints, m_code.size());
  m_code.push_back(std::move(ir));
  if (m_reader->db.has_info(form)) {
    // if we have info, it means we came from real code and we can just use that.
    m_code_debug_source.push_back(form);
  } else {
    // let's see if we're in a macro:
    auto mac_env = lowest_env->macro_expand_env();
    if (mac_env) {
      while (mac_env) {
        if (m_reader->db.has_info(mac_env->macro_use_location())) {
          m_code_debug_source.push_back(mac_env->macro_use_location());
          return;
        }
        auto parent = mac_env->parent();
        if (parent) {
          mac_env = parent->macro_expand_env();
        } else {
          m_code_debug_source.push_back(form);
          return;
        }
      }
    } else {
      m_code_debug_source.push_back(form);
    }
  }
}

void FunctionEnv::finish() {
  resolve_gotos();
}

void FunctionEnv::resolve_gotos() {
  for (auto& gt : unresolved_gotos) {
    auto kv_label = m_labels.find(gt.label);
    if (kv_label == m_labels.end()) {
      throw std::runtime_error("Invalid goto " + gt.label);
    }
    gt.ir->resolve(&kv_label->second);
  }

  for (auto& gt : unresolved_cond_gotos) {
    auto kv_label = m_labels.find(gt.label);
    if (kv_label == m_labels.end()) {
      throw std::runtime_error("invalid when-goto destination " + gt.label);
    }
    gt.ir->label = kv_label->second;
    gt.ir->mark_as_resolved();
  }
}

RegVal* FunctionEnv::make_ireg(const TypeSpec& ts, RegClass reg_class) {
  IRegister ireg;
  ireg.reg_class = reg_class;
  ireg.id = m_iregs.size();
  auto rv = std::make_unique<RegVal>(ireg, coerce_to_reg_type(ts));
  m_iregs.push_back(std::move(rv));
  assert(reg_class != RegClass::INVALID);
  return m_iregs.back().get();
}

std::unordered_map<std::string, Label>& FunctionEnv::get_label_map() {
  return m_labels;
}

std::unordered_map<std::string, Label>& LabelEnv::get_label_map() {
  return m_labels;
}

BlockEnv* LabelEnv::find_block(const std::string& name) {
  (void)name;
  return nullptr;
}

RegVal* FunctionEnv::lexical_lookup(goos::Object sym) {
  if (!sym.is_symbol()) {
    throw std::runtime_error("invalid symbol in lexical_lookup");
  }

  auto kv = params.find(sym.as_symbol()->name);
  if (kv == params.end()) {
    return parent()->lexical_lookup(sym);
  }

  return kv->second;
}

StackVarAddrVal* FunctionEnv::allocate_stack_variable(const TypeSpec& ts, int size_bytes) {
  require_aligned_stack();
  int slots_used = (size_bytes + emitter::GPR_SIZE - 1) / emitter::GPR_SIZE;
  auto result = alloc_val<StackVarAddrVal>(ts, m_stack_var_slots_used, slots_used);
  m_stack_var_slots_used += slots_used;
  return result;
}

StackVarAddrVal* FunctionEnv::allocate_aligned_stack_variable(const TypeSpec& ts,
                                                              int size_bytes,
                                                              int align_bytes) {
  require_aligned_stack();
  if (align_bytes > 16) {
    fmt::print("\n\n\nBad stack align: {} bytes for {}\n\n\n\n", align_bytes, ts.print());
  }
  assert(align_bytes <= 16);
  int align_slots = (align_bytes + emitter::GPR_SIZE - 1) / emitter::GPR_SIZE;
  while (m_stack_var_slots_used % align_slots) {
    m_stack_var_slots_used++;
  }

  // we align our size too. The stack versions of the default new methods in kscheme.cpp round up
  // to 16 bytes and memset this size, which can cause issues if we make this size only 8 byte
  // aligned.
  while (size_bytes % align_bytes) {
    size_bytes++;
  }

  int slots_used = (size_bytes + emitter::GPR_SIZE - 1) / emitter::GPR_SIZE;
  auto result = alloc_val<StackVarAddrVal>(ts, m_stack_var_slots_used, slots_used);
  m_stack_var_slots_used += slots_used;
  return result;
}

RegVal* FunctionEnv::push_reg_val(std::unique_ptr<RegVal> in) {
  m_iregs.push_back(std::move(in));
  return m_iregs.back().get();
}

///////////////////
// LexicalEnv
///////////////////

std::string LexicalEnv::print() {
  return "lexical";
}

RegVal* LexicalEnv::lexical_lookup(goos::Object sym) {
  if (!sym.is_symbol()) {
    throw std::runtime_error("invalid symbol in lexical_lookup");
  }

  auto kv = vars.find(sym.as_symbol()->name);
  if (kv == vars.end()) {
    return parent()->lexical_lookup(sym);
  }

  return kv->second;
}
