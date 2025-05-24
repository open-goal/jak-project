#include "Env.h"

#include <stdexcept>

#include "IR.h"

#include "common/goos/Reader.h"
#include "common/log/log.h"

#include "fmt/core.h"

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
  ASSERT(e);
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

std::vector<std::string> GlobalEnv::list_files_with_prefix(const std::string& prefix) {
  std::vector<std::string> matches = {};
  for (const auto& file : m_files) {
    if (file->name().rfind(prefix) == 0) {
      matches.push_back(file->name());
    }
  }
  return matches;
}

std::vector<std::unique_ptr<FileEnv>>& GlobalEnv::get_files() {
  return m_files;
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
  ASSERT(fe->idx_in_file == -1);
  fe->idx_in_file = m_functions.size();
  ASSERT(!fe->name().empty());
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
      lg::print("{}\n", code->print());
    }
  } else {
    printf("no top level function.\n");
  }
}

bool FileEnv::is_empty() {
  return m_functions.size() == 1 && m_functions.front().get() == m_top_level_func &&
         m_top_level_func->code().empty();
}

void FileEnv::cleanup_after_codegen() {
  m_top_level_func = nullptr;
  m_functions.clear();
  m_statics.clear();
  m_vals.clear();
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
  auto rv = std::make_unique<RegVal>(ireg, ts);
  m_iregs.push_back(std::move(rv));
  ASSERT(reg_class != RegClass::INVALID);
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

  auto kv = params.find(sym.as_symbol());
  if (kv == params.end()) {
    return parent()->lexical_lookup(sym);
  }

  return kv->second;
}

FunctionEnv::StackSpace FunctionEnv::allocate_aligned_stack_space(int size_bytes, int align_bytes) {
  require_aligned_stack();
  ASSERT(align_bytes <= 16);
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
  StackSpace result;
  result.slot_count = slots_used;
  result.start_slot = m_stack_var_slots_used;
  m_stack_var_slots_used += slots_used;
  return result;
}

StackVarAddrVal* FunctionEnv::allocate_aligned_stack_variable(const TypeSpec& ts,
                                                              int size_bytes,
                                                              int align_bytes) {
  if (align_bytes > 16) {
    lg::print("\n\n\nBad stack align: {} bytes for {}\n\n\n\n", align_bytes, ts.print());
  }
  auto space = allocate_aligned_stack_space(size_bytes, align_bytes);
  return alloc_val<StackVarAddrVal>(ts, space.start_slot, space.slot_count);
}

RegVal* FunctionEnv::push_reg_val(std::unique_ptr<RegVal> in) {
  m_iregs.push_back(std::move(in));
  return m_iregs.back().get();
}

StackVarAddrVal* FunctionEnv::allocate_stack_singleton(const TypeSpec& ts,
                                                       int size_bytes,
                                                       int align_bytes) {
  const auto& existing = m_stack_singleton_slots.find(ts.print());
  if (existing == m_stack_singleton_slots.end()) {
    auto space = allocate_aligned_stack_space(size_bytes, align_bytes);
    m_stack_singleton_slots[ts.print()] = space;
    return alloc_val<StackVarAddrVal>(ts, space.start_slot, space.slot_count);
  } else {
    return alloc_val<StackVarAddrVal>(ts, existing->second.start_slot, existing->second.slot_count);
  }
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

  auto kv = vars.find(sym.as_symbol());
  if (kv == vars.end()) {
    return parent()->lexical_lookup(sym);
  }

  return kv->second;
}
