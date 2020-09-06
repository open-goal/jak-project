#include <stdexcept>
#include "Env.h"

///////////////////
// Env
///////////////////

/*!
 * Emit IR into the function currently being compiled.
 */
void Env::emit(std::unique_ptr<IR> ir) {
  // by default, we don't know how, so pass it up and hope for the best.
  m_parent->emit(std::move(ir));
}

/*!
 * Allocate an IRegister with the given type.
 */
RegVal* Env::make_ireg(TypeSpec ts, emitter::RegKind kind) {
  return m_parent->make_ireg(std::move(ts), kind);
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
Val* Env::lexical_lookup(goos::Object sym) {
  return m_parent->lexical_lookup(std::move(sym));
}

BlockEnv* Env::find_block(const std::string& name) {
  return m_parent->find_block(name);
}

RegVal* Env::make_gpr(TypeSpec ts) {
  return make_ireg(std::move(ts), emitter::RegKind::GPR);
}

RegVal* Env::make_xmm(TypeSpec ts) {
  return make_ireg(std::move(ts), emitter::RegKind::XMM);
}

///////////////////
// GlobalEnv
///////////////////

// Because this is the top of the environment chain, all these end the parent calls and provide
// errors, or return that the items were not found.

GlobalEnv::GlobalEnv() : Env(nullptr) {}

std::string GlobalEnv::print() {
  return "global-env";
}

/*!
 * Emit IR into the function currently being compiled.
 */
void GlobalEnv::emit(std::unique_ptr<IR> ir) {
  // by default, we don't know how, so pass it up and hope for the best.
  (void)ir;
  throw std::runtime_error("cannot emit to GlobalEnv");
}

/*!
 * Allocate an IRegister with the given type.
 */
RegVal* GlobalEnv::make_ireg(TypeSpec ts, emitter::RegKind kind) {
  (void)ts;
  (void)kind;
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
Val* GlobalEnv::lexical_lookup(goos::Object sym) {
  (void)sym;
  return nullptr;
}

BlockEnv * GlobalEnv::find_block(const std::string& name) {
  (void)name;
  return nullptr;
}

FileEnv * GlobalEnv::add_file(std::string name) {
  m_files.push_back(std::make_unique<FileEnv>(this, std::move(name)));
  return m_files.back().get();
}

///////////////////
// NoEmitEnv
///////////////////

/*!
 * Get the name of a NoEmitEnv
 */
std::string NoEmitEnv::print() {
  return "no-emit-env";
}

/*!
 * Emit - which is invalid - into a NoEmitEnv and throw an exception.
 */
void NoEmitEnv::emit(std::unique_ptr<IR> ir) {
  (void)ir;
  throw std::runtime_error("emit into a no-emit env!");
}

///////////////////
// FileEnv
///////////////////

FileEnv::FileEnv(Env* parent, std::string name) : Env(parent), m_name(std::move(name)) {}

std::string FileEnv::print() {
  return "file-" + m_name;
}

void FileEnv::add_function(std::unique_ptr<FunctionEnv> fe) {
  m_functions.push_back(std::move(fe));
}

void FileEnv::add_top_level_function(std::unique_ptr<FunctionEnv> fe) {
  // todo, set FE as top level segment
  m_functions.push_back(std::move(fe));
  m_top_level_func = m_functions.back().get();
}

NoEmitEnv * FileEnv::add_no_emit_env() {
  assert(!m_no_emit_env);
  m_no_emit_env = std::make_unique<NoEmitEnv>(this);
  return m_no_emit_env.get();
}

void FileEnv::debug_print_tl() {
  if(m_top_level_func) {
    for(auto& code : m_top_level_func->code()) {
      fmt::print("{}\n", code->print());
    }
  } else {
    printf("no top level function.\n");
  }
}

///////////////////
// FunctionEnv
///////////////////

FunctionEnv::FunctionEnv(Env* parent, std::string name) : DeclareEnv(parent), m_name(std::move(name)){}

std::string FunctionEnv::print() {
  return "function-" + m_name;
}

void FunctionEnv::emit(std::unique_ptr<IR> ir) {
  ir->add_constraints(&m_constraints, m_code.size());
  m_code.push_back(std::move(ir));
}
void FunctionEnv::finish() {
  // todo resolve gotos
}

RegVal * FunctionEnv::make_ireg(TypeSpec ts, emitter::RegKind kind) {
  IRegister ireg;
  ireg.kind = kind;
  ireg.id = m_iregs.size();
  auto rv = std::make_unique<RegVal>(ireg, ts);
  m_iregs.push_back(std::move(rv));
  return m_iregs.back().get();
}