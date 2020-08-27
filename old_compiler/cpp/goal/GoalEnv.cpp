#include "shared_config.h"
#include "logger/Logger.h"
#include "GoalEnv.h"

///////////////////
// GoalEnv
///////////////////
// These just pass on their requests to the parent env, and are used as a default if the given
// env type doesn't support the request.

/*!
 * Emit IR into the function currently being compiled.
 */
void GoalEnv::emit(std::unique_ptr<IR> ir) {
  // by default, we don't know how, so pass it up and hope for the best.
  parent->emit(std::move(ir));
}

/*!
 * Allocate a General Purpose Register with the given type.
 */
std::shared_ptr<Place> GoalEnv::alloc_reg(TypeSpec ct) {
  // we don't know how, so pass it up and hope for the best.
  return parent->alloc_reg(ct);
}

/*!
 * Apply a register constraint to the current function.
 */
void GoalEnv::constrain_reg(RegConstraint constraint) {
  // we don't know how, so pass it up and hope for the best.
  parent->constrain_reg(constraint);
}

/*!
 * Lookup the given object as a lexical variable.
 */
std::shared_ptr<Place> GoalEnv::lexical_lookup(Object sym) {
  // we don't know how, so pass it up and hope for the best.
  return parent->lexical_lookup(sym);
}

/*!
 * Find a block with the given name.  Or return nullptr if not found.
 */
GoalEnv* GoalEnv::find_block(const std::string& block) {
  // we don't know how, so pass it up and hope for the best.
  return parent->find_block(block);
}

/*!
 * Get the label map for the current function.
 */
std::unordered_map<std::string, std::shared_ptr<Label>>& GoalEnv::get_label_map() {
  // we don't know how, so pass it up and hope for the best.
  return parent->get_label_map();
}

/*!
 * Get the static objects in the object file.
 */
std::vector<std::shared_ptr<StaticPlace>>& GoalEnv::get_statics() {
  // we don't know how, so pass it up and hope for the best.
  return parent->get_statics();
}

///////////////////
// GlobalEnv
///////////////////

// Because this is the top of the environment chain, all these end the parent calls and provide
// errors, or return that the items were not found.

/*!
 * Print a global env.
 */
std::string GlobalEnv::print() {
  return "GLOBAL";
}

/*!
 * Emit into a global env, which is invalid.
 */
void GlobalEnv::emit(std::unique_ptr<IR> ir) {
  (void)ir;
  throw std::runtime_error("cannot emit to GlobalEnv");
}

/*!
 * Allocate register in global env, which is invalid.
 */
std::shared_ptr<Place> GlobalEnv::alloc_reg(TypeSpec ct) {
  (void)ct;
  throw std::runtime_error("cannot alloc reg in GlobalEnv");
}

/*!
 * Constraint register in global env, which is invalid.
 */
void GlobalEnv::constrain_reg(RegConstraint constraint) {
  (void)constraint;
  throw std::runtime_error("cannot constrain reg in GlobalEnv");
}

/*!
 * Find block got to the top, and didn't find anything.
 */
GoalEnv* GlobalEnv::find_block(const std::string& block) {
  (void)block;
  return nullptr;
}

/*!
 * Lexical lookup got to the top and didn't find anything.
 */
std::shared_ptr<Place> GlobalEnv::lexical_lookup(Object sym) {
  (void)sym;
  return nullptr;
}

/*!
 * Get static objects, which is invalid in the global env.
 */
std::vector<std::shared_ptr<StaticPlace>>& GlobalEnv::get_statics() {
  throw std::runtime_error("cannot get static list");
}

/*!
 * Get label map, which is invalid in the global env.
 */
std::unordered_map<std::string, std::shared_ptr<Label>>& GlobalEnv::get_label_map() {
  throw std::runtime_error("cannot get label map");
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
// ObjectFileEnv
///////////////////

ObjectFileEnv::ObjectFileEnv(std::string s) : name(std::move(s)) {}

ObjectFileEnv::ObjectFileEnv(std::string _name, std::shared_ptr<GoalEnv>& _parent)
    : GoalEnv(_parent), name(std::move(_name)) {}

/*!
 * Print the name of the object file env.
 */
std::string ObjectFileEnv::print() {
  return name;
}

/*!
 * Get the static objects of this object file.
 */
std::vector<std::shared_ptr<StaticPlace>>& ObjectFileEnv::get_statics() {
  return statics;
}

/*!
 * Add the given function as the top level function of this object file.
 */
void ObjectFileEnv::add_top_level_function(std::shared_ptr<FunctionEnv> f) {
  top_level_function = f;
  f->segment = TOP_LEVEL_SEGMENT;
  functions.push_back(f);  // todo, do we actually want this here?
}

/*!
 * Is this object file empty? Used to determine if a REPL command should not be sent to the target
 * because it would do nothing.
 */
bool ObjectFileEnv::is_empty() {
  // emptiness is when we are a single IR, which is return none.
  if (functions.size() == 1) {
    auto& ir = functions.front()->code;
    if (ir.size() == 1) {
      auto ir_as_return = dynamic_cast<IR_Return*>(ir.front().get());
      if (ir_as_return) {
        auto value_as_none = std::dynamic_pointer_cast<NonePlace>(ir_as_return->value);
        if (value_as_none) {
          return true;
        }
      } else {
        throw std::runtime_error("invalid function without return");
      }
    }
  }
  return false;
}

///////////////////
// DeclareEnv
///////////////////

DeclareEnv::DeclareEnv(std::shared_ptr<GoalEnv> _parent) : GoalEnv(_parent) {}

///////////////////
// FunctionEnv
///////////////////

FunctionEnv::FunctionEnv(std::string s) : name(std::move(s)) {}
FunctionEnv::FunctionEnv(std::string _name, std::shared_ptr<GoalEnv> _parent)
    : DeclareEnv(_parent), name(std::move(_name)) {}

/*!
 * Print the name of a function env.
 */
std::string FunctionEnv::print() {
  return "function-" + name;
}

/*!
 * Emit IR into a function env.
 */
void FunctionEnv::emit(std::unique_ptr<IR> ir) {
  code.emplace_back(std::move(ir));
}

/*!
 * Allocate a register in a function env.
 */
std::shared_ptr<Place> FunctionEnv::alloc_reg(TypeSpec ct) {
  vars.push_back(std::make_shared<GprPlace>(vars.size(), ct));
  return vars.back();
}

/*!
 * Constraint a register in a function env.
 */
void FunctionEnv::constrain_reg(RegConstraint constraint) {
  register_constraints.push_back(constraint);
}

/*!
 * Lexical lookup in a function env.
 */
std::shared_ptr<Place> FunctionEnv::lexical_lookup(Object sym) {
  if (sym.type != SYMBOL) {
    throw std::runtime_error("invalid symbol in lexical_lookup");
  }

  // look at the arguments to the function.
  auto kv = params.find(sym.as_symbol()->name);
  if (kv == params.end()) {
    // if not, see if an enclosing env. has these - this allows a lambda to look at variables
    // outside itself. but this is dangerous.
    return parent->lexical_lookup(sym);
  }

  return kv->second;
}

/*!
 * Get the label map of this function.
 */
std::unordered_map<std::string, std::shared_ptr<Label>>& FunctionEnv::get_label_map() {
  return labels;
}

/*!
 * Allocate an xmm register in this function.
 */
std::shared_ptr<Place> FunctionEnv::alloc_xmm_reg(TypeSpec ct) {
  vars.push_back(std::make_shared<XmmPlace>(vars.size(), ct));
  return vars.back();
}

/*!
 * Resolve gotos by name
 */
void FunctionEnv::resolve_gotos() {
  for (auto& gt : unresolved_gotos) {
    auto kv_label = labels.find(gt.label_name);
    if (kv_label == labels.end()) {
      throw std::runtime_error("Invalid goto " + gt.label_name);
    }
    gt.ir->label = kv_label->second;
    gt.ir->resolved = true;
  }

  for (auto& gt : unresolved_cond_gotos) {
    auto kv_label = labels.find(gt.label_name);
    if (kv_label == labels.end()) {
      throw std::runtime_error("invalid when-goto destination " + gt.label_name);
    }
    gt.ir->label = kv_label->second;
    gt.ir->resolved = true;
  }
}

/*!
 * Do any post-processing passes needed on a function.
 */
void FunctionEnv::finish() {
  resolve_gotos();
}

///////////////////
// BlockEnv
///////////////////

BlockEnv::BlockEnv(std::shared_ptr<GoalEnv> p, const std::string& _name) {
  parent = p;
  name = _name;
}

std::string BlockEnv::print() {
  return "block-" + name;
}

GoalEnv* BlockEnv::find_block(const std::string& block) {
  if (name == block)
    return this;
  return parent->find_block(block);
}

///////////////////
// LexicalEnv
///////////////////

std::shared_ptr<Place> LexicalEnv::lexical_lookup(Object sym) {
  if (sym.type != SYMBOL) {
    throw std::runtime_error("invalid symbol in lexical_lookup");
  }

  auto kv = vars.find(sym.as_symbol()->name);
  if (kv == vars.end()) {
    return parent->lexical_lookup(sym);
  }

  return kv->second;
}

std::string LexicalEnv::print() {
  std::string result = "lexical env:\n";
  for (const auto& kv : vars) {
    result += "(" + kv.first + " " + kv.second->print() + ")";
  }
  return result;
}

///////////////////
// LabelEnv
///////////////////
LabelEnv::LabelEnv(std::shared_ptr<GoalEnv> p) {
  parent = p;
}

std::string LabelEnv::print() {
  return "label-env";
}

std::unordered_map<std::string, std::shared_ptr<Label>>& LabelEnv::get_label_map() {
  return labels;
}

///////////////////
// WithInlineEnv
///////////////////

WithInlineEnv::WithInlineEnv(bool preference) : inline_preference(preference) {}
std::string WithInlineEnv::print() {
  return "inline-env: " + std::to_string((int)inline_preference);
}

///////////////////
// SymbolMacroEnv
///////////////////

SymbolMacroEnv::SymbolMacroEnv(std::shared_ptr<GoalEnv>& p) {
  parent = p;
}

std::string SymbolMacroEnv::print() {
  return "symbol-macro-env";
}
