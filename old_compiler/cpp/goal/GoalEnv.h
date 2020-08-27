/*!
 * @file GoalEnv.h
 * GOAL Environment.  Provides the current context for compilation.
 */

#ifndef JAK_GOALENV_H
#define JAK_GOALENV_H

#include <memory>
#include <string>
#include <vector>
#include <stdexcept>
#include <unordered_map>
#include "codegen/x86.h"
#include "IR.h"
#include "Label.h"
#include "codegen/ColoringAssignment.h"
#include "GoalPlace.h"

class DeclareEnv;

// Parent Class of all Environment Classes
class GoalEnv {
 public:
  std::shared_ptr<GoalEnv> parent = nullptr;

  GoalEnv() = default;
  explicit GoalEnv(std::shared_ptr<GoalEnv>& _parent) : parent(_parent) {}

  // a handful of commonly used environment functions which all environments are required to support

  virtual std::string print() = 0;
  virtual void emit(std::unique_ptr<IR> ir);
  virtual std::shared_ptr<Place> alloc_reg(TypeSpec ct);
  virtual void constrain_reg(RegConstraint constraint);
  virtual std::shared_ptr<Place> lexical_lookup(Object sym);
  virtual GoalEnv* find_block(const std::string& block);
  virtual std::unordered_map<std::string, std::shared_ptr<Label>>& get_label_map();
  virtual std::vector<std::shared_ptr<StaticPlace>>& get_statics();
};

// Highest level environment. This is just a way to catch environment calls which go up too high so
// they end somewhere.
class GlobalEnv : public GoalEnv {
 public:
  std::string print() override;
  void emit(std::unique_ptr<IR> ir) override;
  std::shared_ptr<Place> alloc_reg(TypeSpec ct) override;
  void constrain_reg(RegConstraint constraint) override;
  GoalEnv* find_block(const std::string& block) override;
  std::shared_ptr<Place> lexical_lookup(Object sym) override;
  std::vector<std::shared_ptr<StaticPlace>>& get_statics() override;
  std::unordered_map<std::string, std::shared_ptr<Label>>& get_label_map() override;
};

// An environment which you cannot emit code into. Useful to make sure that compilation does not
// require code, for instance to compute a constant for a static field.
class NoEmitEnv : public GoalEnv {
 public:
  std::string print() override;
  void emit(std::unique_ptr<IR> ir) override;
};

class FunctionEnv;

// An environment representing an object file/translation unit.
class ObjectFileEnv : public GoalEnv {
 public:
  std::string name;                                     // name of object file / translation unit
  std::vector<std::shared_ptr<FunctionEnv>> functions;  // all functions in the file
  std::vector<std::shared_ptr<StaticPlace>> statics;    // all static objects in the file
  std::shared_ptr<FunctionEnv> top_level_function = nullptr;  // the top-level function of the file

  explicit ObjectFileEnv(std::string s);
  ObjectFileEnv(std::string _name, std::shared_ptr<GoalEnv>& _parent);
  std::string print() override;
  std::vector<std::shared_ptr<StaticPlace>>& get_statics() override;
  void add_top_level_function(std::shared_ptr<FunctionEnv> f);
  bool is_empty();
};

// An environment which can receive a (declare ...) statement.
class DeclareEnv : public GoalEnv {
 public:
  DeclareEnv() = default;
  explicit DeclareEnv(std::shared_ptr<GoalEnv> _parent);
  virtual std::string print() = 0;

  struct DeclareSettings {
    bool is_set = false;
    bool inline_by_default = false;
    bool save_code = true;
    bool allow_inline = false;
  } settings;
};

// An environment representing a function
class FunctionEnv : public DeclareEnv {
 public:
  std::string name;                                 // function name
  std::vector<std::unique_ptr<IR>> code;            // all IR of the function
  std::vector<std::shared_ptr<Place>> vars;         // variables of the function (to color)
  std::vector<RegConstraint> register_constraints;  // constraints for coloring
  std::vector<UnresolvedGoto> unresolved_gotos;     // goto's by name that are not resolved
  std::vector<UnresolvedConditionalGoto> unresolved_cond_gotos;
  bool coloring_done = false;                 // is the coloring done?
  bool requires_aligned_stack = false;        // does our stack need to be aligned?
  bool is_asm_func = false;                   // is this an asm function?
  int segment = -1;                           // which segment do we go in?
  int first_instruction;                      // index of our first instruction when emitted
  std::vector<LiveRange> coloring;            // coloring assignment after coloring
  bool uses_saved_reg[SAVED_REG_COUNT] = {};  // which saved regs we use
  bool uses_rbp = false;                      // do we need the rbp register?
  std::vector<RegAllocBonusInstruction> bonus_instructions;        // spill instructions
  std::unordered_map<std::string, std::shared_ptr<Label>> labels;  // labels defined in the function
  std::unordered_map<std::string, std::shared_ptr<Place>> params;  // function arguments
  std::string method_of_type_name =
      "#f";             // if we're a method, the name of the type, or #f otherwise
  int stack_slots = 0;  // how many stack slots do we need?

  explicit FunctionEnv(std::string s);
  FunctionEnv(std::string _name, std::shared_ptr<GoalEnv> _parent);
  std::string print() override;
  void emit(std::unique_ptr<IR> ir) override;
  std::shared_ptr<Place> alloc_reg(TypeSpec ct) override;
  void constrain_reg(RegConstraint constraint) override;
  std::shared_ptr<Place> lexical_lookup(Object sym) override;
  std::unordered_map<std::string, std::shared_ptr<Label>>& get_label_map() override;

  std::shared_ptr<Place> alloc_xmm_reg(TypeSpec ct);
  void finish();
  void resolve_gotos();
};

// An environment for a block
class BlockEnv : public GoalEnv {
 public:
  std::string name;                     // block name
  std::shared_ptr<Label> end_label;     // label to jump to end of block
  std::shared_ptr<Place> return_value;  // the register holding the result of the block
  std::vector<TypeSpec> return_types;   // the possible types of return_value.

  BlockEnv(std::shared_ptr<GoalEnv> p, const std::string& _name);
  std::string print() override;
  GoalEnv* find_block(const std::string& block) override;
};

// An environment for holding variables. Can also receive declares.
class LexicalEnv : public DeclareEnv {
 public:
  LexicalEnv() = default;
  std::shared_ptr<Place> lexical_lookup(Object sym) override;
  std::string print() override;
  std::unordered_map<std::string, std::shared_ptr<Place>> vars;
};

// A label name space
class LabelEnv : public GoalEnv {
 public:
  explicit LabelEnv(std::shared_ptr<GoalEnv> p);
  std::string print() override;
  std::unordered_map<std::string, std::shared_ptr<Label>>& get_label_map() override;
  std::unordered_map<std::string, std::shared_ptr<Label>> labels;
};

// An env representing a preference for inlining.
class WithInlineEnv : public GoalEnv {
 public:
  bool inline_preference = false;

  WithInlineEnv(bool preference);
  std::string print() override;
};

// An env for a symbol macro namespace
class SymbolMacroEnv : public GoalEnv {
 public:
  std::unordered_map<std::shared_ptr<SymbolObject>, Object> macros;

  explicit SymbolMacroEnv(std::shared_ptr<GoalEnv>& p);
  std::string print() override;
};

/*!
 * Search up the environment chain until we find a parent env of the given type.
 */
template <typename T>
std::shared_ptr<T> get_parent_env_of_type(std::shared_ptr<GoalEnv> in) {
  for (;;) {
    auto attempt = std::dynamic_pointer_cast<T>(in);
    if (attempt)
      return attempt;
    if (std::dynamic_pointer_cast<GlobalEnv>(in)) {
      return nullptr;
    }
    in = in->parent;
  }
}

#endif  // JAK_GOALENV_H
