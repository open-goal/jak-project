#pragma once

#include <vector>
#include <array>
#include "decompiler/Function/BasicBlocks.h"
#include "decompiler/Disasm/Register.h"
#include "decompiler/util/TP_Type.h"
#include "common/type_system/TypeSystem.h"

namespace decompiler {

class InstrTypeState;
class DecompWarnings;
struct PossibleType;
struct RegisterTypeState;

struct DerefHint {
  struct Token {
    enum class Kind { INTEGER, FIELD, VAR, INVALID } kind = Kind::INVALID;
    int integer = 0;
    std::string name;
    bool matches(const FieldReverseLookupOutput::Token& other) const;
  };
  std::vector<Token> tokens;

  bool matches(const FieldReverseLookupOutput& value) const;
};

/*!
 * Represents a reference to a type decision made on a previous instruction.
 */
struct TypeChoiceParent {
  RegisterTypeState* reg_type = nullptr;
  int idx_in_parent = -1;
};

/*!
 * Represents a possibility for the type in a register.
 * Can be "invalid", meaning it is eliminated from the possible types due to a constraint.
 * Use is_valid to check that it hasn't been eliminated.
 */
struct PossibleType {
  TP_Type type;  // the actual type.
  std::optional<FieldReverseLookupOutput>
      deref_path;     // the field accessed to get here, assuming we did a deref.
  double score = 0.;  // the sum of scores of all derefs to get here.

  // if we are a child, 0.
  // otherwise, the number of children who have a reference to us.
  int child_count = 0;

  TypeChoiceParent parent;  // the type we used to get this type.

  void eliminate();       // if possible, prune to avoid using this type.
  bool is_valid() const;  // true, unless we were eliminated.

  PossibleType(const TP_Type& tp_type) : type(tp_type) {}

 private:
  mutable bool m_valid_cache = true;
};

/*!
 * The set of all possible types in a register.
 */
struct RegisterTypeState {
  // this is just for printing errors, and isn't used by the analysis
  std::optional<TypeSpec> override_type;

  // if we're simplified to a single type, this will hold in the index in the possible types vector.

  // the types we can be.
  std::vector<PossibleType> possible_types;

  RegisterTypeState() = default;
  RegisterTypeState(const PossibleType& single_type) : possible_types({single_type}) {}
  void reduce_to_single_best_type(DecompWarnings* warnings, int op_idx, const DerefHint* hint);
  bool is_single_type() const;
  const PossibleType& get_single_type_decision() const;
  const TP_Type& get_single_tp_type() const;

  bool try_elimination(const TypeSpec& desired_types, const TypeSystem& ts);
  bool can_eliminate_to_get(const TypeSpec& desired_types, const TypeSystem& ts) const;

 private:
  mutable std::optional<int> single_type_cache;
};

/*!
 * During setup, this contains a alloc flag and a uid.
 * While it's running, it contains a pointer.
 */
/*
struct RegisterNode {
 RegisterTypeState* ptr() { return (RegisterTypeState*)data; }
 bool alloc() { return data & 1; }
 u64 uid() { return data >> 32; }
 void set_alloc() { data |= 1; }
 void set_uid(u64 uid) { data |= (uid << 32); }

private:
 uintptr_t data = 0;
 static_assert(sizeof(uintptr_t) == 8);
};
 */

struct RegisterNode {
  RegisterTypeState* ptr() { return m_ptr; }
  void set_ptr(RegisterTypeState* ptr) { m_ptr = ptr; }
  bool alloc() const { return !!m_ptr; }
  void set_alloc(RegisterTypeState* state) {
    m_ptr = state;
    m_alloc_point = true;
  }
  bool is_alloc_point() const { return m_alloc_point; }
  s64 uid() const { return m_uid; }
  void set_uid(s64 val) { m_uid = val; }

 private:
  RegisterTypeState* m_ptr = nullptr;
  s32 m_uid = 0;
  bool m_alloc_point = false;
};

class InstrTypeState {
 public:
  void add_stack_slot(int offset) { m_stack_slots.emplace_back(offset, RegisterNode()); }
  int stack_slot_count() const { return m_stack_slots.size(); }
  std::array<RegisterNode, Reg::MAX_VAR_REG_ID>& regs() { return m_regs; }
  std::vector<std::pair<int, RegisterNode>>& slots() { return m_stack_slots; }

  RegisterNode& get_slot(int offset) {
    for(auto& s : m_stack_slots) {
      if (s.first == offset) {
        return s.second;
      }
    }
    assert(false);
  }

  RegisterNode& get(const Register& reg) {
    assert(reg.reg_id() < Reg::MAX_VAR_REG_ID);
    return m_regs[reg.reg_id()];
  }

 private:
  std::array<RegisterNode, Reg::MAX_VAR_REG_ID> m_regs;
  std::vector<std::pair<int, RegisterNode>> m_stack_slots;
};

struct TypeAnalysisGraph {
  std::vector<InstrTypeState> after_op_types;
  std::vector<InstrTypeState> block_start_types;

  BlockTopologicalSort topo_sort;

  RegisterTypeState* alloc_regstate();

  std::vector<std::unique_ptr<RegisterTypeState>> node_pool;
};

class Function;
class DecompilerTypeSystem;
TypeAnalysisGraph make_analysis_graph(const TypeSpec& my_type,
                                      DecompilerTypeSystem& dts,
                                      Function& func,
                                      bool verbose);

}  // namespace decompiler