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

  const PossibleType& get() const;
  PossibleType& get();
  void remove_ref();
};

/*!
 * Represents a possibility for the type in a register.
 * Can be "invalid", meaning it is eliminated from the possible types due to a constraint.
 * Use is_valid to check that it hasn't been eliminated.
 */
struct PossibleType {
  TP_Type type;  // the actual type.

  // the field accessed to get here, assuming we did a deref.
  std::optional<FieldReverseLookupOutput> deref_path;

  // the sum of scores of all derefs to get here.
  // this can be used to compare us to others in the same RegisterTypeState.
  double score = 0.;

  // if we are a child, 0.
  // otherwise, the number of children who have a reference to us.
  int child_count = 0;

  TypeChoiceParent parent;  // the type we used to get this type.

  void eliminate();       // if possible, prune to avoid using this type.
  bool is_valid() const;  // true, unless we were eliminated.

  PossibleType(const TP_Type& tp_type) : type(tp_type) {}
  PossibleType() = default;

  std::string to_string() const;

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

  bool is_temp_node = false;

  RegisterTypeState() = default;
  explicit RegisterTypeState(const PossibleType& single_type) : possible_types({single_type}) {
    single_type_cache = 0;
  }
  explicit RegisterTypeState(const TP_Type& single_type)
      : RegisterTypeState(PossibleType(single_type)) {}
  explicit RegisterTypeState(const TypeSpec& type)
      : RegisterTypeState(TP_Type::make_from_ts(type)) {}
  explicit RegisterTypeState(const std::string& type)
      : RegisterTypeState(TP_Type::make_from_ts(type)) {}
  void reduce_to_single_best_type(DecompWarnings* warnings, int op_idx, const DerefHint* hint);
  bool is_single_type() const;
  const PossibleType& get_single_type_decision() const;
  const TP_Type& get_single_tp_type() const;

  bool try_elimination(const TypeSpec& desired_types, const TypeSystem& ts);
  bool can_eliminate_to_get(const TypeSpec& desired_types, const TypeSystem& ts) const;

  void add_possibility(TypeChoiceParent& parent,
                       const TP_Type& type,
                       double delta_score,
                       const std::optional<FieldReverseLookupOutput>& deref);

  RegisterTypeState copy_and_make_child();

  std::string to_string() const;

 private:
  mutable std::optional<int> single_type_cache;
};

/*!
 * During setup, this contains a alloc flag and a uid.
 * While it's running, it contains a pointer.
 */

struct RegisterNode {
  RegisterTypeState* ptr() { return m_ptr; }
  const RegisterTypeState* ptr() const { return m_ptr; }
  void set_cast_temp_ptr(RegisterTypeState* ptr) {
    m_ptr = ptr;
    m_flags |= FLAG_CAST_TEMP;
  }
  bool alloc() const { return !!m_ptr; }
  void set_alloc(RegisterTypeState* state) {
    m_ptr = state;
    m_flags |= FLAG_ALLOC_POINT;
  }
  void set_clobber(RegisterTypeState* state) {
    m_ptr = state;
    m_flags |= FLAG_CLOBBER;
  }
  bool is_alloc_point() const { return m_flags & FLAG_ALLOC_POINT; }
  bool is_clobber() const { return m_flags & FLAG_CLOBBER; }
  bool is_cast() const { return m_flags & FLAG_CAST_TEMP; }
  s64 uid() const { return m_uid; }
  void set_uid(s64 val) { m_uid = val; }

 private:
  RegisterTypeState* m_ptr = nullptr;
  s32 m_uid = 0;

  u8 m_flags = 0;
  static constexpr u8 FLAG_ALLOC_POINT = 1;
  static constexpr u8 FLAG_CLOBBER = 2;
  static constexpr u8 FLAG_CAST_TEMP = 4;
  static constexpr u8 FLAG_CAST_FINAL = 8;
};

class InstrTypeState {
 public:
  void add_stack_slot(int offset) { m_stack_slots.emplace_back(offset, RegisterNode()); }
  int stack_slot_count() const { return m_stack_slots.size(); }
  std::array<RegisterNode, Reg::MAX_VAR_REG_ID>& regs() { return m_regs; }
  std::vector<std::pair<int, RegisterNode>>& slots() { return m_stack_slots; }
  const std::array<RegisterNode, Reg::MAX_VAR_REG_ID>& regs() const { return m_regs; }
  const std::vector<std::pair<int, RegisterNode>>& slots() const { return m_stack_slots; }

  RegisterNode& get_slot(int offset) {
    for (auto& s : m_stack_slots) {
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

  RegisterTypeState& get_state(const Register& reg) { return *get(reg).ptr(); }
  RegisterTypeState& get_slot_state(int offset) { return *get_slot(offset).ptr(); }
  void assign(const Register& reg, const RegisterTypeState& value);

 private:
  std::array<RegisterNode, Reg::MAX_VAR_REG_ID> m_regs;
  std::vector<std::pair<int, RegisterNode>> m_stack_slots;
};

struct TypeAnalysisGraph {
  std::vector<InstrTypeState> after_op_types;
  std::vector<InstrTypeState> block_start_types;
  std::vector<std::unique_ptr<RegisterTypeState>> final_cast_nodes;

  BlockTopologicalSort topo_sort;

  RegisterTypeState* alloc_regstate();

  std::vector<std::unique_ptr<RegisterTypeState>> node_pool;
};

class Function;
class DecompilerTypeSystem;
std::shared_ptr<TypeAnalysisGraph> allocate_analysis_graph(const TypeSpec& my_type,
                                                           DecompilerTypeSystem& dts,
                                                           Function& func,
                                                           bool verbose);
bool run_multi_type_analysis(const TypeSpec& my_type,
                             DecompilerTypeSystem& dts,
                             Function& func,
                             TypeAnalysisGraph& graph);

}  // namespace decompiler