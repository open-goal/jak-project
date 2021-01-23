#include <stdexcept>
#include <unordered_set>
#include <algorithm>
#include "Env.h"
#include "Form.h"
#include "decompiler/analysis/atomic_op_builder.h"

namespace decompiler {
std::string Env::get_variable_name(Register reg, int atomic_idx, VariableMode mode) const {
  return m_var_names.lookup(reg, atomic_idx, mode).name();
}

/*!
 * Update the Env with the result of the type analysis pass.
 */
void Env::set_types(const std::vector<TypeState>& block_init_types,
                    const std::vector<TypeState>& op_end_types,
                    const FunctionAtomicOps& atomic_ops) {
  m_block_init_types = block_init_types;
  m_op_end_types = op_end_types;

  // cache the init types (this ends up being faster)
  m_op_init_types.resize(op_end_types.size(), nullptr);
  for (int block_idx = 0; block_idx < int(m_block_init_types.size()); block_idx++) {
    int first_op = atomic_ops.block_id_to_first_atomic_op.at(block_idx);
    int end_op = atomic_ops.block_id_to_end_atomic_op.at(block_idx);
    if (end_op > first_op) {
      m_op_init_types.at(first_op) = &m_block_init_types.at(block_idx);
      for (int op_idx = first_op; op_idx < (end_op - 1); op_idx++) {
        m_op_init_types.at(op_idx + 1) = &m_op_end_types.at(op_idx);
      }
    }
  }

  for (auto x : m_op_init_types) {
    assert(x);
  }

  m_has_types = true;
}

std::string Env::print_local_var_types(const Form* top_level_form) const {
  assert(has_local_vars());
  std::vector<std::string> entries;

  if (top_level_form) {
    VariableSet var_set;
    top_level_form->collect_vars(var_set);

    // we want to sort them for easier reading:
    std::vector<std::pair<RegId, Variable>> vars;

    for (auto& x : var_set) {
      vars.push_back(std::make_pair(get_ssa_var(x), x));
    }

    std::sort(vars.begin(), vars.end(),
              [](const std::pair<RegId, Variable>& a, const std::pair<RegId, Variable>& b) {
                return a.first < b.first;
              });

    RegId* prev = nullptr;
    for (auto& x : vars) {
      // sorted by ssa var and there are likely duplicates of Variables and SSA vars, only print
      // unique ssa variables.
      if (prev && x.first == *prev) {
        continue;
      }
      prev = &x.first;
      auto& map = x.second.mode() == VariableMode::WRITE ? m_var_names.write_vars.at(x.second.reg())
                                                         : m_var_names.read_vars.at(x.second.reg());
      auto& info = map.at(x.first.id);

      if (info.initialized) {
        entries.push_back(fmt::format("{}: {}", info.name(), info.type.typespec().print()));
      } else {
        assert(false);
      }
    }
  } else {
    std::unordered_map<Register, std::unordered_set<int>, Register::hash> printed;

    for (auto& reg_info : m_var_names.read_vars) {
      auto& reg_printed = printed[reg_info.first];
      for (int var_id = 0; var_id < int(reg_info.second.size()); var_id++) {
        auto& info = reg_info.second.at(var_id);
        if (info.initialized) {
          reg_printed.insert(var_id);
          entries.push_back(fmt::format("{}: {}", info.name(), info.type.typespec().print()));
        }
      }
    }

    for (auto& reg_info : m_var_names.write_vars) {
      auto& reg_printed = printed[reg_info.first];
      for (int var_id = 0; var_id < int(reg_info.second.size()); var_id++) {
        auto& info = reg_info.second.at(var_id);
        if (info.initialized) {
          if (reg_printed.find(var_id) == reg_printed.end()) {
            entries.push_back(fmt::format("{}: {}", info.name(), info.type.typespec().print()));
          }
        }
      }
    }
  }

  int max_len = 0;
  for (auto& entry : entries) {
    if (int(entry.length()) > max_len) {
      max_len = entry.length();
    }
  }

  constexpr int row_len = 100;
  int per_row = std::max(1, row_len / max_len);
  int entry_len = 100 / per_row;

  std::string result;

  for (int entry_id = 0; entry_id < int(entries.size()); entry_id++) {
    if ((entry_id % per_row) == 0) {
      // onto a new line!
      if (entry_id != 0) {
        result += '\n';
      }
      result += ";; ";
    }
    result += ' ';
    result += entries.at(entry_id);
    result += std::string(std::max(0, entry_len - int(entries.at(entry_id).length())), ' ');
  }

  result += '\n';

  return result;
}

std::unordered_set<RegId, RegId::hash> Env::get_ssa_var(const VariableSet& vars) const {
  std::unordered_set<RegId, RegId::hash> result;
  for (auto& x : vars) {
    result.insert(get_ssa_var(x));
  }
  return result;
}

RegId Env::get_ssa_var(const Variable& var) const {
  return m_var_names.lookup(var.reg(), var.idx(), var.mode()).reg_id;
}
}  // namespace decompiler