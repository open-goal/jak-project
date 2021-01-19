#include <stdexcept>
#include <unordered_set>
#include "Env.h"

namespace decompiler {
std::string Env::get_variable_name(Register reg, int atomic_idx, VariableMode mode) const {
  return m_var_names.lookup(reg, atomic_idx, mode).name();
}

/*!
 * Update the Env with the result of the type analysis pass.
 */
void Env::set_types(const std::vector<TypeState>& block_init_types,
                    const std::vector<TypeState>& op_end_types) {
  m_block_init_types = block_init_types;
  m_op_end_types = op_end_types;
  m_has_types = true;
}

std::string Env::print_local_var_types() const {
  assert(has_local_vars());
  std::vector<std::string> entries;
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
}  // namespace decompiler