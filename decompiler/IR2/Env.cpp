#include "Env.h"

#include <algorithm>
#include <stdexcept>
#include <unordered_set>

#include "AtomicOp.h"
#include "Form.h"

#include "common/goos/PrettyPrinter.h"
#include "common/log/log.h"
#include "common/util/math_util.h"

#include "decompiler/Function/Function.h"
#include "decompiler/analysis/atomic_op_builder.h"
#include "decompiler/util/DecompilerTypeSystem.h"

namespace decompiler {

constexpr const char* reg_names[] = {"a0-0", "a1-0", "a2-0", "a3-0",
                                     "t0-0", "t1-0", "t2-0", "t3-0"};

const char* get_reg_name(int idx) {
  if (idx >= 8) {
    return "INVALID";
  } else {
    return reg_names[idx];
  }
}

void Env::set_remap_for_function(const Function& func) {
  std::vector<std::string> default_arg_names = {};
  if (func.guessed_name.kind == FunctionName::FunctionKind::V_STATE ||
      func.guessed_name.kind == FunctionName::FunctionKind::NV_STATE) {
    default_arg_names = get_state_handler_arg_names(func.guessed_name.handler_kind);
  }
  int nargs = func.type.arg_count() - 1;
  for (int i = 0; i < nargs; i++) {
    if ((int)default_arg_names.size() > i) {
      m_var_remap[get_reg_name(i)] = default_arg_names.at(i);
    } else {
      m_var_remap[get_reg_name(i)] = ("arg" + std::to_string(i));
    }
  }
  if (func.type.try_get_tag("behavior")) {
    m_var_remap["s6-0"] = "self";
    m_pp_mapped_by_behavior = true;
  } else {
    m_var_remap["s6-0"] = "pp";
  }
}

void Env::set_remap_for_new_method(const TypeSpec& ts) {
  int nargs = ts.arg_count() - 1;
  m_var_remap["a0-0"] = "allocation";
  m_var_remap["a1-0"] = "type-to-make";
  for (int i = 2; i < nargs; i++) {
    m_var_remap[get_reg_name(i)] = ("arg" + std::to_string(i - 2));
  }
  if (ts.try_get_tag("behavior")) {
    m_var_remap["s6-0"] = "self";
    m_pp_mapped_by_behavior = true;
  } else {
    m_var_remap["s6-0"] = "pp";
  }
}

void Env::set_remap_for_relocate_method(const TypeSpec& ts) {
  int nargs = ts.arg_count() - 1;
  m_var_remap["a0-0"] = "this";
  if (ts.get_arg(1).base_type() == "kheap") {
    m_var_remap["a1-0"] = "heap";
  } else {
    m_var_remap["a1-0"] = "offset";
  }
  if (nargs > 1 && ts.get_arg(2).base_type() == "pointer") {
    m_var_remap["a2-0"] = "name";
  }
  for (int i = 3; i < nargs; i++) {
    m_var_remap[get_reg_name(i)] = ("arg" + std::to_string(i - 3));
  }
  if (ts.try_get_tag("behavior")) {
    m_var_remap["s6-0"] = "self";
    m_pp_mapped_by_behavior = true;
  } else {
    m_var_remap["s6-0"] = "pp";
  }
}

void Env::set_remap_for_memusage_method(const TypeSpec& ts) {
  int nargs = ts.arg_count() - 1;
  m_var_remap["a0-0"] = "this";
  m_var_remap["a1-0"] = "usage";
  m_var_remap["a2-0"] = "flags";
  for (int i = 3; i < nargs; i++) {
    m_var_remap[get_reg_name(i)] = ("arg" + std::to_string(i - 3));
  }
  if (ts.try_get_tag("behavior")) {
    m_var_remap["s6-0"] = "self";
    m_pp_mapped_by_behavior = true;
  } else {
    m_var_remap["s6-0"] = "pp";
  }
}

void Env::set_remap_for_method(const TypeSpec& ts) {
  int nargs = ts.arg_count() - 1;
  m_var_remap["a0-0"] = "this";
  for (int i = 1; i < nargs; i++) {
    m_var_remap[get_reg_name(i)] = ("arg" + std::to_string(i - 1));
  }
  if (ts.try_get_tag("behavior")) {
    m_var_remap["s6-0"] = "self";
    m_pp_mapped_by_behavior = true;
  } else {
    m_var_remap["s6-0"] = "pp";
  }
}

void Env::map_args_from_config(const std::vector<std::string>& args_names,
                               const std::unordered_map<std::string, std::string>& var_names) {
  for (size_t i = 0; i < args_names.size(); i++) {
    m_var_remap[get_reg_name(i)] = args_names[i];
  }

  for (auto& x : var_names) {
    m_var_remap[x.first] = x.second;
  }
}

void Env::map_args_from_config(
    const std::vector<std::string>& args_names,
    const std::unordered_map<std::string, LocalVarOverride>& var_overrides) {
  for (size_t i = 0; i < args_names.size(); i++) {
    m_var_remap[get_reg_name(i)] = args_names[i];
  }

  for (auto& x : var_overrides) {
    m_var_remap[x.first] = x.second.name;
  }
}

const std::string& Env::remapped_name(const std::string& name) const {
  auto kv = m_var_remap.find(name);
  if (kv != m_var_remap.end()) {
    return kv->second;
  } else {
    return name;
  }
}

goos::Object Env::get_variable_name_with_cast(const RegisterAccess& access) const {
  auto result = get_variable_and_cast(access);
  if (result.cast) {
    return pretty_print::build_list("the-as", result.cast->print(), result.name);
  } else {
    return pretty_print::to_symbol(result.name);
  }
}

std::string Env::get_variable_name(const RegisterAccess& access) const {
  return get_variable_and_cast(access).name;
}

std::string Env::get_variable_name_name_only(const RegisterAccess& access) const {
  if (is_stack_slot_access(access)) {
    return get_spill_slot_var_name(get_stack_slot_offset_from_access(access));
  }

  if (access.reg().get_kind() == Reg::FPR || access.reg().get_kind() == Reg::GPR) {
    auto& var_info = m_var_names.lookup(access.reg(), access.idx(), access.mode());
    return var_info.name();

  } else {
    return std::string(access.reg().to_charp());
  }
}

VariableWithCast Env::get_variable_and_cast(const RegisterAccess& access) const {
  if (is_stack_slot_access(access)) {
    VariableWithCast result;
    auto original_name = get_spill_slot_var_name(get_stack_slot_offset_from_access(access));
    auto remapped = m_var_remap.find(original_name);
    result.name = remapped != m_var_remap.end() ? remapped->second : original_name;
    return result;
  }

  if (access.reg().get_kind() == Reg::FPR || access.reg().get_kind() == Reg::GPR) {
    auto& var_info = m_var_names.lookup(access.reg(), access.idx(), access.mode());
    // this is a bit of a confusing process.  The first step is to grab the auto-generated name:
    std::string original_name = var_info.name();
    auto lookup_name = original_name;

    // and then see if there's a user remapping of it
    auto remapped = m_var_remap.find(original_name);
    if (remapped != m_var_remap.end()) {
      lookup_name = remapped->second;
    }

    if (types_succeeded) {
      // get the type of the variable. This is the type of thing if we do no casts.
      // first, get the type the decompiler found
      auto type_of_var = var_info.type.typespec();
      // and the user's type.
      auto retype_kv = m_var_retype.find(original_name);
      if (retype_kv != m_var_retype.end()) {
        type_of_var = retype_kv->second;
      }

      // next, we insert type casts that make enforce the user override.
      auto type_kv = m_typecasts.find(access.idx());
      if (type_kv != m_typecasts.end()) {
        for (auto& x : type_kv->second) {
          if (x.reg == access.reg()) {
            // let's make sure the above claim is true
            TypeSpec type_in_reg;
            if (has_type_analysis() && access.mode() == AccessMode::READ) {
              type_in_reg = get_types_for_op_mode(access.idx(), AccessMode::READ)
                                .get(access.reg())
                                .typespec();
              if (type_in_reg.print() != x.type_name) {
                lg::error(
                    "Decompiler type consistency error. There was a typecast for reg {} at idx {} "
                    "(var {}) to type {}, but the actual type is {} ({})",
                    access.reg().to_charp(), access.idx(), lookup_name, x.type_name,
                    type_in_reg.print(), type_in_reg.print());
                ASSERT(false);
              }

              if (type_of_var != type_in_reg) {
                // TODO - use the when possible?
                VariableWithCast result;
                result.cast = TypeSpec(x.type_name);
                result.name = lookup_name;
                return result;
              }
            }
          }
        }
      }

      // type analysis stuff runs before variable types, so we insert casts that account
      // for the changing types due to the lca(uses) that is used to generate variable types.
      auto type_of_reg =
          get_types_for_op_mode(access.idx(), access.mode()).get(access.reg()).typespec();
      if (access.mode() == AccessMode::READ) {
        // note - this may be stricter than needed. but that's ok.

        if (type_of_var != type_of_reg) {
          //        lg::print("casting {} (reg {}, idx {}): reg type {} var type {} remapped var
          //        type
          //        {}\n ",
          //                   lookup_name, reg.to_charp(), atomic_idx, type_of_reg.print(),
          //                   var_info.type.typespec().print(), type_of_var.print());
          VariableWithCast result;
          result.cast = type_of_reg;
          result.name = lookup_name;
          return result;
        }
      } else {
        // if we're setting a variable, we are a little less strict.
        // let's leave this to set!'s for now. This is tricky with stuff like (if y x) where the
        // move is eliminated so the RegisterAccess points to the "wrong" place.
        //      if (!dts->ts.tc(type_of_var, type_of_reg)) {
        //        lg::print("op {} reg {} type {}\n", atomic_idx, reg.to_charp(),
        //        get_types_for_op_mode(atomic_idx, mode).get(reg).print()); return
        //        pretty_print::build_list("the-as", type_of_reg.print(), lookup_name);
        //      }
      }
    }

    VariableWithCast result;
    result.name = lookup_name;
    return result;

  } else {
    VariableWithCast result;
    result.name = access.reg().to_charp();
    return result;
  }
}

goos::Object Env::get_variable_name_with_cast(Register reg, int atomic_idx, AccessMode mode) const {
  return get_variable_name_with_cast(RegisterAccess(mode, reg, atomic_idx, true));
}

std::optional<TypeSpec> Env::get_user_cast_for_access(const RegisterAccess& access) const {
  if (is_stack_slot_access(access)) {
    return {};
  }

  if (access.reg().get_kind() == Reg::FPR || access.reg().get_kind() == Reg::GPR) {
    auto& var_info = m_var_names.lookup(access.reg(), access.idx(), access.mode());
    std::string original_name = var_info.name();

    auto type_kv = m_typecasts.find(access.idx());
    if (type_kv != m_typecasts.end()) {
      for (auto& x : type_kv->second) {
        if (x.reg == access.reg()) {
          // let's make sure the above claim is true
          TypeSpec type_in_reg;
          if (has_type_analysis() && access.mode() == AccessMode::READ) {
            type_in_reg =
                get_types_for_op_mode(access.idx(), AccessMode::READ).get(access.reg()).typespec();
            if (type_in_reg.print() != x.type_name) {
              lg::error(
                  "Decompiler type consistency error. There was a typecast for reg {} at idx {} "
                  "(var {}) to type {}, but the actual type is {} ({})",
                  access.reg().to_charp(), access.idx(), original_name, x.type_name,
                  type_in_reg.print(), type_in_reg.print());
              ASSERT(false);
            }
          }

          auto cast_type = dts->parse_type_spec(x.type_name);
          return cast_type;
        }
      }
    }
  }
  return {};
}

/*!
 * Get the type of the variable currently in the register.
 * NOTE: this is _NOT_ the most specific type known to the decompiler, but instead the type
 * of the variable.
 */
TypeSpec Env::get_variable_type(const RegisterAccess& access, bool using_user_var_types) const {
  if (is_stack_slot_access(access)) {
    auto offset = get_stack_slot_offset_from_access(access);
    auto it = stack_slot_entries.find(offset);
    if (it != stack_slot_entries.end()) {
      auto type_of_var = it->second.typespec;
      if (using_user_var_types) {
        auto retype_kv = m_var_retype.find(it->second.name());
        if (retype_kv != m_var_retype.end()) {
          type_of_var = retype_kv->second;
        }
      }
      return type_of_var;
    }
    return TypeSpec("object");
  }

  if (access.reg().get_kind() == Reg::FPR || access.reg().get_kind() == Reg::GPR) {
    auto& var_info = m_var_names.lookup(access.reg(), access.idx(), access.mode());
    std::string original_name = var_info.name();

    auto type_of_var = var_info.type.typespec();
    if (using_user_var_types) {
      auto retype_kv = m_var_retype.find(original_name);
      if (retype_kv != m_var_retype.end()) {
        type_of_var = retype_kv->second;
      }
    }

    return type_of_var;
  } else {
    throw std::runtime_error("Types are not supported for this kind of register");
  }
}

TP_Type Env::get_variable_tp_type(const RegisterAccess& access, bool using_user_var_types) const {
  if (is_stack_slot_access(access)) {
    auto offset = get_stack_slot_offset_from_access(access);
    auto it = stack_slot_entries.find(offset);
    if (it != stack_slot_entries.end()) {
      auto type_of_var = it->second.tp_type;
      if (using_user_var_types) {
        auto retype_kv = m_var_retype.find(it->second.name());
        if (retype_kv != m_var_retype.end()) {
          type_of_var = TP_Type::make_from_ts(retype_kv->second);
        }
      }
      return type_of_var;
    }
    return TP_Type::make_from_ts(TypeSpec("object"));
  }

  if (access.reg().get_kind() == Reg::FPR || access.reg().get_kind() == Reg::GPR) {
    auto& var_info = m_var_names.lookup(access.reg(), access.idx(), access.mode());
    if (using_user_var_types) {
      auto retype_kv = m_var_retype.find(var_info.name());
      if (retype_kv != m_var_retype.end()) {
        return TP_Type::make_from_ts(retype_kv->second);
      }
    }
    return var_info.type;
  }

  throw std::runtime_error("TP types are not supported for this kind of register");
}

/*!
 * Update the Env with the result of the type analysis pass.
 */
void Env::set_types(const std::vector<TypeState>& block_init_types,
                    const std::vector<TypeState>& op_end_types,
                    const FunctionAtomicOps& atomic_ops,
                    const TypeSpec& my_type) {
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
    ASSERT(x);
  }

  m_has_types = true;

  // check the actual return type:
  if (my_type.last_arg() != TypeSpec("none")) {
    auto as_end = dynamic_cast<const FunctionEndOp*>(atomic_ops.ops.back().get());
    if (as_end) {
      m_type_analysis_return_type = get_types_before_op((int)atomic_ops.ops.size() - 1)
                                        .get(Register(Reg::GPR, Reg::V0))
                                        .typespec();
    }
  }
}

std::string Env::print_local_var_types(const Form* top_level_form) const {
  ASSERT(has_local_vars());
  auto var_info = extract_visible_variables(top_level_form);
  std::vector<std::string> entries;
  for (auto x : var_info) {
    entries.push_back(fmt::format("{}: {}", x.name(), x.type.typespec().print()));
  }

  int max_len = 0;
  for (auto& entry : entries) {
    if (int(entry.length()) > max_len) {
      max_len = entry.length();
    }
  }

  constexpr int row_len = 100;
  // avoid divide by zero on empty env case.
  int per_row = max_len ? std::max(1, row_len / max_len) : 1;
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

std::vector<VariableNames::VarInfo> Env::extract_visible_variables(
    const Form* top_level_form) const {
  ASSERT(has_local_vars());
  std::vector<VariableNames::VarInfo> entries;
  if (top_level_form) {
    RegAccessSet var_set;
    top_level_form->collect_vars(var_set, true);

    // we want to sort them for easier reading:
    std::vector<std::pair<RegId, RegisterAccess>> vars;

    for (auto& x : var_set) {
      if (is_stack_slot_access(x)) {
        continue;
      }
      if (x.reg().get_kind() == Reg::FPR || x.reg().get_kind() == Reg::GPR) {
        vars.push_back(std::make_pair(get_program_var_id(x), x));
      }
    }

    std::sort(vars.begin(), vars.end(),
              [](const std::pair<RegId, RegisterAccess>& a,
                 const std::pair<RegId, RegisterAccess>& b) { return a.first < b.first; });

    RegId* prev = nullptr;
    for (auto& x : vars) {
      // sorted by ssa var and there are likely duplicates of Variables and SSA vars, only print
      // unique ssa variables.
      if (prev && x.first == *prev) {
        continue;
      }
      prev = &x.first;
      auto& map = x.second.mode() == AccessMode::WRITE ? m_var_names.write_vars.at(x.second.reg())
                                                       : m_var_names.read_vars.at(x.second.reg());
      auto& info = map.at(x.first.id);

      if (info.initialized) {
        entries.push_back(info);
      } else {
        ASSERT(false);
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
          entries.push_back(info);
        }
      }
    }

    for (auto& reg_info : m_var_names.write_vars) {
      auto& reg_printed = printed[reg_info.first];
      for (int var_id = 0; var_id < int(reg_info.second.size()); var_id++) {
        auto& info = reg_info.second.at(var_id);
        if (info.initialized) {
          if (reg_printed.find(var_id) == reg_printed.end()) {
            entries.push_back(info);
          }
        }
      }
    }
  }
  return entries;
}

FunctionVariableDefinitions Env::local_var_type_list(const Form* top_level_form,
                                                     int nargs_to_ignore) const {
  ASSERT(nargs_to_ignore <= 8);
  auto vars = extract_visible_variables(top_level_form);
  std::unordered_set<int> visible_stack_slots;
  if (top_level_form) {
    RegAccessSet var_set;
    top_level_form->collect_vars(var_set, true);
    for (const auto& var : var_set) {
      if (is_stack_slot_access(var)) {
        visible_stack_slots.insert(get_stack_slot_offset_from_access(var));
      }
    }
  }

  FunctionVariableDefinitions result;
  std::vector<goos::Object> elts;
  elts.push_back(pretty_print::to_symbol("local-vars"));
  int count = 0;
  for (auto& x : vars) {
    if (x.reg_id.reg.get_kind() == Reg::GPR && x.reg_id.reg.get_gpr() < Reg::A0 + nargs_to_ignore &&
        x.reg_id.reg.get_gpr() >= Reg::A0 && x.reg_id.id == 0) {
      continue;
    }

    if (x.reg_id.reg == Register(Reg::GPR, Reg::S6)) {
      result.had_pp = true;
      continue;
    }

    std::string lookup_name = x.name();
    auto remapped = m_var_remap.find(lookup_name);
    if (remapped != m_var_remap.end()) {
      lookup_name = remapped->second;
    }

    if (m_vars_defined_in_let.find(lookup_name) != m_vars_defined_in_let.end()) {
      continue;
    }

    count++;
    elts.push_back(pretty_print::build_list(lookup_name, x.type.typespec().print()));
  }

  // sort in increasing offset.
  // it looks like this is the order the GOAL compiler itself used.
  std::vector<StackSpillEntry> spills;
  for (auto& x : stack_slot_entries) {
    if (top_level_form && !visible_stack_slots.count(x.first)) {
      continue;
    }
    spills.push_back(x.second);
  }
  std::sort(spills.begin(), spills.end(),
            [](const StackSpillEntry& a, const StackSpillEntry& b) { return a.offset < b.offset; });
  for (auto& x : spills) {
    if (m_vars_defined_in_let.find(x.name()) != m_vars_defined_in_let.end()) {
      continue;
    }
    elts.push_back(pretty_print::build_list(x.name(), x.typespec.print()));
    count++;
  }

  result.count = count;
  if (count > 0) {
    result.local_vars = pretty_print::build_list(elts);
  }
  return result;
}

std::unordered_set<RegId, RegId::hash> Env::get_ssa_var(const RegAccessSet& vars) const {
  std::unordered_set<RegId, RegId::hash> result;
  for (auto& x : vars) {
    result.insert(get_program_var_id(x));
  }
  return result;
}

RegId Env::get_program_var_id(const RegisterAccess& var) const {
  if (is_stack_slot_access(var)) {
    return RegId(Register(Reg::GPR, Reg::SP), get_stack_slot_var_id_from_access(var));
  }
  return m_var_names.lookup(var.reg(), var.idx(), var.mode()).reg_id;
}

const UseDefInfo& Env::get_use_def_info(const RegisterAccess& ra) const {
  ASSERT(has_local_vars());
  if (is_stack_slot_access(ra)) {
    return m_stack_slot_use_def_info.at(get_program_var_id(ra));
  }
  auto var_id = get_program_var_id(ra);
  return m_var_names.use_def_info.at(var_id);
}

RegisterAccess Env::get_stack_slot_access_for_op(int op_id, int offset) const {
  auto it = m_stack_slot_var_by_op.find(op_id);
  if (it == m_stack_slot_var_by_op.end()) {
    return make_stack_slot_access(offset);
  }
  return make_stack_slot_access(offset, it->second);
}

void Env::disable_def(const RegisterAccess& access, DecompWarnings& warnings) {
  if (is_stack_slot_access(access)) {
    // Stack-slot use/def info is intentionally immutable during expression building.
    // Unlike registers, the stack-slot analysis models value lifetimes through merged control flow
    // with synthetic phi-like vars, and mutating counts here would desynchronize later accesses
    // from that analysis.
    return;
  }
  if (has_local_vars()) {
    m_var_names.disable_def(access, warnings);
  }
}

void Env::disable_use(const RegisterAccess& access) {
  if (is_stack_slot_access(access)) {
    // See disable_def above.
    return;
  }
  if (has_local_vars()) {
    m_var_names.disable_use(access);
  }
}

void Env::rebuild_stack_slot_use_def_info() {
  m_stack_slot_use_def_info.clear();
  m_stack_slot_var_by_op.clear();

  if (!func || !func->ir2.atomic_ops) {
    return;
  }

  const auto& ops = *func->ir2.atomic_ops;
  const int block_count = (int)ops.block_id_to_first_atomic_op.size();
  std::vector<int> op_to_block(ops.ops.size(), -1);
  for (int block_id = 0; block_id < block_count; block_id++) {
    for (int op_id = ops.block_id_to_first_atomic_op.at(block_id);
         op_id < ops.block_id_to_end_atomic_op.at(block_id); op_id++) {
      op_to_block.at(op_id) = block_id;
    }
  }

  std::unordered_set<int> offsets;
  for (int op_id = 0; op_id < (int)ops.ops.size(); op_id++) {
    if (auto* store = dynamic_cast<const StackSpillStoreOp*>(ops.ops.at(op_id).get())) {
      offsets.insert(store->offset());
    } else if (auto* load = dynamic_cast<const StackSpillLoadOp*>(ops.ops.at(op_id).get())) {
      offsets.insert(load->offset());
    }
  }

  int next_var_id = 1;
  for (int offset : offsets) {
    std::vector<bool> reads_before_write(block_count, false);
    std::vector<bool> writes(block_count, false);

    for (int block_id = 0; block_id < block_count; block_id++) {
      bool seen_write = false;
      for (int op_id = ops.block_id_to_first_atomic_op.at(block_id);
           op_id < ops.block_id_to_end_atomic_op.at(block_id); op_id++) {
        const auto* op = ops.ops.at(op_id).get();
        if (auto* load = dynamic_cast<const StackSpillLoadOp*>(op)) {
          if (load->offset() == offset && !seen_write) {
            reads_before_write.at(block_id) = true;
          }
        } else if (auto* store = dynamic_cast<const StackSpillStoreOp*>(op)) {
          if (store->offset() == offset) {
            writes.at(block_id) = true;
            seen_write = true;
          }
        }
      }
    }

    std::vector<bool> live_in(block_count, false);
    std::vector<bool> live_out(block_count, false);
    bool changed = true;
    while (changed) {
      changed = false;
      for (int block_id = block_count - 1; block_id >= 0; block_id--) {
        bool new_live_out = false;
        for (int succ : {func->basic_blocks.at(block_id).succ_branch,
                         func->basic_blocks.at(block_id).succ_ft}) {
          if (succ != -1 && live_in.at(succ)) {
            new_live_out = true;
          }
        }

        bool new_live_in =
            reads_before_write.at(block_id) || (new_live_out && !writes.at(block_id));
        if (new_live_in != live_in.at(block_id) || new_live_out != live_out.at(block_id)) {
          live_in.at(block_id) = new_live_in;
          live_out.at(block_id) = new_live_out;
          changed = true;
        }
      }
    }

    std::vector<int> phi_var(block_count, -1);
    std::vector<std::vector<int>> phi_sources(block_count);
    for (int block_id = 0; block_id < block_count; block_id++) {
      if (live_in.at(block_id)) {
        phi_var.at(block_id) = next_var_id++;
      }
    }

    for (int block_id = 0; block_id < block_count; block_id++) {
      int current_var = live_in.at(block_id) ? phi_var.at(block_id) : -1;
      for (int op_id = ops.block_id_to_first_atomic_op.at(block_id);
           op_id < ops.block_id_to_end_atomic_op.at(block_id); op_id++) {
        const auto* op = ops.ops.at(op_id).get();
        if (auto* load = dynamic_cast<const StackSpillLoadOp*>(op)) {
          if (load->offset() != offset) {
            continue;
          }
          ASSERT(current_var != -1);
          m_stack_slot_var_by_op[op_id] = current_var;
          auto& info = m_stack_slot_use_def_info[RegId(Register(Reg::GPR, Reg::SP), current_var)];
          info.uses.push_back({op_id, block_id, AccessMode::READ, false});
          info.ssa_vars.insert(current_var);
        } else if (auto* store = dynamic_cast<const StackSpillStoreOp*>(op)) {
          if (store->offset() != offset) {
            continue;
          }
          current_var = next_var_id++;
          m_stack_slot_var_by_op[op_id] = current_var;
          auto& info = m_stack_slot_use_def_info[RegId(Register(Reg::GPR, Reg::SP), current_var)];
          info.defs.push_back({op_id, block_id, AccessMode::WRITE, false});
          info.ssa_vars.insert(current_var);
        }
      }

      if (!live_out.at(block_id)) {
        continue;
      }

      ASSERT(current_var != -1);
      for (int succ :
           {func->basic_blocks.at(block_id).succ_branch, func->basic_blocks.at(block_id).succ_ft}) {
        if (succ != -1 && live_in.at(succ)) {
          phi_sources.at(succ).push_back(current_var);
        }
      }
    }

    for (int block_id = 0; block_id < block_count; block_id++) {
      if (phi_var.at(block_id) == -1) {
        continue;
      }
      int first_op = ops.block_id_to_first_atomic_op.at(block_id);
      for (int src_var : phi_sources.at(block_id)) {
        auto& info = m_stack_slot_use_def_info[RegId(Register(Reg::GPR, Reg::SP), src_var)];
        info.uses.push_back({first_op, block_id, AccessMode::READ, false});
      }
    }
  }
}

/*!
 * Set the stack hints. This must be done before type analysis.
 * This actually parses the types, so it should be done after the dts is set up.
 */
void Env::set_stack_structure_hints(const std::vector<StackStructureHint>& hints) {
  for (auto& hint : hints) {
    add_stack_structure_hint(hint);
  }
}

void Env::add_stack_structure_hint(const StackStructureHint& hint) {
  StackStructureEntry entry;
  entry.hint = hint;

  switch (hint.container_type) {
    case StackStructureHint::ContainerType::NONE: {
      // parse the type spec.
      TypeSpec base_typespec = dts->parse_type_spec(hint.element_type);
      if (base_typespec.base_type() == "object") {
        throw std::runtime_error(
            fmt::format("Got a stack structure hint for type object at offset {}. This is usually "
                        "a sign that stack structure guessing got inconsistent types.",
                        hint.stack_offset));
      }
      auto type_info = dts->ts.lookup_type(base_typespec);
      // just a plain object on the stack.
      if (!type_info->is_reference()) {
        throw std::runtime_error(
            fmt::format("Stack variable type {} is not a reference and cannot be stored directly "
                        "on the stack at offset {}. Use an array instead.",
                        base_typespec.print(), hint.stack_offset));
      }
      entry.ref_type = base_typespec;
      entry.size = type_info->get_size_in_memory();
      // sanity check the alignment
      if (align(entry.hint.stack_offset, type_info->get_in_memory_alignment()) !=
          entry.hint.stack_offset) {
        lg::error("Misaligned stack variable of type {} offset {} required align {}\n",
                  entry.ref_type.print(), entry.hint.stack_offset,
                  type_info->get_in_memory_alignment());
      }
    } break;

    case StackStructureHint::ContainerType::INLINE_ARRAY: {
      TypeSpec base_typespec = dts->parse_type_spec(hint.element_type);
      auto type_info = dts->ts.lookup_type(base_typespec);
      if (!type_info->is_reference()) {
        throw std::runtime_error(
            fmt::format("Stack inline-array element type {} is not a reference and cannot be "
                        "stored in an inline-array. Use an array instead.",
                        base_typespec.print()));
      }

      entry.ref_type = TypeSpec("inline-array", {TypeSpec(base_typespec)});
      entry.size = 1;  // we assume that there is no constant propagation into this array and
      // make this only trigger in get_stack_type if we hit exactly.
      // sanity check the alignment
      if (align(entry.hint.stack_offset, type_info->get_in_memory_alignment()) !=
          entry.hint.stack_offset) {
        lg::error("Misaligned stack variable of type {} offset {} required align {}\n",
                  entry.ref_type.print(), entry.hint.stack_offset,
                  type_info->get_in_memory_alignment());
      }
    } break;

    case StackStructureHint::ContainerType::ARRAY: {
      TypeSpec base_typespec = dts->parse_type_spec(hint.element_type);
      entry.ref_type = TypeSpec("pointer", {TypeSpec(base_typespec)});
      entry.size = 1;  // we assume that there is no constant propagation into this array and
      // make this only trigger in get_stack_type if we hit exactly.
      break;
    }
    default:
      ASSERT(false);
  }

  m_stack_structures.push_back(entry);
}

std::optional<std::string> Env::get_art_elt_name(int idx) const {
  ASSERT(dts);
  auto it = dts->art_group_info.find(art_group());
  if (it == dts->art_group_info.end()) {
    return {};
  } else {
    const auto& art_group = it->second;
    auto it2 = art_group.find(idx);
    if (it2 == art_group.end()) {
      return {};
    } else {
      return it2->second;
    }
  }
}

std::optional<std::string> Env::get_part_group_name(int id) const {
  ASSERT(dts);
  auto it = dts->part_group_table.find(id);
  if (it == dts->part_group_table.end()) {
    return {};
  } else {
    return it->second;
  }
}

std::optional<std::string> Env::get_joint_node_name(int idx) const {
  ASSERT(dts);
  auto it = dts->jg_info.find(joint_geo());
  if (it == dts->jg_info.end()) {
    return {};
  } else {
    const auto& jg = it->second;
    auto it2 = jg.find(idx);
    if (it2 == jg.end()) {
      return {};
    } else {
      return it2->second;
    }
  }
}

}  // namespace decompiler
