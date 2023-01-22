#include "DecompilerTypeSystem.h"

#include "TP_Type.h"

#include "common/goos/Reader.h"
#include "common/log/log.h"
#include "common/type_system/defenum.h"
#include "common/type_system/deftype.h"
#include "common/util/string_util.h"

#include "decompiler/Disasm/Register.h"

namespace decompiler {
DecompilerTypeSystem::DecompilerTypeSystem(GameVersion version) {
  ts.add_builtin_types(version);
}

namespace {
// some utilities for parsing the type def file

goos::Object& car(const goos::Object& pair) {
  if (pair.is_pair()) {
    return pair.as_pair()->car;
  } else {
    throw std::runtime_error("car called on something that was not a pair: " + pair.print());
  }
}

goos::Object& cdr(const goos::Object& pair) {
  if (pair.is_pair()) {
    return pair.as_pair()->cdr;
  } else {
    throw std::runtime_error("cdr called on something that was not a pair");
  }
}

template <typename T>
void for_each_in_list(goos::Object& list, T f) {
  goos::Object* iter = &list;
  while (iter->is_pair()) {
    f(car(*iter));
    iter = &cdr(*iter);
  }

  if (!iter->is_empty_list()) {
    throw std::runtime_error("malformed list");
  }
}
}  // namespace

void DecompilerTypeSystem::parse_type_defs(const std::vector<std::string>& file_path) {
  auto read = m_reader.read_from_file(file_path);
  auto& data = cdr(read);

  for_each_in_list(data, [&](goos::Object& o) {
    try {
      if (car(o).as_symbol()->name == "define-extern") {
        auto symbol_metadata = DefinitionMetadata();
        auto* rest = &cdr(o);
        auto sym_name = car(*rest);
        rest = &cdr(*rest);
        // check for docstring
        if (rest->is_pair() && car(*rest).is_string()) {
          symbol_metadata.docstring = str_util::trim_newline_indents(car(*rest).as_string()->data);
          rest = &cdr(*rest);
        }
        auto sym_type = car(*rest);
        if (!cdr(*rest).is_empty_list()) {
          throw std::runtime_error("malformed define-extern");
        }
        symbol_metadata.definition_info = m_reader.db.get_short_info_for(o);
        add_symbol(sym_name.as_symbol()->name, parse_typespec(&ts, sym_type), symbol_metadata);
      } else if (car(o).as_symbol()->name == "deftype") {
        auto dtr = parse_deftype(cdr(o), &ts);
        dtr.type_info->m_metadata.definition_info = m_reader.db.get_short_info_for(o);
        if (dtr.create_runtime_type) {
          add_symbol(dtr.type.base_type(), "type", dtr.type_info->m_metadata);
        }
        // declare the type's states globally
        for (auto& state : dtr.type_info->get_states_declared_for_type()) {
          // TODO - get definition info for the state definitions specifically
          add_symbol(state.first, state.second, dtr.type_info->m_metadata);
        }
        // add state documentation to the DTS
        virtual_state_metadata.emplace(dtr.type.base_type(),
                                       dtr.type_info->m_virtual_state_definition_meta);
        for (const auto& [state_name, meta] : dtr.type_info->m_state_definition_meta) {
          state_metadata.emplace(state_name, meta);
        }
      } else if (car(o).as_symbol()->name == "declare-type") {
        auto* rest = &cdr(o);
        auto type_name = car(*rest);
        rest = &cdr(*rest);
        auto type_kind = car(*rest);
        if (!cdr(*rest).is_empty_list()) {
          throw std::runtime_error("malformed declare-type");
        }
        ts.forward_declare_type_as(type_name.as_symbol()->name, type_kind.as_symbol()->name);
      } else if (car(o).as_symbol()->name == "defenum") {
        auto symbol_metadata = DefinitionMetadata();
        parse_defenum(cdr(o), &ts, &symbol_metadata);
        symbol_metadata.definition_info = m_reader.db.get_short_info_for(o);
        auto* rest = &cdr(o);
        const auto& enum_name = car(*rest).as_symbol()->name;
        symbol_metadata_map[enum_name] = symbol_metadata;
        // so far, enums are never runtime types so there's no symbol for them.
      } else {
        throw std::runtime_error("Decompiler cannot parse " + car(o).print());
      }
    } catch (std::exception& e) {
      auto info = m_reader.db.get_info_for(o);
      lg::error("{} when parsing decompiler type file:{}", e.what(), info);
      throw e;
    }
  });
}

TypeSpec DecompilerTypeSystem::parse_type_spec(const std::string& str) const {
  auto read = m_reader.read_from_string(str);
  auto data = cdr(read);
  return parse_typespec(&ts, car(data));
}

std::string DecompilerTypeSystem::dump_symbol_types() {
  ASSERT(symbol_add_order.size() == symbols.size());
  std::string result;
  for (auto& symbol_name : symbol_add_order) {
    auto skv = symbol_types.find(symbol_name);
    if (skv == symbol_types.end()) {
      result += fmt::format(";;(define-extern {} object) ;; unknown type\n", symbol_name);
    } else {
      result += fmt::format("(define-extern {} {})\n", symbol_name, skv->second.print());
    }
  }
  return result;
}

void DecompilerTypeSystem::add_type_flags(const std::string& name, u64 flags) {
  auto kv = type_flags.find(name);
  if (kv != type_flags.end()) {
    if (kv->second != flags) {
      lg::warn("duplicated type flags for {}, was 0x{:x}, now 0x{:x}", name.c_str(), kv->second,
               flags);
      lg::warn("duplicated type flags that are inconsistent!");
    }
  }
  type_flags[name] = flags;
}

void DecompilerTypeSystem::add_type_parent(const std::string& child, const std::string& parent) {
  auto kv = type_parents.find(child);
  if (kv != type_parents.end()) {
    if (kv->second != parent) {
      lg::warn("duplicated type parents for {} was {} now {}", child.c_str(), kv->second.c_str(),
               parent.c_str());
      throw std::runtime_error("duplicated type parents that are inconsistent!");
    }
  }
  type_parents[child] = parent;
}

std::string DecompilerTypeSystem::lookup_parent_from_inspects(const std::string& child) const {
  auto kv_tp = type_parents.find(child);
  if (kv_tp != type_parents.end()) {
    return kv_tp->second;
  }

  return "UNKNOWN";
}

bool DecompilerTypeSystem::lookup_flags(const std::string& type, u64* dest) const {
  auto kv = type_flags.find(type);
  if (kv != type_flags.end()) {
    *dest = kv->second;
    return true;
  }
  return false;
}

void DecompilerTypeSystem::add_symbol(const std::string& name,
                                      const TypeSpec& type_spec,
                                      const DefinitionMetadata& symbol_metadata) {
  add_symbol(name);
  auto skv = symbol_types.find(name);
  if (skv == symbol_types.end() || skv->second == type_spec) {
    symbol_types[name] = type_spec;
    // TODO - could get rid of this if there is a way to go from TypeSpec -> full Type
    if (symbol_metadata.definition_info) {
      symbol_metadata_map[name] = symbol_metadata;
    }
  } else {
    if (ts.tc(type_spec, skv->second)) {
    } else {
      lg::warn("Attempting to redefine type of symbol {} from {} to {}", name, skv->second.print(),
               type_spec.print());
      throw std::runtime_error("Type redefinition");
    }
  }
}

/*!
 * Compute the least common ancestor of two TP Types.
 */
TP_Type DecompilerTypeSystem::tp_lca(const TP_Type& existing,
                                     const TP_Type& add,
                                     bool* changed) const {
  // starting from most vague to most specific

  // simplest case, no difference.
  if (existing == add) {
    *changed = false;
    return existing;
  }

  // being sometimes uninitialized should not modify types.
  if (add.kind == TP_Type::Kind::UNINITIALIZED) {
    *changed = false;
    return existing;
  }

  // replace anything that's uninitialized sometimes.
  if (existing.kind == TP_Type::Kind::UNINITIALIZED) {
    *changed = true;  // existing != none because of previous check.
    return add;
  }

  // similar to before, false as null shouldn't modify types.
  if (add.kind == TP_Type::Kind::FALSE_AS_NULL) {
    *changed = false;
    return existing;
  }

  // replace any false as nulls.
  if (existing.kind == TP_Type::Kind::FALSE_AS_NULL) {
    *changed = true;  // existing != false because of previous check.
    return add;
  }

  // different values, but the same kind.
  if (existing.kind == add.kind) {
    switch (existing.kind) {
      case TP_Type::Kind::TYPESPEC: {
        auto new_result = TP_Type::make_from_ts(coerce_to_reg_type(ts.lowest_common_ancestor(
            existing.get_objects_typespec(), add.get_objects_typespec())));
        *changed = (new_result != existing);
        return new_result;
      }
      case TP_Type::Kind::TYPE_OF_TYPE_OR_CHILD: {
        auto new_result = TP_Type::make_type_allow_virtual_object(ts.lowest_common_ancestor(
            existing.get_type_objects_typespec(), add.get_type_objects_typespec()));
        *changed = (new_result != existing);
        return new_result;
      }

      case TP_Type::Kind::TYPE_OF_TYPE_NO_VIRTUAL: {
        auto new_result = TP_Type::make_type_no_virtual_object(ts.lowest_common_ancestor(
            existing.get_type_objects_typespec(), add.get_type_objects_typespec()));
        *changed = (new_result != existing);
        return new_result;
      }

      case TP_Type::Kind::PRODUCT_WITH_CONSTANT:
        // we know they are different.
        *changed = true;
        return TP_Type::make_from_ts(TypeSpec("int"));
      case TP_Type::Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT:
        *changed = true;
        // todo - there might be cases where we need to LCA the base types??
        return TP_Type::make_from_ts(TypeSpec("object"));
      case TP_Type::Kind::OBJECT_NEW_METHOD:
        *changed = true;
        // this case should never happen I think.
        return TP_Type::make_from_ts(TypeSpec("function"));
      case TP_Type::Kind::STRING_CONSTANT: {
        auto existing_count = get_format_arg_count(existing.get_string());
        auto added_count = get_format_arg_count(add.get_string());
        *changed = true;
        if (added_count == existing_count) {
          return TP_Type::make_from_format_string(existing_count);
        } else {
          return TP_Type::make_from_ts(TypeSpec("string"));
        }
      }
      case TP_Type::Kind::INTEGER_CONSTANT:
        *changed = true;
        return TP_Type::make_from_ts(TypeSpec("int"));
      case TP_Type::Kind::FORMAT_STRING:
        if (existing.get_format_string_arg_count() == add.get_format_string_arg_count()) {
          *changed = false;
          return existing;
        } else {
          *changed = true;
          return TP_Type::make_from_ts(TypeSpec("string"));
        }
      case TP_Type::Kind::INTEGER_CONSTANT_PLUS_VAR:
        if (existing.get_integer_constant() == add.get_integer_constant()) {
          auto new_t = coerce_to_reg_type(ts.lowest_common_ancestor(existing.get_objects_typespec(),
                                                                    add.get_objects_typespec()));
          auto new_child = TP_Type::make_from_integer_constant_plus_var(
              existing.get_integer_constant(), new_t, new_t);
          *changed = (new_child != existing);
          return new_child;
        } else {
          *changed = true;
          return TP_Type::make_from_ts("int");
        }

      case TP_Type::Kind::INTEGER_CONSTANT_PLUS_VAR_MULT:
        // a bit lazy here, but I don't think you can ever merge these.
        *changed = true;
        return TP_Type::make_from_ts("int");

      case TP_Type::Kind::VIRTUAL_METHOD:
        // never allow this to remain method
        *changed = true;
        return TP_Type::make_from_ts(
            ts.lowest_common_ancestor(existing.typespec(), add.typespec()));

      case TP_Type::Kind::NON_VIRTUAL_METHOD:
        // never allow this to remain method
        *changed = true;
        return TP_Type::make_from_ts(
            ts.lowest_common_ancestor(existing.typespec(), add.typespec()));

      case TP_Type::Kind::LABEL_ADDR:
        *changed = false;
        return existing;
      case TP_Type::Kind::SYMBOL:
        *changed = true;
        return TP_Type::make_from_ts("symbol");

      case TP_Type::Kind::FALSE_AS_NULL:
      case TP_Type::Kind::UNINITIALIZED:
      case TP_Type::Kind::DYNAMIC_METHOD_ACCESS:
      case TP_Type::Kind::INVALID:
      default:
        ASSERT(false);
        return {};
    }
  } else {
    // trying to combine two of different types.
    if (existing.can_be_format_string() && add.can_be_format_string()) {
      int existing_count = get_format_arg_count(existing);
      int add_count = get_format_arg_count(add);
      TP_Type result_type;
      if (existing_count == add_count) {
        result_type = TP_Type::make_from_format_string(existing_count);
      } else {
        result_type = TP_Type::make_from_ts(TypeSpec("string"));
      }

      *changed = (result_type != existing);
      return result_type;
    }

    if (existing.kind == TP_Type::Kind::TYPE_OF_TYPE_NO_VIRTUAL &&
        add.kind == TP_Type::Kind::TYPE_OF_TYPE_OR_CHILD) {
      auto result_type = TP_Type::make_type_no_virtual_object(ts.lowest_common_ancestor(
          existing.get_type_objects_typespec(), add.get_type_objects_typespec()));
      *changed = (result_type != existing);
      return result_type;
    }

    if (existing.kind == TP_Type::Kind::TYPE_OF_TYPE_OR_CHILD &&
        add.kind == TP_Type::Kind::TYPE_OF_TYPE_NO_VIRTUAL) {
      auto result_type = TP_Type::make_type_no_virtual_object(ts.lowest_common_ancestor(
          existing.get_type_objects_typespec(), add.get_type_objects_typespec()));
      *changed = (result_type != existing);
      return result_type;
    }

    // otherwise, as an absolute fallback, convert both to TypeSpecs and do TypeSpec LCA
    auto new_result = TP_Type::make_from_ts(
        coerce_to_reg_type(ts.lowest_common_ancestor(existing.typespec(), add.typespec())));
    *changed = (new_result != existing);
    return new_result;
  }
}

/*!
 * Find the least common ancestor of an entire typestate.
 */
bool DecompilerTypeSystem::tp_lca(TypeState* combined, const TypeState& add) {
  bool result = false;
  for (int i = 0; i < 32; i++) {
    bool diff = false;
    auto new_type = tp_lca(combined->gpr_types[i], add.gpr_types[i], &diff);
    if (diff) {
      result = true;
      combined->gpr_types[i] = new_type;
    }
  }

  for (int i = 0; i < 32; i++) {
    bool diff = false;
    auto new_type = tp_lca(combined->fpr_types[i], add.fpr_types[i], &diff);
    if (diff) {
      result = true;
      combined->fpr_types[i] = new_type;
    }
  }

  for (auto& x : add.spill_slots) {
    //    auto existing = combined->spill_slots.find(x.first);
    //    if (existing == combined->spill_slots.end()) {
    //      result = true;
    //      combined->spill_slots.insert({existing->first, existing->second});
    //    }
    bool diff = false;
    auto new_type = tp_lca(combined->spill_slots[x.first], x.second, &diff);
    if (diff) {
      result = true;
      combined->spill_slots[x.first] = new_type;
    }
  }

  bool diff = false;
  auto new_type = tp_lca(combined->next_state_type, add.next_state_type, &diff);
  if (diff) {
    result = true;
    combined->next_state_type = new_type;
  }

  return result;
}

int DecompilerTypeSystem::get_format_arg_count(const std::string& str) const {
  auto bad_it = bad_format_strings.find(str);
  if (bad_it != bad_format_strings.end()) {
    return bad_it->second;
  }

  static const std::vector<std::string> code_ignore_list = {
      "%",  "T",   "0L", "1L", "3L",   "1k",   "1K",   "2j", "0k",
      "0K", "30L", "1T", "2T", "100h", "200h", "350h", "t"};

  int arg_count = 0;
  for (size_t i = 0; i < str.length(); i++) {
    if (str.at(i) == '~') {
      i++;  // also eat the next character.

      bool code_takes_no_arg = false;
      for (auto& ignored_code : code_ignore_list) {
        size_t j = i;
        bool match = true;
        for (const char c : ignored_code) {
          if (j > str.length()) {
            match = false;
            break;
          }
          if (str.at(j) != c) {
            match = false;
            break;
          }
          j++;
        }
        if (match) {
          code_takes_no_arg = true;
          break;
        }
      }

      if (!code_takes_no_arg) {
        arg_count++;
      }
    }
  }
  return arg_count;
}

int DecompilerTypeSystem::get_format_arg_count(const TP_Type& type) const {
  if (type.is_constant_string()) {
    return get_format_arg_count(type.get_string());
  } else {
    return type.get_format_string_arg_count();
  }
}

int DecompilerTypeSystem::get_dynamic_format_arg_count(const std::string& func_name,
                                                       int op_idx) const {
  auto kv = format_ops_with_dynamic_string_by_func_name.find(func_name);
  if (kv == format_ops_with_dynamic_string_by_func_name.end()) {
    throw std::runtime_error(fmt::format("Unknown dynamic format string."));
  } else {
    auto& formats = kv->second;
    auto the_format =
        std::find_if(formats.begin(), formats.end(),
                     [op_idx](const std::vector<int> vec) { return vec.at(0) == op_idx; });
    if (the_format == formats.end()) {
      throw std::runtime_error(fmt::format("Unknown dynamic format string."));
    }
    return the_format->at(1);
  }
}

TypeSpec DecompilerTypeSystem::lookup_symbol_type(const std::string& name) const {
  auto kv = symbol_types.find(name);
  if (kv == symbol_types.end()) {
    throw std::runtime_error(
        fmt::format("Decompiler type system did not know the type of symbol {}. Add it!", name));
  } else {
    return kv->second;
  }
}

bool DecompilerTypeSystem::should_attempt_cast_simplify(const TypeSpec& expected,
                                                        const TypeSpec& actual) const {
  if (expected == TypeSpec("meters") && actual == TypeSpec("float")) {
    return true;
  }

  if (expected == TypeSpec("seconds") && actual == TypeSpec("int64")) {
    return true;
  }

  if (expected == TypeSpec("degrees") && actual == TypeSpec("float")) {
    return true;
  }

  return !ts.tc(expected, actual);
}
}  // namespace decompiler
