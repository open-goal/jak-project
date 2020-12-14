#include "DecompilerTypeSystem.h"
#include "common/goos/Reader.h"
#include "common/type_system/deftype.h"
#include "decompiler/Disasm/Register.h"
#include "third-party/spdlog/include/spdlog/spdlog.h"
#include "TP_Type.h"

DecompilerTypeSystem::DecompilerTypeSystem() {
  ts.add_builtin_types();
}

namespace {
// some utilities for parsing the type def file

goos::Object& car(goos::Object& pair) {
  if (pair.is_pair()) {
    return pair.as_pair()->car;
  } else {
    throw std::runtime_error("car called on something that wasn't a pair: " + pair.print());
  }
}

goos::Object& cdr(goos::Object& pair) {
  if (pair.is_pair()) {
    return pair.as_pair()->cdr;
  } else {
    throw std::runtime_error("cdr called on something that wasn't a pair");
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
  auto data = cdr(read);

  for_each_in_list(data, [&](goos::Object& o) {
    if (car(o).as_symbol()->name == "define-extern") {
      auto* rest = &cdr(o);
      auto sym_name = car(*rest);
      rest = &cdr(*rest);
      auto sym_type = car(*rest);
      if (!cdr(*rest).is_empty_list()) {
        throw std::runtime_error("malformed define-extern");
      }
      add_symbol(sym_name.as_symbol()->name, parse_typespec(&ts, sym_type));

    } else if (car(o).as_symbol()->name == "deftype") {
      parse_deftype(cdr(o), &ts);
    } else if (car(o).as_symbol()->name == "declare-type") {
      auto* rest = &cdr(o);
      auto type_name = car(*rest);
      rest = &cdr(*rest);
      auto type_kind = car(*rest);
      if (!cdr(*rest).is_empty_list()) {
        throw std::runtime_error("malformed declare-type");
      }
      if (type_kind.as_symbol()->name == "basic") {
        ts.forward_declare_type_as_basic(type_name.as_symbol()->name);
      } else if (type_kind.as_symbol()->name == "structure") {
        ts.forward_declare_type_as_structure(type_name.as_symbol()->name);
      } else {
        throw std::runtime_error("bad declare-type");
      }
    } else {
      throw std::runtime_error("Decompiler cannot parse " + car(o).print());
    }
  });
}

TypeSpec DecompilerTypeSystem::parse_type_spec(const std::string& str) {
  auto read = m_reader.read_from_string(str);
  auto data = cdr(read);
  return parse_typespec(&ts, data);
}

std::string DecompilerTypeSystem::dump_symbol_types() {
  assert(symbol_add_order.size() == symbols.size());
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
    spdlog::warn("duplicated type flags for {}, was 0x{:x}, now 0x{:x}", name.c_str(), kv->second,
                 flags);
    if (kv->second != flags) {
      spdlog::warn("duplicated type flags that are inconsistent!");
    }
  }
  type_flags[name] = flags;
}

void DecompilerTypeSystem::add_type_parent(const std::string& child, const std::string& parent) {
  auto kv = type_parents.find(child);
  if (kv != type_parents.end()) {
    spdlog::warn("duplicated type parents for {} was {} now {}", child.c_str(), kv->second.c_str(),
                 parent.c_str());
    if (kv->second != parent) {
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

void DecompilerTypeSystem::add_symbol(const std::string& name, const TypeSpec& type_spec) {
  add_symbol(name);
  auto skv = symbol_types.find(name);
  if (skv == symbol_types.end() || skv->second == type_spec) {
    symbol_types[name] = type_spec;
  } else {
    if (ts.typecheck(type_spec, skv->second, "", false, false)) {
    } else {
      spdlog::warn("Attempting to redefine type of symbol {} from {} to {}\n", name,
                   skv->second.print(), type_spec.print());
      throw std::runtime_error("Type redefinition");
    }
  }
}

/*!
 * Compute the least common ancestor of two TP Types.
 */
TP_Type DecompilerTypeSystem::tp_lca(const TP_Type& existing, const TP_Type& add, bool* changed) {
  // starting from most vague to most specific

  // simplist case, no difference.
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
        auto new_result = TP_Type::make_from_typespec(coerce_to_reg_type(ts.lowest_common_ancestor(
            existing.get_objects_typespec(), add.get_objects_typespec())));
        *changed = (new_result != existing);
        return new_result;
      }
      case TP_Type::Kind::TYPE_OF_TYPE_OR_CHILD: {
        auto new_result = TP_Type::make_type_object(ts.lowest_common_ancestor(
            existing.get_type_objects_typespec(), add.get_type_objects_typespec()));
        *changed = (new_result != existing);
        return new_result;
      }

      case TP_Type::Kind::PRODUCT_WITH_CONSTANT:
        // we know they are different.
        *changed = true;
        return TP_Type::make_from_typespec(TypeSpec("int"));
      case TP_Type::Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT:
        *changed = true;
        // todo - there might be cases where we need to LCA the base types??
        return TP_Type::make_from_typespec(TypeSpec("object"));
      case TP_Type::Kind::OBJECT_NEW_METHOD:
        *changed = true;
        // this case should never happen I think.
        return TP_Type::make_from_typespec(TypeSpec("function"));
      case TP_Type::Kind::STRING_CONSTANT:
        *changed = true;
        return TP_Type::make_from_typespec(TypeSpec("string"));
      case TP_Type::Kind::INTEGER_CONSTANT:
        *changed = true;
        return TP_Type::make_from_typespec(TypeSpec("int"));

      case TP_Type::Kind::FALSE_AS_NULL:
      case TP_Type::Kind::UNINITIALIZED:
      case TP_Type::Kind::DYNAMIC_METHOD_ACCESS:
      case TP_Type::Kind::INVALID:
      default:
        assert(false);
    }
  } else {
    // trying to combine two of different types.
    // if we find any special cases we can add it here.
    // otherwise, as an absolute fallback, convert both to TypeSpecs and do TypeSpec LCA
    auto new_result =
        TP_Type::make_from_typespec(ts.lowest_common_ancestor(existing.typespec(), add.typespec()));
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

  return result;
}
