#include "symbol_def_map.h"

#include "common/link_types.h"
#include "common/log/log.h"

#include "decompiler/ObjectFile/ObjectFileDB.h"

#include "third-party/json.hpp"

namespace decompiler {

void SymbolMapBuilder::add_object(const ObjectFileData& data) {
  // skip non-code files
  if (data.obj_version != 3) {
    return;
  }
  m_first_detections.emplace_back();
  m_first_detections.back().object_file_name = data.name_from_map;
  // add load/stores from all functions
  for (const auto& seg_functions : data.linked_data.functions_by_seg) {
    for (const auto& function : seg_functions) {
      add_load_store_from_function(function, &m_first_detections.back());
    }
  }

  // add deftypes in the top level function
  const auto& top_level_functions = data.linked_data.functions_by_seg.at(TOP_LEVEL_SEGMENT);
  ASSERT(top_level_functions.size() == 1);
  add_deftypes_from_top_level_function(top_level_functions.at(0), &m_first_detections.back());
}

void SymbolMapBuilder::build_map() {
  // build a map where each symbol appears only once.  If a symbol appears as both a type and a
  // load/store, then the load/store will be removed.
  m_result.clear();
  for (auto& obj_info : m_first_detections) {
    ObjectSymbolList result;
    result.object_file_name = obj_info.object_file_name;

    for (auto& sym_info : obj_info.symbols) {
      if (sym_info.is_type ||
          (!sym_info.is_type && (m_seen_types.find(sym_info.name) == m_seen_types.end()))) {
        result.symbols.push_back(sym_info);
      }
    }
    m_result.push_back(result);
  }
}

std::string SymbolMapBuilder::convert_to_json() const {
  nlohmann::json result;

  for (const auto& file : m_result) {
    if (file.symbols.empty()) {
      continue;
    }
    nlohmann::json syms;
    for (const auto& sym : file.symbols) {
      syms.push_back(sym.name);
    }
    result[file.object_file_name] = syms;
  }

  // adding the 4 here should make it pretty print
  return result.dump(4);
}

namespace {
std::optional<std::string> get_loaded_or_stored_symbol_name(const AtomicOp* op) {
  // look for (set! <blah> <SYMBOL>)
  auto as_set = dynamic_cast<const SetVarOp*>(op);
  if (as_set) {
    // source is a single thing:
    if (as_set->src().is_identity()) {
      const auto& src_atom = as_set->src().get_arg(0);
      // sym val means loading the value in the symbol
      if (src_atom.is_sym_val()) {
        return src_atom.get_str();
      }
    }
  }

  auto as_store = dynamic_cast<const StoreOp*>(op);
  if (as_store) {
    if (as_store->addr().is_identity()) {
      const auto& src_atom = as_store->addr().get_arg(0);
      if (src_atom.is_sym_val()) {
        return src_atom.get_str();
      }
    }
  }

  return std::nullopt;
}
}  // namespace

void SymbolMapBuilder::add_load_store_from_function(const Function& f, ObjectSymbolList* output) {
  if (!f.ir2.atomic_ops_succeeded) {
    if (!f.suspected_asm) {
      // some asm functions will use mips2c which doesn't require atomic ops.
      // we can't do anything with these, but we shouldn't warn.
      lg::error("Atomic ops failed in {}", f.name());
    }

    return;
  }

  for (const auto& op : f.ir2.atomic_ops->ops) {
    const auto sym = get_loaded_or_stored_symbol_name(op.get());
    if (sym) {
      if (m_seen_symbols.find(*sym) == m_seen_symbols.end()) {
        SymbolInfo info;
        info.name = *sym;
        info.is_type = false;
        output->symbols.push_back(info);
        m_seen_symbols.insert(*sym);
      }
    }
  }
}

void SymbolMapBuilder::add_deftypes_from_top_level_function(const Function& f,
                                                            ObjectSymbolList* output) {
  for (const auto& name : f.types_defined) {
    if (m_seen_types.find(name) == m_seen_types.end()) {
      SymbolInfo info;
      info.name = name;
      info.is_type = true;
      output->symbols.push_back(info);
      m_seen_types.insert(name);
    }
  }
}

}  // namespace decompiler
