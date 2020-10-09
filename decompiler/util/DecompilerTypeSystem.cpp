#include "DecompilerTypeSystem.h"
#include "common/goos/Reader.h"
#include "common/type_system/deftype.h"

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
  goos::Reader reader;
  auto read = reader.read_from_file(file_path);
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
    } else {
      throw std::runtime_error("Decompiler cannot parse " + car(o).print());
    }
  });
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