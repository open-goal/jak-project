#include <logger/Logger.h>
#include "Goal.h"

/*!
 * Helper to iterate through a Goos list.
 */
void Goal::for_each_in_list(Object list, const std::function<void(Object)>& f) {
  while (list.type == PAIR) {
    auto lap = list.as_pair();
    f(lap->car);
    list = lap->cdr;
  }

  if (list.type != EMPTY_LIST) {
    throw_compile_error(list, "invalid list in for_each_in_list");
  }
}

/*!
 * Get the length of a list.
 */
int Goal::list_length(Object list) {
  int l = 0;
  for_each_in_list(list, [&](Object o) {
    (void)o;
    l++;
  });
  return l;
}

SymbolTable& Goal::get_symbol_table() {
  return goos.reader.symbolTable;
}

std::shared_ptr<StringObject> Goal::as_string_obj(Object obj) {
  if (obj.type != STRING) {
    throw_compile_error(obj, "expected " + obj.print() + " to be a string.");
  }
  return obj.as_string();
}

std::string Goal::quoted_sym_as_string(Object obj) {
  auto expected_quote = pair_car(obj);
  if (symbol_string(expected_quote) != "quote") {
    throw_compile_error(obj, "expected to be a quoted symbol");
  }

  obj = pair_cdr(obj);
  auto item = pair_car(obj);
  expect_empty_list(pair_cdr(obj));

  return symbol_string(item);
}

std::string Goal::as_string(Object obj) {
  return as_string_obj(obj)->data;
}

std::vector<std::string> Goal::as_string_list(Object obj) {
  std::vector<std::string> result;
  while (obj.type != ObjectType::EMPTY_LIST) {
    result.push_back(as_string(pair_car(obj)));
    obj = pair_cdr(obj);
  }
  return result;
}

std::shared_ptr<PairObject> Goal::as_pair_obj(Object obj) {
  if (obj.type != PAIR) {
    throw_compile_error(obj, "expected to be a pair: " + obj.print());
  }
  return obj.as_pair();
}

Object Goal::pair_car(Object obj) {
  return as_pair_obj(obj)->car;
}

Object Goal::pair_cdr(Object obj) {
  return as_pair_obj(obj)->cdr;
}

void Goal::expect_empty_list(Object obj) {
  if (obj.type != EMPTY_LIST) {
    throw_compile_error(obj, "expected to be an empty list");
  }
}

std::shared_ptr<SymbolObject> Goal::as_symbol_obj(Object obj) {
  if (obj.type != SYMBOL) {
    throw_compile_error(obj, "expected to be be a symbol: " + obj.print());
  }
  return obj.as_symbol();
}

std::string Goal::symbol_string(Object obj) {
  return as_symbol_obj(obj)->name;
}

/*!
 * Get a constant named `name`, or throw a compile error on error_form.
 */
Object Goal::get_constant_or_error(Object error_form, const std::string& name) {
  auto kv = global_constants.find(SymbolObject::make_new(get_symbol_table(), name).as_symbol());
  if (kv == global_constants.end()) {
    throw_compile_error(error_form, "failed to find constant named " + name);
  }
  return kv->second;
}

bool Goal::write_to_binary_file(const std::string& name, void* data, uint32_t size) {
  FILE* fp = fopen(name.c_str(), "wb");
  if (!fp) {
    return false;
  }

  if (fwrite(data, size, 1, fp) != 1) {
    return false;
  }

  fclose(fp);

  return true;
}

Object Goal::read_from_file(const std::string& file_name) {
  return goos.reader.read_from_file(file_name);
}

Object Goal::read_from_stdin_prompt(const std::string& prompt_name) {
  return goos.reader.read_from_stdin(prompt_name);
}

Object Goal::read_from_string(const std::string& str) {
  return goos.reader.read_from_string(str);
}

#include "DefaultConfig.h"

void Goal::setup_default_config() {
  for (auto& e : default_config) {
    auto read = read_from_string(e.second);
    // read will be in the form (top-level <stuff>)
    read = pair_cdr(read);
    if (pair_cdr(read).type != EMPTY_LIST) {
      ice("The default for configuration option \"" + e.first +
          "\" is invalid.  Check DefaultConfig.h");
    }
    set_config(e.first, pair_car(read));
  }
}

Object Goal::get_config(const std::string& name) {
  auto kv = config_data.find(name);
  if (kv == config_data.end()) {
    ice("No compiler configuration entry named " + name + " could be found. Check DefaultConfig.h");
  }
  return kv->second.value;
}

void Goal::ice(const std::string& error) {
  gLogger.log(MSG_ICE, "[ICE] %s\n", error.c_str());
  throw std::runtime_error("ICE");
}

void Goal::set_config(const std::string& name, const Object& value) {
  config_data[name] = {name, value};
}

std::shared_ptr<Place> Goal::compile_set_config(const Object& form,
                                                Object rest,
                                                std::shared_ptr<GoalEnv> env) {
  (void)form;
  (void)env;
  auto name = symbol_string(pair_car(rest));
  rest = pair_cdr(rest);
  auto value = pair_car(rest);
  expect_empty_list(pair_cdr(rest));

  set_config(name, value);

  return get_none();
}