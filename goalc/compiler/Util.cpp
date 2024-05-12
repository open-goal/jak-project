#include "common/goos/ParseHelpers.h"
#include "common/type_system/deftype.h"
#include "common/util/json_util.h"
#include "common/util/string_util.h"

#include "goalc/compiler/Compiler.h"
#include "goalc/compiler/IR.h"

void Compiler::save_repl_history() {
  m_repl->save_history();
}

void Compiler::print_to_repl(const std::string& str) {
  m_repl->print_to_repl(str);
}

std::string Compiler::get_prompt() {
  std::string prompt = fmt::format(fmt::emphasis::bold | fg(fmt::color::cyan), "g > ");
  if (m_listener.is_connected()) {
    prompt = fmt::format(fmt::emphasis::bold | fg(fmt::color::lime_green), "gc> ");
  }
  if (m_debugger.is_halted()) {
    prompt = fmt::format(fmt::emphasis::bold | fg(fmt::color::magenta), "gs> ");
  } else if (m_debugger.is_attached()) {
    prompt = fmt::format(fmt::emphasis::bold | fg(fmt::color::red), "gr> ");
  }
  return "\033[0m" + prompt;
}

std::string Compiler::get_repl_input() {
  auto str = m_repl->readline(get_prompt());
  if (str) {
    m_repl->add_to_history(str);
    return str;
  } else {
    return "";
  }
}

void Compiler::compile_and_send_from_string(const std::string& source_code) {
  if (!connect_to_target()) {
    throw std::runtime_error(
        "Compiler failed to connect to target for compile_and_send_from_string.");
  }

  auto code = m_goos.reader.read_from_string(source_code);
  auto compiled = compile_object_file("test-code", code, true);
  ASSERT(!compiled->is_empty());
  color_object_file(compiled);
  auto data = codegen_object_file(compiled);
  m_listener.send_code(data);
  if (!m_listener.most_recent_send_was_acked()) {
    print_compiler_warning("Runtime is not responding after sending test code. Did it crash?\n");
  }
}

std::vector<std::string> Compiler::run_test_from_file(const std::string& source_code) {
  try {
    if (!connect_to_target()) {
      throw std::runtime_error("Compiler::run_test_from_file couldn't connect!");
    }

    auto code = m_goos.reader.read_from_file({source_code});
    auto compiled = compile_object_file("test-code", code, true);
    if (compiled->is_empty()) {
      return {};
    }
    color_object_file(compiled);
    auto data = codegen_object_file(compiled);
    m_listener.record_messages(ListenerMessageKind::MSG_PRINT);
    m_listener.send_code(data);
    if (!m_listener.most_recent_send_was_acked()) {
      print_compiler_warning("Runtime is not responding after sending test code. Did it crash?\n");
    }
    return m_listener.stop_recording_messages();
  } catch (std::exception& e) {
    lg::print("[Compiler] Failed to compile test program {}: {}\n", source_code, e.what());
    throw e;
  }
}

std::vector<std::string> Compiler::run_test_from_string(const std::string& src,
                                                        const std::string& obj_name) {
  try {
    if (!connect_to_target()) {
      throw std::runtime_error("Compiler::run_test_from_file couldn't connect!");
    }

    auto code = m_goos.reader.read_from_string({src});
    auto compiled = compile_object_file(obj_name, code, true);
    if (compiled->is_empty()) {
      return {};
    }
    color_object_file(compiled);
    auto data = codegen_object_file(compiled);
    m_listener.record_messages(ListenerMessageKind::MSG_PRINT);
    m_listener.send_code(data);
    if (!m_listener.most_recent_send_was_acked()) {
      print_compiler_warning("Runtime is not responding after sending test code. Did it crash?\n");
    }
    return m_listener.stop_recording_messages();
  } catch (std::exception& e) {
    lg::print("[Compiler] Failed to compile test program from string {}: {}\n", src, e.what());
    throw e;
  }
}

/*!
 * Just run the front end on a string. Will not do register allocation or code generation.
 * Useful for typechecking, defining types,  or running strings that invoke the compiler again.
 */
void Compiler::run_front_end_on_string(const std::string& src) {
  auto code = m_goos.reader.read_from_string({src});
  compile_object_file("run-on-string", code, true);
}

/*!
 * Just run the front end on a file. Will not do register allocation or code generation.
 * Useful for typechecking, defining types,  or running strings that invoke the compiler again.
 */
void Compiler::run_front_end_on_file(const std::vector<std::string>& path) {
  auto code = m_goos.reader.read_from_file(path);
  compile_object_file("run-on-file", code, true);
}

/*!
 * Run the entire compilation process on the input source code. Will generate an object file, but
 * won't save it anywhere.
 */
void Compiler::run_full_compiler_on_string_no_save(const std::string& src,
                                                   const std::optional<std::string>& string_name) {
  auto code = m_goos.reader.read_from_string(src, true, string_name);
  auto compiled = compile_object_file("run-on-string", code, true);
  color_object_file(compiled);
  codegen_object_file(compiled);
}

std::vector<std::string> Compiler::run_test_no_load(const std::string& source_code) {
  auto code = m_goos.reader.read_from_file({source_code});
  compile_object_file("test-code", code, true);
  return {};
}

void Compiler::shutdown_target() {
  if (m_debugger.is_attached()) {
    m_debugger.detach();
  }

  if (m_listener.is_connected()) {
    m_listener.send_reset(true);
  }
}

bool Compiler::knows_object_file(const std::string& name) {
  return m_debugger.knows_object(name);
}

/*!
 * Parse arguments into a goos::Arguments format.
 */
goos::Arguments Compiler::get_va(const goos::Object& form, const goos::Object& rest) {
  goos::Arguments args;

  std::string err;
  if (!goos::get_va(rest, &err, &args)) {
    throw_compiler_error(form, "{}", err);
  }
  return args;
}

/*!
 * Parse arguments into a goos::Arguments format.
 */
goos::Arguments Compiler::get_va_no_named(const goos::Object& form, const goos::Object& rest) {
  (void)form;
  goos::Arguments args;
  goos::get_va_no_named(rest, &args);
  return args;
}

/*!
 * Check arguments in a goos::Arguments format (named and unnamed) and throw a compiler error if it
 * fails.
 */
void Compiler::va_check(
    const goos::Object& form,
    const goos::Arguments& args,
    const std::vector<std::optional<goos::ObjectType>>& unnamed,
    const std::unordered_map<std::string, std::pair<bool, std::optional<goos::ObjectType>>>&
        named) {
  std::string err;
  if (!goos::va_check(args, unnamed, named, &err)) {
    throw_compiler_error(form, "{}", err);
  }
}

/*!
 * Iterate through elements of a goos list and apply the given function. Throw compiler error if the
 * list is invalid.
 */
void Compiler::for_each_in_list(const goos::Object& list,
                                const std::function<void(const goos::Object&)>& f) {
  const goos::Object* iter = &list;
  while (iter->is_pair()) {
    auto lap = iter->as_pair();
    f(lap->car);
    iter = &lap->cdr;
  }

  if (!iter->is_empty_list()) {
    throw_compiler_error(list, "Invalid list: {}", list.print());
  }
}

/*!
 * Convert a goos::Object that's a string to a std::string. Must be a string.
 */
const std::string& Compiler::as_string(const goos::Object& o) {
  return o.as_string()->data;
}

/*!
 * Convert a goos::Object that's a symbol to a std::string. Must be a symbol.
 */
std::string Compiler::symbol_string(const goos::Object& o) {
  return o.as_symbol().name_ptr;
}

/*!
 * Convert a single quoted symbol into a std::string. Like 'hi -> "hi". Error if not a quoted
 * symbol.
 */
std::string Compiler::quoted_sym_as_string(const goos::Object& o) {
  auto args = get_va(o, o);
  va_check(o, args, {{goos::ObjectType::SYMBOL}, {goos::ObjectType::SYMBOL}}, {});
  if (symbol_string(args.unnamed.at(0)) != "quote") {
    throw_compiler_error(o, "Invalid quoted symbol: {}.", o.print());
  }
  return symbol_string(args.unnamed.at(1));
}

/*!
 * Get a thing that's quoted. Error if the thing isn't quoted.
 */
goos::Object Compiler::unquote(const goos::Object& o) {
  auto args = get_va(o, o);
  va_check(o, args, {{goos::ObjectType::SYMBOL}, {}}, {});
  if (symbol_string(args.unnamed.at(0)) != "quote") {
    throw_compiler_error(o, "Invalid quoted symbol: {}.", o.print());
  }
  return args.unnamed.at(1);
}

/*!
 * Determine if o is a quoted symbol like 'test.
 */
bool Compiler::is_quoted_sym(const goos::Object& o) {
  if (o.is_pair()) {
    auto car = pair_car(o);
    auto cdr = pair_cdr(o);
    if (car.is_symbol() && car.as_symbol() == "quote") {
      if (cdr.is_pair()) {
        auto thing = pair_car(cdr);
        if (thing.is_symbol()) {
          if (pair_cdr(cdr).is_empty_list()) {
            return true;
          }
        }
      }
    }
  }
  return false;
}

const goos::Object& Compiler::pair_car(const goos::Object& o) {
  return o.as_pair()->car;
}

const goos::Object& Compiler::pair_cdr(const goos::Object& o) {
  return o.as_pair()->cdr;
}

void Compiler::expect_empty_list(const goos::Object& o) {
  if (!o.is_empty_list()) {
    throw_compiler_error(o, "expected to be an empty list");
  }
}

TypeSpec Compiler::parse_typespec(const goos::Object& src, Env* env) {
  if (src.is_pair() && src.as_pair()->car.is_symbol("current-method-type") &&
      src.as_pair()->cdr.is_empty_list()) {
    return env->function_env()->method_of_type_name;
  }
  if (src.is_pair() && src.as_pair()->car.is_symbol("current-method-function-type") &&
      src.as_pair()->cdr.is_empty_list()) {
    return env->function_env()->method_function_type.substitute_for_method_call(
        env->function_env()->method_of_type_name);
  }
  if (m_settings.check_for_requires) {
    TypeSpec ts = ::parse_typespec(&m_ts, src);
    const auto& type_name = ts.base_type();
    if (!type_name.empty()) {
      const auto& symbol_info = m_symbol_info.lookup_exact_name(type_name);
      if (!symbol_info.empty()) {
        const auto& result = symbol_info.at(0);
        if (result->m_def_location.has_value() &&
            !env->file_env()->m_missing_required_files.contains(
                result->m_def_location->file_path) &&
            env->file_env()->m_required_files.find(result->m_def_location->file_path) ==
                env->file_env()->m_required_files.end() &&
            !str_util::ends_with(result->m_def_location->file_path,
                                 env->file_env()->name() + ".gc")) {
          lg::warn("Missing require in {} for {} over {}", env->file_env()->name(),
                   result->m_def_location->file_path, type_name);
          env->file_env()->m_missing_required_files.insert(result->m_def_location->file_path);
        }
      }
    }
  }

  return ::parse_typespec(&m_ts, src);
}

bool Compiler::is_local_symbol(const goos::Object& obj, Env* env) {
  // check in the symbol macro env.
  auto mlet_env = env->symbol_macro_env();
  while (mlet_env) {
    if (mlet_env->macros.find(obj.as_symbol()) != mlet_env->macros.end()) {
      return true;
    }
    mlet_env = mlet_env->parent()->symbol_macro_env();
  }

  // check lexical
  if (env->lexical_lookup(obj)) {
    return true;
  }

  // check global constants
  if (m_global_constants.lookup(obj.as_symbol())) {
    return true;
  }

  return false;
}

bool Compiler::is_none(Val* in) {
  return dynamic_cast<None*>(in);
}

bool Compiler::is_basic(const TypeSpec& ts) {
  return m_ts.tc(m_ts.make_typespec("basic"), ts);
}

bool Compiler::is_structure(const TypeSpec& ts) {
  return m_ts.tc(m_ts.make_typespec("structure"), ts);
}

bool Compiler::is_bitfield(const TypeSpec& ts) {
  return m_ts.is_bitfield_type(ts.base_type());
}

bool Compiler::is_pair(const TypeSpec& ts) {
  return m_ts.tc(m_ts.make_typespec("pair"), ts);
}

bool Compiler::is_symbol(const TypeSpec& ts) {
  return m_ts.tc(m_ts.make_typespec("symbol"), ts);
}

bool Compiler::get_true_or_false(const goos::Object& form, const goos::Object& boolean) {
  // todo try other things.
  if (boolean.is_symbol()) {
    if (boolean.as_symbol() == "#t") {
      return true;
    }
    if (boolean.as_symbol() == "#f") {
      return false;
    }
  }
  throw_compiler_error(form, "The value {} cannot be used as a boolean.", boolean.print());
  return false;
}

std::vector<goos::Object> Compiler::get_list_as_vector(const goos::Object& o,
                                                       goos::Object* rest_out,
                                                       int max_length) {
  std::vector<goos::Object> result;

  auto* cur = &o;
  int n = 0;
  while (true) {
    if (max_length >= 0 && n >= max_length) {
      if (rest_out) {
        *rest_out = *cur;
      } else {
        throw std::runtime_error("get_list_as_vector would discard arguments");
      }
      return result;
    }

    if (cur->is_pair()) {
      result.push_back(cur->as_pair()->car);
      cur = &cur->as_pair()->cdr;
      n++;
    } else if (cur->is_empty_list()) {
      if (rest_out) {
        *rest_out = goos::Object::make_empty_list();
      }
      return result;
    }
  }
}

void Compiler::compile_constant_product(const goos::Object& form,
                                        RegVal* dest,
                                        RegVal* src,
                                        int stride,
                                        Env* env) {
  // todo - support imul with an imm.
  ASSERT(stride);

  bool is_power_of_two = (stride & (stride - 1)) == 0;
  if (stride == 1) {
    env->emit_ir<IR_RegSet>(form, dest, src);
  } else if (is_power_of_two) {
    for (int i = 0; i < 16; i++) {
      if (stride == (1 << i)) {
        env->emit_ir<IR_RegSet>(form, dest, src);
        env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::SHL_64, dest, i);
        return;
      }
    }
    ASSERT(false);
  } else {
    // get the multiplier
    env->emit_ir<IR_LoadConstant64>(form, dest, stride);
    env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::IMUL_32, dest, src);
  }
}
