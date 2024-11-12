/*!
 * @file CompilerControl.cpp
 * Compiler implementation for forms which actually control the compiler.
 */

#include <regex>
#include <stack>

#include "common/repl/repl_wrapper.h"
#include "common/util/DgoWriter.h"
#include "common/util/FileUtil.h"
#include "common/util/Timer.h"
#include "common/util/string_util.h"

#include "goalc/compiler/Compiler.h"
#include "goalc/compiler/IR.h"
#include "goalc/compiler/docs/DocTypes.h"
#include "goalc/compiler/symbol_info.h"
#include "goalc/data_compiler/dir_tpages.h"
#include "goalc/data_compiler/game_count.h"
#include "goalc/data_compiler/game_text_common.h"
/*!
 * Exit the compiler. Disconnects the listener and tells the target to reset itself.
 * Will actually exit the next time the REPL runs.
 */
Val* Compiler::compile_exit(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {}, {});

  if (m_debugger.is_attached()) {
    m_debugger.detach();
  }

  if (m_listener.is_connected()) {
    m_listener.send_reset(false);
  }
  // flag for the REPL.
  m_want_exit = true;
  if (m_repl) {
    m_repl->save_history();
  }
  return get_none();
}

/*!
 * Evaluate GOOS code. It's not possible to get the result, so this is really only useful to get
 * a side effect. Used to bootstrap the GOAL/GOOS macro system.
 */
Val* Compiler::compile_seval(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  try {
    for_each_in_list(rest, [&](const goos::Object& o) {
      m_goos.eval_with_rewind(o, m_goos.global_environment.as_env_ptr());
    });
  } catch (std::runtime_error& e) {
    throw_compiler_error(form, "Error while evaluating GOOS: {}", e.what());
  }
  return get_none();
}

/*!
 * Compile a "data file"
 */
Val* Compiler::compile_asm_data_file(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::SYMBOL, goos::ObjectType::STRING}, {});
  auto kind = symbol_string(args.unnamed.at(0));
  if (kind == "game-count") {
    compile_game_count(as_string(args.unnamed.at(1)), m_make.compiler_output_prefix());
  } else if (kind == "dir-tpages") {
    compile_dir_tpages(as_string(args.unnamed.at(1)), m_make.compiler_output_prefix());
  } else {
    throw_compiler_error(form, "The option {} was not recognized for asm-data-file.", kind);
  }
  return get_none();
}

/*!
 * Compile a "text data file"
 */
Val* Compiler::compile_asm_text_file(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::SYMBOL}, {{"files", {true, goos::ObjectType::PAIR}}});

  // what kind of text file?
  const auto kind = symbol_string(args.unnamed.at(0));

  // compile files.
  if (kind == "subtitle" || kind == "subtitle-v2") {
    std::vector<GameSubtitleDefinitionFile> inputs;
    // open all project files specified (usually one).
    for_each_in_list(args.named.at("files"), [this, &inputs, &form, &kind](const goos::Object& o) {
      if (o.is_string()) {
        open_subtitle_project(kind, o.as_string()->data, inputs);
      } else {
        throw_compiler_error(form, "Invalid object {} in asm-text-file files list.", o.print());
      }
    });
    GameSubtitleDB db;
    if (kind == "subtitle") {
    } else {
      db.m_subtitle_version = GameSubtitleDB::SubtitleFormat::V2;
    }
    compile_game_subtitles(inputs, db, m_make.compiler_output_prefix());
  } else if (kind == "text") {
    std::vector<GameTextDefinitionFile> inputs;
    // open all project files specified (usually one).
    for_each_in_list(args.named.at("files"), [this, &inputs, &form, &kind](const goos::Object& o) {
      if (o.is_string()) {
        open_text_project(kind, o.as_string()->data, inputs);
      } else {
        throw_compiler_error(form, "Invalid object {} in asm-text-file files list.", o.print());
      }
    });
    GameTextDB db;
    compile_game_text(inputs, db, m_make.compiler_output_prefix());
  } else {
    throw_compiler_error(form, "The option {} was not recognized for asm-text-file.", kind);
  }
  return get_none();
}

/*!
 * Compile a file, and optionally color, save, or load.
 * This should only be used for v3 "code object" files.
 */
Val* Compiler::compile_asm_file(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  int i = 0;
  CompilationOptions options;
  bool no_throw = false;

  // parse arguments
  bool last_was_disasm = false;
  for_each_in_list(rest, [&](const goos::Object& o) {
    if (last_was_disasm) {
      last_was_disasm = false;
      if (o.type == goos::ObjectType::STRING) {
        options.disassembly_output_file = as_string(o);
        i++;
        return;
      }
    }
    if (i == 0) {
      options.filename = as_string(o);
    } else {
      auto setting = symbol_string(o);
      if (setting == ":load") {
        options.load = true;
      } else if (setting == ":color") {
        options.color = true;
      } else if (setting == ":write") {
        options.write = true;
      } else if (setting == ":no-code") {
        options.no_code = true;
      } else if (setting == ":no-throw") {
        no_throw = true;
      } else if (setting == ":disassemble") {
        options.disassemble = true;
        last_was_disasm = true;
      } else if (setting == ":disasm-code-only") {
        options.disasm_code_only = true;
      } else {
        throw_compiler_error(form, "The option {} was not recognized for asm-file.", setting);
      }
    }
    i++;
  });

  try {
    asm_file(options);
  } catch (std::runtime_error& e) {
    if (!no_throw) {
      throw_compiler_error(form, "Error while compiling file: {}", e.what());
    }
  }

  return get_none();
}

/*!
 * Simple help / documentation command
 */
Val* Compiler::compile_repl_help(const goos::Object&, const goos::Object&, Env*) {
  m_repl.get()->print_help_message();
  return get_none();
}

/*!
 * Print out all set keybinds for the REPL (by our tooling)
 */
Val* Compiler::compile_repl_keybinds(const goos::Object&, const goos::Object&, Env*) {
  m_repl.get()->print_keybind_help();
  return get_none();
}

/*!
 * Connect the compiler to a target. Takes an optional IP address / port, defaults to
 * 127.0.0.1 and 8112, which is the local computer and the default port for the DECI2 over IP
 * implementation.
 */
Val* Compiler::compile_listen_to_target(const goos::Object& form,
                                        const goos::Object& rest,
                                        Env* env) {
  (void)env;
  std::string ip = "127.0.0.1";
  int port = -1;
  bool got_port = false, got_ip = false;

  for_each_in_list(rest, [&](const goos::Object& o) {
    if (o.is_string()) {
      if (got_ip) {
        throw_compiler_error(form, "listen-to-target can only use 1 IP address");
      }
      got_ip = true;
      ip = o.as_string()->data;
    } else if (o.is_int()) {
      if (got_port) {
        throw_compiler_error(form, "listen-to-target can only use 1 port number");
      }
      got_port = true;
      port = o.integer_obj.value;
    } else {
      throw_compiler_error(form, "invalid argument to listen-to-target: \"{}\"", o.print());
    }
  });

  int retries = 30;
  if (m_repl) {
    retries = m_repl->repl_config.target_connect_attempts;
  }
  auto connected = m_listener.connect_to_target(retries, ip, port);
  if (connected && m_repl) {
    m_repl->reload_startup_file();
    for (const auto& line : m_repl->startup_file.run_after_listen) {
      handle_repl_string(line);
    }
  }
  return get_none();
}

Val* Compiler::compile_repl_clear_screen(const goos::Object&, const goos::Object&, Env*) {
  m_repl.get()->clear_screen();
  return get_none();
}

/*!
 * Send the target a command to reset, which totally resets the state of the target.
 * Optionally takes a :shutdown command which causes the exec_runtime function of the target
 * to return after MachineShutdown.
 */
Val* Compiler::compile_reset_target(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  bool shutdown = false;
  for_each_in_list(rest, [&](const goos::Object& o) {
    if (o.is_symbol() && symbol_string(o) == ":shutdown") {
      shutdown = true;
    } else {
      throw_compiler_error(form, "invalid argument to reset-target: \"{}\"", o.print());
    }
  });
  m_listener.send_reset(shutdown);
  return get_none();
}

/*!
 * Send a "poke" message to the target. This can be used to check if the target is still alive and
 * acknowledges commands, and also tells that target that somebody is connected so it will flush
 * its outgoing buffers that have been storing data from startup.
 */
Val* Compiler::compile_poke(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {}, {});
  m_listener.send_poke();
  return get_none();
}

/*!
 * Enter a goos REPL.
 */
Val* Compiler::compile_gs(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {}, {});
  m_goos.execute_repl(*m_repl.get());
  return get_none();
}

/*!
 * Set a compiler setting by name.
 */
Val* Compiler::compile_set_config(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::SYMBOL, {}}, {});
  m_settings.set(symbol_string(args.unnamed.at(0)), args.unnamed.at(1));
  return get_none();
}

/*!
 * Ignore the "in-package" statement and anything it contains at the top of GOAL files.
 */
Val* Compiler::compile_in_package(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)form;
  (void)rest;
  (void)env;
  return get_none();
}

/*!
 * TODO - this form will denote which .DGO/.CGO the file should be bundled within
 */
Val* Compiler::compile_bundles(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)form;
  (void)rest;
  (void)env;
  return get_none();
}

/*!
 * TODO - this form will denote a dependency for the given file
 */
Val* Compiler::compile_require(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)form;
  (void)rest;
  (void)env;
  return get_none();
}

/*!
 * Build dgo files. Takes a string argument pointing to the DGO description file, which is read
 * and parsed here.
 */
Val* Compiler::compile_build_dgo(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::STRING}, {});
  auto dgo_desc = pair_cdr(m_goos.reader.read_from_file({args.unnamed.at(0).as_string()->data}));

  for_each_in_list(dgo_desc, [&](const goos::Object& dgo) {
    DgoDescription desc;
    auto first = pair_car(dgo);
    desc.dgo_name = as_string(first);
    auto dgo_rest = pair_cdr(dgo);

    for_each_in_list(dgo_rest, [&](const goos::Object& entry) {
      auto e_arg = get_va(dgo, entry);
      va_check(dgo, e_arg, {goos::ObjectType::STRING, goos::ObjectType::STRING}, {});
      DgoDescription::DgoEntry o;
      o.file_name = as_string(e_arg.unnamed.at(0));
      o.name_in_dgo = as_string(e_arg.unnamed.at(1));
      if (o.file_name.substr(o.file_name.length() - 3) != ".go") {
        desc.entries.push_back(o);
      } else {
        // allow data objects to be missing.
        if (fs::exists(file_util::get_file_path(
                {"out", m_make.compiler_output_prefix(), "obj", o.file_name}))) {
          desc.entries.push_back(o);
        }
      }
    });

    build_dgo(desc, m_make.compiler_output_prefix());
  });

  return get_none();
}

Val* Compiler::compile_reload(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {}, {});
  m_want_reload = true;
  return get_none();
}

std::string Compiler::make_symbol_info_description(const symbol_info::SymbolInfo* info) {
  switch (info->m_kind) {
    case symbol_info::Kind::GLOBAL_VAR:
      return fmt::format("[Global Variable] Type: {} Defined: {}",
                         m_symbol_types.lookup(m_goos.intern_ptr(info->m_name))->print(),
                         m_goos.reader.db.get_info_for(info->m_def_form));
    case symbol_info::Kind::LANGUAGE_BUILTIN:
      return fmt::format("[Built-in Form] {}\n", info->m_name);
    case symbol_info::Kind::METHOD:
      return fmt::format("[Method] Type: {} Method Name: {} Defined: {}",
                         info->m_method_info.defined_in_type, info->m_name,
                         m_goos.reader.db.get_info_for(info->m_def_form));
    case symbol_info::Kind::TYPE:
      return fmt::format("[Type] Name: {} Defined: {}", info->m_name,
                         m_goos.reader.db.get_info_for(info->m_def_form));
    case symbol_info::Kind::MACRO:
      return fmt::format("[Macro] Name: {} Defined: {}", info->m_name,
                         m_goos.reader.db.get_info_for(info->m_def_form));
    case symbol_info::Kind::CONSTANT:
      return fmt::format(
          "[Constant] Name: {} Value: {} Defined: {}", info->m_name,
          m_global_constants.lookup(m_goos.reader.symbolTable.intern(info->m_name.c_str()))
              ->print(),
          m_goos.reader.db.get_info_for(info->m_def_form));
    case symbol_info::Kind::FUNCTION:
      return fmt::format("[Function] Name: {} Defined: {}", info->m_name,
                         m_goos.reader.db.get_info_for(info->m_def_form));
    case symbol_info::Kind::FWD_DECLARED_SYM:
      return fmt::format("[Forward-Declared] Name: {} Defined: {}", info->m_name,
                         m_goos.reader.db.get_info_for(info->m_def_form));
    default:
      ASSERT(false);
      return {};
  }
}

Val* Compiler::compile_get_info(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::SYMBOL}, {});

  const auto result = m_symbol_info.lookup_exact_name(args.unnamed.at(0).as_symbol().name_ptr);
  if (result.empty()) {
    lg::print("No results found.\n");
  } else {
    for (const auto& info : result) {
      lg::print("{}", make_symbol_info_description(info));
    }
  }

  return get_none();
}

replxx::Replxx::completions_t Compiler::find_symbols_or_object_file_by_prefix(
    std::string const& context,
    int& contextLen,
    std::vector<std::string> const& user_data) {
  (void)contextLen;
  (void)user_data;
  replxx::Replxx::completions_t completions;

  // If we are trying to execute a `(ml ...)` we can automatically get the object file
  // insert quotes if needed as well.
  if (str_util::starts_with(context, "(ml ")) {
    std::string file_name_prefix = context.substr(4);
    // Trim string just incase, extra whitespace is valid LISP
    file_name_prefix = str_util::trim(file_name_prefix);
    // Remove quotes
    file_name_prefix.erase(remove(file_name_prefix.begin(), file_name_prefix.end(), '"'),
                           file_name_prefix.end());
    if (file_name_prefix.empty()) {
      return completions;
    }

    // Get all the potential object file names
    const auto& matches = m_global_env->list_files_with_prefix(file_name_prefix);
    for (const auto& match : matches) {
      completions.push_back(fmt::format("\"{}\")", match));
    }
  } else {
    // TODO - GOAL's method calling syntax sucks for method name auto-completion
    // maybe something that could be improved? Though it would be a radical departure from
    // the syntax
    const auto [token, stripped_leading_paren] = m_repl->get_current_repl_token(context);
    // Otherwise, look for symbols
    auto possible_forms = lookup_symbol_names_starting_with(token, 100);

    for (auto& x : possible_forms) {
      completions.push_back(stripped_leading_paren ? "(" + x : x);
    }
  }

  return completions;
}

replxx::Replxx::hints_t Compiler::find_hints_by_prefix(std::string const& context,
                                                       int& contextLen,
                                                       replxx::Replxx::Color& color,
                                                       std::vector<std::string> const& user_data) {
  (void)contextLen;
  (void)user_data;
  auto token = m_repl->get_current_repl_token(context);
  auto possible_forms = lookup_symbol_names_starting_with(token.first, 100);

  replxx::Replxx::hints_t hints;

  // TODO - hints for `(ml ...` as well

  // Only show hints if there are <= 3 possibilities
  if (possible_forms.size() <= 3) {
    for (auto& x : possible_forms) {
      hints.push_back(token.second ? "(" + x : x);
    }
  }

  // set hint color to green if single match found
  if (hints.size() == 1) {
    color = replxx::Replxx::Color::GREEN;
  }

  return hints;
}

void Compiler::repl_coloring(
    std::string const& context,
    replxx::Replxx::colors_t& colors,
    std::vector<std::pair<std::string, replxx::Replxx::Color>> const& regex_color) {
  (void)regex_color;
  using cl = replxx::Replxx::Color;
  // TODO - a proper circular queue would be cleaner to use
  std::deque<cl> paren_colors = {cl::GREEN, cl::CYAN, cl::MAGENTA};
  std::stack<std::pair<char, cl>> expression_stack;

  std::pair<int, std::string> curr_symbol = {-1, ""};
  for (std::string::size_type i = 0; i < context.size(); i++) {
    char curr = context.at(i);
    // We lookup every potential symbol and color it based on it's type
    if (std::isspace(curr) || curr == ')') {
      // Lookup the symbol, if its legit, color it
      if (!curr_symbol.second.empty() && curr_symbol.second.at(0) == '(') {
        curr_symbol.second.erase(0, 1);
        curr_symbol.first++;
      }
      const auto matching_symbols = lookup_exact_name_info(curr_symbol.second);
      if (matching_symbols.size() == 1) {
        for (int pos = curr_symbol.first; pos <= int(i); pos++) {
          // TODO - currently just coloring all types brown/gold
          // - would be nice to have a different color for globals, functions, etc
          colors.at(pos) = cl::BROWN;
        }
      }
      curr_symbol = {-1, ""};
    } else {
      if (curr_symbol.first == -1) {
        curr_symbol.first = i;
      }
      curr_symbol.second += curr;
    }
    // Rainbow paren coloring and known-form coloring
    if (curr == '(') {
      cl color = paren_colors.front();
      expression_stack.push({curr, color});
      colors.at(i) = color;
      paren_colors.pop_front();
      paren_colors.push_back(color);
    } else if (curr == ')') {
      if (expression_stack.empty()) {
        colors.at(i) = cl::RED;
      } else {
        auto& matching_paren = expression_stack.top();
        expression_stack.pop();
        if (matching_paren.first == '(') {
          if (i == context.size() - 1 && !expression_stack.empty()) {
            colors.at(i) = cl::RED;
          } else {
            colors.at(i) = matching_paren.second;
          }
        }
      }
    }
    // Reset the color order
    if (expression_stack.empty()) {
      paren_colors = {cl::GREEN, cl::CYAN, cl::MAGENTA};
    }
  }

  // TODO - general syntax highlighting with AST
}

Val* Compiler::compile_autocomplete(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::SYMBOL}, {});

  Timer timer;
  auto result = m_symbol_info.lookup_names_starting_with(args.unnamed.at(0).as_symbol().name_ptr);
  auto time = timer.getMs();

  for (auto& x : result) {
    lg::print(" {}\n", x);
  }

  lg::print("Autocomplete: {}/{} symbols matched, took {:.2f} ms\n", result.size(),
            m_symbol_info.symbol_count(), time);

  return get_none();
}

Val* Compiler::compile_update_macro_metadata(const goos::Object& form,
                                             const goos::Object& rest,
                                             Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  // We have to manually check the args here, as an empty list is considered something distinct from
  // a pair
  if (args.unnamed.size() != 3 || args.unnamed.at(0).type != goos::ObjectType::SYMBOL ||
      args.unnamed.at(1).type != goos::ObjectType::STRING ||
      (args.unnamed.at(2).type != goos::ObjectType::PAIR &&
       args.unnamed.at(2).type != goos::ObjectType::EMPTY_LIST)) {
    throw_compiler_error(form, "Invalid arguments provided to `update-macro-metadata");
  }

  auto& name = args.unnamed.at(0).as_symbol().name_ptr;

  auto arg_spec = m_goos.parse_arg_spec(form, args.unnamed.at(2));
  m_symbol_info.add_macro(name, arg_spec, form, args.unnamed.at(1).as_string()->data);
  return get_none();
}

std::vector<symbol_info::SymbolInfo*> Compiler::lookup_symbol_info_by_file(
    const std::string& file_path) const {
  return m_symbol_info.lookup_symbols_by_file(file_path);
}

std::vector<symbol_info::SymbolInfo*> Compiler::lookup_symbol_info_by_prefix(
    const std::string& prefix) const {
  return m_symbol_info.lookup_symbols_starting_with(prefix);
}

std::set<std::string> Compiler::lookup_symbol_names_starting_with(const std::string& prefix,
                                                                  int max_count) const {
  if (m_goos.reader.check_string_is_valid(prefix)) {
    return m_symbol_info.lookup_names_starting_with(prefix, max_count);
  }
  return {};
}

std::vector<symbol_info::SymbolInfo*> Compiler::lookup_exact_name_info(
    const std::string& name) const {
  if (m_goos.reader.check_string_is_valid(name)) {
    return m_symbol_info.lookup_exact_name(name);
  } else {
    return {};
  }
}

std::optional<TypeSpec> Compiler::lookup_typespec(const std::string& symbol_name) {
  const auto it = m_symbol_types.lookup(m_goos.intern_ptr(symbol_name));
  if (it) {
    return *it;
  }
  return {};
}

std::tuple<std::unordered_map<std::string, Docs::SymbolDocumentation>,
           std::unordered_map<std::string, Docs::FileDocumentation>>
Compiler::generate_per_file_symbol_info() {
  // TODO - remove this function, all required information has been consolidated into `SymbolInfo`
  // it just has to be serialized in the same way, I will do it later
  const auto symbols = m_symbol_info.get_all_symbols();

  std::unordered_map<std::string, Docs::SymbolDocumentation> all_symbols;
  std::unordered_map<std::string, Docs::FileDocumentation> file_docs;

  lg::info("Processing {} symbols...", symbols.size());
  int count = 0;
  for (const auto& sym_info : symbols) {
    count++;
    if (count % 100 == 0 || count == (int)symbols.size()) {
      lg::info("Processing [{}/{}] symbols...", count, symbols.size());
    }
    std::optional<Docs::DefinitionLocation> def_loc;
    const auto& goos_info = m_goos.reader.db.get_short_info_for(sym_info->m_def_form);
    if (goos_info) {
      Docs::DefinitionLocation new_def_loc;
      new_def_loc.filename = file_util::convert_to_unix_path_separators(file_util::split_path_at(
          goos_info->filename, {"goal_src", version_to_game_name(m_version)}));
      new_def_loc.line_idx = goos_info->line_idx_to_display;
      new_def_loc.char_idx = goos_info->pos_in_line;
      def_loc = new_def_loc;
    }

    Docs::SymbolDocumentation sym_doc;
    sym_doc.name = sym_info->m_name;
    sym_doc.description = sym_info->m_docstring;
    sym_doc.kind = sym_info->m_kind;
    sym_doc.def_location = def_loc;

    if (all_symbols.count(sym_info->m_name) > 1) {
      lg::error("A symbol was defined twice, how did this happen? {}", sym_info->m_name);
    } else {
      all_symbols.emplace(sym_info->m_name, sym_doc);
    }

    Docs::FileDocumentation file_doc;
    std::string file_doc_key;
    if (!goos_info) {
      file_doc_key = "unknown";
    } else {
      file_doc_key = file_util::convert_to_unix_path_separators(
          file_util::split_path_at(goos_info->filename, {"goal_src"}));
    }

    if (file_docs.count(file_doc_key) != 0) {
      file_doc = file_docs.at(file_doc_key);
    } else {
      file_doc = Docs::FileDocumentation();
    }

    // TODO - states / enums / built-ins
    if (sym_info->m_kind == symbol_info::Kind::GLOBAL_VAR ||
        sym_info->m_kind == symbol_info::Kind::CONSTANT) {
      Docs::VariableDocumentation var;
      var.name = sym_info->m_name;
      var.description = sym_info->m_docstring;
      if (sym_info->m_kind == symbol_info::Kind::CONSTANT) {
        var.type = "unknown";  // Unfortunately, constants are not properly typed
      } else {
        var.type = m_symbol_types.lookup(m_goos.intern_ptr(var.name))->base_type();
      }
      var.def_location = def_loc;
      if (sym_info->m_kind == symbol_info::Kind::GLOBAL_VAR) {
        file_doc.global_vars.push_back(var);
      } else {
        file_doc.constants.push_back(var);
      }
    } else if (sym_info->m_kind == symbol_info::Kind::FUNCTION) {
      Docs::FunctionDocumentation func;
      func.name = sym_info->m_name;
      func.description = sym_info->m_docstring;
      func.def_location = def_loc;
      func.args = Docs::get_args_from_docstring(sym_info->m_args, func.description);
      // The last arg in the typespec is the return type
      const auto func_type = m_symbol_types.lookup(m_goos.intern_ptr(func.name));
      func.return_type = func_type->last_arg().base_type();
      file_doc.functions.push_back(func);
    } else if (sym_info->m_kind == symbol_info::Kind::TYPE) {
      Docs::TypeDocumentation type;
      type.name = sym_info->m_name;
      type.description = sym_info->m_docstring;
      type.def_location = def_loc;
      const auto& type_info = m_ts.lookup_type(type.name);
      type.parent_type = type_info->get_parent();
      type.size = type_info->get_size_in_memory();
      type.method_count = type_info->get_methods_defined_for_type().size();
      if (m_ts.typecheck_and_throw(m_ts.make_typespec("structure"), m_ts.make_typespec(type.name),
                                   "", false, false, false)) {
        auto struct_info = dynamic_cast<StructureType*>(type_info);
        for (const auto& field : struct_info->fields()) {
          Docs::FieldDocumentation field_doc;
          field_doc.name = field.name();
          field_doc.description = "";
          field_doc.type = field.type().base_type();
          field_doc.is_array = field.is_array();
          field_doc.is_inline = field.is_inline();
          field_doc.is_dynamic = field.is_dynamic();
          type.fields.push_back(field_doc);
        }
      }
      for (const auto& method : type_info->get_methods_defined_for_type()) {
        // Check to see if it's a state
        if (m_ts.typecheck_and_throw(m_ts.make_typespec("state"), method.type, "", false, false,
                                     false)) {
          Docs::TypeStateDocumentation state_doc;
          state_doc.id = method.id;
          state_doc.is_virtual = true;
          state_doc.name = method.name;
          type.states.push_back(state_doc);
        } else {
          Docs::TypeMethodDocumentation method_doc;
          method_doc.id = method.id;
          method_doc.name = method.name;
          method_doc.is_override = method.overrides_parent;
          type.methods.push_back(method_doc);
        }
      }
      for (const auto& [state_name, state_info] : type_info->get_states_declared_for_type()) {
        Docs::TypeStateDocumentation state_doc;
        state_doc.name = state_name;
        state_doc.is_virtual = false;
        type.states.push_back(state_doc);
      }
      file_doc.types.push_back(type);
    } else if (sym_info->m_kind == symbol_info::Kind::MACRO) {
      Docs::MacroDocumentation macro_doc;
      macro_doc.name = sym_info->m_name;
      macro_doc.description = sym_info->m_docstring;
      macro_doc.def_location = def_loc;
      // TODO - rewrite all of this to use the new symbol map, make sure macros work
      /*const auto& arg_spec = m_macro_specs[macro_doc.name];
      for (const auto& arg : arg_spec.unnamed) {
        macro_doc.args.push_back(arg);
      }
      for (const auto& arg : arg_spec.named) {
        std::optional<std::string> def_value;
        if (arg.second.has_default) {
          def_value = arg.second.default_value.print();
        }
        macro_doc.kwargs.push_back({arg.first, def_value});
      }
      if (!arg_spec.rest.empty()) {
        macro_doc.variadic_arg = arg_spec.rest;
      }*/
      file_doc.macros.push_back(macro_doc);
    } else if (sym_info->m_kind == symbol_info::Kind::METHOD) {
      Docs::MethodDocumentation method_doc;
      method_doc.name = sym_info->m_name;
      method_doc.description = sym_info->m_docstring;
      method_doc.def_location = def_loc;
      const auto& method_info = sym_info->m_method_info;
      method_doc.id = method_info.id;
      method_doc.type = sym_info->m_method_info.defined_in_type;
      method_doc.is_override = method_info.overrides_parent;
      method_doc.args = Docs::get_args_from_docstring(sym_info->m_args, method_doc.description);
      // The last arg in the typespec is the return type
      const auto& method_type = method_info.type;
      method_doc.return_type = method_type.last_arg().base_type();
      method_doc.is_builtin = method_doc.id <= 9;
      file_doc.methods.push_back(method_doc);
    }
    file_docs[file_doc_key] = file_doc;
  }
  return {all_symbols, file_docs};
}

Val* Compiler::compile_load_project(const goos::Object& form, const goos::Object& rest, Env*) {
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::STRING}, {});
  m_make.load_project_file(args.unnamed.at(0).as_string()->data);
  return get_none();
}

Val* Compiler::compile_make(const goos::Object& form, const goos::Object& rest, Env*) {
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::STRING},
           {{"force", {false, {goos::ObjectType::SYMBOL}}},
            {"verbose", {false, {goos::ObjectType::SYMBOL}}},
            {"report", {false, {goos::ObjectType::SYMBOL}}}});
  bool force = false;
  if (args.has_named("force")) {
    force = get_true_or_false(form, args.get_named("force"));
  }

  bool verbose = false;
  if (args.has_named("verbose")) {
    verbose = get_true_or_false(form, args.get_named("verbose"));
  }

  bool report = false;
  if (args.has_named("report")) {
    report = get_true_or_false(form, args.get_named("report"));
  }

  m_make.make(args.unnamed.at(0).as_string()->data, force, verbose, report);
  return get_none();
}

Val* Compiler::compile_print_debug_compiler_stats(const goos::Object& form,
                                                  const goos::Object& rest,
                                                  Env*) {
  auto args = get_va(form, rest);
  va_check(form, args, {}, {});

  lg::print("Spill operations (total): {}\n", m_debug_stats.num_spills);
  lg::print("Spill operations (v1 only): {}\n", m_debug_stats.num_spills_v1);
  lg::print("Eliminated moves: {}\n", m_debug_stats.num_moves_eliminated);
  lg::print("Total functions: {}\n", m_debug_stats.total_funcs);
  lg::print("Functions requiring v1: {}\n", m_debug_stats.funcs_requiring_v1_allocator);
  lg::print("Size of autocomplete prefix tree: {}\n", m_symbol_info.symbol_count());

  return get_none();
}

Val* Compiler::compile_gen_docs(const goos::Object& form, const goos::Object& rest, Env*) {
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::STRING}, {});

  const auto& doc_path = fs::path(args.unnamed.at(0).as_string()->data);
  lg::info("Saving docs to: {}", doc_path.string());

  const auto [all_symbols, file_docs] = generate_per_file_symbol_info();

  json symbol_map_data(all_symbols);
  file_util::write_text_file(
      doc_path / fmt::format("{}-symbol-map.json", version_to_game_name(m_version)),
      symbol_map_data.dump());
  json file_docs_data(file_docs);
  file_util::write_text_file(
      doc_path / fmt::format("{}-file-docs.json", version_to_game_name(m_version)),
      file_docs_data.dump());

  return get_none();
}

Val* Compiler::compile_export_requires(const goos::Object& form, const goos::Object& rest, Env*) {
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::STRING}, {});

  const auto& export_path = fs::path(args.unnamed.at(0).as_string()->data);
  lg::info("Saving require map to: {}", export_path.string());

  json export_map;
  for (const auto& file : m_global_env->get_files()) {
    export_map[file->name()] = file->m_missing_required_files;
  }

  file_util::write_text_file(
      export_path / fmt::format("{}-missing-requires.json", version_to_game_name(m_version)),
      export_map.dump());

  return get_none();
}

Val* Compiler::compile_gc_text(const goos::Object&, const goos::Object&, Env*) {
  m_goos.reader.db.clear_info();
  return get_none();
}
