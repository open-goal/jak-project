/*!
 * @file CompilerControl.cpp
 * Compiler implementation for forms which actually control the compiler.
 */

#include <filesystem>
#include <regex>
#include <stack>

#include "common/goos/ReplUtils.h"
#include "common/util/DgoWriter.h"
#include "common/util/FileUtil.h"
#include "common/util/Timer.h"
#include "goalc/compiler/Compiler.h"
#include "goalc/compiler/IR.h"
#include "goalc/data_compiler/dir_tpages.h"
#include "goalc/data_compiler/game_count.h"
#include "goalc/data_compiler/game_text.h"
#include "goalc/data_compiler/game_subtitle.h"

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
    compile_game_count(as_string(args.unnamed.at(1)));
  } else if (kind == "dir-tpages") {
    compile_dir_tpages(as_string(args.unnamed.at(1)));
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
  va_check(form, args, {goos::ObjectType::SYMBOL, goos::ObjectType::INTEGER},
           {{"files", {true, goos::ObjectType::PAIR}}});
  auto kind = symbol_string(args.unnamed.at(0));
  if (kind == "subtitle") {
    std::vector<std::string> files;
    for_each_in_list(args.named.at("files"), [this, &files, &form](const goos::Object& o) {
      if (o.is_string()) {
        files.push_back(o.as_string()->data);
      } else {
        throw_compiler_error(form, "Invalid object {} in asm-text-file files list.", o.print());
      }
    });
    compile_game_subtitle(files, (GameTextVersion)args.unnamed.at(1).as_int());
  } else if (kind == "text") {
    std::vector<std::string> files;
    for_each_in_list(args.named.at("files"), [this, &files, &form](const goos::Object& o) {
      if (o.is_string()) {
        files.push_back(o.as_string()->data);
      } else {
        throw_compiler_error(form, "Invalid object {} in asm-text-file files list.", o.print());
      }
    });
    compile_game_text(files, (GameTextVersion)args.unnamed.at(1).as_int());
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
  std::string filename;
  std::string disasm_filename = "";
  bool load = false;
  bool color = false;
  bool write = false;
  bool no_code = false;
  bool disassemble = false;
  bool no_time_prints = false;

  std::vector<std::pair<std::string, double>> timing;
  Timer total_timer;

  // parse arguments
  bool last_was_disasm = false;
  for_each_in_list(rest, [&](const goos::Object& o) {
    if (last_was_disasm) {
      last_was_disasm = false;
      if (o.type == goos::ObjectType::STRING) {
        disasm_filename = as_string(o);
        i++;
        return;
      }
    }
    if (i == 0) {
      filename = as_string(o);
    } else {
      auto setting = symbol_string(o);
      if (setting == ":load") {
        load = true;
      } else if (setting == ":color") {
        color = true;
      } else if (setting == ":write") {
        write = true;
      } else if (setting == ":no-code") {
        no_code = true;
      } else if (setting == ":disassemble") {
        disassemble = true;
        last_was_disasm = true;
      } else if (setting == ":no-time-prints") {
        no_time_prints = true;
      } else {
        throw_compiler_error(form, "The option {} was not recognized for asm-file.", setting);
      }
    }
    i++;
  });

  // READ
  Timer reader_timer;
  auto code = m_goos.reader.read_from_file({filename});
  timing.emplace_back("read", reader_timer.getMs());

  Timer compile_timer;
  std::string obj_file_name = filename;

  // Extract object name from file name.
  for (int idx = int(filename.size()) - 1; idx-- > 0;) {
    if (filename.at(idx) == '\\' || filename.at(idx) == '/') {
      obj_file_name = filename.substr(idx + 1);
      break;
    }
  }
  obj_file_name = obj_file_name.substr(0, obj_file_name.find_last_of('.'));

  // COMPILE
  auto obj_file = compile_object_file(obj_file_name, code, !no_code);
  timing.emplace_back("compile", compile_timer.getMs());

  if (color) {
    // register allocation
    Timer color_timer;
    color_object_file(obj_file);
    timing.emplace_back("color", color_timer.getMs());

    // code/object file generation
    Timer codegen_timer;
    std::vector<u8> data;
    std::string disasm;
    if (disassemble) {
      codegen_and_disassemble_object_file(obj_file, &data, &disasm);
      if (disasm_filename == "") {
        printf("%s\n", disasm.c_str());
      } else {
        file_util::write_text_file(disasm_filename, disasm);
      }
    } else {
      data = codegen_object_file(obj_file);
    }
    timing.emplace_back("codegen", codegen_timer.getMs());

    // send to target
    if (load) {
      if (m_listener.is_connected()) {
        m_listener.send_code(data, obj_file_name);
      } else {
        printf("WARNING - couldn't load because listener isn't connected\n");  // todo log warn
      }
    }

    // save file
    if (write) {
      auto path = file_util::get_file_path({"out", "obj", obj_file_name + ".o"});
      file_util::create_dir_if_needed_for_file(path);
      file_util::write_binary_file(path, (void*)data.data(), data.size());
    }
  } else {
    if (load) {
      printf("WARNING - couldn't load because coloring is not enabled\n");
    }

    if (write) {
      printf("WARNING - couldn't write because coloring is not enabled\n");
    }

    if (disassemble) {
      printf("WARNING - couldn't disassemble because coloring is not enabled\n");
    }
  }

  if (m_settings.print_timing) {
    printf("F: %36s ", obj_file_name.c_str());
    timing.emplace_back("total", total_timer.getMs());
    for (auto& e : timing) {
      printf(" %12s %4.0f", e.first.c_str(), e.second);
    }
    printf("\n");
  } else {
    auto total_time = total_timer.getMs();
    if (total_time > 10.0 && color && !no_time_prints) {
      fmt::print("[ASM-FILE] {} took {:.2f} ms\n", obj_file_name, total_time);
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
 * Connect the compiler to a target. Takes an optional IP address / port, defaults to
 * 127.0.0.1 and 8112, which is the local computer and the default port for the DECI2 over IP
 * implementation.
 */
Val* Compiler::compile_listen_to_target(const goos::Object& form,
                                        const goos::Object& rest,
                                        Env* env) {
  (void)env;
  std::string ip = "127.0.0.1";
  int port = DECI2_PORT;
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

  m_listener.connect_to_target(30, ip, port);
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
        if (std::filesystem::exists(file_util::get_file_path({"out", "obj", o.file_name}))) {
          desc.entries.push_back(o);
        }
      }
    });

    build_dgo(desc);
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

std::string Compiler::make_symbol_info_description(const SymbolInfo& info) {
  switch (info.kind()) {
    case SymbolInfo::Kind::GLOBAL_VAR:
      return fmt::format("[Global Variable] Type: {} Defined: {}",
                         m_symbol_types.at(info.name()).print(),
                         m_goos.reader.db.get_info_for(info.src_form()));
    case SymbolInfo::Kind::LANGUAGE_BUILTIN:
      return fmt::format("[Built-in Form] {}\n", info.name());
    case SymbolInfo::Kind::METHOD:
      return fmt::format("[Method] Type: {} Method Name: {} Defined: {}", info.type(), info.name(),
                         m_goos.reader.db.get_info_for(info.src_form()));
    case SymbolInfo::Kind::TYPE:
      return fmt::format("[Type] Name: {} Defined: {}", info.name(),
                         m_goos.reader.db.get_info_for(info.src_form()));
    case SymbolInfo::Kind::MACRO:
      return fmt::format("[Macro] Name: {} Defined: {}", info.name(),
                         m_goos.reader.db.get_info_for(info.src_form()));
    case SymbolInfo::Kind::CONSTANT:
      return fmt::format(
          "[Constant] Name: {} Value: {} Defined: {}", info.name(),
          m_global_constants.at(m_goos.reader.symbolTable.intern_ptr(info.name())).print(),
          m_goos.reader.db.get_info_for(info.src_form()));
    case SymbolInfo::Kind::FUNCTION:
      return fmt::format("[Function] Name: {} Defined: {}", info.name(),
                         m_goos.reader.db.get_info_for(info.src_form()));
    case SymbolInfo::Kind::FWD_DECLARED_SYM:
      return fmt::format("[Forward-Declared] Name: {} Defined: {}", info.name(),
                         m_goos.reader.db.get_info_for(info.src_form()));
    default:
      ASSERT(false);
      return {};
  }
}

Val* Compiler::compile_get_info(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::SYMBOL}, {});

  auto result = m_symbol_info.lookup_exact_name(args.unnamed.at(0).as_symbol()->name);
  if (!result) {
    fmt::print("No results found.\n");
  } else {
    for (auto& info : *result) {
      fmt::print("{}", make_symbol_info_description(info));
    }
  }

  return get_none();
}

Replxx::completions_t Compiler::find_symbols_by_prefix(std::string const& context,
                                                       int& contextLen,
                                                       std::vector<std::string> const& user_data) {
  (void)contextLen;
  (void)user_data;
  auto token = m_repl->get_current_repl_token(context);
  auto possible_forms = lookup_symbol_infos_starting_with(token.first);
  Replxx::completions_t completions;
  for (auto& x : possible_forms) {
    completions.push_back(token.second ? "(" + x : x);
  }
  return completions;
}

Replxx::hints_t Compiler::find_hints_by_prefix(std::string const& context,
                                               int& contextLen,
                                               Replxx::Color& color,
                                               std::vector<std::string> const& user_data) {
  (void)contextLen;
  (void)user_data;
  auto token = m_repl->get_current_repl_token(context);
  auto possible_forms = lookup_symbol_infos_starting_with(token.first);

  Replxx::hints_t hints;

  // Only show hints if there are <= 3 possibilities
  if (possible_forms.size() <= 3) {
    for (auto& x : possible_forms) {
      hints.push_back(token.second ? "(" + x : x);
    }
  }

  // set hint color to green if single match found
  if (hints.size() == 1) {
    color = Replxx::Color::GREEN;
  }

  return hints;
}

void Compiler::repl_coloring(
    std::string const& context,
    Replxx::colors_t& colors,
    std::vector<std::pair<std::string, Replxx::Color>> const& regex_color) {
  (void)regex_color;
  using cl = Replxx::Color;
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
      std::vector<SymbolInfo>* sym_match = lookup_exact_name_info(curr_symbol.second);
      if (sym_match != nullptr && sym_match->size() == 1) {
        SymbolInfo sym_info = sym_match->at(0);
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

  // TODO - general syntax highlighting with regexes (quotes, symbols, etc)
}

Val* Compiler::compile_autocomplete(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::SYMBOL}, {});

  Timer timer;
  auto result = m_symbol_info.lookup_symbols_starting_with(args.unnamed.at(0).as_symbol()->name);
  auto time = timer.getMs();

  for (auto& x : result) {
    fmt::print(" {}\n", x);
  }

  fmt::print("Autocomplete: {}/{} symbols matched, took {:.2f} ms\n", result.size(),
             m_symbol_info.symbol_count(), time);

  return get_none();
}

Val* Compiler::compile_add_macro_to_autocomplete(const goos::Object& form,
                                                 const goos::Object& rest,
                                                 Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::SYMBOL}, {});
  m_symbol_info.add_macro(args.unnamed.at(0).as_symbol()->name, form);
  return get_none();
}

std::set<std::string> Compiler::lookup_symbol_infos_starting_with(const std::string& prefix) const {
  if (m_goos.reader.check_string_is_valid(prefix)) {
    return m_symbol_info.lookup_symbols_starting_with(prefix);
  }
  return {};
}

std::vector<SymbolInfo>* Compiler::lookup_exact_name_info(const std::string& name) const {
  if (m_goos.reader.check_string_is_valid(name)) {
    return m_symbol_info.lookup_exact_name(name);
  } else {
    return nullptr;
  }
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
            {"verbose", {false, {goos::ObjectType::SYMBOL}}}});
  bool force = false;
  if (args.has_named("force")) {
    force = get_true_or_false(form, args.get_named("force"));
  }

  bool verbose = false;
  if (args.has_named("verbose")) {
    verbose = get_true_or_false(form, args.get_named("verbose"));
  }

  m_make.make(args.unnamed.at(0).as_string()->data, force, verbose);
  return get_none();
}

Val* Compiler::compile_print_debug_compiler_stats(const goos::Object& form,
                                                  const goos::Object& rest,
                                                  Env*) {
  auto args = get_va(form, rest);
  va_check(form, args, {}, {});

  fmt::print("Spill operations (total): {}\n", m_debug_stats.num_spills);
  fmt::print("Spill operations (v1 only): {}\n", m_debug_stats.num_spills_v1);
  fmt::print("Eliminated moves: {}\n", m_debug_stats.num_moves_eliminated);
  fmt::print("Total functions: {}\n", m_debug_stats.total_funcs);
  fmt::print("Functions requiring v1: {}\n", m_debug_stats.funcs_requiring_v1_allocator);
  fmt::print("Size of autocomplete prefix tree: {}\n", m_symbol_info.symbol_count());

  return get_none();
}
