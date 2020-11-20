/*!
 * @file CompilerControl.cpp
 * Compiler implementation for forms which actually control the compiler.
 */

#include "goalc/compiler/Compiler.h"
#include "goalc/compiler/IR.h"
#include "common/util/Timer.h"
#include "common/util/DgoWriter.h"
#include "common/util/FileUtil.h"
#include "goalc/data_compiler/game_text.h"

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
      m_goos.eval_with_rewind(o, m_goos.global_environment.as_env());
    });
  } catch (std::runtime_error& e) {
    throw_compile_error(form, std::string("seval error: ") + e.what());
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
  if (kind == "game-text") {
    compile_game_text(as_string(args.unnamed.at(1)));
  } else {
    throw_compile_error(form, "Unknown asm data file mode");
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
  bool load = false;
  bool color = false;
  bool write = false;
  bool no_code = false;

  std::vector<std::pair<std::string, float>> timing;
  Timer total_timer;

  // parse arguments
  for_each_in_list(rest, [&](const goos::Object& o) {
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
      } else {
        throw_compile_error(form, "invalid option " + setting + " in asm-file form");
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
    auto data = codegen_object_file(obj_file);
    timing.emplace_back("codegen", codegen_timer.getMs());

    // send to target
    if (load) {
      if (m_listener.is_connected()) {
        m_listener.send_code(data);
      } else {
        printf("WARNING - couldn't load because listener isn't connected\n");  // todo spdlog warn
      }
    }

    // save file
    if (write) {
      auto output_name = m_goos.reader.get_source_dir() + "/data/" + obj_file_name + ".o";
      file_util::write_binary_file(output_name, (void*)data.data(), data.size());
    }
  } else {
    if (load) {
      printf("WARNING - couldn't load because coloring is not enabled\n");
    }

    if (write) {
      printf("WARNING - couldn't write because coloring is not enabled\n");
    }
  }

  if (m_settings.print_timing) {
    printf("F: %36s ", obj_file_name.c_str());
    timing.emplace_back("total", total_timer.getMs());
    for (auto& e : timing) {
      printf(" %12s %4.0f", e.first.c_str(), e.second);
    }
    printf("\n");
  }

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
        throw_compile_error(form, "got multiple strings!");
      }
      got_ip = true;
      ip = o.as_string()->data;
    } else if (o.is_int()) {
      if (got_port) {
        throw_compile_error(form, "got multiple ports!");
      }
      got_port = true;
      port = o.integer_obj.value;
    } else {
      throw_compile_error(form, "invalid argument to listen-to-target");
    }
  });

  m_listener.connect_to_target(30, ip, port);
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
      throw_compile_error(form, "invalid argument to reset-target");
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
  m_goos.execute_repl();
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
      if (o.file_name.substr(o.file_name.length() - 3) != ".go") {  // kill v2's for now.
        desc.entries.push_back(o);
      }
    });

    build_dgo(desc);
  });

  return get_none();
}