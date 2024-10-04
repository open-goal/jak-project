#include "Compiler.h"

#include <chrono>
#include <thread>

#include "CompilerException.h"
#include "IR.h"

#include "common/goos/PrettyPrinter.h"
#include "common/link_types.h"
#include "common/util/FileUtil.h"

#include "goalc/make/Tools.h"
#include "goalc/regalloc/Allocator.h"
#include "goalc/regalloc/Allocator_v2.h"

#include "fmt/core.h"

using namespace goos;

Compiler::Compiler(GameVersion version,
                   const std::optional<REPL::Config> repl_config,
                   const std::string& user_profile,
                   std::unique_ptr<REPL::Wrapper> repl)
    : m_version(version),
      m_goos(user_profile),
      m_debugger(&m_listener, &m_goos.reader, version),
      m_make(repl_config, user_profile),
      m_repl(std::move(repl)),
      m_symbol_info(&m_goos.reader.db) {
  m_listener.add_debugger(&m_debugger);
  m_listener.set_default_port(version);
  m_ts.add_builtin_types(m_version);
  m_global_env = std::make_unique<GlobalEnv>();
  m_none = std::make_unique<None>(m_ts.make_typespec("none"));

  // let the build system run us
  m_make.add_tool(std::make_shared<CompilerTool>(this));

  // define game version before loading goal-lib.gc
  m_goos.set_global_variable_by_name("GAME_VERSION", m_goos.intern(game_version_names[m_version]));

  // load GOAL library
  Object library_code = m_goos.reader.read_from_file({"goal_src", "goal-lib.gc"});
  compile_object_file("goal-lib", library_code, false);

  // user profile stuff
  if (user_profile != "#f" && fs::exists(file_util::get_jak_project_dir() / "goal_src" / "user" /
                                         user_profile / "user.gc")) {
    try {
      Object user_code =
          m_goos.reader.read_from_file({"goal_src", "user", user_profile, "user.gc"});
      compile_object_file(user_profile, user_code, false);
    } catch (std::exception& e) {
      print_compiler_warning("REPL Warning: {}\n", e.what());
    }
  }

  // add built-in forms to symbol info
  for (const auto& [builtin_name, builtin_info] : g_goal_forms) {
    m_symbol_info.add_builtin(builtin_name, builtin_info.first);
  }

  // load auto-complete history, only if we are running in the interactive mode.
  if (m_repl) {
    m_repl->load_history();
    // init repl
    m_repl->print_welcome_message(m_make.get_loaded_projects());
    auto& examples = m_repl->examples;
    auto& regex_colors = m_repl->regex_colors;
    m_repl->init_settings();
    using namespace std::placeholders;
    m_repl->get_repl().set_completion_callback(std::bind(
        &Compiler::find_symbols_or_object_file_by_prefix, this, _1, _2, std::cref(examples)));
    m_repl->get_repl().set_hint_callback(
        std::bind(&Compiler::find_hints_by_prefix, this, _1, _2, _3, std::cref(examples)));
    m_repl->get_repl().set_highlighter_callback(
        std::bind(&Compiler::repl_coloring, this, _1, _2, std::cref(regex_colors)));
  }

  // add GOOS forms that get info from the compiler
  setup_goos_forms();
}

Compiler::~Compiler() {
  if (m_listener.is_connected()) {
    m_listener.send_reset(false);  // reset the target
    m_listener.disconnect();
  }
}

ReplStatus Compiler::handle_repl_string(const std::string& input) {
  if (input.empty()) {
    return ReplStatus::OK;
  }

  try {
    // 1). read
    goos::Object code = m_goos.reader.read_from_string(input, true);
    // 2). compile
    auto obj_file = compile_object_file("repl", code, m_listener.is_connected());
    if (m_settings.debug_print_ir) {
      obj_file->debug_print_tl();
    }

    if (!obj_file->is_empty()) {
      // 3). color
      color_object_file(obj_file);

      // 4). codegen
      auto data = codegen_object_file(obj_file);

      // 4). send!
      if (m_listener.is_connected()) {
        m_listener.send_code(data);
        if (!m_listener.most_recent_send_was_acked()) {
          print_compiler_warning("Runtime is not responding. Did it crash?\n");
        }
      }
    }
  } catch (std::exception& e) {
    print_compiler_warning("REPL Error: {}\n", e.what());
  }

  if (m_want_exit) {
    return ReplStatus::WANT_EXIT;
  }

  if (m_want_reload) {
    return ReplStatus::WANT_RELOAD;
  }

  return ReplStatus::OK;
}

FileEnv* Compiler::compile_object_file(const std::string& name,
                                       goos::Object code,
                                       bool allow_emit) {
  auto file_env = m_global_env->add_file(name);
  Env* compilation_env = file_env;

  file_env->add_top_level_function(
      compile_top_level_function("top-level", std::move(code), compilation_env));

  if (!allow_emit && !file_env->is_empty()) {
    throw std::runtime_error("Compilation generated code, but wasn't supposed to");
  }

  return file_env;
}

std::unique_ptr<FunctionEnv> Compiler::compile_top_level_function(const std::string& name,
                                                                  const goos::Object& code,
                                                                  Env* env) {
  auto fe = std::make_unique<FunctionEnv>(env, name, &m_goos.reader);
  fe->set_segment(TOP_LEVEL_SEGMENT);

  Val* result = nullptr;
  try {
    result = compile_error_guard(code, fe.get());
  } catch (DebugFileDeclareException& de) {
    // (declare-file (debug)) will throw this exception. the reason for this is so that we can
    // wrap the entire source code in a (when *debug-segment* ... ) and compile that version
    // instead. therefore, it is recommended to put that declaration as early as possible so that
    // the compiler doesn't waste much time.
    // the actual source code is (top-level ...) right now though so we need some tricks.
    code.as_pair()->cdr = PairObject::make_new(
        PairObject::make_new(
            Object::make_symbol(&m_goos.reader.symbolTable, "when"),
            PairObject::make_new(Object::make_symbol(&m_goos.reader.symbolTable, "*debug-segment*"),
                                 code.as_pair()->cdr)),
        Object::make_empty_list());
    result = compile_error_guard(code, fe.get());
  }

  // only move to return register if we actually got a result
  if (!dynamic_cast<const None*>(result)) {
    fe->emit_ir<IR_Return>(code, fe->make_gpr(result->type()), result->to_gpr(code, fe.get()),
                           emitter::gRegInfo.get_gpr_ret_reg());
  }

  if (!fe->code().empty()) {
    fe->emit_ir<IR_Null>(code);
  }

  fe->finish();
  return fe;
}

Val* Compiler::compile_error_guard(const goos::Object& code, Env* env) {
  try {
    return compile(code, env);
  } catch (CompilerException& ce) {
    if (ce.print_err_stack) {
      bool term;
      auto loc_info = m_goos.reader.db.get_info_for(code, &term);
      if (term) {
        lg::print(fg(fmt::color::yellow) | fmt::emphasis::bold, "Function:\n");
        lg::print("{}\n", env->function_env()->name());
        lg::print(fg(fmt::color::yellow) | fmt::emphasis::bold, "Location:\n");
        lg::print("{}", loc_info);
      }

      lg::print(fg(fmt::color::yellow) | fmt::emphasis::bold, "Code:\n");
      auto code_str = pretty_print::to_string(code, 120);
      if (code_str.size() > 120 * 30) {
        code_str = code_str.substr(0, 120 * 30) + "...";
      }
      lg::print("{}\n", code_str);

      if (term) {
        ce.print_err_stack = false;
      }
      std::string line(80, '-');
      line.push_back('\n');
      lg::print("{}", line);
    }
    throw ce;
  }

  catch (std::runtime_error& e) {
    lg::print(fg(fmt::color::crimson) | fmt::emphasis::bold, "-- Compilation Error! --\n");
    lg::print(fmt::emphasis::bold, "{}\n", e.what());
    bool term;
    auto loc_info = m_goos.reader.db.get_info_for(code, &term);
    if (term) {
      lg::print(fg(fmt::color::yellow) | fmt::emphasis::bold, "Function:\n");
      lg::print("{}\n", env->function_env()->name());
      lg::print(fg(fmt::color::yellow) | fmt::emphasis::bold, "Location:\n");
      lg::print("{}", loc_info);
    }

    lg::print(fg(fmt::color::yellow) | fmt::emphasis::bold, "Code:\n");
    auto code_str = pretty_print::to_string(code, 120);
    if (code_str.size() > 120 * 30) {
      code_str = code_str.substr(0, 120 * 30) + "...";
    }
    lg::print("{}\n", code_str);

    CompilerException ce("Compiler Exception");
    if (term) {
      ce.print_err_stack = false;
    }
    std::string line(80, '-');
    line.push_back('\n');
    lg::print("{}", line);
    throw ce;
  }
}

void Compiler::color_object_file(FileEnv* env) {
  int num_spills_in_file = 0;
  for (auto& f : env->functions()) {
    AllocationInput input;
    input.is_asm_function = f->is_asm_func;
    for (auto& i : f->code()) {
      input.instructions.push_back(i->to_rai());
      // input.debug_instruction_names.push_back(i->print());
    }

    for (auto& reg_val : f->reg_vals()) {
      if (reg_val->forced_on_stack()) {
        input.force_on_stack_regs.insert(reg_val->ireg().id);
      }
    }

    input.max_vars = f->max_vars();
    input.constraints = f->constraints();
    input.stack_slots_for_stack_vars = f->stack_slots_used_for_stack_vars();
    input.function_name = f->name();

    if (m_settings.debug_print_regalloc) {
      input.debug_settings.print_input = true;
      input.debug_settings.print_result = true;
      input.debug_settings.print_analysis = true;
      input.debug_settings.allocate_log_level = 2;
    }

    m_debug_stats.total_funcs++;

    auto regalloc_result_2 = allocate_registers_v2(input);

    if (regalloc_result_2.ok) {
      if (regalloc_result_2.num_spilled_vars > 0) {
        // lg::print("Function {} has {} spilled vars.\n", f->name(),
        //  regalloc_result_2.num_spilled_vars);
      }
      num_spills_in_file += regalloc_result_2.num_spills;
      f->set_allocations(std::move(regalloc_result_2));
    } else {
      lg::print(
          "Warning: function {} failed register allocation with the v2 allocator. Falling back to "
          "the v1 allocator.\n",
          f->name());
      m_debug_stats.funcs_requiring_v1_allocator++;
      auto regalloc_result = allocate_registers(input);
      m_debug_stats.num_spills_v1 += regalloc_result.num_spills;
      num_spills_in_file += regalloc_result.num_spills;
      f->set_allocations(std::move(regalloc_result));
    }
  }

  m_debug_stats.num_spills += num_spills_in_file;
}

std::vector<u8> Compiler::codegen_object_file(FileEnv* env) {
  try {
    auto debug_info = &m_debugger.get_debug_info_for_object(env->name());
    debug_info->clear();
    CodeGenerator gen(env, debug_info, m_version);
    bool ok = true;
    auto result = gen.run(&m_ts);
    for (auto& f : env->functions()) {
      if (f->settings.print_asm) {
        lg::print("{}\n", debug_info->disassemble_function_by_name(f->name(), &ok, &m_goos.reader));
      }
    }
    auto stats = gen.get_obj_stats();
    m_debug_stats.num_moves_eliminated += stats.moves_eliminated;
    env->cleanup_after_codegen();
    return result;
  } catch (std::exception& e) {
    throw_compiler_error_no_code("Error during codegen: {}", e.what());
  }
  return {};
}

bool Compiler::codegen_and_disassemble_object_file(FileEnv* env,
                                                   std::vector<u8>* data_out,
                                                   std::string* asm_out,
                                                   bool omit_ir) {
  auto debug_info = &m_debugger.get_debug_info_for_object(env->name());
  debug_info->clear();
  CodeGenerator gen(env, debug_info, m_version);
  *data_out = gen.run(&m_ts);
  bool ok = true;
  *asm_out = debug_info->disassemble_all_functions(&ok, &m_goos.reader, omit_ir);
  return ok;
}

bool Compiler::connect_to_target() {
  if (!m_listener.is_connected()) {
    for (int i = 0; i < 1000; i++) {
      m_listener.connect_to_target();
      std::this_thread::sleep_for(std::chrono::microseconds(10000));
      if (m_listener.is_connected()) {
        break;
      }
    }
    if (!m_listener.is_connected()) {
      return false;
    }
  }
  return true;
}

void Compiler::typecheck(const goos::Object& form,
                         const TypeSpec& expected,
                         const TypeSpec& actual,
                         const std::string& error_message) {
  (void)form;
  if (!m_ts.typecheck_and_throw(expected, actual, error_message, false, false, true)) {
    throw_compiler_error(form, "Typecheck failed. For {}, got a \"{}\" when expecting a \"{}\"",
                         error_message, actual.print(), expected.print());
  }
}

/*!
 * Like typecheck, but will allow Val* to be #f if the destination isn't a number.
 * Also will convert to register types for the type checking.
 */
void Compiler::typecheck_reg_type_allow_false(const goos::Object& form,
                                              const TypeSpec& expected,
                                              const Val* actual,
                                              const std::string& error_message) {
  if (!m_ts.typecheck_and_throw(m_ts.make_typespec("number"), expected, "", false, false)) {
    auto as_sym_val = dynamic_cast<const SymbolVal*>(actual);
    if (as_sym_val && as_sym_val->name() == "#f") {
      return;
    }
  }
  typecheck(form, expected, coerce_to_reg_type(actual->type()), error_message);
}

void Compiler::setup_goos_forms() {
  m_goos.register_form("get-enum-vals", [&](const goos::Object& form, goos::Arguments& args,
                                            const std::shared_ptr<goos::EnvironmentObject>& env) {
    m_goos.eval_args(&args, env);
    va_check(form, args, {goos::ObjectType::SYMBOL}, {});
    std::vector<Object> enum_vals;

    const auto& enum_name = args.unnamed.at(0).as_symbol().name_ptr;
    auto enum_type = m_ts.try_enum_lookup(enum_name);
    if (!enum_type) {
      throw_compiler_error(form, "Unknown enum {} in get-enum-vals", enum_name);
    }

    std::vector<std::pair<std::string, s64>> sorted_values;
    for (auto& val : enum_type->entries()) {
      sorted_values.emplace_back(val.first,
                                 enum_type->is_bitfield() ? (u64)1 << val.second : val.second);
    }

    std::sort(sorted_values.begin(), sorted_values.end(),
              [](const std::pair<std::string, s64>& a, const std::pair<std::string, s64>& b) {
                return a.second < b.second;
              });

    for (auto& thing : sorted_values) {
      enum_vals.push_back(PairObject::make_new(m_goos.intern(thing.first),
                                               goos::Object::make_integer(thing.second)));
    }

    return goos::build_list(enum_vals);
  });
}

void Compiler::asm_file(const CompilationOptions& options) {
  // If the filename provided is not a valid path but it's a name (with or without an extension)
  // attempt to find it in the defined `asmFileSearchDirs`
  //
  // For example - (ml "process-drawable.gc")
  // - This allows you to load a file without precisely defining the entire path
  //
  // If multiple candidates are found, abort

  std::string file_name = options.filename;
  std::string file_path = file_util::get_file_path({file_name});

  if (!file_util::file_exists(file_path)) {
    if (file_path.empty()) {
      lg::print("ERROR - can't load a file without a providing a path\n");
      return;
    } else if (m_repl && m_repl->repl_config.asm_file_search_dirs.empty()) {
      lg::print(
          "ERROR - can't load a file that doesn't exist - '{}' and no search dirs are defined\n",
          file_path);
      return;
    }
    std::string base_name = file_util::base_name_no_ext(file_path);
    // Attempt the find the full path of the file (ignore extension)
    std::vector<fs::path> candidate_paths = {};
    if (m_repl) {
      for (const auto& dir : m_repl->repl_config.asm_file_search_dirs) {
        std::string base_dir = file_util::get_file_path({dir});
        const auto& results = file_util::find_files_recursively(
            base_dir, std::regex(fmt::format("^{}(\\..*)?$", base_name)));
        for (const auto& result : results) {
          candidate_paths.push_back(result);
        }
      }
    }

    if (candidate_paths.empty()) {
      lg::print("ERROR - attempt to find object file automatically, but found nothing\n");
      return;
    } else if (candidate_paths.size() > 1) {
      lg::print("ERROR - attempt to find object file automatically, but found multiple\n");
      return;
    }
    // Found the file!, use it!
    file_path = candidate_paths.at(0).string();
  }

  // Evict any symbols we have indexed for this file, this is what
  // helps to ensure we have an up to date and accurate symbol index
  m_symbol_info.evict_symbols_using_file_index(file_path);

  auto code = m_goos.reader.read_from_file({file_path});

  std::string obj_file_name = file_path;

  // Extract object name from file name.
  for (int idx = int(file_path.size()) - 1; idx-- > 0;) {
    if (file_path.at(idx) == '\\' || file_path.at(idx) == '/') {
      obj_file_name = file_path.substr(idx + 1);
      break;
    }
  }
  obj_file_name = obj_file_name.substr(0, obj_file_name.find_last_of('.'));

  // COMPILE
  auto obj_file = compile_object_file(obj_file_name, code, !options.no_code);

  if (options.color) {
    // register allocation
    color_object_file(obj_file);

    // code/object file generation
    std::vector<u8> data;
    std::string disasm;
    if (options.disassemble) {
      codegen_and_disassemble_object_file(obj_file, &data, &disasm, options.disasm_code_only);
      if (options.disassembly_output_file.empty()) {
        lg::print("{}\n", disasm);
      } else {
        file_util::write_text_file(options.disassembly_output_file, disasm);
      }
    } else {
      data = codegen_object_file(obj_file);
    }

    // send to target
    if (options.load) {
      if (m_listener.is_connected()) {
        m_listener.send_code(data, obj_file_name);
      } else {
        lg::print("WARNING - couldn't load because listener isn't connected\n");  // todo log warn
      }
    }

    // save file
    if (options.write) {
      auto path = file_util::get_jak_project_dir() / "out" / m_make.compiler_output_prefix() /
                  "obj" / (obj_file_name + ".o");
      file_util::create_dir_if_needed_for_file(path);
      file_util::write_binary_file(path, (void*)data.data(), data.size());
    }
  } else {
    if (options.load) {
      lg::print("WARNING - couldn't load because coloring is not enabled\n");
    }

    if (options.write) {
      lg::print("WARNING - couldn't write because coloring is not enabled\n");
    }

    if (options.disassemble) {
      lg::print("WARNING - couldn't disassemble because coloring is not enabled\n");
    }
  }
}
