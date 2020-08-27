/*!
 * @file Goal.cpp
 * The GOAL Compiler!
 */

#include <memory>
#include <cstring>
#include <unordered_map>

#include "Goal.h"
#include "codegen/Coloring.h"
#include "codegen/x86_Emitter.h"
#include "logger/Logger.h"
#include "util.h"

/*!
 * Initialize GOAL Compiler and load libraries
 */
Goal::Goal() {
  init_logger();

  // set up some type system stuff
  types.fill_with_default_types();
  get_none()->type = get_base_typespec("none");

  // the configuration is not loaded yet, so initialize configuration to the default:
  setup_default_config();

  // set up the global environment
  global_env = std::make_shared<GlobalEnv>();

  // read the GOAL library
  auto goal_lib = read_from_file("old_compiler/gc/goal-lib.gc");

  // setup environments for compiling the library
  auto library_env = std::make_shared<ObjectFileEnv>("init-env", global_env);
  auto library_f_env = std::make_shared<FunctionEnv>("init-func", library_env);

  // compile the GOAL library, with no coloring or object file generation
  compile_error_guard(goal_lib, library_f_env);
}

/*!
 * The REPL Loop.  This is kept as simple as possible!
 */
void Goal::execute_repl() {
  // read, evaluate, print loop!
  while (!want_exit) {
    try {
      // get a line from the user
      Object code = read_from_stdin_prompt(std::string("goal(") +
                                           (listener.is_connected() ? "c" : "n") + ")");

      // compile
      auto repl_obj_file = compile_object_file("repl", code);

      // early exit if there's nothing to send
      if (repl_obj_file->is_empty())
        continue;

      // color
      color_object_file(repl_obj_file);

      // emit
      auto data = codegen_object_file(repl_obj_file);

      // optionally print the function's IR for debugging.
      if (truthy(get_config("debug-print-ir"))) {
        for (auto& x : repl_obj_file->top_level_function->code) {
          gLogger.log(MSG_WARN, "%s\n", x->print().c_str());
        }
      }

      // send to target, if connected
      if (listener.is_connected()) {
        listener.send_code(data);

        // if the target died, receiver_got_ack will be false.
        if (!listener.receiver_got_ack) {
          gLogger.log(MSG_WARN, "Target did not respond!\n");
        }
      }

    } catch (std::exception& e) {
      printf("REPL Error: %s\n", e.what());
    }
  }

  // repl has ended, disconnect from target.
  listener.set_connected(false);
}

/*!
 * Shutdown GOAL
 */
Goal::~Goal() {
  gLogger.close();
}

/*!
 * Set logger settings.
 */
void Goal::init_logger() {
  gLogger.set_file("compiler.txt");
  gLogger.config[MSG_COLOR].kind = LOG_FILE;
  gLogger.config[MSG_DEBUG].kind = LOG_IGNORE;
  gLogger.config[MSG_TGT].color = COLOR_GREEN;
  gLogger.config[MSG_TGT_INFO].color = COLOR_BLUE;
  gLogger.config[MSG_WARN].color = COLOR_RED;
  gLogger.config[MSG_ICE].color = COLOR_RED;
  gLogger.config[MSG_ERR].color = COLOR_RED;
}

/*!
 * Compile an object file from code.
 */
std::shared_ptr<ObjectFileEnv> Goal::compile_object_file(const std::string& name,
                                                         const Object& code) {
  auto obj_env = std::make_shared<ObjectFileEnv>(name, global_env);

  // the top-level function is the function containing all statements in a file.
  obj_env->add_top_level_function(compile_top_level_function("top-level", code, obj_env));

  return obj_env;
}

/*!
 * Compile a function from code
 */
std::shared_ptr<FunctionEnv> Goal::compile_top_level_function(
    const std::string& name,
    const Object& code,
    std::shared_ptr<ObjectFileEnv> object) {
  auto func_env = std::make_shared<FunctionEnv>(name, object);
  func_env->segment = TOP_LEVEL_SEGMENT;

  // a temporary type, as we don't know the return type yet (but this is okay!)
  auto return_reg = func_env->alloc_reg(get_base_typespec("none"));

  // compile, resolve to GPR, and return
  auto return_ir = make_unique<IR_Return>(
      resolve_to_gpr(compile_error_guard(code, func_env), func_env), return_reg);

  // correct the type
  return_reg->type = return_ir->value->type;

  // and emit the final return.
  func_env->emit(std::move(return_ir));

  // clean up any gotos (which might jump to the return statement, so we do it here)
  func_env->finish();
  return func_env;
}

/*!
 * Create a valid coloring for a function, or throw an error.
 */
void Goal::color_function(const std::shared_ptr<FunctionEnv>& func) {
  if (!do_linear_scan_coloring(*func.get())) {
    ice("Coloring failed for function " + func->print());
  }
}

/*!
 * Color all functions in an object file
 */
void Goal::color_object_file(const std::shared_ptr<ObjectFileEnv>& obj) {
  for (auto& f : obj->functions) {
    color_function(f);
  }
}

/*!
 * Convert an object file with colored functions into a code blob to be loaded.
 */
std::vector<uint8_t> Goal::codegen_object_file(const std::shared_ptr<ObjectFileEnv>& obj) {
  x86_Emitter emitter;

  // give all static objects to the emitter
  for (auto& static_obj : obj->statics) {
    emitter.run(static_obj->object.get(), static_obj->object->segment);
  }

  // give all functions to the emitter
  for (auto& f : obj->functions) {
    f->first_instruction = emitter.run(f.get(), f->segment);
  }

  // run the emitter
  auto output = emitter.write();

  // debug print just the code sections
  if (truthy(get_config("debug-print-obj"))) {
    for (auto& s : output.code) {
      gLogger.log(MSG_WARN, "---\n");
      for (auto x : s) {
        gLogger.log(MSG_WARN, "%02x\n", x);
      }
    }
  }

  // combine all sections
  return output.to_vector();
}

/*!
 * Compile with an error stack checkpoint.
 */
std::shared_ptr<Place> Goal::compile_error_guard(Object obj, std::shared_ptr<GoalEnv> env) {
  try {
    return compile(obj, env);
  } catch (std::runtime_error& e) {
    printf(
        "------------------------------------------------------------------------------------------"
        "-\n");
    auto obj_print = obj.print();
    if (obj_print.length() > 80) {
      obj_print = obj_print.substr(0, 80);
      obj_print += "...";
    }
    printf("object: %s\nfrom  : %s\n", obj_print.c_str(), goos.reader.db.get_info_for(obj).c_str());
    throw e;
  }
}

/*!
 * Signal a compilation error.
 */
void Goal::throw_compile_error(Object o, const std::string& err) {
  gLogger.log(MSG_ERR, "[Error] Could not compile %s!\nReason: %s\n", o.print().c_str(),
              err.c_str());
  throw std::runtime_error(err);
}
