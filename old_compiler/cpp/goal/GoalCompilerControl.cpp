/*!
 * @file GoalCompileControl.cpp
 * GOAL Compiler Forms related to controlling the compiler.
 */

#include <unistd.h>
#include <string.h>
#include "Goal.h"
#include "Timer.h"

/*!
 * Enter an interactive GOOS REPL
 */
std::shared_ptr<Place> Goal::compile_gs(const Object& form,
                                        Object rest,
                                        std::shared_ptr<GoalEnv> env) {
  (void)env;
  if (rest.type != EMPTY_LIST) {
    throw_compile_error(form, "gs form should have no argumetns");
  }
  goos.execute_repl();
  return get_none();
}

/*!
 * Exit the compiler when finished compiling the current thing.
 */
std::shared_ptr<Place> Goal::compile_exit(const Object& form,
                                          Object rest,
                                          std::shared_ptr<GoalEnv> env) {
  (void)env;
  if (rest.type != EMPTY_LIST) {
    throw_compile_error(form, ":exit form should have no arguments");
  }

  // so DECI2 doesn't freak out when the connection is dropped.
  if (listener.is_connected()) {
    listener.send_reset();
  }

  want_exit = true;
  return get_none();
}

/*!
 * Compile a file.
 */
std::shared_ptr<Place> Goal::compile_asm_file(const Object& form,
                                              Object rest,
                                              std::shared_ptr<GoalEnv> env) {
  (void)env;
  int i = 0;
  std::string filename;
  bool load = false;
  bool color = false;
  bool write = false;

  std::vector<std::pair<std::string, float>> timing;
  Timer total_timer;

  for_each_in_list(rest, [&](Object o) {
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
      } else {
        throw_compile_error(form, "invalid option " + setting + " in asm-file form");
      }
    }
    i++;
  });

  Timer reader_timer;
  auto code = goos.reader.read_from_file(filename);
  timing.emplace_back("read", reader_timer.getMs());

  Timer compile_timer;
  std::string obj_file_name = basename(filename.c_str());
  obj_file_name = obj_file_name.substr(0, obj_file_name.find_last_of('.'));
  auto obj_file = compile_object_file(obj_file_name, code);
  timing.emplace_back("compile", compile_timer.getMs());

  if (color) {
    Timer color_timer;
    color_object_file(obj_file);
    timing.emplace_back("color", color_timer.getMs());

    Timer codegen_timer;
    auto data = codegen_object_file(obj_file);
    timing.emplace_back("codegen", codegen_timer.getMs());

    if (load) {
      if (listener.is_connected()) {
        listener.send_code(data);
      } else {
        printf("WARNING - couldn't load because listener isn't connected\n");
      }
    }

    if (write) {
      auto output_dir = as_string(get_constant_or_error(form, "*compiler-output-path*"));
      auto output_name = output_dir + obj_file_name + ".go";
      if (!write_to_binary_file(output_name, (void*)data.data(), data.size())) {
        printf("WARNING - failed to write output file!\n");
      }
    }
  } else {
    if (load) {
      printf("WARNING - couldn't load because coloring is not enabled\n");
    }

    if (write) {
      printf("WARNING - couldn't write because coloring is not enabled\n");
    }
  }

  if (truthy(get_config("print-asm-file-time"))) {
    for (auto& e : timing) {
      printf(" %12s %4.2f\n", e.first.c_str(), e.second);
    }
  }

  return get_none();
}

struct TestResult {
  std::string name, note, expected, actual;
  bool pass;
};

/*!
 * Run the built-in compiler tests
 */
std::shared_ptr<Place> Goal::compile_test(const Object& form,
                                          Object rest,
                                          std::shared_ptr<GoalEnv> env) {
  (void)form;
  (void)rest;
  (void)env;

  std::vector<TestResult> results;
  Timer all_test_timer;

  std::string test_prefix = as_string(get_constant_or_error(form, "*goal-test-prefix*"));
  auto test_list = get_constant_or_error(form, "*goal-test-files*");
  std::vector<std::string> tests;
  for_each_in_list(test_list, [&](Object o) { tests.push_back(as_string(o)); });

  for (auto& test : tests) {
    printf("Run test file %s\n", test.c_str());
    listener.clear_pending_incoming();
    Timer total_timer;
    auto test_file = test_prefix + test;
    TestResult result;
    result.name = test;

    Timer read_timer;
    auto test_code = goos.reader.read_from_file(test_file);
    auto read_time = read_timer.getMs();

    Timer compile_timer;
    auto test_ofe = compile_object_file(test_file, test_code);
    auto compile_time = compile_timer.getMs();

    Timer color_timer;
    color_object_file(test_ofe);
    auto color_time = color_timer.getMs();

    Timer codegen_timer;
    auto data = codegen_object_file(test_ofe);
    auto codegen_time = codegen_timer.getMs();

    Timer listener_send_timer;
    if (listener.is_connected()) {
      listener.send_code(data);
    } else {
      result.note = "Listener wasn't connected, so test didn't run";
      result.pass = false;
    }
    auto listener_send_time = listener_send_timer.getMs();

    Timer listener_wait_timer;
    int retry_count = 0;
    while (!listener.has_pending()) {
      usleep(1000);
      retry_count++;
      if (retry_count > 1000 && !listener.is_connected()) {
        printf("failed to get a result after a long wait, test has failed.\n");
        return get_none();
      }
    }
    auto listener_wait_time = listener_wait_timer.getMs();

    Timer check_timer;
    auto target_result = listener.pop_pending();
    result.actual = target_result;

    auto newline_pos = result.actual.find('\n');
    if (newline_pos != std::string::npos) {
      result.actual = result.actual.substr(0, newline_pos);
    }

    auto expected = goos.get_object_by_name("*test-expected*");
    result.expected = expected.print() + "\n";

    if (expected.type == SYMBOL && expected.as_symbol()->name == "automatic-pass") {
      result.pass = true;
    } else {
      int quote_count = 0;
      for (uint32_t i = 0; i < result.actual.size(); i++) {
        if (result.actual[i] == '"') {
          quote_count++;
        }

        if (quote_count == 2) {
          result.actual = result.actual.substr(0, i + 1);
          break;
        }
      }

      if (result.actual.back() != '\n')
        result.actual.push_back('\n');
      result.pass = result.actual == result.expected;
    }

    results.push_back(result);
    auto check_time = check_timer.getMs();

    printf("Time Summary:\n");
    printf(" read: %.3f ms\n", read_time);
    printf(" compile: %.3f ms\n", compile_time);
    printf(" color: %.3f ms\n", color_time);
    printf(" codegen: %.3f ms\n", codegen_time);
    printf(" send: %.3f ms\n", listener_send_time);
    printf(" run: %.3f ms\n", listener_wait_time);
    printf(" check: %.3f ms\n", check_time);
    printf(" total: %.3f ms\n", total_timer.getMs());
  }

  bool all_pass = true;
  for (auto& result : results) {
    printf("TEST %s\n", result.name.c_str());
    printf(" expected (%02ld): %s", result.expected.size(), result.expected.c_str());
    printf(" actual (%02ld): %s", result.actual.size(), result.actual.c_str());
    if (!result.note.empty()) {
      printf(" note: %s\n", result.note.c_str());
    }
    if (result.pass) {
      printf(" OK\n");
    } else {
      printf(" NG\n");
      all_pass = false;
    }
  }

  if (all_pass) {
    printf("All %ld tests pass in %.4f seconds!\n", tests.size(), all_test_timer.getSeconds());
  } else {
    printf("Failed tests:\n");
    for (auto& x : results) {
      if (!x.pass)
        printf(" %s\n", x.name.c_str());
    }
  }

  return get_none();
}

/*!
 * This ignores (in-package goal) at the top of GOAL files.  Real GOAL did this, so we do too.
 */
std::shared_ptr<Place> Goal::compile_in_package(const Object& form,
                                                Object rest,
                                                std::shared_ptr<GoalEnv> env) {
  (void)form;
  (void)rest;
  (void)env;
  return get_none();
}
