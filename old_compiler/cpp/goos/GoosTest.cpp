#include <memory.h>
#include "GoosTest.h"

GoosTest::GoosTest(const std::string& filename) : file(filename) {}

bool GoosTest::run() {
  printf("-------------------------------------\n");

  try {
    auto test_program = goos.reader.read_from_file(file);
    goos.eval_with_rewind(test_program, goos.global_environment.as_env());
  } catch (std::runtime_error& e) {
    printf("Test in file %s threw exception:\n%s\n", file.c_str(), e.what());
    return false;
  }

  printf("Test file: %s\n", basename(file.c_str()));
  printf("Test name: %s\n", goos.get_object_by_name("*test-name*").print().c_str());
  Object lhs = goos.get_object_by_name("*test-expected*");
  Object rhs = goos.get_object_by_name("*test-actual*");
  if (lhs == rhs) {
    printf("got %s\n", lhs.print().c_str());
    return true;
  } else {
    printf("%s\nis not equal to\n%s\n", lhs.print().c_str(), rhs.print().c_str());
    return false;
  }
}

static const std::string test_prefix = "goal/gs/tests/";

bool run_all_tests() {
  std::vector<std::string> test_files;
  try {
    Goos goos;
    Object test_def_prog = goos.reader.read_from_file(test_prefix + "test-definition.gs");
    Object test_list = goos.eval_with_rewind(test_def_prog, goos.global_environment.as_env());

    Object o = test_list;
    for (;;) {
      if (o.type == PAIR) {
        auto op = o.as_pair();
        auto test_obj = op->car;
        if (test_obj.type != STRING) {
          throw std::runtime_error("invalid test name " + test_obj.print());
        }
        test_files.push_back(test_obj.as_string()->data);
        o = op->cdr;
      } else if (o.type == EMPTY_LIST) {
        break;
      } else {
        throw std::runtime_error("malformed test list");
      }
    }

  } catch (std::exception& e) {
    printf("failed to load test list: %s\n", e.what());
  }

  bool all_okay = true;
  std::vector<std::string> failed = {};
  for (const auto& test : test_files) {
    auto test_name = test_prefix + test;
    GoosTest goos_test(test_name);
    if (!goos_test.run()) {
      failed.push_back(test);
      all_okay = false;
    }
  }

  printf("-------------------------------------\n");
  if (all_okay) {
    printf("all tests passed!\n");
  } else {
    printf("failed tests:\n");
    for (const auto& fail : failed) {
      printf(" %s\n", fail.c_str());
    }
  }
  return all_okay;
}