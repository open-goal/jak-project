#include "gtest/gtest.h"
#include "common/type_system/TypeSystem.h"
#include "third-party/fmt/core.h"

TEST(TypeSystem, Construction) {
  TypeSystem ts;
  ts.add_builtin_types();
  fmt::print("{}", ts.print_all_type_information());
}

TEST(TypeSystem, DefaultMethods) {
  TypeSystem ts;
  ts.add_builtin_types();


  ts.assert_method_id("object", "new", 0);
  ts.assert_method_id("object", "delete", 1);
  ts.assert_method_id("object", "print", 2);
  ts.assert_method_id("object", "inspect", 3);
  ts.assert_method_id("object", "length", 4);
  ts.assert_method_id("object", "asize-of", 5);
  ts.assert_method_id("object", "copy", 6);
  ts.assert_method_id("object", "relocate", 7);
  ts.assert_method_id("object", "mem-usage", 8);
}