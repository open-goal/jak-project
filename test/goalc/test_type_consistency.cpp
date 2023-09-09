#include "goalc/compiler/Compiler.h"
#include "gtest/gtest.h"

void add_common_expected_type_mismatches(Compiler& c) {
  c.add_ignored_define_extern_symbol("draw-drawable-tree-tfrag");
  c.add_ignored_define_extern_symbol("draw-drawable-tree-trans-tfrag");
  c.add_ignored_define_extern_symbol("draw-drawable-tree-dirt-tfrag");
  c.add_ignored_define_extern_symbol("draw-drawable-tree-ice-tfrag");
  c.add_ignored_define_extern_symbol("tfrag-init-buffer");
}

void add_jak1_expected_type_mismatches(Compiler& c) {
  c.add_ignored_type_definition("game-option");
}

void add_jak2_expected_type_mismatches(Compiler& c) {
  c.add_ignored_type_definition("editable-plane");
}

// TODO - debatably delete these now that jak 1 is complete
TEST(Jak1TypeConsistency, MANUAL_TEST_TypeConsistencyWithBuildFirst) {
  Compiler compiler(GameVersion::Jak1);
  compiler.enable_throw_on_redefines();
  add_common_expected_type_mismatches(compiler);
  add_jak1_expected_type_mismatches(compiler);
  compiler.run_test_no_load("test/goalc/source_templates/with_game/test-build-all-code.gc");
  compiler.run_test_no_load("decompiler/config/jak1/all-types.gc");
}

// TODO - debatably delete these now that jak 1 is complete
TEST(Jak1TypeConsistency, TypeConsistency) {
  Compiler compiler(GameVersion::Jak1);
  compiler.enable_throw_on_redefines();
  add_common_expected_type_mismatches(compiler);
  add_jak1_expected_type_mismatches(compiler);
  compiler.run_test_no_load("decompiler/config/jak1/all-types.gc");
  compiler.run_test_no_load("test/goalc/source_templates/with_game/test-build-all-code.gc");
}

TEST(Jak2TypeConsistency, TypeConsistency) {
  Compiler compiler(GameVersion::Jak2);
  compiler.enable_throw_on_redefines();
  add_common_expected_type_mismatches(compiler);
  add_jak2_expected_type_mismatches(compiler);
  compiler.run_test_no_load("decompiler/config/jak2/all-types.gc");
  compiler.run_test_no_load("test/goalc/source_templates/with_game/test-build-all-code.gc");
}

TEST(Jak2TypeConsistency, MANUAL_TEST_TypeConsistencyWithBuildFirst) {
  Compiler compiler(GameVersion::Jak2);
  compiler.enable_throw_on_redefines();
  add_common_expected_type_mismatches(compiler);
  add_jak2_expected_type_mismatches(compiler);
  compiler.run_test_no_load("test/goalc/source_templates/with_game/test-build-all-code.gc");
  compiler.run_test_no_load("decompiler/config/jak2/all-types.gc");
}
