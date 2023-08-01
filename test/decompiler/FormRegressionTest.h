#pragma once

#include <memory>

#include "decompiler/Disasm/InstructionParser.h"
#include "decompiler/Function/Function.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "gtest/gtest.h"

namespace decompiler {
struct RegisterTypeCast;
}

struct TestSettings {
  bool do_expressions = false;
  bool allow_pairs = false;
  std::string method_name;
  std::vector<std::pair<std::string, std::string>> strings;
  std::string casts_json;
  std::string var_map_json;
  std::string stack_structure_json;
  GameVersion version = GameVersion::Jak1;
};

class FormRegressionTest : public ::testing::TestWithParam<GameVersion> {
 protected:
  static std::unique_ptr<decompiler::InstructionParser> parser;
  static std::unique_ptr<decompiler::DecompilerTypeSystem> dts;

  static void TearDownTestCase();

  struct TestData {
    explicit TestData(int instrs, GameVersion version) : func(0, instrs, version), file(version) {}
    decompiler::Function func;
    decompiler::LinkedObjectFile file;

    void add_string_at_label(const std::string& label_name, const std::string& data);
  };

  std::unique_ptr<TestData> make_function(const std::string& code,
                                          const TypeSpec& function_type,
                                          const TestSettings& settings);
  void test(const std::string& code,
            const std::string& type,
            const std::string& expected,
            const TestSettings& settings);

  void test_final_function(const std::string& code,
                           const std::string& type,
                           const std::string& expected,
                           bool allow_pairs = false,
                           const std::vector<std::pair<std::string, std::string>>& strings = {},
                           const std::string& cast_json = "",
                           const std::string& var_map_json = "");

  void test_no_expr(const std::string& code,
                    const std::string& type,
                    const std::string& expected,
                    bool allow_pairs = false,
                    const std::string& method_name = "",
                    const std::vector<std::pair<std::string, std::string>>& strings = {},
                    const std::string& cast_json = "",
                    const std::string& var_map_json = "") {
    TestSettings settings;
    settings.allow_pairs = allow_pairs;
    settings.method_name = method_name;
    settings.strings = strings;
    settings.casts_json = cast_json;
    settings.var_map_json = var_map_json;
    settings.do_expressions = false;
    test(code, type, expected, settings);
  }

  void test_with_expr(const std::string& code,
                      const std::string& type,
                      const std::string& expected,
                      bool allow_pairs = false,
                      const std::string& method_name = "",
                      const std::vector<std::pair<std::string, std::string>>& strings = {},
                      const std::string& cast_json = "",
                      const std::string& var_map_json = "") {
    TestSettings settings;
    settings.allow_pairs = allow_pairs;
    settings.method_name = method_name;
    settings.strings = strings;
    settings.casts_json = cast_json;
    settings.var_map_json = var_map_json;
    settings.do_expressions = true;
    test(code, type, expected, settings);
  }

  void test_with_stack_structures(const std::string& code,
                                  const std::string& type,
                                  const std::string& expected,
                                  const std::string& stack_map_json,
                                  const std::string& cast_json = "",
                                  const std::string& var_map_json = "");
};

class FormRegressionTestJak1 : public FormRegressionTest {
 protected:
  static void SetUpTestCase();
};

class FormRegressionTestJak2 : public FormRegressionTest {
 protected:
  static void SetUpTestCase();
};
