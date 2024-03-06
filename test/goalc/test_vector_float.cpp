#include <regex>
#include <stdexcept>
#include <string>
#include <thread>

#include "inja.hpp"

#include "goalc/compiler/Compiler.h"
#include "gtest/gtest.h"
#include "test/goalc/framework/test_runner.h"

#include "fmt/core.h"
#include "third-party/json.hpp"

class WithMinimalGameTests : public ::testing::Test {
 public:
  static void SetUpTestSuite() {
    shared_compiler = std::make_unique<SharedCompiler>(GameVersion::Jak1);
    try {
      shared_compiler->compiler.run_front_end_on_string("(build-kernel)");
    } catch (std::exception& e) {
      fprintf(stderr, "caught exception %s\n", e.what());
      EXPECT_TRUE(false);
    }
    shared_compiler->runtime_thread = std::thread(GoalTest::runtime_with_kernel_jak1);
    shared_compiler->runner.c = &shared_compiler->compiler;

    shared_compiler->compiler.run_test_from_string(
        "(dgo-load \"kernel\" global (link-flag output-load-msg output-load-true-msg execute-login "
        "print-login) #x200000)");

    const auto minimal_files = {"goal_src/jak1/engine/math/vector-h.gc"};
    for (auto& file : minimal_files) {
      shared_compiler->compiler.run_test_from_string(fmt::format("(ml \"{}\")", file));
    }

    shared_compiler->compiler.run_test_from_string("(set! *use-old-listener-print* #t)");
  }

  static void TearDownTestSuite() {
    shared_compiler->compiler.shutdown_target();
    shared_compiler->runtime_thread.join();
    shared_compiler.reset();
  }

  void SetUp() {
    GoalTest::createDirIfAbsent(GoalTest::getTemplateDir(testCategory));
    GoalTest::createDirIfAbsent(GoalTest::getGeneratedDir(testCategory));
  }

  void TearDown() {}

  struct SharedCompiler {
    SharedCompiler(GameVersion v) : compiler(v) {}
    std::thread runtime_thread;
    Compiler compiler;
    GoalTest::CompilerTestRunner runner;
  };

  static std::unique_ptr<SharedCompiler> shared_compiler;

  std::string testCategory = "with_game";
  inja::Environment env{GoalTest::getTemplateDir(testCategory),
                        GoalTest::getGeneratedDir(testCategory)};
};

std::unique_ptr<WithMinimalGameTests::SharedCompiler> WithMinimalGameTests::shared_compiler;

struct VectorFloatRegister {
  float x = 0;
  float y = 0;
  float z = 0;
  float w = 0;

  void setJson(nlohmann::json& data, std::string vectorKey) {
    data[fmt::format("{}x", vectorKey)] = x;
    data[fmt::format("{}y", vectorKey)] = y;
    data[fmt::format("{}z", vectorKey)] = z;
    data[fmt::format("{}w", vectorKey)] = w;
  }

  float getBroadcastElement(emitter::Register::VF_ELEMENT bc, float defValue) {
    switch (bc) {
      case emitter::Register::VF_ELEMENT::X:
        return x;
      case emitter::Register::VF_ELEMENT::Y:
        return y;
      case emitter::Register::VF_ELEMENT::Z:
        return z;
      case emitter::Register::VF_ELEMENT::W:
        return w;
      default:
        return defValue;
    }
  }

  std::string toGOALFormat() {
    std::string answer = fmt::format("({:.4f}, {:.4f}, {:.4f}, {:.4f})", x, y, z, w);
    // {fmt} formats negative 0 as "-0.000", just going to flip any negative zeros to positives as I
    // don't think is an OpenGOAL issue
    // Additionally, GOAL doesn't have -/+ Inf it seems, so replace with NaN. -nan is also just NaN
    return std::regex_replace(std::regex_replace(answer, std::regex("-0.0000"), "0.0000"),
                              std::regex("nan|inf|-nan|-inf"), "NaN");
  }

  std::string toGOALFormat(float) {
    std::string answer = fmt::format("{:.4f}", x);
    // {fmt} formats negative 0 as "-0.000", just going to flip any negative zeros to positives as I
    // don't think is an OpenGOAL issue
    // Additionally, GOAL doesn't have -/+ Inf it seems, so replace with NaN
    return std::regex_replace(std::regex_replace(answer, std::regex("-0.0000"), "0.0000"),
                              std::regex("nan|inf|-nan|-inf"), "NaN");
  }
};

struct VectorFloatTestCase {
  VectorFloatRegister dest = {11, 22, 33, 44};
  int destinationMask = -1;
  emitter::Register::VF_ELEMENT bc = emitter::Register::VF_ELEMENT::NONE;

  std::string getOperationBroadcast() {
    switch (bc) {
      case emitter::Register::VF_ELEMENT::X:
        return ".x";
      case emitter::Register::VF_ELEMENT::Y:
        return ".y";
      case emitter::Register::VF_ELEMENT::Z:
        return ".z";
      case emitter::Register::VF_ELEMENT::W:
        return ".w";
      default:
        return "";
    }
  }

  virtual VectorFloatRegister getExpectedResult() = 0;
  virtual void setJson(nlohmann::json& data, std::string func) = 0;

  virtual ~VectorFloatTestCase() = default;
};

struct VectorFloatTestCase_TwoOperand : VectorFloatTestCase {
  VectorFloatRegister input1 = {1.5, -1.5, 0.0, 100.5};
  VectorFloatRegister input2 = {-5.5, -0.0, 10.0, 7.5};

  std::function<float(float, float)> operation;

  VectorFloatRegister getExpectedResult() {
    VectorFloatRegister expectedResult;
    expectedResult.x = destinationMask & 0b0001
                           ? operation(input1.x, input2.getBroadcastElement(bc, input2.x))
                           : dest.x;
    expectedResult.y = destinationMask & 0b0010
                           ? operation(input1.y, input2.getBroadcastElement(bc, input2.y))
                           : dest.y;
    expectedResult.z = destinationMask & 0b0100
                           ? operation(input1.z, input2.getBroadcastElement(bc, input2.z))
                           : dest.z;
    expectedResult.w = destinationMask & 0b1000
                           ? operation(input1.w, input2.getBroadcastElement(bc, input2.w))
                           : dest.w;
    return expectedResult;
  }

  void setJson(nlohmann::json& data, std::string func) {
    input1.setJson(data, "v1");
    input2.setJson(data, "v2");
    dest.setJson(data, "dest");
    data["operation"] = fmt::format("{}", func);
    if (destinationMask == -1) {
      data["destinationMask"] = false;
    } else {
      data["destinationMask"] = fmt::format("{:b}", destinationMask);
    }
  }
};

std::vector<VectorFloatTestCase_TwoOperand> vectorMathCaseGen_TwoOperand() {
  std::vector<VectorFloatTestCase_TwoOperand> cases = {};
  for (int i = 0; i <= 15; i++) {
    VectorFloatTestCase_TwoOperand testCase = VectorFloatTestCase_TwoOperand();
    testCase.destinationMask = i;
    cases.push_back(testCase);
    // Re-add each case with each broadcast variant
    for (int j = 0; j < 4; j++) {
      VectorFloatTestCase_TwoOperand testCaseBC = VectorFloatTestCase_TwoOperand();
      testCaseBC.destinationMask = i;
      testCaseBC.bc = static_cast<emitter::Register::VF_ELEMENT>(j);
      cases.push_back(testCaseBC);
    }
  }
  return cases;
}

class VectorFloatParameterizedTestFixtureWithRunner_TwoOperand
    : public WithMinimalGameTests,
      public ::testing::WithParamInterface<VectorFloatTestCase_TwoOperand> {
 protected:
  std::string templateFile = "test-vector-math-2-operand.template.gc";
};

// NOTE - an excellent article -
// https://www.sandordargo.com/blog/2019/04/24/parameterized-testing-with-gtest

// --- 2 Operand VF Operations

TEST_P(VectorFloatParameterizedTestFixtureWithRunner_TwoOperand, VF_ADD_XYZW_DEST) {
  VectorFloatTestCase_TwoOperand testCase = GetParam();
  testCase.operation = [](float x, float y) { return x + y; };

  nlohmann::json data;
  testCase.setJson(data, fmt::format(".add{}.vf", testCase.getOperationBroadcast()));

  std::string outFile = shared_compiler->runner.test_file_name(
      fmt::format("vector-math-add{}-{{}}.generated.gc", testCase.getOperationBroadcast()));
  env.write(templateFile, data, outFile);
  shared_compiler->runner.run_test(
      testCategory, outFile, {fmt::format("{}\n0\n", testCase.getExpectedResult().toGOALFormat())});
}

TEST_P(VectorFloatParameterizedTestFixtureWithRunner_TwoOperand, VF_SUB_XYZW_DEST) {
  VectorFloatTestCase_TwoOperand testCase = GetParam();
  testCase.operation = [](float x, float y) { return x - y; };

  nlohmann::json data;
  testCase.setJson(data, fmt::format(".sub{}.vf", testCase.getOperationBroadcast()));

  std::string outFile = shared_compiler->runner.test_file_name(
      fmt::format("vector-math-sub{}-{{}}.generated.gc", testCase.getOperationBroadcast()));
  env.write(templateFile, data, outFile);
  shared_compiler->runner.run_test(
      testCategory, outFile, {fmt::format("{}\n0\n", testCase.getExpectedResult().toGOALFormat())});
}

TEST_P(VectorFloatParameterizedTestFixtureWithRunner_TwoOperand, VF_MUL_XYZW_DEST) {
  VectorFloatTestCase_TwoOperand testCase = GetParam();
  testCase.operation = [](float x, float y) { return x * y; };

  nlohmann::json data;
  testCase.setJson(data, fmt::format(".mul{}.vf", testCase.getOperationBroadcast()));

  std::string outFile = shared_compiler->runner.test_file_name(
      fmt::format("vector-math-mul{}-{{}}.generated.gc", testCase.getOperationBroadcast()));
  env.write(templateFile, data, outFile);
  shared_compiler->runner.run_test(
      testCategory, outFile, {fmt::format("{}\n0\n", testCase.getExpectedResult().toGOALFormat())});
}

TEST_P(VectorFloatParameterizedTestFixtureWithRunner_TwoOperand, VF_MIN_XYZW_DEST) {
  VectorFloatTestCase_TwoOperand testCase = GetParam();
  testCase.operation = [](float x, float y) { return fmin(x, y); };

  nlohmann::json data;
  testCase.setJson(data, fmt::format(".min{}.vf", testCase.getOperationBroadcast()));

  std::string outFile = shared_compiler->runner.test_file_name(
      fmt::format("vector-math-min{}-{{}}.generated.gc", testCase.getOperationBroadcast()));
  env.write(templateFile, data, outFile);
  shared_compiler->runner.run_test(
      testCategory, outFile, {fmt::format("{}\n0\n", testCase.getExpectedResult().toGOALFormat())});
}

TEST_P(VectorFloatParameterizedTestFixtureWithRunner_TwoOperand, VF_MAX_XYZW_DEST) {
  VectorFloatTestCase_TwoOperand testCase = GetParam();
  testCase.operation = [](float x, float y) { return fmax(x, y); };

  nlohmann::json data;
  testCase.setJson(data, fmt::format(".max{}.vf", testCase.getOperationBroadcast()));

  std::string outFile = shared_compiler->runner.test_file_name(
      fmt::format("vector-math-max{}-{{}}.generated.gc", testCase.getOperationBroadcast()));
  env.write(templateFile, data, outFile);
  shared_compiler->runner.run_test(
      testCategory, outFile, {fmt::format("{}\n0\n", testCase.getExpectedResult().toGOALFormat())});
}

INSTANTIATE_TEST_SUITE_P(WithGameTests_VectorFloatTests,
                         VectorFloatParameterizedTestFixtureWithRunner_TwoOperand,
                         ::testing::ValuesIn(vectorMathCaseGen_TwoOperand()));

// --- 1 Operand VF Operations

struct VectorFloatTestCase_SingleOperand : VectorFloatTestCase {
  VectorFloatRegister input1 = {1.5, -1.5, 0.0, 100.5};

  std::function<float(float)> operation;

  VectorFloatRegister getExpectedResult() {
    VectorFloatRegister expectedResult;
    expectedResult.x =
        destinationMask & 0b0001 ? operation(input1.getBroadcastElement(bc, input1.x)) : dest.x;
    expectedResult.y =
        destinationMask & 0b0010 ? operation(input1.getBroadcastElement(bc, input1.y)) : dest.y;
    expectedResult.z =
        destinationMask & 0b0100 ? operation(input1.getBroadcastElement(bc, input1.z)) : dest.z;
    expectedResult.w =
        destinationMask & 0b1000 ? operation(input1.getBroadcastElement(bc, input1.w)) : dest.w;
    return expectedResult;
  }

  void setJson(nlohmann::json& data, std::string func) {
    input1.setJson(data, "v1");
    dest.setJson(data, "dest");
    data["operation"] = fmt::format("{}", func);
    if (destinationMask == -1) {
      data["destinationMask"] = false;
    } else {
      data["destinationMask"] = fmt::format("{:b}", destinationMask);
    }
  }
};

std::vector<VectorFloatTestCase_SingleOperand> vectorMathCaseGen_SingleOperand_NoBroadcast() {
  std::vector<VectorFloatTestCase_SingleOperand> cases = {};
  for (int i = 0; i <= 15; i++) {
    VectorFloatTestCase_SingleOperand testCase = VectorFloatTestCase_SingleOperand();
    testCase.destinationMask = i;
    cases.push_back(testCase);
  }
  return cases;
}

class VectorFloatParameterizedTestFixtureWithRunner_SingleOperand
    : public WithMinimalGameTests,
      public ::testing::WithParamInterface<VectorFloatTestCase_SingleOperand> {
 protected:
  std::string templateFile = "test-vector-math-1-operand.template.gc";
};

TEST_P(VectorFloatParameterizedTestFixtureWithRunner_SingleOperand, VF_ABS_DEST) {
  VectorFloatTestCase_SingleOperand testCase = GetParam();
  testCase.operation = [](float x) { return fabs(x); };

  nlohmann::json data;
  testCase.setJson(data, ".abs.vf");

  std::string outFile = shared_compiler->runner.test_file_name("vector-math-abs-{}.generated.gc");
  env.write(templateFile, data, outFile);
  shared_compiler->runner.run_test(
      testCategory, outFile, {fmt::format("{}\n0\n", testCase.getExpectedResult().toGOALFormat())});
}

INSTANTIATE_TEST_SUITE_P(WithGameTests_VectorFloatTests,
                         VectorFloatParameterizedTestFixtureWithRunner_SingleOperand,
                         ::testing::ValuesIn(vectorMathCaseGen_SingleOperand_NoBroadcast()));

// --- 2 Operand With ACC VF Operations
// TODO - these pollute tests, it would be nicer long-term to move these into the framework
// namespace

struct VectorFloatTestCase_TwoOperandACC : VectorFloatTestCase {
  VectorFloatRegister input1 = {1.5, -1.5, 0.0, 100.5};
  VectorFloatRegister input2 = {-5.5, -0.0, 10.0, 7.5};
  VectorFloatRegister acc = {-15.5, -0.0, 20.0, 70.5};

  std::function<float(float, float, float)> operation;

  VectorFloatRegister getExpectedResult() {
    VectorFloatRegister expectedResult;
    expectedResult.x = destinationMask & 0b0001
                           ? operation(input1.x, input2.getBroadcastElement(bc, input2.x), acc.x)
                           : dest.x;
    expectedResult.y = destinationMask & 0b0010
                           ? operation(input1.y, input2.getBroadcastElement(bc, input2.y), acc.y)
                           : dest.y;
    expectedResult.z = destinationMask & 0b0100
                           ? operation(input1.z, input2.getBroadcastElement(bc, input2.z), acc.z)
                           : dest.z;
    expectedResult.w = destinationMask & 0b1000
                           ? operation(input1.w, input2.getBroadcastElement(bc, input2.w), acc.w)
                           : dest.w;
    return expectedResult;
  }

  void setJson(nlohmann::json& data, std::string func) {
    input1.setJson(data, "v1");
    input2.setJson(data, "v2");
    acc.setJson(data, "acc");
    dest.setJson(data, "dest");
    data["operation"] = fmt::format("{}", func);
    if (destinationMask == -1) {
      data["destinationMask"] = false;
    } else {
      data["destinationMask"] = fmt::format("{:b}", destinationMask);
    }
  }
};

// TODO - unnecessary duplication for these generation methods, use some templates (only the type
// changes)
std::vector<VectorFloatTestCase_TwoOperandACC> vectorMathCaseGen_TwoOperandACC() {
  std::vector<VectorFloatTestCase_TwoOperandACC> cases = {};
  for (int i = 0; i <= 15; i++) {
    VectorFloatTestCase_TwoOperandACC testCase = VectorFloatTestCase_TwoOperandACC();
    testCase.destinationMask = i;
    cases.push_back(testCase);
    // Re-add each case with each broadcast variant
    for (int j = 0; j < 4; j++) {
      VectorFloatTestCase_TwoOperandACC testCaseBC = VectorFloatTestCase_TwoOperandACC();
      testCaseBC.destinationMask = i;
      testCaseBC.bc = static_cast<emitter::Register::VF_ELEMENT>(j);
      cases.push_back(testCaseBC);
    }
  }
  return cases;
}

class VectorFloatParameterizedTestFixtureWithRunner_TwoOperandACC
    : public WithMinimalGameTests,
      public ::testing::WithParamInterface<VectorFloatTestCase_TwoOperandACC> {
 protected:
  std::string templateFile = "test-vector-math-2-operand-acc.template.gc";
};

TEST_P(VectorFloatParameterizedTestFixtureWithRunner_TwoOperandACC, VF_MUL_ADD_XYZW_DEST) {
  VectorFloatTestCase_TwoOperandACC testCase = GetParam();
  testCase.operation = [](float x, float y, float acc) { return (x * y) + acc; };

  nlohmann::json data;
  testCase.setJson(data, fmt::format(".add.mul{}.vf", testCase.getOperationBroadcast()));

  std::string outFile = shared_compiler->runner.test_file_name(
      fmt::format("vector-math-add-mul{}-{{}}.generated.gc", testCase.getOperationBroadcast()));
  env.write(templateFile, data, outFile);
  shared_compiler->runner.run_test(
      testCategory, outFile, {fmt::format("{}\n0\n", testCase.getExpectedResult().toGOALFormat())});
}

TEST_P(VectorFloatParameterizedTestFixtureWithRunner_TwoOperandACC, VF_MUL_SUB_XYZW_DEST) {
  VectorFloatTestCase_TwoOperandACC testCase = GetParam();
  testCase.operation = [](float x, float y, float acc) { return acc - (x * y); };

  nlohmann::json data;
  testCase.setJson(data, fmt::format(".sub.mul{}.vf", testCase.getOperationBroadcast()));

  std::string outFile = shared_compiler->runner.test_file_name(
      fmt::format("vector-math-sub-mul{}-{{}}.generated.gc", testCase.getOperationBroadcast()));
  env.write(templateFile, data, outFile);
  shared_compiler->runner.run_test(
      testCategory, outFile, {fmt::format("{}\n0\n", testCase.getExpectedResult().toGOALFormat())});
}

INSTANTIATE_TEST_SUITE_P(WithGameTests_VectorFloatTests,
                         VectorFloatParameterizedTestFixtureWithRunner_TwoOperandACC,
                         ::testing::ValuesIn(vectorMathCaseGen_TwoOperandACC()));

// ---- Two Operand Quotient Register Operations

struct VectorFloatTestCase_TwoOperandQuotient : VectorFloatTestCase {
  VectorFloatRegister input1 = {1.5, -1.5, 0.0, 100.5};
  VectorFloatRegister input2 = {-5.5, -0.0, 10.0, 10.0};

  int fsf = 0;
  int ftf = 0;

  std::function<float(float, float)> operation;

  VectorFloatRegister getExpectedResult() {
    float operand1 =
        input1.getBroadcastElement(static_cast<emitter::Register::VF_ELEMENT>(fsf), input1.x);
    float operand2 =
        input2.getBroadcastElement(static_cast<emitter::Register::VF_ELEMENT>(ftf), input2.x);
    float result = operation(operand1, operand2);
    VectorFloatRegister expectedResult;
    expectedResult.x = result;
    expectedResult.y = result;
    expectedResult.z = result;
    expectedResult.w = result;
    return expectedResult;
  }

  void setJson(nlohmann::json& data, std::string func) {
    input1.setJson(data, "v1");
    input2.setJson(data, "v2");
    dest.setJson(data, "dest");
    data["operation"] = fmt::format("{}", func);
    data["ftf"] = fmt::format("{:b}", ftf);
    data["fsf"] = fmt::format("{:b}", fsf);
  }
};

std::vector<VectorFloatTestCase_TwoOperandQuotient> vectorMathCaseGen_TwoOperandQuotient() {
  std::vector<VectorFloatTestCase_TwoOperandQuotient> cases = {};
  for (int i = 0; i <= 3; i++) {
    VectorFloatTestCase_TwoOperandQuotient testCase = VectorFloatTestCase_TwoOperandQuotient();
    testCase.fsf = i;
    for (int j = 0; j <= 3; j++) {
      testCase.ftf = j;
      cases.push_back(testCase);
    }
  }
  return cases;
}

class VectorFloatParameterizedTestFixtureWithRunner_TwoOperandQuotient
    : public WithMinimalGameTests,
      public ::testing::WithParamInterface<VectorFloatTestCase_TwoOperandQuotient> {
 protected:
  std::string templateFile = "test-vector-math-division.template.gc";
};

TEST_P(VectorFloatParameterizedTestFixtureWithRunner_TwoOperandQuotient, VF_DIV_FTF_FSF) {
  VectorFloatTestCase_TwoOperandQuotient testCase = GetParam();
  testCase.operation = [](float x, float y) { return x / y; };

  nlohmann::json data;
  testCase.setJson(data, ".div.vf");

  std::string outFile = shared_compiler->runner.test_file_name("vector-math-div-{}.generated.gc");
  env.write(templateFile, data, outFile);
  shared_compiler->runner.run_test(
      testCategory, outFile,
      {fmt::format("{}\n0\n",
                   testCase.getExpectedResult().toGOALFormat(testCase.getExpectedResult().x))});
}

INSTANTIATE_TEST_SUITE_P(WithGameTests_VectorFloatTests,
                         VectorFloatParameterizedTestFixtureWithRunner_TwoOperandQuotient,
                         ::testing::ValuesIn(vectorMathCaseGen_TwoOperandQuotient()));

// ---- Single Operand Quotient Register Operations

struct VectorFloatTestCase_OneOperandQuotient : VectorFloatTestCase {
  VectorFloatRegister input1 = {2, -2, 0.0, 100};

  int ftf = 0;

  std::function<float(float)> operation;

  VectorFloatRegister getExpectedResult() {
    float operand1 =
        input1.getBroadcastElement(static_cast<emitter::Register::VF_ELEMENT>(ftf), input1.x);
    float result = operation(operand1);
    VectorFloatRegister expectedResult;
    expectedResult.x = result;
    expectedResult.y = result;
    expectedResult.z = result;
    expectedResult.w = result;
    return expectedResult;
  }

  void setJson(nlohmann::json& data, std::string func) {
    input1.setJson(data, "v1");
    dest.setJson(data, "dest");
    data["operation"] = fmt::format("{}", func);
    data["ftf"] = fmt::format("{:b}", ftf);
  }
};

std::vector<VectorFloatTestCase_OneOperandQuotient> vectorMathCaseGen_OneOperandQuotient() {
  std::vector<VectorFloatTestCase_OneOperandQuotient> cases = {};
  for (int i = 0; i <= 3; i++) {
    VectorFloatTestCase_OneOperandQuotient testCase = VectorFloatTestCase_OneOperandQuotient();
    testCase.ftf = i;
    cases.push_back(testCase);
  }
  return cases;
}

class VectorFloatParameterizedTestFixtureWithRunner_OneOperandQuotient
    : public WithMinimalGameTests,
      public ::testing::WithParamInterface<VectorFloatTestCase_OneOperandQuotient> {
 protected:
  std::string templateFile = "test-vector-math-sqrt.template.gc";
};

TEST_P(VectorFloatParameterizedTestFixtureWithRunner_OneOperandQuotient, VF_SQRT_FTF) {
  VectorFloatTestCase_OneOperandQuotient testCase = GetParam();
  testCase.operation = [](float x) { return sqrt(x); };

  nlohmann::json data;
  testCase.setJson(data, ".sqrt.vf");

  std::string outFile = shared_compiler->runner.test_file_name("vector-math-sqrt-{}.generated.gc");
  env.write(templateFile, data, outFile);
  shared_compiler->runner.run_test(
      testCategory, outFile,
      {fmt::format("{}\n0\n",
                   testCase.getExpectedResult().toGOALFormat(testCase.getExpectedResult().x))});
}

INSTANTIATE_TEST_SUITE_P(WithGameTests_VectorFloatTests,
                         VectorFloatParameterizedTestFixtureWithRunner_OneOperandQuotient,
                         ::testing::ValuesIn(vectorMathCaseGen_OneOperandQuotient()));
