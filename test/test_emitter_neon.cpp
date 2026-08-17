#include "goalc/emitter/CodeTester.h"
#include "goalc/emitter/IGen.h"
#include "gtest/gtest.h"

using namespace emitter;

namespace {
const auto instr_set = InstructionSet::ARM64;

CodeTester create_tester(int code_capacity = 1024) {
  CodeTester tester(instr_set);
  tester.init_code_buffer(code_capacity);
  return tester;
}
};  // namespace

TEST(NEONEmitter, VF_NOP) {
  CodeTester tester = create_tester();
  tester.emit(IGen::nop_vf(tester.generator()));
  EXPECT_EQ(tester.dump_to_hex_string(true), "1F2003D5");
}

TEST(NEONEmitter, WAIT_VF) {
  CodeTester tester = create_tester();
  tester.emit(IGen::wait_vf(tester.generator()));
  EXPECT_EQ(tester.dump_to_hex_string(true), "1F2003D5");
}