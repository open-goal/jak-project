#include "gtest/gtest.h"
#include "goalc/emitter/CodeTester.h"
#include "goalc/emitter/IGen.h"

using namespace emitter;

TEST(EmitterExperimentalTest, VF_NOP) {
  CodeTester tester;
	tester.init_code_buffer(1024);
  tester.emit(IGen::nop_vf());

	// ASK - easy way to get the opcode listing (that you know is correct) other than hand-crafting it?
	EXPECT_EQ(tester.dump_to_hex_string(true), "D9D0");
}