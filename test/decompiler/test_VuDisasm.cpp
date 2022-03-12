
#include "third-party/fmt/core.h"
#include "gtest/gtest.h"
#include "decompiler/util/DataParser.h"
#include "decompiler/VuDisasm/VuDisassembler.h"
#include "common/util/FileUtil.h"

using namespace decompiler;

namespace {
std::vector<u32> get_test_data(const std::string& name) {
  auto text = file_util::read_text_file(
      file_util::get_file_path({fmt::format("test/decompiler/vu_reference/{}.txt", name)}));

  auto parsed = parse_data(text);

  std::vector<u32> data;
  for (auto& w : parsed.words) {
    EXPECT_EQ(w.kind(), LinkedWord::Kind::PLAIN_DATA);
    data.push_back(w.data);
  }
  return data;
}

std::string get_expected(const std::string& name) {
  return file_util::read_text_file(
      file_util::get_file_path({fmt::format("test/decompiler/vu_reference/{}-result.txt", name)}));
}
}  // namespace

TEST(VuDisasm, SpriteDistort) {
  auto data = get_test_data("sprite-distort");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("sprite-distort"));
}

TEST(VuDisasm, BackgroundVu0) {
  auto data = get_test_data("background-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("background-vu0"));
}

TEST(VuDisasm, CollideVu0) {
  auto data = get_test_data("collide-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("collide-vu0"));
}

TEST(VuDisasm, BonesVu0) {
  auto data = get_test_data("bones-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("bones-vu0"));
}

TEST(VuDisasm, ShadowVu0) {
  auto data = get_test_data("shadow-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("shadow-vu0"));
}

TEST(VuDisasm, OceanVu0) {
  auto data = get_test_data("ocean-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("ocean-vu0"));
}

TEST(VuDisasm, GenericVu0) {
  auto data = get_test_data("generic-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  // disasm.add_label_with_name(48, "JUMP_48");
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("generic-vu0"));
  // fmt::print("{}\n", disasm.to_string_with_cpp(prog, true));
}

TEST(VuDisasm, MercnericVu0) {
  auto data = get_test_data("mercneric-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  // disasm.add_label_with_name(314 - 280, "JUMP_314");
  // disasm.add_label_with_name(326 - 280, "JUMP_326");
  // disasm.add_label_with_name(353 - 280, "JUMP_353");
  // disasm.add_label_with_name(386 - 280, "JUMP_386");
  // disasm.add_label_with_name(427 - 280, "JUMP_427");
  // disasm.add_label_with_name(438 - 280, "JUMP_438");
  // disasm.add_label_with_name(454 - 280, "JUMP_454");

  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("mercneric-vu0"));
  // disasm.add_label_with_name(0, "vcallms_280");
  // disasm.add_label_with_name(303 - 280, "vcallms_303");

  // fmt::print("{}\n", disasm.to_string_with_cpp(prog, true));
}

TEST(VuDisasm, OceanTexture) {
  auto data = get_test_data("ocean-texture");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("ocean-texture"));
}

TEST(VuDisasm, OceanMid) {
  auto data = get_test_data("ocean-mid");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("ocean-mid"));
}

// TEST(VuDisasm, OceanNear) {
//   auto data = get_test_data("ocean-near");
//   VuDisassembler disasm(VuDisassembler::VuKind::VU1);
//   auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
//   fmt::print("{}\n", disasm.to_string(prog));
//   // EXPECT_EQ(disasm.to_string(prog), get_expected("ocean-mid"));
// }

TEST(VuDisasm, Sky) {
  auto data = get_test_data("sky");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("sky"));
}

TEST(VuDisasm, Shrub) {
  auto data = get_test_data("shrub");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("shrub"));
}

TEST(VuDisasm, Shadow) {
  auto data = get_test_data("shadow");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("shadow"));
}

TEST(VuDisasm, TNear) {
  auto data = get_test_data("tnear");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("tnear"));
}

TEST(VuDisasm, Sprite) {
  auto data = get_test_data("sprite");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("sprite"));
}

TEST(VuDisasm, Tie) {
  auto data = get_test_data("tie");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("tie"));
}

TEST(VuDisasm, Generic) {
  auto data = get_test_data("generic");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("generic"));
  // fmt::print("{}\n", disasm.to_string_with_cpp(prog, false));
}

TEST(VuDisasm, TieNear) {
  auto data = get_test_data("tie-near");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("tie-near"));
}

TEST(VuDisasm, Tfrag) {
  auto data = get_test_data("tfrag");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("tfrag"));
}

TEST(VuDisasm, Merc) {
  auto data = get_test_data("merc");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("merc"));
}

TEST(VuDisasm, MercToC) {
  auto data = get_test_data("merc");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  disasm.add_label_with_name(0x1a1, "JUMP_1A1");
  disasm.add_label_with_name(0x48e, "JUMP_48E");
  disasm.add_label_with_name(0x539, "JUMP_539");
  disasm.add_label_with_name(0x243, "JUMP_243");
  disasm.add_label_with_name(20, "ENTER_20");
  disasm.add_label_with_name(35, "ENTER_35");
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  // fmt::print("{}\n", disasm.to_string_with_cpp(prog));
  // EXPECT_EQ(disasm.to_string_with_cpp(prog), get_expected("merc"));
}