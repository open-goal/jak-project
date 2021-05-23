
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
    EXPECT_EQ(w.kind, LinkedWord::Kind::PLAIN_DATA);
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
  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("sprite-distort"));
}

TEST(VuDisasm, BackgroundVu0) {
  auto data = get_test_data("background-vu0");
  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("background-vu0"));
}

TEST(VuDisasm, CollideVu0) {
  auto data = get_test_data("collide-vu0");
  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("collide-vu0"));
}

TEST(VuDisasm, BonesVu0) {
  auto data = get_test_data("bones-vu0");
  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("bones-vu0"));
}

TEST(VuDisasm, ShadowVu0) {
  auto data = get_test_data("shadow-vu0");
  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("shadow-vu0"));
}

TEST(VuDisasm, OceanVu0) {
  auto data = get_test_data("ocean-vu0");
  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("ocean-vu0"));
}

TEST(VuDisasm, GenericVu0) {
  auto data = get_test_data("generic-vu0");
  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("generic-vu0"));
}

TEST(VuDisasm, MercnericVu0) {
  auto data = get_test_data("mercneric-vu0");
  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("mercneric-vu0"));
}

TEST(VuDisasm, OceanTexture) {
  auto data = get_test_data("ocean-texture");
  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("ocean-texture"));
}

TEST(VuDisasm, Sky) {
  auto data = get_test_data("sky");
  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("sky"));
}

TEST(VuDisasm, Shrub) {
  auto data = get_test_data("shrub");
  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("shrub"));
}

TEST(VuDisasm, Shadow) {
  auto data = get_test_data("shadow");
  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("shadow"));
}

TEST(VuDisasm, TNear) {
  auto data = get_test_data("tnear");
  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("tnear"));
}

TEST(VuDisasm, Sprite) {
  auto data = get_test_data("sprite");
  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("sprite"));
}

TEST(VuDisasm, Tie) {
  auto data = get_test_data("tie");
  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("tie"));
}

TEST(VuDisasm, Generic) {
  auto data = get_test_data("generic");
  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("generic"));
}

TEST(VuDisasm, TieNear) {
  auto data = get_test_data("tie-near");
  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("tie-near"));
}

TEST(VuDisasm, Tfrag) {
  auto data = get_test_data("tfrag");
  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("tfrag"));
}

TEST(VuDisasm, Merc) {
  auto data = get_test_data("merc");
  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(disasm.to_string(prog), get_expected("merc"));
}