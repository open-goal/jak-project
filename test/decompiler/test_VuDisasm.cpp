
#include "common/util/FileUtil.h"
#include "common/util/string_util.h"

#include "decompiler/VuDisasm/VuDisassembler.h"
#include "decompiler/util/DataParser.h"
#include "gtest/gtest.h"

#include "fmt/format.h"

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
  return str_util::trim(file_util::read_text_file(
      file_util::get_file_path({fmt::format("test/decompiler/vu_reference/{}-result.txt", name)})));
}
}  // namespace

// TEST(VuDisasm, DumpResults_JakX_Draft) {
//   struct VuData {
//     std::string name;
//     std::vector<u32> data;
//     VuDisassembler::VuKind kind;
//     std::string disasm;

//     VuData(const std::string& name, VuDisassembler::VuKind kind)
//         : name(name), data(get_test_data("jakx/" + name)), kind(kind) {
//       VuDisassembler vu_disasm(kind);
//       disasm = vu_disasm.to_string(vu_disasm.disassemble(data.data(), data.size() * 4, false));
//     }
//   };
//   auto path = file_util::get_file_path({"test/decompiler/vu_reference/jakx"});
//   for (const auto& entry : fs::directory_iterator(path)) {
//     if (entry.is_regular_file() &&
//         entry.path().filename().string().find("result") == std::string::npos) {
//       auto name = entry.path().filename().stem().string();
//       auto kind = name.find("vu0") != std::string::npos ? VuDisassembler::VuKind::VU0
//                                                         : VuDisassembler::VuKind::VU1;
//       VuData prog(name, kind);
//       file_util::write_text_file(path + "/" + prog.name + "-result.txt", prog.disasm);
//     }
//   }
// }

TEST(VuDisasm, ShadowVu0_JakX) {
  auto data = get_test_data("jakx/shadow-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/shadow-vu0"));
}

TEST(VuDisasm, ShadowVu1_JakX) {
  auto data = get_test_data("jakx/shadow-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/shadow-vu1"));
}

TEST(VuDisasm, OceanMid_JakX) {
  auto data = get_test_data("jakx/ocean-mid-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/ocean-mid-vu1"));
}

TEST(VuDisasm, OceanNear_JakX) {
  auto data = get_test_data("jakx/ocean-near-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/ocean-near-vu1"));
}

TEST(VuDisasm, MercnericVu0_JakX) {
  auto data = get_test_data("jakx/mercneric-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/mercneric-vu0"));
}

TEST(VuDisasm, GenericVu0_JakX) {
  auto data = get_test_data("jakx/generic-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/generic-vu0"));
}

TEST(VuDisasm, GenericVu1_JakX) {
  auto data = get_test_data("jakx/generic-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/generic-vu1"));
}

TEST(VuDisasm, Merc_JakX) {
  auto data = get_test_data("jakx/merc-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/merc-vu1"));
}

TEST(VuDisasm, Emerc_JakX) {
  auto data = get_test_data("jakx/emerc-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/emerc-vu1"));
}

TEST(VuDisasm, ShrubVu1_JakX) {
  auto data = get_test_data("jakx/shrub-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/shrub-vu1"));
}

TEST(VuDisasm, Sprite_JakX) {
  auto data = get_test_data("jakx/sprite-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/sprite-vu1"));
}

TEST(VuDisasm, SpriteDistort_JakX) {
  auto data = get_test_data("jakx/sprite-distort-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/sprite-distort-vu1"));
}

TEST(VuDisasm, SpriteGlow_JakX) {
  auto data = get_test_data("jakx/sprite-glow-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/sprite-glow-vu1"));
}

TEST(VuDisasm, Sparks_JakX) {
  auto data = get_test_data("jakx/sparks-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/sparks-vu1"));
}

TEST(VuDisasm, Tfrag_JakX) {
  auto data = get_test_data("jakx/tfrag-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/tfrag-vu1"));
}

TEST(VuDisasm, TfragNear_JakX) {
  auto data = get_test_data("jakx/tnear-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/tnear-vu1"));
}

TEST(VuDisasm, Tie_JakX) {
  auto data = get_test_data("jakx/tie-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/tie-vu1"));
}

TEST(VuDisasm, TieNear_JakX) {
  auto data = get_test_data("jakx/tie-near-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/tie-near-vu1"));
}

TEST(VuDisasm, Etie_JakX) {
  auto data = get_test_data("jakx/etie-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/etie-vu1"));
}

TEST(VuDisasm, EtieNear_JakX) {
  auto data = get_test_data("jakx/etn-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/etn-vu1"));
}

TEST(VuDisasm, BackgroundVu0_JakX) {
  auto data = get_test_data("jakx/background-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/background-vu0"));
}

TEST(VuDisasm, CollideVu0_JakX) {
  auto data = get_test_data("jakx/collide-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/collide-vu0"));
}

TEST(VuDisasm, BonesVu0_JakX) {
  auto data = get_test_data("jakx/bones-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/bones-vu0"));
}

TEST(VuDisasm, ForegroundVu0_JakX) {
  auto data = get_test_data("jakx/foreground-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jakx/foreground-vu0"));
}

// TEST(VuDisasm, DumpResults_Jak3_Draft) {
//   struct VuData {
//     std::string name;
//     std::vector<u32> data;
//     VuDisassembler::VuKind kind;
//     std::string disasm;

//     VuData(const std::string& name, VuDisassembler::VuKind kind)
//         : name(name), data(get_test_data("jak3/" + name)), kind(kind) {
//       VuDisassembler vu_disasm(kind);
//       disasm = vu_disasm.to_string(vu_disasm.disassemble(data.data(), data.size() * 4, false));
//     }
//   };
//   auto path = file_util::get_file_path({"test/decompiler/vu_reference/jak3"});
//   for (const auto& entry : fs::directory_iterator(path)) {
//     if (entry.is_regular_file() &&
//         entry.path().filename().string().find("result") == std::string::npos) {
//       auto name = entry.path().filename().stem().string();
//       auto kind = name.find("vu0") != std::string::npos ? VuDisassembler::VuKind::VU0
//                                                         : VuDisassembler::VuKind::VU1;
//       VuData prog(name, kind);
//       file_util::write_text_file(path + "/" + prog.name + "-result.txt", prog.disasm);
//     }
//   }
// }

TEST(VuDisasm, ShadowVu0_Jak3) {
  auto data = get_test_data("jak3/shadow-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/shadow-vu0"));
}

TEST(VuDisasm, ShadowVu1_Jak3) {
  auto data = get_test_data("jak3/shadow-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/shadow-vu1"));
}

TEST(VuDisasm, OceanTexture_Jak3) {
  auto data = get_test_data("jak3/ocean-texture-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/ocean-texture-vu1"));
}

TEST(VuDisasm, OceanMid_Jak3) {
  auto data = get_test_data("jak3/ocean-mid-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/ocean-mid-vu1"));
}

TEST(VuDisasm, OceanNear_Jak3) {
  auto data = get_test_data("jak3/ocean-near-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/ocean-near-vu1"));
}

TEST(VuDisasm, OceanVu0_Jak3) {
  auto data = get_test_data("jak3/ocean-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/ocean-vu0"));
}

TEST(VuDisasm, MercnericVu0_Jak3) {
  auto data = get_test_data("jak3/mercneric-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/mercneric-vu0"));
}

TEST(VuDisasm, GenericVu0_Jak3) {
  auto data = get_test_data("jak3/generic-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/generic-vu0"));
}

TEST(VuDisasm, GenericVu1_Jak3) {
  auto data = get_test_data("jak3/generic-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/generic-vu1"));
}

TEST(VuDisasm, Merc_Jak3) {
  auto data = get_test_data("jak3/merc-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/merc-vu1"));
}

TEST(VuDisasm, Emerc_Jak3) {
  auto data = get_test_data("jak3/emerc-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/emerc-vu1"));
}

TEST(VuDisasm, ShrubVu1_Jak3) {
  auto data = get_test_data("jak3/shrub-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/shrub-vu1"));
}

TEST(VuDisasm, Sprite_Jak3) {
  auto data = get_test_data("jak3/sprite-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/sprite-vu1"));
}

TEST(VuDisasm, SpriteDistort_Jak3) {
  auto data = get_test_data("jak3/sprite-distort-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/sprite-distort-vu1"));
}

TEST(VuDisasm, SpriteGlow_Jak3) {
  auto data = get_test_data("jak3/sprite-glow-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/sprite-glow-vu1"));
}

TEST(VuDisasm, Hfrag_Jak3) {
  auto data = get_test_data("jak3/hfrag-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/hfrag-vu1"));
}

TEST(VuDisasm, Tfrag_Jak3) {
  auto data = get_test_data("jak3/tfrag-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/tfrag-vu1"));
}

TEST(VuDisasm, TfragNear_Jak3) {
  auto data = get_test_data("jak3/tnear-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/tnear-vu1"));
}

TEST(VuDisasm, Tie_Jak3) {
  auto data = get_test_data("jak3/tie-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/tie-vu1"));
}

TEST(VuDisasm, TieNear_Jak3) {
  auto data = get_test_data("jak3/tie-near-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/tie-near-vu1"));
}

TEST(VuDisasm, Etie_Jak3) {
  auto data = get_test_data("jak3/etie-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/etie-vu1"));
}

TEST(VuDisasm, EtieNear_Jak3) {
  auto data = get_test_data("jak3/etn-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/etn-vu1"));
}

TEST(VuDisasm, BackgroundVu0_Jak3) {
  auto data = get_test_data("jak3/background-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/background-vu0"));
}

TEST(VuDisasm, CollideVu0_Jak3) {
  auto data = get_test_data("jak3/collide-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/collide-vu0"));
}

TEST(VuDisasm, BonesVu0_Jak3) {
  auto data = get_test_data("jak3/bones-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/bones-vu0"));
}

TEST(VuDisasm, ForegroundVu0_Jak3) {
  auto data = get_test_data("jak3/foreground-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak3/foreground-vu0"));
}

// TEST(VuDisasm, DumpResults_Jak2) {
//   struct VuData {
//     std::string name;
//     std::vector<u32> data;
//     VuDisassembler::VuKind kind;
//     std::string disasm;
//
//     VuData(const std::string& name, VuDisassembler::VuKind kind)
//         : name(name), data(get_test_data("jak2/" + name)), kind(kind) {
//       VuDisassembler vu_disasm(kind);
//       disasm = vu_disasm.to_string(vu_disasm.disassemble(data.data(), data.size() * 4, false));
//     }
//   };
//   auto path = file_util::get_file_path({"test/decompiler/vu_reference/jak2"});
//   for (const auto& entry : fs::directory_iterator(path)) {
//     if (entry.is_regular_file() &&
//         entry.path().filename().string().find("result") == std::string::npos) {
//       auto name = entry.path().filename().stem().string();
//       auto kind = name.find("vu0") != std::string::npos ? VuDisassembler::VuKind::VU0
//                                                         : VuDisassembler::VuKind::VU1;
//       VuData prog(name, kind);
//       file_util::write_text_file(path + "/" + prog.name + "-result.txt", prog.disasm);
//     }
//   }
// }

TEST(VuDisasm, ShadowVu0_Jak2) {
  auto data = get_test_data("jak2/shadow-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak2/shadow-vu0"));
}

TEST(VuDisasm, ShadowVu1_Jak2) {
  auto data = get_test_data("jak2/shadow-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak2/shadow-vu1"));
}

TEST(VuDisasm, OceanTexture_Jak2) {
  auto data = get_test_data("jak2/ocean-texture-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak2/ocean-texture-vu1"));
}

TEST(VuDisasm, OceanMid_Jak2) {
  auto data = get_test_data("jak2/ocean-mid-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak2/ocean-mid-vu1"));
}

TEST(VuDisasm, OceanNear_Jak2) {
  auto data = get_test_data("jak2/ocean-near-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak2/ocean-near-vu1"));
}

TEST(VuDisasm, OceanVu0_Jak2) {
  auto data = get_test_data("jak2/ocean-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak2/ocean-vu0"));
}

TEST(VuDisasm, Merc_Jak2) {
  auto data = get_test_data("jak2/merc-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak2/merc-vu1"));
}

TEST(VuDisasm, Emerc) {
  auto data = get_test_data("jak2/emerc-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak2/emerc-vu1"));
}

TEST(VuDisasm, Shrub_Jak2) {
  auto data = get_test_data("jak2/shrub-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak2/shrub-vu1"));
}

TEST(VuDisasm, Sprite_Jak2) {
  auto data = get_test_data("jak2/sprite-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak2/sprite-vu1"));
}

TEST(VuDisasm, SpriteDistort_Jak2) {
  auto data = get_test_data("jak2/sprite-distort-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak2/sprite-distort-vu1"));
}

TEST(VuDisasm, SpriteGlow_Jak2) {
  auto data = get_test_data("jak2/sprite-glow-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak2/sprite-glow-vu1"));
}

TEST(VuDisasm, Tie_Jak2) {
  auto data = get_test_data("jak2/tie-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak2/tie-vu1"));
}

TEST(VuDisasm, etie_Jak2) {
  auto data = get_test_data("jak2/etie-vu1");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak2/etie-vu1"));
}

TEST(VuDisasm, SpriteDistort) {
  auto data = get_test_data("jak1/sprite-distort");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak1/sprite-distort"));
}

TEST(VuDisasm, BackgroundVu0) {
  auto data = get_test_data("jak1/background-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak1/background-vu0"));
}

TEST(VuDisasm, CollideVu0) {
  auto data = get_test_data("jak1/collide-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak1/collide-vu0"));
}

TEST(VuDisasm, BonesVu0) {
  auto data = get_test_data("jak1/bones-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak1/bones-vu0"));
}

TEST(VuDisasm, BonesVu0_Jak2) {
  auto data = get_test_data("jak2/bones-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak2/bones-vu0"));
}

TEST(VuDisasm, ForegroundVu0_Jak2) {
  auto data = get_test_data("jak2/foreground-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak2/foreground-vu0"));
}

TEST(VuDisasm, ShadowVu0) {
  auto data = get_test_data("jak1/shadow-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak1/shadow-vu0"));
}

TEST(VuDisasm, OceanVu0) {
  auto data = get_test_data("jak1/ocean-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak1/ocean-vu0"));
}

TEST(VuDisasm, GenericVu0) {
  auto data = get_test_data("jak1/generic-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  // disasm.add_label_with_name(48, "JUMP_48");
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak1/generic-vu0"));
  // fmt::print("{}\n", disasm.to_string_with_cpp(prog, true));
}

TEST(VuDisasm, MercnericVu0) {
  auto data = get_test_data("jak1/mercneric-vu0");
  VuDisassembler disasm(VuDisassembler::VuKind::VU0);
  // disasm.add_label_with_name(314 - 280, "JUMP_314");
  // disasm.add_label_with_name(326 - 280, "JUMP_326");
  // disasm.add_label_with_name(353 - 280, "JUMP_353");
  // disasm.add_label_with_name(386 - 280, "JUMP_386");
  // disasm.add_label_with_name(427 - 280, "JUMP_427");
  // disasm.add_label_with_name(438 - 280, "JUMP_438");
  // disasm.add_label_with_name(454 - 280, "JUMP_454");

  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak1/mercneric-vu0"));
  // disasm.add_label_with_name(0, "vcallms_280");
  // disasm.add_label_with_name(303 - 280, "vcallms_303");

  // fmt::print("{}\n", disasm.to_string_with_cpp(prog, true));
}

TEST(VuDisasm, OceanTexture) {
  auto data = get_test_data("jak1/ocean-texture");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak1/ocean-texture"));
}

TEST(VuDisasm, OceanMid) {
  auto data = get_test_data("jak1/ocean-mid");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  /*
  disasm.add_label_with_name(41, "JUMP_41");
  disasm.add_label_with_name(43, "JUMP_43");
  disasm.add_label_with_name(46, "JUMP_46");
  disasm.add_label_with_name(73, "JUMP_73");
  disasm.add_label_with_name(107, "JUMP_107");
  disasm.add_label_with_name(275, "JUMP_275");
   */

  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak1/ocean-mid"));
}

TEST(VuDisasm, OceanNear) {
  auto data = get_test_data("jak1/ocean-near");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  // disasm.add_label_with_name(39, "JUMP_39");
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  // fmt::print("{}\n", disasm.to_string_with_cpp(prog, false));
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak1/ocean-near"));
}

TEST(VuDisasm, Sky) {
  auto data = get_test_data("jak1/sky");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak1/sky"));
}

TEST(VuDisasm, Shrub) {
  auto data = get_test_data("jak1/shrub");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak1/shrub"));
}

TEST(VuDisasm, Shadow) {
  auto data = get_test_data("jak1/shadow");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak1/shadow"));
}

TEST(VuDisasm, TNear) {
  auto data = get_test_data("jak1/tnear");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak1/tnear"));
}

TEST(VuDisasm, Sprite) {
  auto data = get_test_data("jak1/sprite");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak1/sprite"));
}

TEST(VuDisasm, Tie) {
  auto data = get_test_data("jak1/tie");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak1/tie"));
}

TEST(VuDisasm, Generic) {
  auto data = get_test_data("jak1/generic");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak1/generic"));
  // fmt::print("{}\n", disasm.to_string_with_cpp(prog, false));
}

TEST(VuDisasm, TieNear) {
  auto data = get_test_data("jak1/tie-near");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak1/tie-near"));
}

TEST(VuDisasm, Tfrag) {
  auto data = get_test_data("jak1/tfrag");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak1/tfrag"));
}

TEST(VuDisasm, Merc) {
  auto data = get_test_data("jak1/merc");
  VuDisassembler disasm(VuDisassembler::VuKind::VU1);
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);
  EXPECT_EQ(str_util::trim(disasm.to_string(prog)), get_expected("jak1/merc"));
}

TEST(VuDisasm, MercToC) {
  auto data = get_test_data("jak1/merc");
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
