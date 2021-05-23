
#include "third-party/fmt/core.h"
#include "gtest/gtest.h"
#include "decompiler/util/DataParser.h"
#include "decompiler/VuDisasm/VuDisassembler.h"
#include "common/util/FileUtil.h"

using namespace decompiler;

namespace {
std::string get_test_case(const std::string& name) {
  return file_util::read_text_file(
      file_util::get_file_path({fmt::format("test/decompiler/vu_reference/{}.txt", name)}));
}
}  // namespace

TEST(VuDisasm, SpriteDistort) {
  auto parsed = parse_data(get_test_case("sprite-distort"));

  std::vector<u32> data;
  for (auto& w : parsed.words) {
    ASSERT_EQ(w.kind, LinkedWord::Kind::PLAIN_DATA);
    data.push_back(w.data);
  }

  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4);

  EXPECT_EQ(disasm.to_string(prog),
            "  lq.xyzw vf01, 489(vi00)    |  nop                      \n"  // some constant
            "  lq.xyzw vf05, 490(vi00)    |  nop                      \n"  // some constant
            "  ilw.x vi01, 511(vi00)      |  nop                      \n"  // outer loop counter
            "  iaddiu vi04, vi00, 0x200   |  nop                      \n"  // base for something
            "  iaddi vi02, vi00, 0x0      |  nop                      \n"  // prim data?
            "L2:\n"
            "  ilw.w vi07, 1(vi04)        |  nop                      \n"  // inner chain
            "  ior vi05, vi02, vi00       |  nop                      \n"  // incrementing store ptr
            "  ilw.x vi06, 477(vi07)      |  nop                      \n"  // data ptr
            "  sqi.xyzw vf01, vi05        |  nop                      \n"  // store gfx data
            "  iaddiu vi08, vi07, 0x4000  |  nop                      \n"  //
            "  iaddiu vi08, vi08, 0x4000  |  nop                      \n"
            "  isw.x vi08, -1(vi05)       |  nop                      \n"
            "  lqi.xyzw vf02, vi04        |  nop                      \n"
            "  lqi.xyzw vf03, vi04        |  nop                      \n"
            "  lqi.xyzw vf04, vi04        |  nop                      \n"
            "  nop                        |  ftoi4.xyzw vf14, vf02    \n"
            "L1:\n"
            "  lqi.xyzw vf06, vi06        |  nop                      \n"
            "  lqi.xyzw vf07, vi06        |  nop                      \n"
            "  lq.xyzw vf08, 0(vi06)      |  nop                      \n"
            "  lq.xyzw vf09, 1(vi06)      |  nop                      \n"
            "  iaddi vi07, vi07, -0x1     |  muly.xyzw vf10, vf06, vf04\n"
            "  nop                        |  mulz.xyzw vf11, vf07, vf04\n"
            "  nop                        |  muly.xyzw vf12, vf08, vf04\n"
            "  nop                        |  mulz.xyzw vf13, vf09, vf04\n"
            "  nop                        |  mulx.xyzw vf06, vf06, vf04\n"
            "  nop                        |  mulx.xyzw vf07, vf07, vf04\n"
            "  nop                        |  mulx.xyzw vf08, vf08, vf04\n"
            "  nop                        |  mulx.xyzw vf09, vf09, vf04\n"
            "  nop                        |  add.xyzw vf10, vf10, vf02\n"
            "  nop                        |  add.xyzw vf11, vf11, vf03\n"
            "  nop                        |  add.xyzw vf12, vf12, vf02\n"
            "  nop                        |  add.xyzw vf13, vf13, vf03\n"
            "  nop                        |  add.xyzw vf06, vf06, vf02\n"
            "  nop                        |  add.xyzw vf07, vf07, vf03\n"
            "  nop                        |  add.xyzw vf08, vf08, vf02\n"
            "  nop                        |  add.xyzw vf09, vf09, vf03\n"
            "  nop                        |  ftoi4.xyzw vf10, vf10    \n"
            "  nop                        |  ftoi4.xyzw vf12, vf12    \n"
            "  nop                        |  ftoi4.xyzw vf06, vf06    \n"
            "  nop                        |  ftoi4.xyzw vf08, vf08    \n"
            "  sqi.xyzw vf07, vi05        |  nop                      \n"
            "  sqi.xyzw vf05, vi05        |  nop                      \n"
            "  sqi.xyzw vf06, vi05        |  nop                      \n"
            "  sqi.xyzw vf09, vi05        |  nop                      \n"
            "  sqi.xyzw vf05, vi05        |  nop                      \n"
            "  sqi.xyzw vf08, vi05        |  nop                      \n"
            "  sqi.xyzw vf11, vi05        |  nop                      \n"
            "  sqi.xyzw vf05, vi05        |  nop                      \n"
            "  sqi.xyzw vf10, vi05        |  nop                      \n"
            "  sqi.xyzw vf13, vi05        |  nop                      \n"
            "  sqi.xyzw vf05, vi05        |  nop                      \n"
            "  sqi.xyzw vf12, vi05        |  nop                      \n"
            "  sqi.xyzw vf03, vi05        |  nop                      \n"
            "  sqi.xyzw vf05, vi05        |  nop                      \n"
            "  ibne vi00, vi07, L1        |  nop                      \n"
            "  sqi.xyzw vf14, vi05        |  nop                      \n"
            "  xgkick vi02                |  nop                      \n"  // go!
            "  iaddi vi01, vi01, -0x1     |  nop                      \n"
            "  iaddiu vi03, vi00, 0xb0    |  nop                      \n"
            "  ibne vi00, vi01, L2        |  nop                      \n"
            "  isub vi02, vi03, vi02      |  nop                      \n"
            "  nop                        |  nop :e                   \n"
            "  nop                        |  nop                      \n");
}

TEST(VuDisasm, BackgroundVu0) {
  auto parsed = parse_data(get_test_case("background-vu0"));

  std::vector<u32> data;
  for (auto& w : parsed.words) {
    ASSERT_EQ(w.kind, LinkedWord::Kind::PLAIN_DATA);
    data.push_back(w.data);
  }

  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);

  EXPECT_EQ(disasm.to_string(prog),
            "  sq.xyzw vf24, 4(vi00)      |  maxw.xyzw vf01, vf00, vf00\n"
            "  sq.xyzw vf25, 5(vi00)      |  nop                      \n"
            "  sq.xyzw vf26, 6(vi00)      |  nop                      \n"
            "  sq.xyzw vf27, 7(vi00)      |  nop                      \n"
            "  sq.xyzw vf16, 0(vi00)      |  mulz.xyzw vf24, vf01, vf24\n"
            "  sq.xyzw vf17, 1(vi00)      |  mulz.xyzw vf25, vf01, vf25\n"
            "  sq.xyzw vf18, 2(vi00)      |  mulz.xyzw vf26, vf01, vf26\n"
            "  sq.xyzw vf19, 3(vi00)      |  mulz.xyzw vf27, vf01, vf27\n"
            "  sq.xyzw vf24, 12(vi00)     |  nop                      \n"
            "  sq.xyzw vf25, 13(vi00)     |  nop                      \n"
            "  sq.xyzw vf26, 14(vi00)     |  nop                      \n"
            "  sq.xyzw vf27, 15(vi00)     |  nop                      \n"
            "  sq.xyzw vf28, 8(vi00)      |  nop                      \n"
            "  sq.xyzw vf29, 9(vi00)      |  nop                      \n"
            "  sq.xyzw vf30, 10(vi00)     |  nop                      \n"
            "  sq.xyzw vf31, 11(vi00)     |  nop :e                   \n"
            "  iaddiu vi02, vi00, 0xf0    |  nop                      \n"
            "  lq.xyzw vf16, 0(vi00)      |  nop                      \n"
            "  lq.xyzw vf17, 1(vi00)      |  nop                      \n"
            "  lq.xyzw vf18, 2(vi00)      |  nop                      \n"
            "  lq.xyzw vf19, 3(vi00)      |  nop                      \n"
            "  lq.xyzw vf24, 12(vi00)     |  nop                      \n"
            "  lq.xyzw vf25, 13(vi00)     |  nop                      \n"
            "  lq.xyzw vf26, 14(vi00)     |  nop :e                   \n"
            "  lq.xyzw vf27, 15(vi00)     |  nop                      \n"
            "  lq.xyzw vf16, 0(vi00)      |  nop                      \n"
            "  lq.xyzw vf17, 1(vi00)      |  nop                      \n"
            "  lq.xyzw vf18, 2(vi00)      |  nop :e                   \n"
            "  lq.xyzw vf19, 3(vi00)      |  nop                      \n"
            "  lq.xyzw vf24, 4(vi00)      |  nop                      \n"
            "  lq.xyzw vf25, 5(vi00)      |  nop                      \n"
            "  lq.xyzw vf26, 6(vi00)      |  nop :e                   \n"
            "  lq.xyzw vf27, 7(vi00)      |  nop                      \n"
            "  lq.xyzw vf24, 12(vi00)     |  mulax.xyzw ACC, vf16, vf02\n"
            "  lq.xyzw vf25, 13(vi00)     |  madday.xyzw ACC, vf17, vf02\n"
            "  lq.xyzw vf26, 14(vi00)     |  maddaz.xyzw ACC, vf18, vf02\n"
            "  lq.xyzw vf27, 15(vi00)     |  msubaw.xyzw ACC, vf19, vf00\n"
            "  nop                        |  maddw.xyzw vf04, vf01, vf02\n"
            "  nop                        |  mulax.xyzw ACC, vf24, vf02\n"
            "  nop                        |  madday.xyzw ACC, vf25, vf02\n"
            "  nop                        |  maddaz.xyzw ACC, vf26, vf02 :e\n"
            "  fmand vi01, vi02           |  maddw.xyzw vf06, vf27, vf00\n"
            "  lq.xyzw vf16, 0(vi00)      |  nop                      \n"
            "  lq.xyzw vf17, 1(vi00)      |  nop                      \n"
            "  lq.xyzw vf18, 2(vi00)      |  nop                      \n"
            "  lq.xyzw vf19, 3(vi00)      |  nop                      \n"
            "  lq.xyzw vf28, 8(vi00)      |  mulax.xyzw ACC, vf16, vf02\n"
            "  lq.xyzw vf29, 9(vi00)      |  madday.xyzw ACC, vf17, vf02\n"
            "  lq.xyzw vf30, 10(vi00)     |  maddaz.xyzw ACC, vf18, vf02\n"
            "  lq.xyzw vf31, 11(vi00)     |  msubaw.xyzw ACC, vf19, vf00\n"
            "  lq.xyzw vf24, 4(vi00)      |  maddw.xyzw vf04, vf01, vf02\n"
            "  lq.xyzw vf25, 5(vi00)      |  mulax.xyzw ACC, vf28, vf02\n"
            "  lq.xyzw vf26, 6(vi00)      |  madday.xyzw ACC, vf29, vf02\n"
            "  lq.xyzw vf27, 7(vi00)      |  maddaz.xyzw ACC, vf30, vf02\n"
            "  fmand vi01, vi02           |  maddw.xyzw vf05, vf31, vf00\n"
            "  nop                        |  mulax.xyzw ACC, vf24, vf02\n"
            "  nop                        |  madday.xyzw ACC, vf25, vf02\n"
            "  nop                        |  maddaz.xyzw ACC, vf26, vf02 :e\n"
            "  nop                        |  maddw.xyzw vf06, vf27, vf00\n");
}

TEST(VuDisasm, CollideVu0) {
  auto parsed = parse_data(get_test_case("collide-vu0"));

  std::vector<u32> data;
  for (auto& w : parsed.words) {
    ASSERT_EQ(w.kind, LinkedWord::Kind::PLAIN_DATA);
    data.push_back(w.data);
  }

  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);

  EXPECT_EQ(disasm.to_string(prog),
            "  nop                        |  mulaw.xyzw ACC, vf04, vf00\n"
            "  nop                        |  maddax.xyzw ACC, vf01, vf05\n"
            "  nop                        |  madday.xyzw ACC, vf02, vf05\n"
            "  nop                        |  maddz.xyz vf05, vf03, vf05\n"
            "  nop                        |  mulaw.xyzw ACC, vf04, vf00\n"
            "  nop                        |  maddax.xyzw ACC, vf01, vf06\n"
            "  nop                        |  madday.xyzw ACC, vf02, vf06\n"
            "  nop                        |  maddz.xyz vf06, vf03, vf06\n"
            "  nop                        |  mulaw.xyzw ACC, vf04, vf00\n"
            "  nop                        |  maddax.xyzw ACC, vf01, vf07\n"
            "  nop                        |  madday.xyzw ACC, vf02, vf07\n"
            "  nop                        |  maddz.xyz vf07, vf03, vf07\n"
            "  nop                        |  mulaw.xyzw ACC, vf04, vf00\n"
            "  nop                        |  maddax.xyzw ACC, vf01, vf08\n"
            "  nop                        |  madday.xyzw ACC, vf02, vf08\n"
            "  nop                        |  maddz.xyz vf08, vf03, vf08\n"
            "  nop                        |  subw.xyz vf09, vf05, vf05\n"
            "  nop                        |  subw.xyz vf11, vf06, vf06\n"
            "  nop                        |  subw.xyz vf13, vf07, vf07\n"
            "  nop                        |  subw.xyz vf15, vf08, vf08\n"
            "  nop                        |  addw.xyz vf10, vf05, vf05\n"
            "  nop                        |  addw.xyz vf12, vf06, vf06\n"
            "  nop                        |  addw.xyz vf14, vf07, vf07\n"
            "  nop                        |  addw.xyz vf16, vf08, vf08\n"
            "  nop                        |  ftoi0.xyzw vf09, vf09    \n"
            "  nop                        |  ftoi0.xyzw vf11, vf11    \n"
            "  nop                        |  ftoi0.xyzw vf13, vf13    \n"
            "  nop                        |  ftoi0.xyzw vf15, vf15    \n"
            "  nop                        |  ftoi0.xyzw vf10, vf10    \n"
            "  nop                        |  ftoi0.xyzw vf12, vf12    \n"
            "  nop                        |  ftoi0.xyzw vf14, vf14 :e \n"
            "  nop                        |  ftoi0.xyzw vf16, vf16    \n"
            "  nop                        |  itof0.xyzw vf20, vf20    \n"
            "  nop                        |  itof12.xyzw vf17, vf17   \n"
            "  nop                        |  itof12.xyzw vf18, vf18   \n"
            "  nop                        |  itof12.xyzw vf19, vf19   \n"
            "  nop                        |  add.xyz vf20, vf20, vf23 \n"
            "  nop                        |  itof12.xyzw vf21, vf21   \n"
            "  nop                        |  mulax.xyzw ACC, vf01, vf20\n"
            "  nop                        |  madday.xyzw ACC, vf02, vf20\n"
            "  nop                        |  maddaz.xyzw ACC, vf03, vf20\n"
            "  nop                        |  maddw.xyz vf20, vf04, vf00\n"
            "  nop                        |  mulax.xyzw ACC, vf01, vf17\n"
            "  nop                        |  madday.xyzw ACC, vf02, vf17\n"
            "  nop                        |  maddaz.xyzw ACC, vf03, vf17\n"
            "  nop                        |  maddx.xyz vf17, vf04, vf00\n"
            "  nop                        |  mulax.xyzw ACC, vf01, vf18\n"
            "  nop                        |  madday.xyzw ACC, vf02, vf18\n"
            "  nop                        |  maddaz.xyzw ACC, vf03, vf18\n"
            "  nop                        |  maddx.xyz vf18, vf04, vf00\n"
            "  nop                        |  mulax.xyzw ACC, vf01, vf19\n"
            "  nop                        |  madday.xyzw ACC, vf02, vf19\n"
            "  nop                        |  maddaz.xyzw ACC, vf03, vf19 :e\n"
            "  nop                        |  maddx.xyz vf19, vf04, vf00\n"
            "  nop                        |  mulaw.xyzw ACC, vf20, vf00\n"
            "  nop                        |  maddax.xyzw ACC, vf17, vf05\n"
            "  nop                        |  madday.xyzw ACC, vf18, vf05\n"
            "  nop                        |  maddz.xyzw vf09, vf19, vf05\n"
            "  nop                        |  mulaw.xyzw ACC, vf20, vf00\n"
            "  nop                        |  maddax.xyzw ACC, vf17, vf06\n"
            "  nop                        |  madday.xyzw ACC, vf18, vf06\n"
            "  nop                        |  maddz.xyzw vf11, vf19, vf06\n"
            "  nop                        |  mulaw.xyzw ACC, vf20, vf00\n"
            "  nop                        |  maddax.xyzw ACC, vf17, vf07\n"
            "  nop                        |  madday.xyzw ACC, vf18, vf07\n"
            "  nop                        |  maddz.xyzw vf13, vf19, vf07\n"
            "  nop                        |  mulaw.xyzw ACC, vf20, vf00\n"
            "  nop                        |  maddax.xyzw ACC, vf17, vf08\n"
            "  nop                        |  madday.xyzw ACC, vf18, vf08\n"
            "  nop                        |  maddz.xyzw vf15, vf19, vf08\n"
            "  nop                        |  mulw.x vf22, vf21, vf05  \n"
            "  nop                        |  mulw.y vf22, vf21, vf06  \n"
            "  nop                        |  mulw.z vf22, vf21, vf07  \n"
            "  nop                        |  mulw.w vf22, vf21, vf08  \n"
            "  nop                        |  addx.xyz vf10, vf09, vf22\n"
            "  nop                        |  subx.xyz vf09, vf09, vf22\n"
            "  nop                        |  addy.xyz vf12, vf11, vf22\n"
            "  nop                        |  suby.xyz vf11, vf11, vf22\n"
            "  nop                        |  addz.xyz vf14, vf13, vf22\n"
            "  nop                        |  subz.xyz vf13, vf13, vf22\n"
            "  nop                        |  addw.xyz vf16, vf15, vf22\n"
            "  nop                        |  subw.xyz vf15, vf15, vf22\n"
            "  nop                        |  ftoi0.xyzw vf10, vf10    \n"
            "  nop                        |  ftoi0.xyzw vf09, vf09    \n"
            "  nop                        |  ftoi0.xyzw vf12, vf12    \n"
            "  nop                        |  ftoi0.xyzw vf11, vf11    \n"
            "  nop                        |  ftoi0.xyzw vf14, vf14    \n"
            "  nop                        |  ftoi0.xyzw vf13, vf13    \n"
            "  nop                        |  ftoi0.xyzw vf16, vf16 :e \n"
            "  nop                        |  ftoi0.xyzw vf15, vf15    \n");
}

TEST(VuDisasm, BonesVu0) {
  auto parsed = parse_data(get_test_case("bones-vu0"));

  std::vector<u32> data;
  for (auto& w : parsed.words) {
    ASSERT_EQ(w.kind, LinkedWord::Kind::PLAIN_DATA);
    data.push_back(w.data);
  }

  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);

  EXPECT_EQ(disasm.to_string(prog),
            "  nop                        |  mulax.xyzw ACC, vf05, vf01\n"
            "  nop                        |  madday.xyzw ACC, vf06, vf01\n"
            "  nop                        |  maddaz.xyzw ACC, vf07, vf01\n"
            "  nop                        |  maddw.xyzw vf13, vf08, vf01\n"
            "  nop                        |  mulax.xyzw ACC, vf05, vf02\n"
            "  nop                        |  madday.xyzw ACC, vf06, vf02\n"
            "  nop                        |  maddaz.xyzw ACC, vf07, vf02\n"
            "  nop                        |  maddw.xyzw vf14, vf08, vf02\n"
            "  nop                        |  mulax.xyzw ACC, vf05, vf03\n"
            "  nop                        |  madday.xyzw ACC, vf06, vf03\n"
            "  nop                        |  maddaz.xyzw ACC, vf07, vf03\n"
            "  nop                        |  maddw.xyzw vf15, vf08, vf03\n"
            "  nop                        |  mulax.xyzw ACC, vf05, vf04\n"
            "  nop                        |  madday.xyzw ACC, vf06, vf04\n"
            "  nop                        |  maddaz.xyzw ACC, vf07, vf04\n"
            "  nop                        |  maddw.xyzw vf16, vf08, vf04\n"
            "  nop                        |  opmula.xyz ACC, vf14, vf15\n"
            "  nop                        |  opmsub.xyz vf09, vf15, vf14\n"
            "  nop                        |  opmula.xyz ACC, vf15, vf13\n"
            "  nop                        |  opmsub.xyz vf10, vf13, vf15\n"
            "  nop                        |  opmula.xyz ACC, vf13, vf14\n"
            "  nop                        |  mul.xyz vf12, vf13, vf09 \n"
            "  nop                        |  opmsub.xyz vf11, vf14, vf13\n"
            "  nop                        |  mulax.xyzw ACC, vf28, vf13\n"
            "  nop                        |  madday.xyzw ACC, vf29, vf13\n"
            "  nop                        |  maddaz.xyzw ACC, vf30, vf13\n"
            "  nop                        |  maddw.xyzw vf13, vf31, vf13\n"
            "  nop                        |  mulax.w ACC, vf00, vf12  \n"
            "  nop                        |  madday.w ACC, vf00, vf12 \n"
            "  nop                        |  maddz.w vf12, vf00, vf12 \n"
            "  nop                        |  mulax.xyzw ACC, vf28, vf14\n"
            "  nop                        |  madday.xyzw ACC, vf29, vf14\n"
            "  nop                        |  maddaz.xyzw ACC, vf30, vf14\n"
            "  div Q, vf00.w, vf12.w      |  maddw.xyzw vf14, vf31, vf14\n"
            "  nop                        |  mulax.xyzw ACC, vf28, vf15\n"
            "  nop                        |  madday.xyzw ACC, vf29, vf15\n"
            "  nop                        |  maddaz.xyzw ACC, vf30, vf15\n"
            "  nop                        |  maddw.xyzw vf15, vf31, vf15\n"
            "  nop                        |  mulax.xyzw ACC, vf28, vf16\n"
            "  nop                        |  madday.xyzw ACC, vf29, vf16\n"
            "  nop                        |  maddaz.xyzw ACC, vf30, vf16\n"
            "  nop                        |  maddw.xyzw vf16, vf31, vf16\n"
            "  nop                        |  mul.xyzw vf09, vf09, Q   \n"
            "  nop                        |  mul.xyzw vf10, vf10, Q   \n"
            "  nop                        |  mul.xyzw vf11, vf11, Q   \n"
            "  nop                        |  mulax.xyzw ACC, vf25, vf09\n"
            "  nop                        |  madday.xyzw ACC, vf26, vf09\n"
            "  nop                        |  maddz.xyzw vf09, vf27, vf09\n"
            "  nop                        |  mulax.xyzw ACC, vf25, vf10\n"
            "  nop                        |  madday.xyzw ACC, vf26, vf10\n"
            "  nop                        |  maddz.xyzw vf10, vf27, vf10\n"
            "  nop                        |  mulax.xyzw ACC, vf25, vf11\n"
            "  nop                        |  madday.xyzw ACC, vf26, vf11 :e\n"
            "  nop                        |  maddz.xyzw vf11, vf27, vf11\n"
            "  nop                        |  mulax.xyzw ACC, vf01, vf04\n"
            "  nop                        |  madday.xyzw ACC, vf02, vf04\n"
            "  nop                        |  maddz.xyzw vf07, vf03, vf04\n"
            "  nop                        |  mulax.xyzw ACC, vf01, vf05\n"
            "  nop                        |  madday.xyzw ACC, vf02, vf05\n"
            "  nop                        |  maddz.xyzw vf08, vf03, vf05\n"
            "  nop                        |  mulax.xyzw ACC, vf01, vf06\n"
            "  nop                        |  madday.xyzw ACC, vf02, vf06 :e\n"
            "  nop                        |  maddz.xyzw vf09, vf03, vf06\n");
}

TEST(VuDisasm, ShadowVu0) {
  auto parsed = parse_data(get_test_case("shadow-vu0"));

  std::vector<u32> data;
  for (auto& w : parsed.words) {
    ASSERT_EQ(w.kind, LinkedWord::Kind::PLAIN_DATA);
    data.push_back(w.data);
  }

  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);

  EXPECT_EQ(disasm.to_string(prog),
            "  nop                        |  sub.xyzw vf05, vf03, vf02\n"
            "  nop                        |  sub.xyzw vf06, vf04, vf02\n"
            "  nop                        |  sub.xyzw vf10, vf08, vf07\n"
            "  nop                        |  sub.xyzw vf11, vf09, vf07\n"
            "  nop                        |  sub.xyzw vf15, vf13, vf12\n"
            "  nop                        |  sub.xyzw vf16, vf14, vf12\n"
            "  nop                        |  sub.xyzw vf20, vf18, vf17\n"
            "  nop                        |  sub.xyzw vf21, vf19, vf17\n"
            "  nop                        |  opmula.xyz ACC, vf05, vf06\n"
            "  nop                        |  opmsub.xyz vf05, vf06, vf05\n"
            "  nop                        |  opmula.xyz ACC, vf10, vf11\n"
            "  nop                        |  opmsub.xyz vf10, vf11, vf10\n"
            "  nop                        |  opmula.xyz ACC, vf15, vf16\n"
            "  nop                        |  opmsub.xyz vf15, vf16, vf15\n"
            "  nop                        |  opmula.xyz ACC, vf20, vf21\n"
            "  nop                        |  opmsub.xyz vf20, vf21, vf20\n"
            "  nop                        |  mul.xyz vf05, vf05, vf01 \n"
            "  nop                        |  mul.xyz vf10, vf10, vf01 \n"
            "  nop                        |  mul.xyz vf15, vf15, vf01 \n"
            "  nop                        |  mul.xyz vf20, vf20, vf01 \n"
            "  nop                        |  addx.y vf05, vf05, vf05  \n"
            "  nop                        |  addx.y vf10, vf10, vf10  \n"
            "  nop                        |  addx.y vf15, vf15, vf15  \n"
            "  nop                        |  addx.y vf20, vf20, vf20  \n"
            "  nop                        |  addz.y vf22, vf05, vf05  \n"
            "  nop                        |  addz.y vf23, vf10, vf10  \n"
            "  nop                        |  addz.y vf24, vf15, vf15 :e\n"
            "  nop                        |  addz.y vf25, vf20, vf20  \n"
            "  nop                        |  mul.xyzw vf27, vf20, Q   \n"
            "  div Q, vf13.x, vf17.x      |  sub.xyzw vf19, vf01, vf03\n"
            "  move.xyzw vf23, vf07       |  sub.xyzw vf20, vf01, vf04\n"
            "  nop                        |  sub.xyzw vf21, vf01, vf05\n"
            "  move.xyzw vf25, vf09       |  sub.xyzw vf22, vf01, vf06\n"
            "  move.xyzw vf26, vf10       |  sub.xyzw vf24, vf08, vf27\n"
            "  nop                        |  mul.xyzw vf11, vf03, vf02\n"
            "  nop                        |  mul.xyz vf15, vf19, vf02 \n"
            "  div Q, vf14.x, vf18.x      |  mul.xyzw vf12, vf04, vf02\n"
            "  move.xyzw vf07, vf03       |  mul.xyzw vf28, vf28, Q   \n"
            "  move.xyzw vf08, vf04       |  mul.xyz vf16, vf20, vf02 \n"
            "  move.xyzw vf09, vf05       |  addy.x vf11, vf11, vf11  \n"
            "  move.xyzw vf10, vf06       |  addy.x vf15, vf15, vf15  \n"
            "  nop                        |  sub.xyzw vf25, vf25, vf28\n"
            "  nop                        |  addy.x vf12, vf12, vf12  \n"
            "  nop                        |  mul.xyzw vf29, vf29, Q   \n"
            "  nop                        |  addy.x vf16, vf16, vf16  \n"
            "  nop                        |  addz.x vf11, vf11, vf11  \n"
            "  nop                        |  addz.x vf15, vf15, vf15  \n"
            "  nop                        |  sub.xyzw vf26, vf26, vf29\n"
            "  nop                        |  addz.x vf12, vf12, vf12  \n"
            "  nop                        |  addz.x vf16, vf16, vf16  \n"
            "  nop                        |  addw.x vf11, vf11, vf11  \n"
            "  nop                        |  mul.xyzw vf13, vf09, vf02\n"
            "  nop                        |  addw.x vf12, vf12, vf12  \n"
            "  nop                        |  mul.xyz vf17, vf21, vf02 \n"
            "  nop                        |  mul.xyzw vf14, vf10, vf02\n"
            "  div Q, vf11.x, vf15.x      |  mul.xyz vf18, vf22, vf02 \n"
            "  nop                        |  addy.x vf13, vf13, vf13  \n"
            "  nop                        |  addy.x vf17, vf17, vf17  \n"
            "  nop                        |  addy.x vf14, vf14, vf14  \n"
            "  nop                        |  addy.x vf18, vf18, vf18  \n"
            "  nop                        |  addz.x vf13, vf13, vf13  \n"
            "  nop                        |  addz.x vf17, vf17, vf17  \n"
            "  div Q, vf12.x, vf16.x      |  addz.x vf14, vf14, vf14  \n"
            "  nop                        |  mul.xyzw vf19, vf19, Q   \n"
            "  move.xyzw vf28, vf21       |  addz.x vf18, vf18, vf18  \n"
            "  move.xyzw vf29, vf22       |  addw.x vf13, vf13, vf13  \n"
            "  nop                        |  addw.x vf14, vf14, vf14 :e\n"
            "  nop                        |  sub.xyzw vf07, vf07, vf19\n"
            "  nop                        |  mul.xyzw vf27, vf20, Q   \n"
            "  div Q, vf13.x, vf17.x      |  nop                      \n"
            "  move.xyzw vf23, vf07       |  nop                      \n"
            "  nop                        |  nop                      \n"
            "  move.xyzw vf25, vf09       |  nop                      \n"
            "  move.xyzw vf26, vf10       |  sub.xyzw vf24, vf08, vf27\n"
            "  nop                        |  nop                      \n"
            "  nop                        |  nop                      \n"
            "  div Q, vf14.x, vf18.x      |  nop                      \n"
            "  nop                        |  mul.xyzw vf28, vf28, Q   \n"
            "  nop                        |  nop                      \n"
            "  nop                        |  nop                      \n"
            "  nop                        |  nop                      \n"
            "  nop                        |  sub.xyzw vf25, vf25, vf28\n"
            "  nop                        |  nop                      \n"
            "  nop                        |  mul.xyzw vf29, vf29, Q   \n"
            "  nop                        |  nop                      \n"
            "  nop                        |  nop                      \n"
            "  nop                        |  nop :e                   \n"
            "  nop                        |  sub.xyzw vf26, vf26, vf29\n");
}

TEST(VuDisasm, OceanVu0) {
  auto parsed = parse_data(get_test_case("ocean-vu0"));

  std::vector<u32> data;
  for (auto& w : parsed.words) {
    ASSERT_EQ(w.kind, LinkedWord::Kind::PLAIN_DATA);
    data.push_back(w.data);
  }

  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);

  EXPECT_EQ(disasm.to_string(prog),
            "  nop                        |  mulay.x ACC, vf12, vf02  \n"
            "  nop                        |  mulax.z ACC, vf12, vf03  \n"
            "  nop                        |  sub.xz vf24, vf12, vf02  \n"
            "  nop                        |  mulaz.x ACC, vf12, vf02  \n"
            "  nop                        |  mulay.z ACC, vf12, vf03  \n"
            "  nop                        |  sub.xz vf25, vf12, vf02  \n"
            "  nop                        |  mulaw.x ACC, vf12, vf02  \n"
            "  nop                        |  mulaz.z ACC, vf12, vf03  \n"
            "  nop                        |  sub.xz vf26, vf12, vf02  \n"
            "  nop                        |  mulax.x ACC, vf12, vf04  \n"
            "  nop                        |  mulaw.z ACC, vf12, vf03  \n"
            "  nop                        |  sub.xz vf27, vf12, vf02  \n"
            "  nop                        |  mul.xz vf28, vf24, vf24  \n"
            "  nop                        |  mul.xz vf29, vf25, vf25  \n"
            "  nop                        |  mul.xz vf30, vf26, vf26  \n"
            "  nop                        |  mul.xz vf31, vf27, vf27  \n"
            "  nop                        |  subx.y vf24, vf01, vf28  \n"
            "  nop                        |  subx.y vf25, vf01, vf29  \n"
            "  nop                        |  subx.y vf26, vf01, vf30  \n"
            "  nop                        |  subx.y vf27, vf01, vf31  \n"
            "  nop                        |  subz.y vf24, vf24, vf28  \n"
            "  nop                        |  subz.y vf25, vf25, vf29  \n"
            "  nop                        |  subz.y vf26, vf26, vf30  \n"
            "  nop                        |  subz.y vf27, vf27, vf31  \n"
            "  nop                        |  mulx.w vf24, vf01, vf02  \n"
            "  nop                        |  muly.w vf25, vf01, vf02  \n"
            "  nop                        |  mulz.w vf26, vf01, vf02  \n"
            "  nop                        |  mulw.w vf27, vf01, vf02  \n"
            "  nop                        |  mulax.xyzw ACC, vf05, vf24\n"
            "  nop                        |  madday.xyzw ACC, vf06, vf24\n"
            "  nop                        |  maddz.xyz vf16, vf07, vf24\n"
            "  nop                        |  subw.z vf24, vf24, vf00  \n"
            "  nop                        |  mulax.xyzw ACC, vf05, vf25\n"
            "  nop                        |  madday.xyzw ACC, vf06, vf25\n"
            "  nop                        |  maddz.xyz vf17, vf07, vf25\n"
            "  div Q, vf00.w, vf24.z      |  subw.z vf25, vf25, vf00  \n"
            "  nop                        |  mulax.xyzw ACC, vf05, vf26\n"
            "  nop                        |  madday.xyzw ACC, vf06, vf26\n"
            "  nop                        |  maddz.xyz vf18, vf07, vf26\n"
            "  nop                        |  subw.z vf26, vf26, vf00  \n"
            "  nop                        |  mulax.xyzw ACC, vf05, vf27\n"
            "  nop                        |  madday.xyzw ACC, vf06, vf27\n"
            "  nop                        |  mul.w vf20, vf00, Q      \n"
            "  div Q, vf00.w, vf25.z      |  maddz.xyz vf19, vf07, vf27\n"
            "  nop                        |  subw.z vf27, vf27, vf00  \n"
            "  nop                        |  maxx.xyz vf16, vf16, vf00\n"
            "  nop                        |  maxx.xyz vf17, vf17, vf00\n"
            "  nop                        |  maxx.xyz vf18, vf18, vf00\n"
            "  nop                        |  maxx.xyz vf19, vf19, vf00\n"
            "  nop                        |  mul.w vf21, vf00, Q      \n"
            "  div Q, vf00.w, vf26.z      |  mula.xyzw ACC, vf01, vf11\n"
            "  nop                        |  maddax.xyz ACC, vf08, vf16\n"
            "  nop                        |  madday.xyz ACC, vf09, vf16\n"
            "  nop                        |  maddz.xyz vf20, vf10, vf16\n"
            "  nop                        |  mula.xyzw ACC, vf01, vf11\n"
            "  nop                        |  maddax.xyz ACC, vf08, vf17\n"
            "  nop                        |  madday.xyz ACC, vf09, vf17\n"
            "  nop                        |  mul.w vf22, vf00, Q      \n"
            "  nop                        |  maddz.xyz vf21, vf10, vf17\n"
            "  div Q, vf00.w, vf27.z      |  mula.xyzw ACC, vf01, vf11\n"
            "  nop                        |  maddax.xyz ACC, vf08, vf18\n"
            "  nop                        |  madday.xyz ACC, vf09, vf18\n"
            "  nop                        |  maddz.xyz vf22, vf10, vf18\n"
            "  nop                        |  mula.xyzw ACC, vf01, vf11\n"
            "  nop                        |  maddax.xyz ACC, vf08, vf19\n"
            "  nop                        |  madday.xyz ACC, vf09, vf19\n"
            "  nop                        |  maddz.xyz vf23, vf10, vf19\n"
            "  nop                        |  mul.w vf23, vf00, Q      \n"
            "  nop                        |  miniy.xyz vf20, vf20, vf12\n"
            "  nop                        |  miniy.xyz vf21, vf21, vf12\n"
            "  nop                        |  miniy.xyz vf22, vf22, vf12 :e\n"
            "  nop                        |  miniy.xyz vf23, vf23, vf12\n");
}

TEST(VuDisasm, GenericVu0) {
  auto parsed = parse_data(get_test_case("generic-vu0"));

  std::vector<u32> data;
  for (auto& w : parsed.words) {
    ASSERT_EQ(w.kind, LinkedWord::Kind::PLAIN_DATA);
    data.push_back(w.data);
  }

  VuDisassembler disasm;
  auto prog = disasm.disassemble(data.data(), data.size() * 4, false);

  EXPECT_EQ(disasm.to_string(prog), get_test_case("generic-vu0-result"));
}