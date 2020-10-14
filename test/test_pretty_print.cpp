#include "gtest/gtest.h"
#include "common/goos/Reader.h"
#include "common/util/FileUtil.h"
#include "common/goos/PrettyPrinter.h"

using namespace goos;

namespace {
Object read(const std::string& str) {
  auto body = pretty_print::get_pretty_printer_reader().read_from_string(str).as_pair()->cdr;
  EXPECT_TRUE(body.as_pair()->cdr.is_empty_list());
  return body.as_pair()->car;
}

std::string pprint(const Object& o, int len = 80) {
  return pretty_print::to_string(o, len);
}

// read then pretty print a string.
std::string ppr(const std::string& in, int len = 80) {
  return pprint(read(in), len);
}
}  // namespace

TEST(PrettyPrinter, Basics) {
  EXPECT_EQ(ppr("test"), "test");
  EXPECT_EQ(ppr("(l 12 asdf)"), "(l 12 asdf)");

  // force it to break
  EXPECT_EQ(ppr("(thing 12 asd asfd sdfjk)", 10), "(thing\n  12\n  asd\n  asfd\n  sdfjk\n  )");
}

TEST(PrettyPrinter, ReadAgain) {
  // first read the gcommon file
  auto gcommon_code = pretty_print::get_pretty_printer_reader().read_from_file(
      {"goal_src", "kernel", "gcommon.gc"});
  // pretty print it
  auto printed_gcommon = pretty_print::to_string(gcommon_code);
  auto gcommon_code2 = pretty_print::get_pretty_printer_reader()
                           .read_from_string(printed_gcommon)
                           .as_pair()
                           ->cdr.as_pair()
                           ->car;
  auto printed_gcommon2 = pretty_print::to_string(gcommon_code);
  EXPECT_TRUE(gcommon_code == gcommon_code2);
}

TEST(PrettyPrinter, ReadAgainVeryShortLines) {
  // first read the gcommon file
  auto gcommon_code = pretty_print::get_pretty_printer_reader().read_from_file(
      {"goal_src", "kernel", "gcommon.gc"});
  // pretty print it but with a very short line length. This looks terrible but will hopefully
  // hit many of the cases for line breaking.
  auto printed_gcommon = pretty_print::to_string(gcommon_code, 80);
  auto gcommon_code2 = pretty_print::get_pretty_printer_reader()
                           .read_from_string(printed_gcommon)
                           .as_pair()
                           ->cdr.as_pair()
                           ->car;
  auto printed_gcommon2 = pretty_print::to_string(gcommon_code);
  EXPECT_TRUE(gcommon_code == gcommon_code2);
}