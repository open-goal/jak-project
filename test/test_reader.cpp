/*!
 * @file test_reader.cpp
 * Test the reader.
 * For some reason this runs at ~5 fps in CLion IDE.
 */

#include "common/goos/Reader.h"
#include "common/util/FileUtil.h"

#include "gtest/gtest.h"

using namespace goos;

TEST(GoosReader, Construction) {
  // test that reader is able to find the source directory
  Reader reader;
}

namespace {
bool check_first_integer(Object o, int64_t x) {
  return o.as_pair()->cdr.as_pair()->car.as_int() == x;
}

bool check_first_float(Object o, double x) {
  return o.as_pair()->cdr.as_pair()->car.as_float() == x;
}

bool check_first_symbol(Object o, const std::string& sym) {
  return o.as_pair()->cdr.as_pair()->car.as_symbol() == sym;
}

bool check_first_string(Object o, const std::string& str) {
  return o.as_pair()->cdr.as_pair()->car.as_string()->data == str;
}
}  // namespace

TEST(GoosReader, Integer) {
  Reader reader;
  EXPECT_TRUE(check_first_integer(reader.read_from_string("123"), 123));
  EXPECT_TRUE(check_first_integer(reader.read_from_string("1"), 1));
  EXPECT_TRUE(check_first_integer(reader.read_from_string("-1"), -1));
  EXPECT_TRUE(check_first_integer(reader.read_from_string("0"), 0));
  EXPECT_TRUE(check_first_integer(reader.read_from_string("9223372036854775807"), INT64_MAX));
  EXPECT_TRUE(check_first_integer(reader.read_from_string("-9223372036854775808"), INT64_MIN));

  EXPECT_TRUE(check_first_integer(reader.read_from_string("-0"), 0));
  EXPECT_TRUE(check_first_integer(reader.read_from_string("-000000"), 0));

  EXPECT_TRUE(check_first_integer(
      reader.read_from_string(
          "-0000000000000000000000000000000000000000000000000000000000000000000000000000000001"),
      -1));

  EXPECT_TRUE(reader.read_from_string("--0").as_pair()->cdr.as_pair()->car.is_symbol());
  // too big or too small.
  EXPECT_ANY_THROW(reader.read_from_string("9223372036854775808"));
  EXPECT_ANY_THROW(reader.read_from_string("-9223372036854775809"));
}

TEST(GoosReader, Hex) {
  Reader reader;
  EXPECT_TRUE(check_first_integer(reader.read_from_string("#x0"), 0));
  EXPECT_TRUE(check_first_integer(reader.read_from_string("#x1"), 1));
  EXPECT_TRUE(check_first_integer(reader.read_from_string("#xf"), 15));
  EXPECT_TRUE(check_first_integer(reader.read_from_string("#xF"), 15));
  EXPECT_TRUE(check_first_integer(reader.read_from_string("#x0F"), 15));
  EXPECT_TRUE(check_first_integer(
      reader.read_from_string("#x0000000000000000000000000000000000000000000000000000f"), 15));

  EXPECT_TRUE(check_first_integer(reader.read_from_string("#xffffffff"), UINT32_MAX));
  EXPECT_TRUE(check_first_integer(reader.read_from_string("#x100000000"), (1LL << 32LL)));
  EXPECT_TRUE(check_first_integer(reader.read_from_string("#x7FFFFFFFFFFFFFFF"), INT64_MAX));
  EXPECT_TRUE(check_first_integer(reader.read_from_string("#x8000000000000000"), INT64_MIN));
  EXPECT_TRUE(check_first_integer(reader.read_from_string("#xffffffffffffffff"), -1));

  EXPECT_ANY_THROW(reader.read_from_string("#x10000000000000000"));

  EXPECT_TRUE(check_first_symbol(reader.read_from_string("#x"), "#x"));
  EXPECT_TRUE(check_first_symbol(reader.read_from_string("#x-1"), "#x-1"));
  EXPECT_TRUE(check_first_symbol(reader.read_from_string("#x.1"), "#x.1"));
}

TEST(GoosReader, Binary) {
  Reader reader;

  EXPECT_TRUE(check_first_integer(reader.read_from_string("#b0"), 0));
  EXPECT_TRUE(check_first_integer(reader.read_from_string("#b0000000000"), 0));
  EXPECT_TRUE(check_first_integer(
      reader.read_from_string("#b000000000000000000000000000000000000000000000000000000000000000000"
                              "00000000000000000000000000000000"),
      0));
  EXPECT_TRUE(check_first_integer(reader.read_from_string("#b1"), 1));
  EXPECT_TRUE(check_first_integer(reader.read_from_string("#b10"), 2));
  EXPECT_TRUE(check_first_integer(reader.read_from_string("#b01011"), 11));
  EXPECT_TRUE(check_first_integer(
      reader.read_from_string("#b1111111111111111111111111111111111111111111111111111111111111111"),
      -1));
  EXPECT_TRUE(check_first_integer(
      reader.read_from_string(
          "#b000001111111111111111111111111111111111111111111111111111111111111111"),
      -1));

  EXPECT_TRUE(check_first_integer(
      reader.read_from_string("#b0111111111111111111111111111111111111111111111111111111111111111"),
      INT64_MAX));
  EXPECT_TRUE(check_first_integer(
      reader.read_from_string("#b1000000000000000000000000000000000000000000000000000000000000000"),
      INT64_MIN));

  EXPECT_ANY_THROW(reader.read_from_string(
      "#b11111111111111111111111111111111111111111111111111111111111111111"));

  EXPECT_TRUE(check_first_symbol(reader.read_from_string("#b"), "#b"));
  EXPECT_TRUE(check_first_symbol(reader.read_from_string("#b-1"), "#b-1"));
  EXPECT_TRUE(check_first_symbol(reader.read_from_string("#b.1"), "#b.1"));
}

TEST(GoosReader, Float) {
  Reader reader;

  EXPECT_TRUE(check_first_float(reader.read_from_string("1.6"), 1.6));
  EXPECT_TRUE(check_first_float(reader.read_from_string("0000001.6"), 1.6));
  EXPECT_TRUE(check_first_float(reader.read_from_string("0.6"), 0.6));
  EXPECT_TRUE(check_first_float(reader.read_from_string("00000.6"), 0.6));
  EXPECT_TRUE(check_first_float(reader.read_from_string("-0.6"), -0.6));
  EXPECT_TRUE(check_first_float(reader.read_from_string("-000000.6"), -0.6));
  EXPECT_TRUE(check_first_float(reader.read_from_string("-.6"), -.6));

  EXPECT_TRUE(check_first_float(reader.read_from_string("1."), 1));
  EXPECT_TRUE(check_first_float(reader.read_from_string("1.0"), 1));
  EXPECT_TRUE(check_first_float(reader.read_from_string("01."), 1));
  EXPECT_TRUE(check_first_float(reader.read_from_string("01.0"), 1));

  EXPECT_TRUE(check_first_float(reader.read_from_string("0."), 0));
  EXPECT_TRUE(check_first_float(reader.read_from_string(".0"), 0));
  EXPECT_TRUE(check_first_float(reader.read_from_string("0.0"), 0));
  EXPECT_TRUE(check_first_float(reader.read_from_string("000."), 0));
  EXPECT_TRUE(check_first_float(reader.read_from_string(".000"), 0));
  EXPECT_TRUE(check_first_float(reader.read_from_string("0.000"), 0));
  EXPECT_TRUE(check_first_float(reader.read_from_string("000.0"), 0));
  EXPECT_TRUE(check_first_float(reader.read_from_string("000.0000"), 0));

  EXPECT_TRUE(check_first_float(reader.read_from_string("-0."), 0));
  EXPECT_TRUE(check_first_float(reader.read_from_string("-.0"), 0));
  EXPECT_TRUE(check_first_float(reader.read_from_string("-0.0"), 0));
  EXPECT_TRUE(check_first_float(reader.read_from_string("-000."), 0));
  EXPECT_TRUE(check_first_float(reader.read_from_string("-.000"), 0));
  EXPECT_TRUE(check_first_float(reader.read_from_string("-0.000"), 0));
  EXPECT_TRUE(check_first_float(reader.read_from_string("-000.0"), 0));
  EXPECT_TRUE(check_first_float(reader.read_from_string("-000.0000"), 0));

  EXPECT_TRUE(check_first_symbol(reader.read_from_string("1e0"), "1e0"));
  EXPECT_ANY_THROW(reader.read_from_string("."));
}

TEST(GoosReader, Boolean) {
  Reader reader;
  EXPECT_TRUE(check_first_symbol(reader.read_from_string("#f"), "#f"));
  EXPECT_TRUE(check_first_symbol(reader.read_from_string("#t"), "#t"));
}

TEST(GoosReader, String) {
  Reader reader;
  EXPECT_TRUE(
      check_first_string(reader.read_from_string("\"testing string ()\""), "testing string ()"));
  EXPECT_TRUE(check_first_string(reader.read_from_string("\"\""), ""));
  EXPECT_TRUE(check_first_string(reader.read_from_string("\"  \\t  \""), "  \t  "));
  EXPECT_TRUE(check_first_string(reader.read_from_string("\"  \\n  \""), "  \n  "));
  EXPECT_TRUE(check_first_string(reader.read_from_string("\"test  \\n\""), "test  \n"));
  EXPECT_TRUE(check_first_string(reader.read_from_string("\"  \\\\  \""), "  \\  "));
  EXPECT_ANY_THROW(reader.read_from_string("\"\\\""));   // "\" invalid escape
  EXPECT_ANY_THROW(reader.read_from_string("\"\\w\""));  // "\w" invalid escape
}

TEST(GoosReader, StringWithNumberEscapes) {
  Reader reader;

  // build a weird test string
  std::string str;
  for (int i = 1; i < 256; i++) {
    str.push_back(i);
  }

  // create a readable string:
  std::string readable = "\"";
  readable += goos::get_readable_string(str.data());
  readable.push_back('"');

  EXPECT_TRUE(check_first_string(reader.read_from_string(readable), str));
  EXPECT_ANY_THROW(reader.read_from_string("\"\\c\""));
  EXPECT_ANY_THROW(reader.read_from_string("\"\\c1\""));
  EXPECT_ANY_THROW(reader.read_from_string("\"\\cag\""));
  EXPECT_ANY_THROW(reader.read_from_string("\"\\c-1\""));
  EXPECT_ANY_THROW(reader.read_from_string("\"\\c-2\""));
}

TEST(GoosReader, Symbol) {
  std::vector<std::string> test_symbols = {
      "test", "test-two", "__werid-sym__", "-a", "-", "/", "*", "+", "a", "#f"};

  Reader reader;

  for (const auto& sym : test_symbols) {
    EXPECT_TRUE(check_first_symbol(reader.read_from_string(sym), sym));
  }
}

namespace {
bool first_list_matches(Object o, std::vector<Object> stuff) {
  auto& lst = o.as_pair()->cdr.as_pair()->car;
  for (const auto& x : stuff) {
    const auto& check = x.as_pair()->cdr.as_pair()->car;
    if (lst.as_pair()->car != check) {
      return false;
    }
    lst = lst.as_pair()->cdr;
  }

  return lst.is_empty_list();
}

bool first_array_matches(Object o, std::vector<Object> stuff) {
  auto array = o.as_pair()->cdr.as_pair()->car.as_array();
  if (stuff.size() != array->size()) {
    return false;
  }

  for (size_t i = 0; i < array->size(); i++) {
    if ((*array)[i] != stuff.at(i)) {
      return false;
    }
  }
  return true;
}

bool first_pair_matches(Object o, Object car, Object cdr) {
  auto& lst = o.as_pair()->cdr.as_pair()->car;
  return (lst.as_pair()->car == car) && (lst.as_pair()->cdr == cdr);
}

bool print_matches(Object o, const std::string& expected) {
  return o.as_pair()->cdr.as_pair()->car.print() == expected;
}

bool first_char_matches(Object o, char c) {
  return o.as_pair()->cdr.as_pair()->car.as_char() == c;
}

}  // namespace

TEST(GoosReader, List) {
  Reader reader;
  auto r = [&](std::string s) { return reader.read_from_string(s); };
  EXPECT_TRUE(first_list_matches(r("()"), {}));
  EXPECT_TRUE(first_list_matches(r("(1)"), {r("1")}));
  EXPECT_TRUE(first_list_matches(r("  (  1 )  "), {r("1")}));
  EXPECT_TRUE(first_list_matches(r("(1 2 3)"), {r("1"), r("2"), r("3")}));
  EXPECT_TRUE(first_list_matches(r("  (  1  bbbb  3  )  "), {r("1"), r("bbbb"), r("3")}));

  EXPECT_TRUE(first_pair_matches(r("(1 . 2)"), Object::make_integer(1), Object::make_integer(2)));

  EXPECT_TRUE(print_matches(r("  (  1  .  2  ) "), "(1 . 2)"));
  EXPECT_TRUE(print_matches(r("  (  1      1  .  2   ) "), "(1 1 . 2)"));
  EXPECT_TRUE(print_matches(r("  (  1  .   ( 1  .  2 )  ) "), "(1 1 . 2)"));
  EXPECT_TRUE(
      print_matches(r("  ( 1  ( 1  2 )  ( 1  ( 12  3 ) )  .  3 ) "), "(1 (1 2) (1 (12 3)) . 3)"));
  EXPECT_TRUE(
      print_matches(r("  ( 1  ( 1  2 )  ( 1  ( 12  3 ) )  .  (   ) ) "), "(1 (1 2) (1 (12 3)))"));

  std::vector<std::string> expected_to_throw = {"(",      ")",         " (",      "  )()() ",
                                                ")(",     "(1 2 ))",   "(( 1 2)", "(1 . . 2)",
                                                "(1 . )", "(1 . 2 3)", "( . 2)"};

  for (const auto& x : expected_to_throw) {
    EXPECT_ANY_THROW(r(x));
  }
}

TEST(GoosReader, Comments) {
  Reader reader;
  auto r = [&](std::string s) { return reader.read_from_string(s); };
  EXPECT_TRUE(first_list_matches(r(";;\n(1)\n;;"), {r("1")}));
  EXPECT_TRUE(first_list_matches(r(";;\n(;1\n1;)\n);;\n;"), {r("1")}));

  r(";");
  r(" ;");
  r("\n;");
  r(";\n");

  EXPECT_TRUE(first_list_matches(
      r("#|multi line\n com(((((ment |# (1) #| multi line\n comm)))))ent |#"), {r("1")}));
  EXPECT_TRUE(first_list_matches(
      r("#| #| multi l#|ine\n com#|ment |# (1) #| multi line\n commen))))))t |#"), {r("1")}));

  std::vector<std::string> expected_to_throw = {"|#", "#| |# |#"};

  for (const auto& x : expected_to_throw) {
    EXPECT_ANY_THROW(r(x));
  }
}

TEST(GoosReader, Char) {
  Reader reader;
  auto r = [&](std::string s) { return reader.read_from_string(s); };

  EXPECT_TRUE(first_char_matches(r("#\\c"), 'c'));
  EXPECT_TRUE(first_char_matches(r("#\\n"), 'n'));
  EXPECT_TRUE(first_char_matches(r("#\\\\n"), '\n'));
  EXPECT_TRUE(first_char_matches(r("#\\\\t"), '\t'));
  EXPECT_TRUE(first_char_matches(r("#\\\\s"), ' '));
}

TEST(GoosReader, Array) {
  Reader reader;
  auto r = [&](std::string s) { return reader.read_from_string(s); };
  EXPECT_TRUE(print_matches(r("  #(  ) "), "#()"));
  EXPECT_TRUE(first_array_matches(r("#()"), {}));
  EXPECT_TRUE(first_array_matches(r("#(1 2)"), {Object::make_integer(1), Object::make_integer(2)}));
  EXPECT_TRUE(first_array_matches(r("#( 1 #| 2 |# 3 )"),
                                  {Object::make_integer(1), Object::make_integer(3)}));
  EXPECT_TRUE(
      first_array_matches(r("#( 1 #|2|# 3 )"), {Object::make_integer(1), Object::make_integer(3)}));
}

TEST(GoosReader, Macros) {
  Reader reader;
  auto r = [&](std::string s) { return reader.read_from_string(s); };
  EXPECT_TRUE(print_matches(r("'x"), "(quote x)"));
  EXPECT_TRUE(print_matches(r("`x"), "(quasiquote x)"));
  EXPECT_TRUE(print_matches(r(",x"), "(unquote x)"));
  EXPECT_TRUE(print_matches(r(",@x"), "(unquote-splicing x)"));
}

TEST(GoosReader, TopLevel) {
  Reader reader;
  auto r = [&](std::string s) { return reader.read_from_string(s); };
  EXPECT_EQ(r("x").print(), "(top-level x)");
}

TEST(GoosReader, FromFile) {
  Reader reader;
  auto result = reader.read_from_file({"test", "test_data", "test_reader_file0.gc"}).print();
  EXPECT_TRUE(result == "(top-level (1 2 3 4))");
}

TEST(GoosReader, TextDb) {
  // very specific to this particular test file, but whatever.
  Reader reader;
  auto result = reader.read_from_file({"test", "test_data", "test_reader_file0.gc"})
                    .as_pair()
                    ->cdr.as_pair()
                    ->car;
  std::string expected = "test/test_data/test_reader_file0.gc:5\n(1 2 3 4)\n ^\n";
  EXPECT_EQ(expected, reader.db.get_info_for(result));
}
