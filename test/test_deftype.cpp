#include "gtest/gtest.h"
#include "common/type_system/TypeSystem.h"
#include "common/type_system/deftype.h"
#include "common/goos/Reader.h"
#include "third-party/fmt/core.h"

TEST(Deftype, deftype) {
  TypeSystem ts;
  ts.add_builtin_types();
  std::string input =
      "(deftype my-type (basic) ((f1 int64) (f2 string) (f3 int8) (f4 type :inline)))";
  goos::Reader reader;
  auto in = reader.read_from_string(input).as_pair()->cdr.as_pair()->car.as_pair()->cdr;
  auto result = parse_deftype(in, &ts);

  auto& f = dynamic_cast<StructureType*>(ts.lookup_type(result.type))->fields();
  EXPECT_EQ(f.size(), 5);

  auto& tf = f.at(0);
  EXPECT_EQ(tf.name(), "type");
  EXPECT_EQ(tf.offset(), 0);
  EXPECT_EQ(tf.type().print(), "type");
  EXPECT_EQ(tf.is_inline(), false);

  auto& f1 = f.at(1);
  EXPECT_EQ(f1.name(), "f1");
  EXPECT_EQ(f1.offset(), 8);
  EXPECT_EQ(f1.type().print(), "int64");
  EXPECT_EQ(f1.is_inline(), false);

  auto& f2 = f.at(2);
  EXPECT_EQ(f2.name(), "f2");
  EXPECT_EQ(f2.offset(), 16);
  EXPECT_EQ(f2.type().print(), "string");
  EXPECT_EQ(f2.is_inline(), false);

  auto& f3 = f.at(3);
  EXPECT_EQ(f3.name(), "f3");
  EXPECT_EQ(f3.offset(), 20);
  EXPECT_EQ(f3.type().print(), "int8");
  EXPECT_EQ(f3.is_inline(), false);

  auto& f4 = f.at(4);
  EXPECT_EQ(f4.name(), "f4");
  EXPECT_EQ(f4.offset(), 32);
  EXPECT_EQ(f4.type().print(), "type");
  EXPECT_EQ(f4.is_inline(), true);
}