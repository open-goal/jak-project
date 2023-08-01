#include "common/goos/ParseHelpers.h"
#include "common/goos/Reader.h"
#include "common/type_system/TypeSystem.h"
#include "common/type_system/deftype.h"

#include "gtest/gtest.h"

TEST(TypeSystem, Construction) {
  // test that we can add all builtin types without any type errors
  TypeSystem ts;
  ts.add_builtin_types(GameVersion::Jak1);

  // useful for debugging.
  //  fmt::print("{}", ts.print_all_type_information());
}

TEST(TypeSystem, DefaultMethods) {
  TypeSystem ts;
  ts.add_builtin_types(GameVersion::Jak1);

  // check that default methods have the right ID's used by the kernel
  ts.assert_method_id("object", "new", GOAL_NEW_METHOD);
  ts.assert_method_id("object", "delete", GOAL_DEL_METHOD);
  ts.assert_method_id("object", "print", GOAL_PRINT_METHOD);
  ts.assert_method_id("object", "inspect", GOAL_INSPECT_METHOD);
  ts.assert_method_id("object", "length", GOAL_LENGTH_METHOD);
  ts.assert_method_id("object", "asize-of", GOAL_ASIZE_METHOD);
  ts.assert_method_id("object", "copy", GOAL_COPY_METHOD);
  ts.assert_method_id("object", "relocate", GOAL_RELOC_METHOD);
  ts.assert_method_id("object", "mem-usage", GOAL_MEMUSAGE_METHOD);

  // check that they are inherited.
  ts.assert_method_id("function", "new", GOAL_NEW_METHOD);
  ts.assert_method_id("function", "delete", GOAL_DEL_METHOD);
  ts.assert_method_id("function", "print", GOAL_PRINT_METHOD);
  ts.assert_method_id("function", "inspect", GOAL_INSPECT_METHOD);
  ts.assert_method_id("function", "length", GOAL_LENGTH_METHOD);
  ts.assert_method_id("function", "asize-of", GOAL_ASIZE_METHOD);
  ts.assert_method_id("function", "copy", GOAL_COPY_METHOD);
  ts.assert_method_id("function", "relocate", GOAL_RELOC_METHOD);
  ts.assert_method_id("function", "mem-usage", GOAL_MEMUSAGE_METHOD);
}

TEST(TypeSystemReverse, NestedInlineWeird) {
  // tests the case where we're accessing nested inline arrays, with a dynamic inner access
  // and constant outer access, which will be constant propagated by the GOAL compiler.
  TypeSystem ts;
  ts.add_builtin_types(GameVersion::Jak1);
  goos::Reader reader;
  auto add_type = [&](const std::string& str) {
    auto& in = reader.read_from_string(str).as_pair()->cdr.as_pair()->car.as_pair()->cdr;
    parse_deftype(in, &ts);
  };

  add_type(
      "(deftype rgba (uint32)\n"
      "  ((r uint8 :offset 0)\n"
      "   (g uint8 :offset 8)\n"
      "   (b uint8 :offset 16)\n"
      "   (a uint8 :offset 24)\n"
      "   )\n"
      "  :flag-assert #x900000004\n"
      "  )");

  add_type(
      "(deftype char-color (structure)\n"
      "  ((color rgba 4 :offset-assert 0)\n"
      "   )\n"
      "  :method-count-assert 9\n"
      "  :size-assert         #x10\n"
      "  :flag-assert         #x900000010\n"
      "  )");

  add_type(
      "(deftype font-work (structure)\n"
      "  (\n"
      "   (color-table        char-color       64  :inline    :offset 1984)\n"
      "   (last-color         uint64                 :offset-assert 3008)\n"
      "   (save-last-color    uint64                 :offset-assert 3016)\n"
      "   (buf                basic                  :offset-assert 3024)\n"
      "   (str-ptr            uint32                 :offset-assert 3028)\n"
      "   (flags              uint32                 :offset-assert 3032)\n"
      "   (reg-save           uint32        5       :offset-assert 3036)\n"
      "   )\n"
      "  :method-count-assert 9\n"
      "  :size-assert         #xbf0\n"
      "  :flag-assert         #x900000bf0\n"
      "  )");

  FieldReverseLookupInput input;
  input.stride = 4;
  input.base_type = ts.make_typespec("font-work");
  input.offset = 2496;
  DerefKind dk;
  dk.size = 4;
  dk.sign_extend = false;
  dk.is_store = false;
  dk.reg_kind = RegClass::GPR_64;
  input.deref = dk;
  auto result = ts.reverse_field_lookup(input);
  EXPECT_TRUE(result.success);

  EXPECT_EQ(result.tokens.at(0).print(), "color-table");
  EXPECT_EQ(result.tokens.at(1).print(), "32");  // 32 * 16 + 1984 = 2496
  EXPECT_EQ(result.tokens.at(2).print(), "color");
  EXPECT_EQ(result.tokens.at(3).kind, FieldReverseLookupOutput::Token::Kind::VAR_IDX);
}

TEST(TypeSystem, TypeSpec) {
  TypeSystem ts;
  ts.add_builtin_types(GameVersion::Jak1);

  // try some simple typespecs
  auto string_typespec = ts.make_typespec("string");
  auto function_typespec = ts.make_typespec("function");
  EXPECT_EQ(string_typespec.print(), "string");
  EXPECT_EQ(string_typespec.base_type(), "string");
  EXPECT_TRUE(function_typespec == function_typespec);
  EXPECT_FALSE(function_typespec == string_typespec);

  // try some pointer typespecs
  auto pointer_function_typespec = ts.make_pointer_typespec("function");
  EXPECT_EQ(pointer_function_typespec.print(), "(pointer function)");
  EXPECT_EQ(pointer_function_typespec.get_single_arg(), ts.make_typespec("function"));
  EXPECT_EQ(pointer_function_typespec.base_type(), "pointer");

  // try some function typespecs
  auto test_function_typespec = ts.make_function_typespec({"string", "symbol"}, "integer");
  EXPECT_EQ(test_function_typespec.base_type(), "function");
  EXPECT_EQ(test_function_typespec.print(), "(function string symbol integer)");

  // try the none typespec
  EXPECT_EQ(ts.make_typespec("none").base_type(), "none");
}

TEST(TypeSystem, TypeSpecEquality) {
  TypeSystem ts;
  ts.add_builtin_types(GameVersion::Jak1);

  auto pointer_to_function = ts.make_pointer_typespec("function");
  auto ia_to_function = ts.make_inline_array_typespec("function");
  auto pointer_to_string = ts.make_pointer_typespec("string");

  EXPECT_TRUE(pointer_to_function == pointer_to_function);
  EXPECT_FALSE(pointer_to_function == ia_to_function);
  EXPECT_FALSE(pointer_to_string == pointer_to_function);
}

TEST(TypeSystem, RuntimeTypes) {
  TypeSystem ts;
  ts.add_builtin_types(GameVersion::Jak1);

  // pointers and inline arrays should all become simple pointers
  EXPECT_EQ(ts.get_runtime_type(ts.make_typespec("pointer")), "pointer");
  EXPECT_EQ(ts.get_runtime_type(ts.make_typespec("inline-array")), "pointer");
  EXPECT_EQ(ts.get_runtime_type(ts.make_pointer_typespec("function")), "pointer");
  EXPECT_EQ(ts.get_runtime_type(ts.make_inline_array_typespec("function")), "pointer");

  // functions of any kind should become function
  EXPECT_EQ(ts.get_runtime_type(ts.make_function_typespec({"integer", "string"}, "symbol")),
            "function");
}

TEST(TypeSystem, ForwardDeclaration) {
  TypeSystem ts;
  ts.add_builtin_types(GameVersion::Jak1);

  // before forward declaring, lookup and creating a typespec should fail
  EXPECT_ANY_THROW(ts.lookup_type("test-type"));
  EXPECT_ANY_THROW(ts.make_typespec("test-type"));

  // after forward declaring, we should be able to create typespec, but not do a full lookup
  ts.forward_declare_type_as_type("test-type");

  EXPECT_TRUE(ts.make_typespec("test-type").print() == "test-type");
  EXPECT_ANY_THROW(ts.lookup_type("test-type"));
}

TEST(TypeSystem, DerefInfoNoLoadInfoOrStride) {
  // test the parts of deref info, other than the part where it tells you how to load or the stride.
  TypeSystem ts;
  ts.add_builtin_types(GameVersion::Jak1);

  // can't dereference a non-pointer
  EXPECT_FALSE(ts.get_deref_info(ts.make_typespec("string")).can_deref);
  // can't deref a pointer with no type
  EXPECT_FALSE(ts.get_deref_info(ts.make_typespec("pointer")).can_deref);
  EXPECT_FALSE(ts.get_deref_info(ts.make_typespec("inline-array")).can_deref);

  // test pointer to reference type
  auto type_spec =
      ts.make_pointer_typespec(ts.make_function_typespec({"string", "symbol"}, "int32"));
  auto info = ts.get_deref_info(type_spec);
  EXPECT_TRUE(info.can_deref);
  EXPECT_TRUE(info.mem_deref);
  EXPECT_FALSE(info.sign_extend);  // it's a memory address being loaded
  EXPECT_EQ(info.reg, RegClass::GPR_64);
  EXPECT_EQ(info.stride, 4);
  EXPECT_EQ(info.result_type.print(), "(function string symbol int32)");
  EXPECT_EQ(info.load_size, 4);

  // test pointer to value type
  type_spec = ts.make_pointer_typespec("int64");
  info = ts.get_deref_info(type_spec);
  EXPECT_TRUE(info.can_deref);
  EXPECT_TRUE(info.mem_deref);
  EXPECT_EQ(info.load_size, 8);
  EXPECT_EQ(info.stride, 8);
  EXPECT_EQ(info.sign_extend, true);
  EXPECT_EQ(info.reg, RegClass::GPR_64);
  EXPECT_EQ(info.result_type.print(), "int64");

  // test inline-array (won't work because type is dynamically sized)
  type_spec = ts.make_inline_array_typespec("type");
  info = ts.get_deref_info(type_spec);
  EXPECT_FALSE(info.can_deref);

  // TODO - replace with a better type.
  // TODO - maybe block basic or structure from being inline-array-able?
  type_spec = ts.make_inline_array_typespec("basic");
  auto type = ts.lookup_type("basic");
  info = ts.get_deref_info(type_spec);
  EXPECT_TRUE(info.can_deref);
  EXPECT_FALSE(info.mem_deref);
  EXPECT_EQ(info.stride, (type->get_size_in_memory() + 15) & (~15));
  EXPECT_EQ(info.result_type.print(), "basic");
  EXPECT_EQ(info.load_size, -1);
}

TEST(TypeSystem, AddMethodAndLookupMethod) {
  TypeSystem ts;
  ts.add_builtin_types(GameVersion::Jak1);

  auto parent_info =
      ts.declare_method(ts.lookup_type("structure"), "test-method-1", "test docstring", false,
                        ts.make_function_typespec({"integer"}, "string"), false);

  // when trying to add the same method to a child, should return the parent's method
  auto child_info_same =
      ts.declare_method(ts.lookup_type("basic"), "test-method-1", "test docstring", false,
                        ts.make_function_typespec({"integer"}, "string"), false);

  EXPECT_EQ(parent_info.id, child_info_same.id);
  EXPECT_EQ(parent_info.id, GOAL_MEMUSAGE_METHOD + 1);

  // any amount of fiddling with method types should cause an error
  EXPECT_ANY_THROW(ts.declare_method(ts.lookup_type("basic"), "test-method-1", "test docstring",
                                     false, ts.make_function_typespec({"integer"}, "integer"),
                                     false));
  EXPECT_ANY_THROW(ts.declare_method(ts.lookup_type("basic"), "test-method-1", "test docstring",
                                     false, ts.make_function_typespec({}, "string"), false));
  EXPECT_ANY_THROW(
      ts.declare_method(ts.lookup_type("basic"), "test-method-1", "test docstring", false,
                        ts.make_function_typespec({"integer", "string"}, "string"), false));
  EXPECT_ANY_THROW(ts.declare_method(ts.lookup_type("basic"), "test-method-1", "test docstring",
                                     false, ts.make_function_typespec({"string"}, "string"),
                                     false));

  ts.declare_method(ts.lookup_type("basic"), "test-method-2", "test docstring", false,
                    ts.make_function_typespec({"integer"}, "string"), false);

  EXPECT_EQ(parent_info.id, ts.lookup_method("basic", "test-method-1").id);
  EXPECT_EQ(parent_info.id, ts.lookup_method("structure", "test-method-1").id);
  EXPECT_EQ(parent_info.id + 1, ts.lookup_method("basic", "test-method-2").id);
  EXPECT_ANY_THROW(ts.lookup_method("structure", "test-method-2"));

  EXPECT_EQ(ts.lookup_method("basic", "test-method-1").defined_in_type, "structure");
  EXPECT_EQ(ts.lookup_method("basic", "test-method-1").type.print(), "(function integer string)");
  EXPECT_EQ(ts.lookup_method("basic", "test-method-1").name, "test-method-1");

  auto id = ts.lookup_method("basic", "test-method-1").id;
  MethodInfo info;
  EXPECT_TRUE(ts.try_lookup_method("basic", id, &info));
  EXPECT_FALSE(ts.try_lookup_method("not-a-real-type-name", id, &info));
  EXPECT_FALSE(ts.try_lookup_method("basic", id * 2, &info));
}

TEST(TypeSystem, NewMethod) {
  TypeSystem ts;
  ts.add_builtin_types(GameVersion::Jak1);
  ts.add_type("test-1", std::make_unique<BasicType>("basic", "test-1", false, 0));
  ts.declare_method(ts.lookup_type("test-1"), "new", "test docstring", false,
                    ts.make_function_typespec({"symbol", "string"}, "test-1"), false);
  ts.add_type("test-2", std::make_unique<BasicType>("test-1", "test-2", false, 0));
  ts.declare_method(ts.lookup_type("test-2"), "new", "test docstring", false,
                    ts.make_function_typespec({"symbol", "string", "symbol"}, "test-2"), false);

  EXPECT_EQ(ts.lookup_method("test-1", "new").type.print(), "(function symbol string test-1)");
  EXPECT_EQ(ts.lookup_method("test-2", "new").type.print(),
            "(function symbol string symbol test-2)");

  ts.add_type("test-3", std::make_unique<BasicType>("test-1", "test-3", false, 0));
  EXPECT_EQ(ts.lookup_method("test-3", "new").type.print(), "(function symbol string test-1)");

  ts.add_type("test-4", std::make_unique<BasicType>("test-2", "test-4", false, 0));
  EXPECT_EQ(ts.lookup_method("test-4", "new").type.print(),
            "(function symbol string symbol test-2)");
}

TEST(TypeSystem, MethodSubstitute) {
  TypeSystem ts;
  ts.add_builtin_types(GameVersion::Jak1);
  ts.add_type("test-1", std::make_unique<BasicType>("basic", "test-1", false, 0));
  ts.declare_method(ts.lookup_type("test-1"), "new", "test docstring", false,
                    ts.make_function_typespec({"symbol", "string", "_type_"}, "_type_"), false);

  auto final_type = ts.lookup_method("test-1", "new").type.substitute_for_method_call("test-1");
  EXPECT_EQ(final_type.print(), "(function symbol string test-1 test-1)");
}

namespace {
bool ts_name_name(TypeSystem& ts, const std::string& ex, const std::string& act) {
  return ts.typecheck_and_throw(ts.make_typespec(ex), ts.make_typespec(act), "", false, false);
}
}  // namespace

TEST(TypeSystem, TypeCheck) {
  TypeSystem ts;
  ts.add_builtin_types(GameVersion::Jak1);

  EXPECT_TRUE(ts_name_name(ts, "none",
                           "none"));  // none - none _shouldn't_ fail (for function return types!)
  EXPECT_TRUE(ts_name_name(ts, "object", "object"));
  EXPECT_TRUE(ts_name_name(ts, "object", "type"));
  EXPECT_TRUE(ts_name_name(ts, "basic", "type"));
  EXPECT_FALSE(ts_name_name(ts, "type", "basic"));
  EXPECT_TRUE(ts_name_name(ts, "type", "type"));

  auto f = ts.make_typespec("function");
  auto f_s_n = ts.make_function_typespec({"string"}, "none");
  auto f_b_n = ts.make_function_typespec({"basic"}, "none");

  // complex
  EXPECT_TRUE(ts.typecheck_and_throw(f, f_s_n));
  EXPECT_TRUE(ts.typecheck_and_throw(f_s_n, f_s_n));
  EXPECT_TRUE(ts.typecheck_and_throw(f_b_n, f_s_n));

  EXPECT_FALSE(ts.typecheck_and_throw(f_s_n, f, "", false, false));
  EXPECT_FALSE(ts.typecheck_and_throw(f_s_n, f_b_n, "", false, false));

  // number of parameter mismatch
  auto f_s_s_n = ts.make_function_typespec({"string", "string"}, "none");
  EXPECT_FALSE(ts.typecheck_and_throw(f_s_n, f_s_s_n, "", false, false));
  EXPECT_FALSE(ts.typecheck_and_throw(f_s_s_n, f_s_n, "", false, false));
}

TEST(TypeSystem, FieldLookup) {
  // note - this test isn't testing the specific needs_deref, type of the returned info.  Until more
  // stuff is set up that test is kinda useless - it would just be testing against the exact
  // implementation of lookup_field_info

  TypeSystem ts;
  ts.add_builtin_types(GameVersion::Jak1);

  EXPECT_EQ(ts.lookup_field_info("type", "parent").field.offset(), 8);
  EXPECT_EQ(ts.lookup_field_info("string", "data").type.print(), "(pointer uint8)");

  EXPECT_ANY_THROW(ts.lookup_field_info("type", "not-a-real-field"));
}

TEST(TypeSystem, get_path_up_tree) {
  TypeSystem ts;
  ts.add_builtin_types(GameVersion::Jak1);
  EXPECT_EQ(ts.get_path_up_tree("type"),
            std::vector<std::string>({"type", "basic", "structure", "object"}));
}

TEST(TypeSystem, lca) {
  TypeSystem ts;
  ts.add_builtin_types(GameVersion::Jak1);
  EXPECT_EQ(
      ts.lowest_common_ancestor(ts.make_typespec("string"), ts.make_typespec("basic")).print(),
      "basic");
  EXPECT_EQ(
      ts.lowest_common_ancestor(ts.make_typespec("basic"), ts.make_typespec("string")).print(),
      "basic");
  EXPECT_EQ(
      ts.lowest_common_ancestor(ts.make_typespec("string"), ts.make_typespec("int32")).print(),
      "object");
  EXPECT_EQ(
      ts.lowest_common_ancestor(ts.make_typespec("int32"), ts.make_typespec("string")).print(),
      "object");
  EXPECT_EQ(
      ts.lowest_common_ancestor({ts.make_typespec("int32"), ts.make_typespec("string")}).print(),
      "object");
  EXPECT_EQ(ts.lowest_common_ancestor({ts.make_typespec("int32")}).print(), "int32");
  EXPECT_EQ(ts.lowest_common_ancestor({ts.make_typespec("string"), ts.make_typespec("kheap"),
                                       ts.make_typespec("pointer")})
                .print(),
            "object");

  EXPECT_EQ(ts.lowest_common_ancestor(ts.make_pointer_typespec("int32"),
                                      ts.make_pointer_typespec("string"))
                .print(),
            "(pointer object)");
}

TEST(TypeSystem, DecompLookupsTypeOfBasic) {
  TypeSystem ts;
  ts.add_builtin_types(GameVersion::Jak1);

  auto string_type = ts.make_typespec("string");
  FieldReverseLookupInput input;
  input.stride = 0;
  input.base_type = string_type;
  input.offset = -4;
  DerefKind dk;
  dk.size = 4;
  dk.sign_extend = false;
  dk.is_store = false;
  dk.reg_kind = RegClass::GPR_64;
  input.deref = dk;
  auto result = ts.reverse_field_lookup(input);

  EXPECT_TRUE(result.success);
  EXPECT_FALSE(result.addr_of);
  EXPECT_TRUE(result.result_type == ts.make_typespec("type"));
  EXPECT_EQ(result.tokens.size(), 1);
  EXPECT_EQ(result.tokens.at(0).name, "type");
}

TEST(TypeSystem, DecompLookupsMethod) {
  TypeSystem ts;
  ts.add_builtin_types(GameVersion::Jak1);

  auto type_type = ts.make_typespec("type");

  FieldReverseLookupInput input;
  input.stride = 0;
  input.base_type = type_type;
  input.offset = 16;
  DerefKind dk;
  dk.size = 4;
  dk.sign_extend = false;
  dk.is_store = false;
  dk.reg_kind = RegClass::GPR_64;
  input.deref = dk;
  auto result = ts.reverse_field_lookup(input);

  EXPECT_TRUE(result.success);
  EXPECT_FALSE(result.addr_of);
  EXPECT_TRUE(result.result_type == ts.make_typespec("function"));
  EXPECT_EQ(result.tokens.size(), 2);
  EXPECT_EQ(result.tokens.at(0).name, "method-table");
  EXPECT_EQ(result.tokens.at(1).idx, 0);

  input.stride = 0;
  input.base_type = type_type;
  input.offset = 24;
  dk.size = 4;
  dk.sign_extend = false;
  dk.is_store = false;
  dk.reg_kind = RegClass::GPR_64;
  input.deref = dk;
  result = ts.reverse_field_lookup(input);

  EXPECT_TRUE(result.success);
  EXPECT_FALSE(result.addr_of);
  EXPECT_TRUE(result.result_type == ts.make_typespec("function"));
  EXPECT_EQ(result.tokens.size(), 2);
  EXPECT_EQ(result.tokens.at(0).name, "method-table");
  EXPECT_EQ(result.tokens.at(1).idx, 2);

  input.stride = 0;
  input.base_type = type_type;
  input.offset = 24;
  input.deref = std::nullopt;
  result = ts.reverse_field_lookup(input);

  EXPECT_TRUE(result.success);
  EXPECT_TRUE(result.addr_of);
  EXPECT_TRUE(result.result_type == ts.make_pointer_typespec("function"));
  EXPECT_EQ(result.tokens.size(), 2);
  EXPECT_EQ(result.tokens.at(0).name, "method-table");
  EXPECT_EQ(result.tokens.at(1).idx, 2);
}

TEST(Deftype, deftype) {
  TypeSystem ts;
  ts.add_builtin_types(GameVersion::Jak1);
  std::string input =
      "(deftype my-type (basic) ((f1 int64) (f2 string) (f3 int8) (f4 type :inline) (f5 uint64 "
      ":overlay-at f1)))";
  goos::Reader reader;
  auto& in = reader.read_from_string(input).as_pair()->cdr.as_pair()->car.as_pair()->cdr;
  auto result = parse_deftype(in, &ts);

  auto& f = dynamic_cast<StructureType*>(ts.lookup_type(result.type))->fields();
  EXPECT_EQ(f.size(), 6);

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

  auto& f5 = f.at(5);
  EXPECT_EQ(f5.name(), "f5");
  EXPECT_EQ(f5.offset(), f1.offset());
  EXPECT_EQ(f5.type().print(), "uint64");
  EXPECT_EQ(f5.is_inline(), false);
}

// TODO - a big test to make sure all the builtin types are what we expect.
