#include "GoalType.h"
#include "TypeContainer.h"

static const std::string default_methods[] = {
    "new", "delete", "print", "inspect", "length", "asize-of", "copy", "relocate", "mem-usage"};

std::string GoalField::print() {
  auto result = type.type->get_name() + " " + name + " :offset " + std::to_string(offset);
  if (is_inline) {
    result += " :inline";
  }
  return result;
}

std::string GoalBitField::print() {
  auto result =
      type.print() + " " + name + " off " + std::to_string(offset) + " sz " + std::to_string(size);
  return result;
}

void TypeContainer::fill_with_default_types() {
  // NONE (not runtime)
  types["none"] = std::make_shared<SimpleType>(0, nullptr, "none", "");

  // OBJECT
  auto object_type = std::make_shared<SimpleType>(4, nullptr, "object");
  object_type->is_parent_type = true;
  types["object"] = object_type;

  // OBJECT 64 (not runtime)
  auto object64_type = std::make_shared<SimpleType>(8, types["object"], "object64", "object");
  object64_type->is_parent_type = true;
  types["object64"] = object64_type;

  // STRUCTURE
  auto structure_type = std::make_shared<StructureType>(4, types["object"], "structure");
  structure_type->is_parent_type = true;
  types["structure"] = structure_type;

  // BASIC
  // can't set the type field's type yet
  auto basic_type = std::make_shared<BasicType>(4, types["structure"], "basic", nullptr);
  basic_type->is_parent_type = true;
  types["basic"] = basic_type;

  // TYPE (out of order...)
  // can't set the type field's type yet
  auto type_type = std::make_shared<BasicType>(0x38, types["basic"], "type", nullptr);
  types["type"] = type_type;

  // now we can.
  basic_type->fields.front().type = TypeSpec(type_type);
  type_type->fields.front().type = TypeSpec(type_type);

  // SYMBOL (out of order...)
  auto symbol_type = std::make_shared<SymbolType>(types["basic"], type_type);
  symbol_type->fields.emplace_back(types["object"], "value", 4);
  types["symbol"] = symbol_type;

  // STRING
  auto string_type = std::make_shared<BasicType>(8, types["basic"], "string", type_type);
  string_type->dynamic = true;
  types["string"] = string_type;

  // BOOLEAN (not runtime)
  auto boolean_type = std::make_shared<BooleanType>(types["symbol"], type_type);
  types["boolean"] = boolean_type;

  // FUNCTION
  auto function_type = std::make_shared<FunctionType>(types["basic"], types["type"]);
  function_type->dynamic = true;
  types["function"] = function_type;

  // TODO - VU FUNCTION
  // TODO - LINK BLOCK

  // KHEAP
  auto kheap_type = std::make_shared<StructureType>(16, types["structure"], "kheap");
  types["kheap"] = kheap_type;

  // ARRAY
  auto array_type = std::make_shared<BasicType>(16, types["basic"], "array", type_type);
  array_type->dynamic = true;
  types["array"] = array_type;

  // PAIR
  auto pair_type = std::make_shared<SimpleType>(8, types["object"], "pair");
  pair_type->is_value_type = true;
  types["pair"] = pair_type;

  // PROCESS TREE
  auto process_tree_type =
      std::make_shared<BasicType>(32, types["basic"], "process-tree", type_type);
  types["process-tree"] = process_tree_type;

  // PROCESS
  auto process_type = std::make_shared<BasicType>(112, types["process-tree"], "process", type_type);
  types["process"] = process_type;

  // THREAD (this one is redefined in gkernel-h.gc)
  auto thread_type = std::make_shared<BasicType>(0x28, types["basic"], "thread", type_type);
  types["thread"] = thread_type;

  // CONNECTABLE
  auto connectable_type = std::make_shared<StructureType>(16, types["structure"], "connectable");
  types["connectable"] = connectable_type;

  // STACK FRAME
  auto stack_frame_type =
      std::make_shared<BasicType>(0xc, types["basic"], "stack-frame", type_type);
  types["stack-frame"] = stack_frame_type;

  // TODO - FILE-STREAM

  // POINTER
  auto pointer_type = std::make_shared<SimpleType>(4, types["object"], "pointer");
  pointer_type->is_value_type = true;
  pointer_type->load_signed = false;
  types["pointer"] = pointer_type;

  // NUMBER
  auto number_type = std::make_shared<SimpleType>(8, types["object"], "number");
  number_type->is_parent_type = true;
  number_type->is_value_type = true;
  types["number"] = number_type;

  // FLOAT
  auto float_type = std::make_shared<SimpleType>(4, types["number"], "float");
  float_type->is_value_type = true;
  float_type->minimum_alignment = 4;
  float_type->load_size = 4;
  float_type->load_xmm_32_prefer = true;
  float_type->load_signed = false;
  types["float"] = float_type;

  // INTEGER
  auto integer_type = std::make_shared<SimpleType>(8, types["number"], "integer");
  integer_type->is_parent_type = true;
  integer_type->is_value_type = true;
  types["integer"] = integer_type;

  // BINTEGER
  auto binteger_type = std::make_shared<SimpleType>(8, types["integer"], "binteger");
  binteger_type->is_value_type = true;
  types["binteger"] = binteger_type;

  // SINTEGER
  auto sinteger_type = std::make_shared<SimpleType>(8, types["integer"], "sinteger");
  sinteger_type->is_parent_type = true;
  sinteger_type->is_value_type = true;
  types["sinteger"] = sinteger_type;

  // INT8
  auto int8_type = std::make_shared<SimpleType>(1, types["sinteger"], "int8");
  int8_type->is_value_type = true;
  int8_type->load_size = 1;
  int8_type->load_signed = true;
  types["int8"] = int8_type;

  // INT16
  auto int16_type = std::make_shared<SimpleType>(2, types["sinteger"], "int16");
  int16_type->is_value_type = true;
  int16_type->load_size = 2;
  int16_type->load_signed = true;
  int16_type->minimum_alignment = 2;
  types["int16"] = int16_type;

  // INT32
  auto int32_type = std::make_shared<SimpleType>(4, types["sinteger"], "int32");
  int32_type->is_value_type = true;
  int32_type->load_size = 4;
  int32_type->load_signed = true;
  types["int32"] = int32_type;

  // INT64
  auto int64_type = std::make_shared<SimpleType>(8, types["sinteger"], "int64");
  int64_type->is_value_type = true;
  int64_type->load_size = 8;
  int64_type->load_signed = true;
  int64_type->minimum_alignment = 8;
  types["int64"] = int64_type;

  // UINTEGER
  auto uinteger_type = std::make_shared<SimpleType>(8, types["integer"], "uinteger");
  uinteger_type->is_parent_type = true;
  uinteger_type->is_value_type = true;
  types["uinteger"] = uinteger_type;

  // UINT8
  auto uint8_type = std::make_shared<SimpleType>(1, types["uinteger"], "uint8");
  uint8_type->is_value_type = true;
  uint8_type->load_size = 1;
  uint8_type->load_signed = false;
  types["uint8"] = uint8_type;

  // UINT16
  auto uint16_type = std::make_shared<SimpleType>(2, types["uinteger"], "uint16");
  uint16_type->is_value_type = true;
  uint16_type->load_size = 2;
  uint16_type->load_signed = false;
  types["uint16"] = uint16_type;

  // UINT32
  auto uint32_type = std::make_shared<SimpleType>(4, types["uinteger"], "uint32");
  uint32_type->is_value_type = true;
  uint32_type->load_size = 4;
  uint32_type->load_signed = false;
  types["uint32"] = uint32_type;

  // UINT64
  auto uint64_type = std::make_shared<SimpleType>(8, types["uinteger"], "uint64");
  uint64_type->is_value_type = true;
  uint64_type->load_size = 8;
  uint64_type->load_signed = false;
  uint64_type->minimum_alignment = 8;
  types["uint64"] = uint64_type;

  // INLINE-ARRAY (not runtime)
  auto inline_array_type = std::make_shared<SimpleType>(4, types["pointer"], "inline-array");
  types["inline-array"] = inline_array_type;

  // TYPE
  type_type->fields.emplace_back(types["symbol"], "symbol", 0 + 4);
  type_type->fields.emplace_back(types["type"], "parent", 4 + 4);
  type_type->fields.emplace_back(types["uint16"], "asize", 8 + 4);
  type_type->fields.emplace_back(types["uint16"], "padded-size", 10 + 4);
  type_type->fields.emplace_back(types["uint16"], "heap-base", 12 + 4);
  type_type->fields.emplace_back(types["uint16"], "num-methods", 14 + 4);
  type_type->fields.emplace_back(types["function"], "methods", 16 + 4, false, true);

  // STRING
  string_type->fields.emplace_back(types["int32"], "allocated-length", 4);
  string_type->fields.emplace_back(types["uint8"], "data", 8, false, true);

  // KHEAP
  kheap_type->fields.emplace_back(types["pointer"], "base", 0);
  kheap_type->fields.emplace_back(types["pointer"], "top", 4);
  kheap_type->fields.emplace_back(types["pointer"], "cur", 8);
  kheap_type->fields.emplace_back(types["pointer"], "top-base", 12);

  // ARRAY
  array_type->fields.emplace_back(types["int32"], "length", 4 + 0);
  array_type->fields.emplace_back(types["int32"], "allocated-length", 4 + 4);
  array_type->fields.emplace_back(types["type"], "elt-type", 4 + 8);

  // PROCESS TREE
  process_tree_type->fields.emplace_back(types["basic"], "name", 0 + 4);
  process_tree_type->fields.emplace_back(types["int32"], "mask", 4 + 4);
  process_tree_type->fields.emplace_back(TypeSpec(types["pointer"], {types["process-tree"]}),
                                         "parent", 8 + 4);
  process_tree_type->fields.emplace_back(TypeSpec(types["pointer"], {types["process-tree"]}),
                                         "brother", 12 + 4);
  process_tree_type->fields.emplace_back(TypeSpec(types["pointer"], {types["process-tree"]}),
                                         "child", 16 + 4);
  process_tree_type->fields.emplace_back(
      TypeSpec(types["pointer"], {TypeSpec(types["process-tree"])}), "ppointer", 20 + 4);
  process_tree_type->fields.emplace_back(types["process-tree"], "self", 24 + 4);

  // PROCESS
  process_type->fields.emplace_back(types["basic"], "name", 0 + 4);
  process_type->fields.emplace_back(types["int32"], "mask", 4 + 4);
  process_type->fields.emplace_back(TypeSpec(types["pointer"], {types["process-tree"]}), "parent",
                                    8 + 4);
  process_type->fields.emplace_back(TypeSpec(types["pointer"], {types["process-tree"]}), "brother",
                                    12 + 4);
  process_type->fields.emplace_back(TypeSpec(types["pointer"], {types["process-tree"]}), "child",
                                    16 + 4);
  process_type->fields.emplace_back(TypeSpec(types["pointer"], {TypeSpec(types["process-tree"])}),
                                    "ppointer", 20 + 4);
  process_type->fields.emplace_back(types["process-tree"], "self", 24 + 4);
  process_type->fields.emplace_back(types["basic"], "pool", 0x1c + 4);
  process_type->fields.emplace_back(types["basic"], "status", 0x20 + 4);
  process_type->fields.emplace_back(types["int32"], "pid", 0x24 + 4);
  process_type->fields.emplace_back(types["thread"], "main-thread", 0x28 + 4);
  process_type->fields.emplace_back(types["thread"], "top-thread", 0x2c + 4);
  process_type->fields.emplace_back(types["basic"], "entity", 0x30 + 4);
  process_type->fields.emplace_back(types["basic"], "state", 0x34 + 4);
  process_type->fields.emplace_back(types["function"], "trans-hook", 0x38 + 4);
  process_type->fields.emplace_back(types["function"], "post-hook", 0x3c + 4);
  process_type->fields.emplace_back(types["basic"], "event-hook", 0x40 + 4);
  process_type->fields.emplace_back(types["int32"], "allocated-length", 0x44 + 4);
  process_type->fields.emplace_back(types["basic"], "next-state", 0x48 + 4);
  process_type->fields.emplace_back(types["pointer"], "heap-base", 0x4c + 4);
  process_type->fields.emplace_back(types["pointer"], "heap-top", 0x50 + 4);
  process_type->fields.emplace_back(types["pointer"], "heap-cur", 0x54 + 4);
  process_type->fields.emplace_back(types["stack-frame"], "stack-frame-top", 0x58 + 4);
  process_type->fields.emplace_back(types["connectable"], "connection-list", 0x5c + 4);
  process_type->fields.back().is_inline = true;
  //  process_type->fields.emplace_back(types["basic"], "pool", 0x1c + 4); todo - connection list
  process_type->fields.emplace_back(types["uint8"], "stack", 0x6c + 4);
  process_type->fields.back().is_dynamic = true;

  stack_frame_type->fields.emplace_back(types["basic"], "name", 4);
  stack_frame_type->fields.emplace_back(types["stack-frame"], "next", 8);

  add_method("process-tree", "new", {"type", "symbol", "basic"});

  for (auto& kv : types) {
    for (auto& name : default_methods) {
      add_method(kv.first, name, TypeSpec(types["function"]));
    }
  }

  add_method("type", "new", {"type", "symbol", "type", "integer"});

  //  for(auto& type : types) {
  //    printf("----%s----\n%s\n\n", type.first.c_str(), type.second->print().c_str());
  //  }

  // string's size (u32), data (inline array) field.
  // symbol's value, hash, string field.
  // boolean's value, hash, string field.
}

bool TypeSpec::typecheck_base_only(TypeSpec& more_specific, TypeContainer& types) {
  auto other = more_specific.type;
  if (other->get_name() == type->get_name()) {
    return true;
  }

  while (other->get_name() != "object") {
    other = types.lookup(other->parent);
    if (other->get_name() == type->get_name()) {
      return true;
    }
  }
  return false;
}

TypeSpec::TypeSpec(Object& o, TypeContainer& types) {
  switch (o.type) {
    case SYMBOL: {
      auto t = types.types.find(o.as_symbol()->name);
      if (t == types.types.end()) {
        throw std::runtime_error("typespec cannot be created because the type is unknown: " +
                                 o.print());
      }
      type = t->second;
    } break;
    default:
      throw std::runtime_error("can't make typespec from " + o.print());
  }
}

std::string TypeSpec::print() {
  if (ts_args.empty()) {
    return type->get_name();
  } else {
    std::string result = "(" + type->get_name();
    for (auto& x : ts_args) {
      result += " " + x.print();
    }
    return result + ")";
  }
}

bool BitfieldType::find_field(const std::string& field_name, GoalBitField* field) {
  for (auto& f : fields) {
    if (f.name == field_name) {
      *field = f;
      return true;
    }
  }
  return false;
}