#include "third-party/fmt/core.h"
#include "StaticObject.h"
#include "common/goal_constants.h"

namespace {
template <typename T>
uint32_t push_data_to_byte_vector(T data, std::vector<uint8_t>& v) {
  auto* ptr = (uint8_t*)(&data);
  for (std::size_t i = 0; i < sizeof(T); i++) {
    v.push_back(ptr[i]);
  }
  return sizeof(T);
}
}  // namespace

////////////////
// StaticString
////////////////
StaticString::StaticString(std::string text_data, int _seg)
    : StaticBasic(_seg, "string"), text(std::move(text_data)) {}

std::string StaticString::print() const {
  return fmt::format("static-string \"{}\"", text);
}

void StaticString::generate(emitter::ObjectGenerator* gen) {
  rec = gen->add_static_to_seg(seg, 16);
  auto& d = gen->get_static_data(rec);

  // add "string" type tag:
  gen->link_static_type_ptr(rec, d.size(), "string");
  for (int i = 0; i < POINTER_SIZE; i++) {
    d.push_back(0xbe);
  }

  // add allocated size
  push_data_to_byte_vector<u32>(text.size(), d);

  // add chars
  for (auto c : text) {
    d.push_back(c);
  }
  d.push_back(0);
}

////////////////
// StaticFloat
////////////////

StaticFloat::StaticFloat(float _value, int _seg) : value(_value), seg(_seg) {}

StaticObject::LoadInfo StaticFloat::get_load_info() const {
  LoadInfo info;
  info.requires_load = true;
  info.load_size = 4;
  info.load_signed = false;
  info.prefer_xmm = true;
  return info;
}

void StaticFloat::generate(emitter::ObjectGenerator* gen) {
  rec = gen->add_static_to_seg(seg, 4);
  auto& d = gen->get_static_data(rec);
  push_data_to_byte_vector<float>(value, d);
}

int StaticFloat::get_addr_offset() const {
  return 0;
}

std::string StaticFloat::print() const {
  return fmt::format("(sf {})", value);
}

///////////////////
// StaticStructure
///////////////////

StaticStructure::StaticStructure(int _seg) : seg(_seg) {}

std::string StaticStructure::print() const {
  return "static-structure";
}

StaticObject::LoadInfo StaticStructure::get_load_info() const {
  LoadInfo info;
  info.requires_load = false;
  info.prefer_xmm = false;
  return info;
}

int StaticStructure::get_addr_offset() const {
  return 0;
}

void StaticStructure::generate_structure(emitter::ObjectGenerator* gen) {
  rec = gen->add_static_to_seg(seg, 16);
  auto& d = gen->get_static_data(rec);
  d.insert(d.end(), data.begin(), data.end());
  for (auto& sym : symbols) {
    gen->link_static_symbol_ptr(rec, sym.offset, sym.name);
  }

  for (auto& ptr : pointers) {
    gen->link_static_pointer(rec, ptr.offset_in_this, ptr.dest->rec, ptr.offset_in_dest);
  }

  for (auto& type : types) {
    gen->link_static_type_ptr(rec, type.offset, type.name);
  }
}

void StaticStructure::generate(emitter::ObjectGenerator* gen) {
  generate_structure(gen);
}

void StaticStructure::add_symbol_record(std::string name, int offset) {
  SymbolRecord srec;
  srec.name = std::move(name);
  srec.offset = offset;
  symbols.push_back(srec);
}

void StaticStructure::add_pointer_record(int offset_in_this,
                                         StaticStructure* dest,
                                         int offset_in_dest) {
  PointerRecord prec;
  prec.offset_in_this = offset_in_this;
  prec.dest = dest;
  prec.offset_in_dest = offset_in_dest;
  pointers.push_back(prec);
}

void StaticStructure::add_type_record(std::string name, int offset) {
  SymbolRecord srec;
  srec.name = std::move(name);
  srec.offset = offset;
  types.push_back(srec);
}

///////////////////
// StaticBasic
///////////////////

StaticBasic::StaticBasic(int _seg, std::string _type_name)
    : StaticStructure(_seg), type_name(std::move(_type_name)) {
  add_type_record(type_name, 0);
}

int StaticBasic::get_addr_offset() const {
  return BASIC_OFFSET;
}

///////////////////
// StaticPair
///////////////////

StaticPair::StaticPair(StaticResult car, StaticResult cdr, int _seg)
    : StaticStructure(_seg), m_car(std::move(car)), m_cdr(std::move(cdr)) {}

int StaticPair::get_addr_offset() const {
  return PAIR_OFFSET;
}

void StaticPair::generate(emitter::ObjectGenerator* gen) {
  data.resize(2 * POINTER_SIZE);  // size of pair
  generate_item(m_car, 0);
  generate_item(m_cdr, 4);
  generate_structure(gen);
}

void StaticPair::generate_item(const StaticResult& item, int offset) {
  if (item.is_reference()) {
    add_pointer_record(offset, item.reference(), item.reference()->get_addr_offset());
  } else if (item.is_symbol()) {
    add_symbol_record(item.symbol_name(), offset);
    u32 symbol_placeholder = 0xffffffff;
    memcpy(data.data() + offset, &symbol_placeholder, POINTER_SIZE);
  } else if (item.is_constant_data()) {
    // if it's a constant data, it should always be a boxed integer for a pair.
    // or I guess you could put a normal integer too. Either way, we assume signed here,
    // though we may need to allow overflow so you can store either signed/unsigned things in pairs
    s32 value = item.get_as_s32();
    memcpy(data.data() + offset, &value, POINTER_SIZE);
  }
}

///////////////////
// StaticResult
///////////////////

StaticResult StaticResult::make_structure_reference(StaticStructure* structure, TypeSpec ts) {
  StaticResult result;
  result.m_kind = Kind::STRUCTURE_REFERENCE;
  result.m_struct = structure;
  result.m_ts = std::move(ts);
  return result;
}

StaticResult StaticResult::make_constant_data(u64 value, TypeSpec ts) {
  StaticResult result;
  result.m_kind = Kind::CONSTANT_DATA;
  result.m_constant_data = value;
  result.m_ts = std::move(ts);
  return result;
}

StaticResult StaticResult::make_symbol(const std::string& name) {
  StaticResult result;
  result.m_kind = Kind::SYMBOL;
  result.m_symbol = name;
  result.m_ts = TypeSpec("symbol");
  return result;
}