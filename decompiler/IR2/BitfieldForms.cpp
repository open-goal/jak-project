#include "Form.h"
#include "common/goos/PrettyPrinter.h"
#include "common/util/Range.h"
#include "common/util/BitUtils.h"

namespace decompiler {

////////////////////////////////
// BitfieldStaticDefElement
////////////////////////////////

BitfieldStaticDefElement::BitfieldStaticDefElement(const TypeSpec& type,
                                                   const std::vector<BitFieldDef>& field_defs)
    : m_type(type), m_field_defs(field_defs) {}

goos::Object BitfieldStaticDefElement::to_form_internal(const Env&) const {
  std::vector<goos::Object> result;

  result.push_back(pretty_print::to_symbol(fmt::format("new 'static '{}", m_type.print())));

  for (auto& def : m_field_defs) {
    if (def.is_signed) {
      result.push_back(
          pretty_print::to_symbol(fmt::format(":{} {}", def.field_name, (s64)def.value)));
    } else {
      result.push_back(
          pretty_print::to_symbol(fmt::format(":{} #x{:x}", def.field_name, def.value)));
    }
  }

  return pretty_print::build_list(result);
}

void BitfieldStaticDefElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void BitfieldStaticDefElement::apply_form(const std::function<void(Form*)>&) {}
void BitfieldStaticDefElement::collect_vars(RegAccessSet&, bool) const {}
void BitfieldStaticDefElement::get_modified_regs(RegSet&) const {}

ModifiedCopyBitfieldElement::ModifiedCopyBitfieldElement(
    const TypeSpec& type,
    Form* base,
    const std::vector<BitfieldFormDef>& field_modifications)
    : m_type(type), m_base(base), m_field_modifications(field_modifications) {
  m_base->parent_element = this;
}

goos::Object ModifiedCopyBitfieldElement::to_form_internal(const Env& env) const {
  std::vector<goos::Object> result = {pretty_print::to_symbol("copy-and-set-bf")};
  result.push_back(m_base->to_form(env));
  for (auto& def : m_field_modifications) {
    result.push_back(pretty_print::to_symbol(fmt::format(":{}", def.field_name)));
    result.push_back(def.value->to_form(env));
  }
  return pretty_print::build_list(result);
}

void ModifiedCopyBitfieldElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  for (auto& mod : m_field_modifications) {
    mod.value->apply(f);
  }
  m_base->apply(f);
}

void ModifiedCopyBitfieldElement::apply_form(const std::function<void(Form*)>& f) {
  for (auto& mod : m_field_modifications) {
    mod.value->apply_form(f);
  }
  m_base->apply_form(f);
}

void ModifiedCopyBitfieldElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    for (auto& mod : m_field_modifications) {
      mod.value->collect_vars(vars, recursive);
    }
    m_base->collect_vars(vars, recursive);
  }
}

void ModifiedCopyBitfieldElement::get_modified_regs(RegSet& regs) const {
  for (auto& mod : m_field_modifications) {
    mod.value->get_modified_regs(regs);
  }
  m_base->get_modified_regs(regs);
}

////////////////////////////////
// BitfieldReadElement
////////////////////////////////

BitfieldReadElement::BitfieldReadElement(Form* base_value, const TypeSpec& ts)
    : m_base(base_value), m_type(ts) {
  m_base->parent_element = this;
}

goos::Object BitfieldReadElement::to_form_internal(const Env& env) const {
  return pretty_print::build_list("incomplete-bitfield-access", m_base->to_form(env));
}

void BitfieldReadElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  m_base->apply(f);
}

void BitfieldReadElement::apply_form(const std::function<void(Form*)>& f) {
  m_base->apply_form(f);
}

void BitfieldReadElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  m_base->collect_vars(vars, recursive);
}

void BitfieldReadElement::get_modified_regs(RegSet& regs) const {
  m_base->get_modified_regs(regs);
}

namespace {
const BitField& find_field(const TypeSystem& ts,
                           const BitFieldType* type,
                           int start_bit,
                           int size,
                           std::optional<bool> looking_for_unsigned) {
  for (auto& field : type->fields()) {
    if (field.size() == size && field.offset() == start_bit) {
      bool is_int = ts.tc(TypeSpec("integer"), field.type());
      bool is_uint = ts.tc(TypeSpec("uinteger"), field.type());
      // allow sign match, or unsigned+not a number.

      if (looking_for_unsigned) {
        bool want_unsigned = *looking_for_unsigned;
        if ((is_int && !want_unsigned) || (is_uint && want_unsigned) ||
            (!is_int && !is_uint && want_unsigned)) {
          // matched!
          return field;
        }
      } else {
        return field;
      }
    }
  }
  throw std::runtime_error(
      fmt::format("Unmatched field: start bit {}, size {}, signed? {}, type {}", start_bit, size,
                  !looking_for_unsigned, type->get_name()));
}

std::optional<BitField> find_field_from_mask(const TypeSystem& ts,
                                             const BitFieldType* type,
                                             uint64_t mask) {
  // try to find a field that is masked by this:
  auto mask_range = get_bit_range(mask);
  if (!mask_range) {
    // it wasn't even a valid mask...
    return {};
  }

  return find_field(ts, type, mask_range->first(), mask_range->size(), {});
}

template <typename T>
T extract_bitfield(T input, int start_bit, int size) {
  int end_bit = start_bit + size;
  T left_shifted = input << (64 - end_bit);
  return left_shifted >> (64 - size);
}
}  // namespace

/*!
 * Add a step to the bitfield access. If this completes the access, returns a form representing the
 * access
 */
FormElement* BitfieldReadElement::push_step(const BitfieldManip step,
                                            const TypeSystem& ts,
                                            FormPool& pool) {
  if (m_steps.empty() && step.kind == BitfieldManip::Kind::LEFT_SHIFT) {
    // for left/right shift combo to get a field.
    m_steps.push_back(step);
    return nullptr;
  }

  if (m_steps.empty() && step.is_right_shift()) {
    // could be a single step right shift to get a field
    bool is_unsigned = step.right_shift_unsigned();
    int shift_size = step.get_shift_start_bit();
    int size = shift_size - step.amount;
    int start_bit = shift_size - size;
    auto type = ts.lookup_type(m_type);
    auto as_bitfield = dynamic_cast<BitFieldType*>(type);
    assert(as_bitfield);
    auto field = find_field(ts, as_bitfield, start_bit, size, is_unsigned);
    return pool.alloc_element<DerefElement>(m_base, false,
                                            DerefToken::make_field_name(field.name()));
  }

  if (m_steps.size() == 1 && m_steps.at(0).kind == BitfieldManip::Kind::LEFT_SHIFT) {
    // second op in left/right shift combo
    int end_bit = 64 - m_steps.at(0).amount;
    if (!step.is_right_shift()) {
      throw std::runtime_error("Unexpected step 1");
    }
    bool is_unsigned = step.right_shift_unsigned();

    int size = 64 - step.amount;
    int start_bit = end_bit - size;
    assert(start_bit >= 0);

    auto type = ts.lookup_type(m_type);
    auto as_bitfield = dynamic_cast<BitFieldType*>(type);
    assert(as_bitfield);
    auto field = find_field(ts, as_bitfield, start_bit, size, is_unsigned);
    return pool.alloc_element<DerefElement>(m_base, false,
                                            DerefToken::make_field_name(field.name()));
  }

  if (m_steps.empty() && step.kind == BitfieldManip::Kind::LOGAND) {
    // and with mask
    m_steps.push_back(step);
    return nullptr;
  }

  if (m_steps.size() == 1 && m_steps.at(0).kind == BitfieldManip::Kind::LOGAND &&
      step.kind == BitfieldManip::Kind::NONZERO_COMPARE) {
    auto type = ts.lookup_type(m_type);
    auto as_bitfield = dynamic_cast<BitFieldType*>(type);
    assert(as_bitfield);
    auto field = find_field_from_mask(ts, as_bitfield, m_steps.at(0).amount);
    if (field) {
      auto get_field = pool.alloc_element<DerefElement>(m_base, false,
                                                        DerefToken::make_field_name(field->name()));
      get_field->inline_nested();
      return pool.alloc_element<GenericElement>(
          GenericOperator::make_compare(IR2_Condition::Kind::NONZERO),
          pool.alloc_single_form(nullptr, get_field));
    }
  }

  if (m_steps.size() == 1 && m_steps.at(0).kind == BitfieldManip::Kind::LOGAND &&
      step.kind == BitfieldManip::Kind::LOGIOR_WITH_CONSTANT_INT) {
    // this is setting a bitfield.
    // first, let's check that the mask is used properly:
    u64 mask = m_steps.at(0).amount;
    u64 value = step.amount;
    if ((mask & value) != 0) {
      throw std::runtime_error(fmt::format(
          "Got invalid bitmask set: mask was 0x{:x} and value was 0x{:x}", mask, value));
    }
    auto type = ts.lookup_type(m_type);
    auto as_bitfield = dynamic_cast<BitFieldType*>(type);
    assert(as_bitfield);
    // use the mask to figure out the field.
    auto field = find_field_from_mask(ts, as_bitfield, ~mask);
    assert(field);
    bool is_signed =
        ts.tc(TypeSpec("int"), field->type()) && !ts.tc(TypeSpec("uint"), field->type());

    // use the field to figure out what value is being set.
    u64 set_value;
    if (is_signed) {
      set_value = extract_bitfield<s64>(value, field->offset(), field->size());
    } else {
      set_value = extract_bitfield<u64>(value, field->offset(), field->size());
    }

    BitfieldFormDef def;
    def.field_name = field->name();
    def.value = pool.alloc_single_element_form<SimpleAtomElement>(
        nullptr, SimpleAtom::make_int_constant(set_value));
    return pool.alloc_element<ModifiedCopyBitfieldElement>(m_type, m_base,
                                                           std::vector<BitfieldFormDef>{def});
  }

  throw std::runtime_error("Unknown state in BitfieldReadElement");
}

std::vector<BitFieldDef> decompile_static_bitfield(const TypeSpec& type,
                                                   const TypeSystem& ts,
                                                   u64 value) {
  u64 touched_bits = 0;
  std::vector<BitFieldDef> result;

  auto type_info = dynamic_cast<BitFieldType*>(ts.lookup_type(type));
  assert(type_info);

  for (auto& field : type_info->fields()) {
    u64 bitfield_value;
    bool is_signed = ts.tc(TypeSpec("int"), field.type()) && !ts.tc(TypeSpec("uint"), field.type());
    if (is_signed) {
      // signed
      s64 signed_value = value;
      bitfield_value = extract_bitfield<s64>(signed_value, field.offset(), field.size());
    } else {
      // unsigned
      bitfield_value = extract_bitfield<u64>(value, field.offset(), field.size());
    }

    if (bitfield_value != 0) {
      BitFieldDef def;
      def.value = bitfield_value;
      def.field_name = field.name();
      def.is_signed = is_signed;
      result.push_back(def);
    }

    for (int i = field.offset(); i < field.offset() + field.size(); i++) {
      touched_bits |= (u64(1) << i);
    }
  }

  u64 untouched_but_set = value & (~touched_bits);

  if (untouched_but_set) {
    throw std::runtime_error(
        fmt::format("Failed to decompile static bitfield of type {}. Original value is 0x{:x} but "
                    "we didn't touch",
                    type.print(), value, untouched_but_set));
  }
  return result;
}

}  // namespace decompiler