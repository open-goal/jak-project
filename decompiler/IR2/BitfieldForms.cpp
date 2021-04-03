#include "Form.h"
#include "common/goos/PrettyPrinter.h"

namespace decompiler {
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
                           bool looking_for_unsigned) {
  for (auto& field : type->fields()) {
    if (field.size() == size && field.offset() == start_bit) {
      bool is_int = ts.tc(TypeSpec("integer"), field.type());
      bool is_uint = ts.tc(TypeSpec("uinteger"), field.type());
      // allow sign match, or unsigned+not a number.
      if ((is_int && !looking_for_unsigned) || (is_uint && looking_for_unsigned) ||
          (!is_int && !is_uint && looking_for_unsigned)) {
        // matched!
        return field;
      }
    }
  }
  throw std::runtime_error(
      fmt::format("Unmatched field: start bit {}, size {}, signed? {}, type {}", start_bit, size,
                  !looking_for_unsigned, type->get_name()));
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
    m_steps.push_back(step);
    return nullptr;
  }

  if (m_steps.empty() && step.is_right_shift()) {
    // could be a single step!
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

  throw std::runtime_error("Unknown state in BitfieldReadElement");
}

}  // namespace decompiler