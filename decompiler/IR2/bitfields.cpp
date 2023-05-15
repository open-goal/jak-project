#include "bitfields.h"

#include "common/goos/PrettyPrinter.h"
#include "common/log/log.h"
#include "common/util/BitUtils.h"
#include "common/util/Range.h"
#include "common/util/print_float.h"

#include "decompiler/Function/Function.h"
#include "decompiler/IR2/Form.h"
#include "decompiler/IR2/GenericElementMatcher.h"
#include "decompiler/util/DecompilerTypeSystem.h"

namespace decompiler {

////////////////////////////////
// BitfieldStaticDefElement
////////////////////////////////

BitfieldStaticDefElement::BitfieldStaticDefElement(const TypeSpec& type,
                                                   const std::vector<BitFieldDef>& field_defs)
    : m_type(type), m_field_defs(field_defs) {
  for (auto& x : field_defs) {
    x.value->parent_element = this;
  }
}

BitfieldStaticDefElement::BitfieldStaticDefElement(
    const TypeSpec& type,
    const std::vector<BitFieldConstantDef>& field_defs,
    FormPool& pool)
    : m_type(type) {
  for (auto& x : field_defs) {
    m_field_defs.push_back(BitFieldDef::from_constant(x, pool));
    m_field_defs.back().value->parent_element = this;
  }
}

goos::Object BitfieldStaticDefElement::to_form_internal(const Env& env) const {
  std::vector<goos::Object> result;

  result.push_back(pretty_print::to_symbol(fmt::format("new 'static '{}", m_type.print())));

  for (auto& def : m_field_defs) {
    auto def_as_atom = form_as_atom(def.value);
    if (def_as_atom && def_as_atom->is_int()) {
      u64 v = def_as_atom->get_int();
      if (def.is_float) {
        float vf;
        memcpy(&vf, &v, 4);
        result.push_back(pretty_print::to_symbol(
            fmt::format(":{} {}", def.field_name, float_to_string(vf, true))));
      } else if (def.is_signed) {
        result.push_back(pretty_print::to_symbol(fmt::format(":{} {}", def.field_name, (s64)v)));
      } else {
        result.push_back(pretty_print::to_symbol(fmt::format(":{} #x{:x}", def.field_name, v)));
      }
    } else {
      // TODO: will this make ugly massive lines?
      result.push_back(pretty_print::to_symbol(
          fmt::format(":{} {}", def.field_name, def.value->to_string(env))));
    }
  }

  return pretty_print::build_list(result);
}

void BitfieldStaticDefElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  for (auto& x : m_field_defs) {
    x.value->apply(f);
  }
}

void BitfieldStaticDefElement::apply_form(const std::function<void(Form*)>& f) {
  for (auto& x : m_field_defs) {
    x.value->apply_form(f);
  }
}
void BitfieldStaticDefElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    for (auto& x : m_field_defs) {
      x.value->collect_vars(vars, recursive);
    }
  }
}
void BitfieldStaticDefElement::get_modified_regs(RegSet& regs) const {
  for (auto& x : m_field_defs) {
    x.value->get_modified_regs(regs);
  }
}

ModifiedCopyBitfieldElement::ModifiedCopyBitfieldElement(
    const TypeSpec& type,
    Form* base,
    bool from_pcpyud,
    const std::vector<BitFieldDef>& field_modifications)
    : m_type(type),
      m_base(base),
      m_field_modifications(field_modifications),
      m_from_pcpyud(from_pcpyud) {
  m_base->parent_element = this;
  for (auto& mod : m_field_modifications) {
    if (mod.value) {
      mod.value->parent_element = this;
    }
  }
}

goos::Object ModifiedCopyBitfieldElement::to_form_internal(const Env& env) const {
  if (m_field_modifications.size() == 1) {
    std::vector<goos::Object> result = {pretty_print::to_symbol("copy-and-set-field")};
    result.push_back(m_base->to_form(env));
    for (auto& def : m_field_modifications) {
      result.push_back(pretty_print::to_symbol(fmt::format("{}", def.field_name)));
      result.push_back(def.value->to_form(env));
    }
    return pretty_print::build_list(result);
  } else {
    std::vector<goos::Object> result = {pretty_print::to_symbol("copy-and-set-bf-multi")};
    result.push_back(m_base->to_form(env));
    for (auto& def : m_field_modifications) {
      result.push_back(pretty_print::to_symbol(fmt::format(":{}", def.field_name)));
      result.push_back(def.value->to_form(env));
    }
    return pretty_print::build_list(result);
  }
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

BitfieldAccessElement::BitfieldAccessElement(Form* base_value, const TypeSpec& ts)
    : m_base(base_value), m_type(ts) {
  m_base->parent_element = this;
}

goos::Object BitfieldAccessElement::to_form_internal(const Env& env) const {
  if (m_current_result) {
    return m_current_result->to_form(env);
  }
  return pretty_print::build_list("incomplete-bitfield-access", m_base->to_form(env));
}

void BitfieldAccessElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  m_base->apply(f);
}

void BitfieldAccessElement::apply_form(const std::function<void(Form*)>& f) {
  m_base->apply_form(f);
}

void BitfieldAccessElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  m_base->collect_vars(vars, recursive);
}

void BitfieldAccessElement::get_modified_regs(RegSet& regs) const {
  m_base->get_modified_regs(regs);
}

std::optional<BitField> try_find_field(const TypeSystem& ts,
                                       const BitFieldType* type,
                                       int start_bit,
                                       int size,
                                       std::optional<bool> looking_for_unsigned) {
  for (auto& field : type->fields()) {
    if (field.size() == size && field.offset() == start_bit) {
      // the GOAL compiler sign extended floats.
      bool is_int =
          ts.tc(TypeSpec("integer"), field.type()) || ts.tc(TypeSpec("float"), field.type());
      bool is_uint = ts.tc(TypeSpec("uinteger"), field.type());
      // allow sign match, or unsigned+not a number.

      if (looking_for_unsigned) {
        bool want_unsigned = *looking_for_unsigned;
        if ((is_int && !want_unsigned) || (is_uint && want_unsigned) ||
            (!is_int && !is_uint && want_unsigned)) {
          // matched!
          return field;
        }

        // this is a hack to work around bad code where extracting the (pointer process)
        // would sign extend the address.
        if (!is_int && !is_uint && type->get_name() == "handle") {
          return field;
        }
      } else {
        return field;
      }
    }
  }
  return {};
}

BitField find_field(const TypeSystem& ts,
                    const BitFieldType* type,
                    int start_bit,
                    int size,
                    std::optional<bool> looking_for_unsigned) {
  auto result = try_find_field(ts, type, start_bit, size, looking_for_unsigned);
  if (!result) {
    throw std::runtime_error(
        fmt::format("Unmatched field: start bit {}, size {}, signed? {}, type {}", start_bit, size,
                    !looking_for_unsigned, type->get_name()));
  } else {
    return *result;
  }
}

namespace {
std::optional<BitField> find_field_from_mask(const TypeSystem& ts,
                                             const BitFieldType* type,
                                             uint64_t mask,
                                             bool upper_64) {
  // try to find a field that is masked by this:
  auto mask_range = get_bit_range(mask);
  if (!mask_range) {
    // it wasn't even a valid mask...
    return {};
  }

  if (upper_64) {
    mask_range = Range<int>(mask_range->first() + 64, mask_range->last() + 64);
  }

  return find_field(ts, type, mask_range->first(), mask_range->size(), {});
}

Form* strip_int_or_uint_cast(Form* in) {
  auto as_cast = in->try_as_element<CastElement>();
  if (as_cast && (as_cast->type() == TypeSpec("int") || as_cast->type() == TypeSpec("uint"))) {
    return as_cast->source();
  }
  return in;
}

std::optional<BitFieldDef> get_bitfield_initial_set(Form* form,
                                                    const BitFieldType* type,
                                                    const TypeSystem& ts,
                                                    int offset_in_bitfield) {
  // (shr (shl arg1 59) 44) for example
  {
    auto matcher = Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::SHR),
                               {Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::SHL),
                                            {Matcher::any(0), Matcher::any_integer(1)}),
                                Matcher::any_integer(2)});
    auto mr = match(matcher, strip_int_or_uint_cast(form));
    if (mr.matched) {
      auto value = mr.maps.forms.at(0);
      int left = mr.maps.ints.at(1);
      int right = mr.maps.ints.at(2);
      int size = 64 - left;
      int offset = left - right;
      auto f = find_field(ts, type, offset + offset_in_bitfield, size, {});
      BitFieldDef def;
      def.value = value;
      def.field_name = f.name();
      def.is_signed = false;  // we don't know.
      return def;
    }
  }

  {
    auto matcher = Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::DIVISION),
                               {Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::SHL),
                                            {Matcher::any(0), Matcher::any_integer(1)}),
                                Matcher::any_integer(2)});
    auto mr = match(matcher, strip_int_or_uint_cast(form));
    if (mr.matched) {
      auto power_of_two = get_power_of_two(mr.maps.ints.at(2));
      if (power_of_two) {
        auto value = mr.maps.forms.at(0);
        int left = mr.maps.ints.at(1);
        int right = *power_of_two;
        int size = 64 - left;
        int offset = left - right;
        auto f = find_field(ts, type, offset + offset_in_bitfield, size, {});
        BitFieldDef def;
        def.value = value;
        def.field_name = f.name();
        def.is_signed = false;  // we don't know.
        return def;
      }
    }
  }

  // also possible to omit the shr if it would be zero:
  auto matcher_no_shr = Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::SHL),
                                    {Matcher::any(0), Matcher::any_integer(1)});
  auto mr_no_shr = match(matcher_no_shr, strip_int_or_uint_cast(form));
  if (mr_no_shr.matched) {
    auto value = mr_no_shr.maps.forms.at(0);
    int left = mr_no_shr.maps.ints.at(1);
    int right = 0;
    int size = 64 - left;
    int offset = left - right;
    auto f = try_find_field(ts, type, offset + offset_in_bitfield, size, {});
    if (!f) {
      return {};
    }
    BitFieldDef def;
    def.value = value;
    def.field_name = f->name();
    def.is_signed = false;  // we don't know.
    return def;
  }

  auto matcher_sllv =
      Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::ASM_SLLV_R0), {Matcher::any(0)});
  auto mr_sllv = match(matcher_sllv, strip_int_or_uint_cast(form));
  if (mr_sllv.matched) {
    auto value = mr_sllv.maps.forms.at(0);
    int size = 32;
    int offset = 0;
    auto f = find_field(ts, type, offset + offset_in_bitfield, size, {});
    BitFieldDef def;
    def.value = value;
    def.field_name = f.name();
    def.is_signed = false;
    return def;
  }

  return {};
}

}  // namespace

void BitfieldAccessElement::push_pcpyud(const TypeSystem& ts, FormPool& pool, const Env& /*env*/) {
  if (!m_steps.empty()) {
    throw std::runtime_error("Unexpected pcpyud!");
  }
  m_got_pcpyud = true;

  // look for a 64-bit field
  auto type = ts.lookup_type(m_type);
  auto as_bitfield = dynamic_cast<BitFieldType*>(type);
  ASSERT(as_bitfield);
  auto field = try_find_field(ts, as_bitfield, 64, 64, true);
  if (field) {
    auto result =
        pool.alloc_element<DerefElement>(m_base, false, DerefToken::make_field_name(field->name()));
    result->inline_nested();
    m_current_result = pool.alloc_single_form(this, result);
  }
}

std::string BitfieldAccessElement::debug_print(const Env& env) const {
  std::string result = "BitfieldAccessElement:";
  if (m_got_pcpyud) {
    result += "pcpyud";
  }
  result += '\n';
  result += fmt::format("base: {}\n", m_base->to_string(env));
  for (auto& step : m_steps) {
    result += fmt::format(" {}\n", step.print());
  }
  return result;
}

std::optional<BitField> BitfieldAccessElement::get_set_field_0(const TypeSystem& ts) const {
  if (m_got_pcpyud) {
    return {};
  }
  if (m_steps.size() != 1) {
    return {};
  }

  auto& step = m_steps.at(0);
  if (step.kind != BitfieldManip::Kind::LOGAND_WITH_CONSTANT_INT) {
    return {};
  }

  u64 mask = step.amount;
  auto type = ts.lookup_type(m_type);
  auto as_bitfield = dynamic_cast<BitFieldType*>(type);
  ASSERT(as_bitfield);
  // use the mask to figure out the field.
  auto field = find_field_from_mask(ts, as_bitfield, ~mask, m_got_pcpyud);
  if (field) {
    return field;
  } else {
    return {};
  }
}

/*!
 * Add a step to the bitfield access. If this completes the access, returns a form representing the
 * access
 */
FormElement* BitfieldAccessElement::push_step(const BitfieldManip step,
                                              const TypeSystem& ts,
                                              FormPool& pool,
                                              const Env& env) {
  int pcpyud_offset = m_got_pcpyud ? 64 : 0;

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
    int start_bit = pcpyud_offset + shift_size - size;
    auto type = ts.lookup_type(m_type);
    auto as_bitfield = dynamic_cast<BitFieldType*>(type);
    ASSERT(as_bitfield);
    auto field = find_field(ts, as_bitfield, start_bit, size, is_unsigned);
    auto result =
        pool.alloc_element<DerefElement>(m_base, false, DerefToken::make_field_name(field.name()));
    result->inline_nested();
    return result;
  }

  if (m_steps.empty() && step.kind == BitfieldManip::Kind::SLLV_SEXT) {
    bool is_unsigned = false;
    int size = 32;
    int start_bit = 0 + pcpyud_offset;
    auto type = ts.lookup_type(m_type);
    auto as_bitfield = dynamic_cast<BitFieldType*>(type);
    ASSERT(as_bitfield);
    auto field = find_field(ts, as_bitfield, start_bit, size, is_unsigned);
    auto result =
        pool.alloc_element<DerefElement>(m_base, false, DerefToken::make_field_name(field.name()));
    result->inline_nested();
    return result;
  }

  if (m_steps.empty() && step.kind == BitfieldManip::Kind::PEXTUW) {
    if (m_got_pcpyud) {
      throw std::runtime_error("unknown pcpyud PEXTUW sequence in bitfield");
    }
    bool is_unsigned = true;
    int size = 32;
    int start_bit = 64;
    auto type = ts.lookup_type(m_type);
    auto as_bitfield = dynamic_cast<BitFieldType*>(type);
    ASSERT(as_bitfield);
    auto field = find_field(ts, as_bitfield, start_bit, size, is_unsigned);
    auto result =
        pool.alloc_element<DerefElement>(m_base, false, DerefToken::make_field_name(field.name()));
    result->inline_nested();
    return result;
  }

  if (m_steps.size() == 1 && m_steps.at(0).kind == BitfieldManip::Kind::LEFT_SHIFT) {
    // second op in left/right shift combo
    int end_bit = pcpyud_offset + 64 - m_steps.at(0).amount;
    if (!step.is_right_shift()) {
      throw std::runtime_error("Unexpected step 1");
    }
    bool is_unsigned = step.right_shift_unsigned();

    int size = 64 - step.amount;
    int start_bit = end_bit - size;
    bool neg = false;
    if (start_bit < 0) {
      // throw std::runtime_error("push_step: Bad bitfield start bit");
      size += start_bit;
      start_bit = 0;
      neg = true;
    }

    auto type = ts.lookup_type(m_type);
    auto as_bitfield = dynamic_cast<BitFieldType*>(type);
    ASSERT(as_bitfield);
    auto field = find_field(ts, as_bitfield, start_bit, size, is_unsigned);
    auto deref =
        pool.alloc_element<DerefElement>(m_base, false, DerefToken::make_field_name(field.name()));
    deref->inline_nested();
    if (neg) {
      ASSERT(is_unsigned);
      auto result = pool.alloc_element<GenericElement>(
          GenericOperator::make_fixed(FixedOperatorKind::SHL),
          pool.form<CastElement>(TypeSpec("int"), pool.alloc_single_form(nullptr, deref)),
          pool.form<SimpleAtomElement>(SimpleAtom::make_int_constant(64 - step.amount)));
      return result;
    } else {
      return deref;
    }
  }

  if (m_steps.empty() && step.kind == BitfieldManip::Kind::LOGAND_WITH_CONSTANT_INT) {
    // and with mask
    // no need to do anything with pcpyud here.
    m_steps.push_back(step);
    return nullptr;
  }

  if (m_steps.size() == 1 && m_steps.at(0).kind == BitfieldManip::Kind::LOGAND_WITH_CONSTANT_INT &&
      step.kind == BitfieldManip::Kind::NONZERO_COMPARE) {
    if (m_got_pcpyud) {
      throw std::runtime_error("unknown pcpyud NONZERO_COMPARE sequence in bitfield");
    }
    auto type = ts.lookup_type(m_type);
    auto as_bitfield = dynamic_cast<BitFieldType*>(type);
    ASSERT(as_bitfield);
    auto field = find_field_from_mask(ts, as_bitfield, m_steps.at(0).amount, false);  // todo PCPYUP
    if (field) {
      auto get_field = pool.alloc_element<DerefElement>(m_base, false,
                                                        DerefToken::make_field_name(field->name()));
      get_field->inline_nested();
      return pool.alloc_element<GenericElement>(
          GenericOperator::make_compare(IR2_Condition::Kind::NONZERO),
          pool.alloc_single_form(nullptr, get_field));
    }
  }

  if (m_steps.size() == 1 && m_steps.at(0).kind == BitfieldManip::Kind::LOGAND_WITH_CONSTANT_INT &&
      step.kind == BitfieldManip::Kind::LOGIOR_WITH_CONSTANT_INT) {
    // this is setting a bitfield to a constant.
    // first, let's check that the mask is used properly:
    u64 mask = m_steps.at(0).amount;
    u64 value = step.amount;
    if ((mask & value) != 0) {
      throw std::runtime_error(fmt::format(
          "Got invalid bitmask set: mask was 0x{:x} and value was 0x{:x}", mask, value));
    }
    auto type = ts.lookup_type(m_type);
    auto as_bitfield = dynamic_cast<BitFieldType*>(type);
    ASSERT(as_bitfield);
    // use the mask to figure out the field.
    auto field = find_field_from_mask(ts, as_bitfield, ~mask, m_got_pcpyud);
    ASSERT(field);
    bool is_signed =
        ts.tc(TypeSpec("int"), field->type()) && !ts.tc(TypeSpec("uint"), field->type());

    // use the field to figure out what value is being set.
    u64 set_value;
    if (is_signed) {
      set_value = extract_bitfield<s64>(value, field->offset() - pcpyud_offset, field->size());
    } else {
      set_value = extract_bitfield<u64>(value, field->offset() - pcpyud_offset, field->size());
    }

    BitFieldDef def;
    def.field_name = field->name();
    def.value = pool.form<SimpleAtomElement>(SimpleAtom::make_int_constant(set_value));
    return pool.alloc_element<ModifiedCopyBitfieldElement>(m_type, m_base, m_got_pcpyud,
                                                           std::vector<BitFieldDef>{def});
  }

  if (m_steps.empty() && step.kind == BitfieldManip::Kind::LOGIOR_WITH_CONSTANT_INT) {
    // this is a rare case of setting a bitfield to a constant.  Usually we clear the field with an
    // and, then set the bits we want with an or.  In the case where we want to set all the bits to
    // 1, we can omit the and to clear first.

    // in this case, we expect the value we're oring with to be the appropriate mask for the field.

    lg::info("Rare bitfield set!");
    u64 value = step.amount;
    auto type = ts.lookup_type(m_type);
    auto as_bitfield = dynamic_cast<BitFieldType*>(type);
    ASSERT(as_bitfield);
    auto field = find_field_from_mask(ts, as_bitfield, value, m_got_pcpyud);
    ASSERT(field);
    bool is_signed =
        ts.tc(TypeSpec("int"), field->type()) && !ts.tc(TypeSpec("uint"), field->type());

    // use the field to figure out what value is being set.
    s64 set_value;
    if (is_signed) {
      set_value = extract_bitfield<s64>(value, field->offset() - pcpyud_offset, field->size());
    } else {
      set_value = extract_bitfield<u64>(value, field->offset() - pcpyud_offset, field->size());
    }

    BitFieldDef def;
    def.field_name = field->name();
    def.value = pool.form<SimpleAtomElement>(SimpleAtom::make_int_constant(set_value));
    return pool.alloc_element<ModifiedCopyBitfieldElement>(m_type, m_base, m_got_pcpyud,
                                                           std::vector<BitFieldDef>{def});
  }

  if (m_steps.size() == 1 && m_steps.at(0).kind == BitfieldManip::Kind::LOGAND_WITH_CONSTANT_INT &&
      step.kind == BitfieldManip::Kind::LOGIOR_WITH_FORM) {
    // this is setting a bitfield to a variable
    u64 mask = m_steps.at(0).amount;

    auto type = ts.lookup_type(m_type);
    auto as_bitfield = dynamic_cast<BitFieldType*>(type);
    ASSERT(as_bitfield);
    // use the mask to figure out the field.
    auto field = find_field_from_mask(ts, as_bitfield, ~mask, m_got_pcpyud);
    ASSERT(field);

    auto val = get_bitfield_initial_set(step.value, as_bitfield, ts, pcpyud_offset);

    if (!val) {
      throw std::runtime_error(
          fmt::format("Failed to get_bitfield_initial_set: {}\n", step.value->to_string(env)));
    }

    if (val->field_name != field->name()) {
      throw std::runtime_error("Incompatible bitfield set");
    }

    return pool.alloc_element<ModifiedCopyBitfieldElement>(m_type, m_base, m_got_pcpyud,
                                                           std::vector<BitFieldDef>{*val});
  }

  lg::error("Invalid state in BitfieldReadElement. Previous steps:");
  for (auto& old_step : m_steps) {
    lg::error("  {}", old_step.print());
  }
  lg::error("Current: {}", step.print());
  if (m_got_pcpyud) {
    lg::error("Got pcpyud");
  }

  throw std::runtime_error("Unknown state in BitfieldReadElement");
}

namespace {
/*!
 * Nested on the left, will reverse the order.
 */
std::vector<Form*> compact_nested_logiors(GenericElement* input, const Env&) {
  std::vector<Form*> result;
  GenericElement* next = input;

  while (next) {
    ASSERT(next->elts().size() == 2);
    result.push_back(next->elts().at(1));
    auto next_next = strip_int_or_uint_cast(next->elts().at(0));
    next = next_next->try_as_element<GenericElement>();
    if (!next || !next->op().is_fixed(FixedOperatorKind::LOGIOR)) {
      result.push_back(next_next);
      break;
    }
  }

  return result;
}

}  // namespace

/*!
 * If this could be an integer constant, figure out what the value is.
 */
std::optional<u64> get_goal_integer_constant(Form* in, const Env&) {
  auto as_atom = form_as_atom(in);
  if (as_atom && as_atom->is_int()) {
    return as_atom->get_int();
  }

  // also (shl <something> 32)
  auto matcher = Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::SHL),
                             {Matcher::any(1), Matcher::integer(32)});
  auto mr = match(matcher, in);
  if (mr.matched) {
    auto arg_as_atom = form_as_atom(mr.maps.forms.at(1));
    if (arg_as_atom && arg_as_atom->is_int()) {
      u64 result = arg_as_atom->get_int();
      result <<= 32ull;
      return result;
    }
  }

  // also (shl <something> 16)
  matcher = Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::SHL),
                        {Matcher::any(1), Matcher::integer(16)});
  mr = match(matcher, in);
  if (mr.matched) {
    auto arg_as_atom = form_as_atom(mr.maps.forms.at(1));
    if (arg_as_atom && arg_as_atom->is_int()) {
      u64 result = arg_as_atom->get_int();
      result <<= 16ull;
      return result;
    }
  }

  // also (shl (shl <something> 16) 32)
  // why.
  matcher = Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::SHL),
                        {Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::SHL),
                                     {Matcher::any(1), Matcher::integer(16)}),
                         Matcher::integer(32)});
  mr = match(matcher, in);
  if (mr.matched) {
    auto arg_as_atom = form_as_atom(mr.maps.forms.at(1));
    if (arg_as_atom && arg_as_atom->is_int()) {
      u64 result = arg_as_atom->get_int();
      result <<= 48ull;
      return result;
    }
  }
  return {};
}

BitFieldDef BitFieldDef::from_constant(const BitFieldConstantDef& constant, FormPool& pool) {
  BitFieldDef bfd;
  bfd.field_name = constant.field_name;
  bfd.is_signed = constant.is_signed;
  bfd.is_float = constant.is_float;
  if (constant.nested_field) {
    std::vector<BitFieldDef> defs;
    for (auto& x : constant.nested_field->fields) {
      defs.push_back(BitFieldDef::from_constant(x, pool));
    }
    bfd.value = pool.form<BitfieldStaticDefElement>(constant.nested_field->field_type, defs);
  } else if (constant.enum_constant) {
    bfd.value = pool.form<ConstantTokenElement>(*constant.enum_constant);
  } else {
    bfd.value = pool.form<SimpleAtomElement>(SimpleAtom::make_int_constant(constant.value));
  }

  return bfd;
}

namespace {
Form* cast_sound_name(FormPool& pool, const Env& env, Form* in) {
  auto matcher = Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::PCPYLD),
                             {Matcher::any(1), Matcher::any(0)});
  auto mr = match(matcher, in);
  if (!mr.matched) {
    return nullptr;
  }

  auto hi = mr.maps.forms.at(1);
  auto lo = mr.maps.forms.at(0);

  auto hi_int = get_goal_integer_constant(strip_int_or_uint_cast(hi), env);
  auto lo_int = get_goal_integer_constant(strip_int_or_uint_cast(lo), env);
  if (!hi_int || !lo_int) {
    return nullptr;
  }

  char name[17];
  memcpy(name, &lo_int.value(), 8);
  memcpy(name + 8, &hi_int.value(), 8);
  name[16] = '\0';

  bool got_zero = false;
  for (int i = 0; i < 16; i++) {
    if (name[i] == 0) {
      got_zero = true;
    } else {
      if (got_zero) {
        return nullptr;
      }
    }
  }

  return pool.form<ConstantTokenElement>(fmt::format("(static-sound-name \"{}\")", name));
}

std::optional<std::vector<BitFieldDef>> get_field_defs_from_expr(const BitFieldType* type_info,
                                                                 Form* in,
                                                                 const TypeSpec& typespec,
                                                                 FormPool& pool,
                                                                 const Env& env,
                                                                 std::optional<int> offset) {
  auto in_as_generic = strip_int_or_uint_cast(in)->try_as_element<GenericElement>();
  std::vector<Form*> args;
  if (in_as_generic && in_as_generic->op().is_fixed(FixedOperatorKind::LOGIOR)) {
    args = compact_nested_logiors(in_as_generic, env);
  } else {
    args = {strip_int_or_uint_cast(in)};
  }

  if (!args.empty()) {
    std::vector<BitFieldDef> field_defs;

    for (auto it = args.begin(); it != args.end(); it++) {
      auto constant = get_goal_integer_constant(*it, env);
      if (constant) {
        auto constant_defs =
            try_decompile_bitfield_from_int(typespec, env.dts->ts, *constant, false, offset);
        if (!constant_defs) {
          return std::nullopt;  // failed
        }
        for (auto& x : *constant_defs) {
          field_defs.push_back(BitFieldDef::from_constant(x, pool));
        }

        args.erase(it);
        break;
      }
    }

    // now variables
    for (auto& arg : args) {
      // it's a 64-bit constant so correct to set offset 0 here.
      auto maybe_field = get_bitfield_initial_set(strip_int_or_uint_cast(arg), type_info,
                                                  env.dts->ts, offset ? *offset : 0);
      if (!maybe_field) {
        // failed, just return cast.
        return std::nullopt;
      }

      BitField field_info;
      if (!type_info->lookup_field(maybe_field->field_name, &field_info)) {
        ASSERT(false);
      }
      if (field_info.type() == TypeSpec("symbol") || field_info.type() == TypeSpec("type")) {
        maybe_field->value = strip_int_or_uint_cast(maybe_field->value);
      }

      if (field_info.type() == TypeSpec("float")) {
        auto stripped = strip_int_or_uint_cast(maybe_field->value);
        auto integer = get_goal_integer_constant(stripped, env);
        if (integer) {
          float value_f;
          u32 value_i = *integer;
          memcpy(&value_f, &value_i, 4);
          maybe_field->value = pool.form<ConstantFloatElement>(value_f);
        }
      }

      auto field_type_as_bitfield =
          dynamic_cast<BitFieldType*>(env.dts->ts.lookup_type(field_info.type()));
      if (field_type_as_bitfield) {
        maybe_field->value = cast_to_bitfield(field_type_as_bitfield, field_info.type(), pool, env,
                                              maybe_field->value);
      }

      field_defs.push_back(*maybe_field);
    }
    return field_defs;
  }
  return std::vector<BitFieldDef>();
}
}  // namespace

/*!
 * Cast the given form to a bitfield.
 * If the form could have been a (new 'static 'bitfieldtype ...) it will attempt to generate this.
 */
Form* cast_to_bitfield(const BitFieldType* type_info,
                       const TypeSpec& typespec,
                       FormPool& pool,
                       const Env& env,
                       Form* in) {
  in = strip_int_or_uint_cast(in);

  // special case for sound-name bitfield to string
  if (type_info->get_name() == "sound-name") {
    auto as_sound_name = cast_sound_name(pool, env, in);
    if (as_sound_name) {
      return as_sound_name;
    }
    // just do a normal cast if that failed.
    return pool.form<CastElement>(typespec, in);
  }

  // check if it's just a constant:
  auto in_as_atom = form_as_atom(in);
  if (in_as_atom && in_as_atom->is_int()) {
    s64 val = in_as_atom->get_int();
    if (type_info->get_name() == "gif-tag-regs" && (u64)val == 0xeeeeeeeeeeeeeeee) {
      return pool.form<ConstantTokenElement>("GIF_REGS_ALL_AD");
    }

    // will always be 64-bits
    auto fields = try_decompile_bitfield_from_int(typespec, env.dts->ts, val, false, {});
    if (!fields) {
      return pool.form<CastElement>(typespec, in);
    }
    return pool.form<BitfieldStaticDefElement>(typespec, *fields, pool);
  }

  bool bitfield_128 = type_info->get_size_in_memory() == 16;
  if (bitfield_128) {
    auto in_no_cast = strip_int_or_uint_cast(in);
    auto pcpyld_matcher =
        Matcher::op_fixed(FixedOperatorKind::PCPYLD, {Matcher::any(0), Matcher::any(1)});
    auto mr = match(pcpyld_matcher, in_no_cast);
    if (mr.matched) {
      auto upper = mr.maps.forms.at(0);
      auto lower = mr.maps.forms.at(1);

      auto upper_defs = get_field_defs_from_expr(type_info, upper, typespec, pool, env, 64);
      auto lower_defs = get_field_defs_from_expr(type_info, lower, typespec, pool, env, 0);

      if (upper_defs && lower_defs) {
        lower_defs->insert(lower_defs->end(), upper_defs->begin(), upper_defs->end());
        return pool.form<BitfieldStaticDefElement>(typespec, *lower_defs);
      }
    }
    return pool.form<CastElement>(typespec, in);
  } else {
    // dynamic bitfield def
    auto field_defs = get_field_defs_from_expr(type_info, in, typespec, pool, env, {});
    if (field_defs) {
      return pool.form<BitfieldStaticDefElement>(typespec, *field_defs);
    }
  }

  // all failed, just return whatever.
  return pool.form<CastElement>(typespec, in);
}

Form* cast_to_bitfield_enum(const EnumType* type_info,
                            const TypeSpec& typespec,
                            FormPool& pool,
                            const Env& env,
                            Form* in) {
  ASSERT(type_info->is_bitfield());
  auto integer = get_goal_integer_constant(strip_int_or_uint_cast(in), env);
  if (integer) {
    return cast_to_bitfield_enum(type_info, pool, env, *integer);
  } else {
    // all failed, just return whatever.
    return pool.form<CastElement>(typespec, in);
  }
}

Form* cast_to_int_enum(const EnumType* type_info,
                       const TypeSpec& typespec,
                       FormPool& pool,
                       const Env& env,
                       Form* in) {
  ASSERT(!type_info->is_bitfield());
  auto integer = get_goal_integer_constant(strip_int_or_uint_cast(in), env);
  if (integer) {
    return cast_to_int_enum(type_info, pool, env, *integer);
  } else {
    // all failed, just return whatever.
    return pool.form<CastElement>(typespec, in);
  }
}

Form* cast_to_int_enum(const EnumType* type_info, FormPool& pool, const Env& env, s64 in) {
  ASSERT(!type_info->is_bitfield());
  auto entry = decompile_int_enum_from_int(TypeSpec(type_info->get_name()), env.dts->ts, in);
  auto oper =
      GenericOperator::make_function(pool.form<ConstantTokenElement>(type_info->get_name()));
  return pool.form<GenericElement>(oper, pool.form<ConstantTokenElement>(entry));
}

Form* cast_to_bitfield_enum(const EnumType* type_info,
                            FormPool& pool,
                            const Env& env,
                            s64 in,
                            bool no_head) {
  ASSERT(type_info->is_bitfield());
  if (in == -1) {
    return nullptr;
  }
  auto elts = decompile_bitfield_enum_from_int(TypeSpec(type_info->get_name()), env.dts->ts, in);
  if (no_head) {
    ASSERT(elts.size() >= 1);
  }
  auto oper = GenericOperator::make_function(
      pool.form<ConstantTokenElement>(no_head ? elts.at(0) : type_info->get_name()));
  if (no_head) {
    elts.erase(elts.begin());
  }

  std::vector<Form*> form_elts;
  for (auto& x : elts) {
    form_elts.push_back(pool.form<ConstantTokenElement>(x));
  }
  return pool.form<GenericElement>(oper, form_elts);
}

}  // namespace decompiler
