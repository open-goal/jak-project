#pragma once

#include <cassert>

#include "common/common_types.h"
#include "decompiler/IR2/Form.h"
#include "decompiler/util/data_decompile.h"

namespace decompiler {
struct BitfieldManip {
  enum class Kind {
    LEFT_SHIFT,
    RIGHT_SHIFT_LOGICAL,
    RIGHT_SHIFT_LOGICAL_32BIT,
    RIGHT_SHIFT_ARITH,
    LOGAND,
    LOGIOR_WITH_CONSTANT_INT,
    NONZERO_COMPARE,
    INVALID
  } kind = Kind::INVALID;
  s64 amount = -1;

  bool is_right_shift() const {
    return kind == Kind::RIGHT_SHIFT_ARITH || kind == Kind::RIGHT_SHIFT_LOGICAL ||
           kind == Kind::RIGHT_SHIFT_LOGICAL_32BIT;
  }

  bool right_shift_unsigned() const {
    assert(is_right_shift());
    return kind == Kind::RIGHT_SHIFT_LOGICAL || kind == Kind::RIGHT_SHIFT_LOGICAL_32BIT;
  }

  bool is_64bit_shift() const {
    return kind == Kind::RIGHT_SHIFT_LOGICAL || kind == Kind::RIGHT_SHIFT_ARITH ||
           kind == Kind::LEFT_SHIFT;
  }

  int get_shift_start_bit() const {
    if (is_64bit_shift()) {
      return 64;
    } else {
      return 32;
    }
  }

  BitfieldManip(Kind k, s64 imm) : kind(k), amount(imm) {}
};

class BitfieldReadElement : public FormElement {
 public:
  BitfieldReadElement(Form* base_value, const TypeSpec& ts);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  FormElement* push_step(const BitfieldManip step, const TypeSystem& ts, FormPool& pool);

 private:
  Form* m_base = nullptr;
  TypeSpec m_type;
  std::vector<BitfieldManip> m_steps;
};

struct BitFieldDef {
  bool is_signed = false;
  Form* value = nullptr;
  std::string field_name;

  static BitFieldDef from_constant(const BitFieldConstantDef& constant, FormPool& pool);
};

class BitfieldStaticDefElement : public FormElement {
 public:
  BitfieldStaticDefElement(const TypeSpec& type, const std::vector<BitFieldDef>& field_defs);
  BitfieldStaticDefElement(const TypeSpec& type,
                           const std::vector<BitFieldConstantDef>& field_defs,
                           FormPool& pool);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  void update_from_stack(const Env&,
                         FormPool&,
                         FormStack&,
                         std::vector<FormElement*>* result,
                         bool) override {
    mark_popped();
    result->push_back(this);
  }

 private:
  TypeSpec m_type;
  std::vector<BitFieldDef> m_field_defs;
};

struct BitfieldFormDef {
  Form* value;
  std::string field_name;
};

/*!
 * This represents copying a bitfield object, then modifying the type.
 * It's an intermediate step to modifying a bitfield in place and it's not expected to appear
 * in the final output.
 */
class ModifiedCopyBitfieldElement : public FormElement {
 public:
  ModifiedCopyBitfieldElement(const TypeSpec& type,
                              Form* base,
                              const std::vector<BitfieldFormDef>& field_modifications);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;

  Form* base() const { return m_base; }
  const std::vector<BitfieldFormDef> mods() const { return m_field_modifications; }

 private:
  TypeSpec m_type;
  Form* m_base = nullptr;
  std::vector<BitfieldFormDef> m_field_modifications;
};

Form* cast_to_bitfield(const BitFieldType* type_info,
                       const TypeSpec& typespec,
                       FormPool& pool,
                       const Env& env,
                       Form* in);

}  // namespace decompiler
