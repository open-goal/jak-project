#pragma once

#include "common/common_types.h"
#include "common/util/Assert.h"

#include "decompiler/IR2/Form.h"
#include "decompiler/util/data_decompile.h"

namespace decompiler {
struct BitfieldManip {
  enum class Kind {
    LEFT_SHIFT,
    RIGHT_SHIFT_LOGICAL,
    RIGHT_SHIFT_LOGICAL_32BIT,
    RIGHT_SHIFT_ARITH,
    LOGAND_WITH_CONSTANT_INT,
    LOGIOR_WITH_CONSTANT_INT,
    LOGIOR_WITH_FORM,
    LOGAND_WITH_FORM,
    NONZERO_COMPARE,
    SLLV_SEXT,  // sllv x, y, r0
    PEXTUW,
    INVALID
  } kind = Kind::INVALID;
  s64 amount = -1;
  Form* value = nullptr;

  bool is_right_shift() const {
    return kind == Kind::RIGHT_SHIFT_ARITH || kind == Kind::RIGHT_SHIFT_LOGICAL ||
           kind == Kind::RIGHT_SHIFT_LOGICAL_32BIT;
  }

  bool right_shift_unsigned() const {
    ASSERT(is_right_shift());
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
  static BitfieldManip from_form(Kind k, Form* val) {
    BitfieldManip result;
    result.kind = k;
    result.value = val;
    return result;
  }

  std::string print() const {
    switch (kind) {
      case Kind::LEFT_SHIFT:
        return "left-shift";
      case Kind::RIGHT_SHIFT_LOGICAL:
        return "right-shift-logical";
      case Kind::RIGHT_SHIFT_LOGICAL_32BIT:
        return "right-shift-logical32";
      case Kind::LOGAND_WITH_CONSTANT_INT:
        return "logand-constant";
      case Kind::LOGIOR_WITH_CONSTANT_INT:
        return "logior-constant";
      case Kind::LOGIOR_WITH_FORM:
        return "logior-form";
      case Kind::LOGAND_WITH_FORM:
        return "logand-form";
      case Kind::NONZERO_COMPARE:
        return "nonzero-compare";
      case Kind::SLLV_SEXT:
        return "sllv-sext";
      case Kind::PEXTUW:
        return "pextuw";
      case Kind::INVALID:
      default:
        ASSERT(false);
    }
  }

 private:
  BitfieldManip() = default;
};

class BitfieldAccessElement : public FormElement {
 public:
  BitfieldAccessElement(Form* base_value, const TypeSpec& ts);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;
  FormElement* push_step(const BitfieldManip step,
                         const TypeSystem& ts,
                         FormPool& pool,
                         const Env& env);
  void push_pcpyud(const TypeSystem& ts, FormPool& pool, const Env& env);
  std::string debug_print(const Env& env) const;
  bool has_pcpyud() const { return m_got_pcpyud; }
  const std::vector<BitfieldManip>& steps() const { return m_steps; }
  std::optional<BitField> get_set_field_0(const TypeSystem& ts) const;

 private:
  bool m_got_pcpyud = false;
  Form* m_base = nullptr;

  // if we aren't sure if we are done or not, store the result here.
  Form* m_current_result = nullptr;

  TypeSpec m_type;
  std::vector<BitfieldManip> m_steps;
};

struct BitFieldDef {
  bool is_signed = false;
  bool is_float = false;
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

  const TypeSpec& bitfield_type() const { return m_type; }
  const std::vector<BitFieldDef>& defs() const { return m_field_defs; }

 private:
  TypeSpec m_type;
  std::vector<BitFieldDef> m_field_defs;
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
                              bool from_pcpyud,
                              const std::vector<BitFieldDef>& field_modifications);
  goos::Object to_form_internal(const Env& env) const override;
  void apply(const std::function<void(FormElement*)>& f) override;
  void apply_form(const std::function<void(Form*)>& f) override;
  void collect_vars(RegAccessSet& vars, bool recursive) const override;
  void get_modified_regs(RegSet& regs) const override;

  Form* base() const { return m_base; }
  const std::vector<BitFieldDef> mods() const { return m_field_modifications; }
  bool from_pcpyud() const { return m_from_pcpyud; }
  void clear_pcpyud_flag() {
    ASSERT(m_from_pcpyud);
    m_from_pcpyud = false;
  }

 private:
  TypeSpec m_type;
  Form* m_base = nullptr;
  std::vector<BitFieldDef> m_field_modifications;
  bool m_from_pcpyud = false;
};

Form* cast_to_bitfield(const BitFieldType* type_info,
                       const TypeSpec& typespec,
                       FormPool& pool,
                       const Env& env,
                       Form* in);

Form* cast_to_bitfield_enum(const EnumType* type_info,
                            const TypeSpec& typespec,
                            FormPool& pool,
                            const Env& env,
                            Form* in);

Form* cast_to_int_enum(const EnumType* type_info,
                       const TypeSpec& typespec,
                       FormPool& pool,
                       const Env& env,
                       Form* in);

Form* cast_to_int_enum(const EnumType* type_info, FormPool& pool, const Env& env, s64 in);
Form* cast_to_bitfield_enum(const EnumType* type_info,
                            FormPool& pool,
                            const Env& env,
                            s64 in,
                            bool no_head = false);

std::optional<u64> get_goal_integer_constant(Form* in, const Env&);

BitField find_field(const TypeSystem& ts,
                    const BitFieldType* type,
                    int start_bit,
                    int size,
                    std::optional<bool> looking_for_unsigned);
}  // namespace decompiler
