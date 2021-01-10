#include "third-party/fmt/core.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "common/log/log.h"
#include "AtomicOp.h"

namespace decompiler {
std::string AtomicOp::reg_type_info_as_string(const TypeState& init_types,
                                              const TypeState& end_types) const {
  std::string result;

  auto read_mask = regs_to_gpr_mask(m_read_regs);
  auto write_mask = regs_to_gpr_mask(m_write_regs);
  auto clobber_mask = regs_to_gpr_mask(m_clobber_regs);

  result += fmt::format("[{}] -> [{}]", init_types.print_gpr_masked(read_mask),
                        end_types.print_gpr_masked(write_mask));

  if (clobber_mask) {
    result += "cl: ";
    for (auto& reg : m_clobber_regs) {
      result += reg.to_string();
      result += ' ';
    }
  }

  return result;
}

TP_Type SimpleAtom::get_type(const TypeState& input,
                             const Env& env,
                             const DecompilerTypeSystem& dts) const {
  switch (m_kind) {
    case Kind::EMPTY_LIST:
      return TP_Type::make_from_ts("pair");
    case Kind::VARIABLE:
      return input.get(var().reg());
    case Kind::INTEGER_CONSTANT:
      return TP_Type::make_from_integer(m_int);
    case Kind::SYMBOL_PTR:
      if (m_string == "#f") {
        return TP_Type::make_false();
      } else {
        return TP_Type::make_from_ts("symbol");
      }
    case Kind::SYMBOL_VAL: {
      if (m_string == "#f") {
        // if we ever read the false symbol, it should contain the false symbol as its value.
        return TP_Type::make_false();
      } else if (m_string == "__START-OF-TABLE__") {
        // another annoying special case. We have a fake symbol called __START-OF-TABLE__
        // which actually means that you get the first address in the symbol table.
        // it's not really a linked symbol, but the basic op builder represents it as one.
        return TP_Type::make_from_ts(TypeSpec("pointer"));
      }

      // look up the type of the symbol
      auto type = dts.symbol_types.find(m_string);
      if (type == dts.symbol_types.end()) {
        throw std::runtime_error("Don't have the type of symbol " + m_string);
      }

      if (type->second == TypeSpec("type")) {
        // if we get a type by symbol, we should remember which type we got it from.
        return TP_Type::make_type_object(TypeSpec(m_string));
      }

      // otherwise, just return a normal typespec
      return TP_Type::make_from_ts(type->second);
    }
    case Kind::STATIC_ADDRESS: {
      auto label = env.file->labels.at(m_int);
      // strings are 16-byte aligned, but functions are 8 byte aligned?
      if ((label.offset & 7) == BASIC_OFFSET) {
        // it's a basic! probably.
        const auto& word =
            env.file->words_by_seg.at(label.target_segment).at((label.offset - 4) / 4);
        if (word.kind == LinkedWord::TYPE_PTR) {
          if (word.symbol_name == "string") {
            return TP_Type::make_from_string(env.file->get_goal_string_by_label(label));
          } else {
            // otherwise, some other static basic.
            return TP_Type::make_from_ts(TypeSpec(word.symbol_name));
          }
        }
      } else if ((label.offset & 7) == PAIR_OFFSET) {
        return TP_Type::make_from_ts(TypeSpec("pair"));
      }
      throw std::runtime_error("IR_StaticAddress couldn't figure out the type: " + label.name);
    }
    case Kind::INVALID:
    default:
      assert(false);
  }
  return {};
}

TP_Type SimpleExpression::get_type(const TypeState& input,
                                   const Env& env,
                                   const DecompilerTypeSystem& dts) const {
  assert(false);
  return {};
}

TypeState IR2_BranchDelay::propagate_types(const TypeState& input,
                                           const Env& env,
                                           DecompilerTypeSystem& dts) const {
  assert(false);
  return {};
}

/////////////////////////////////////////
// Implementations of propagate_types_internal
/////////////////////////////////////////

TypeState AtomicOp::propagate_types(const TypeState& input,
                                    const Env& env,
                                    DecompilerTypeSystem& dts) const {
  // do op-specific type propagation
  TypeState result = propagate_types_internal(input, env, dts);
  // clobber
  for (auto reg : m_clobber_regs) {
    result.get(reg) = TP_Type::make_uninitialized();
  }
  return result;
}

TypeState SetVarOp::propagate_types_internal(const TypeState& input,
                                             const Env& env,
                                             DecompilerTypeSystem& dts) const {
  TypeState result = input;
  result.get(m_dst.reg()) = m_src.get_type(input, env, dts);
  return result;
}

TypeState AsmOp::propagate_types_internal(const TypeState& input,
                                          const Env& env,
                                          DecompilerTypeSystem& dts) const {
  TypeState result = input;
  if (m_dst.has_value()) {
    result.get(m_dst->reg()) = TP_Type::make_from_ts("int");
  }
  return result;
}

TypeState SetVarConditionOp::propagate_types_internal(const TypeState& input,
                                                      const Env& env,
                                                      DecompilerTypeSystem& dts) const {
  TypeState result = input;
  result.get(m_dst.reg()) = TP_Type::make_from_ts("symbol");
  return result;
}

TypeState StoreOp::propagate_types_internal(const TypeState& input,
                                            const Env& env,
                                            DecompilerTypeSystem& dts) const {
  (void)env;
  (void)dts;
  return input;
}

TypeState LoadVarOp::propagate_types_internal(const TypeState& input,
                                              const Env& env,
                                              DecompilerTypeSystem& dts) const {
  assert(false);
  return {};
}

TypeState BranchOp::propagate_types_internal(const TypeState& input,
                                             const Env& env,
                                             DecompilerTypeSystem& dts) const {
  return m_branch_delay.propagate_types(input, env, dts);
}

TypeState SpecialOp::propagate_types_internal(const TypeState& input,
                                              const Env& env,
                                              DecompilerTypeSystem& dts) const {
  (void)env;
  (void)dts;
  // none of these write anything. Suspend clobbers, but this is taken care of automatically
  switch (m_kind) {
    case Kind::NOP:
    case Kind::BREAK:
    case Kind::CRASH:
    case Kind::SUSPEND:
      return input;
    default:
      assert(false);
  }
}

TypeState CallOp::propagate_types_internal(const TypeState& input,
                                           const Env& env,
                                           DecompilerTypeSystem& dts) const {
  assert(false);
  return {};
}

TypeState ConditionalMoveFalseOp::propagate_types_internal(const TypeState& input,
                                                           const Env& env,
                                                           DecompilerTypeSystem& dts) const {
  (void)env;
  (void)dts;
  // these should only appear when paired with a (set! dest #t) earlier, so this expression
  // shouldn't set any types.  Still, double check and override if this fails.
  TypeState result = input;
  if (result.get(m_dst.reg()).typespec() != TypeSpec("symbol")) {
    lg::warn("Conditional Moved #f into something of type {}",
             result.get(m_dst.reg()).typespec().print());
    result.get(m_dst.reg()) = TP_Type::make_from_ts("symbol");
  }

  return result;
}

}  // namespace decompiler