/*!
 * @file GenericElementMatcher.h
 *
 * The Matcher is supposed to match up forms to templates, and extract the variables actually used.
 */

#pragma once
#include "Form.h"

namespace decompiler {

struct MatchResult {
  bool matched = false;
  struct Maps {
    std::vector<std::optional<Variable>> regs;
  } maps;
};

class Matcher {
 public:
  static Matcher any_reg(int match_id = -1);
  static Matcher op(GenericOperator op, const std::vector<Matcher>& args);
  static Matcher fixed_op(FixedOperatorKind op, const std::vector<Matcher>& args);
  static Matcher match_or(const std::vector<Matcher>& args);
  static Matcher cast(const std::string& type, Matcher value);
  static Matcher any();
  static Matcher integer(std::optional<int> value);
  static Matcher any_reg_cast_to_int_or_uint(int match_id = -1);

  enum class Kind {
    ANY_REG,     // matching any register
    GENERIC_OP,  // matching
    OR,
    CAST,
    ANY,
    INT,
    INVALID
  };

  bool do_match(const Form* input, MatchResult::Maps* maps_out) const;

 private:
  GenericOperator m_gen_op;
  std::vector<Matcher> m_sub_matchers;
  Kind m_kind = Kind::INVALID;
  int m_reg_out_id = -1;
  std::optional<int> m_int_match;
  std::string m_str;
};

MatchResult match(const Matcher& spec, const Form* input);

}  // namespace decompiler