/*!
 * @file GenericElementMatcher.h
 *
 * The Matcher is supposed to match up forms to templates, and extract the variables actually used.
 */

#pragma once
#include "Env.h"
#include "Form.h"

namespace decompiler {
class DerefTokenMatcher;
class GenericOpMatcher;
class LetEntryMatcher;

struct MatchResult {
  bool matched = false;
  struct Maps {
    std::vector<std::optional<RegisterAccess>> regs;
    std::unordered_map<int, std::string> strings;
    std::unordered_map<int, Form*> forms;
    std::unordered_map<int, s64> label;
    std::unordered_map<int, s64> ints;
    std::unordered_map<int, float> floats;
  } maps;

  Form* int_or_form_to_form(FormPool& pool, int key_idx) {
    if (maps.forms.find(key_idx) != maps.forms.end()) {
      return maps.forms.at(key_idx);
    } else {
      return pool.form<SimpleAtomElement>(SimpleAtom::make_int_constant(maps.ints.at(key_idx)));
    }
  }
};

class Matcher {
 public:
  static Matcher any_reg(int match_id = -1);
  static Matcher same_var(int match_id);
  static Matcher var_name(const std::string& name);
  static Matcher any_label(int match_id = -1);
  static Matcher reg(Register reg);
  static Matcher s6();
  static Matcher op(const GenericOpMatcher& op, const std::vector<Matcher>& args);
  static Matcher func(const Matcher& match, const std::vector<Matcher>& args);
  static Matcher func(const std::string& name, const std::vector<Matcher>& args);
  static Matcher op_fixed(FixedOperatorKind op, const std::vector<Matcher>& args);
  static Matcher op_with_rest(const GenericOpMatcher& op, const std::vector<Matcher>& args);
  static Matcher func_with_rest(const Matcher& match, const std::vector<Matcher>& args);
  static Matcher func_with_rest(const std::string& name, const std::vector<Matcher>& args);
  static Matcher set(const Matcher& dst, const Matcher& src);    // form-form
  static Matcher set_var(const Matcher& src, int dst_match_id);  // var-form
  static Matcher match_or(const std::vector<Matcher>& args);
  static Matcher cast(const std::string& type, Matcher value);
  static Matcher cast_to_any(int type_out, Matcher value);
  static Matcher any(int match_id = -1);
  static Matcher integer(std::optional<int> value);
  static Matcher any_integer(int match_id = -1);
  static Matcher single(std::optional<float> value);
  static Matcher any_single(int match_id = -1);
  static Matcher any_reg_cast_to_int_or_uint(int match_id = -1);
  static Matcher any_quoted_symbol(int match_id = -1);
  static Matcher any_symbol(int match_id = -1);
  static Matcher quoted_symbol(const std::string& name);
  static Matcher symbol(const std::string& name);
  static Matcher deref(const Matcher& root,
                       bool is_addr_of,
                       const std::vector<DerefTokenMatcher>& tokens);
  static Matcher if_with_else(const Matcher& condition,
                              const Matcher& true_case,
                              const Matcher& false_case);
  static Matcher if_no_else(const Matcher& condition, const Matcher& true_case);
  static Matcher while_loop(const Matcher& condition, const Matcher& body);
  static Matcher until_loop(const Matcher& condition, const Matcher& body);
  static Matcher any_constant_token(int match_id = -1);
  static Matcher constant_token(const std::string& name);
  static Matcher or_expression(const std::vector<Matcher>& elts);
  static Matcher begin(const std::vector<Matcher>& elts);
  static Matcher let(bool is_star,
                     const std::vector<LetEntryMatcher>& entries,
                     const std::vector<Matcher>& elts);
  static Matcher unmerged_let(const std::vector<LetEntryMatcher>& entries,
                              const std::vector<Matcher>& elts);

  enum class Kind {
    ANY_REG,     // matching any register
    GENERIC_OP,  // matching
    GENERIC_OP_WITH_REST,
    OR,
    CAST,
    CAST_TO_ANY,
    ANY,
    INT,
    ANY_INT,
    FLOAT,
    ANY_FLOAT,
    ANY_QUOTED_SYMBOL,
    ANY_SYMBOL,
    DEREF_OP,
    SET,
    SET_VAR,
    ANY_LABEL,
    SYMBOL,
    IF_WITH_ELSE,
    IF_NO_ELSE,
    WHILE_LOOP,
    UNTIL_LOOP,
    ANY_CONSTANT_TOKEN,
    CONSTANT_TOKEN,
    SC_OR,
    BEGIN,
    REG,  // a specific register. like s6.
    QUOTED_SYMBOL,
    SAME_VAR,
    LET,
    UNMERGED_LET,
    VAR_NAME,
    INVALID
  };

  bool do_match(Form* input, MatchResult::Maps* maps_out, const Env* const env) const;

 private:
  std::vector<Matcher> m_sub_matchers;
  std::vector<DerefTokenMatcher> m_token_matchers;
  std::vector<LetEntryMatcher> m_entry_matchers;
  std::shared_ptr<GenericOpMatcher> m_gen_op_matcher;
  bool m_deref_is_addr_of = false;
  bool m_let_is_star = false;
  Kind m_kind = Kind::INVALID;
  union {
    int m_out_id = -1;
    int m_reg_out_id;
    int m_string_out_id;
    int m_form_match;
    int m_label_out_id;
    int m_int_out_id;
    int m_float_out_id;
  };
  std::optional<int> m_int_match;
  std::optional<float> m_float_match;
  std::optional<Register> m_reg;
  std::string m_str;
};

MatchResult match(const Matcher& spec, Form* input, const Env* const env = nullptr);
MatchResult match(const Matcher& spec, FormElement* input, const Env* const env = nullptr);

class DerefTokenMatcher {
 public:
  static DerefTokenMatcher string(const std::string& str);
  static DerefTokenMatcher integer(int value);
  static DerefTokenMatcher any_string(int match_id = -1);
  static DerefTokenMatcher any_integer(int match_id = -1);
  static DerefTokenMatcher any_expr(int match_id = -1);
  static DerefTokenMatcher any_expr_or_int(int match_id = -1);

  enum class Kind {
    STRING,
    ANY_STRING,
    CONSTANT_INTEGER,
    ANY_INTEGER,
    ANY_EXPR,
    ANY_EXPR_OR_INT,
    INVALID
  };

  bool do_match(DerefToken& input, MatchResult::Maps* maps_out) const;

 private:
  Kind m_kind = Kind::INVALID;
  std::string m_str;
  int m_int = -1;
  int m_str_out_id = -1;
};

class GenericOpMatcher {
 public:
  static GenericOpMatcher fixed(FixedOperatorKind kind);
  static GenericOpMatcher func(const Matcher& func_matcher);
  static GenericOpMatcher condition(IR2_Condition::Kind condition);
  static GenericOpMatcher or_match(const std::vector<GenericOpMatcher>& matchers);

  enum class Kind { FIXED, FUNC, CONDITION, OR, INVALID };

  bool do_match(GenericOperator& input, MatchResult::Maps* maps_out, const Env* const env) const;

 private:
  Kind m_kind = Kind::INVALID;
  FixedOperatorKind m_fixed_kind = FixedOperatorKind::INVALID;
  IR2_Condition::Kind m_condition_kind = IR2_Condition::Kind::INVALID;
  std::vector<GenericOpMatcher> m_sub_matchers;
  Matcher m_func_matcher;
};

class LetEntryMatcher {
 public:
  static LetEntryMatcher name(std::optional<Matcher> src_matcher, const std::string& name);
  static LetEntryMatcher any(std::optional<Matcher> src_matcher, int match_id = -1);

  enum class Kind { ANY, NAME, INVALID };

  bool do_match(const LetElement::Entry& input,
                MatchResult::Maps* maps_out,
                const Env* const env) const;

 private:
  Kind m_kind = Kind::INVALID;
  int m_reg_out_id = -1;
  std::optional<Matcher> m_src_matcher;
  std::string m_reg_name;
};

}  // namespace decompiler
