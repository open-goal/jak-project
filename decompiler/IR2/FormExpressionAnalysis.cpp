#include <unordered_map>
#include <unordered_set>

#include "Form.h"
#include "FormStack.h"
#include "GenericElementMatcher.h"

#include "common/goos/PrettyPrinter.h"
#include "common/log/log.h"
#include "common/type_system/state.h"
#include "common/util/Assert.h"
#include "common/util/BitUtils.h"
#include "common/util/print_float.h"

#include "decompiler/IR2/ExpressionHelpers.h"
#include "decompiler/IR2/bitfields.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "decompiler/util/data_decompile.h"
#include "decompiler/util/type_utils.h"

/*
 * TODO
 * - use var_to_form over expressions for vars
 * - check out if we can push/pop variables instead of registers?
 */

/*!
 * Basic idea: push partial expressions to the stack and pop them off as they are used.
 * Leftovers are left on the stack and flushed out as set!
 *  But the challenge is knowing it's safe to pop something off of the stack.
 *  If the value is used after the read again, then it's not.
 *
 * The tricky situation is to accidentally generate this
 *  [simplified, we would never group like this, but similar things are possible]
 * (+ s5 (begin (set! s5 z) (+ x y)))
 * when the value of s5 used is after the (set! s5 z).
 *
 * To avoid that case, we make sure that anything after s5 in the expression cannot modify s5. If it
 * does, we just don't do the expression building and leave it as smaller expressions. To accomplish
 * this, we submit a batch of registers to pop, and the stack takes care of making sure this
 * property will hold.
 *
 * But what about
 * (+ (* s5 s4) (begin (set! s5 z) (+ x y)))
 *  Luckily this isn't a problem. The actual (* s5 s4) will only be inserted if the multiply
 *  instruction actually occurs before the second term in the outer addition.
 *  The issue only occurs when a pop fails and we just insert a variable name.
 *  In other words, this variable "insert" is the only that lets us bypass ordering.
 *    (which makes me wonder if I should have solved this by being more careful with that...
 *     but I have no immediate ideas unless we allow backtracking in popping which is bad)
 *
 * Now sometimes it's too hard to figure out exactly all of the variables we might pop and do
 * it all in one batch. We pop one thing, but we know that there are registers it shouldn't modify.
 * Instead of the barrier register approach, we do something easier: explicitly forbid
 * "outside of expression register variable side effects".
 * This means that you modify a register that's visible outside of the expression. Confusingly,
 * memory access doesn't count as a side effect and register read/writes that are part of a
 * chained together expression do not count.
 * This is kind of a hacky workaround that I'd really like to remove eventually.
 * I think it can basically be solved by being strategic in when we update from stack and we can
 * avoid the highly general "update some unknown vector of unknown things from the stack"
 */

namespace decompiler {

namespace {
Form* strip_pcypld_64(Form* in) {
  auto m = match(Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::PCPYLD),
                             {Matcher::integer(0), Matcher::any(0)}),
                 in);
  if (m.matched) {
    return m.maps.forms.at(0);
  } else {
    return in;
  }
}

std::optional<float> get_goal_float_constant(FormElement* in) {
  auto as_fc = dynamic_cast<ConstantFloatElement*>(in);
  if (as_fc) {
    return as_fc->value();
  }
  return {};
}

std::optional<float> get_goal_float_constant(Form* in) {
  auto elt = in->try_as_single_element();
  if (elt) {
    return get_goal_float_constant(elt);
  } else {
    return {};
  }
}

bool cond_has_only_single_elements(CondWithElseElement* in) {
  for (auto& entry : in->entries) {
    if (entry.body->elts().size() > 1) {
      return false;
    }
  }
  if (in->else_ir->elts().size() > 1) {
    return false;
  }
  return true;
}
}  // namespace

Form* try_cast_simplify(Form* in,
                        const TypeSpec& new_type,
                        FormPool& pool,
                        const Env& env,
                        bool tc_pass) {
  auto in_as_cast = dynamic_cast<CastElement*>(in->try_as_single_element());
  if (in_as_cast && in_as_cast->type() == new_type) {
    return in;  // no need to cast again, it already has it!
  }

  auto in_as_reslump = in->try_as_element<ResLumpMacroElement>();
  if (in_as_reslump) {
    in_as_reslump->apply_cast(new_type);
    return in;
  }

  if (env.version >= GameVersion::Jak2) {
    if (new_type == TypeSpec("float")) {
      auto ic = get_goal_integer_constant(in, env);
      if (ic) {
        // ASSERT(*ic <= UINT32_MAX);
        ASSERT((s64)*ic == (s64)(s32)*ic);
        float f;
        memcpy(&f, &ic.value(), sizeof(float));
        return pool.form<ConstantFloatElement>(f);
      }
    }
  }

  if (new_type == TypeSpec("meters")) {
    auto fc = get_goal_float_constant(in);

    if (!fc && env.version >= GameVersion::Jak2) {
      auto ic = get_goal_integer_constant(in, env);
      if (ic) {
        ASSERT((s64)*ic == (s64)(s32)*ic);
        float f;
        memcpy(&f, &ic.value(), sizeof(float));
        fc = f;
      }
    }

    if (fc) {
      double div = (double)*fc / METER_LENGTH;  // GOOS will use doubles here
      if (div * METER_LENGTH == *fc) {
        return pool.form<GenericElement>(
            GenericOperator::make_function(pool.form<ConstantTokenElement>("meters")),
            pool.form<ConstantTokenElement>(float_to_string(div, false)));
      } else {
        lg::error("Floating point value {} could not be converted to meters.", *fc);
      }
    }
  } else if (new_type == TypeSpec("degrees")) {
    auto fc = get_goal_float_constant(in);
    if (fc) {
      double div = (double)*fc / DEGREES_LENGTH;  // GOOS will use doubles here
      if (div * DEGREES_LENGTH == *fc) {
        return pool.form<GenericElement>(
            GenericOperator::make_function(pool.form<ConstantTokenElement>("degrees")),
            pool.form<ConstantFloatElement>(div));
      } else {
        lg::error("Floating point value {} could not be converted to degrees.", *fc);
      }
    }
  } else if (new_type == TypeSpec("handle")) {
    auto in_generic = in->try_as_element<GenericElement>();
    if (in_generic && (in_generic->op().is_fixed(FixedOperatorKind::PROCESS_TO_HANDLE) ||
                       in_generic->op().is_fixed(FixedOperatorKind::PPOINTER_TO_HANDLE))) {
      return in;
    }
  } else if (new_type == TypeSpec("process")) {
    auto in_generic = in->try_as_element<GenericElement>();
    if (in_generic && in_generic->op().is_fixed(FixedOperatorKind::PPOINTER_TO_PROCESS)) {
      return in;
    }
  } else if (new_type == TypeSpec("time-frame")) {
    auto ic = get_goal_integer_constant(in, env);
    if (ic) {
      s64 value = *ic;
      if (std::abs(value) <= 1) {
        // if they used a 1 as a time-frame, they likely just want to literally remove 1
        // instead of (seconds 0.0034) or whatever the result would be.
        // also 0 is decompiled as just 0.
        return pool.form<SimpleAtomElement>(SimpleAtom::make_int_constant(value));
      }

      return pool.form<ConstantTokenElement>(
          fmt::format("(seconds {})", fixed_point_to_string(value, TICKS_PER_SECOND)));
    } else {
      // not a constant like (seconds 2), probably an expression or function call. we pretend that a
      // cast to int happened instead.
      // TODO hardcode cases for rand-vu-int-range:
      // (rand-vu-int-range 1200 2400) -> (rand-vu-int-range (seconds 4) (seconds 8))
      // return try_cast_simplify(in, TypeSpec("int"), pool, env, tc_pass);
      auto g = dynamic_cast<GenericElement*>(in->try_as_single_element());
      if (g && g->op().kind() == GenericOperator::Kind::FUNCTION_EXPR) {
        auto f = dynamic_cast<SimpleExpressionElement*>(g->op().func()->try_as_single_element());
        if (f && f->expr().is_identity() && f->expr().get_arg(0).is_sym_val()) {
          auto& func_name = f->expr().get_arg(0).get_str();
          if (func_name == "rand-vu-int-range" || func_name == "nav-enemy-rnd-int-range") {
            std::vector<Form*> new_forms;
            for (auto& e : g->elts()) {
              auto as_atom_expr =
                  dynamic_cast<SimpleExpressionElement*>(e->try_as_single_element());
              if (as_atom_expr && as_atom_expr->expr().is_identity()) {
                new_forms.push_back(try_cast_simplify(e, TypeSpec("time-frame"), pool, env, true));
              } else {
                new_forms.push_back(e);
              }
            }
            // return a new rand-vu-int-range with casted args, and its own cast is stripped for
            // free!
            return pool.alloc_single_element_form<GenericElement>(in->parent_element, g->op(),
                                                                  new_forms);
          }
        }
      }
      if (tc_pass) {
        return in;
      } else {
        return nullptr;
      }
    }
  }

  auto type_info = env.dts->ts.lookup_type_allow_partial_def(new_type);
  auto bitfield_info = dynamic_cast<BitFieldType*>(type_info);
  auto enum_info = dynamic_cast<EnumType*>(type_info);
  auto* in_as_cond = in->try_as_element<CondWithElseElement>();

  // try to fix (the-as <enum> (if foo 12 13)) type stuff by applying the casts inside a cond if:
  // - it's casting to a bitfield/enum (this could be expanded to more in the future if needed)
  // - the cond has an explicit else case (otherwise the #f from not hitting any case...)
  // - it's not a sound-id - these are basically used like ints so it gets worse
  // - the cond doesn't have multiple entries in the body
  //    in theory this could be better if we could only apply a cast to the last element in the body
  //    but this is a bit too much work for exactly 1 case in jak 1.
  if ((bitfield_info || enum_info) && in_as_cond && type_info->get_name() != "sound-id" &&
      cond_has_only_single_elements(in_as_cond)) {
    for (auto& cas : in_as_cond->entries) {
      cas.body = try_cast_simplify(cas.body, new_type, pool, env, tc_pass);
      cas.body->parent_element = in_as_cond;
    }
    in_as_cond->else_ir = try_cast_simplify(in_as_cond->else_ir, new_type, pool, env, tc_pass);
    in_as_cond->else_ir->parent_element = in_as_cond;
    return in;
  }

  if (bitfield_info) {
    // todo remove this.
    if (bitfield_info->get_load_size() == 8) {
      in = strip_pcypld_64(in);
    }
    return cast_to_bitfield(bitfield_info, new_type, pool, env, in);
  }

  if (enum_info) {
    if (enum_info->is_bitfield()) {
      return cast_to_bitfield_enum(enum_info, new_type, pool, env, in);
    } else {
      return cast_to_int_enum(enum_info, new_type, pool, env, in);
    }
  }

  auto as_atom = form_as_atom(in);
  if (as_atom && as_atom->is_var()) {
    if (env.get_variable_type(as_atom->var(), true) == new_type) {
      // we are a variable with the right type.
      return pool.form<SimpleAtomElement>(*as_atom, true);
    }
  }

  if (tc_pass) {
    return in;
  } else {
    return nullptr;
  }
}

bool Form::has_side_effects() {
  bool has_side_effect = false;
  apply([&](FormElement* elt) {
    if (dynamic_cast<SetVarElement*>(elt)) {
      has_side_effect = true;
    }
  });
  return has_side_effect;
}

bool FormElement::has_side_effects() {
  bool has_side_effect = false;
  apply([&](FormElement* elt) {
    if (dynamic_cast<SetVarElement*>(elt)) {
      has_side_effect = true;
    }
  });
  return has_side_effect;
}

namespace {

bool is_power_of_two(int in, int* out) {
  int x = 1;
  for (int i = 0; i < 32; i++) {
    if (x == in) {
      *out = i;
      return true;
    }
    x = x * 2;
  }
  return false;
}

/*!
 * Imagine:
 *   x = foo
 *   { // some macro/inlined thing
 *     read from x
 *     return x;
 *   }
 *
 * and you want to transform it to
 * x = some_macro(foo, blah, ...)
 *
 * this will get you foo (and pop it from the stack), assuming the stack is sitting right after the
 * point where the inline thing evaluated foo.
 *
 * For later book-keeping of reg use, if it gets you something new, it will set found_orig_out,
 * and also give you the regaccess for the x of the x = foo.
 *
 * If you use this, you are responsible for adding code that sets x again.
 */
Form* repop_passthrough_arg(Form* in,
                            FormStack& stack,
                            const Env& env,
                            RegisterAccess* orig_out,
                            bool* found_orig_out) {
  *found_orig_out = false;

  auto as_atom = form_as_atom(in);
  if (as_atom && as_atom->is_var()) {
    return stack.pop_reg(as_atom->var().reg(), {}, env, true, -1, orig_out, found_orig_out);
  }
  return in;
}

/*!
 * Create a form which represents a variable.
 */
Form* var_to_form(const RegisterAccess& var, FormPool& pool) {
  return pool.form<SimpleAtomElement>(SimpleAtom::make_var(var));
}

/*!
 * Pop values of off the expression stack
 * @param vars     : list of variables to pop. In order of source code evaluation.
 * @param env      : the decompilation environment
 * @param pool     : form allocation pool
 * @param stack    : stack to pop from
 * @param output   : list of locations to push results.
 * @param consumes : if you have a different list of variables that are consumed by this operation.
 */
void pop_helper(const std::vector<RegisterAccess>& vars,
                const Env& env,
                FormPool& pool,
                FormStack& stack,
                const std::vector<std::vector<FormElement*>*>& output,
                bool allow_side_effects,
                const std::optional<RegSet>& consumes = std::nullopt,
                const std::vector<int>& times_used = {}) {
  // to submit to stack to attempt popping
  std::vector<Register> submit_regs;
  // submit_reg[i] is for var submit_reg_to_var[i]
  std::vector<size_t> submit_reg_to_var;

  // build submission for stack
  std::unordered_map<Register, int, Register::hash> reg_counts;
  for (auto& v : vars) {
    reg_counts[v.reg()]++;
  }

  for (size_t var_idx = 0; var_idx < vars.size(); var_idx++) {
    const auto& var = vars.at(var_idx);
    auto& ri = env.reg_use().op.at(var.idx());
    RegSet consumes_to_use = consumes.value_or(ri.consumes);
    if (consumes_to_use.find(var.reg()) != consumes_to_use.end()) {
      if (reg_counts.at(var.reg()) == 1) {
        // we consume the register, so it's safe to try popping.

        int times = 1;
        if (!times_used.empty()) {
          times = times_used.at(var_idx);
        }

        auto& use_def = env.get_use_def_info(var);
        if (use_def.use_count() == times && use_def.def_count() == 1) {
          submit_reg_to_var.push_back(var_idx);
          submit_regs.push_back(var.reg());
        } else {
          // auto var_id = env.get_program_var_id(var);
          //          lg::print(
          //              "Unsafe to pop {}: used {} times, def {} times, expected use {} ({} {} rd:
          //              {}) ({} "
          //              "{})\n",
          //              var.to_string(env), use_def.use_count(), use_def.def_count(), times,
          //              var.reg().to_string(), var.idx(), var.mode() == AccessMode::READ,
          //              var_id.reg.to_string(), var_id.id);

          //          if (var.to_string(env) == "a3-0") {
          //            for (auto& use : use_def.uses) {
          //              if (!use.disabled) {
          //                lg::print("  at instruction {}\n", use.op_id);
          //              }
          //            }
          //          }
        }
      }
    }
  }

  // submit and get a result! If the stack has nothing to pop, the result here may be nullptr.
  std::vector<Form*> pop_result;
  // loop in reverse (later vals first)
  for (size_t i = submit_regs.size(); i-- > 0;) {
    // figure out what var we are:
    auto var_idx = submit_reg_to_var.at(i);

    // anything _less_ than this should be unmodified by the pop
    // it's fine to modify yourself in your pop.
    RegSet pop_barrier_regs;
    for (size_t j = 0; j < var_idx; j++) {
      pop_barrier_regs.insert(vars.at(j).reg());
    }

    // do the pop, with the barrier to prevent out-of-sequence popping.
    pop_result.push_back(
        stack.pop_reg(submit_regs.at(i), pop_barrier_regs, env, allow_side_effects));
  }
  // now flip back to the source order for making the final result
  std::reverse(pop_result.begin(), pop_result.end());

  // final result forms. Will be nullptr if: we didn't try popping OR popping from stack failed.
  std::vector<Form*> forms;
  forms.resize(vars.size(), nullptr);
  if (!pop_result.empty()) {
    // success!
    for (size_t i = 0; i < submit_regs.size(); i++) {
      // fill out vars from our submission
      forms.at(submit_reg_to_var.at(i)) = pop_result.at(i);
    }
  }

  // write the output
  for (size_t i = 0; i < forms.size(); i++) {
    if (forms.at(i)) {
      // we got a form. inline these in the result
      for (auto x : forms.at(i)->elts()) {
        output.at(i)->push_back(x);
      }
    } else {
      // we got nothing, just insert the variable name.
      output.at(i)->push_back(pool.alloc_element<SimpleExpressionElement>(
          SimpleAtom::make_var(vars.at(i)).as_expr(), vars.at(i).idx()));
    }
  }
}

/*!
 * This should be used to generate all casts.
 */
Form* cast_form(Form* in,
                const TypeSpec& new_type,
                FormPool& pool,
                const Env& env,
                bool tc_pass = false) {
  auto result = try_cast_simplify(in, new_type, pool, env, tc_pass);
  if (result) {
    return result;
  }

  return pool.form<CastElement>(new_type, in);
}

/*!
 * Pop each variable in the input list into a form. The variables should be given in the order
 * they are evaluated in the source. It is safe to put the result of these in the same expression.
 * This uses the barrier register approach, but it is only effective if you put all registers
 * appearing at the same level.
 */
std::vector<Form*> pop_to_forms(const std::vector<RegisterAccess>& vars,
                                const Env& env,
                                FormPool& pool,
                                FormStack& stack,
                                bool allow_side_effects,
                                const std::optional<RegSet>& consumes = std::nullopt,
                                const std::vector<int>& times_to_use = {}) {
  std::vector<Form*> forms;
  std::vector<std::vector<FormElement*>> forms_out;
  std::vector<std::vector<FormElement*>*> form_ptrs;
  forms_out.resize(vars.size());
  form_ptrs.reserve(vars.size());
  forms.reserve(vars.size());
  for (auto& x : forms_out) {
    form_ptrs.push_back(&x);
  }

  pop_helper(vars, env, pool, stack, form_ptrs, allow_side_effects, consumes, times_to_use);

  for (auto& x : forms_out) {
    forms.push_back(pool.alloc_sequence_form(nullptr, x));
  }

  // add casts, if needed.
  ASSERT(vars.size() == forms.size());
  for (size_t i = 0; i < vars.size(); i++) {
    auto atom = form_as_atom(forms[i]);
    bool is_var = atom && atom->is_var();
    auto cast = env.get_user_cast_for_access(vars[i]);
    // only cast if we didn't get a var (compacting expressions).
    // there is a separate system for casting variables that will do a better job.
    if (cast && !is_var) {
      forms[i] = cast_form(forms[i], *cast, pool, env);
      // pool.form<CastElement>(*cast, forms[i]);
    }
  }

  return forms;
}

/*!
 * type == float (exactly)?
 */
bool is_float_type(const Env& env, int my_idx, RegisterAccess var) {
  auto type = env.get_types_before_op(my_idx).get(var.reg()).typespec();
  return env.dts->ts.tc(TypeSpec("float"), type);
}

/*!
 * type == int (exactly)?
 * note: time-frame is special.
 */
bool is_int_type(const TypeSpec& type) {
  return type == TypeSpec("int") || type == TypeSpec("time-frame");
}
bool is_int_type(const Env& env, int my_idx, RegisterAccess var) {
  auto type = env.get_types_before_op(my_idx).get(var.reg()).typespec();
  return is_int_type(type);
}

bool is_pointer_type(const Env& env, int my_idx, RegisterAccess var) {
  auto type = env.get_types_before_op(my_idx).get(var.reg()).typespec();
  return type.base_type() == "pointer";
}

/*!
 * type == uint (exactly)?
 */
bool is_uint_type(const TypeSpec& type) {
  return type == TypeSpec("uint");
}
bool is_uint_type(const Env& env, int my_idx, RegisterAccess var) {
  auto type = env.get_types_before_op(my_idx).get(var.reg()).typespec();
  return is_uint_type(type);
}

bool is_ptr_or_child(const Env& env, int my_idx, RegisterAccess var, bool) {
  // Now that decompiler types are synced up properly, we don't want this.
  //    auto type = as_var ? env.get_variable_type(var, true).base_type()
  //                       : env.get_types_before_op(my_idx).get(var.reg()).typespec().base_type();
  auto type = env.get_types_before_op(my_idx).get(var.reg()).typespec().base_type();
  return type == "pointer";
}

}  // namespace

/*!
 * Update a form to use values from the stack. Won't push to the stack.
 * This should be used to update a Form that immediately follows something being pushed.
 * Will only change the first element of the form - anything after that will jump sequencing
 */
void Form::update_children_from_stack(const Env& env,
                                      FormPool& pool,
                                      FormStack& stack,
                                      bool allow_side_effects) {
  ASSERT(!m_elements.empty());

  std::vector<FormElement*> new_elts;

  for (size_t i = 0; i < m_elements.size(); i++) {
    if (i == 0) {
      // only bother doing the first one.
      if (!m_elements[i]->is_popped()) {
        m_elements[i]->update_from_stack(env, pool, stack, &new_elts, allow_side_effects);
      } else {
        new_elts.push_back(m_elements[i]);
      }

    } else {
      new_elts.push_back(m_elements[i]);
    }
  }

  for (auto& x : new_elts) {
    x->parent_form = this;
  }

  m_elements = new_elts;
}

/*!
 * Default update_from_stack for an element if no specific one is provided.
 */
void FormElement::update_from_stack(const Env& env,
                                    FormPool&,
                                    FormStack&,
                                    std::vector<FormElement*>*,
                                    bool) {
  throw std::runtime_error(fmt::format("update_from_stack NYI for {}", to_string(env)));
}

namespace {
/*!
 * Try to make a pretty looking constant out of value for comparing to something of type.
 * If we can't do anything nice, return nullptr.
 */
Form* try_make_constant_for_compare(Form* value,
                                    const TypeSpec& type,
                                    FormPool& pool,
                                    const Env& env) {
  if (get_goal_integer_constant(value, env) &&
      (env.dts->ts.try_enum_lookup(type) || type == TypeSpec("time-frame"))) {
    return cast_form(value, type, pool, env);
  }
  return nullptr;
}

Form* make_cast_if_needed(Form* in,
                          const TypeSpec& in_type,
                          const TypeSpec& out_type,
                          FormPool& pool,
                          const Env& env) {
  if (in_type == out_type) {
    return in;
  }

  if (out_type == TypeSpec("float") && env.dts->ts.tc(TypeSpec("float"), in_type)) {
    return in;
  }

  if (out_type == TypeSpec("time-frame") && in_type == TypeSpec("int")) {
    // we want a time-frame from an int.
    return try_cast_simplify(in, TypeSpec("time-frame"), pool, env, true);
  }

  return cast_form(in, out_type, pool, env);
}

std::vector<Form*> make_casts_if_needed(const std::vector<Form*>& in,
                                        const std::vector<TypeSpec>& in_types,
                                        const TypeSpec& out_type,
                                        FormPool& pool,
                                        const Env& env) {
  std::vector<Form*> out;
  TypeSpec actual_out = out_type;
  ASSERT(in.size() == in_types.size());
  for (size_t i = 0; i < in_types.size(); i++) {
    // check if there's variables of aliased types. we will cast to those instead.
    if (!get_goal_integer_constant(in.at(i), env) && out_type == TypeSpec("int")) {
      if (in_types.at(i) == TypeSpec("time-frame")) {
        // if we want int, we can output time-frame as well
        actual_out = TypeSpec("time-frame");
      }
    } else if (!get_goal_float_constant(in.at(i)) && out_type == TypeSpec("float")) {
      if (in_types.at(i) == TypeSpec("meters")) {
        // if we want float, we can output meters as well
        // actual_out = TypeSpec("meters");
      }
    }
  }
  for (size_t i = 0; i < in_types.size(); i++) {
    out.push_back(make_cast_if_needed(in.at(i), in_types.at(i), actual_out, pool, env));
  }
  return out;
}
}  // namespace

/*!
 * Update a LoadSourceElement from the stack.
 */
void LoadSourceElement::update_from_stack(const Env& env,
                                          FormPool& pool,
                                          FormStack& stack,
                                          std::vector<FormElement*>* result,
                                          bool allow_side_effects) {
  mark_popped();
  m_addr->update_children_from_stack(env, pool, stack, allow_side_effects);

  // most of the time, the AtomicOpForm logic is able to figure the load, but sometimes
  // it's impossible before expressions:

  //  ori a2, r0, 33708
  //  lw a3, *level*(s7)
  //  daddu a2, a2, a3
  //  lq a2, 0(a2)

  /*
  if (m_load_source_ro && m_load_source_ro->offset == 0) {
    // maybe a case like above, try to improve
  }
  */

  result->push_back(this);
}

namespace {
FormElement* label_to_form_element(const Env& env, const SimpleAtom& atom, FormPool& pool) {
  auto lab = env.file->labels.at(atom.label());
  if (env.file->is_string(lab.target_segment, lab.offset)) {
    auto str = env.file->get_goal_string(lab.target_segment, lab.offset / 4 - 1, false);
    return pool.alloc_element<StringConstantElement>(str);
  } else {
    // look for a label hint:
    const auto& hint = env.file->label_db->lookup(lab.name);
    if (!hint.known) {
      throw std::runtime_error(
          fmt::format("Label {} was unknown in FormExpressionAnalysis.", hint.name));
    }
    if (hint.is_value) {
      return nullptr;
    }
    if (hint.result_type.base_type() == "function") {
      return nullptr;
    } else {
      return pool.alloc_element<DecompiledDataElement>(lab, hint);
    }
  }
}
}  // namespace

void SimpleExpressionElement::update_from_stack_identity(const Env& env,
                                                         FormPool& pool,
                                                         FormStack& stack,
                                                         std::vector<FormElement*>* result,
                                                         bool allow_side_effects) {
  auto& arg = m_expr.get_arg(0);
  if (arg.is_var()) {
    auto forms = pop_to_forms({arg.var()}, env, pool, stack, allow_side_effects);
    for (auto x : forms.at(0)->elts()) {
      result->push_back(x);
    }
  } else if (arg.is_static_addr()) {
    auto as_label_form_element = label_to_form_element(env, arg, pool);
    if (as_label_form_element) {
      result->push_back(as_label_form_element);
    } else {
      result->push_back(this);
    }
    return;
  } else if (arg.is_sym_ptr() || arg.is_sym_val() || arg.is_int() || arg.is_empty_list() ||
             arg.is_sym_val_ptr()) {
    result->push_back(this);
    return;
  } else {
    throw std::runtime_error(fmt::format(
        "SimpleExpressionElement::update_from_stack_identity NYI for {}", to_string(env)));
  }
}

bool u64_valid_for_float_constant(u64 in) {
  u32 top = in >> 32;
  if (top == 0 || top == UINT32_MAX) {
    return true;
  } else {
    return false;
  }
}

void SimpleExpressionElement::update_from_stack_gpr_to_fpr(const Env& env,
                                                           FormPool& pool,
                                                           FormStack& stack,
                                                           std::vector<FormElement*>* result,
                                                           bool allow_side_effects) {
  auto src = m_expr.get_arg(0);
  auto src_type = env.get_types_before_op(src.var().idx()).get(src.var().reg());
  std::vector<FormElement*> src_fes;
  if (src.is_var()) {
    auto forms = pop_to_forms({src.var()}, env, pool, stack, allow_side_effects);
    for (auto x : forms.at(0)->elts()) {
      src_fes.push_back(x);
    }
  } else {
    src_fes = {this};
  }

  // set ourself to identity.
  m_expr = src.as_expr();

  if (env.dts->ts.tc(TypeSpec("float"), src_type.typespec())) {
    // got a float as an input, we can convert it to an FPR with no effect.
    for (auto x : src_fes) {
      result->push_back(x);
    }
  } else {
    if (env.version != GameVersion::Jak1) {
      auto frm = pool.alloc_sequence_form(nullptr, src_fes);
      if (src_fes.size() == 1) {
        auto int_constant = get_goal_integer_constant(frm, env);
        if (int_constant && u64_valid_for_float_constant(*int_constant)) {
          float flt;
          memcpy(&flt, &int_constant.value(), sizeof(float));
          if (proper_float(flt)) {
            result->push_back(pool.alloc_element<ConstantFloatElement>(flt));
            return;
          }
        }
      }
      // converting something else to an FPR, put an expression around it.
      result->push_back(pool.alloc_element<GenericElement>(
          GenericOperator::make_fixed(FixedOperatorKind::GPR_TO_FPR), frm));
    } else {
      result->push_back(pool.alloc_element<GenericElement>(
          GenericOperator::make_fixed(FixedOperatorKind::GPR_TO_FPR),
          pool.alloc_sequence_form(nullptr, src_fes)));
    }
  }
}

void SimpleExpressionElement::update_from_stack_fpr_to_gpr(const Env& env,
                                                           FormPool& pool,
                                                           FormStack& stack,
                                                           std::vector<FormElement*>* result,
                                                           bool allow_side_effects) {
  auto src = m_expr.get_arg(0);
  auto src_type = env.get_types_before_op(m_my_idx).get(src.var().reg());
  if (env.dts->ts.tc(TypeSpec("float"), src_type.typespec()) ||
      src_type.typespec() == TypeSpec("int")) {
    // set ourself to identity.
    m_expr = src.as_expr();
    // then go again.
    ASSERT(m_popped);
    m_popped = false;
    update_from_stack(env, pool, stack, result, allow_side_effects);
  } else {
    throw std::runtime_error(fmt::format("FPR -> GPR applied to a {} in {} at {}", src_type.print(),
                                         to_string(env), m_my_idx));
  }
}

void SimpleExpressionElement::update_from_stack_div_s(const Env& env,
                                                      FormPool& pool,
                                                      FormStack& stack,
                                                      std::vector<FormElement*>* result,
                                                      bool allow_side_effects) {
  if (is_float_type(env, m_my_idx, m_expr.get_arg(0).var()) &&
      is_float_type(env, m_my_idx, m_expr.get_arg(1).var())) {
    // todo - check the order here

    auto args = pop_to_forms({m_expr.get_arg(0).var(), m_expr.get_arg(1).var()}, env, pool, stack,
                             allow_side_effects);
    auto new_form = pool.alloc_element<GenericElement>(
        GenericOperator::make_fixed(FixedOperatorKind::DIVISION), args.at(0), args.at(1));
    result->push_back(new_form);
  } else {
    throw std::runtime_error(
        fmt::format("Floating point division attempted on invalid types at OP: [{}].", m_my_idx));
  }
}

/*!
 * Update a two-argument form that uses two floats.
 */
void SimpleExpressionElement::update_from_stack_float_2(const Env& env,
                                                        FixedOperatorKind kind,
                                                        FormPool& pool,
                                                        FormStack& stack,
                                                        std::vector<FormElement*>* result,
                                                        bool allow_side_effects) {
  if (is_float_type(env, m_my_idx, m_expr.get_arg(0).var()) &&
      is_float_type(env, m_my_idx, m_expr.get_arg(1).var())) {
    auto args = pop_to_forms({m_expr.get_arg(0).var(), m_expr.get_arg(1).var()}, env, pool, stack,
                             allow_side_effects);
    auto new_form = pool.alloc_element<GenericElement>(GenericOperator::make_fixed(kind),
                                                       args.at(0), args.at(1));
    result->push_back(new_form);
  } else {
    auto type0 = env.get_types_before_op(m_my_idx).get(m_expr.get_arg(0).var().reg());
    auto type1 = env.get_types_before_op(m_my_idx).get(m_expr.get_arg(1).var().reg());
    throw std::runtime_error(fmt::format(
        "[OP: {}] - Floating point math attempted on invalid types: {} and {} in op {}.", m_my_idx,
        type0.print(), type1.print(), to_string(env)));
  }
}

namespace {
std::vector<Form*> get_math_op_elements(Form* in, FixedOperatorKind kind) {
  auto gen_elt = in->try_as_element<GenericElement>();
  if (gen_elt && gen_elt->op().is_fixed(kind)) {
    return gen_elt->elts();
  } else {
    return {in};
  }
}

FormElement* make_and_compact_math_op(Form* arg0,
                                      Form* arg1,
                                      const std::optional<TypeSpec>& arg0_cast,
                                      const std::optional<TypeSpec>& arg1_cast,
                                      FormPool& pool,
                                      const Env& env,
                                      FixedOperatorKind operator_kind,
                                      bool inline_first,
                                      bool inline_second) {
  if (!arg1_cast) {
    std::vector<Form*> arg0_elts;
    if (inline_first) {
      arg0_elts = get_math_op_elements(arg0, operator_kind);
    } else {
      arg0_elts = {arg0};
    }

    ASSERT(!arg0_elts.empty());
    if (arg0_cast) {
      arg0_elts.front() = cast_form(arg0_elts.front(), *arg0_cast, pool, env);
    }

    // it's fine to only cast the first thing here - the rest are already cast properly.
    std::vector<Form*> arg1_elts;
    if (inline_second) {
      arg1_elts = get_math_op_elements(arg1, operator_kind);
    } else {
      arg1_elts = {arg1};
    }

    ASSERT(!arg1_elts.empty());
    if (arg1_cast) {
      arg1_elts.front() = cast_form(arg1_elts.front(), *arg1_cast, pool, env);
    }

    // add all together
    arg0_elts.insert(arg0_elts.end(), arg1_elts.begin(), arg1_elts.end());
    return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(operator_kind),
                                              arg0_elts);
  } else {
    if (arg0_cast) {
      arg0 = cast_form(arg0, *arg0_cast, pool, env);
    }

    if (arg1_cast) {
      arg1 = cast_form(arg1, *arg1_cast, pool, env);
    }
    return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(operator_kind), arg0,
                                              arg1);
  }
}
}  // namespace

/*!
 * Update a two-argument form that uses two floats.
 * This is for operations like * and + that can be nested
 * (* (* a b) c) -> (* a b c)
 * Note that we only apply this to the _first_ argument to keep the order of operations the same.
 */
void SimpleExpressionElement::update_from_stack_float_2_nestable(const Env& env,
                                                                 FixedOperatorKind kind,
                                                                 FormPool& pool,
                                                                 FormStack& stack,
                                                                 std::vector<FormElement*>* result,
                                                                 bool allow_side_effects) {
  if (is_float_type(env, m_my_idx, m_expr.get_arg(0).var()) &&
      is_float_type(env, m_my_idx, m_expr.get_arg(1).var())) {
    auto args = pop_to_forms({m_expr.get_arg(0).var(), m_expr.get_arg(1).var()}, env, pool, stack,
                             allow_side_effects);
    auto new_form =
        make_and_compact_math_op(args.at(0), args.at(1), {}, {}, pool, env, kind, true, false);
    result->push_back(new_form);
  } else {
    auto type0 = env.get_types_before_op(m_my_idx).get(m_expr.get_arg(0).var().reg());
    auto type1 = env.get_types_before_op(m_my_idx).get(m_expr.get_arg(1).var().reg());
    throw std::runtime_error(fmt::format(
        "[OP: {}] - Floating point math attempted on invalid types: {} and {} in op {}.", m_my_idx,
        type0.print(), type1.print(), to_string(env)));
  }
}

void SimpleExpressionElement::update_from_stack_float_1(const Env& env,
                                                        FixedOperatorKind kind,
                                                        FormPool& pool,
                                                        FormStack& stack,
                                                        std::vector<FormElement*>* result,
                                                        bool allow_side_effects) {
  if (is_float_type(env, m_my_idx, m_expr.get_arg(0).var())) {
    auto args = pop_to_forms({m_expr.get_arg(0).var()}, env, pool, stack, allow_side_effects);
    auto new_form =
        pool.alloc_element<GenericElement>(GenericOperator::make_fixed(kind), args.at(0));
    result->push_back(new_form);
  } else {
    throw std::runtime_error(
        fmt::format("Floating point division attempted on invalid types at OP: [{}].", m_my_idx));
  }
}

void SimpleExpressionElement::update_from_stack_si_1(const Env& env,
                                                     FixedOperatorKind kind,
                                                     FormPool& pool,
                                                     FormStack& stack,
                                                     std::vector<FormElement*>* result,
                                                     bool allow_side_effects) {
  auto in_type = env.get_types_before_op(m_my_idx).get(m_expr.get_arg(0).var().reg()).typespec();
  auto arg = pop_to_forms({m_expr.get_arg(0).var()}, env, pool, stack, allow_side_effects).at(0);
  result->push_back(pool.alloc_element<GenericElement>(
      GenericOperator::make_fixed(kind),
      make_cast_if_needed(arg, in_type, TypeSpec("int"), pool, env)));
}

void SimpleExpressionElement::update_from_stack_add_i(const Env& env,
                                                      FormPool& pool,
                                                      FormStack& stack,
                                                      std::vector<FormElement*>* result,
                                                      bool allow_side_effects) {
  auto& arg0_type = env.get_types_before_op(m_my_idx).get(m_expr.get_arg(0).var().reg());
  auto arg0_i = is_int_type(arg0_type.typespec());
  auto arg0_u = is_uint_type(arg0_type.typespec());

  bool arg1_reg = m_expr.get_arg(1).is_var();
  bool arg1_i = true;
  bool arg1_u = true;
  bool arg1_timeframe = false;
  if (arg1_reg) {
    auto arg1_type =
        env.get_types_before_op(m_my_idx).get(m_expr.get_arg(1).var().reg()).typespec();
    arg1_timeframe = arg1_type == TypeSpec("time-frame");
    arg1_i = is_int_type(arg1_type);
    arg1_u = is_uint_type(arg1_type);
  }

  std::vector<Form*> args;

  if (arg1_reg) {
    args = pop_to_forms({m_expr.get_arg(0).var(), m_expr.get_arg(1).var()}, env, pool, stack,
                        allow_side_effects);
  } else {
    // arg1 might be a label.
    // do arg0 like a normal var
    args = pop_to_forms({m_expr.get_arg(0).var()}, env, pool, stack, allow_side_effects);

    // then try to simplify the label
    if (m_expr.get_arg(1).is_label()) {
      auto as_lab = label_to_form_element(env, m_expr.get_arg(1), pool);
      if (as_lab) {
        args.push_back(pool.alloc_single_form(nullptr, as_lab));
      } else {
        args.push_back(pool.form<SimpleAtomElement>(m_expr.get_arg(1)));
      }
    } else {
      args.push_back(pool.form<SimpleAtomElement>(m_expr.get_arg(1)));
    }
  }

  bool arg0_ptr = is_ptr_or_child(env, m_my_idx, m_expr.get_arg(0).var(), true);
  bool arg1_ptr = false;

  // Look for getting an address inside of an object.
  // (+ <integer 108 + int> process). array style access with a stride of 1.
  // in the case, both are vars.
  if (arg1_reg) {
    // lookup types.
    auto& arg1_type = env.get_types_before_op(m_my_idx).get(m_expr.get_arg(1).var().reg());
    arg1_ptr = is_ptr_or_child(env, m_my_idx, m_expr.get_arg(1).var(), true);

    // try to find symbol to string stuff
    auto arg0_int = get_goal_integer_constant(args.at(0), env);

    if (arg0_int && ((s64)*arg0_int == SYMBOL_TO_STRING_MEM_OFFSET_DECOMP[env.version]) &&
        allowable_base_type_for_symbol_to_string(arg1_type.typespec())) {
      result->push_back(pool.alloc_element<GetSymbolStringPointer>(args.at(1)));
      return;
    }

    auto addition_matcher =
        GenericOpMatcher::or_match({GenericOpMatcher::fixed(FixedOperatorKind::ADDITION),
                                    GenericOpMatcher::fixed(FixedOperatorKind::ADDITION_PTR)});
    if (arg0_type.kind == TP_Type::Kind::INTEGER_CONSTANT_PLUS_VAR) {
      // try to see if this is valid, from the type system.
      FieldReverseLookupInput input;
      input.offset = arg0_type.get_integer_constant();
      input.stride = 1;
      input.base_type = arg1_type.typespec();
      auto out = env.dts->ts.reverse_field_lookup(input);
      if (out.success && out.has_variable_token()) {
        // it is. now we have to modify things
        // first, look for the index

        auto arg0_matcher =
            Matcher::op(addition_matcher, {Matcher::any(0), Matcher::integer(input.offset)});
        auto match_result = match(arg0_matcher, args.at(0));
        if (match_result.matched) {
          bool used_index = false;
          std::vector<DerefToken> tokens;
          for (auto& tok : out.tokens) {
            if (tok.kind == FieldReverseLookupOutput::Token::Kind::VAR_IDX) {
              ASSERT(!used_index);
              used_index = true;
              tokens.push_back(DerefToken::make_int_expr(match_result.maps.forms.at(0)));
            } else {
              tokens.push_back(to_token(tok));
            }
          }
          ASSERT(used_index);
          result->push_back(pool.alloc_element<DerefElement>(args.at(1), out.addr_of, tokens));
          return;
        } else {
          throw std::runtime_error(
              fmt::format("Failed to match for stride 1 address access with add: {}",
                          args.at(0)->to_string(env)));
        }
      }
    } else if (arg0_type.kind == TP_Type::Kind::INTEGER_CONSTANT_PLUS_VAR_MULT) {
      // try to see if this is valid, from the type system.
      FieldReverseLookupInput input;
      input.offset = arg0_type.get_add_int_constant();
      input.stride = arg0_type.get_mult_int_constant();
      input.base_type = arg1_type.typespec();
      auto out = env.dts->ts.reverse_field_lookup(input);
      if (out.success && out.has_variable_token()) {
        // it is. now we have to modify things
        // first, look for the index
        int p2;
        if (is_power_of_two(input.stride, &p2)) {
          // (+ (shl (-> a0-0 reg-count) 3) 28)
          auto arg0_matcher =
              Matcher::op(addition_matcher,
                          {Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::MULTIPLICATION),
                                       {Matcher::any(0), Matcher::integer(input.stride)}),
                           Matcher::integer(input.offset)});
          auto match_result = match(arg0_matcher, args.at(0));
          if (match_result.matched) {
            bool used_index = false;
            std::vector<DerefToken> tokens;
            for (auto& tok : out.tokens) {
              if (tok.kind == FieldReverseLookupOutput::Token::Kind::VAR_IDX) {
                ASSERT(!used_index);
                used_index = true;
                tokens.push_back(DerefToken::make_int_expr(match_result.maps.forms.at(0)));
              } else {
                tokens.push_back(to_token(tok));
              }
            }
            ASSERT(used_index);
            result->push_back(pool.alloc_element<DerefElement>(args.at(1), out.addr_of, tokens));
            return;
          } else {
            throw std::runtime_error(
                fmt::format("Failed to match for stride (power 2 {}) with add: {}", input.stride,
                            args.at(0)->to_string(env)));
          }
        } else {
          auto int_matcher = Matcher::integer(input.stride);
          auto arg0_matcher = Matcher::op(
              addition_matcher,
              {Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::MULTIPLICATION),
                           {Matcher::match_or({Matcher::cast("uint", int_matcher), int_matcher}),
                            Matcher::any(0)}),
               Matcher::integer(input.offset)});
          auto match_result = match(arg0_matcher, args.at(0));
          if (match_result.matched) {
            bool used_index = false;
            std::vector<DerefToken> tokens;
            for (auto& tok : out.tokens) {
              if (tok.kind == FieldReverseLookupOutput::Token::Kind::VAR_IDX) {
                ASSERT(!used_index);
                used_index = true;
                tokens.push_back(DerefToken::make_int_expr(match_result.maps.forms.at(0)));
              } else {
                tokens.push_back(to_token(tok));
              }
            }
            ASSERT(used_index);
            result->push_back(pool.alloc_element<DerefElement>(args.at(1), out.addr_of, tokens));
            return;
          } else {
            throw std::runtime_error(
                fmt::format("Failed to match for stride (non power 2 {}) with add: {}",
                            input.stride, args.at(0)->to_string(env)));
          }
        }
      }
    } else if (arg1_type.kind == TP_Type::Kind::PRODUCT_WITH_CONSTANT &&
               arg0_type.kind == TP_Type::Kind::TYPESPEC &&
               arg0_type.typespec().base_type() == "inline-array") {
      FieldReverseLookupInput rd_in;
      rd_in.deref = std::nullopt;
      rd_in.stride = arg1_type.get_multiplier();
      rd_in.offset = 0;
      rd_in.base_type = arg0_type.typespec();
      auto rd = env.dts->ts.reverse_field_multi_lookup(rd_in);
      int idx_of_success = -1;
      if (rd.success) {
        for (int i = 0; i < (int)rd.results.size(); i++) {
          if (rd.results.at(i).has_variable_token()) {
            idx_of_success = i;
            break;
          }
        }
      }

      if (idx_of_success >= 0) {
        auto& rd_ok = rd.results.at(idx_of_success);
        auto stride_matcher = Matcher::match_or(
            {Matcher::cast("uint", Matcher::integer(rd_in.stride)),
             Matcher::cast("int", Matcher::integer(rd_in.stride)), Matcher::integer(rd_in.stride)});
        auto arg1_matcher = Matcher::match_or(
            {Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::MULTIPLICATION),
                         {Matcher::any(0), stride_matcher}),
             Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::MULTIPLICATION),
                         {stride_matcher, Matcher::any(0)})});
        auto match_result = match(arg1_matcher, args.at(1));
        if (match_result.matched) {
          bool used_index = false;
          std::vector<DerefToken> tokens;
          for (auto& tok : rd_ok.tokens) {
            if (tok.kind == FieldReverseLookupOutput::Token::Kind::VAR_IDX) {
              ASSERT(!used_index);
              used_index = true;
              tokens.push_back(DerefToken::make_int_expr(match_result.maps.forms.at(0)));
            } else {
              tokens.push_back(to_token(tok));
            }
          }
          ASSERT(used_index);
          result->push_back(pool.alloc_element<DerefElement>(args.at(0), rd_ok.addr_of, tokens));
          return;
        } else {
          throw std::runtime_error(fmt::format(
              "Failed to match product_with_constant inline array access 1 at Op. {}", m_my_idx));
        }
      }
    } else if (arg0_type.kind == TP_Type::Kind::PRODUCT_WITH_CONSTANT &&
               arg1_type.kind == TP_Type::Kind::TYPESPEC &&
               arg1_type.typespec().base_type() == "inline-array") {
      FieldReverseLookupInput rd_in;
      rd_in.deref = std::nullopt;
      rd_in.stride = arg0_type.get_multiplier();
      rd_in.offset = 0;
      rd_in.base_type = arg1_type.typespec();
      auto rd = env.dts->ts.reverse_field_multi_lookup(rd_in);
      int idx_of_success = -1;
      if (rd.success) {
        for (int i = 0; i < (int)rd.results.size(); i++) {
          if (rd.results.at(i).has_variable_token()) {
            idx_of_success = i;
            break;
          }
        }
      }
      // lg::print("here {} {} {}\n", rd_in.base_type.print(), rd.success,
      // rd.has_variable_token());

      if (idx_of_success >= 0) {
        auto& rd_ok = rd.results.at(idx_of_success);
        auto arg0_matcher = Matcher::match_or(
            {Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::MULTIPLICATION),
                         {Matcher::any(0), Matcher::integer(rd_in.stride)}),
             Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::MULTIPLICATION),
                         {Matcher::match_or({Matcher::cast("uint", Matcher::integer(rd_in.stride)),
                                             Matcher::integer(rd_in.stride)}),
                          Matcher::any(0)})});
        auto match_result = match(arg0_matcher, args.at(0));
        if (match_result.matched) {
          bool used_index = false;
          std::vector<DerefToken> tokens;
          for (auto& tok : rd_ok.tokens) {
            if (tok.kind == FieldReverseLookupOutput::Token::Kind::VAR_IDX) {
              ASSERT(!used_index);
              used_index = true;
              tokens.push_back(DerefToken::make_int_expr(match_result.maps.forms.at(0)));
            } else {
              tokens.push_back(to_token(tok));
            }
          }
          ASSERT(used_index);
          result->push_back(pool.alloc_element<DerefElement>(args.at(1), rd_ok.addr_of, tokens));
          return;
        } else {
          // TODO - output error to IR
          lg::error("Bad {} at OP: {}", args.at(0)->to_string(env), m_my_idx);
          throw std::runtime_error("Failed to match product_with_constant inline array access 2.");
        }
      }
    } else if (arg0_type.kind == TP_Type::Kind::INTEGER_CONSTANT) {
      // try to see if this is valid, from the type system.
      FieldReverseLookupInput input;
      input.offset = arg0_type.get_integer_constant();
      input.stride = 0;
      input.base_type = arg1_type.typespec();
      auto out = env.dts->ts.reverse_field_lookup(input);
      if (out.success && !out.has_variable_token()) {
        // it is. now we have to modify things
        // first, look for the index
        std::vector<DerefToken> tokens;
        for (auto& tok : out.tokens) {
          tokens.push_back(to_token(tok));
        }

        result->push_back(pool.alloc_element<DerefElement>(args.at(1), out.addr_of, tokens));
        return;
      }
    }
  }

  if (env.dts->ts.tc(TypeSpec("structure"), arg0_type.typespec()) && m_expr.get_arg(1).is_int() &&
      arg0_type.kind != TP_Type::Kind::INTEGER_CONSTANT_PLUS_VAR) {
    auto type_info = env.dts->ts.lookup_type(arg0_type.typespec());
    if (type_info->get_size_in_memory() == m_expr.get_arg(1).get_int()) {
      auto new_form = pool.alloc_element<GenericElement>(
          GenericOperator::make_fixed(FixedOperatorKind::ADDITION_PTR), args.at(0), args.at(1));
      result->push_back(new_form);
      return;
    }
  }

  auto& name = env.func->guessed_name;
  if (name.kind == FunctionName::FunctionKind::METHOD && name.method_id == 7 &&
      env.func->type.arg_count() == 3) {
    if ((env.dts->ts.tc(TypeSpec("structure"), arg0_type.typespec()) ||
         arg0_type.typespec().base_type() == "inline-array") &&
        (arg1_i || arg1_u)) {
      auto new_form = pool.alloc_element<GenericElement>(
          GenericOperator::make_fixed(FixedOperatorKind::ADDITION_PTR), args.at(0), args.at(1));
      result->push_back(new_form);
      return;
    }
  }

  if (arg0_ptr && arg0_type.kind != TP_Type::Kind::INTEGER_CONSTANT_PLUS_VAR) {
    auto new_form = pool.alloc_element<GenericElement>(
        GenericOperator::make_fixed(FixedOperatorKind::ADDITION_PTR), args.at(0), args.at(1));
    result->push_back(new_form);
  } else if (arg1_ptr && arg0_type.is_integer_constant()) {
    // this is a bit weird, but (&+ thing <constant>) sometimes becomes (&+ <constant> thing).
    // in these cases, we flip the argument order.
    auto new_form = pool.alloc_element<GenericElement>(
        GenericOperator::make_fixed(FixedOperatorKind::ADDITION_PTR), args.at(1), args.at(0));
    result->push_back(new_form);
  } else {
    std::optional<TypeSpec> arg0_cast, arg1_cast;

    if (!arg0_i && !arg0_u && arg0_type.typespec() != TypeSpec("binteger") &&
        !env.dts->ts.tc(TypeSpec("integer"), arg0_type.typespec())) {
      arg0_cast = TypeSpec(arg0_i ? "int" : "uint");
    }

    if (arg0_type.typespec() == TypeSpec("time-frame") && (!arg1_i || !arg1_reg)) {
      arg1_cast = TypeSpec("time-frame");
    } else if (!arg1_i && !arg1_u) {
      arg1_cast = TypeSpec(arg0_i ? "int" : "uint");
    }

    if (arg0_type.typespec() == TypeSpec("time-frame") && !arg1_timeframe) {
      auto as_generic = dynamic_cast<GenericElement*>(args.at(1)->try_as_single_element());
      if (as_generic && as_generic->op().kind() == GenericOperator::Kind::FUNCTION_EXPR) {
        auto as_func_head = dynamic_cast<SimpleExpressionElement*>(
            as_generic->op().func()->try_as_single_element());
        if (as_func_head && as_func_head->expr().is_identity() &&
            as_func_head->expr().get_arg(0).is_sym_val()) {
          auto& func_name = as_func_head->expr().get_arg(0).get_str();
          if (func_name == "rand-vu-int-range") {
            arg1_cast = TypeSpec("time-frame");
          }
        }
      }
    }

    result->push_back(make_and_compact_math_op(args.at(0), args.at(1), arg0_cast, arg1_cast, pool,
                                               env, FixedOperatorKind::ADDITION, true, true));
  }
}

void SimpleExpressionElement::update_from_stack_mult_si(const Env& env,
                                                        FormPool& pool,
                                                        FormStack& stack,
                                                        std::vector<FormElement*>* result,
                                                        bool allow_side_effects) {
  if (m_expr.get_arg(0).is_int()) {
    // annoyingly there's a mult3 v1, r0, v1 in jak 2.
    auto arg1_i = is_int_type(env, m_my_idx, m_expr.get_arg(1).var());

    auto args = pop_to_forms({m_expr.get_arg(1).var()}, env, pool, stack, allow_side_effects);

    if (!arg1_i) {
      args.at(0) = pool.form<CastElement>(TypeSpec("int"), args.at(1));
    }

    auto new_form = pool.alloc_element<GenericElement>(
        GenericOperator::make_fixed(FixedOperatorKind::MULTIPLICATION),
        pool.form<SimpleAtomElement>(SimpleAtom::make_int_constant(m_expr.get_arg(0).get_int())),
        args.at(0));

    result->push_back(new_form);
  } else {
    auto arg0_i = is_int_type(env, m_my_idx, m_expr.get_arg(0).var());
    auto arg1_i = is_int_type(env, m_my_idx, m_expr.get_arg(1).var());

    auto args = pop_to_forms({m_expr.get_arg(0).var(), m_expr.get_arg(1).var()}, env, pool, stack,
                             allow_side_effects);

    if (!arg0_i) {
      args.at(0) = pool.form<CastElement>(TypeSpec("int"), args.at(0));
    }

    if (!arg1_i) {
      args.at(1) = pool.form<CastElement>(TypeSpec("int"), args.at(1));
    }

    auto new_form = pool.alloc_element<GenericElement>(
        GenericOperator::make_fixed(FixedOperatorKind::MULTIPLICATION), args.at(0), args.at(1));
    result->push_back(new_form);
  }
}

void SimpleExpressionElement::update_from_stack_force_si_2(const Env& env,
                                                           FixedOperatorKind kind,
                                                           FormPool& pool,
                                                           FormStack& stack,
                                                           std::vector<FormElement*>* result,
                                                           bool allow_side_effects,
                                                           bool reverse) {
  auto arg0_type = env.get_types_before_op(m_my_idx).get(m_expr.get_arg(0).var().reg()).typespec();
  bool is_timeframe = arg0_type == TypeSpec("time-frame");
  auto arg0_i = is_int_type(arg0_type);
  bool arg1_i = true;
  bool arg1_reg = m_expr.get_arg(1).is_var();
  if (arg1_reg) {
    auto arg1_type =
        env.get_types_before_op(m_my_idx).get(m_expr.get_arg(1).var().reg()).typespec();
    // bool is_timeframe = arg1_type == TypeSpec("time-frame");
    arg1_i = is_int_type(arg1_type);
  } else {
    ASSERT(m_expr.get_arg(1).is_int());
  }

  std::vector<Form*> args;
  if (arg1_reg) {
    if (reverse) {
      args = pop_to_forms({m_expr.get_arg(1).var(), m_expr.get_arg(0).var()}, env, pool, stack,
                          allow_side_effects);
      auto temp = args.at(1);
      args.at(1) = args.at(0);
      args.at(0) = temp;
    } else {
      args = pop_to_forms({m_expr.get_arg(0).var(), m_expr.get_arg(1).var()}, env, pool, stack,
                          allow_side_effects);
    }

  } else {
    args = pop_to_forms({m_expr.get_arg(0).var()}, env, pool, stack, allow_side_effects);
    args.push_back(pool.form<SimpleAtomElement>(m_expr.get_arg(1)));
  }

  switch (kind) {
    case FixedOperatorKind::MOD:
    case FixedOperatorKind::MIN:
    case FixedOperatorKind::MAX:
      // can use time-frame
      break;
    default:
      // makes little sense to divide by a time most of the time.
      is_timeframe = false;
      break;
  }

  if (!arg0_i) {
    args.at(0) =
        cast_form(args.at(0), is_timeframe ? TypeSpec("time-frame") : TypeSpec("int"), pool, env);
  }

  if (!arg1_i) {
    args.at(1) =
        cast_form(args.at(1), is_timeframe ? TypeSpec("time-frame") : TypeSpec("int"), pool, env);
  }

  auto new_form =
      pool.alloc_element<GenericElement>(GenericOperator::make_fixed(kind), args.at(0), args.at(1));
  result->push_back(new_form);
}

void SimpleExpressionElement::update_from_stack_force_ui_2(const Env& env,
                                                           FixedOperatorKind kind,
                                                           FormPool& pool,
                                                           FormStack& stack,
                                                           std::vector<FormElement*>* result,
                                                           bool allow_side_effects) {
  bool arg0_constant = !m_expr.get_arg(0).is_var();
  bool arg0_u;
  if (!arg0_constant) {
    arg0_u = is_uint_type(env, m_my_idx, m_expr.get_arg(0).var());
  } else {
    arg0_u = m_expr.get_arg(0).is_int();
  }
  bool arg1_u = true;
  bool arg1_reg = m_expr.get_arg(1).is_var();
  if (arg1_reg) {
    arg1_u = is_uint_type(env, m_my_idx, m_expr.get_arg(1).var());
  } else {
    ASSERT(m_expr.get_arg(1).is_int());
  }

  std::vector<Form*> args;
  if (arg0_constant) {
    args = pop_to_forms({m_expr.get_arg(1).var()}, env, pool, stack, allow_side_effects);
    args.push_back(pool.form<SimpleAtomElement>(m_expr.get_arg(0)));
  } else {
    if (arg1_reg) {
      args = pop_to_forms({m_expr.get_arg(0).var(), m_expr.get_arg(1).var()}, env, pool, stack,
                          allow_side_effects);
    } else {
      args = pop_to_forms({m_expr.get_arg(0).var()}, env, pool, stack, allow_side_effects);
      args.push_back(pool.form<SimpleAtomElement>(m_expr.get_arg(1)));
    }
  }

  if (!arg0_u) {
    args.at(0) = pool.form<CastElement>(TypeSpec("uint"), args.at(0));
  }

  if (!arg1_u) {
    args.at(1) = pool.form<CastElement>(TypeSpec("uint"), args.at(1));
  }

  auto new_form =
      pool.alloc_element<GenericElement>(GenericOperator::make_fixed(kind), args.at(0), args.at(1));
  result->push_back(new_form);
}

void SimpleExpressionElement::update_from_stack_pcypld(const Env& env,
                                                       FormPool& pool,
                                                       FormStack& stack,
                                                       std::vector<FormElement*>* result,
                                                       bool allow_side_effects) {
  std::vector<Form*> args;
  std::vector<RegisterAccess> ras;
  for (int arg_idx = 0; arg_idx < m_expr.args(); arg_idx++) {
    if (m_expr.get_arg(arg_idx).is_var()) {
      ras.push_back(m_expr.get_arg(arg_idx).var());
    }
  }
  auto popped_args = pop_to_forms(ras, env, pool, stack, allow_side_effects);

  int ras_idx = 0;
  for (int arg_idx = 0; arg_idx < m_expr.args(); arg_idx++) {
    if (m_expr.get_arg(arg_idx).is_var()) {
      args.push_back(popped_args.at(ras_idx));
      ras_idx++;
    } else {
      args.push_back(pool.form<SimpleAtomElement>(m_expr.get_arg(arg_idx)));
    }
  }

  /*
    pcpyud v1, s4, r0
    ld a0, L152(fp)
    and v1, v1, a0
    lui a0, 1
    dsll32 a0, a0, 0
    or v1, v1, a0
    pcpyld v1, v1, s4
    por s4, v1, r0
   */

  auto as_mod = args.at(0)->try_as_element<ModifiedCopyBitfieldElement>();
  if (as_mod && as_mod->from_pcpyud()) {
    auto base_form = as_mod->base()->to_form(env);
    auto a1_form = args.at(1)->to_form(env);
    if (base_form == a1_form) {
      as_mod->clear_pcpyud_flag();
      result->push_back(as_mod);
      return;
    } else {
      lg::warn("pcpyud rewrite form fail: {} {}", base_form.print(), a1_form.print());
    }
  }
  auto new_form = pool.alloc_element<GenericElement>(
      GenericOperator::make_fixed(FixedOperatorKind::PCPYLD), args.at(0), args.at(1));
  result->push_back(new_form);
}

void SimpleExpressionElement::update_from_stack_vector_plus_minus_cross(
    FixedOperatorKind op_kind,
    const Env& env,
    FormPool& pool,
    FormStack& stack,
    std::vector<FormElement*>* result,
    bool allow_side_effects) {
  std::vector<Form*> popped_args =
      pop_to_forms({m_expr.get_arg(0).var(), m_expr.get_arg(1).var(), m_expr.get_arg(2).var()}, env,
                   pool, stack, allow_side_effects);

  for (int i = 0; i < 3; i++) {
    auto arg_type = env.get_types_before_op(m_my_idx).get(m_expr.get_arg(i).var().reg());
    if (arg_type.typespec() != TypeSpec("vector")) {
      popped_args.at(i) = cast_form(popped_args.at(i), TypeSpec("vector"), pool, env);
    }
  }

  auto new_form = pool.alloc_element<GenericElement>(
      GenericOperator::make_fixed(op_kind),
      std::vector<Form*>{popped_args.at(0), popped_args.at(1), popped_args.at(2)});
  result->push_back(new_form);
}

void SimpleExpressionElement::update_from_stack_vector_plus_float_times(
    const Env& env,
    FormPool& pool,
    FormStack& stack,
    std::vector<FormElement*>* result,
    bool allow_side_effects) {
  std::vector<Form*> popped_args = pop_to_forms({m_expr.get_arg(0).var(), m_expr.get_arg(1).var(),
                                                 m_expr.get_arg(2).var(), m_expr.get_arg(3).var()},
                                                env, pool, stack, allow_side_effects);

  for (int i = 0; i < 4; i++) {
    auto arg_type = env.get_types_before_op(m_my_idx).get(m_expr.get_arg(i).var().reg());
    TypeSpec desired_type(i == 3 ? "float" : "vector");
    if (arg_type.typespec() != desired_type) {
      popped_args.at(i) = cast_form(popped_args.at(i), desired_type, pool, env);
    }
  }

  auto new_form = pool.alloc_element<GenericElement>(
      GenericOperator::make_fixed(FixedOperatorKind::VECTOR_PLUS_FLOAT_TIMES),
      std::vector<Form*>{popped_args.at(0), popped_args.at(1), popped_args.at(2),
                         popped_args.at(3)});
  result->push_back(new_form);
}

void SimpleExpressionElement::update_from_stack_vector_float_product(
    const Env& env,
    FormPool& pool,
    FormStack& stack,
    std::vector<FormElement*>* result,
    bool allow_side_effects) {
  std::vector<Form*> popped_args =
      pop_to_forms({m_expr.get_arg(0).var(), m_expr.get_arg(1).var(), m_expr.get_arg(2).var()}, env,
                   pool, stack, allow_side_effects);

  for (int i = 0; i < 3; i++) {
    auto arg_type = env.get_types_before_op(m_my_idx).get(m_expr.get_arg(i).var().reg());
    TypeSpec desired_type(i == 2 ? "float" : "vector");
    if (arg_type.typespec() != desired_type) {
      popped_args.at(i) = cast_form(popped_args.at(i), desired_type, pool, env);
    }
  }

  auto new_form = pool.alloc_element<GenericElement>(
      GenericOperator::make_fixed(FixedOperatorKind::VECTOR_FLOAT_PRODUCT),
      std::vector<Form*>{popped_args.at(0), popped_args.at(1), popped_args.at(2)});
  result->push_back(new_form);
}

void SimpleExpressionElement::update_from_stack_vectors_in_common(FixedOperatorKind kind,
                                                                  const Env& env,
                                                                  FormPool& pool,
                                                                  FormStack& stack,
                                                                  std::vector<FormElement*>* result,
                                                                  bool allow_side_effects) {
  std::vector<RegisterAccess> register_acccesses;
  for (int arg_idx = 0; arg_idx < m_expr.args(); arg_idx++) {
    register_acccesses.push_back(m_expr.get_arg(arg_idx).var());
  }
  std::vector<Form*> popped_args =
      pop_to_forms(register_acccesses, env, pool, stack, allow_side_effects);

  for (int i = 0; i < m_expr.args(); i++) {
    auto arg_type = env.get_types_before_op(m_my_idx).get(m_expr.get_arg(i).var().reg());
    if (arg_type.typespec() != TypeSpec("vector")) {
      popped_args.at(i) = cast_form(popped_args.at(i), TypeSpec("vector"), pool, env);
    }
  }

  auto new_form =
      pool.alloc_element<GenericElement>(GenericOperator::make_fixed(kind), popped_args);
  result->push_back(new_form);
}

void SimpleExpressionElement::update_from_stack_copy_first_int_2(const Env& env,
                                                                 FixedOperatorKind kind,
                                                                 FormPool& pool,
                                                                 FormStack& stack,
                                                                 std::vector<FormElement*>* result,
                                                                 bool allow_side_effects) {
  auto arg0_type = env.get_variable_type(m_expr.get_arg(0).var(), true);
  auto arg0_i = is_int_type(arg0_type);
  auto arg0_u = is_uint_type(arg0_type);
  if (!m_expr.get_arg(1).is_var()) {
    auto args = pop_to_forms({m_expr.get_arg(0).var()}, env, pool, stack, allow_side_effects);

    if (!arg0_i && !arg0_u) {
      auto bti = dynamic_cast<EnumType*>(env.dts->ts.lookup_type(arg0_type));
      if (bti) {
        auto new_form = pool.alloc_element<GenericElement>(
            GenericOperator::make_fixed(kind), args.at(0),
            cast_form(pool.form<SimpleAtomElement>(m_expr.get_arg(1)), arg0_type, pool, env));
        result->push_back(new_form);
      } else {
        auto new_form = pool.alloc_element<GenericElement>(
            GenericOperator::make_fixed(kind), pool.form<CastElement>(TypeSpec("int"), args.at(0)),
            pool.form<SimpleAtomElement>(m_expr.get_arg(1)));
        result->push_back(new_form);
      }
    } else {
      auto new_form =
          pool.alloc_element<GenericElement>(GenericOperator::make_fixed(kind), args.at(0),
                                             pool.form<SimpleAtomElement>(m_expr.get_arg(1)));
      result->push_back(new_form);
    }

    return;
  }
  auto arg1_type = env.get_types_before_op(m_my_idx).get(m_expr.get_arg(1).var().reg()).typespec();
  auto arg1_i = is_int_type(arg1_type);
  auto arg1_u = is_uint_type(arg1_type);

  auto args = pop_to_forms({m_expr.get_arg(0).var(), m_expr.get_arg(1).var()}, env, pool, stack,
                           allow_side_effects);

  if ((arg0_i && arg1_i) || (arg0_u && arg1_u)) {
    if (kind == FixedOperatorKind::SUBTRACTION && arg0_i) {
      if (arg0_type == TypeSpec("time-frame") && arg1_type != TypeSpec("time-frame")) {
        args.at(1) = cast_form(args.at(1), TypeSpec("time-frame"), pool, env);
      } else if (arg1_type == TypeSpec("time-frame") && arg0_type != TypeSpec("time-frame")) {
        args.at(0) = cast_form(args.at(0), TypeSpec("time-frame"), pool, env);
      }
    }
    auto new_form = pool.alloc_element<GenericElement>(GenericOperator::make_fixed(kind),
                                                       args.at(0), args.at(1));
    result->push_back(new_form);
  } else {
    auto cast = pool.form<CastElement>(TypeSpec(arg0_i ? "int" : "uint"), args.at(1));
    if (kind == FixedOperatorKind::SUBTRACTION &&
        is_pointer_type(env, m_my_idx, m_expr.get_arg(0).var())) {
      kind = FixedOperatorKind::SUBTRACTION_PTR;
    }
    auto new_form =
        pool.alloc_element<GenericElement>(GenericOperator::make_fixed(kind), args.at(0), cast);
    result->push_back(new_form);
  }
}

namespace {
Form* strip_int_or_uint_cast(Form* in) {
  auto as_cast = in->try_as_element<CastElement>();
  if (as_cast && (as_cast->type() == TypeSpec("int") || as_cast->type() == TypeSpec("uint"))) {
    return as_cast->source();
  }
  return in;
}
}  // namespace

FormElement* SimpleExpressionElement::update_from_stack_logor_or_logand_helper(
    const Env& env,
    FixedOperatorKind kind,
    FormPool& pool,
    FormStack& stack,
    bool allow_side_effects) {
  // grab the normal variable type
  TypeSpec arg0_type;
  if (m_expr.get_arg(0).is_var()) {
    arg0_type = env.get_variable_type(m_expr.get_arg(0).var(), true);
  } else if (m_expr.get_arg(0).is_int(0)) {
    arg0_type = TypeSpec("int");  // ??
  } else {
    ASSERT(false);
  }

  // and try to get it as a bitfield
  auto type_info = env.dts->ts.lookup_type(arg0_type);
  auto bitfield_info = dynamic_cast<BitFieldType*>(type_info);
  bool had_pcpyud = false;
  TypeSpec bitfield_type = arg0_type;

  if (!bitfield_info && m_expr.get_arg(0).is_var()) {
    // the above won't work if we're already done a pcpyud to grab the upper 64 bits.
    // we need to grab the type in the register (a TP_type) and check
    const auto& arg0_reg_type =
        env.get_types_before_op(m_expr.get_arg(0).var().idx()).get(m_expr.get_arg(0).var().reg());
    if (arg0_reg_type.kind == TP_Type::Kind::PCPYUD_BITFIELD) {
      // yes!
      had_pcpyud = true;
      bitfield_info =
          dynamic_cast<BitFieldType*>(env.dts->ts.lookup_type(arg0_reg_type.get_bitfield_type()));
      ASSERT(bitfield_info);
    } else if (arg0_reg_type.kind == TP_Type::Kind::PCPYUD_BITFIELD_AND) {
      // already have the pcpyud in the thing.
      bitfield_info =
          dynamic_cast<BitFieldType*>(env.dts->ts.lookup_type(arg0_reg_type.get_bitfield_type()));
      ASSERT(bitfield_info);
    }
  }

  if (bitfield_info && arg0_type.base_type() != "time-frame" && m_expr.get_arg(1).is_int()) {
    // andi, ori with bitfield.
    auto base = pop_to_forms({m_expr.get_arg(0).var()}, env, pool, stack, allow_side_effects).at(0);
    auto read_elt = dynamic_cast<BitfieldAccessElement*>(base->try_as_single_element());
    if (!read_elt) {
      read_elt = pool.alloc_element<BitfieldAccessElement>(base, bitfield_type);
      ASSERT(!had_pcpyud);
    } else {
      if (had_pcpyud) {
        ASSERT(read_elt->has_pcpyud());
      }
    }

    BitfieldManip::Kind manip_kind;
    if (kind == FixedOperatorKind::LOGAND) {
      manip_kind = BitfieldManip::Kind::LOGAND_WITH_CONSTANT_INT;
    } else if (kind == FixedOperatorKind::LOGIOR) {
      manip_kind = BitfieldManip::Kind::LOGIOR_WITH_CONSTANT_INT;
    } else {
      ASSERT(false);
    }

    BitfieldManip step(manip_kind, m_expr.get_arg(1).get_int());
    auto other = read_elt->push_step(step, env.dts->ts, pool, env);
    if (other) {
      return other;
    } else {
      return read_elt;
    }

  } else if (!m_expr.get_arg(1).is_var()) {
    // andi, something else (don't think this can happen?)
    std::vector<FormElement*> result;
    update_from_stack_copy_first_int_2(env, kind, pool, stack, &result, allow_side_effects);
    ASSERT(result.size() == 1);
    return result.at(0);
  } else {
    // and, two forms
    auto arg1_type = env.get_variable_type(m_expr.get_arg(1).var(), true);
    auto arg0_i =
        m_expr.get_arg(0).is_var() ? is_int_type(env, m_my_idx, m_expr.get_arg(0).var()) : true;
    auto arg0_u =
        m_expr.get_arg(0).is_var() ? is_uint_type(env, m_my_idx, m_expr.get_arg(0).var()) : false;
    auto arg1_i = is_int_type(env, m_my_idx, m_expr.get_arg(1).var());
    auto arg1_u = is_uint_type(env, m_my_idx, m_expr.get_arg(1).var());
    auto arg0_n = arg0_i || arg0_u;
    auto arg1_n = arg1_i || arg1_u;

    std::vector<Form*> args;

    if (m_expr.get_arg(0).is_var()) {
      args = pop_to_forms({m_expr.get_arg(0).var(), m_expr.get_arg(1).var()}, env, pool, stack,
                          allow_side_effects);
    } else {
      args = pop_to_forms({m_expr.get_arg(1).var()}, env, pool, stack, allow_side_effects);
      args.insert(args.begin(), pool.form<SimpleAtomElement>(
                                    SimpleAtom::make_int_constant(m_expr.get_arg(0).get_int())));
    }

    if (bitfield_info) {
      // either the immediate didn't fit in the 16-bit imm or it's with a variable
      bool made_new_read_elt = false;
      auto read_elt = dynamic_cast<BitfieldAccessElement*>(args.at(0)->try_as_single_element());
      if (!read_elt) {
        read_elt = pool.alloc_element<BitfieldAccessElement>(args.at(0), bitfield_type);
        made_new_read_elt = true;
        ASSERT(!had_pcpyud);
      } else {
        if (had_pcpyud) {
          ASSERT(read_elt->has_pcpyud());
        }
      }

      auto stripped_arg1 = strip_int_or_uint_cast(args.at(1));
      // auto arg1_atom = form_as_atom(strip_int_or_uint_cast(args.at(1)));
      auto arg1_as_int = get_goal_integer_constant(stripped_arg1, env);
      if (arg1_as_int) {
        BitfieldManip::Kind manip_kind;
        if (kind == FixedOperatorKind::LOGAND) {
          manip_kind = BitfieldManip::Kind::LOGAND_WITH_CONSTANT_INT;
        } else if (kind == FixedOperatorKind::LOGIOR) {
          manip_kind = BitfieldManip::Kind::LOGIOR_WITH_CONSTANT_INT;
        } else {
          ASSERT(false);
        }
        BitfieldManip step(manip_kind, *arg1_as_int);
        auto other = read_elt->push_step(step, env.dts->ts, pool, env);
        // ASSERT(!other);  // shouldn't be complete.
        if (other) {
          return other;
        } else {
          return read_elt;
        }
      } else if (!made_new_read_elt) {
        BitfieldManip::Kind manip_kind;
        if (kind == FixedOperatorKind::LOGAND) {
          manip_kind = BitfieldManip::Kind::LOGAND_WITH_FORM;
        } else if (kind == FixedOperatorKind::LOGIOR) {
          manip_kind = BitfieldManip::Kind::LOGIOR_WITH_FORM;
        } else {
          ASSERT(false);
        }
        auto step = BitfieldManip::from_form(manip_kind, stripped_arg1);
        auto other = read_elt->push_step(step, env.dts->ts, pool, env);
        if (other) {
          return other;
        } else {
          return read_elt;
        }
      }
    }

    if (((arg0_i || arg0_u) && (arg1_i || arg1_u)) ||
        (arg0_n && arg1_type.base_type() == "pointer") ||
        (arg1_n && arg0_type.base_type() == "pointer")) {
      // types already good
      // we also allow (logand intvar pointer) and (logand pointer intvar)
      auto new_form = pool.alloc_element<GenericElement>(GenericOperator::make_fixed(kind),
                                                         args.at(0), args.at(1));
      return new_form;
      // types bad, insert cast.
    } else {
      // this is an ugly hack to make (logand (lognot (enum-bitfield xxxx)) work.
      // I have only one example for this, so I think this unlikely to work in all cases.
      if (m_expr.get_arg(1).is_var()) {
        auto eti = env.dts->ts.try_enum_lookup(arg1_type.base_type());
        if (eti) {
          auto integer = get_goal_integer_constant(strip_int_or_uint_cast(args.at(0)), env);
          if (integer && ((s64)*integer) < 0) {
            // clearing a bitfield.
            auto elts = decompile_bitfield_enum_from_int(arg1_type, env.dts->ts, ~*integer);
            auto oper = GenericOperator::make_function(
                pool.form<ConstantTokenElement>(arg1_type.base_type()));
            std::vector<Form*> form_elts;
            for (auto& x : elts) {
              form_elts.push_back(pool.form<ConstantTokenElement>(x));
            }
            auto inverted = pool.form<GenericElement>(oper, form_elts);
            auto normal = pool.form<GenericElement>(
                GenericOperator::make_fixed(FixedOperatorKind::LOGNOT), inverted);
            auto new_form = pool.alloc_element<GenericElement>(GenericOperator::make_fixed(kind),
                                                               normal, args.at(1));
            return new_form;
          }
        }
      }

      bool arg0_int_like = env.dts->ts.tc(TypeSpec("integer"), arg0_type);
      bool arg1_int_like = env.dts->ts.tc(TypeSpec("integer"), arg1_type);

      if ((arg0_int_like) && (arg1_int_like)) {
        // we might have a bitfield in arg1.
        auto arg1_bitfield = dynamic_cast<BitFieldType*>(env.dts->ts.lookup_type(arg1_type));
        auto arg1_enum = dynamic_cast<EnumType*>(env.dts->ts.lookup_type(arg1_type));
        if ((arg1_bitfield || arg1_enum) && arg0_type != arg1_type) {
          auto new_form = pool.alloc_element<GenericElement>(
              GenericOperator::make_fixed(kind), cast_form(args.at(0), arg1_type, pool, env),
              args.at(1));
          return new_form;
        } else {
          auto new_form = pool.alloc_element<GenericElement>(GenericOperator::make_fixed(kind),
                                                             args.at(0), args.at(1));
          return new_form;
        }
      }
      // types bad, insert cast.
      auto cast = pool.form<CastElement>(TypeSpec(arg0_i ? "int" : "uint"), args.at(1));
      auto new_form =
          pool.alloc_element<GenericElement>(GenericOperator::make_fixed(kind), args.at(0), cast);
      return new_form;
    }
  }
}

void SimpleExpressionElement::update_from_stack_logor_or_logand(const Env& env,
                                                                FixedOperatorKind kind,
                                                                FormPool& pool,
                                                                FormStack& stack,
                                                                std::vector<FormElement*>* result,
                                                                bool allow_side_effects) {
  auto element =
      update_from_stack_logor_or_logand_helper(env, kind, pool, stack, allow_side_effects);

  /*
     (defmacro logclear (a b)
       "Returns the result of setting the bits in b to zero in a"
       `(logand (lognot ,b) ,a)
       )
   */

  constexpr int a_form = 0;
  constexpr int b_form = 1;

  auto lognot_submatcher =
      Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::LOGNOT), {Matcher::any(b_form)});
  auto lognot_submatchers =
      Matcher::match_or({Matcher::cast("uint", lognot_submatcher),
                         Matcher::cast("int", lognot_submatcher), lognot_submatcher});

  auto logclear_matcher =
      Matcher::match_or({Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::LOGAND),
                                     {lognot_submatchers, Matcher::any(a_form)}),
                         Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::LOGAND),
                                     {Matcher::any(a_form), lognot_submatchers})});

  auto mr = match(logclear_matcher, element);
  if (mr.matched) {
    result->push_back(pool.alloc_element<GenericElement>(
        GenericOperator::make_fixed(FixedOperatorKind::LOGCLEAR),
        std::vector<Form*>{mr.maps.forms.at(a_form), mr.maps.forms.at(b_form)}));

    return;
  }

  // (-> (the-as process-drawable (-> v1-32 0)) pid)
  // (-> v1-61 0 pid)
  auto just_deref_matcher = Matcher::match_or(
      {Matcher::deref(Matcher::any_reg(0), false,
                      {DerefTokenMatcher::integer(0), DerefTokenMatcher::string("pid")}),
       Matcher::deref({Matcher::cast_to_any(4, Matcher::deref(Matcher::any_reg(0), false,
                                                              {DerefTokenMatcher::integer(0)}))},
                      false, {DerefTokenMatcher::string("pid")})});

  // jak 1:
  // (logior (shl (-> v1-61 0 pid) 32) (.asm.sllv.r0 v1-61))
  // jak 2:
  // (logior (if v1-61 (shl (-> v1-61 0 pid) 32) 0) (.asm.sllv.r0 v1-61))
  auto pid_deref_matcher =
      Matcher::op_fixed(FixedOperatorKind::SHL, {just_deref_matcher, Matcher::integer(32)});

  auto make_handle_matcher = Matcher::op_fixed(
      FixedOperatorKind::LOGIOR,
      {env.version == GameVersion::Jak1
           ? pid_deref_matcher
           : Matcher::if_with_else(
                 Matcher::op(GenericOpMatcher::condition(IR2_Condition::Kind::TRUTHY),
                             {Matcher::any_reg(2)}),
                 pid_deref_matcher, Matcher::integer(0)),
       Matcher::op_fixed(FixedOperatorKind::ASM_SLLV_R0, {Matcher::any_reg(1)})});

  auto handle_mr = match(make_handle_matcher, element);

  if (handle_mr.matched) {
    auto var_a = handle_mr.maps.regs.at(0).value();
    auto var_b = handle_mr.maps.regs.at(1).value();
    const auto& var_name = env.get_variable_name(var_a);
    if (var_name == env.get_variable_name(var_b) &&
        (env.version == GameVersion::Jak1 ||
         var_name == env.get_variable_name(handle_mr.maps.regs.at(2).value())) &&
        env.dts->ts.tc(TypeSpec("pointer", {TypeSpec("process-tree")}),
                       env.get_variable_type(var_a, true))) {
      auto* menv = const_cast<Env*>(&env);
      menv->disable_use(var_a);

      auto repopped = stack.pop_reg(var_b, {}, env, true, stack.size() - 1);

      if (!repopped) {
        lg::warn("repop failed.\n{}", stack.print(env));
        repopped = var_to_form(var_b, pool);
      }

      auto proc_to_ppointer_matcher =
          Matcher::op_fixed(FixedOperatorKind::PROCESS_TO_PPOINTER, {Matcher::any(0)});
      auto proc_to_ppointer_mr = match(proc_to_ppointer_matcher, repopped);
      if (proc_to_ppointer_mr.matched) {
        element = pool.alloc_element<GenericElement>(
            GenericOperator::make_fixed(FixedOperatorKind::PROCESS_TO_HANDLE),
            proc_to_ppointer_mr.maps.forms.at(0));
      } else {
        element = pool.alloc_element<GenericElement>(
            GenericOperator::make_fixed(FixedOperatorKind::PPOINTER_TO_HANDLE), repopped);
      }
    }
  }

  result->push_back(element);
}

void SimpleExpressionElement::update_from_stack_left_shift(const Env& env,
                                                           FormPool& pool,
                                                           FormStack& stack,
                                                           std::vector<FormElement*>* result,
                                                           bool allow_side_effects) {
  TypeSpec arg0_type;
  auto& arg0 = m_expr.get_arg(0);
  if (arg0.is_var()) {
    arg0_type = env.get_variable_type(m_expr.get_arg(0).var(), true);
    auto type_info = env.dts->ts.lookup_type(arg0_type);
    auto bitfield_info = dynamic_cast<BitFieldType*>(type_info);
    if (arg0_type.base_type() != "time-frame" && bitfield_info && m_expr.get_arg(1).is_int()) {
      auto base =
          pop_to_forms({m_expr.get_arg(0).var()}, env, pool, stack, allow_side_effects).at(0);
      auto read_elt = pool.alloc_element<BitfieldAccessElement>(base, arg0_type);
      BitfieldManip step(BitfieldManip::Kind::LEFT_SHIFT, m_expr.get_arg(1).get_int());
      auto other = read_elt->push_step(step, env.dts->ts, pool, env);
      ASSERT(!other);  // shouldn't be complete.
      result->push_back(read_elt);
    } else {
      // try to turn this into a multiplication, if possible
      if (m_expr.get_arg(1).is_int()) {
        auto args = pop_to_forms({m_expr.get_arg(0).var()}, env, pool, stack, allow_side_effects);
        int sa = m_expr.get_arg(1).get_int();

        auto as_ba = args.at(0)->try_as_element<BitfieldAccessElement>();
        if (as_ba) {
          BitfieldManip step(BitfieldManip::Kind::LEFT_SHIFT, m_expr.get_arg(1).get_int());
          auto other = as_ba->push_step(step, env.dts->ts, pool, env);
          ASSERT(!other);  // shouldn't be complete.
          result->push_back(as_ba);
          return;
        }

        // somewhat arbitrary threshold to switch from multiplications to shift.
        if (sa < 10) {
          s64 multiplier = (s64(1) << sa);

          auto new_form = pool.alloc_element<GenericElement>(
              GenericOperator::make_fixed(FixedOperatorKind::MULTIPLICATION), args.at(0),
              pool.form<SimpleAtomElement>(SimpleAtom::make_int_constant(multiplier)));
          result->push_back(new_form);
          return;
        }

        auto arg0_i = is_int_type(env, m_my_idx, m_expr.get_arg(0).var());
        auto arg0_u = is_uint_type(env, m_my_idx, m_expr.get_arg(0).var());
        if (!arg0_i && !arg0_u) {
          auto new_form = pool.alloc_element<GenericElement>(
              GenericOperator::make_fixed(FixedOperatorKind::SHL),
              pool.form<CastElement>(TypeSpec("int"), args.at(0)),
              pool.form<SimpleAtomElement>(m_expr.get_arg(1)));
          result->push_back(new_form);
        } else {
          auto new_form = pool.alloc_element<GenericElement>(
              GenericOperator::make_fixed(FixedOperatorKind::SHL), args.at(0),
              pool.form<SimpleAtomElement>(m_expr.get_arg(1)));
          result->push_back(new_form);
        }

        return;
      }

      update_from_stack_copy_first_int_2(env, FixedOperatorKind::SHL, pool, stack, result,
                                         allow_side_effects);
    }
  } else if ((arg0.is_sym_val("#f") || arg0.is_sym_ptr("#f")) && m_expr.get_arg(1).is_int()) {
    auto new_form = pool.alloc_element<GenericElement>(
        GenericOperator::make_fixed(FixedOperatorKind::SHL),
        pool.form<CastElement>(TypeSpec("int"), pool.form<SimpleAtomElement>(m_expr.get_arg(0))),
        pool.form<SimpleAtomElement>(m_expr.get_arg(1)));
    result->push_back(new_form);
    return;

  } else {
    ASSERT(false);
  }
}

void SimpleExpressionElement::update_from_stack_right_shift_logic(const Env& env,
                                                                  FormPool& pool,
                                                                  FormStack& stack,
                                                                  std::vector<FormElement*>* result,
                                                                  bool allow_side_effects) {
  auto arg0_type = env.get_variable_type(m_expr.get_arg(0).var(), true);
  auto type_info = env.dts->ts.lookup_type(arg0_type);
  auto bitfield_info = dynamic_cast<BitFieldType*>(type_info);
  if (bitfield_info && m_expr.get_arg(1).is_int()) {
    auto base = pop_to_forms({m_expr.get_arg(0).var()}, env, pool, stack, allow_side_effects).at(0);
    auto read_elt = pool.alloc_element<BitfieldAccessElement>(base, arg0_type);
    BitfieldManip step(BitfieldManip::Kind::RIGHT_SHIFT_LOGICAL, m_expr.get_arg(1).get_int());
    auto other = read_elt->push_step(step, env.dts->ts, pool, env);
    ASSERT(other);  // should be a high field.
    result->push_back(other);
  } else {
    auto arg0_i = is_int_type(env, m_my_idx, m_expr.get_arg(0).var());
    auto arg0_u = is_uint_type(env, m_my_idx, m_expr.get_arg(0).var());
    if (m_expr.get_arg(1).is_int()) {
      auto arg =
          pop_to_forms({m_expr.get_arg(0).var()}, env, pool, stack, allow_side_effects).at(0);
      auto as_bitfield_access = dynamic_cast<BitfieldAccessElement*>(arg->try_as_single_element());

      if (as_bitfield_access) {
        BitfieldManip step(BitfieldManip::Kind::RIGHT_SHIFT_LOGICAL, m_expr.get_arg(1).get_int());
        auto next = as_bitfield_access->push_step(step, env.dts->ts, pool, env);
        if (next) {
          result->push_back(next);
        } else {
          result->push_back(as_bitfield_access);
        }
      } else {
        /*
        int sa = m_expr.get_arg(1).get_int();
        if (sa < 10) {
          if (!arg0_u) {
            arg = cast_form(arg, TypeSpec("uint"), pool, env);
          }
          s64 multiplier = (s64(1) << sa);

          auto new_form = pool.alloc_element<GenericElement>(
              GenericOperator::make_fixed(FixedOperatorKind::DIVISION), arg,
              pool.alloc_single_element_form<SimpleAtomElement>(
                  nullptr, SimpleAtom::make_int_constant(multiplier)));
          result->push_back(new_form);
          return;
        }
         */

        if (!arg0_i && !arg0_u) {
          auto new_form = pool.alloc_element<GenericElement>(
              GenericOperator::make_fixed(FixedOperatorKind::SHR),
              pool.form<CastElement>(TypeSpec("int"), arg),
              pool.form<SimpleAtomElement>(m_expr.get_arg(1)));
          result->push_back(new_form);
        } else {
          auto new_form = pool.alloc_element<GenericElement>(
              GenericOperator::make_fixed(FixedOperatorKind::SHR), arg,
              pool.form<SimpleAtomElement>(m_expr.get_arg(1)));
          result->push_back(new_form);
        }
      }
    } else {
      update_from_stack_copy_first_int_2(env, FixedOperatorKind::SHR, pool, stack, result,
                                         allow_side_effects);
    }
  }
}

void SimpleExpressionElement::update_from_stack_right_shift_arith(const Env& env,
                                                                  FormPool& pool,
                                                                  FormStack& stack,
                                                                  std::vector<FormElement*>* result,
                                                                  bool allow_side_effects) {
  auto arg0_type = env.get_variable_type(m_expr.get_arg(0).var(), true);
  auto type_info = env.dts->ts.lookup_type(arg0_type);
  auto bitfield_info = dynamic_cast<BitFieldType*>(type_info);
  if (bitfield_info && arg0_type != TypeSpec("time-frame") && m_expr.get_arg(1).is_int()) {
    auto base = pop_to_forms({m_expr.get_arg(0).var()}, env, pool, stack, allow_side_effects).at(0);
    auto read_elt = pool.alloc_element<BitfieldAccessElement>(base, arg0_type);
    BitfieldManip step(BitfieldManip::Kind::RIGHT_SHIFT_ARITH, m_expr.get_arg(1).get_int());
    auto other = read_elt->push_step(step, env.dts->ts, pool, env);
    ASSERT(other);  // should be a high field.
    result->push_back(other);
  } else {
    if (m_expr.get_arg(1).is_int()) {
      if (m_expr.get_arg(1).get_int() < 10) {
        auto arg0_i = is_int_type(env, m_my_idx, m_expr.get_arg(0).var());
        auto arg =
            pop_to_forms({m_expr.get_arg(0).var()}, env, pool, stack, allow_side_effects).at(0);
        int sa = m_expr.get_arg(1).get_int();

        if (!arg0_i) {
          arg = cast_form(arg, TypeSpec("int"), pool, env);
        }
        s64 multiplier = (s64(1) << sa);

        auto new_form = pool.alloc_element<GenericElement>(
            GenericOperator::make_fixed(FixedOperatorKind::DIVISION), arg,
            pool.form<SimpleAtomElement>(SimpleAtom::make_int_constant(multiplier)));
        result->push_back(new_form);
        return;
      }

      auto arg0_i = is_int_type(env, m_my_idx, m_expr.get_arg(0).var());
      auto arg0_u = is_uint_type(env, m_my_idx, m_expr.get_arg(0).var());
      auto args = pop_to_forms({m_expr.get_arg(0).var()}, env, pool, stack, allow_side_effects);

      auto as_ba = args.at(0)->try_as_element<BitfieldAccessElement>();
      if (as_ba) {
        BitfieldManip step(BitfieldManip::Kind::RIGHT_SHIFT_ARITH, m_expr.get_arg(1).get_int());
        auto other = as_ba->push_step(step, env.dts->ts, pool, env);
        ASSERT(other);  // should be a high field.
        result->push_back(other);
        return;
      }

      if (!arg0_i && !arg0_u) {
        auto new_form =
            pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::SAR),
                                               pool.form<CastElement>(TypeSpec("int"), args.at(0)),
                                               pool.form<SimpleAtomElement>(m_expr.get_arg(1)));
        result->push_back(new_form);
      } else {
        auto new_form = pool.alloc_element<GenericElement>(
            GenericOperator::make_fixed(FixedOperatorKind::SAR), args.at(0),
            pool.form<SimpleAtomElement>(m_expr.get_arg(1)));
        result->push_back(new_form);
      }

      return;
    }

    update_from_stack_copy_first_int_2(env, FixedOperatorKind::SAR, pool, stack, result,
                                       allow_side_effects);
  }
}

void SimpleExpressionElement::update_from_stack_lognot(const Env& env,
                                                       FormPool& pool,
                                                       FormStack& stack,
                                                       std::vector<FormElement*>* result,
                                                       bool allow_side_effects) {
  auto args = pop_to_forms({m_expr.get_arg(0).var()}, env, pool, stack, allow_side_effects);
  auto new_form = pool.alloc_element<GenericElement>(
      GenericOperator::make_fixed(FixedOperatorKind::LOGNOT), args.at(0));
  result->push_back(new_form);
}

void SimpleExpressionElement::update_from_stack_int_to_float(const Env& env,
                                                             FormPool& pool,
                                                             FormStack& stack,
                                                             std::vector<FormElement*>* result,
                                                             bool allow_side_effects) {
  auto var = m_expr.get_arg(0).var();
  auto arg = pop_to_forms({var}, env, pool, stack, allow_side_effects).at(0);
  // if we convert from a GPR to FPR, then immediately to int to float, we can strip away the
  // the gpr->fpr operation beacuse it doesn't matter.
  auto fpr_convert_matcher =
      Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::GPR_TO_FPR), {Matcher::any(0)});
  auto type = env.get_types_before_op(var.idx()).get(var.reg()).typespec();
  // want to allow any child of integer so integer enums can also be converted to floats.
  if (env.dts->ts.tc(TypeSpec("integer"), type) || type == TypeSpec("seconds")) {
    auto mr = match(fpr_convert_matcher, arg);
    if (mr.matched) {
      arg = mr.maps.forms.at(0);
    }
    result->push_back(pool.alloc_element<CastElement>(TypeSpec("float"), arg, true));
  } else {
    throw std::runtime_error(fmt::format("At op {}, used int to float on a {} from {}: {}",
                                         m_my_idx, type.print(), var.to_form(env).print(),
                                         arg->to_string(env)));
  }
}

void SimpleExpressionElement::update_from_stack_float_to_int(const Env& env,
                                                             FormPool& pool,
                                                             FormStack& stack,
                                                             std::vector<FormElement*>* result,
                                                             bool allow_side_effects) {
  auto var = m_expr.get_arg(0).var();
  auto arg = pop_to_forms({var}, env, pool, stack, allow_side_effects).at(0);
  auto type = env.get_types_before_op(var.idx()).get(var.reg()).typespec();
  auto fpr_convert_matcher =
      Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::GPR_TO_FPR), {Matcher::any(0)});
  auto mr = match(fpr_convert_matcher, arg);
  if (type == TypeSpec("float")) {
    result->push_back(pool.alloc_element<CastElement>(TypeSpec("int"), arg, true));
  } else if (env.dts->ts.tc(type, TypeSpec("float")) && mr.matched) {
    // this is a parent of float. normally we would fix this with a manual cast, but that is too
    // effective since float->int requires the type to be EXACTLY float.
    // so we add a cast to float first.
    // we also strip the gpr->fpr here
    result->push_back(pool.alloc_element<CastElement>(
        TypeSpec("int"), pool.form<CastElement>(TypeSpec("float"), mr.maps.forms.at(0), true),
        true));
  } else {
    throw std::runtime_error(
        fmt::format("Used float to int on a {}: {}", type.print(), to_string(env)));
  }
}

namespace {
GenericElement* allocate_fixed_op(FormPool& pool, FixedOperatorKind kind, Form* op1) {
  return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(kind), op1);
}
}  // namespace

void SimpleExpressionElement::update_from_stack_subu_l32_s7(const Env& env,
                                                            FormPool& pool,
                                                            FormStack& stack,
                                                            std::vector<FormElement*>* result,
                                                            bool allow_side_effects) {
  auto var = m_expr.get_arg(0).var();
  auto arg = pop_to_forms({var}, env, pool, stack, allow_side_effects).at(0);
  auto type = env.get_types_before_op(var.idx()).get(var.reg()).typespec();
  if (type != TypeSpec("handle")) {
    env.func->warnings.warning(
        ".subu (32-bit) used on a {} at idx {}. This probably should be a handle.", type.print(),
        var.idx());
  }
  result->push_back(allocate_fixed_op(pool, FixedOperatorKind::L32_NOT_FALSE_CBOOL, arg));
}

void SimpleExpressionElement::update_from_stack(const Env& env,
                                                FormPool& pool,
                                                FormStack& stack,
                                                std::vector<FormElement*>* result,
                                                bool allow_side_effects) {
  mark_popped();
  switch (m_expr.kind()) {
    case SimpleExpression::Kind::IDENTITY:
      update_from_stack_identity(env, pool, stack, result, allow_side_effects);
      break;
    case SimpleExpression::Kind::GPR_TO_FPR:
      update_from_stack_gpr_to_fpr(env, pool, stack, result, allow_side_effects);
      break;
    case SimpleExpression::Kind::FPR_TO_GPR:
      update_from_stack_fpr_to_gpr(env, pool, stack, result, allow_side_effects);
      break;
    case SimpleExpression::Kind::DIV_S:
      update_from_stack_div_s(env, pool, stack, result, allow_side_effects);
      break;
    case SimpleExpression::Kind::SUB_S:
      update_from_stack_float_2(env, FixedOperatorKind::SUBTRACTION, pool, stack, result,
                                allow_side_effects);
      break;
    case SimpleExpression::Kind::MUL_S:
      update_from_stack_float_2_nestable(env, FixedOperatorKind::MULTIPLICATION, pool, stack,
                                         result, allow_side_effects);
      break;
    case SimpleExpression::Kind::ADD_S:
      update_from_stack_float_2_nestable(env, FixedOperatorKind::ADDITION, pool, stack, result,
                                         allow_side_effects);
      break;
    case SimpleExpression::Kind::MAX_S:
      update_from_stack_float_2(env, FixedOperatorKind::FMAX, pool, stack, result,
                                allow_side_effects);
      break;
    case SimpleExpression::Kind::MIN_S:
      update_from_stack_float_2(env, FixedOperatorKind::FMIN, pool, stack, result,
                                allow_side_effects);
      break;
    case SimpleExpression::Kind::SQRT_S:
      update_from_stack_float_1(env, FixedOperatorKind::SQRT, pool, stack, result,
                                allow_side_effects);
      break;
    case SimpleExpression::Kind::ABS_S:
      update_from_stack_float_1(env, FixedOperatorKind::FABS, pool, stack, result,
                                allow_side_effects);
      break;
    case SimpleExpression::Kind::NEG:
      update_from_stack_si_1(env, FixedOperatorKind::SUBTRACTION, pool, stack, result,
                             allow_side_effects);
      break;
    case SimpleExpression::Kind::NEG_S:
      update_from_stack_float_1(env, FixedOperatorKind::SUBTRACTION, pool, stack, result,
                                allow_side_effects);
      break;
    case SimpleExpression::Kind::ADD:
      update_from_stack_add_i(env, pool, stack, result, allow_side_effects);
      break;
    case SimpleExpression::Kind::SUB:
      update_from_stack_copy_first_int_2(env, FixedOperatorKind::SUBTRACTION, pool, stack, result,
                                         allow_side_effects);
      break;
    case SimpleExpression::Kind::MUL_SIGNED:
      update_from_stack_mult_si(env, pool, stack, result, allow_side_effects);
      break;
    case SimpleExpression::Kind::DIV_SIGNED:
      update_from_stack_force_si_2(env, FixedOperatorKind::DIVISION, pool, stack, result,
                                   allow_side_effects, false);
      break;
    case SimpleExpression::Kind::DIV_UNSIGNED:
      update_from_stack_force_ui_2(env, FixedOperatorKind::DIVISION, pool, stack, result,
                                   allow_side_effects);
      break;
    case SimpleExpression::Kind::MOD_SIGNED:
      update_from_stack_force_si_2(env, FixedOperatorKind::MOD, pool, stack, result,
                                   allow_side_effects, false);
      break;
    case SimpleExpression::Kind::MOD_UNSIGNED:
      update_from_stack_force_ui_2(env, FixedOperatorKind::MOD, pool, stack, result,
                                   allow_side_effects);
      break;
    case SimpleExpression::Kind::MIN_SIGNED:
      update_from_stack_force_si_2(env, FixedOperatorKind::MIN, pool, stack, result,
                                   allow_side_effects, false);
      break;
    case SimpleExpression::Kind::MAX_SIGNED:
      update_from_stack_force_si_2(env, FixedOperatorKind::MAX, pool, stack, result,
                                   allow_side_effects, false);
      break;
    case SimpleExpression::Kind::AND:
      update_from_stack_logor_or_logand(env, FixedOperatorKind::LOGAND, pool, stack, result,
                                        allow_side_effects);
      break;
    case SimpleExpression::Kind::OR:
      update_from_stack_logor_or_logand(env, FixedOperatorKind::LOGIOR, pool, stack, result,
                                        allow_side_effects);
      break;
    case SimpleExpression::Kind::NOR:
      update_from_stack_copy_first_int_2(env, FixedOperatorKind::LOGNOR, pool, stack, result,
                                         allow_side_effects);
      break;
    case SimpleExpression::Kind::XOR:
      update_from_stack_copy_first_int_2(env, FixedOperatorKind::LOGXOR, pool, stack, result,
                                         allow_side_effects);
      break;
    case SimpleExpression::Kind::LOGNOT:
      update_from_stack_lognot(env, pool, stack, result, allow_side_effects);
      break;
    case SimpleExpression::Kind::LEFT_SHIFT:
      update_from_stack_left_shift(env, pool, stack, result, allow_side_effects);
      break;
    case SimpleExpression::Kind::RIGHT_SHIFT_LOGIC:
      update_from_stack_right_shift_logic(env, pool, stack, result, allow_side_effects);
      break;
    case SimpleExpression::Kind::RIGHT_SHIFT_ARITH:
      update_from_stack_right_shift_arith(env, pool, stack, result, allow_side_effects);
      break;
    case SimpleExpression::Kind::MUL_UNSIGNED:
      update_from_stack_force_ui_2(env, FixedOperatorKind::MULTIPLICATION, pool, stack, result,
                                   allow_side_effects);
      break;
    case SimpleExpression::Kind::INT_TO_FLOAT:
      update_from_stack_int_to_float(env, pool, stack, result, allow_side_effects);
      break;
    case SimpleExpression::Kind::FLOAT_TO_INT:
      update_from_stack_float_to_int(env, pool, stack, result, allow_side_effects);
      break;
    case SimpleExpression::Kind::PCPYLD:
      update_from_stack_pcypld(env, pool, stack, result, allow_side_effects);
      break;
    case SimpleExpression::Kind::VECTOR_PLUS:
      update_from_stack_vector_plus_minus_cross(FixedOperatorKind::VECTOR_PLUS, env, pool, stack,
                                                result, allow_side_effects);
      break;
    case SimpleExpression::Kind::VECTOR_MINUS:
      update_from_stack_vector_plus_minus_cross(FixedOperatorKind::VECTOR_MINUS, env, pool, stack,
                                                result, allow_side_effects);
      break;
    case SimpleExpression::Kind::VECTOR_CROSS:
      update_from_stack_vector_plus_minus_cross(FixedOperatorKind::VECTOR_CROSS, env, pool, stack,
                                                result, allow_side_effects);
      break;
    case SimpleExpression::Kind::VECTOR_FLOAT_PRODUCT:
      update_from_stack_vector_float_product(env, pool, stack, result, allow_side_effects);
      break;
    case SimpleExpression::Kind::SUBU_L32_S7:
      update_from_stack_subu_l32_s7(env, pool, stack, result, allow_side_effects);
      break;
    case SimpleExpression::Kind::VECTOR_3_DOT:
      update_from_stack_vectors_in_common(FixedOperatorKind::VECTOR_3_DOT, env, pool, stack, result,
                                          allow_side_effects);
      break;
    case SimpleExpression::Kind::VECTOR_4_DOT:
      update_from_stack_vectors_in_common(FixedOperatorKind::VECTOR_4_DOT, env, pool, stack, result,
                                          allow_side_effects);
      break;
    case SimpleExpression::Kind::VECTOR_LENGTH:
      update_from_stack_vectors_in_common(FixedOperatorKind::VECTOR_LENGTH, env, pool, stack,
                                          result, allow_side_effects);
      break;
    case SimpleExpression::Kind::VECTOR_PLUS_FLOAT_TIMES:
      update_from_stack_vector_plus_float_times(env, pool, stack, result, allow_side_effects);
      break;
    default:
      throw std::runtime_error(
          fmt::format("SimpleExpressionElement::update_from_stack NYI for {}", to_string(env)));
  }
}

///////////////////
// SetVarElement
///////////////////

void SetVarElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  mark_popped();
  for (auto x : m_src->elts()) {
    ASSERT(x->parent_form == m_src);
  }
  ASSERT(m_src->parent_element == this);

  // hack for method stuff
  if (is_dead_set()) {
    stack.push_value_to_reg_dead(m_dst, m_src, true, m_src_type, m_var_info);
    return;
  }

  // if we are a reg-reg move that consumes the original, push it without popping from stack.
  // it is the Stack's responsibility to untangle these later on.
  if (m_src->is_single_element()) {
    auto src_as_se = dynamic_cast<SimpleExpressionElement*>(m_src->back());
    if (src_as_se) {
      if (src_as_se->expr().kind() == SimpleExpression::Kind::IDENTITY &&
          src_as_se->expr().get_arg(0).is_var()) {
        // this can happen late in the case of coloring moves which are also gpr -> fpr's
        // so they don't get caught by SetVarOp::get_as_form's check.
        if (env.op_id_is_eliminated_coloring_move(src_as_se->expr().get_arg(0).var().idx())) {
          m_var_info.is_eliminated_coloring_move = true;
        }

        auto var = src_as_se->expr().get_arg(0).var();
        auto& info = env.reg_use().op.at(var.idx());
        if (var.reg() == Register(Reg::GPR, Reg::S6) ||
            info.consumes.find(var.reg()) != info.consumes.end()) {
          stack.push_non_seq_reg_to_reg(m_dst, src_as_se->expr().get_arg(0).var(), m_src,
                                        m_src_type, m_var_info);
          return;
        }
      }
    }
  }

  // we aren't a reg-reg move, so update our source
  m_src->update_children_from_stack(env, pool, stack, true);

  for (auto x : m_src->elts()) {
    ASSERT(x->parent_form == m_src);
  }

  if (auto test0 = m_src->to_string(env) == "(* 0.00024414062 (-> arg0 y))") {
    printf("");
  }
  if (m_src->is_single_element()) {
    auto src_as_se = dynamic_cast<SimpleExpressionElement*>(m_src->back());
    if (src_as_se) {
      if (src_as_se->expr().kind() == SimpleExpression::Kind::IDENTITY &&
          m_dst.reg().get_kind() == Reg::FPR && src_as_se->expr().get_arg(0).is_int() &&
          src_as_se->expr().get_arg(0).get_int() == 0) {
        // not sure this is the best place for this.
        stack.push_value_to_reg(m_dst, pool.form<ConstantFloatElement>(0.0), true, m_src_type,
                                m_var_info);
        return;
      }

      // this might get skipped earlier because gpr->fpr gets wrapped in an operation that's
      // stripped off by update_children_from_stack.
      if (src_as_se->expr().kind() == SimpleExpression::Kind::IDENTITY &&
          src_as_se->expr().get_arg(0).is_var()) {
        if (env.op_id_is_eliminated_coloring_move(src_as_se->expr().get_arg(0).var().idx())) {
          m_var_info.is_eliminated_coloring_move = true;
        }
      }
    }

    // (* 0.125 b) -> (/ b 8)
    // adds explicit cast b to float if necessary
    auto src_as_ge = dynamic_cast<GenericElement*>(m_src->back());
    if (src_as_ge) {
      auto mr = match(Matcher::op_fixed(FixedOperatorKind::MULTIPLICATION,
                                        {Matcher::any_single(0), Matcher::any(1)}),
                      m_src);
      if (mr.matched && std::abs(mr.maps.floats.at(0)) < 1 && mr.maps.floats.at(0) != 0) {
        auto inverse_mult = mr.maps.floats.at(0);
        float divisor_num = 1.0f / inverse_mult;

        // note: float inaccuracies lead to convergent values, so we can do this safely
        if ((int)divisor_num != divisor_num && 1.0f / std::roundf(divisor_num) == inverse_mult) {
          lg::debug("managed to round divisor - cool !! {} -> {} ({})", divisor_num,
                    std::roundf(divisor_num), inverse_mult);
          divisor_num = std::roundf(divisor_num);
        }

        int divisor_int = (int)divisor_num;
        bool integer = divisor_int == divisor_num;
        if (integer) {
          auto elt = mr.maps.forms.at(1)->try_as_single_element();
          auto b_as_simple = dynamic_cast<SimpleExpressionElement*>(elt);
          // WARNING : there is an assumption here that derefs DO NOT have implicit casts!
          auto b_as_deref = dynamic_cast<DerefElement*>(elt);
          if (b_as_deref ||
              (b_as_simple && b_as_simple->expr().kind() == SimpleExpression::Kind::IDENTITY)) {
            // TODO check if op is float, cast if so
            Form* divisor = nullptr;
            if (divisor_num == 4096.0f) {
              divisor = pool.form<ConstantTokenElement>("METER_LENGTH");
            } else if (integer && divisor_int % 4096 == 0) {
              divisor = pool.form<GenericElement>(
                  GenericOperator::make_function(pool.form<ConstantTokenElement>("meters")),
                  pool.form<SimpleAtomElement>(divisor_int / 4096, true));
            } else if (integer && divisor_int % 2048 == 0) {
              divisor = pool.form<GenericElement>(
                  GenericOperator::make_function(pool.form<ConstantTokenElement>("meters")),
                  pool.form<ConstantFloatElement>(divisor_num / (float)METER_LENGTH));
            } else if (integer) {
              divisor = pool.form<SimpleAtomElement>(divisor_int, true);
            } else {
              // this shouldn't run because of the checks before.
              divisor = pool.form<ConstantFloatElement>(divisor_num);
            }
            if (divisor) {
              if (b_as_deref || (b_as_simple->expr().is_var() &&
                                 env.get_types_before_op(b_as_simple->expr().var().idx())
                                         .get(b_as_simple->expr().var().reg())
                                         .typespec() == TypeSpec("float"))) {
                *m_src->back_ref() = pool.alloc_element<GenericElement>(
                    GenericOperator::make_fixed(FixedOperatorKind::DIVISION), mr.maps.forms.at(1),
                    divisor);
              } else {
                *m_src->back_ref() = pool.alloc_element<GenericElement>(
                    GenericOperator::make_fixed(FixedOperatorKind::DIVISION),
                    pool.form<CastElement>(TypeSpec("float"), mr.maps.forms.at(1), true), divisor);
              }
              m_src->back()->parent_form = m_src;
            }
          }
        }
      }
    }
  }

  stack.push_value_to_reg(m_dst, m_src, true, m_src_type, m_var_info);
  for (auto x : m_src->elts()) {
    ASSERT(x->parent_form == m_src);
  }
}

FormElement* SetFormFormElement::make_set_time(const Env& /*env*/,
                                               FormPool& pool,
                                               FormStack& /*stack*/) {
  auto matcher = match(
      Matcher::op(GenericOpMatcher::func(Matcher::constant_token("current-time")), {}), m_src);
  if (matcher.matched) {
    return pool.alloc_element<GenericElement>(
        GenericOperator::make_function(pool.form<ConstantTokenElement>("set-time!")), m_dst);
  }
  return nullptr;
}

void SetFormFormElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  ASSERT(m_popped);
  ASSERT(m_real_push_count == 0);
  m_real_push_count++;

  // check for bitfield setting:
  auto src_as_bf_set = dynamic_cast<ModifiedCopyBitfieldElement*>(m_src->try_as_single_element());
  if (src_as_bf_set && !src_as_bf_set->from_pcpyud() && src_as_bf_set->mods().size() == 1) {
    auto dst_form = m_dst->to_form(env);
    auto src_form = src_as_bf_set->base()->to_form(env);
    if (dst_form == src_form) {
      // success!
      auto value = src_as_bf_set->mods().at(0).value;
      value->parent_element = this;

      // make the (-> thing bitfield)
      auto field_token = DerefToken::make_field_name(src_as_bf_set->mods().at(0).field_name);
      auto loc_elt = pool.alloc_element<DerefElement>(m_dst, false, field_token);
      loc_elt->inline_nested();
      auto loc = pool.alloc_single_form(nullptr, loc_elt);
      loc->parent_element = this;

      m_dst = loc;
      m_src = value;
    }
  } else if (src_as_bf_set) {
    lg::warn("invalid bf set: {}", src_as_bf_set->to_string(env));
  }

  // setting a bitfield to zero is wonky.
  auto bfa = dynamic_cast<BitfieldAccessElement*>(m_src->try_as_single_element());
  if (bfa) {
    auto zero_set = bfa->get_set_field_0(env.dts->ts);
    if (zero_set) {
      auto field_token = DerefToken::make_field_name(zero_set->name());
      auto loc_elt = pool.alloc_element<DerefElement>(m_dst, false, field_token);
      loc_elt->inline_nested();
      auto loc = pool.alloc_single_form(nullptr, loc_elt);
      loc->parent_element = this;
      m_dst = loc;
      auto zero = SimpleAtom::make_int_constant(0);
      auto zero_form = pool.form<SimpleAtomElement>(zero);
      m_src = zero_form;
    }
  }

  typedef struct {
    FixedOperatorKind kind;
    FixedOperatorKind inplace_kind;
    std::vector<int> inplace_arg;
  } InplaceOpInfo;

  const static InplaceOpInfo in_place_ops[] = {
      {FixedOperatorKind::ADDITION, FixedOperatorKind::ADDITION_IN_PLACE, {0, 1}},
      {FixedOperatorKind::ADDITION_PTR, FixedOperatorKind::ADDITION_PTR_IN_PLACE, {0, 1}},
      {FixedOperatorKind::LOGAND, FixedOperatorKind::LOGAND_IN_PLACE, {0, 1}},
      {FixedOperatorKind::LOGIOR, FixedOperatorKind::LOGIOR_IN_PLACE, {0, 1}},
      {FixedOperatorKind::LOGCLEAR, FixedOperatorKind::LOGCLEAR_IN_PLACE, {0, 1}},
      {FixedOperatorKind::LOGXOR, FixedOperatorKind::LOGXOR_IN_PLACE, {0, 1}}};

  typedef struct {
    std::string orig_name;
    std::string inplace_name;
    int inplace_arg;
  } InplaceCallInfo;

  const static InplaceCallInfo in_place_calls[] = {{"seek", "seek!", 0}, {"seekl", "seekl!", 0}};

  auto as_set_time = make_set_time(env, pool, stack);
  if (as_set_time) {
    stack.push_form_element(as_set_time, true);
    return;
  }

  auto src_as_generic = m_src->try_as_element<GenericElement>();
  if (src_as_generic) {
    if (src_as_generic->op().is_func()) {
      auto funchelt = dynamic_cast<SimpleExpressionElement*>(src_as_generic->op().func()->at(0));
      if (funchelt && funchelt->expr().get_arg(0).is_sym_val()) {
        const auto& funcname = funchelt->expr().get_arg(0).get_str();
        for (const auto& call_info : in_place_calls) {
          if (funcname == call_info.orig_name) {
            auto dst_form = m_dst->to_form(env);
            auto src_form_in_func = src_as_generic->elts().at(call_info.inplace_arg)->to_form(env);

            if (dst_form == src_form_in_func) {
              auto new_func_op = GenericOperator::make_function(
                  pool.form<ConstantTokenElement>(call_info.inplace_name));
              GenericElement* inplace_call = nullptr;

              if (funcname == "seek" || funcname == "seekl") {
                inplace_call = pool.alloc_element<GenericElement>(
                    new_func_op, std::vector<Form*>{m_dst, src_as_generic->elts().at(1),
                                                    src_as_generic->elts().at(2)});
              }
              ASSERT_MSG(
                  inplace_call,
                  fmt::format("no appropriate inplace call was generated for (set! {}) -> {}",
                              call_info.orig_name, call_info.inplace_name));
              stack.push_form_element(inplace_call, true);
              return;
            }
          }
        }
      }
    } else {
      for (const auto& op_info : in_place_ops) {
        if (src_as_generic->op().is_fixed(op_info.kind)) {
          auto dst_form = m_dst->to_form(env);

          for (int inplace_arg : op_info.inplace_arg) {
            auto add_form_0 = src_as_generic->elts().at(inplace_arg)->to_form(env);

            if (dst_form == add_form_0) {
              if (inplace_arg != 0) {
                std::swap(src_as_generic->elts().at(0), src_as_generic->elts().at(1));
              }
              src_as_generic->op() = GenericOperator::make_fixed(op_info.inplace_kind);
              stack.push_form_element(src_as_generic, true);
              return;
            }
          }
        }
      }
    }
  }

  stack.push_form_element(this, true);
}

void StoreInSymbolElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  auto sym =
      pool.form<SimpleExpressionElement>(SimpleAtom::make_sym_val(m_sym_name).as_expr(), m_my_idx);
  auto val = pool.form<SimpleExpressionElement>(m_value, m_my_idx);
  val->update_children_from_stack(env, pool, stack, true);

  if (m_cast_for_set) {
    // we'd need to cast for set. Let's see if we can simplify it instead:
    auto simplified = try_cast_simplify(val, *m_cast_for_set, pool, env, false);
    if (simplified) {
      if (m_cast_for_define && *m_cast_for_define == *m_cast_for_set) {
        // if we'd need exactly the same cast for a define, we can drop it too
        m_cast_for_define = {};
      }
      m_cast_for_set = {};
      val = simplified;
    }
  }

  auto elt = pool.alloc_element<SetFormFormElement>(sym, val, m_cast_for_set, m_cast_for_define);
  elt->mark_popped();
  stack.push_form_element(elt, true);
}

void StoreInPairElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  auto op = m_is_car ? FixedOperatorKind::CAR : FixedOperatorKind::CDR;
  if (m_value.is_var()) {
    auto vars = std::vector<RegisterAccess>({m_value.var(), m_pair});
    auto popped = pop_to_forms(vars, env, pool, stack, true);
    auto addr = pool.form<GenericElement>(GenericOperator::make_fixed(op), popped.at(1));
    addr->mark_popped();
    auto fr = pool.alloc_element<SetFormFormElement>(addr, popped.at(0));
    fr->mark_popped();
    stack.push_form_element(fr, true);
  } else {
    auto val = pool.form<SimpleExpressionElement>(m_value, m_my_idx);
    val->mark_popped();
    auto addr = pool.form<GenericElement>(GenericOperator::make_fixed(op),
                                          pop_to_forms({m_pair}, env, pool, stack, true).at(0));
    addr->mark_popped();
    auto fr = pool.alloc_element<SetFormFormElement>(addr, val);
    fr->mark_popped();
    stack.push_form_element(fr, true);
  }
}

namespace {

Form* make_optional_cast(const std::optional<TypeSpec>& cast_type,
                         Form* in,
                         FormPool& pool,
                         const Env& env) {
  if (cast_type) {
    return cast_form(in, *cast_type, pool, env);
  } else {
    return in;
  }
}

bool try_to_rewrite_vector_inline_ctor(const Env& env,
                                       FormPool& pool,
                                       FormStack& stack,
                                       const std::string& type_name) {
  // now, let's check for a matrix initialization.
  auto matrix_entries = stack.try_getting_active_stack_entries({true, false});
  if (matrix_entries) {
    // the (set! var (new 'stack-no-clear 'matrix))
    if (matrix_entries->at(0).destination->reg() == Register(Reg::GPR, Reg::R0)) {
      return false;
    }
    auto var_name = env.get_variable_name(*matrix_entries->at(0).destination);
    auto src = matrix_entries->at(0).source->try_as_element<StackStructureDefElement>();
    if (!src) {
      return false;
    }
    if (src->type() != TypeSpec(type_name)) {
      return false;
    }

    // zeroing the rows:
    std::vector<RegisterAccess> write_vars;

    auto elt = matrix_entries->at(1).elt;

    std::vector<DerefTokenMatcher> token_matchers = {DerefTokenMatcher::string("vec"),
                                                     DerefTokenMatcher::string("quad")};
    if (type_name == "vector") {
      token_matchers = {DerefTokenMatcher::string("quad")};
    }

    if (env.version >= GameVersion::Jak2) {
      token_matchers = {DerefTokenMatcher::string("quad")};
    }

    auto matcher = Matcher::set(Matcher::deref(Matcher::any_reg(0), false, token_matchers),
                                Matcher::cast("uint128", Matcher::integer(0)));

    auto mr = match(matcher, elt);
    if (mr.matched) {
      if (var_name != env.get_variable_name(*mr.maps.regs.at(0))) {
        return false;
      }
      write_vars.push_back(*mr.maps.regs.at(0));
    } else {
      return false;
    }

    // success!
    for (auto& wv : write_vars) {
      env.get_use_def_info(wv);
      Env* menv = const_cast<Env*>(&env);
      menv->disable_use(wv);
    }
    stack.pop(2);

    stack.push_value_to_reg(
        *matrix_entries->at(0).destination,
        pool.form<GenericElement>(GenericOperator::make_function(
            pool.form<ConstantTokenElement>(fmt::format("new-stack-{}0", type_name)))),
        true, TypeSpec(type_name));
    return true;
  }
  return false;
}

bool try_to_rewrite_matrix_inline_ctor(const Env& env, FormPool& pool, FormStack& stack) {
  // now, let's check for a matrix initialization.
  auto matrix_entries = stack.try_getting_active_stack_entries({true, false, false, false, false});
  if (matrix_entries) {
    // the (set! var (new 'stack-no-clear 'matrix))
    if (matrix_entries->at(0).destination->reg() == Register(Reg::GPR, Reg::R0)) {
      return false;
    }
    auto var_name = env.get_variable_name(*matrix_entries->at(0).destination);
    auto src = matrix_entries->at(0).source->try_as_element<StackStructureDefElement>();
    if (!src) {
      return false;
    }
    if (src->type() != TypeSpec("matrix")) {
      return false;
    }

    // zeroing the rows:
    std::vector<RegisterAccess> write_vars;
    if (env.version == GameVersion::Jak1) {
      for (int i = 0; i < 4; i++) {
        auto elt = matrix_entries->at(i + 1).elt;

        auto matcher = Matcher::set(
            Matcher::deref(Matcher::any_reg(0), false,
                           {DerefTokenMatcher::string("vector"), DerefTokenMatcher::integer(i),
                            DerefTokenMatcher::string("quad")}),
            Matcher::cast("uint128", Matcher::integer(0)));

        auto mr = match(matcher, elt);
        if (mr.matched) {
          if (var_name != env.get_variable_name(*mr.maps.regs.at(0))) {
            return false;
          }
          write_vars.push_back(*mr.maps.regs.at(0));
        } else {
          return false;
        }
      }
    } else {
      for (int i = 0; i < 4; i++) {
        auto elt = matrix_entries->at(i + 1).elt;

        Matcher matcher;
        if (i == 3) {
          matcher = Matcher::set(Matcher::deref(Matcher::any_reg(0), false,
                                                {DerefTokenMatcher::string("trans"),
                                                 DerefTokenMatcher::string("quad")}),
                                 Matcher::cast("uint128", Matcher::integer(0)));

        } else {
          matcher = Matcher::set(
              Matcher::deref(Matcher::any_reg(0), false,
                             {DerefTokenMatcher::string("quad"), DerefTokenMatcher::integer(i)}),
              Matcher::cast("uint128", Matcher::integer(0)));
        }

        auto mr = match(matcher, elt);
        if (mr.matched) {
          if (var_name != env.get_variable_name(*mr.maps.regs.at(0))) {
            return false;
          }
          write_vars.push_back(*mr.maps.regs.at(0));
        } else {
          return false;
        }
      }
    }

    // success!
    for (auto& wv : write_vars) {
      env.get_use_def_info(wv);
      Env* menv = const_cast<Env*>(&env);
      menv->disable_use(wv);
    }
    stack.pop(5);

    stack.push_value_to_reg(*matrix_entries->at(0).destination,
                            pool.form<GenericElement>(GenericOperator::make_function(
                                pool.form<ConstantTokenElement>("new-stack-matrix0"))),
                            true, TypeSpec("matrix"));
    return true;
  }
  return false;
}

}  // namespace

void StorePlainDeref::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  mark_popped();

  if (m_expr.is_var()) {
    // this matches the order in Compiler::compile_set
    auto vars = std::vector<RegisterAccess>({m_expr.var(), m_base_var});
    // for 16-byte stores, the order is backward. Why????
    if (size() == 16) {
      std::swap(vars.at(0), vars.at(1));
    }
    auto popped = pop_to_forms(vars, env, pool, stack, true);
    if (size() == 16) {
      std::swap(popped.at(0), popped.at(1));
    }
    m_dst->try_as_element<DerefElement>()->set_base(
        make_optional_cast(m_dst_cast_type, popped.at(1), pool, env));
    m_dst->mark_popped();
    m_dst->try_as_element<DerefElement>()->inline_nested();
    auto fr = pool.alloc_element<SetFormFormElement>(
        m_dst, make_optional_cast(m_src_cast_type, popped.at(0), pool, env));
    // so the bitfield set check can run
    fr->mark_popped();
    fr->push_to_stack(env, pool, stack);
  } else {
    auto vars = std::vector<RegisterAccess>({m_base_var});
    auto popped = pop_to_forms(vars, env, pool, stack, true);
    m_dst->try_as_element<DerefElement>()->set_base(
        make_optional_cast(m_dst_cast_type, popped.at(0), pool, env));
    m_dst->mark_popped();
    m_dst->try_as_element<DerefElement>()->inline_nested();
    auto val = pool.form<SimpleExpressionElement>(m_expr, m_my_idx);
    val->mark_popped();
    auto fr = pool.alloc_element<SetFormFormElement>(
        m_dst, make_optional_cast(m_src_cast_type, val, pool, env));
    fr->mark_popped();
    stack.push_form_element(fr, true);
  }

  if (!try_to_rewrite_matrix_inline_ctor(env, pool, stack)) {
    if (!try_to_rewrite_vector_inline_ctor(env, pool, stack, "vector")) {
      try_to_rewrite_vector_inline_ctor(env, pool, stack, "quaternion");
    }
  }
}

void StoreArrayAccess::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  mark_popped();
  Form* expr_form = nullptr;
  Form* array_form = nullptr;
  if (m_expr.is_var()) {
    auto vars = std::vector<RegisterAccess>({m_expr.var(), m_base_var});
    auto popped = pop_to_forms(vars, env, pool, stack, true);
    m_dst->mark_popped();
    expr_form = popped.at(0);
    array_form = popped.at(1);
  } else {
    auto vars = std::vector<RegisterAccess>({m_base_var});
    auto popped = pop_to_forms(vars, env, pool, stack, true);
    m_dst->mark_popped();
    expr_form = pool.form<SimpleExpressionElement>(m_expr, m_my_idx);
    array_form = popped.at(0);
  }

  std::vector<FormElement*> forms_out;
  m_dst->update_with_val(array_form, env, pool, &forms_out, true);
  auto form_out = pool.alloc_sequence_form(nullptr, forms_out);

  auto fr = pool.alloc_element<SetFormFormElement>(
      form_out, make_optional_cast(m_src_cast_type, expr_form, pool, env));
  fr->mark_popped();
  fr->push_to_stack(env, pool, stack);
}

///////////////////
// AshElement
///////////////////

void AshElement::update_from_stack(const Env& env,
                                   FormPool& pool,
                                   FormStack& stack,
                                   std::vector<FormElement*>* result,
                                   bool allow_side_effects) {
  mark_popped();
  auto forms = pop_to_forms({value, shift_amount}, env, pool, stack, allow_side_effects, consumed);
  auto new_form = pool.alloc_element<GenericElement>(
      GenericOperator::make_fixed(FixedOperatorKind::ARITH_SHIFT), forms.at(0), forms.at(1));
  result->push_back(new_form);
}

///////////////////
// AbsElement
///////////////////

void AbsElement::update_from_stack(const Env& env,
                                   FormPool& pool,
                                   FormStack& stack,
                                   std::vector<FormElement*>* result,
                                   bool allow_side_effects) {
  mark_popped();
  auto forms = pop_to_forms({source}, env, pool, stack, allow_side_effects, consumed);
  auto new_form = pool.alloc_element<GenericElement>(
      GenericOperator::make_fixed(FixedOperatorKind::ABS), forms.at(0));
  result->push_back(new_form);
}

namespace {
/*!
 * Try to recognize setting the next state.
 */
Form* get_set_next_state(FormElement* set_elt, const Env& env) {
  auto as_set = dynamic_cast<SetFormFormElement*>(set_elt);
  if (!as_set) {
    return nullptr;
  }

  auto dst = as_set->dst();
  auto dst_matcher =
      Matcher::deref(Matcher::any_reg(0), false, {DerefTokenMatcher::string("next-state")});
  auto mr = match(dst_matcher, dst);
  if (!mr.matched) {
    lg::error("failed to match dst {}", dst->to_string(env));
    return nullptr;
  }

  if (mr.maps.regs.at(0)->reg() != Register(Reg::GPR, Reg::S6)) {
    lg::error("failed to match pp reg, got {}", mr.maps.regs.at(0)->reg().to_string());
    return nullptr;
  }

  return as_set->src();
}
}  // namespace

///////////////////
// FunctionCallElement
///////////////////

namespace {

std::optional<RegisterAccess> get_form_reg_acc(Form* in) {
  auto as_simple_atom = dynamic_cast<SimpleAtomElement*>(in->try_as_single_active_element());
  if (as_simple_atom) {
    if (as_simple_atom->atom().is_var()) {
      return as_simple_atom->atom().var();
    }
  }

  auto as_expr = dynamic_cast<SimpleExpressionElement*>(in->try_as_single_active_element());
  if (as_expr && as_expr->expr().is_identity()) {
    auto atom = as_expr->expr().get_arg(0);
    if (atom.is_var()) {
      return atom.var();
    }
  }

  return {};
}
}  // namespace

void FunctionCallElement::update_from_stack(const Env& env,
                                            FormPool& pool,
                                            FormStack& stack,
                                            std::vector<FormElement*>* result,
                                            bool allow_side_effects) {
  mark_popped();
  std::vector<Form*> args;
  auto nargs = m_op->arg_vars().size();
  args.resize(nargs, nullptr);

  std::vector<RegisterAccess> all_pop_vars = {m_op->function_var()};
  for (size_t i = 0; i < nargs; i++) {
    all_pop_vars.push_back(m_op->arg_vars().at(i));
  }

  TypeSpec function_type;
  auto& in_type_state = env.get_types_before_op(all_pop_vars.at(0).idx());
  auto& tp_type = in_type_state.get(all_pop_vars.at(0).reg());
  if (env.has_type_analysis()) {
    function_type = tp_type.typespec();
  }

  // if we're actually a go:
  Form* go_next_state = nullptr;
  if (tp_type.kind == TP_Type::Kind::ENTER_STATE_FUNCTION) {
    auto& next_state_type = in_type_state.next_state_type;
    if (next_state_type.typespec().base_type() != "state") {
      throw std::runtime_error("Bad state type in expressions (not state): " +
                               next_state_type.print());
    }
    if (next_state_type.typespec().arg_count() == 0) {
      throw std::runtime_error("Bad state type in expressions (no args): " +
                               next_state_type.print());
    }

    // modify our type for the go.
    function_type = state_to_go_function(next_state_type.typespec(), TypeSpec("object"));

    // up next, we need to deal with the
    // (set! (-> pp next-state) process-drawable-art-error)
    auto stack_back = stack.pop_back(pool);

    auto next_state = get_set_next_state(stack_back, env);
    if (!next_state) {
      throw std::runtime_error(
          fmt::format("Expressions couldn't figure out this go. The back of the stack was {} and "
                      "we expected to see something set (-> pp next-state) instead.",
                      stack_back->to_string(env)));
    }
    go_next_state = next_state;
  }

  bool swap_function =
      tp_type.kind == TP_Type::Kind::NON_VIRTUAL_METHOD && all_pop_vars.size() >= 2;
  if (tp_type.kind == TP_Type::Kind::NON_VIRTUAL_METHOD) {
    // this is a hack to make some weird macro for calling res-lump methods work
    if (env.dts->ts.tc(TypeSpec("res-lump"), tp_type.method_from_type())) {
      swap_function = false;
    }
  }

  if (swap_function) {
    std::swap(all_pop_vars.at(0), all_pop_vars.at(1));
  }

  auto unstacked = pop_to_forms(all_pop_vars, env, pool, stack, allow_side_effects);
  if (swap_function) {
    std::swap(unstacked.at(0), unstacked.at(1));
    std::swap(all_pop_vars.at(0), all_pop_vars.at(1));
  }

  if (tp_type.kind == TP_Type::Kind::RUN_FUNCTION_IN_PROCESS_FUNCTION) {
    if (unstacked.at(0)->to_string(env) == "run-function-in-process") {
      unstacked.at(0) = pool.form<ConstantTokenElement>("run-now-in-process");
    } else {
      // couldn't pop. need to add a cast.
      TypeSpec failed_cast("function");
      for (int i = 0; i < ((int)unstacked.size()) - 1; i++) {
        failed_cast.add_arg(TypeSpec("object"));
      }
      failed_cast.add_arg(TypeSpec("none"));
      unstacked.at(0) = pool.form<CastElement>(failed_cast, unstacked.at(0));
    }
  }

  if (tp_type.kind == TP_Type::Kind::SET_TO_RUN_FUNCTION) {
    if (unstacked.at(0)->to_string(env) != "set-to-run") {
      throw std::runtime_error(
          fmt::format("Expression pass could not find the set-to-run function. Found "
                      "{} instead. Make sure there are no casts on this function.",
                      all_pop_vars.at(0).to_string(env)));
    }
    unstacked.at(0) = pool.form<ConstantTokenElement>("run-next-time-in-process");
    // also need to clean up (-> <blah> main-thread) to just <blah>
    auto matcher =
        Matcher::deref(Matcher::any(0), false, {DerefTokenMatcher::string("main-thread")});
    auto mr = match(matcher, unstacked.at(1));
    if (!mr.matched) {
      throw std::runtime_error(fmt::format("set-to-run called on a strange thread: {}",
                                           unstacked.at(1)->to_string(env)));
    }
    unstacked.at(1) = mr.maps.forms.at(0);
  }

  std::vector<Form*> arg_forms;
  bool has_good_types = env.has_type_analysis() && function_type.arg_count() == nargs + 1;
  TypeSpec first_arg_type;

  for (size_t arg_id = 0; arg_id < nargs; arg_id++) {
    auto val = unstacked.at(arg_id + 1);  // first is the function itself.
    auto& var = all_pop_vars.at(arg_id + 1);
    if (has_good_types) {
      auto actual_arg_type = env.get_types_before_op(var.idx()).get(var.reg()).typespec();

      if (arg_id == 0) {
        first_arg_type = actual_arg_type;
      }

      auto desired_arg_type = function_type.get_arg(arg_id);
      if (env.dts->should_attempt_cast_simplify(desired_arg_type, actual_arg_type)) {
        arg_forms.push_back(cast_form(val, desired_arg_type, pool, env,
                                      env.dts->ts.tc(desired_arg_type, actual_arg_type)));
      } else {
        arg_forms.push_back(val);
      }
    } else {
      arg_forms.push_back(val);
    }
  }

  FormElement* new_form = nullptr;

  if (go_next_state) {
    // see if we're a virtual go
    Matcher virtual_go_state_matcher =
        Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::METHOD_OF_OBJECT),
                    {Matcher::any(0), Matcher::any_constant_token(1)});
    auto virtual_go_mr = match(virtual_go_state_matcher, go_next_state);
    if (virtual_go_mr.matched && virtual_go_mr.maps.forms.at(0)->to_string(env) == "self") {
      arg_forms.insert(arg_forms.begin(),
                       pool.form<ConstantTokenElement>(virtual_go_mr.maps.strings.at(1)));
      auto go_form = pool.alloc_element<GenericElement>(
          GenericOperator::make_function(pool.form<ConstantTokenElement>("go-virtual")), arg_forms);
      result->push_back(go_form);
      return;
    }

    arg_forms.insert(arg_forms.begin(), go_next_state);
    auto go_form = pool.alloc_element<GenericElement>(
        GenericOperator::make_function(pool.form<ConstantTokenElement>("go")), arg_forms);
    result->push_back(go_form);
    return;
  }

  // tpage and texture macros
  {
    auto func = Matcher::symbol("lookup-texture-by-id");
    auto func_fast = Matcher::symbol("lookup-texture-by-id-fast");
    auto mr = match(func, unstacked.at(0));
    auto mr_fast = match(func_fast, unstacked.at(0));
    if (mr.matched || mr_fast.matched) {
      auto tex_id = Matcher::any_integer(0);
      auto mr2 = match(tex_id, unstacked.at(1));
      if (mr2.matched) {
        auto id = mr2.maps.ints.at(0);
        u16 tpage = (id & 0xfff00000) >> 20;
        u16 idx = (id & 0x000fff00) >> 8;
        auto fixed_id = tpage << 16 | idx;
        if (!env.dts->textures.empty() &&
            env.dts->textures.find(fixed_id) != env.dts->textures.end()) {
          std::vector<Form*> macro_args;
          auto tex = env.dts->textures.at(fixed_id);
          macro_args.push_back(pool.form<ConstantTokenElement>(tex.name));
          macro_args.push_back(pool.form<ConstantTokenElement>(tex.tpage_name));
          auto macro = pool.alloc_element<GenericElement>(
              GenericOperator::make_function(pool.form<ConstantTokenElement>("get-texture")),
              macro_args);
          result->push_back(macro);
          return;
        }
      }
    }
  }

  {
    // deal with virtual method calls.
    auto matcher = Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::METHOD_OF_OBJECT),
                               {Matcher::any_reg(0), Matcher::any(1)});
    auto mr = match(matcher, unstacked.at(0));
    if (mr.matched && nargs >= 1) {
      auto vtable_reg = mr.maps.regs.at(0);
      ASSERT(vtable_reg);
      auto vtable_var_name = env.get_variable_name(*vtable_reg);
      auto arg0_mr = match(Matcher::any_reg(0), unstacked.at(1));
      if (arg0_mr.matched && env.get_variable_name(*arg0_mr.maps.regs.at(0)) == vtable_var_name) {
        if (tp_type.kind != TP_Type::Kind::VIRTUAL_METHOD &&
            tp_type.kind != TP_Type::Kind::GET_ART_BY_NAME_METHOD) {
          throw std::runtime_error(
              "Method internal mismatch. METHOD_OF_OBJECT operator didn't get a VIRTUAL_METHOD "
              "type.");
        }

        bool is_res_lump = tp_type.method_from_type().base_type() == "res-lump" ||
                           tp_type.method_from_type().base_type() == "entity-actor";
        bool should_use_virtual =
            env.dts->ts.should_use_virtual_methods(tp_type.method_from_type(), tp_type.method_id());

        if (!should_use_virtual && !is_res_lump) {
          throw std::runtime_error(
              fmt::format("Method call on {} id {} at {} used a virtual call unexpectedly.",
                          tp_type.method_from_type().print(), tp_type.method_id(), "ya"));
        }

        if (should_use_virtual) {
          // lg::print("STACK\n{}\n\n", stack.print(env));
          auto pop = pop_to_forms({*arg0_mr.maps.regs.at(0)}, env, pool, stack, allow_side_effects,
                                  {}, {2})
                         .at(0);
          // lg::print("GOT: {}\n", pop->to_string(env));
          arg_forms.at(0) = pop;
          auto head = mr.maps.forms.at(1);
          auto head_obj = head->to_form(env);

          // rewrite get-art-by-name-method calls to get-art-by-name, which is a macro that
          // will apply the appropriate cast.
          if (tp_type.kind == TP_Type::Kind::GET_ART_BY_NAME_METHOD) {
            ASSERT(head_obj.is_symbol("get-art-by-name-method"));
            head = pool.form<ConstantTokenElement>("get-art-by-name");
          } else {
            if (head_obj.is_symbol("set-subtask-hook!")) {
              // change the integer argument to a constant.
              auto arg2o = arg_forms.at(2)->to_form(env);
              if (arg2o.is_int()) {
                int hook = arg2o.as_int();
                static const std::vector<std::string> hook_names = {
                    "TASK_MANAGER_INIT_HOOK",     "TASK_MANAGER_CLEANUP_HOOK",
                    "TASK_MANAGER_UPDATE_HOOK",   "TASK_MANAGER_CODE_HOOK",
                    "TASK_MANAGER_COMPLETE_HOOK", "TASK_MANAGER_FAIL_HOOK",
                    "TASK_MANAGER_EVENT_HOOK"};
                if (hook >= 0 && hook < (int)hook_names.size()) {
                  arg_forms.at(2) = pool.alloc_single_element_form<ConstantTokenElement>(
                      arg_forms.at(2)->parent_element, hook_names.at(hook));
                }
              }
            } else if (head_obj.is_symbol() &&
                       tp_type.method_from_type().base_type() == "setting-control" &&
                       arg_forms.at(0)->to_form(env).is_symbol("*setting-control*") &&
                       arg_forms.size() > 1) {
              auto arg1_reg = get_form_reg_acc(arg_forms.at(1));
              bool has_params = true;
              if (arg1_reg && arg1_reg->reg().is_s6()) {
                std::string new_head;
                if (head_obj.is_symbol("add-setting")) {
                  new_head = "add-setting!";
                } else if (head_obj.is_symbol("set-setting")) {
                  new_head = "set-setting!";
                } else if (head_obj.is_symbol("remove-setting")) {
                  new_head = "remove-setting!";
                  has_params = false;
                }
                if (!new_head.empty()) {
                  auto oldp = head->parent_element;
                  head = pool.form<ConstantTokenElement>(new_head);
                  head->parent_element = oldp;
                  arg_forms.erase(arg_forms.begin());
                  arg_forms.erase(arg_forms.begin());
                  if (has_params) {
                    auto argset = arg_forms.at(0)->to_string(env);
                    argset = argset.substr(1);
                    auto argsym = arg_forms.at(1)->to_string(env);
                    // convert the float param
                    if (env.version >= GameVersion::Jak2) {
                      static const std::unordered_set<std::string> use_degrees_settings = {
                          "matrix-blend-max-angle",
                          "fov",
                      };
                      static const std::unordered_set<std::string> use_meters_settings = {
                          "string-spline-max-move",
                          "string-spline-max-move-player",
                          "string-spline-accel",
                          "string-spline-accel-player",
                          "string-max-length",
                          "string-min-length",
                          "string-max-height",
                          "string-min-height",
                          "gun-max-height",
                          "gun-min-height",
                          "head-offset",
                          "foot-offset",
                          "extra-follow-height",
                          "target-height",
                      };
                      auto argf = arg_forms.at(2);
                      arg_forms.at(2) = try_cast_simplify(argf, TypeSpec("float"), pool, env, true);
                      if (argsym != "'rel") {
                        if (use_degrees_settings.find(argset) != use_degrees_settings.end()) {
                          arg_forms.at(2) = try_cast_simplify(arg_forms.at(2), TypeSpec("degrees"),
                                                              pool, env, true);
                        } else if (use_meters_settings.find(argset) != use_meters_settings.end()) {
                          arg_forms.at(2) = try_cast_simplify(arg_forms.at(2), TypeSpec("meters"),
                                                              pool, env, true);
                        }
                      }
                      arg_forms.at(2)->parent_element = argf->parent_element;
                    }
                    // convert the int param
                    static const std::unordered_map<std::string, std::string> use_bitenum_settings =
                        {
                            {"process-mask", "process-mask"},
                            {"features", "game-feature"},
                            {"slave-options", "cam-slave-options"},
                            {"minimap", "minimap-flag"},
                            {"task-mask", "task-mask"},
                        };
                    static const std::unordered_map<std::string, std::string> use_enum_settings = {
                        {"sound-flava", "music-flava"},
                        {"exclusive-task", "game-task"},
                    };
                    auto argi = arg_forms.at(3);
                    auto argi_o = argi->to_form(env);
                    if (argi_o.is_int()) {
                      if (use_bitenum_settings.find(argset) != use_bitenum_settings.end()) {
                        auto en = env.dts->ts.try_enum_lookup(use_bitenum_settings.at(argset));
                        if (en) {
                          arg_forms.at(3) = cast_to_bitfield_enum(en, pool, env, argi_o.as_int());
                        }
                      } else if (use_enum_settings.find(argset) != use_enum_settings.end()) {
                        auto en = env.dts->ts.try_enum_lookup(use_enum_settings.at(argset));
                        if (en) {
                          arg_forms.at(3) = cast_to_int_enum(en, pool, env, argi_o.as_int());
                        }
                      }
                    }
                    arg_forms.at(3)->parent_element = argi->parent_element;
                  }
                }
              }
            } else if (env.func->process_stack_size > 0 && head_obj.is_symbol("stack-size-set!")) {
              // override process stack size
              auto old_size = arg_forms.at(1)->to_form(env);
              if (old_size.is_int()) {
                arg_forms.at(1) = pool.alloc_single_element_form<ConstantTokenElement>(
                    arg_forms.at(1)->parent_element, std::to_string(env.func->process_stack_size));
                env.func->warnings.info("Process stack size was changed from {} to {}",
                                        old_size.as_int(), env.func->process_stack_size);
              }
            }
          }

          new_form =
              pool.alloc_element<GenericElement>(GenericOperator::make_function(head), arg_forms);
          result->push_back(new_form);
          ASSERT(!go_next_state);
          return;
        }
      }
    }
  }

  // check for sound-play stuff
  {
    if (arg_forms.size() == 7 && unstacked.at(0)->to_form(env).is_symbol("sound-play-by-name")) {
      auto ssn = arg_forms.at(0)->to_string(env);
      static const std::string ssn_check = "(static-sound-name \"";
      // idk what a good way to do this is :(
      if (ssn.substr(0, ssn_check.size()) == ssn_check) {
        // get sound name
        auto sound_name = ssn.substr(ssn_check.size(), ssn.size() - ssn_check.size() - 2);

        // get sound id
        auto so_id_f = arg_forms.at(1);
        if (so_id_f->to_string(env) == "(new-sound-id)") {
          so_id_f = nullptr;
        }

        // get sound volume
        bool panic = false;
        auto so_vol_o = arg_forms.at(2)->to_form(env);
        Form* so_vol_f = nullptr;
        if (so_vol_o.is_int()) {
          auto vol_as_flt_temp = fixed_point_to_float(so_vol_o.as_int(), 1024) * 100;
          // fixed point convert is good but floating point accuracy sucks so we can do even better
          // with a hardcoded case
          for (int i = 0; i < 100; ++i) {
            if (int(i * 1024.0f / 100.0f) == so_vol_o.as_int()) {
              vol_as_flt_temp = i;
              break;
            }
          }
          // make the number now...
          if (int(vol_as_flt_temp * 1024.0f / 100.0f) == so_vol_o.as_int()) {
            if (so_vol_o.as_int() != 1024) {
              so_vol_f = pool.form<ConstantTokenElement>(float_to_string(vol_as_flt_temp, false));
            }
          }
        } else {
          auto mr_vol = match(
              Matcher::cast("int", Matcher::op_fixed(FixedOperatorKind::MULTIPLICATION,
                                                     {Matcher::single(10.24f), Matcher::any(0)})),
              arg_forms.at(2));
          if (mr_vol.matched) {
            so_vol_f = mr_vol.maps.forms.at(0);
          } else {
            // AAAHHHH i dont know how to handle this volume thing!
            panic = true;
          }
        }

        // get sound pitch mod
        auto so_pitch_o = arg_forms.at(3)->to_form(env);
        Form* so_pitch_f = nullptr;
        if (so_pitch_o.is_int()) {
          if (so_pitch_o.as_int() != 0) {
            so_pitch_f =
                pool.form<ConstantTokenElement>(fixed_point_to_string(so_pitch_o.as_int(), 1524));
          }
        } else {
          auto mr_pitch = match(
              Matcher::cast("int", Matcher::op_fixed(FixedOperatorKind::MULTIPLICATION,
                                                     {Matcher::single(1524), Matcher::any(0)})),
              arg_forms.at(3));
          if (mr_pitch.matched) {
            so_pitch_f = mr_pitch.maps.forms.at(0);
          } else {
            panic = true;
          }
        }

        // rest
        if (!panic) {
          auto so_bend = arg_forms.at(4);
          if (so_bend->to_form(env).is_int(0)) {
            so_bend = nullptr;
          }
          auto elt_group = arg_forms.at(5)->try_as_element<GenericElement>();
          if (elt_group && elt_group->op().is_func() &&
              elt_group->op().func()->to_form(env).is_symbol("sound-group")) {
            Form* so_group_f = nullptr;
            if (elt_group->elts().size() == 1 &&
                !elt_group->elts().at(0)->to_form(env).is_symbol("sfx")) {
              so_group_f = pool.form<ConstantTokenElement>(
                  elt_group->elts().at(0)->to_form(env).as_symbol().name_ptr);
            }
            auto so_positional_f = arg_forms.at(6);
            if (so_positional_f->to_form(env).is_symbol("#t")) {
              so_positional_f = nullptr;
            }
            // now make the macro call!
            std::vector<Form*> macro_args;
            macro_args.push_back(pool.form<StringConstantElement>(sound_name));
            if (so_id_f) {
              macro_args.push_back(pool.form<ConstantTokenElement>(":id"));
              macro_args.push_back(so_id_f);
            }
            if (so_vol_f) {
              macro_args.push_back(pool.form<ConstantTokenElement>(":vol"));
              macro_args.push_back(so_vol_f);
            }
            if (so_pitch_f) {
              macro_args.push_back(pool.form<ConstantTokenElement>(":pitch"));
              macro_args.push_back(so_pitch_f);
            }
            if (so_bend) {
              macro_args.push_back(pool.form<ConstantTokenElement>(":bend"));
              macro_args.push_back(so_bend);
            }
            if (so_group_f) {
              macro_args.push_back(pool.form<ConstantTokenElement>(":group"));
              macro_args.push_back(so_group_f);
            }
            if (so_positional_f) {
              macro_args.push_back(pool.form<ConstantTokenElement>(":position"));
              macro_args.push_back(so_positional_f);
            }

            new_form = pool.alloc_element<GenericElement>(
                GenericOperator::make_function(pool.form<ConstantTokenElement>("sound-play")),
                macro_args);
            result->push_back(new_form);
            return;
          }
        }
      }
    }
  }

  new_form = pool.alloc_element<GenericElement>(GenericOperator::make_function(unstacked.at(0)),
                                                arg_forms);

  {
    // detect method calls:
    // ex: ((method-of-type pair new) (quote global) pair gp-0 a3-0)
    constexpr int type_for_method = 0;
    constexpr int method_name = 1;

    auto deref_matcher = Matcher::op(
        GenericOpMatcher::fixed(FixedOperatorKind::METHOD_OF_TYPE),
        {Matcher::any_symbol(type_for_method), Matcher::any_constant_token(method_name)});

    auto matcher = Matcher::op_with_rest(GenericOpMatcher::func(deref_matcher), {});
    auto temp_form = pool.alloc_single_form(nullptr, new_form);
    auto match_result = match(matcher, temp_form);
    if (match_result.matched) {
      const auto type_1 = match_result.maps.strings.at(type_for_method);
      const auto& name = match_result.maps.strings.at(method_name);

      if (name == "new" && type_1 == "object") {
        // calling the new method of object. This is a special case that turns into an (object-new
        // macro. The arguments are allocation type-to-make and size of type
        // symbol, type, int.
        std::vector<Form*> new_args = dynamic_cast<GenericElement*>(new_form)->elts();

        // if needed, cast to to correct type.
        std::vector<TypeSpec> expected_arg_types = {TypeSpec("symbol"), TypeSpec("type"),
                                                    TypeSpec("int")};
        ASSERT(new_args.size() >= 3);
        for (size_t i = 0; i < 3; i++) {
          auto& var = all_pop_vars.at(i + 1);  // 0 is the function itself.
          auto arg_type = env.get_types_before_op(var.idx()).get(var.reg()).typespec();
          if (!env.dts->ts.tc(expected_arg_types.at(i), arg_type)) {
            new_args.at(i) = pool.form<CastElement>(expected_arg_types.at(i), new_args.at(i));
          }
        }

        auto new_op = pool.alloc_element<GenericElement>(
            GenericOperator::make_fixed(FixedOperatorKind::OBJECT_NEW), new_args);
        result->push_back(new_op);
        ASSERT(!go_next_state);
        return;
      }
      if (name == "new" && type_1 == "type") {
        std::vector<Form*> new_args = dynamic_cast<GenericElement*>(new_form)->elts();
        auto new_op = pool.alloc_element<GenericElement>(
            GenericOperator::make_fixed(FixedOperatorKind::TYPE_NEW), new_args);
        result->push_back(new_op);
        ASSERT(!go_next_state);
        return;
      } else if (name == "new") {
        constexpr int allocation = 2;
        constexpr int type_for_arg = 3;
        auto alloc_matcher = Matcher::any_quoted_symbol(allocation);
        auto type_arg_matcher = Matcher::any_symbol(type_for_arg);
        matcher = Matcher::op_with_rest(GenericOpMatcher::func(deref_matcher),
                                        {alloc_matcher, type_arg_matcher});
        match_result = match(matcher, temp_form);
        if (match_result.matched) {
          auto& alloc = match_result.maps.strings.at(allocation);
          if (alloc != "global" && alloc != "debug" && alloc != "process" &&
              alloc != "loading-level") {
            throw std::runtime_error("Unrecognized heap symbol for new: " + alloc);
          }
          auto type_2 = match_result.maps.strings.at(type_for_arg);
          if (type_1 != type_2) {
            throw std::runtime_error(
                fmt::format("Inconsistent types in method call: {} and {}", type_1, type_2));
          }

          if (type_2 == "array") {
            type_2 = "boxed-array";
          }

          auto quoted_type = pool.form<SimpleAtomElement>(SimpleAtom::make_sym_ptr(type_2));

          if (alloc == "global" && type_1 == "pair") {
            // cons!
            // (new 'global 'pair a b) -> (cons a b)
            std::vector<Form*> cons_args = {dynamic_cast<GenericElement*>(new_form)->elts().at(2),
                                            dynamic_cast<GenericElement*>(new_form)->elts().at(3)};
            auto cons_op = pool.alloc_element<GenericElement>(
                GenericOperator::make_fixed(FixedOperatorKind::CONS), cons_args);
            result->push_back(cons_op);
            ASSERT(!go_next_state);
            return;
          } else {
            // just normal construction on the heap
            std::vector<Form*> new_args = dynamic_cast<GenericElement*>(new_form)->elts();
            new_args.at(1) = quoted_type;

            auto new_op = pool.alloc_element<GenericElement>(
                GenericOperator::make_fixed(FixedOperatorKind::NEW), new_args);
            result->push_back(new_op);
            ASSERT(!go_next_state);
            return;
          }
        }
        // possible else case here to catch fixed-type new's in a nicer way
      }
    }
  }

  {
    // detect method calls:
    // ex: ((method-of-type x blah) arg...)
    constexpr int method_name = 0;
    constexpr int type_source = 1;

    auto deref_matcher =
        Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::METHOD_OF_TYPE),
                    {Matcher::any(type_source), Matcher::any_constant_token(method_name)});

    auto matcher = Matcher::op_with_rest(GenericOpMatcher::func(deref_matcher), {});
    auto temp_form = pool.alloc_single_form(nullptr, new_form);
    auto match_result = match(matcher, temp_form);
    if (match_result.matched) {
      const auto name = match_result.maps.strings.at(method_name);
      if (name != "new") {
        // only do these checks on non-new methods.  New methods are treated as functions because
        // they are never virtual and are never called like a method.
        if (tp_type.kind != TP_Type::Kind::NON_VIRTUAL_METHOD) {
          throw std::runtime_error(fmt::format(
              "Method internal mismatch. METHOD_OF_TYPE operator didn't get a NON_VIRTUAL_METHOD "
              "type. Got {} instead. {} {}",
              tp_type.print(), name, match_result.maps.forms.at(type_source)->to_string(env)));
        }
      }

      auto type_source_form = match_result.maps.forms.at(type_source);
      auto type_source_obj = type_source_form->to_form(env);

      bool is_res_lump = type_source_obj.is_symbol("res-lump");
      if (name == "get-property-value-float" && is_res_lump) {
        auto as_macro = handle_get_property_value_float(arg_forms, pool, env);
        if (as_macro) {
          result->push_back(as_macro);
          return;
        }
      } else if (name == "get-property-data" && is_res_lump) {
        auto as_macro = handle_get_property_data(arg_forms, pool, env);
        if (as_macro) {
          result->push_back(as_macro);
          return;
        }
      } else if (name == "get-property-struct" && is_res_lump) {
        auto as_macro = handle_get_property_struct(arg_forms, pool, env);
        if (as_macro) {
          result->push_back(as_macro);
          return;
        }
      } else if (name == "get-property-value" && is_res_lump) {
        auto as_macro = handle_get_property_value(arg_forms, pool, env);
        if (as_macro) {
          result->push_back(as_macro);
          return;
        }
      } else if (name == "eval!" && arg_forms.size() == 2 &&
                 type_source_obj.is_symbol("script-context")) {
        // jak 2 macro for eval'ing a "script"
        auto arg_context = arg_forms.at(0);  // the script context
        auto arg_script = arg_forms.at(1);   // the script itself
        auto mr_new_script_context =
            match(Matcher::op_fixed(FixedOperatorKind::NEW,
                                    {Matcher::constant_token("'stack"),
                                     Matcher::constant_token("'script-context"), Matcher::any(0),
                                     Matcher::any(1), Matcher::any(2)}),
                  arg_context);
        if (mr_new_script_context.matched) {
          // grab the arguments passed to the context alloc. nullptr = dont put in macro (uses
          // defaults)
          auto ctx_key = mr_new_script_context.maps.forms.at(0);  // a key given to the context
          auto ctx_proc =
              mr_new_script_context.maps.forms.at(1);  // the process the context's bound to
          auto ctx_vector = mr_new_script_context.maps.forms.at(2);  // some vector for positioning

          // default for arg is (process->ppointer self) [weird]
          auto mr_ctx_key = match(
              Matcher::op_fixed(FixedOperatorKind::PROCESS_TO_PPOINTER, {Matcher::s6()}), ctx_key);
          if (mr_ctx_key.matched) {
            ctx_key = nullptr;
          }
          // default for process arg is just self
          auto mr_ctx_proc = match(Matcher::s6(), ctx_proc);
          if (mr_ctx_proc.matched) {
            ctx_proc = nullptr;
          }
          // default for vector is just #f
          auto mr_ctx_vector = match(Matcher::cast("vector", Matcher::symbol("#f")), ctx_vector);
          if (mr_ctx_vector.matched) {
            ctx_vector = nullptr;
          }

          // build the macro!
          std::vector<Form*> macro_args;

          macro_args.push_back(arg_script);
          if (ctx_key) {
            macro_args.push_back(pool.form<ConstantTokenElement>(":key"));
            macro_args.push_back(ctx_key);
          }
          if (ctx_proc) {
            macro_args.push_back(pool.form<ConstantTokenElement>(":proc"));
            macro_args.push_back(ctx_proc);
          }
          if (ctx_vector) {
            macro_args.push_back(pool.form<ConstantTokenElement>(":vector"));
            macro_args.push_back(ctx_vector);
          }

          result->push_back(pool.alloc_element<GenericElement>(
              GenericOperator::make_function(pool.form<ConstantTokenElement>("script-eval")),
              macro_args));
          return;
        }
      }

      // if the type is the exact type of the argument, we want to build it into a method call
      if (type_source_obj.is_symbol(first_arg_type.base_type()) && name != "new") {
        if (env.dts->ts.should_use_virtual_methods(tp_type.method_from_type(),
                                                   tp_type.method_id())) {
          throw std::runtime_error(fmt::format(
              "Expected type {} method id {} to use virtual methods, but it didn't.  Set option "
              ":final in the deftype to disable virtual method calls",
              tp_type.method_from_type().print(), tp_type.method_id()));
        }
        auto method_op = pool.form<ConstantTokenElement>(name);
        auto gop = GenericOperator::make_function(method_op);

        result->push_back(pool.alloc_element<GenericElement>(gop, arg_forms));
        ASSERT(!go_next_state);
        return;
      }

      if (name == "new" && arg_forms.size() >= 2) {
        bool got_stack_new = true;
        // method
        // (the-as symbol (new 'stack-no-clear 'draw-context))
        //  draw-context
        auto first_cast = arg_forms.at(0)->try_as_element<CastElement>();
        if (!first_cast || first_cast->type() != TypeSpec("symbol")) {
          got_stack_new = false;
        }

        if (got_stack_new) {
          auto new_op = first_cast->source()->try_as_element<StackStructureDefElement>();
          if (!new_op || new_op->type().base_type() != type_source_form->to_string(env)) {
            got_stack_new = false;
          }
        }

        if (got_stack_new) {
          if (arg_forms.at(1)->to_string(env) != type_source_form->to_string(env)) {
            got_stack_new = false;
          }
        }

        if (got_stack_new) {
          std::vector<Form*> stack_new_args;
          stack_new_args.push_back(pool.form<ConstantTokenElement>("'stack"));
          if (type_source_form->to_string(env) == "array") {
            stack_new_args.push_back(pool.form<ConstantTokenElement>("'boxed-array"));
          } else {
            stack_new_args.push_back(pool.form<ConstantTokenElement>(
                fmt::format("'{}", type_source_form->to_string(env))));
          }
          for (size_t i = 2; i < arg_forms.size(); i++) {
            stack_new_args.push_back(arg_forms.at(i));
          }
          result->push_back(pool.alloc_element<GenericElement>(
              GenericOperator::make_fixed(FixedOperatorKind::NEW), stack_new_args));
          ASSERT(!go_next_state);
          return;
        }
      }

      auto method_op = pool.form<GetMethodElement>(type_source_form, name, false);
      auto gop = GenericOperator::make_function(method_op);

      result->push_back(pool.alloc_element<GenericElement>(gop, arg_forms));

      return;
    }
  }

  // detect launch-particles macro
  {
    if (unstacked.at(0)->to_form(env).is_symbol("sp-launch-particles-var")) {
      auto system = arg_forms.at(0);
      auto part = arg_forms.at(1);
      auto origin = arg_forms.at(2);
      auto launch_state = arg_forms.at(3);
      auto launch_control = arg_forms.at(4);
      auto rate = arg_forms.at(5);
      std::vector<Form*> macro;

      if (system->to_string(env) != "*sp-particle-system-2d*") {
        macro.push_back(pool.form<ConstantTokenElement>(":system"));
        macro.push_back(system);
      }
      macro.push_back(part);
      macro.push_back(origin);

      auto mr_launch_state =
          match(Matcher::cast("sparticle-launch-state", Matcher::symbol("#f")), launch_state);
      auto mr_launch_control =
          match(Matcher::cast("sparticle-launch-control", Matcher::symbol("#f")), launch_control);
      if (!mr_launch_state.matched) {
        macro.push_back(pool.form<ConstantTokenElement>(":launch-state"));
        macro.push_back(launch_state);
      }
      if (!mr_launch_control.matched) {
        macro.push_back(pool.form<ConstantTokenElement>(":launch-control"));
        macro.push_back(launch_control);
      }
      if (rate->to_string(env) != "1.0") {
        macro.push_back(pool.form<ConstantTokenElement>(":rate"));
        macro.push_back(rate);
      }

      if (env.version != GameVersion::Jak1) {
        macro.push_back(pool.form<ConstantTokenElement>(":origin-is-matrix"));
        macro.push_back(pool.form<ConstantTokenElement>("#t"));
      }

      new_form = pool.alloc_element<GenericElement>(
          GenericOperator::make_function(pool.form<ConstantTokenElement>("launch-particles")),
          macro);
    }
  }

  // detect call-parent-method
  {
    const auto& guessed_name = env.func->guessed_name;
    if (guessed_name.kind == FunctionName::FunctionKind::METHOD) {
      // detect stuff like: ((find-parent-method...) arg...)
      auto mr_find_parent =
          match(Matcher::func(Matcher::symbol("find-parent-method"),
                              {Matcher::symbol(env.func->method_of_type),
                               Matcher::integer(env.func->guessed_name.method_id)}),
                unstacked.at(0)

          );
      if (mr_find_parent.matched) {
        new_form = pool.alloc_element<GenericElement>(
            GenericOperator::make_function(pool.form<ConstantTokenElement>("call-parent-method")),
            arg_forms);
      }
    } else if (guessed_name.kind == FunctionName::FunctionKind::V_STATE && arg_forms.size() == 2) {
      // here, simply detect (find-parent-method...)
      //
      auto mr_find_parent = match(Matcher::symbol("find-parent-method"), unstacked.at(0));
      if (mr_find_parent.matched) {
        auto state_info =
            env.dts->ts.lookup_method(guessed_name.type_name, guessed_name.state_name);
        if (arg_forms.at(0)->to_string(env) == guessed_name.type_name &&
            arg_forms.at(1)->to_string(env) == std::to_string(state_info.id)) {
          new_form = pool.alloc_element<GenericElement>(
              GenericOperator::make_function(pool.form<ConstantTokenElement>("find-parent-state")));
        }
      }
    }
  }

  result->push_back(new_form);
}

void FunctionCallElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  std::vector<FormElement*> rewritten;
  update_from_stack(env, pool, stack, &rewritten, true);
  for (auto x : rewritten) {
    stack.push_form_element(x, true);
  }
}

///////////////////
// DerefElement
///////////////////
ConstantTokenElement* DerefElement::try_as_art_const(const Env& env, FormPool& pool) {
  auto mr = match(
      Matcher::deref(Matcher::s6(), false,
                     {DerefTokenMatcher::string("draw"), DerefTokenMatcher::string("art-group"),
                      DerefTokenMatcher::string("data"), DerefTokenMatcher::any_integer(0)}),
      this);

  if (mr.matched) {
    auto elt_name = env.get_art_elt_name(mr.maps.ints.at(0));
    if (elt_name) {
      return pool.alloc_element<ConstantTokenElement>(*elt_name);
    } else {
      lg::error("function `{}`: did not find art element {} in {}", env.func->name(),
                mr.maps.ints.at(0), env.art_group());
    }
  }

  return nullptr;
}

GenericElement* DerefElement::try_as_joint_node_index(const Env& env, FormPool& pool) {
  auto mr =
      match(Matcher::deref(Matcher::s6(), false,
                           {DerefTokenMatcher::string("node-list"),
                            DerefTokenMatcher::string("data"), DerefTokenMatcher::any_integer(0)}),
            this);

  if (mr.matched) {
    // lg::print("func {} joint-geo: {}\n", env.func->name(), env.joint_geo());
    auto info = env.dts->jg_info;
    std::vector<Form*> args;
    auto joint_name = env.get_joint_node_name(mr.maps.ints.at(0));
    args.push_back(pool.form<ConstantTokenElement>(env.joint_geo()));
    if (joint_name) {
      args.push_back(pool.form<ConstantTokenElement>(joint_name.value()));
      return pool.alloc_element<GenericElement>(
          GenericOperator::make_function(pool.form<ConstantTokenElement>("joint-node")), args);
    } else {
      lg::error("function `{}`: did not find joint node {} in {}", env.func->name(),
                mr.maps.ints.at(0), env.joint_geo());
    }
  }

  return nullptr;
}

GenericElement* DerefElement::try_as_curtime(const Env& env, FormPool& pool) {
  if (env.version == GameVersion::Jak1) {
    auto mr = match(Matcher::deref(Matcher::symbol("*display*"), false,
                                   {DerefTokenMatcher::string("base-frame-counter")}),
                    this);
    if (mr.matched) {
      return pool.alloc_element<GenericElement>(
          GenericOperator::make_function(pool.form<ConstantTokenElement>("current-time")));
    }
  } else {
    auto mr = match(Matcher::deref(Matcher::s6(), false,
                                   {DerefTokenMatcher::string("clock"),
                                    DerefTokenMatcher::string("frame-counter")}),
                    this);
    if (mr.matched) {
      return pool.alloc_element<GenericElement>(
          GenericOperator::make_function(pool.form<ConstantTokenElement>("current-time")));
    }
  }

  return nullptr;
}

GenericElement* DerefElement::try_as_seconds_per_frame(const Env& env, FormPool& pool) {
  if (env.version == GameVersion::Jak1) {
    auto mr = match(Matcher::deref(Matcher::symbol("*display*"), false,
                                   {DerefTokenMatcher::string("seconds-per-frame")}),
                    this);
    if (mr.matched) {
      return pool.alloc_element<GenericElement>(
          GenericOperator::make_function(pool.form<ConstantTokenElement>("seconds-per-frame")));
    }
  } else {
    auto mr = match(Matcher::deref(Matcher::s6(), false,
                                   {DerefTokenMatcher::string("clock"),
                                    DerefTokenMatcher::string("seconds-per-frame")}),
                    this);
    if (mr.matched) {
      return pool.alloc_element<GenericElement>(
          GenericOperator::make_function(pool.form<ConstantTokenElement>("seconds-per-frame")));
    }
  }
  return nullptr;
}

void DerefElement::update_from_stack(const Env& env,
                                     FormPool& pool,
                                     FormStack& stack,
                                     std::vector<FormElement*>* result,
                                     bool allow_side_effects) {
  mark_popped();
  // todo - update var tokens from stack?
  m_base->update_children_from_stack(env, pool, stack, allow_side_effects);

  // look for sym->str-ptr
  auto sym_str = m_base->try_as_element<GetSymbolStringPointer>();
  if (sym_str && m_tokens.size() == 1 && m_tokens.at(0).is_int(0)) {
    result->push_back(pool.alloc_element<GenericElement>(
        GenericOperator::make_fixed(FixedOperatorKind::SYMBOL_TO_STRING), sym_str->src()));
    return;
  }

  // merge nested ->'s
  inline_nested();

  auto as_art = try_as_art_const(env, pool);
  if (as_art) {
    result->push_back(as_art);
    return;
  }

  auto as_jnode = try_as_joint_node_index(env, pool);
  if (as_jnode) {
    result->push_back(as_jnode);
    return;
  }

  auto as_simple_expr = m_base->try_as_element<SimpleExpressionElement>();
  if (env.version >= GameVersion::Jak2 && as_simple_expr && as_simple_expr->expr().is_identity() &&
      as_simple_expr->expr().get_arg(0).is_sym_val() &&
      as_simple_expr->expr().get_arg(0).get_str() == "*game-info*" && m_tokens.size() >= 2 &&
      m_tokens.at(0).is_field_name("sub-task-list") && m_tokens.at(1).is_int()) {
    m_tokens.at(1) = DerefToken::make_int_expr(cast_to_int_enum(
        env.dts->ts.try_enum_lookup("game-task-node"), pool, env, m_tokens.at(1).int_constant()));
  }

  // current-time macro
  auto as_curtime = try_as_curtime(env, pool);
  if (as_curtime) {
    result->push_back(as_curtime);
    return;
  }

  // seconds-per-frame macro
  auto as_seconds_per_frame = try_as_seconds_per_frame(env, pool);
  if (as_seconds_per_frame) {
    result->push_back(as_seconds_per_frame);
    return;
  }

  result->push_back(this);
}

void DerefElement::inline_nested() {
  auto as_deref = dynamic_cast<DerefElement*>(m_base->try_as_single_element());
  if (as_deref) {
    if (!m_is_addr_of && !as_deref->is_addr_of()) {
      m_tokens.insert(m_tokens.begin(), as_deref->tokens().begin(), as_deref->tokens().end());
      m_base = as_deref->m_base;
    }
  }
}

///////////////////
// UntilElement
///////////////////

void UntilElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  // in asm:
  // LTOP:
  //  body
  //  condition
  //  jump to top
  // so we can end up getting the body/condition wrong.
  // the way the CfgPass works means that we put too much in condition.
  // we can safely move stuff from the top of condition to the bottom of body.

  mark_popped();

  std::vector<FormElement*> condition_to_body;
  {
    FormStack condition_temp_stack(false);
    for (auto& entry : condition->elts()) {
      entry->push_to_stack(env, pool, condition_temp_stack);
    }
    condition_to_body = condition_temp_stack.rewrite(pool, env);
    condition->clear();
    ASSERT(!condition_to_body.empty());
    condition->push_back(condition_to_body.back());
    condition_to_body.pop_back();
  }

  {
    FormStack body_temp_stack(false);
    for (auto& entry : body->elts()) {
      entry->push_to_stack(env, pool, body_temp_stack);
    }
    auto new_entries = body_temp_stack.rewrite(pool, env);
    body->clear();

    for (auto e : new_entries) {
      if (!dynamic_cast<EmptyElement*>(e)) {
        body->push_back(e);
      }
    }
    for (auto e : condition_to_body) {
      if (!dynamic_cast<EmptyElement*>(e)) {
        body->push_back(e);
      }
    }

    if (body->size() == 0) {
      body->push_back(pool.alloc_element<EmptyElement>());
    }
  }

  stack.push_form_element(this, true);
  if (false_destination) {
    env.func->warnings.warning("new jak 2 until loop case, check carefully");
    stack.push_value_to_reg(*false_destination,
                            pool.form<SimpleAtomElement>(SimpleAtom::make_sym_val("#f")), true,
                            TypeSpec("symbol"));
    RegAccessSet accessed_regs;
    body->collect_vars(accessed_regs, true);
    condition->collect_vars(accessed_regs, true);
    auto check_name = env.get_variable_name(*false_destination);
    for (auto& reg : accessed_regs) {
      if (env.get_variable_name(reg) == check_name) {
        ASSERT_MSG(false, fmt::format("Jak 2 loop uses delay slot variable improperly: {} {}\n",
                                      env.func->name(), check_name));
      }
    }
  }
}

void WhileElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  mark_popped();
  bool first = true;
  for (auto form : {body, condition}) {
    FormStack temp_stack(first && stack.is_root());
    first = false;
    for (auto& entry : form->elts()) {
      entry->push_to_stack(env, pool, temp_stack);
    }
    auto new_entries = temp_stack.rewrite(pool, env);
    form->clear();
    for (auto e : new_entries) {
      form->push_back(e);
    }
  }
  stack.push_form_element(this, true);
}

namespace {
// (if x (-> x ppointer)) -> (process->ppointer x)
Form* try_rewrite_as_process_to_ppointer(CondNoElseElement* value,
                                         FormStack& stack,
                                         FormPool& pool,
                                         const Env& env) {
  if (value->entries.size() != 1) {
    return nullptr;
  }

  auto condition = value->entries.at(0).condition;
  auto body = value->entries[0].body;

  // safe to look for a reg directly here.
  auto condition_matcher =
      Matcher::op(GenericOpMatcher::condition(IR2_Condition::Kind::TRUTHY), {Matcher::any_reg(0)});
  auto condition_mr = match(condition_matcher, condition);
  if (!condition_mr.matched) {
    return nullptr;
  }

  auto body_matcher =
      Matcher::deref(Matcher::any_reg(0), false, {DerefTokenMatcher::string("ppointer")});
  auto body_mr = match(body_matcher, body);

  if (!body_mr.matched) {
    return nullptr;
  }

  auto body_var = *body_mr.maps.regs.at(0);
  auto condition_var = *condition_mr.maps.regs.at(0);

  if (env.get_variable_name(body_var) != env.get_variable_name(condition_var)) {
    return nullptr;
  }

  // lg::print("Matched condition {} in {}\n", condition_var.to_string(env),
  // value->to_string(env));

  auto* menv = const_cast<Env*>(&env);
  menv->disable_use(body_var);
  auto repopped = stack.pop_reg(condition_var, {}, env, true);
  if (!repopped) {
    repopped = var_to_form(condition_var, pool);
  } else {
    if (!env.dts->ts.tc(TypeSpec("process"), env.get_variable_type(condition_var, true))) {
      repopped = pool.form<CastElement>(TypeSpec("process"), repopped);
    }
  }

  return pool.form<GenericElement>(
      GenericOperator::make_fixed(FixedOperatorKind::PROCESS_TO_PPOINTER), repopped);
}

// (if x (-> x 0 self)) -> (ppointer->process x)
Form* try_rewrite_as_pppointer_to_process(CondNoElseElement* value,
                                          FormStack& stack,
                                          FormPool& pool,
                                          const Env& env) {
  if (value->entries.size() != 1) {
    return nullptr;
  }

  auto condition = value->entries.at(0).condition;
  auto body = value->entries[0].body;

  // safe to look for a reg directly here.
  auto condition_matcher =
      Matcher::op(GenericOpMatcher::condition(IR2_Condition::Kind::TRUTHY), {Matcher::any_reg(0)});
  auto condition_mr = match(condition_matcher, condition);
  if (!condition_mr.matched) {
    return nullptr;
  }

  auto body_matcher =
      Matcher::deref(Matcher::any_reg(0), false,
                     {DerefTokenMatcher::integer(0), DerefTokenMatcher::string("self")});
  auto body_mr = match(body_matcher, body);

  if (!body_mr.matched) {
    return nullptr;
  }

  auto body_var = *body_mr.maps.regs.at(0);
  auto condition_var = *condition_mr.maps.regs.at(0);

  if (env.get_variable_name(body_var) != env.get_variable_name(condition_var)) {
    return nullptr;
  }

  // lg::print("Matched condition {} in {}\n", condition_var.to_string(env),
  // value->to_string(env));

  auto* menv = const_cast<Env*>(&env);
  menv->disable_use(body_var);
  auto repopped = stack.pop_reg(condition_var, {}, env, true);
  if (!repopped) {
    repopped = var_to_form(condition_var, pool);
  }

  return pool.form<GenericElement>(
      GenericOperator::make_fixed(FixedOperatorKind::PPOINTER_TO_PROCESS), repopped);
}

// (if (> (-> self skel active-channels) 0)
//   (-> self skel root-channel 0 frame-group)
//   )
// (ja-group :chan 0)
Form* try_rewrite_as_ja_group(CondNoElseElement* value,
                              FormStack& /*stack*/,
                              FormPool& pool,
                              const Env& env) {
  if (value->entries.size() != 1) {
    return nullptr;
  }

  auto condition = value->entries.at(0).condition;
  auto body = value->entries[0].body;

  // safe to look for a reg directly here.
  auto condition_matcher = Matcher::op_fixed(
      FixedOperatorKind::GT, {Matcher::deref(Matcher::s6(), false,
                                             {DerefTokenMatcher::string("skel"),
                                              DerefTokenMatcher::string("active-channels")}),
                              Matcher::any(0)});
  auto condition_mr = match(condition_matcher, condition);
  if (!condition_mr.matched) {
    return nullptr;
  }

  auto body_matcher = Matcher::deref(
      Matcher::s6(), false,
      {DerefTokenMatcher::string("skel"), DerefTokenMatcher::string("root-channel"),
       DerefTokenMatcher::any_expr_or_int(0), DerefTokenMatcher::string("frame-group")});
  auto body_mr = match(body_matcher, body);

  if (!body_mr.matched) {
    return nullptr;
  }

  auto chan = body_mr.int_or_form_to_form(pool, 0);
  auto obj_chan = chan->to_form(env);
  if (condition_mr.maps.forms.at(0)->to_form(env) == obj_chan) {
    auto func = GenericOperator::make_function(pool.form<ConstantTokenElement>("ja-group"));
    if (obj_chan.is_int(0)) {
      return pool.form<GenericElement>(func);
    } else {
      return pool.form<GenericElement>(func, pool.form<ConstantTokenElement>(":chan"),
                                       condition_mr.maps.forms.at(0));
    }
  }

  return nullptr;
}
}  // namespace

///////////////////
// CondNoElseElement
///////////////////
void CondNoElseElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  mark_popped();
  if (already_rewritten) {
    stack.push_form_element(this, true);
    return;
  }

  // the first condition is special
  auto first_condition = entries.front().condition;
  // lets evaluate in on the parent stack...
  for (auto x : first_condition->elts()) {
    x->push_to_stack(env, pool, stack);
  }

  RegisterAccess write_as_value = final_destination;
  bool first = true;
  for (auto& entry : entries) {
    for (auto form : {entry.condition, entry.body}) {
      if (form == first_condition) {
        form->clear();
        form->push_back(stack.pop_back(pool));
      } else {
        FormStack temp_stack(first && stack.is_root());
        first = false;
        for (auto& elt : form->elts()) {
          elt->push_to_stack(env, pool, temp_stack);
        }

        std::vector<FormElement*> new_entries;

        if (form == entry.body && used_as_value) {
          // try to advance us to the real write so we don't use the final_destination,
          // which may contain the wrong variable, but right register.
          std::optional<RegisterAccess> written_var;
          new_entries = rewrite_to_get_var(temp_stack, pool, final_destination, env, &written_var);
          if (written_var) {
            write_as_value = *written_var;
          }
        } else {
          new_entries = temp_stack.rewrite(pool, env);
        }

        form->clear();
        for (auto e : new_entries) {
          form->push_back(e);
        }
      }
    }
  }

  if (used_as_value) {
    // TODO - is this wrong?
    auto as_process_to_ppointer = try_rewrite_as_process_to_ppointer(this, stack, pool, env);
    if (as_process_to_ppointer) {
      stack.push_value_to_reg(write_as_value, as_process_to_ppointer, true,
                              env.get_variable_type(final_destination, false));
    } else {
      auto as_ppointer_to_process = try_rewrite_as_pppointer_to_process(this, stack, pool, env);
      if (as_ppointer_to_process) {
        stack.push_value_to_reg(write_as_value, as_ppointer_to_process, true,
                                env.get_variable_type(final_destination, false));
      } else {
        auto as_ja_group = try_rewrite_as_ja_group(this, stack, pool, env);
        if (as_ja_group) {
          stack.push_value_to_reg(write_as_value, as_ja_group, true,
                                  env.get_variable_type(final_destination, false));
        } else {
          //        lg::print("func {} final destination {} type {}\n", env.func->name(),
          //                   final_destination.to_string(env),
          //                   env.get_variable_type(final_destination, false).print());
          stack.push_value_to_reg(write_as_value, pool.alloc_single_form(nullptr, this), true,
                                  env.get_variable_type(final_destination, false));
        }
      }
    }

  } else {
    stack.push_form_element(this, true);
  }
  already_rewritten = true;
}

void CondWithElseElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  mark_popped();
  if (already_rewritten) {
    stack.push_form_element(this, true);
    return;
  }
  // first, let's try to detect if all bodies write the same value
  std::optional<RegisterAccess> last_var;
  bool rewrite_as_set = true;

  // the first condition is special
  auto first_condition = entries.front().condition;
  // lets evaluate in on the parent stack...
  for (auto x : first_condition->elts()) {
    x->push_to_stack(env, pool, stack);
  }

  // process conditions and bodies
  for (auto& entry : entries) {
    for (auto form : {entry.condition, entry.body}) {
      if (form == first_condition) {
        form->clear();
        form->push_back(stack.pop_back(pool));
      } else {
        FormStack temp_stack(false);
        if (form == entry.body) {
          auto as_setvar = dynamic_cast<SetVarElement*>(form->elts().back());
          if (as_setvar && as_setvar->is_dead_set() && as_setvar->src_type() != TypeSpec("float")) {
            rewrite_as_set = false;
          }
        }
        for (auto& elt : form->elts()) {
          elt->push_to_stack(env, pool, temp_stack);
        }

        std::vector<FormElement*> new_entries;
        new_entries = temp_stack.rewrite(pool, env);

        form->clear();
        for (auto e : new_entries) {
          form->push_back(e);
        }
      }
    }
  }

  // process else.
  FormStack temp_stack(false);
  for (auto& elt : else_ir->elts()) {
    elt->push_to_stack(env, pool, temp_stack);
  }

  std::vector<FormElement*> new_entries;
  new_entries = temp_stack.rewrite(pool, env);

  else_ir->clear();
  for (auto e : new_entries) {
    else_ir->push_back(e);
  }

  // collect all forms which should write the output.
  std::vector<Form*> write_output_forms;
  for (const auto& entry : entries) {
    write_output_forms.push_back(entry.body);
  }
  write_output_forms.push_back(else_ir);

  // check all to see if they write the value.
  std::vector<SetVarElement*> dest_sets;
  std::vector<TypeSpec> source_types;  // only explicit accesses that aren't move-eliminated

  int empty_count = 0;
  for (auto form : write_output_forms) {
    auto last_in_body = dynamic_cast<SetVarElement*>(form->elts().back());
    if (last_in_body) {
      dest_sets.push_back(last_in_body);
      if (last_var.has_value()) {
        if (last_var->reg() != last_in_body->dst().reg()) {
          rewrite_as_set = false;
          break;
        }
        // note: we use the dest type here because the rewrite will leave behind a cast to this.
        auto type = env.get_variable_type(last_in_body->dst(), true);
        source_types.push_back(type);
      }
      last_var = last_in_body->dst();
    }
    empty_count++;
  }

  if (empty_count > 0 && env.aggressively_reject_cond_to_value_rewrite) {
    rewrite_as_set = false;
  }

  if (!last_var.has_value()) {
    rewrite_as_set = false;
  }

  // determine if set destination is used
  bool set_unused = false;
  if (rewrite_as_set) {
    auto& info = env.reg_use().op.at(last_var->idx());
    if (info.written_and_unused.find(last_var->reg()) != info.written_and_unused.end()) {
      set_unused = true;
    }
  }

  // rewrite extra sets as needed.
  if (rewrite_as_set && !set_unused) {
    for (auto& entry : entries) {
      rewrite_to_get_var(entry.body->elts(), pool, *last_var, env);
      entry.body->claim_all_children();
    }
    rewrite_to_get_var(else_ir->elts(), pool, *last_var, env);
    else_ir->claim_all_children();
  }

  // update register info
  if (rewrite_as_set && !set_unused) {
    // might not be the same if a set is eliminated by a coloring move.
    // ASSERT(dest_sets.size() == write_output_forms.size());
    if (!dest_sets.empty()) {
      for (size_t i = 0; i < dest_sets.size() - 1; i++) {
        auto var = dest_sets.at(i)->dst();
        auto* env2 = const_cast<Env*>(&env);
        env2->disable_def(var, env2->func->warnings);
      }
    }
  }

  // merge conds in the else block.
  auto else_as_another_cond = else_ir->try_as_element<CondWithElseElement>();
  if (else_as_another_cond) {
    while (else_as_another_cond) {
      for (auto& e : else_as_another_cond->entries) {
        entries.push_back(e);
      }
      else_ir = else_as_another_cond->else_ir;
      else_as_another_cond = else_ir->try_as_element<CondWithElseElement>();
    }
  }

  if (rewrite_as_set) {
    if (set_unused) {
      stack.push_form_element(this, true);
    } else {
      // We may need to insert a cast here.

      // Note:
      // I think this might skip a cast if you have something like
      // (set! x (if y z (expr))) and z requires a cast, but the move from z to x is
      // eliminated by GOAL's register allocator.

      //      lg::print("func: {}\n", env.func->name());
      //
      //      lg::print("checking:\n");
      //      for (auto& t : source_types) {
      //        lg::print("  {}\n", t.print());
      //      }

      auto expected_type = env.get_variable_type(*last_var, true);
      //       lg::print("The expected type is {}\n", expected_type.print());
      auto result_type =
          source_types.empty() ? expected_type : env.dts->ts.lowest_common_ancestor(source_types);
      //       lg::print("but we actually got {}\n", result_type.print());

      Form* result_value = pool.alloc_single_form(nullptr, this);
      if (!env.dts->ts.tc(expected_type, result_type)) {
        result_value = pool.form<CastElement>(expected_type, result_value);
      }
      //      lg::print("{}\n", result_value->to_string(env));

      stack.push_value_to_reg(*last_var, result_value, true,
                              env.get_variable_type(*last_var, false));
    }
  } else {
    stack.push_form_element(this, true);
  }
  already_rewritten = true;
}

///////////////////
// ShortCircuitElement
///////////////////

FormElement* sc_to_handle_get_proc(ShortCircuitElement* elt,
                                   const Env& env,
                                   FormPool& pool,
                                   FormStack& stack) {
  if (elt->kind != ShortCircuitElement::AND) {
    return nullptr;
  }

  if (elt->entries.size() < 2) {
    return nullptr;
  }

  Form* last = elt->entries[elt->entries.size() - 1].condition;
  Form* second_to_last = elt->entries[elt->entries.size() - 2].condition;

  auto result = last_two_in_and_to_handle_get_proc(second_to_last, last, env, pool, stack,
                                                   elt->entries.size() > 2);
  if (!result) {
    return nullptr;
  }

  if (elt->entries.size() == 2) {
    // just replace the whole thing
    return result;
  } else {
    elt->entries.pop_back();
    elt->entries.back().condition = pool.alloc_single_form(elt, result);
  }

  return nullptr;
}

void ShortCircuitElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  mark_popped();
  if (already_rewritten) {
    stack.push_form_element(this, true);
    return;
  }

  // the first condition is special
  auto first_condition = entries.front().condition;
  // lets evaluate in on the parent stack...
  for (auto x : first_condition->elts()) {
    x->push_to_stack(env, pool, stack);
  }

  for (int i = 0; i < int(entries.size()); i++) {
    auto& entry = entries.at(i);
    if (entry.condition == first_condition) {
      entry.condition->clear();
      entry.condition->push_back(stack.pop_back(pool));
    } else {
      FormStack temp_stack(false);
      for (auto& elt : entry.condition->elts()) {
        elt->push_to_stack(env, pool, temp_stack);
      }

      std::vector<FormElement*> new_entries;
      if (i == int(entries.size()) - 1) {
        new_entries = rewrite_to_get_var(temp_stack, pool, final_result, env);
      } else {
        new_entries = temp_stack.rewrite(pool, env);
      }

      entry.condition->clear();
      for (auto e : new_entries) {
        if (dynamic_cast<EmptyElement*>(e)) {
          continue;
        }
        entry.condition->push_back(e);
      }
      if (entry.condition->elts().empty()) {
        entry.condition->push_back(pool.alloc_element<EmptyElement>());
      }
    }
  }

  FormElement* to_push = this;
  auto as_handle_get = sc_to_handle_get_proc(this, env, pool, stack);
  if (as_handle_get) {
    to_push = as_handle_get;
  }

  ASSERT(used_as_value.has_value());
  stack.push_value_to_reg(final_result, pool.alloc_single_form(nullptr, to_push), true,
                          env.get_variable_type(final_result, false));
  already_rewritten = true;
}

void ShortCircuitElement::update_from_stack(const Env& env,
                                            FormPool& pool,
                                            FormStack& stack,
                                            std::vector<FormElement*>* result,
                                            bool) {
  mark_popped();
  (void)stack;
  if (already_rewritten) {
    result->push_back(this);
    return;
  }
  for (int i = 0; i < int(entries.size()); i++) {
    auto& entry = entries.at(i);
    FormStack temp_stack(false);
    for (auto& elt : entry.condition->elts()) {
      elt->push_to_stack(env, pool, temp_stack);
    }

    std::vector<FormElement*> new_entries;
    if (i == int(entries.size()) - 1) {
      new_entries = rewrite_to_get_var(temp_stack, pool, final_result, env);
    } else {
      new_entries = temp_stack.rewrite(pool, env);
    }

    entry.condition->clear();
    for (auto e : new_entries) {
      entry.condition->push_back(e);
    }
  }
  result->push_back(this);
  already_rewritten = true;
}

namespace {
Matcher make_int_uint_cast_matcher(const Matcher& thing) {
  return Matcher::match_or({Matcher::cast("uint", thing), Matcher::cast("int", thing), thing});
}
}  // namespace

///////////////////
// ConditionElement
///////////////////

namespace {
Form* try_make_constant_from_int_for_compare(s64 value,
                                             const TypeSpec& type,
                                             FormPool& pool,
                                             const Env& env) {
  auto enum_type_info = env.dts->ts.try_enum_lookup(type);
  if (enum_type_info) {
    if (enum_type_info->is_bitfield()) {
      if (value != 0) {
        // prefer (zero? x) for bitfield enums.
        return cast_to_bitfield_enum(enum_type_info, pool, env, value);
      }
    } else {
      return cast_to_int_enum(enum_type_info, pool, env, value);
    }
  }
  return nullptr;
}

std::vector<Form*> cast_to_64_bit(const std::vector<Form*>& forms,
                                  const std::vector<TypeSpec>& types,
                                  FormPool& pool,
                                  const Env& env) {
  std::vector<Form*> result;
  for (size_t i = 0; i < forms.size(); i++) {
    if (env.dts->ts.tc(TypeSpec("uint128"), types.at(i))) {
      result.push_back(cast_form(forms[i], TypeSpec("uint"), pool, env));
    } else if (env.dts->ts.tc(TypeSpec("int128"), types.at(i))) {
      result.push_back(cast_form(forms[i], TypeSpec("int"), pool, env));
    } else {
      result.push_back(forms[i]);
    }
  }
  return result;
}

std::vector<Form*> cast_for_64bit_equality_check(const std::vector<Form*>& forms,
                                                 const std::vector<TypeSpec>& types,
                                                 FormPool& pool,
                                                 const Env& env) {
  if (env.version == GameVersion::Jak1) {
    return cast_to_64_bit(forms, types, pool, env);
  }
  std::vector<Form*> result;
  for (size_t i = 0; i < forms.size(); i++) {
    if (env.dts->ts.tc(TypeSpec("uint128"), types.at(i))) {
      result.push_back(cast_form(forms[i], TypeSpec("uint"), pool, env));
    } else if (env.dts->ts.tc(TypeSpec("int128"), types.at(i))) {
      result.push_back(cast_form(forms[i], TypeSpec("int"), pool, env));
    } else if (env.dts->ts.tc(TypeSpec("float"), types.at(i))) {
      result.push_back(cast_form(forms[i], TypeSpec("int"), pool, env));
    } else {
      result.push_back(forms[i]);
    }
  }
  return result;
}

FormElement* try_make_nonzero_logtest(Form* in, FormPool& pool) {
  /*
 (defmacro logtest? (a b)
   "does a have any of the bits in b?"
   `(nonzero? (logand ,a ,b))
   )
 */
  auto static const logand_matcher = Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::LOGAND),
                                                 {Matcher::any(0), Matcher::any(1)});
  auto mr_logand = match(logand_matcher, in);
  if (mr_logand.matched) {
    return pool.alloc_element<GenericElement>(
        GenericOperator::make_fixed(FixedOperatorKind::LOGTEST), mr_logand.maps.forms.at(0),
        mr_logand.maps.forms.at(1));
  }
  return nullptr;
}

FormElement* try_make_focus_test_macro(Form* in, FormPool& pool) {
  /*
(defmacro focus-test? (pfoc &rest status)
`(logtest? (-> (the process-focusable ,pfoc) focus-status) (focus-status ,@status)))

pfoc first:
(logtest? (-> self focus-status) (focus-status dead hit grabbed))

enum first:
(logtest? (focus-status mech) (-> a1-2 focus-status))
  */
  auto logtest_focus_matcher_pfoc = Matcher::op(
      GenericOpMatcher::fixed(FixedOperatorKind::LOGTEST),
      {Matcher::deref(Matcher::any(0), false, {DerefTokenMatcher::string("focus-status")}),
       Matcher::op_with_rest(GenericOpMatcher::func(Matcher::constant_token("focus-status")), {})});
  auto logtest_focus_matcher_enum = Matcher::op(
      GenericOpMatcher::fixed(FixedOperatorKind::LOGTEST),
      {
          Matcher::op_with_rest(GenericOpMatcher::func(Matcher::constant_token("focus-status")),
                                {}),
          Matcher::deref(Matcher::any(0), false, {DerefTokenMatcher::string("focus-status")}),
      });
  auto mr_logtest_pfoc = match(logtest_focus_matcher_pfoc, in);
  auto mr_logtest_enum = match(logtest_focus_matcher_enum, in);
  if (mr_logtest_pfoc.matched) {
    auto logtest_elt = dynamic_cast<GenericElement*>(in->at(0));
    if (logtest_elt != nullptr) {
      auto focus_form = logtest_elt->elts().at(1);
      std::vector<Form*> macro;
      macro.push_back(mr_logtest_pfoc.maps.forms.at(0));
      auto* focus = dynamic_cast<GenericElement*>(focus_form->at(0));
      if (focus) {
        macro.insert(macro.end(), focus->elts().begin(), focus->elts().end());
      }

      return pool.alloc_element<GenericElement>(
          GenericOperator::make_fixed(FixedOperatorKind::FOCUS_TEST), macro);
    }
  }
  if (mr_logtest_enum.matched) {
    auto logtest_elt = dynamic_cast<GenericElement*>(in->at(0));
    if (logtest_elt != nullptr) {
      auto focus_form = logtest_elt->elts().at(0);
      std::vector<Form*> macro;
      macro.push_back(mr_logtest_enum.maps.forms.at(0));
      auto* focus = dynamic_cast<GenericElement*>(focus_form->at(0));
      if (focus) {
        macro.insert(macro.end(), focus->elts().begin(), focus->elts().end());
      }

      return pool.alloc_element<GenericElement>(
          GenericOperator::make_fixed(FixedOperatorKind::FOCUS_TEST), macro);
    }
  }
  return nullptr;
}

FormElement* try_make_logtest_mouse_macro(Form* in, FormPool& pool) {
  /*
(defmacro mouse-pressed ()
  `(-> *mouse* button0-rel 0)
  )

(defmacro mouse-hold ()
  `(-> *mouse* button0-abs 0)
  )

(defmacro mouse-pressed? (&rest buttons)
  `(logtest? (mouse-pressed) (mouse-buttons ,@buttons))
  )

(defmacro mouse-hold? (&rest buttons)
  `(logtest? (mouse-hold) (mouse-buttons ,@buttons))
  )
 */
  auto static const mouse_matcher = Matcher::op(
      GenericOpMatcher::fixed(FixedOperatorKind::LOGTEST),
      {Matcher::deref(Matcher::symbol("*mouse*"), false,
                      {DerefTokenMatcher::any_string(2), DerefTokenMatcher::integer(0)}),
       Matcher::op_with_rest(GenericOpMatcher::func(Matcher::constant_token("mouse-buttons")),
                             {})});
  auto static const mouse_matcher_inv = Matcher::op(
      GenericOpMatcher::fixed(FixedOperatorKind::LOGTEST),
      {Matcher::op_with_rest(GenericOpMatcher::func(Matcher::constant_token("mouse-buttons")), {}),
       Matcher::deref(Matcher::symbol("*mouse*"), false,
                      {DerefTokenMatcher::any_string(2), DerefTokenMatcher::integer(0)})});

  bool inv_match = false;
  auto mr = match(mouse_matcher, in);
  if (!mr.matched) {
    mr = match(mouse_matcher_inv, in);
    inv_match = true;
  }
  if (mr.matched) {
    enum { ABS, REL, NIL } t = NIL;
    if (mr.maps.strings.at(2) == "button0-abs") {
      t = ABS;
    } else if (mr.maps.strings.at(2) == "button0-rel") {
      t = REL;
    }

    if (t != NIL) {
      auto logtest_elt = dynamic_cast<GenericElement*>(in->at(0));
      if (logtest_elt) {
        auto buttons_form = logtest_elt->elts().at(inv_match ? 0 : 1);
        std::vector<Form*> v;
        GenericElement* butts =
            dynamic_cast<GenericElement*>(buttons_form->at(0));  // the form with the buttons itself
        if (butts) {
          v = butts->elts();
        }

        return pool.alloc_element<GenericElement>(
            GenericOperator::make_fixed(t == ABS ? FixedOperatorKind::MOUSE_HOLD_P
                                                 : FixedOperatorKind::MOUSE_PRESSED_P),
            v);
      }
    }
  }
  return nullptr;
}

FormElement* try_make_logtest_cpad_macro(Form* in, FormPool& pool) {
  /*
(defmacro cpad-pressed (pad-idx)
  `(-> *cpad-list* cpads ,pad-idx button0-rel 0)
  )

(defmacro cpad-hold (pad-idx)
  `(-> *cpad-list* cpads ,pad-idx button0-abs 0)
  )

(defmacro cpad-pressed? (pad-idx &rest buttons)
  `(logtest? (cpad-pressed ,pad-idx) (pad-buttons ,@buttons))
  )

(defmacro cpad-hold? (pad-idx &rest buttons)
  `(logtest? (cpad-hold ,pad-idx) (pad-buttons ,@buttons))
  )
 */
  auto static const cpad_matcher = Matcher::op(
      GenericOpMatcher::fixed(FixedOperatorKind::LOGTEST),
      {Matcher::deref(Matcher::symbol("*cpad-list*"), false,
                      {DerefTokenMatcher::string("cpads"), DerefTokenMatcher::any_expr_or_int(0),
                       DerefTokenMatcher::any_string(2), DerefTokenMatcher::integer(0)}),
       Matcher::op_with_rest(GenericOpMatcher::func(Matcher::constant_token("pad-buttons")), {})});
  auto static const cpad_matcher_inv = Matcher::op(
      GenericOpMatcher::fixed(FixedOperatorKind::LOGTEST),
      {Matcher::op_with_rest(GenericOpMatcher::func(Matcher::constant_token("pad-buttons")), {}),
       Matcher::deref(Matcher::symbol("*cpad-list*"), false,
                      {DerefTokenMatcher::string("cpads"), DerefTokenMatcher::any_expr_or_int(0),
                       DerefTokenMatcher::any_string(2), DerefTokenMatcher::integer(0)})});

  bool inv_match = false;
  auto mr = match(cpad_matcher, in);
  if (!mr.matched) {
    mr = match(cpad_matcher_inv, in);
    inv_match = true;
  }
  if (mr.matched) {
    enum { ABS, REL, NIL } t = NIL;
    if (mr.maps.strings.at(2) == "button0-abs") {
      t = ABS;
    } else if (mr.maps.strings.at(2) == "button0-rel") {
      t = REL;
    }

    if (t != NIL) {
      auto logtest_elt = dynamic_cast<GenericElement*>(in->at(0));
      if (logtest_elt) {
        auto buttons_form = logtest_elt->elts().at(inv_match ? 0 : 1);
        std::vector<Form*> v;
        v.push_back(mr.int_or_form_to_form(pool, 0));
        GenericElement* butts =
            dynamic_cast<GenericElement*>(buttons_form->at(0));  // the form with the buttons itself
        if (butts) {
          v.insert(v.end(), butts->elts().begin(), butts->elts().end());
        }

        return pool.alloc_element<GenericElement>(
            GenericOperator::make_fixed(t == ABS ? FixedOperatorKind::CPAD_HOLD_P
                                                 : FixedOperatorKind::CPAD_PRESSED_P),
            v);
      }
    }
  }
  return nullptr;
}

FormElement* convert_logtest_to_fancy_macro(FormPool& pool, Form* logtest_form) {
  auto as_cpad_macro = try_make_logtest_cpad_macro(logtest_form, pool);
  if (as_cpad_macro) {
    return as_cpad_macro;
  }
  auto as_mouse_macro = try_make_logtest_mouse_macro(logtest_form, pool);
  if (as_mouse_macro) {
    return as_mouse_macro;
  }
  auto focus_test_macro = try_make_focus_test_macro(logtest_form, pool);
  if (focus_test_macro) {
    return focus_test_macro;
  }
  return nullptr;
}
}  // namespace

FormElement* ConditionElement::make_zero_check_generic(const Env& env,
                                                       FormPool& pool,
                                                       const std::vector<Form*>& source_forms,
                                                       const std::vector<TypeSpec>& source_types) {
  // (zero? (+ thing small-integer)) -> (= thing (- small-integer))
  ASSERT(source_forms.size() == 1);

  auto enum_type_info = env.dts->ts.try_enum_lookup(source_types.at(0));
  if (enum_type_info && !enum_type_info->is_bitfield()) {
    // (zero? (+ (the-as uint arg0) (the-as uint -2))) check enum value
    auto mr = match(
        Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::ADDITION),
                    {make_int_uint_cast_matcher(Matcher::any(0)),
                     Matcher::match_or({Matcher::any_integer(1),
                                        make_int_uint_cast_matcher(Matcher::any_integer(1))})}),
        source_forms.at(0));
    if (mr.matched) {
      s64 value = mr.maps.ints.at(1);
      value = -value;
      auto enum_constant = cast_to_int_enum(enum_type_info, pool, env, value);
      return pool.alloc_element<GenericElement>(
          GenericOperator::make_fixed(FixedOperatorKind::EQ),
          std::vector<Form*>{mr.maps.forms.at(0), enum_constant});
    }
  }

  {
    auto mr = match(Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::ADDITION),
                                {Matcher::any(0), Matcher::any_integer(1)}),
                    source_forms.at(0));
    if (mr.matched) {
      s64 value = -mr.maps.ints.at(1);
      auto value_form = pool.form<SimpleAtomElement>(SimpleAtom::make_int_constant(value));
      return pool.alloc_element<GenericElement>(
          GenericOperator::make_fixed(FixedOperatorKind::EQ),
          std::vector<Form*>{mr.maps.forms.at(0), value_form});
    }
  }

  auto nice_constant = try_make_constant_from_int_for_compare(0, source_types.at(0), pool, env);
  if (nice_constant) {
    return pool.alloc_element<GenericElement>(
        GenericOperator::make_fixed(FixedOperatorKind::EQ),
        std::vector<Form*>{source_forms.at(0), nice_constant});
  }

  auto as_logtest = try_make_nonzero_logtest(source_forms.at(0), pool);
  if (as_logtest) {
    auto logtest_form = pool.alloc_single_form(nullptr, as_logtest);
    auto fancy_form = convert_logtest_to_fancy_macro(pool, logtest_form);
    if (fancy_form) {
      return pool.alloc_element<GenericElement>(
          GenericOperator::make_compare(IR2_Condition::Kind::FALSE),
          pool.alloc_single_form(nullptr, fancy_form));
    }
    return pool.alloc_element<GenericElement>(
        GenericOperator::make_compare(IR2_Condition::Kind::FALSE), logtest_form);
  }

  return pool.alloc_element<GenericElement>(GenericOperator::make_compare(m_kind), source_forms);
}

FormElement* ConditionElement::make_nonzero_check_generic(const Env& env,
                                                          FormPool& pool,
                                                          const std::vector<Form*>& source_forms,
                                                          const std::vector<TypeSpec>&) {
  // for (nonzero? (-> obj bitfield))
  FormElement* bitfield_compare = nullptr;
  ASSERT(source_forms.size() == 1);
  auto as_bitfield_op =
      dynamic_cast<BitfieldAccessElement*>(source_forms.at(0)->try_as_single_element());
  if (as_bitfield_op) {
    bitfield_compare = as_bitfield_op->push_step(
        BitfieldManip(BitfieldManip::Kind::NONZERO_COMPARE, 0), env.dts->ts, pool, env);
  }

  if (bitfield_compare) {
    return bitfield_compare;
  }

  auto mr = match(Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::ADDITION),
                              {Matcher::any(0), Matcher::any_integer(1)}),
                  source_forms.at(0));
  if (mr.matched) {
    s64 value = -mr.maps.ints.at(1);
    auto value_form = pool.form<SimpleAtomElement>(SimpleAtom::make_int_constant(value));
    return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::NEQ),
                                              std::vector<Form*>{mr.maps.forms.at(0), value_form});
  }

  auto as_logtest = try_make_nonzero_logtest(source_forms.at(0), pool);
  if (as_logtest) {
    auto logtest_form = pool.alloc_single_form(nullptr, as_logtest);
    auto fancy_form = convert_logtest_to_fancy_macro(pool, logtest_form);
    if (fancy_form) {
      return fancy_form;
    } else {
      return as_logtest;
    }
  }

  return pool.alloc_element<GenericElement>(GenericOperator::make_compare(m_kind), source_forms);
}

FormElement* ConditionElement::make_equal_check_generic(const Env& env,
                                                        FormPool& pool,
                                                        const std::vector<Form*>& source_forms,
                                                        const std::vector<TypeSpec>& source_types) {
  ASSERT(source_forms.size() == 2);
  // (= thing '())
  auto ref = source_forms.at(1);
  auto ref_atom = form_as_atom(ref);
  if (ref_atom && ref_atom->get_kind() == SimpleAtom::Kind::EMPTY_LIST) {
    // null?
    return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::NULLP),
                                              source_forms.at(0));
  } else {
    auto nice_constant =
        try_make_constant_for_compare(source_forms.at(1), source_types.at(0), pool, env);
    if (nice_constant) {
      auto forms_with_cast = source_forms;
      forms_with_cast.at(1) = nice_constant;
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::EQ),
                                                forms_with_cast);
    } else {
      nice_constant =
          try_make_constant_for_compare(source_forms.at(0), source_types.at(1), pool, env);
      if (nice_constant) {
        auto forms_with_cast = source_forms;
        forms_with_cast.at(0) = nice_constant;
        return pool.alloc_element<GenericElement>(
            GenericOperator::make_fixed(FixedOperatorKind::EQ), forms_with_cast);
      } else {
        // (= (ja-group)
        //    (-> self draw art-group data 7)
        //    )
        // actually (ja-group? (-> self draw art-group data 7) :channel channel)
        auto mr =
            match(Matcher::op(GenericOpMatcher::func(Matcher::constant_token("ja-group")), {}),
                  source_forms.at(0));
        // check if both things matched
        if (mr.matched) {
          // grab args from the ja-group and pass them on
          std::vector<Form*> macro_args;
          macro_args.push_back(source_forms.at(1));

          auto jagroup = source_forms.at(0)->try_as_element<GenericElement>();
          for (size_t i = 1; i < jagroup->elts().size(); ++i) {
            macro_args.push_back(jagroup->elts().at(i));
          }
          return pool.alloc_element<GenericElement>(
              GenericOperator::make_function(pool.form<ConstantTokenElement>("ja-group?")),
              macro_args);
        }
      }
      return pool.alloc_element<GenericElement>(
          GenericOperator::make_fixed(FixedOperatorKind::EQ),
          cast_for_64bit_equality_check(source_forms, source_types, pool, env));
    }
  }
}

FormElement* ConditionElement::make_not_equal_check_generic(
    const Env& env,
    FormPool& pool,
    const std::vector<Form*>& source_forms,
    const std::vector<TypeSpec>& source_types) {
  ASSERT(source_forms.size() == 2);
  // (!= thing '())
  auto ref = source_forms.at(1);
  auto ref_atom = form_as_atom(ref);
  if (ref_atom && ref_atom->get_kind() == SimpleAtom::Kind::EMPTY_LIST) {
    // null?
    return pool.alloc_element<GenericElement>(
        GenericOperator::make_compare(IR2_Condition::Kind::FALSE),
        pool.form<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::NULLP),
                                  source_forms.at(0)));
  } else {
    auto nice_constant =
        try_make_constant_for_compare(source_forms.at(1), source_types.at(0), pool, env);
    if (nice_constant) {
      auto forms_with_cast = source_forms;
      forms_with_cast.at(1) = nice_constant;
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::NEQ),
                                                forms_with_cast);
    } else {
      return pool.alloc_element<GenericElement>(
          GenericOperator::make_fixed(FixedOperatorKind::NEQ),
          cast_for_64bit_equality_check(source_forms, source_types, pool, env));
    }
  }
}

FormElement* ConditionElement::make_less_than_zero_signed_check_generic(
    const Env& env,
    FormPool& pool,
    const std::vector<Form*>& source_forms,
    const std::vector<TypeSpec>& types) {
  ASSERT(source_forms.size() == 1);
  // (< (shl (the-as int iter) 62) 0) -> (pair? iter)

  // match (shl [(the-as int [x]) | [x]] 62)
  auto shift_match =
      match(Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::SHL),
                        {
                            Matcher::match_or({Matcher::cast("int", Matcher::any(0)),
                                               Matcher::any(0)}),  // the val
                            Matcher::integer(62)  // get the bit in the highest position.
                        }),
            source_forms.at(0));

  if (shift_match.matched) {
    return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::PAIRP),
                                              shift_match.maps.forms.at(0));
  } else {
    auto casted = make_casts_if_needed(source_forms, types, TypeSpec("int"), pool, env);
    auto zero = pool.form<SimpleAtomElement>(SimpleAtom::make_int_constant(0));
    casted.push_back(zero);
    return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::LT),
                                              casted);
  }
}

FormElement* ConditionElement::make_geq_zero_signed_check_generic(
    const Env& env,
    FormPool& pool,
    const std::vector<Form*>& source_forms,
    const std::vector<TypeSpec>& types) {
  ASSERT(source_forms.size() == 1);
  // (>= (shl (the-as int iter) 62) 0) -> (not (pair? iter))

  // match (shl [(the-as int [x]) | [x]] 62)
  auto shift_match =
      match(Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::SHL),
                        {
                            Matcher::match_or({Matcher::cast("int", Matcher::any(0)),
                                               Matcher::any(0)}),  // the val
                            Matcher::integer(62)  // get the bit in the highest position.
                        }),
            source_forms.at(0));

  if (shift_match.matched) {
    return pool.alloc_element<GenericElement>(
        GenericOperator::make_compare(IR2_Condition::Kind::FALSE),
        pool.form<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::PAIRP),
                                  shift_match.maps.forms.at(0)));
  } else {
    auto casted = make_casts_if_needed(source_forms, types, TypeSpec("int"), pool, env);
    auto zero = pool.form<SimpleAtomElement>(SimpleAtom::make_int_constant(0));
    casted.push_back(zero);
    return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::GEQ),
                                              casted);
  }
}

FormElement* ConditionElement::make_geq_zero_unsigned_check_generic(
    const Env& env,
    FormPool& pool,
    const std::vector<Form*>& source_forms,
    const std::vector<TypeSpec>& types) {
  ASSERT(source_forms.size() == 1);
  // (>= (shl (the-as int iter) 62) 0) -> (not (pair? iter))

  // match (shl [(the-as int [x]) | [x]] 62)
  auto shift_match =
      match(Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::SHL),
                        {
                            Matcher::match_or({Matcher::cast("uint", Matcher::any(0)),
                                               Matcher::any(0)}),  // the val
                            Matcher::integer(62)  // get the bit in the highest position.
                        }),
            source_forms.at(0));

  auto casted = make_casts_if_needed(source_forms, types, TypeSpec("uint"), pool, env);
  auto zero = pool.form<SimpleAtomElement>(SimpleAtom::make_int_constant(0));
  casted.push_back(zero);
  return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::GEQ),
                                            casted);
}

FormElement* ConditionElement::make_time_elapsed(const Env& env,
                                                 FormPool& pool,
                                                 const std::vector<Form*>& source_forms,
                                                 const std::vector<TypeSpec>& types) {
  // geq case:
  // (>= (- (current-time) (-> self state-time)) (seconds 5))
  // to
  // (time-elapsed? (-> self state-time) (seconds 5))

  // lt case:
  // (< (- (current-time) (-> self state-time)) (seconds 5))
  // to
  // (not (time-elapsed? (-> self state-time) (seconds 5)))
  auto matcher = match(
      Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::SUBTRACTION),
                  {Matcher::op(GenericOpMatcher::func(Matcher::constant_token("current-time")), {}),
                   Matcher::any(0)}),
      source_forms.at(0));
  if (matcher.matched) {
    auto time_elapsed = matcher.maps.forms.at(0);
    auto time = source_forms.at(1);
    std::vector<Form*> args;
    args.push_back(time_elapsed);
    args.push_back(time);
    // TODO - how to handle unsigned case?
    if (m_kind == IR2_Condition::Kind::LESS_THAN_SIGNED) {
      return pool.alloc_element<GenericElement>(
          GenericOperator::make_compare(IR2_Condition::Kind::FALSE),
          pool.form<GenericElement>(
              GenericOperator::make_function(pool.form<ConstantTokenElement>("time-elapsed?")),
              make_casts_if_needed(args, types, TypeSpec("time-frame"), pool, env)));
    } else if (m_kind == IR2_Condition::Kind::GEQ_SIGNED) {
      return pool.alloc_element<GenericElement>(
          GenericOperator::make_function(pool.form<ConstantTokenElement>("time-elapsed?")),
          make_casts_if_needed(args, types, TypeSpec("time-frame"), pool, env));
    }
  }

  return nullptr;
}

FormElement* ConditionElement::make_generic(const Env& env,
                                            FormPool& pool,
                                            const std::vector<Form*>& source_forms,
                                            const std::vector<TypeSpec>& types) {
  switch (m_kind) {
    case IR2_Condition::Kind::ZERO:
      return make_zero_check_generic(env, pool, source_forms, types);
    case IR2_Condition::Kind::NONZERO:
      return make_nonzero_check_generic(env, pool, source_forms, types);
    case IR2_Condition::Kind::TRUTHY:
    case IR2_Condition::Kind::FALSE:
    case IR2_Condition::Kind::IS_PAIR:
    case IR2_Condition::Kind::IS_NOT_PAIR:
    case IR2_Condition::Kind::ALWAYS:
      // kind of a hack, we fall back to the old condition operator which is special cased
      // to print the truthy condition in a nice way. and we use it for other things that don't
      // require fancy renaming.
      return pool.alloc_element<GenericElement>(GenericOperator::make_compare(m_kind),
                                                source_forms);
    case IR2_Condition::Kind::EQUAL:
      return make_equal_check_generic(env, pool, source_forms, types);

    case IR2_Condition::Kind::NOT_EQUAL:
      return make_not_equal_check_generic(env, pool, source_forms, types);

    case IR2_Condition::Kind::LESS_THAN_SIGNED: {
      auto time_elapsed = make_time_elapsed(env, pool, source_forms, types);
      if (time_elapsed) {
        return time_elapsed;
      }
      return pool.alloc_element<GenericElement>(
          GenericOperator::make_fixed(FixedOperatorKind::LT),
          make_casts_if_needed(source_forms, types, TypeSpec("int"), pool, env));
    }
    case IR2_Condition::Kind::LESS_THAN_UNSIGNED: {
      auto time_elapsed = make_time_elapsed(env, pool, source_forms, types);
      if (time_elapsed) {
        return time_elapsed;
      }
      return pool.alloc_element<GenericElement>(
          GenericOperator::make_fixed(FixedOperatorKind::LT),
          make_casts_if_needed(source_forms, types, TypeSpec("uint"), pool, env));
    }

    case IR2_Condition::Kind::GEQ_SIGNED: {
      auto time_elapsed = make_time_elapsed(env, pool, source_forms, types);
      if (time_elapsed) {
        return time_elapsed;
      }
      return pool.alloc_element<GenericElement>(
          GenericOperator::make_fixed(FixedOperatorKind::GEQ),
          make_casts_if_needed(source_forms, types, TypeSpec("int"), pool, env));
    }
    case IR2_Condition::Kind::GEQ_UNSIGNED: {
      auto time_elapsed = make_time_elapsed(env, pool, source_forms, types);
      if (time_elapsed) {
        return time_elapsed;
      }
      return pool.alloc_element<GenericElement>(
          GenericOperator::make_fixed(FixedOperatorKind::GEQ),
          make_casts_if_needed(source_forms, types, TypeSpec("uint"), pool, env));
    }

    case IR2_Condition::Kind::LESS_THAN_ZERO_SIGNED: {
      return make_less_than_zero_signed_check_generic(env, pool, source_forms, types);
    }

    case IR2_Condition::Kind::LESS_THAN_ZERO_UNSIGNED: {
      auto casted = make_casts_if_needed(source_forms, types, TypeSpec("uint"), pool, env);
      auto zero = pool.form<SimpleAtomElement>(SimpleAtom::make_int_constant(0));
      casted.push_back(zero);
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::LT),
                                                casted);
    }

    case IR2_Condition::Kind::LEQ_ZERO_SIGNED: {
      auto casted = make_casts_if_needed(source_forms, types, TypeSpec("int"), pool, env);
      auto zero = pool.form<SimpleAtomElement>(SimpleAtom::make_int_constant(0));
      casted.push_back(zero);
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::LEQ),
                                                casted);
    }

    case IR2_Condition::Kind::LEQ_ZERO_UNSIGNED: {
      auto casted = make_casts_if_needed(source_forms, types, TypeSpec("uint"), pool, env);
      auto zero = pool.form<SimpleAtomElement>(SimpleAtom::make_int_constant(0));
      casted.push_back(zero);
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::LEQ),
                                                casted);
    }

    case IR2_Condition::Kind::GEQ_ZERO_SIGNED: {
      return make_geq_zero_signed_check_generic(env, pool, source_forms, types);
    }

    case IR2_Condition::Kind::GEQ_ZERO_UNSIGNED: {
      return make_geq_zero_unsigned_check_generic(env, pool, source_forms, types);
    }

    case IR2_Condition::Kind::GREATER_THAN_ZERO_UNSIGNED: {
      auto casted = make_casts_if_needed(source_forms, types, TypeSpec("uint"), pool, env);
      auto zero = pool.form<SimpleAtomElement>(SimpleAtom::make_int_constant(0));
      casted.push_back(zero);
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::GT),
                                                casted);
    }

    case IR2_Condition::Kind::GREATER_THAN_ZERO_SIGNED: {
      auto casted = make_casts_if_needed(source_forms, types, TypeSpec("int"), pool, env);
      auto zero = pool.form<SimpleAtomElement>(SimpleAtom::make_int_constant(0));
      casted.push_back(zero);
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::GT),
                                                casted);
    }

    case IR2_Condition::Kind::FLOAT_NOT_EQUAL: {
      auto casted = make_casts_if_needed(source_forms, types, TypeSpec("float"), pool, env);
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::NEQ),
                                                casted);
    }

    case IR2_Condition::Kind::FLOAT_EQUAL: {
      auto casted = make_casts_if_needed(source_forms, types, TypeSpec("float"), pool, env);
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::EQ),
                                                casted);
    }

    case IR2_Condition::Kind::FLOAT_LEQ: {
      auto casted = make_casts_if_needed(source_forms, types, TypeSpec("float"), pool, env);
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::LEQ),
                                                casted);
    }

    case IR2_Condition::Kind::FLOAT_LESS_THAN: {
      auto casted = make_casts_if_needed(source_forms, types, TypeSpec("float"), pool, env);
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::LT),
                                                casted);
    }

    case IR2_Condition::Kind::FLOAT_GEQ: {
      auto casted = make_casts_if_needed(source_forms, types, TypeSpec("float"), pool, env);
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::GEQ),
                                                casted);
    }

    case IR2_Condition::Kind::FLOAT_GREATER_THAN: {
      // never emitted by normal branch conditions
      auto casted = make_casts_if_needed(source_forms, types, TypeSpec("float"), pool, env);
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::GT),
                                                casted);
    }

    default:
      throw std::runtime_error("ConditionElement::make_generic NYI for kind " +
                               get_condition_kind_name(m_kind));
  }
}

void ConditionElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  mark_popped();
  std::vector<Form*> source_forms, popped_forms;
  std::vector<TypeSpec> source_types;
  std::vector<RegisterAccess> vars;

  for (int i = 0; i < get_condition_num_args(m_kind); i++) {
    if (m_src[i]->is_var()) {
      auto& var = m_src[i]->var();
      vars.push_back(var);
      source_types.push_back(env.get_types_before_op(var.idx()).get(var.reg()).typespec());
    } else if (m_src[i]->is_int()) {
      if (m_src[i]->get_int() == 0 && condition_uses_float(m_kind)) {
        // if we're doing a floating point comparison, and one of our arguments is a constant
        // which is an "integer zero", treat it as a floating point zero.
        source_types.push_back(TypeSpec("float"));
      } else {
        source_types.push_back(TypeSpec("int"));
      }
    } else if (m_src[i]->is_sym_val() && m_src[i]->get_str() == "#f") {
      source_types.push_back(TypeSpec("symbol"));
    } else {
      throw std::runtime_error(fmt::format(
          "Unsupported atom in ConditionElement::push_to_stack: {}", m_src[i]->to_string(env)));
    }
  }
  if (m_flipped) {
    std::reverse(vars.begin(), vars.end());
  }

  popped_forms = pop_to_forms(vars, env, pool, stack, true, m_consumed);
  if (m_flipped) {
    std::reverse(popped_forms.begin(), popped_forms.end());
  }

  int popped_counter = 0;
  for (int i = 0; i < get_condition_num_args(m_kind); i++) {
    if (m_src[i]->is_var()) {
      source_forms.push_back(popped_forms.at(popped_counter++));
    } else {
      source_forms.push_back(pool.form<SimpleAtomElement>(*m_src[i]));
    }
  }
  ASSERT(popped_counter == int(popped_forms.size()));
  ASSERT(source_forms.size() == source_types.size());

  stack.push_form_element(make_generic(env, pool, source_forms, source_types), true);
}

void ConditionElement::update_from_stack(const Env& env,
                                         FormPool& pool,
                                         FormStack& stack,
                                         std::vector<FormElement*>* result,
                                         bool allow_side_effects) {
  mark_popped();
  std::vector<Form*> source_forms, popped_forms;
  std::vector<TypeSpec> source_types;
  std::vector<RegisterAccess> vars;

  for (int i = 0; i < get_condition_num_args(m_kind); i++) {
    if (m_src[i]->is_var()) {
      auto& var = m_src[i]->var();
      vars.push_back(var);
      source_types.push_back(env.get_types_before_op(var.idx()).get(var.reg()).typespec());
    } else if (m_src[i]->is_int()) {
      if (m_src[i]->get_int() == 0 && condition_uses_float(m_kind)) {
        // if we're doing a floating point comparison, and one of our arguments is a constant
        // which is an "integer zero", treat it as a floating point zero.
        source_types.push_back(TypeSpec("float"));
      } else {
        source_types.push_back(TypeSpec("int"));
      }
    } else if (m_src[i]->is_sym_val() && m_src[i]->get_str() == "#f") {
      source_types.push_back(TypeSpec("symbol"));
    } else {
      throw std::runtime_error("Unsupported atom in ConditionElement::update_from_stack: " +
                               m_src[i]->to_string(env));
    }
  }
  if (m_flipped) {
    std::reverse(vars.begin(), vars.end());
  }

  popped_forms = pop_to_forms(vars, env, pool, stack, allow_side_effects, m_consumed);
  if (m_flipped) {
    std::reverse(popped_forms.begin(), popped_forms.end());
  }

  int popped_counter = 0;
  for (int i = 0; i < get_condition_num_args(m_kind); i++) {
    if (m_src[i]->is_var()) {
      source_forms.push_back(popped_forms.at(popped_counter++));
    } else {
      source_forms.push_back(pool.form<SimpleAtomElement>(*m_src[i]));
    }
  }
  ASSERT(popped_counter == int(popped_forms.size()));
  ASSERT(source_forms.size() == source_types.size());

  result->push_back(make_generic(env, pool, source_forms, source_types));
}

void ReturnElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  mark_popped();
  FormStack temp_stack(false);
  for (auto& elt : return_code->elts()) {
    elt->push_to_stack(env, pool, temp_stack);
  }

  std::vector<FormElement*> new_entries;
  std::optional<RegisterAccess> var;
  new_entries = rewrite_to_get_var(temp_stack, pool, env.end_var(), env, &var);

  ASSERT(!new_entries.empty());

  return_code->clear();

  for (int i = 0; i < ((int)new_entries.size()) - 1; i++) {
    stack.push_form_element(new_entries.at(i), true);
  }

  return_code->push_back(new_entries.back());
  if (var) {
    const auto& func_type = env.func->type.last_arg();
    return_type = env.get_variable_type(*var, false);
    // functions with no return can return stuff.
    if (func_type != return_type && func_type != TypeSpec("none")) {
      auto as_cast = return_code->try_as_element<CastElement>();
      if (as_cast) {
        return_code->clear();
        as_cast->set_type(func_type);
        return_code->push_back(as_cast);
      } else {
        return_code = cast_form(return_code, func_type, pool, env);
        return_code->parent_element = this;
      }
    }
  }
  stack.push_form_element(this, true);
}

namespace {

void push_asm_srl_to_stack(const AsmOp* op,
                           FormElement* form_elt,
                           const Env& env,
                           FormPool& pool,
                           FormStack& stack) {
  // we will try to convert this into a bitfield operation. If this fails, fall back to assembly.
  auto var = op->src(0);
  ASSERT(var.has_value());  // srl should always have this.

  auto dst = op->dst();
  ASSERT(dst.has_value());

  auto integer_atom = op->instruction().get_src(1);
  ASSERT(integer_atom.is_imm());
  auto integer = integer_atom.get_imm();

  auto arg0_type = env.get_variable_type(*var, true);
  auto type_info = env.dts->ts.lookup_type(arg0_type);
  auto bitfield_info = dynamic_cast<BitFieldType*>(type_info);
  if (bitfield_info) {
    auto base = pop_to_forms({*var}, env, pool, stack, true).at(0);
    auto read_elt = pool.alloc_element<BitfieldAccessElement>(base, arg0_type);
    BitfieldManip step(BitfieldManip::Kind::RIGHT_SHIFT_LOGICAL_32BIT, integer);
    auto other = read_elt->push_step(step, env.dts->ts, pool, env);
    ASSERT(other);  // should be a high field.
    stack.push_value_to_reg(*dst, pool.alloc_single_form(nullptr, other), true,
                            env.get_variable_type(*dst, true));
  } else {
    //
    auto src_var = pop_to_forms({*var}, env, pool, stack, true).at(0);
    auto as_ba = src_var->try_as_element<BitfieldAccessElement>();
    if (as_ba) {
      BitfieldManip step(BitfieldManip::Kind::RIGHT_SHIFT_LOGICAL_32BIT, integer);
      auto other = as_ba->push_step(step, env.dts->ts, pool, env);
      ASSERT(other);  // should immediately get a field.
      stack.push_value_to_reg(*dst, pool.alloc_single_form(nullptr, other), true,
                              env.get_variable_type(*dst, true));
    } else {
      stack.push_form_element(form_elt, true);
      //  throw std::runtime_error(
      //  fmt::format("Got invalid bitfield manip for srl at op {}: {} type was {}", op->op_id(),
      //             src_var->to_string(env), arg0_type.print()));
    }
  }
}

void push_asm_sllv_to_stack(const AsmOp* op,
                            FormElement* form_elt,
                            const Env& env,
                            FormPool& pool,
                            FormStack& stack) {
  auto var = op->src(0);
  ASSERT(var.has_value());

  auto dst = op->dst();
  ASSERT(dst.has_value());

  auto arg0_type = env.get_variable_type(*var, true);
  auto type_info = env.dts->ts.lookup_type(arg0_type);
  auto bitfield_info = dynamic_cast<BitFieldType*>(type_info);
  if (op->instruction().src[1].is_reg(Register(Reg::GPR, Reg::R0))) {
    if (bitfield_info) {
      auto base = pop_to_forms({*var}, env, pool, stack, true).at(0);
      auto read_elt = pool.alloc_element<BitfieldAccessElement>(base, arg0_type);
      BitfieldManip step(BitfieldManip::Kind::SLLV_SEXT, 0);
      auto other = read_elt->push_step(step, env.dts->ts, pool, env);
      ASSERT(other);  // should immediately get a field.
      stack.push_value_to_reg(*dst, pool.alloc_single_form(nullptr, other), true,
                              env.get_variable_type(*dst, true));
    } else {
      auto src_var = pop_to_forms({*var}, env, pool, stack, true).at(0);
      auto as_ba = src_var->try_as_element<BitfieldAccessElement>();
      if (as_ba) {
        // part of existing chain.
        BitfieldManip step(BitfieldManip::Kind::SLLV_SEXT, 0);
        auto other = as_ba->push_step(step, env.dts->ts, pool, env);
        ASSERT(other);  // should immediately get a field.
        stack.push_value_to_reg(*dst, pool.alloc_single_form(nullptr, other), true,
                                env.get_variable_type(*dst, true));
      } else {
        // push it to a weird looking form for initial bitfield setting.
        // these are lazily converted at the destination.
        stack.push_value_to_reg(
            *dst,
            pool.form<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::ASM_SLLV_R0),
                                      src_var),
            true, env.get_variable_type(*dst, true));
      }
    }
  } else {
    stack.push_form_element(form_elt, true);
  }
}

void push_asm_pcpyud_to_stack(const AsmOp* op,
                              FormElement* form_elt,
                              const Env& env,
                              FormPool& pool,
                              FormStack& stack) {
  // pcpyud v1, gp, r0 for example.

  auto var = op->src(0);
  ASSERT(var.has_value());

  auto dst = op->dst();
  ASSERT(dst.has_value());

  auto possible_r0 = op->src(1);
  ASSERT(possible_r0.has_value());

  auto arg0_type = env.get_variable_type(*var, true);
  auto type_info = env.dts->ts.lookup_type(arg0_type);
  auto bitfield_info = dynamic_cast<BitFieldType*>(type_info);
  if (bitfield_info && possible_r0->reg() == Register(Reg::GPR, Reg::R0)) {
    auto base = pop_to_forms({*var}, env, pool, stack, true).at(0);
    auto read_elt = pool.alloc_element<BitfieldAccessElement>(base, arg0_type);
    read_elt->push_pcpyud(env.dts->ts, pool, env);
    stack.push_value_to_reg(*dst, pool.alloc_single_form(nullptr, read_elt), true,
                            env.get_variable_type(*dst, true));
  } else {
    stack.push_form_element(form_elt, true);
  }
}

void push_asm_pextuw_to_stack(const AsmOp* op,
                              FormElement* form_elt,
                              const Env& env,
                              FormPool& pool,
                              FormStack& stack) {
  // (.pextuw t0-0 r0-0 obj)

  auto var = op->src(1);
  ASSERT(var.has_value());

  auto dst = op->dst();
  ASSERT(dst.has_value());

  auto possible_r0 = op->src(0);
  ASSERT(possible_r0.has_value());

  auto arg0_type = env.get_variable_type(*var, true);
  auto type_info = env.dts->ts.lookup_type(arg0_type);
  auto bitfield_info = dynamic_cast<BitFieldType*>(type_info);
  if (bitfield_info && possible_r0->reg() == Register(Reg::GPR, Reg::R0)) {
    auto base = pop_to_forms({*var}, env, pool, stack, true).at(0);
    auto read_elt = pool.alloc_element<BitfieldAccessElement>(base, arg0_type);
    BitfieldManip step(BitfieldManip::Kind::PEXTUW, 0);
    auto other = read_elt->push_step(step, env.dts->ts, pool, env);
    ASSERT(other);  // should immediately get a field.
    stack.push_value_to_reg(*dst, pool.alloc_single_form(nullptr, other), true,
                            env.get_variable_type(*dst, true));
  } else {
    stack.push_form_element(form_elt, true);
  }
}

/*
void push_asm_madds_to_stack(const AsmOp* op,
                             FormElement* form_elt,
                             const Env& env,
                             FormPool& pool,
                             FormStack& stack) {
  auto src0 = op->src(0);
  ASSERT(src0.has_value());

  auto src1 = op->src(1);
  ASSERT(src1.has_value());

  auto dst = op->dst();
  ASSERT(dst.has_value());

  auto vars = pop_to_forms({*src0, *src1}, env, pool, stack, true);

  stack.push_value_to_reg(
      *dst,
      pool.alloc_single_element_form<GenericElement>(
          nullptr, GenericOperator::make_fixed(FixedOperatorKind::ASM_MADDS), vars),
      true, env.get_variable_type(*dst, true));
}
*/

void push_asm_to_stack(const AsmOp* op,
                       FormElement* form_elt,
                       const Env& env,
                       FormPool& pool,
                       FormStack& stack) {
  switch (op->instruction().kind) {
    case InstructionKind::SRL:
      push_asm_srl_to_stack(op, form_elt, env, pool, stack);
      break;
    case InstructionKind::SLLV:
      push_asm_sllv_to_stack(op, form_elt, env, pool, stack);
      break;
    case InstructionKind::PCPYUD:
      push_asm_pcpyud_to_stack(op, form_elt, env, pool, stack);
      break;
    case InstructionKind::PEXTUW:
      push_asm_pextuw_to_stack(op, form_elt, env, pool, stack);
      break;
      /*
    case InstructionKind::MADDS:
      push_asm_madds_to_stack(op, form_elt, env, pool, stack);
      break;
       */
    default:
      stack.push_form_element(form_elt, true);
      break;
  }
}

}  // namespace

void AtomicOpElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  mark_popped();
  auto as_end = dynamic_cast<const FunctionEndOp*>(m_op);
  if (as_end) {
    // we don't want to push this to the stack (for now at least)
    return;
  }

  auto as_special = dynamic_cast<const SpecialOp*>(m_op);
  if (as_special) {
    if (as_special->kind() == SpecialOp::Kind::NOP ||
        as_special->kind() == SpecialOp::Kind::BREAK ||
        as_special->kind() == SpecialOp::Kind::CRASH ||
        as_special->kind() == SpecialOp::Kind::SUSPEND) {
      stack.push_form_element(this, true);
      return;
    }
  }

  auto as_asm = dynamic_cast<const AsmOp*>(m_op);
  if (as_asm) {
    push_asm_to_stack(as_asm, this, env, pool, stack);
    return;
  }

  auto as_branch = dynamic_cast<AsmBranchOp*>(m_op);
  if (as_branch && !as_branch->is_likely()) {
    // this is a bit of a hack, but we go AsmBranchOp -> AsmBranchElement -> TranslatedAsmBranch
    auto delay = as_branch->branch_delay();
    ASSERT(delay);
    // this might not be enough - we may need to back up to the cfg builder and do something there.
    auto del = pool.form<AtomicOpElement>(delay);
    auto be = pool.alloc_element<AsmBranchElement>(as_branch, del, false);
    be->push_to_stack(env, pool, stack);
    return;
  }

  throw std::runtime_error("Cannot push atomic op to stack: " + m_op->to_string(env));
}

void AsmOpElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  mark_popped();
  push_asm_to_stack(m_op, this, env, pool, stack);
}

void GenericElement::update_from_stack(const Env& env,
                                       FormPool& pool,
                                       FormStack& stack,
                                       std::vector<FormElement*>* result,
                                       bool) {
  mark_popped();
  if (m_elts.size() == 1) {
    // a bit of a hack, but AtomicOpForm uses this for loading car/cdr
    // this is safe to do.
    m_elts.front()->update_children_from_stack(env, pool, stack, true);
  }
  result->push_back(this);
}

void AsmBranchElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  // create a condition element
  RegSet consumed;
  if (env.has_reg_use()) {
    consumed = env.reg_use().op.at(m_branch_op->op_id()).consumes;
  }
  std::optional<SimpleAtom> vars[2];
  for (int i = 0; i < get_condition_num_args(m_branch_op->condition().kind()); i++) {
    vars[i] = m_branch_op->condition().src(i);
  }
  auto ce = pool.alloc_element<ConditionElement>(m_branch_op->condition().kind(), vars[0], vars[1],
                                                 consumed, false);

  // and update it from the stack.
  std::vector<FormElement*> ce_updated;
  ce->update_from_stack(env, pool, stack, &ce_updated, true);

  auto branch_condition = pool.alloc_sequence_form(nullptr, ce_updated);

  auto op = pool.alloc_element<TranslatedAsmBranch>(
      branch_condition, m_branch_delay, m_branch_op->label_id(), m_branch_op->is_likely());
  // lg::print("rewrote as {}\n", op->to_string(env));
  stack.push_form_element(op, true);
}

void BranchElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  // These will appear if we have an asm-branch that looked like a normal branch.
  // create a condition element
  RegSet consumed;
  if (env.has_reg_use()) {
    consumed = env.reg_use().op.at(m_op->op_id()).consumes;
  }
  std::optional<SimpleAtom> vars[2];
  for (int i = 0; i < get_condition_num_args(m_op->condition().kind()); i++) {
    vars[i] = m_op->condition().src(i);
  }
  auto ce = pool.alloc_element<ConditionElement>(m_op->condition().kind(), vars[0], vars[1],
                                                 consumed, false);

  // and update it from the stack.
  std::vector<FormElement*> ce_updated;
  ce->update_from_stack(env, pool, stack, &ce_updated, true);

  auto branch_condition = pool.alloc_sequence_form(nullptr, ce_updated);

  Form* branch_delay = nullptr;
  switch (m_op->branch_delay().kind()) {
    case IR2_BranchDelay::Kind::NOP: {
      branch_delay = nullptr;
    } break;
    case IR2_BranchDelay::Kind::SET_REG_REG: {
      auto src = m_op->branch_delay().var(1);
      auto dst = m_op->branch_delay().var(0);

      auto src_form = pool.form<SimpleAtomElement>(SimpleAtom::make_var(src));

      branch_delay =
          pool.form<SetVarElement>(dst, src_form, true, env.get_variable_type(src, true));
    } break;
    case IR2_BranchDelay::Kind::SET_REG_FALSE: {
      auto dst = m_op->branch_delay().var(0);
      auto src_form = pool.form<SimpleAtomElement>(SimpleAtom::make_sym_val("#f"));

      branch_delay = pool.form<SetVarElement>(dst, src_form, true, TypeSpec("symbol"));
    } break;
    case IR2_BranchDelay::Kind::SET_REG_TRUE: {
      auto dst = m_op->branch_delay().var(0);
      auto src_form = pool.form<SimpleAtomElement>(SimpleAtom::make_sym_val("#t"));

      branch_delay = pool.form<SetVarElement>(dst, src_form, true, TypeSpec("symbol"));
    } break;
    default:
      throw std::runtime_error("Unhandled branch delay in BranchElement::push_to_stack: " +
                               m_op->to_string(env));
  }

  ASSERT(!m_op->likely());
  auto op = pool.alloc_element<TranslatedAsmBranch>(branch_condition, branch_delay,
                                                    m_op->label_id(), m_op->likely());
  // lg::print("rewrote (non-asm) as {}\n", op->to_string(env));
  stack.push_form_element(op, true);
}

void GenericElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  (void)env;
  (void)pool;
  mark_popped();
  stack.push_form_element(this, true);
}

////////////////////////
// DynamicMethodAccess
////////////////////////

void DynamicMethodAccess::update_from_stack(const Env& env,
                                            FormPool& pool,
                                            FormStack& stack,
                                            std::vector<FormElement*>* result,
                                            bool allow_side_effects) {
  mark_popped();
  // auto new_val = stack.pop_reg(m_source, {}, env, allow_side_effects);
  auto new_val = pop_to_forms({m_source}, env, pool, stack, allow_side_effects).at(0);
  auto reg0_matcher =
      Matcher::match_or({Matcher::any_reg(0), Matcher::cast("uint", Matcher::any_reg(0))});
  auto reg1_matcher =
      Matcher::match_or({Matcher::any_reg(1), Matcher::cast("int", Matcher::any_reg(1))});

  // (+ (* method-id 4) (the-as int child-type))
  auto mult_matcher =
      Matcher::op_fixed(FixedOperatorKind::MULTIPLICATION, {reg0_matcher, Matcher::integer(4)});
  auto matcher = Matcher::op_fixed(FixedOperatorKind::ADDITION, {mult_matcher, reg1_matcher});
  auto match_result = match(matcher, new_val);
  if (!match_result.matched) {
    throw std::runtime_error("Could not match DynamicMethodAccess values: " +
                             new_val->to_string(env));
  }

  auto idx = match_result.maps.regs.at(0);
  auto base = match_result.maps.regs.at(1);
  ASSERT(idx.has_value() && base.has_value());

  auto deref = pool.alloc_element<DerefElement>(
      var_to_form(base.value(), pool), false,
      std::vector<DerefToken>{DerefToken::make_field_name("method-table"),
                              DerefToken::make_int_expr(var_to_form(idx.value(), pool))});
  result->push_back(deref);
}

////////////////////////
// ArrayFieldAccess
////////////////////////

void ArrayFieldAccess::update_with_val(Form* new_val,
                                       const Env& env,
                                       FormPool& pool,
                                       std::vector<FormElement*>* result,
                                       bool) {
  int power_of_two = 0;

  if (m_constant_offset == 0) {
    if (m_expected_stride == 1) {
      auto base_matcher =
          Matcher::match_or({Matcher::cast("int", Matcher::any(0)),
                             Matcher::cast("uint", Matcher::any(0)), Matcher::any(0)});
      auto offset_matcher =
          Matcher::match_or({Matcher::cast("int", Matcher::any(1)),
                             Matcher::cast("uint", Matcher::any(1)), Matcher::any(1)});

      // (&+ data-ptr <idx>)
      auto matcher = Matcher::match_or(
          {Matcher::op_fixed(FixedOperatorKind::ADDITION, {base_matcher, offset_matcher}),
           Matcher::op_fixed(FixedOperatorKind::ADDITION_PTR, {base_matcher, offset_matcher})});

      auto match_result = match(matcher, new_val);
      if (!match_result.matched) {
        throw std::runtime_error(
            fmt::format("Failed to match array stride 1 load {}", new_val->to_string(env)));
      }
      auto idx = match_result.maps.forms.at(1);
      auto base = match_result.maps.forms.at(0);
      ASSERT(idx && base);

      if (m_flipped) {
        std::swap(idx, base);
      }

      std::vector<DerefToken> tokens = m_deref_tokens;
      for (auto& x : tokens) {
        if (x.kind() == DerefToken::Kind::EXPRESSION_PLACEHOLDER) {
          x = DerefToken::make_int_expr(idx);
        }
      }
      //      tokens.push_back(DerefToken::make_int_expr(idx));

      auto deref = pool.alloc_element<DerefElement>(base, false, tokens);
      result->push_back(deref);
    } else if (is_power_of_two(m_expected_stride, &power_of_two)) {
      // reg0 is base
      // reg1 is idx

      auto reg0_matcher =
          Matcher::match_or({Matcher::cast("int", Matcher::any(0)),
                             Matcher::cast("uint", Matcher::any(0)), Matcher::any(0)});
      auto reg1_matcher =
          Matcher::match_or({Matcher::cast("uint", Matcher::any(1)), Matcher::any(1)});
      auto mult_matcher = Matcher::op_fixed(FixedOperatorKind::MULTIPLICATION,
                                            {reg1_matcher, Matcher::integer(m_expected_stride)});
      mult_matcher = Matcher::match_or({Matcher::cast("uint", mult_matcher), mult_matcher});
      auto matcher = Matcher::match_or(
          {Matcher::op_fixed(FixedOperatorKind::ADDITION, {reg0_matcher, mult_matcher}),
           Matcher::op_fixed(FixedOperatorKind::ADDITION_PTR, {reg0_matcher, mult_matcher})});
      auto match_result = match(matcher, new_val);
      if (!match_result.matched) {
        matcher = Matcher::match_or(
            {Matcher::op_fixed(FixedOperatorKind::ADDITION, {mult_matcher, reg0_matcher}),
             Matcher::op_fixed(FixedOperatorKind::ADDITION_PTR, {mult_matcher, reg0_matcher})});
        match_result = match(matcher, new_val);
        if (!match_result.matched) {
          result->push_back(this);
          return;
          lg::error("power {}", power_of_two);
          throw std::runtime_error(
              "Couldn't match ArrayFieldAccess (stride power of 2, 0 offset) values: " +
              new_val->to_string(env));
        }
      }

      auto idx = match_result.maps.forms.at(1);
      auto base = match_result.maps.forms.at(0);
      ASSERT(idx && base);

      std::vector<DerefToken> tokens = m_deref_tokens;
      for (auto& x : tokens) {
        if (x.kind() == DerefToken::Kind::EXPRESSION_PLACEHOLDER) {
          x = DerefToken::make_int_expr(idx);
        }
      }
      //      tokens.push_back(DerefToken::make_int_expr(idx));

      auto deref = pool.alloc_element<DerefElement>(base, false, tokens);
      result->push_back(deref);
    } else {
      auto mult_matcher = Matcher::op(
          GenericOpMatcher::fixed(FixedOperatorKind::MULTIPLICATION),
          {Matcher::match_or({Matcher::cast("uint", Matcher::integer(m_expected_stride)),
                              Matcher::integer(m_expected_stride)}),
           Matcher::any(0)});
      mult_matcher = Matcher::match_or(
          {Matcher::cast("uint", mult_matcher), Matcher::cast("int", mult_matcher), mult_matcher});

      auto op_match =
          GenericOpMatcher::or_match({GenericOpMatcher::fixed(FixedOperatorKind::ADDITION),
                                      GenericOpMatcher::fixed(FixedOperatorKind::ADDITION_PTR)});
      auto add_matcher = Matcher::op(op_match, {Matcher::any(1), mult_matcher});
      add_matcher =
          Matcher::match_or({add_matcher, Matcher::op(op_match, {mult_matcher, Matcher::any(1)})});

      auto mr = match(add_matcher, new_val);
      if (!mr.matched) {
        throw std::runtime_error("Failed to match non-power of two case: " +
                                 new_val->to_string(env));
      }

      auto base = strip_int_or_uint_cast(mr.maps.forms.at(1));
      auto idx = mr.maps.forms.at(0);

      ASSERT(idx && base);

      std::vector<DerefToken> tokens = m_deref_tokens;
      for (auto& x : tokens) {
        if (x.kind() == DerefToken::Kind::EXPRESSION_PLACEHOLDER) {
          x = DerefToken::make_int_expr(idx);
        }
      }

      auto deref = pool.alloc_element<DerefElement>(base, false, tokens);
      result->push_back(deref);
    }
  } else {
    if (m_expected_stride == 1) {
      // reg0 is idx
      auto reg0_matcher =
          Matcher::match_or({Matcher::cast("int", Matcher::any(0)),
                             Matcher::cast("uint", Matcher::any(0)), Matcher::any(0)});
      // reg1 is base
      auto reg1_matcher =
          Matcher::match_or({Matcher::cast("int", Matcher::any(1)),
                             Matcher::cast("uint", Matcher::any(1)), Matcher::any(1)});
      auto matcher = Matcher::op_fixed(FixedOperatorKind::ADDITION, {reg0_matcher, reg1_matcher});
      auto match_result = match(matcher, new_val);
      if (!match_result.matched) {
        throw std::runtime_error("Could not match ArrayFieldAccess (stride 1) values: " +
                                 new_val->to_string(env));
      }
      auto idx = match_result.maps.forms.at(0);
      auto base = match_result.maps.forms.at(1);
      ASSERT(idx && base);

      std::vector<DerefToken> tokens = m_deref_tokens;
      for (auto& x : tokens) {
        if (x.kind() == DerefToken::Kind::EXPRESSION_PLACEHOLDER) {
          x = DerefToken::make_int_expr(idx);
        }
      }
      // tokens.push_back(DerefToken::make_int_expr(var_to_form(idx.value(), pool)));

      auto deref = pool.alloc_element<DerefElement>(base, false, tokens);
      result->push_back(deref);
    } else if (is_power_of_two(m_expected_stride, &power_of_two)) {
      // (+ (sll (the-as uint a1-0) 2) (the-as int a0-0))
      // (+ gp-0 (the-as uint (shl (the-as uint (shl (the-as uint s4-0) 2)) 2)))
      auto reg0_matcher =
          Matcher::match_or({Matcher::cast("uint", Matcher::any(0)), Matcher::any(0)});
      auto reg1_matcher =
          Matcher::match_or({Matcher::cast("uint", Matcher::any(1)),
                             Matcher::cast("int", Matcher::any(1)), Matcher::any(1)});
      auto mult_matcher = Matcher::op_fixed(FixedOperatorKind::MULTIPLICATION,
                                            {reg0_matcher, Matcher::integer(m_expected_stride)});
      mult_matcher = Matcher::match_or({Matcher::cast("uint", mult_matcher), mult_matcher});
      auto matcher = Matcher::op_fixed(FixedOperatorKind::ADDITION, {mult_matcher, reg1_matcher});
      matcher = Matcher::match_or({matcher, Matcher::op_fixed(FixedOperatorKind::ADDITION_PTR,
                                                              {reg1_matcher, mult_matcher})});
      auto match_result = match(matcher, new_val);
      Form* idx = nullptr;
      Form* base = nullptr;
      // TODO - figure out why it sometimes happens the other way.
      if (!match_result.matched) {
        matcher = Matcher::op_fixed(FixedOperatorKind::ADDITION, {reg1_matcher, mult_matcher});
        match_result = match(matcher, new_val);
        if (!match_result.matched) {
          throw std::runtime_error("Could not match ArrayFieldAccess (stride power of 2) values: " +
                                   new_val->to_string(env));
        }
      }
      idx = match_result.maps.forms.at(0);
      base = match_result.maps.forms.at(1);

      ASSERT(idx && base);

      std::vector<DerefToken> tokens = m_deref_tokens;
      for (auto& x : tokens) {
        if (x.kind() == DerefToken::Kind::EXPRESSION_PLACEHOLDER) {
          x = DerefToken::make_int_expr(idx);
        }
      }
      // tokens.push_back(DerefToken::make_int_expr(var_to_form(idx.value(), pool)));

      auto deref = pool.alloc_element<DerefElement>(base, false, tokens);
      result->push_back(deref);
    } else {
      // (+ v0-0 (the-as uint (* 12 (+ a3-0 -1))))
      // (+ (the-as uint *texture-page-dir*) (* (the-as uint 12) (-> arg0 page))
      auto mult_matcher = Matcher::op(
          GenericOpMatcher::fixed(FixedOperatorKind::MULTIPLICATION),
          {Matcher::match_or({Matcher::cast("uint", Matcher::integer(m_expected_stride)),
                              Matcher::integer(m_expected_stride)}),
           Matcher::any(0)});
      mult_matcher = Matcher::match_or(
          {Matcher::cast("uint", mult_matcher), Matcher::cast("int", mult_matcher), mult_matcher});

      auto op_match =
          GenericOpMatcher::or_match({GenericOpMatcher::fixed(FixedOperatorKind::ADDITION),
                                      GenericOpMatcher::fixed(FixedOperatorKind::ADDITION_PTR)});
      auto add_matcher = Matcher::op(op_match, {Matcher::any(1), mult_matcher});
      add_matcher =
          Matcher::match_or({add_matcher, Matcher::op(op_match, {mult_matcher, Matcher::any(1)})});

      auto mr = match(add_matcher, new_val);
      if (!mr.matched) {
        throw std::runtime_error("Failed to match non-power of two case: " +
                                 new_val->to_string(env));
      }

      auto base = strip_int_or_uint_cast(mr.maps.forms.at(1));
      auto idx = mr.maps.forms.at(0);

      ASSERT(idx && base);

      std::vector<DerefToken> tokens = m_deref_tokens;
      for (auto& x : tokens) {
        if (x.kind() == DerefToken::Kind::EXPRESSION_PLACEHOLDER) {
          x = DerefToken::make_int_expr(idx);
        }
      }

      auto deref = pool.alloc_element<DerefElement>(base, false, tokens);
      result->push_back(deref);
    }
  }
}

void ArrayFieldAccess::update_from_stack(const Env& env,
                                         FormPool& pool,
                                         FormStack& stack,
                                         std::vector<FormElement*>* result,
                                         bool allow_side_effects) {
  mark_popped();
  auto new_val = pop_to_forms({m_source}, env, pool, stack, allow_side_effects).at(0);
  update_with_val(new_val, env, pool, result, allow_side_effects);
}

////////////////////////
// CastElement
////////////////////////

void CastElement::update_from_stack(const Env& env,
                                    FormPool& pool,
                                    FormStack& stack,
                                    std::vector<FormElement*>* result,
                                    bool allow_side_effects) {
  mark_popped();
  m_source->update_children_from_stack(env, pool, stack, allow_side_effects);
  result->push_back(this);
}

////////////////////////
// TypeOfElement
////////////////////////

void TypeOfElement::update_from_stack(const Env& env,
                                      FormPool& pool,
                                      FormStack& stack,
                                      std::vector<FormElement*>* result,
                                      bool allow_side_effects) {
  mark_popped();
  value->update_children_from_stack(env, pool, stack, allow_side_effects);
  result->push_back(this);
}
////////////////////////
// EmptyElement
////////////////////////

void EmptyElement::push_to_stack(const Env&, FormPool&, FormStack& stack) {
  mark_popped();
  stack.push_form_element(this, true);
}

void StoreElement::push_to_stack(const Env&, FormPool&, FormStack& stack) {
  mark_popped();
  stack.push_form_element(this, true);
}

bool is_symbol_true(const Form* form) {
  auto as_simple = dynamic_cast<SimpleExpressionElement*>(form->try_as_single_element());
  if (as_simple && as_simple->expr().is_identity() && as_simple->expr().get_arg(0).is_sym_ptr() &&
      as_simple->expr().get_arg(0).get_str() == "#t") {
    return true;
  }
  return false;
}

void ConditionalMoveFalseElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  mark_popped();
  // pop the value and the original
  auto popped = pop_to_forms({old_value, source}, env, pool, stack, true);
  if (!is_symbol_true(popped.at(0))) {
    lg::warn("{}: Failed to ConditionalMoveFalseElement::push_to_stack", env.func->name());
    stack.push_value_to_reg(source, popped.at(1), true, TypeSpec("symbol"));
    stack.push_form_element(this, true);
    return;
  }

  Form* val = nullptr;

  if (on_zero) {
    auto as_logtest = try_make_nonzero_logtest(popped.at(1), pool);
    if (as_logtest) {
      auto logtest_form = pool.alloc_single_form(nullptr, as_logtest);
      auto fancy_form = convert_logtest_to_fancy_macro(pool, logtest_form);
      if (fancy_form) {
        val = pool.alloc_single_form(nullptr, fancy_form);
      } else {
        val = logtest_form;
      }
    }
  } else {
    auto as_logtest = try_make_nonzero_logtest(popped.at(1), pool);
    if (as_logtest) {
      auto logtest_form = pool.alloc_single_form(nullptr, as_logtest);
      auto not_form = pool.form<GenericElement>(
          GenericOperator::make_compare(IR2_Condition::Kind::FALSE), logtest_form);
      auto fancy_form = convert_logtest_to_fancy_macro(pool, not_form);
      if (fancy_form) {
        val = pool.alloc_single_form(nullptr, fancy_form);
      } else {
        val = not_form;
      }
    }
  }

  if (!val) {
    val = pool.form<GenericElement>(
        GenericOperator::make_compare(on_zero ? IR2_Condition::Kind::NONZERO
                                              : IR2_Condition::Kind::ZERO),
        std::vector<Form*>{popped.at(1)});
  }

  stack.push_value_to_reg(dest, val, true, TypeSpec("symbol"));
}

///////////////////////////
// StackSpillStoreElement
///////////////////////////
void StackSpillStoreElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  mark_popped();
  Form* src;
  if (m_value.is_var()) {
    src = pop_to_forms({m_value.var()}, env, pool, stack, true).at(0);
  } else {
    src = pool.form<SimpleAtomElement>(m_value);
  }

  auto dst = pool.form<ConstantTokenElement>(env.get_spill_slot_var_name(m_stack_offset));
  if (m_cast_type) {
    src = cast_form(src, *m_cast_type, pool, env);
  }
  stack.push_form_element(pool.alloc_element<SetFormFormElement>(dst, src), true);
}

namespace {

/*!
 * Is the given form an assembly form to load data from a vector to the given vf register?
 */
Form* is_load_store_vector_to_reg(const Register& reg,
                                  FormElement* form,
                                  bool is_load,
                                  int* idx_out) {
  auto as_vf_op = dynamic_cast<VectorFloatLoadStoreElement*>(form);
  if (!as_vf_op) {
    return nullptr;
  }

  if (as_vf_op->is_load() != is_load) {
    return nullptr;
  }

  if (as_vf_op->vf_reg() != reg) {
    return nullptr;
  }

  // check that we actually got a real vector type, not some other thing that happens to have a
  // quad.
  auto& addr_type = as_vf_op->addr_type();
  if (!addr_type || addr_type != TypeSpec("vector")) {
    return nullptr;
  }

  // make sure we load from the right spot, and extract the base.
  auto loc = as_vf_op->location();
  auto matcher = Matcher::deref(Matcher::any(0), true, {DerefTokenMatcher::string("quad")});
  auto mr = match(matcher, loc);
  if (!mr.matched) {
    return nullptr;
  }

  // output the index of the actual store op, for reguse purposes.
  if (idx_out) {
    *idx_out = as_vf_op->my_idx();
  }

  // got it!
  return mr.maps.forms.at(0);
}

/*!
 * Try to convert a form to a regaccess.
 */
std::optional<RegisterAccess> form_as_ra(Form* form) {
  auto as_atom = form_as_atom(form);
  if (as_atom && as_atom->is_var()) {
    return as_atom->var();
  }
  return {};
}

bool try_vector_reset_inline(const Env& env,
                             FormPool& pool,
                             FormStack& stack,
                             FormElement* store_element) {
  // the store
  int store_idx = -1;
  auto store = is_load_store_vector_to_reg(Register(Reg::VF, 0), store_element, false, &store_idx);
  if (!store) {
    return false;
  }

  // remove these from the stack.
  // stack.pop(1);

  // the store here _should_ have failed propagation and just given us a variable.
  // if this is causing issues, we can run this check before propagating, as well call this from
  // the function that attempts the pop.
  auto store_var = form_as_ra(store);
  if (!store_var) {
    env.func->warnings.warning("Almost found vector reset, but couldn't get store var.");
    // stack.push_form_element(new_thing->elts().at(0), true);
    return false;
  }

  // ignore the store as a use. This will allow the entire vector-! expression to be expression
  // propagated, if it is appropriate.
  if (store_var) {
    auto menv = const_cast<Env*>(&env);
    menv->disable_use(*store_var);
  }

  // now try to see if we can pop the first arg (destination vector).
  bool got_orig = false;
  RegisterAccess orig;
  store = repop_passthrough_arg(store, stack, env, &orig, &got_orig);

  if (!store) {
    return false;
  }

  // create the actual  form
  Form* new_thing = pool.form<GenericElement>(
      GenericOperator::make_function(pool.form<ConstantTokenElement>("vector-reset!")),
      std::vector<Form*>{store});

  if (got_orig) {
    // we got a value for the destination.  because we used the special repop passthrough,
    // we're responsible for inserting a set to set the var that we "stole" from.
    // We do this through push_value_to_reg, so it can be propagated if needed, but only if
    // somebody will actually read the output.
    // to tell, we look at the live out of the store op and the end - the earlier one would of
    // course be live out always because the store will read it again.

    auto& op_info = env.reg_use().op.at(store_idx);
    if (op_info.live.find(orig.reg()) == op_info.live.end()) {
      // nobody reads it, don't bother.
      stack.push_form_element(new_thing->elts().at(0), true);
    } else {
      stack.push_value_to_reg(orig, new_thing, true, TypeSpec("vector"));
    }

  } else {
    stack.push_form_element(new_thing->elts().at(0), true);
  }

  return true;
}
}  // namespace

void VectorFloatLoadStoreElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  mark_popped();

  auto loc_as_deref = m_location->try_as_element<DerefElement>();
  if (loc_as_deref) {
    auto root = loc_as_deref->base();
    auto atom = form_as_atom(root);
    if (atom && atom->get_kind() == SimpleAtom::Kind::VARIABLE) {
      m_addr_type = env.get_variable_type(atom->var(), true);
    }
  }

  auto name = env.func->name();
  // don't find vector-! inside of vector-!.
  if (!m_is_load && name != "vector-!" && name != "vector+!" && name != "vector-reset!") {
    if (try_vector_reset_inline(env, pool, stack, this)) {
      return;
    }
  }

  if (loc_as_deref) {
    auto root = loc_as_deref->base();
    auto atom = form_as_atom(root);
    if (atom && atom->get_kind() == SimpleAtom::Kind::VARIABLE) {
      loc_as_deref->set_base(pop_to_forms({atom->var()}, env, pool, stack, true).at(0));
    }
  }
  stack.push_form_element(this, true);
}

void MethodOfTypeElement::update_from_stack(const Env& env,
                                            FormPool& pool,
                                            FormStack& stack,
                                            std::vector<FormElement*>* result,
                                            bool allow_side_effects) {
  mark_popped();
  auto type = pop_to_forms({m_type_reg}, env, pool, stack, allow_side_effects).at(0);

  auto type_as_deref = type->try_as_element<DerefElement>();
  if (type_as_deref) {
    if (type_as_deref->tokens().size() > 1 &&
        type_as_deref->tokens().back().is_field_name("type")) {
      type_as_deref->tokens().pop_back();
      result->push_back(pool.alloc_element<GenericElement>(
          GenericOperator::make_fixed(FixedOperatorKind::METHOD_OF_OBJECT),
          std::vector<Form*>{type, pool.form<ConstantTokenElement>(m_method_info.name)}));
      return;
    } else if (type_as_deref->tokens().size() == 1 &&
               type_as_deref->tokens().back().is_field_name("type")) {
      result->push_back(pool.alloc_element<GenericElement>(
          GenericOperator::make_fixed(FixedOperatorKind::METHOD_OF_OBJECT),
          std::vector<Form*>{type_as_deref->base(),
                             pool.form<ConstantTokenElement>(m_method_info.name)}));
      return;
    }
  }

  result->push_back(pool.alloc_element<GenericElement>(
      GenericOperator::make_fixed(FixedOperatorKind::METHOD_OF_TYPE),
      std::vector<Form*>{type, pool.form<ConstantTokenElement>(m_method_info.name)}));
}

void SimpleAtomElement::update_from_stack(const Env&,
                                          FormPool&,
                                          FormStack&,
                                          std::vector<FormElement*>* result,
                                          bool) {
  mark_popped();
  result->push_back(this);
}

void StringConstantElement::update_from_stack(const Env&,
                                              FormPool&,
                                              FormStack&,
                                              std::vector<FormElement*>* result,
                                              bool) {
  mark_popped();
  result->push_back(this);
}

void GetMethodElement::update_from_stack(const Env&,
                                         FormPool&,
                                         FormStack&,
                                         std::vector<FormElement*>* result,
                                         bool) {
  mark_popped();
  result->push_back(this);
}

void CondNoElseElement::update_from_stack(const Env&,
                                          FormPool&,
                                          FormStack&,
                                          std::vector<FormElement*>* result,
                                          bool) {
  mark_popped();
  result->push_back(this);
}

void ConstantTokenElement::update_from_stack(const Env&,
                                             FormPool&,
                                             FormStack&,
                                             std::vector<FormElement*>* result,
                                             bool) {
  mark_popped();
  result->push_back(this);
}

void ConstantFloatElement::update_from_stack(const Env&,
                                             FormPool&,
                                             FormStack&,
                                             std::vector<FormElement*>* result,
                                             bool) {
  mark_popped();
  result->push_back(this);
}

void StackStructureDefElement::update_from_stack(const Env&,
                                                 FormPool&,
                                                 FormStack&,
                                                 std::vector<FormElement*>* result,
                                                 bool) {
  mark_popped();
  result->push_back(this);
}

void StackSpillValueElement::update_from_stack(const Env&,
                                               FormPool&,
                                               FormStack&,
                                               std::vector<FormElement*>* result,
                                               bool) {
  mark_popped();
  result->push_back(this);
}

void GetSymbolStringPointer::update_from_stack(const Env&,
                                               FormPool&,
                                               FormStack&,
                                               std::vector<FormElement*>* result,
                                               bool) {
  mark_popped();
  result->push_back(this);
}

void DefstateElement::update_from_stack(const Env&,
                                        FormPool&,
                                        FormStack&,
                                        std::vector<FormElement*>* result,
                                        bool) {
  mark_popped();
  result->push_back(this);
}

void DefskelgroupElement::update_from_stack(const Env&,
                                            FormPool&,
                                            FormStack&,
                                            std::vector<FormElement*>* result,
                                            bool) {
  mark_popped();
  result->push_back(this);
}

void DefpartgroupElement::update_from_stack(const Env&,
                                            FormPool&,
                                            FormStack&,
                                            std::vector<FormElement*>* result,
                                            bool) {
  mark_popped();
  result->push_back(this);
}

void DefpartElement::update_from_stack(const Env&,
                                       FormPool&,
                                       FormStack&,
                                       std::vector<FormElement*>* result,
                                       bool) {
  mark_popped();
  result->push_back(this);
}

void ResLumpMacroElement::update_from_stack(const Env&,
                                            FormPool&,
                                            FormStack&,
                                            std::vector<FormElement*>* result,
                                            bool) {
  mark_popped();
  result->push_back(this);
}

void WithDmaBufferAddBucketElement::update_from_stack(const Env&,
                                                      FormPool&,
                                                      FormStack&,
                                                      std::vector<FormElement*>* result,
                                                      bool) {
  mark_popped();
  result->push_back(this);
}

void LabelDerefElement::update_from_stack(const Env& env,
                                          FormPool& pool,
                                          FormStack& stack,
                                          std::vector<FormElement*>* result,
                                          bool allow_side_effects) {
  mark_popped();
  auto label_var = pop_to_forms({m_var}, env, pool, stack, allow_side_effects).at(0);
  auto atom = form_as_atom(label_var);
  if (!atom || !atom->is_label()) {
    throw std::runtime_error(fmt::format("LabelDerefElement didn't get a label, got {} instead",
                                         label_var->to_string(env)));
  }

  if (atom->label() != m_lid) {
    throw std::runtime_error(
        fmt::format("Label ID error in LabelDerefElement: {} vs {}", atom->label(), m_lid));
  }

  auto as_label = make_label_load(m_lid, env, pool, m_size, m_load_kind);
  if (!as_label) {
    throw std::runtime_error(
        fmt::format("Unable to figure out label load for {}", env.file->labels.at(m_lid).name));
  }
  result->push_back(as_label);
}

void LabelElement::push_to_stack(const Env&, FormPool&, FormStack& stack) {
  mark_popped();
  stack.push_form_element(this, true);
}

void BreakElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  mark_popped();
  FormStack temp_stack(false);
  for (auto& elt : return_code->elts()) {
    elt->push_to_stack(env, pool, temp_stack);
  }

  std::vector<FormElement*> new_entries;
  new_entries = temp_stack.rewrite(pool, env);

  ASSERT(!new_entries.empty());
  return_code->clear();

  for (int i = 0; i < ((int)new_entries.size()); i++) {
    stack.push_form_element(new_entries.at(i), true);
  }

  return_code->push_back(pool.alloc_element<EmptyElement>());
  stack.push_form_element(this, true);
}

}  // namespace decompiler
