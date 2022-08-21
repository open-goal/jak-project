#include <algorithm>
#include <array>
#include <limits>
#include <tuple>

#include "common/util/Assert.h"
#include "common/util/print_float.h"
#include "decompiler/IR2/GenericElementMatcher.h"
#include "decompiler/IR2/bitfields.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "insert_lets.h"

namespace decompiler {

/*
Part 1:
Create a std::unordered_map<ProgVar, std::vector<FormElement*>> which maps a program variable to the
collection of FormElement* which reference it.

Part 2:
For each ProgVar, find the lowest common ancestor Form* of the FormElement*'s in the above map.

Part 3:
For each Form*, find the smallest range of FormElement*s which include all uses of the ProgVar

Part 4:
Sort these from the largest to smaller range.

This makes sure that at a single level (in the original tree), we insert larger lets first, leaving
us with only one nesting case to worry about in the next step.

Check the first FormElement* which uses the ProgVar.
If it is a (set! var xxx), then we can insert a let.

If we are inserting directly inside of another let, at the beginning of that let's body, add to that
let. This makes the scope larger than it needs to be, but this seems like it will lead to more
readable code.

If the previous let variables appear in the definition of new one, make the let into a let*
 */

namespace {
FormElement* rewrite_let(LetElement* in, const Env& env, FormPool& pool, LetRewriteStats& stats);

std::vector<Form*> path_up_tree(Form* in, const Env&) {
  std::vector<Form*> path;

  while (in) {
    path.push_back(in);
    //    lg::warn("In: {}", in->to_string(env));
    if (in->parent_element) {
      //      lg::warn("  {}", in->parent_element->to_string(env));
      in = in->parent_element->parent_form;
    } else {
      in = nullptr;
    }
  }
  //  lg::warn("DONE\n");
  return path;
}

Form* lca_form(Form* a, Form* b, const Env& env) {
  (void)env;
  if (!a) {
    return b;
  }
  //
  //  fmt::print("lca {} ({}) and {} ({})\n", a->to_string(env), (void*)a, b->to_string(env),
  //   (void*)b);

  auto a_up = path_up_tree(a, env);
  auto b_up = path_up_tree(b, env);

  int ai = a_up.size() - 1;
  int bi = b_up.size() - 1;

  Form* result = nullptr;
  while (ai >= 0 && bi >= 0) {
    if (a_up.at(ai) == b_up.at(bi)) {
      result = a_up.at(ai);
    } else {
      break;
    }
    ai--;
    bi--;
  }
  if (!result) {
    fmt::print("{} bad form is {}\n\n{}\n", env.func->name(), a->to_string(env), b->to_string(env));
  }
  ASSERT(result);

  // fmt::print("{}\n\n", result->to_string(env));
  return result;
}

bool is_constant_int(const Form* f, int val) {
  auto as_atom = form_as_atom(f);
  return as_atom && as_atom->is_int(val);
}

FormElement* rewrite_as_dotimes(LetElement* in, const Env& env, FormPool& pool) {
  // dotimes OpenGOAL:
  /*
     (defmacro dotimes (var &rest body)
       "Loop like for (int i = 0; i < end; i++)"
       `(let ((,(first var) 0))
         (while (< ,(first var) ,(second var))
                 ,@body
                 (+1! ,(first var))
                 )
          ,@(cddr var)
          )
       )
   */

  // should have this anyway, but double check so we don't throw this away.
  if (in->entries().size() != 1) {
    return nullptr;
  }

  // look for setting a var to zero.
  auto ra = in->entries().at(0).dest;
  auto var = env.get_variable_name(ra);
  if (!is_constant_int(in->entries().at(0).src, 0)) {
    return nullptr;
  }

  // still have to check body for the increment and have to check that the lt operates on the right
  // thing.
  Matcher while_matcher = Matcher::while_loop(
      Matcher::op_fixed(FixedOperatorKind::LT, {Matcher::any_reg(0), Matcher::any(1)}),
      Matcher::any(2));

  auto mr = match(while_matcher, in->body());
  if (!mr.matched) {
    return nullptr;
  }

  // check the lt operation:
  auto lt_var = mr.maps.regs.at(0);
  ASSERT(lt_var);
  if (env.get_variable_name(*lt_var) != var) {
    return nullptr;  // wrong variable checked
  }

  // check the body
  auto body = mr.maps.forms.at(2);
  auto last_in_body = body->elts().back();

  Matcher increment_matcher = Matcher::op_fixed(FixedOperatorKind::ADDITION_IN_PLACE,
                                                {Matcher::any_reg(0), Matcher::integer(1)});

  auto int_mr = match(increment_matcher, last_in_body);
  if (!int_mr.matched) {
    return nullptr;
  }

  auto inc_var = int_mr.maps.regs.at(0);
  ASSERT(inc_var);
  if (env.get_variable_name(*inc_var) != var) {
    return nullptr;  // wrong variable incremented
  }

  // success! here we commit to modifying this:

  // first, remove the increment
  body->pop_back();

  return pool.alloc_element<CounterLoopElement>(CounterLoopElement::Kind::DOTIMES,
                                                in->entries().at(0).dest, *lt_var, *inc_var,
                                                mr.maps.forms.at(1), body);
}

std::tuple<MatchResult, Form*, bool> rewrite_shelled_return_form(
    const Matcher& matcher,
    FormElement* in,
    const Env& env,
    FormPool& pool,
    std::function<Form*(FormElement*, const MatchResult&, const Env&, FormPool&)> func,
    const TypeSpec* const strip_cast) {
  auto mr = match(matcher, in);

  // TODO check forbidden form
  if (!mr.matched) {
    auto as_set = dynamic_cast<SetFormFormElement*>(in);
    if (as_set) {
      auto sub_res = rewrite_shelled_return_form(matcher, as_set->src()->try_as_single_element(),
                                                 env, pool, func, strip_cast);

      if (std::get<0>(sub_res).matched) {
        if (std::get<2>(sub_res) && strip_cast) {
          // strip lowest-level (pointer <type>) cast.
          if (as_set->cast_for_set() && env.dts->ts.tc(*as_set->cast_for_set(), *strip_cast)) {
            as_set->set_cast_for_set({});
          }
          if (as_set->cast_for_define() &&
              env.dts->ts.tc(*as_set->cast_for_define(), *strip_cast)) {
            as_set->set_cast_for_define({});
          }
        }
        return {std::get<0>(sub_res),
                pool.form<SetFormFormElement>(as_set->dst(), std::get<1>(sub_res),
                                              as_set->cast_for_set(), as_set->cast_for_define()),
                false};
      }
    }

    auto as_cast = dynamic_cast<CastElement*>(in);
    if (as_cast) {
      auto sub_res = rewrite_shelled_return_form(
          matcher, as_cast->source()->try_as_single_element(), env, pool, func, strip_cast);

      if (std::get<0>(sub_res).matched) {
        if (std::get<2>(sub_res) && strip_cast && env.dts->ts.tc(as_cast->type(), *strip_cast)) {
          // strip lowest-level (pointer <type>) cast instead.
          return {std::get<0>(sub_res), std::get<1>(sub_res), false};
        }
        return {std::get<0>(sub_res),
                pool.form<CastElement>(as_cast->type(), std::get<1>(sub_res), as_cast->numeric()),
                false};
      }
    }

    auto as_gen = dynamic_cast<GenericElement*>(in);
    if (as_gen) {
      int i = 0;
      int m = as_gen->elts().size();
      for (; i < m; ++i) {
        auto sub_res = rewrite_shelled_return_form(
            matcher, as_gen->elts().at(i)->try_as_single_element(), env, pool, func, strip_cast);

        if (std::get<0>(sub_res).matched) {
          std::vector<Form*> shell_args;
          for (int ii = 0; ii < m; ++ii) {
            if (ii == i) {
              shell_args.push_back(std::get<1>(sub_res));
            } else {
              shell_args.push_back(as_gen->elts().at(ii));
            }
          }

          return {std::get<0>(sub_res), pool.form<GenericElement>(as_gen->op(), shell_args), false};
        }
      }
    }

    auto as_cond_ne = dynamic_cast<CondNoElseElement*>(in);
    if (as_cond_ne) {
      auto sub_res = rewrite_shelled_return_form(
          matcher, as_cond_ne->entries.at(0).condition->try_as_single_element(), env, pool, func,
          strip_cast);

      if (std::get<0>(sub_res).matched) {
        as_cond_ne->entries.at(0).condition = std::get<1>(sub_res);
        return {std::get<0>(sub_res), pool.form<CondNoElseElement>(as_cond_ne->entries), false};
      }
    }

    auto as_cond_e = dynamic_cast<CondWithElseElement*>(in);
    if (as_cond_e) {
      auto sub_res = rewrite_shelled_return_form(
          matcher, as_cond_e->entries.at(0).condition->try_as_single_element(), env, pool, func,
          strip_cast);

      if (std::get<0>(sub_res).matched) {
        as_cond_e->entries.at(0).condition = std::get<1>(sub_res);
        return {std::get<0>(sub_res),
                pool.form<CondWithElseElement>(as_cond_e->entries, as_cond_e->else_ir), false};
      }
    }

    auto as_while = dynamic_cast<WhileElement*>(in);
    if (as_while) {
      auto sub_res = rewrite_shelled_return_form(
          matcher, as_while->condition->try_as_single_element(), env, pool, func, strip_cast);

      if (std::get<0>(sub_res).matched) {
        return {std::get<0>(sub_res), pool.form<WhileElement>(std::get<1>(sub_res), as_while->body),
                false};
      }
    }

    auto as_counter = dynamic_cast<CounterLoopElement*>(in);
    if (as_counter) {
      auto sub_res =
          rewrite_shelled_return_form(matcher, as_counter->counter_value()->try_as_single_element(),
                                      env, pool, func, strip_cast);

      if (std::get<0>(sub_res).matched) {
        return {std::get<0>(sub_res),
                pool.form<CounterLoopElement>(as_counter->kind(), as_counter->var_init(),
                                              as_counter->var_check(), as_counter->var_inc(),
                                              std::get<1>(sub_res), as_counter->body()),
                false};
      }
    }

    return {mr, nullptr, false};
  }

  auto new_f = func(in, mr, env, pool);
  if (!new_f) {
    mr.matched = false;
  }
  return {mr, new_f, true};
}

FormElement* rewrite_as_send_event(LetElement* in,
                                   const Env& env,
                                   FormPool& pool,
                                   LetRewriteStats& stats) {
  if (in->entries().size() != 1) {
    return nullptr;
  }

  // (let ((block-var (new 'stack-no-clear 'event-message-block))
  auto block_var = in->entries().at(0).dest;
  const auto& block_var_reg = block_var.reg();
  auto block_var_name = env.get_variable_name(block_var);
  auto block_src = in->entries().at(0).src->try_as_element<StackStructureDefElement>();
  if (!block_src) {
    return nullptr;
  }

  auto& e = block_src->entry();
  if (e.ref_type != TypeSpec("event-message-block")) {
    return nullptr;
  }

  if (e.hint.container_type != StackStructureHint::ContainerType::NONE) {
    return nullptr;
  }

  auto body = in->body();
  if (body->size() < 4) {  // from, num-params, message, call
    // fmt::print(" fail: size\n");
    return nullptr;
  }

  ////////////////////////////////////////////////////////
  // (set! (-> block-var from) <something>)
  bool not_proc = false;
  Matcher set_from_matcher;
  switch (env.version) {
    case GameVersion::Jak1:
      set_from_matcher = Matcher::set(
          Matcher::deref(Matcher::reg(block_var_reg), false, {DerefTokenMatcher::string("from")}),
          Matcher::any_reg(1));
      break;
    case GameVersion::Jak2:
      // in jak 2, the event message block holds a ppointer instead.
      set_from_matcher = Matcher::set(
          Matcher::deref(Matcher::reg(block_var_reg), false, {DerefTokenMatcher::string("from")}),
          Matcher::op_fixed(FixedOperatorKind::PROCESS_TO_PPOINTER, {Matcher::any_reg(1)}));
      break;
    default:
      ASSERT(false);
  }
  auto from_mr = match(set_from_matcher, body->at(0));
  if (!from_mr.matched) {
    // initial matcher failed. try more advanced "from" matcher now.
    Matcher set_from_form_matcher;
    switch (env.version) {
      case GameVersion::Jak1:
        set_from_form_matcher = Matcher::set(
            Matcher::deref(Matcher::any_reg(0), false, {DerefTokenMatcher::string("from")}),
            Matcher::any(1));
        break;
      case GameVersion::Jak2:
        set_from_form_matcher = Matcher::set(
            Matcher::deref(Matcher::any_reg(0), false, {DerefTokenMatcher::string("from")}),
            Matcher::op_fixed(FixedOperatorKind::PROCESS_TO_PPOINTER, {Matcher::any(1)}));
        break;
      default:
        ASSERT(false);
    }

    from_mr = match(set_from_form_matcher, body->at(0));
    if (!from_mr.matched) {
      return nullptr;
    }
    fmt::print("case 1: {}\n", from_mr.maps.forms.at(1)->to_string(env));
    not_proc = true;
  }

  // if we couldnt match with simple reg matching that means it's a more complex form.
  if (!not_proc) {
    auto from_var = *from_mr.maps.regs.at(1);
    if (from_var.reg() != Register(Reg::GPR, Reg::S6)) {
      // it's OK to not be the s6 register, just means we have to manually specify it later.
      not_proc = true;
    }
  }

  ////////////////////////////////////////////////////////
  // (set! (-> a1-14 num-params) param-count) where param-count is a constant integer
  Matcher set_num_params_matcher = Matcher::set(
      Matcher::deref(Matcher::reg(block_var_reg), false, {DerefTokenMatcher::string("num-params")}),
      Matcher::any_integer(1));
  auto num_params_mr = match(set_num_params_matcher, body->at(1));
  if (!num_params_mr.matched) {
    // fmt::print(" fail: pc1\n");
    return nullptr;
  }
  int param_count = num_params_mr.maps.ints.at(1);
  ASSERT(param_count >= 0 && param_count < 7);
  if (body->size() != 4 + param_count) {
    // fmt::print(" fail: pc3\n");
    return nullptr;
  }

  ////////////////////////////////////////////////////////
  // (set! (-> a1-14 message) the-message-name)
  Matcher set_message_matcher = Matcher::set(
      Matcher::deref(Matcher::reg(block_var_reg), false, {DerefTokenMatcher::string("message")}),
      Matcher::any(1));
  auto set_message_mr = match(set_message_matcher, body->at(2));
  if (!set_message_mr.matched) {
    // fmt::print(" fail: msg1\n");
    return nullptr;
  }
  Form* message_name = set_message_mr.maps.forms.at(1);

  ////////////////////////////////////////////////////////
  // (set! (-> a1-14 param X) the-param-value)
  std::vector<Form*> param_values;
  for (int param_idx = 0; param_idx < param_count; param_idx++) {
    auto as_let = dynamic_cast<LetElement*>(body->at(3 + param_idx));
    if (as_let) {
      auto rewritten = rewrite_let(as_let, env, pool, stats);
      if (rewritten) {
        body->at(3 + param_idx) = rewritten;
        rewritten->parent_form = body;
      }
    }
    auto set_form = body->at(3 + param_idx);
    Matcher set_param_matcher = Matcher::set(
        Matcher::deref(Matcher::reg(block_var_reg), false,
                       {DerefTokenMatcher::string("param"), DerefTokenMatcher::integer(param_idx)}),
        Matcher::any(1));
    auto set_param_mr = match(set_param_matcher, set_form);
    if (!set_param_mr.matched) {
      // fmt::print(" fail: pv {} 1: {}\n", param_idx, set_form->to_string(env));
      return nullptr;
    }

    auto param_val = set_param_mr.maps.forms.at(1);
    auto param_val_cast = param_val->try_as_element<CastElement>();
    // strip uint cast, if we have it.
    if (param_val_cast && param_val_cast->type() == TypeSpec("uint")) {
      param_val = param_val_cast->source();
    }

    // fancy casting if necessary
    if (param_val->to_form(env).is_int()) {
      auto msg_str = message_name->to_string(env);
      auto val = param_val->to_form(env).as_int();
      if (val &&
          ((param_idx == 1 && (msg_str == "'change-state" || msg_str == "'change-state-no-go" ||
                               msg_str == "'art-joint-anim")) ||
           (param_idx == 0 && (msg_str == "'no-look-around" || msg_str == "'no-load-wait" ||
                               msg_str == "'force-blend")))) {
        param_val = pool.form<GenericElement>(
            GenericOperator::make_function(pool.form<ConstantTokenElement>("seconds")),
            pool.form<ConstantTokenElement>(seconds_to_string(param_val->to_form(env).as_int())));
      } else if (param_idx == 1) {
        auto parm0_str = param_values.at(0)->to_string(env);
        if (parm0_str == "'powerup" || parm0_str == "'pickup") {
          auto enum_ts = env.dts->ts.try_enum_lookup("pickup-type");
          if (enum_ts) {
            param_val = cast_to_int_enum(enum_ts, pool, env, param_val->to_form(env).as_int());
          }
        }
      }
    }

    param_values.push_back(param_val);
  }

  ////////////////////////////////////////////////////////
  // (send-event-function <dest> <block-var>)
  Matcher call_matcher = Matcher::op(GenericOpMatcher::func(Matcher::symbol("send-event-function")),
                                     {Matcher::any(0), Matcher::reg(block_var_reg)});
  auto mr_with_shell = rewrite_shelled_return_form(
      call_matcher, body->at(3 + param_count), env, pool,
      [&](FormElement* /*s_in*/, const MatchResult& mr, const Env& /*env*/, FormPool& pool) {
        Form* send_destination = mr.maps.forms.at(0);

        // time to build the macro!
        std::vector<Form*> macro_args = {send_destination, message_name};
        if (not_proc) {
          // something was going on with from. build :from key.
          macro_args.push_back(pool.form<ConstantTokenElement>(":from"));
          // fill in the value for it
          if (from_mr.maps.forms.find(1) != from_mr.maps.forms.end()) {
            // matched some form. we can just copy it.
            macro_args.push_back(from_mr.maps.forms.at(1));
          } else {
            // matched reg but it wasnt s6
            macro_args.push_back(alloc_var_form(*from_mr.maps.regs.at(1), pool));
          }
        }
        for (int i = 0; i < param_count; i++) {
          macro_args.push_back(param_values.at(i));
        }

        auto oper = GenericOperator::make_fixed(FixedOperatorKind::SEND_EVENT);
        return pool.form<GenericElement>(oper, macro_args);
      },
      nullptr);

  if (!std::get<0>(mr_with_shell).matched) {
    // lg::error("shell match failed, dont know this form: {}",
    //           body->at(3 + param_count)->to_string(env));
    return nullptr;
  }

  auto elt = std::get<1>(mr_with_shell)->try_as_single_element();
  return elt;
}

FormElement* rewrite_as_countdown(LetElement* in, const Env& env, FormPool& pool) {
  // dotimes OpenGOAL:
  /*
    (defmacro countdown (var &rest body)
      "Loop like for (int i = end; i-- > 0)"
      `(let ((,(first var) ,(second var)))
         (while (!= ,(first var) 0)
           (set! ,(first var) (- ,(first var) 1))
           ,@body
           )
         )
      )
   */

  // should have this anyway, but double check so we don't throw this away.
  if (in->entries().size() != 1) {
    return nullptr;
  }

  // look for setting a var to the initial value.
  auto ra = in->entries().at(0).dest;
  auto idx_var = env.get_variable_name(ra);

  // still have to check body for the increment and have to check that the lt operates on the right
  // thing.
  Matcher while_matcher = Matcher::while_loop(
      Matcher::op(GenericOpMatcher::condition(IR2_Condition::Kind::NONZERO), {Matcher::any_reg(0)}),
      Matcher::any(2));

  auto mr = match(while_matcher, in->body());
  if (!mr.matched) {
    return nullptr;
  }

  // check the zero operation:
  auto lt_var = mr.maps.regs.at(0);
  ASSERT(lt_var);
  if (env.get_variable_name(*lt_var) != idx_var) {
    return nullptr;  // wrong variable checked
  }

  // check the body
  auto body = mr.maps.forms.at(2);
  auto first_in_body = body->elts().front();

  Matcher increment_matcher = Matcher::op_fixed(FixedOperatorKind::ADDITION_IN_PLACE,
                                                {Matcher::any_reg(0), Matcher::integer(-1)});

  auto int_mr = match(increment_matcher, first_in_body);
  if (!int_mr.matched) {
    return nullptr;
  }

  auto inc_var = int_mr.maps.regs.at(0);
  ASSERT(inc_var);
  if (env.get_variable_name(*inc_var) != idx_var) {
    return nullptr;  // wrong variable incremented
  }

  // success! here we commit to modifying this:

  // first, remove the increment
  body->elts().erase(body->elts().begin());

  return pool.alloc_element<CounterLoopElement>(CounterLoopElement::Kind::COUNTDOWN,
                                                in->entries().at(0).dest, *lt_var, *inc_var,
                                                in->entries().at(0).src, body);
}

FormElement* fix_up_abs(LetElement* in, const Env& env, FormPool& pool) {
  /*
    (let ((v0-0 x)
          )
     (abs v0-0)
     )
   */

  if (in->entries().size() != 1) {
    return nullptr;
  }

  // look for setting a temp.
  auto temp = in->entries().at(0).dest;
  auto temp_name = env.get_variable_name(temp);

  Form* src = in->entries().at(0).src;

  auto body_matcher = Matcher::op_fixed(FixedOperatorKind::ABS, {Matcher::any_reg(0)});
  auto mr = match(body_matcher, in->body());
  if (!mr.matched) {
    return nullptr;
  }

  ASSERT(mr.maps.regs.at(0));
  auto abs_var_name = env.get_variable_name(*mr.maps.regs.at(0));
  if (abs_var_name != temp_name) {
    return nullptr;
  }

  // success!
  return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::ABS),
                                            src);
}

FormElement* fix_up_abs_2(LetElement* in, const Env& env, FormPool& pool) {
  /*
   * (let ((result in))
   *   (set! result (abs result))
   *   ...
   *   )
   *
   * -> should become.
   *    (let ((result (abs in)))
   *      )
   */

  if (in->entries().size() != 1) {
    return nullptr;
  }

  if (in->body()->elts().empty()) {
    return nullptr;
  }

  // look for setting a temp.
  auto temp = in->entries().at(0).dest;
  auto temp_name = env.get_variable_name(temp);

  Form* src = in->entries().at(0).src;

  auto first_as_set = dynamic_cast<SetVarElement*>(in->body()->elts().front());
  if (!first_as_set) {
    return nullptr;
  }

  auto dest_var_name = env.get_variable_name(first_as_set->dst());
  if (dest_var_name != temp_name) {
    return nullptr;
  }

  auto matcher = Matcher::op_fixed(FixedOperatorKind::ABS, {Matcher::any_reg(0)});

  auto mr = match(matcher, first_as_set->src());
  if (!mr.matched) {
    return nullptr;
  }

  ASSERT(mr.maps.regs.at(0));

  auto abs_var_name = env.get_variable_name(*mr.maps.regs.at(0));
  if (abs_var_name != temp_name) {
    return nullptr;
  }

  // success!
  // modify the let entry:
  in->entries().at(0).src = pool.alloc_single_element_form<GenericElement>(
      nullptr, GenericOperator::make_fixed(FixedOperatorKind::ABS), src);
  // remove the (set! x (abs x))
  in->body()->elts().erase(in->body()->elts().begin());
  return in;
}

FormElement* rewrite_empty_let(LetElement* in, const Env&, FormPool&) {
  if (in->entries().size() != 1) {
    return nullptr;
  }

  if (!in->body()->elts().empty()) {
    return nullptr;
  }

  auto reg = in->entries().at(0).dest.reg();
  if (reg.get_kind() == Reg::GPR && !reg.allowed_local_gpr()) {
    return nullptr;
  }

  return in->entries().at(0).src->try_as_single_element();
}

Form* strip_truthy(Form* in) {
  auto as_ge = in->try_as_element<GenericElement>();
  if (as_ge) {
    if (as_ge->op().kind() == GenericOperator::Kind::CONDITION_OPERATOR &&
        as_ge->op().condition_kind() == IR2_Condition::Kind::TRUTHY) {
      in = as_ge->elts().at(0);
    }
  }
  return in;
}

FormElement* rewrite_set_vector(LetElement* in, const Env& env, FormPool& pool) {
  if (in->entries().size() != 1) {
    return nullptr;
  }

  auto in_vec = env.get_variable_name(in->entries().at(0).dest);

  auto& body_elts = in->body()->elts();
  if (body_elts.size() != 4) {
    return nullptr;
  }

  std::vector<Form*> sources;
  for (int i = 0; i < 4; i++) {
    auto elt_as_form_form = dynamic_cast<SetFormFormElement*>(body_elts.at(i));
    if (!elt_as_form_form) {
      return nullptr;
    }
    auto dst = elt_as_form_form->dst();
    sources.push_back(elt_as_form_form->src());
    Matcher dst_matcher = Matcher::deref(Matcher::any_reg(0), false,
                                         {DerefTokenMatcher::string(std::string(1, "xyzw"[i]))});
    auto mr = match(dst_matcher, dst);
    if (!mr.matched) {
      return nullptr;
    }
    if (in_vec != env.get_variable_name(*mr.maps.regs.at(0))) {
      return nullptr;
    }
  }

  std::vector<Form*> args;
  args.push_back(in->entries().at(0).src);
  for (auto& src : sources) {
    args.push_back(src);
  }

  auto op = GenericOperator::make_function(
      pool.alloc_single_element_form<ConstantTokenElement>(nullptr, "set-vector!"));
  return pool.alloc_element<GenericElement>(op, args);
}

FormElement* rewrite_set_vector_2(LetElement* in, const Env& env, FormPool& pool) {
  if (in->entries().size() != 1) {
    return nullptr;
  }

  auto in_vec = env.get_variable_name(in->entries().at(0).dest);
  auto src_as_deref = in->entries().at(0).src->try_as_element<DerefElement>();
  if (!src_as_deref) {
    return nullptr;
  }

  auto& body_elts = in->body()->elts();
  if (body_elts.size() != 4) {
    return nullptr;
  }

  std::vector<Form*> sources;
  for (int i = 0; i < 4; i++) {
    auto elt_as_form_form = dynamic_cast<SetFormFormElement*>(body_elts.at(i));
    if (!elt_as_form_form) {
      return nullptr;
    }
    auto dst = elt_as_form_form->dst();
    sources.push_back(elt_as_form_form->src());
    Matcher dst_matcher = Matcher::deref(
        Matcher::any_reg(0), false,
        {DerefTokenMatcher::integer(0), DerefTokenMatcher::string(std::string(1, "xyzw"[i]))});
    auto mr = match(dst_matcher, dst);
    if (!mr.matched) {
      return nullptr;
    }
    if (in_vec != env.get_variable_name(*mr.maps.regs.at(0))) {
      return nullptr;
    }
  }

  src_as_deref->tokens().push_back(DerefToken::make_int_constant(0));

  std::vector<Form*> args;
  args.push_back(in->entries().at(0).src);
  for (auto& src : sources) {
    args.push_back(src);
  }

  auto op = GenericOperator::make_function(
      pool.alloc_single_element_form<ConstantTokenElement>(nullptr, "set-vector!"));
  return pool.alloc_element<GenericElement>(op, args);
}

ShortCircuitElement* get_or(Form* in) {
  // strip off truthy
  in = strip_truthy(in);

  return in->try_as_element<ShortCircuitElement>();
}

FormElement* rewrite_as_case_no_else(LetElement* in, const Env& env, FormPool& pool) {
  if (in->entries().size() != 1) {
    return nullptr;
  }

  auto* cond = in->body()->try_as_element<CondNoElseElement>();
  if (!cond) {
    return nullptr;
  }

  auto case_var = in->entries().at(0).dest;
  auto& case_var_uses = env.get_use_def_info(case_var);
  int found_uses = 0;
  if (case_var_uses.def_count() != 1) {
    return nullptr;
  }
  auto case_var_name = env.get_variable_name(case_var);

  std::vector<CaseElement::Entry> entries;

  for (auto& e : cond->entries) {
    // first, lets see if its just (= case_var <expr>)
    auto single_matcher =
        Matcher::op_fixed(FixedOperatorKind::EQ, {Matcher::any_reg(0), Matcher::any(1)});

    auto single_matcher_result = match(single_matcher, e.condition);

    Form* single_value = nullptr;
    if (single_matcher_result.matched) {
      auto var_name = env.get_variable_name(*single_matcher_result.maps.regs.at(0));
      if (var_name == case_var_name) {
        single_value = single_matcher_result.maps.forms.at(1);
      }
    }

    if (single_value) {
      entries.push_back({{single_value}, e.body});
      found_uses++;
      continue;
    }

    // try as an or (or (= case_var <expr>) ...)
    auto* as_or = get_or(e.condition);
    if (!as_or) {
      return nullptr;
    }

    CaseElement::Entry current_entry;
    for (auto& or_case : as_or->entries) {
      auto or_single_matcher_result = match(single_matcher, strip_truthy(or_case.condition));
      if (!or_single_matcher_result.matched) {
        return nullptr;
      }
      auto var_name = env.get_variable_name(*or_single_matcher_result.maps.regs.at(0));
      if (var_name != case_var_name) {
        return nullptr;
      }
      found_uses++;
      current_entry.vals.push_back(or_single_matcher_result.maps.forms.at(1));
    }
    current_entry.body = e.body;
    entries.push_back(current_entry);

    // no match
    // return nullptr;
  }

  if (found_uses != case_var_uses.use_count()) {
    return nullptr;
  }

  return pool.alloc_element<CaseElement>(in->entries().at(0).src, entries, nullptr);
  return nullptr;
}

FormElement* rewrite_as_case_with_else(LetElement* in, const Env& env, FormPool& pool) {
  if (in->entries().size() != 1) {
    return nullptr;
  }

  auto* cond = in->body()->try_as_element<CondWithElseElement>();
  if (!cond) {
    return nullptr;
  }

  auto case_var = in->entries().at(0).dest;
  auto& case_var_uses = env.get_use_def_info(case_var);
  int found_uses = 0;
  if (case_var_uses.def_count() != 1) {
    return nullptr;
  }
  auto case_var_name = env.get_variable_name(case_var);

  std::vector<CaseElement::Entry> entries;

  for (auto& e : cond->entries) {
    // first, lets see if its just (= case_var <expr>)
    auto single_matcher =
        Matcher::op_fixed(FixedOperatorKind::EQ, {Matcher::any_reg(0), Matcher::any(1)});

    auto single_matcher_result = match(single_matcher, e.condition);

    Form* single_value = nullptr;
    if (single_matcher_result.matched) {
      auto var_name = env.get_variable_name(*single_matcher_result.maps.regs.at(0));
      if (var_name == case_var_name) {
        single_value = single_matcher_result.maps.forms.at(1);
      }
    }

    if (single_value) {
      entries.push_back({{single_value}, e.body});
      found_uses++;
      continue;
    }

    // try as an or (or (= case_var <expr>) ...)
    auto* as_or = get_or(e.condition);
    if (!as_or) {
      return nullptr;
    }

    CaseElement::Entry current_entry;
    for (auto& or_case : as_or->entries) {
      auto or_single_matcher_result = match(single_matcher, strip_truthy(or_case.condition));
      if (!or_single_matcher_result.matched) {
        return nullptr;
      }
      auto var_name = env.get_variable_name(*or_single_matcher_result.maps.regs.at(0));
      if (var_name != case_var_name) {
        return nullptr;
      }
      found_uses++;
      current_entry.vals.push_back(or_single_matcher_result.maps.forms.at(1));
    }
    current_entry.body = e.body;
    entries.push_back(current_entry);

    // no match
    // return nullptr;
  }

  if (found_uses != case_var_uses.use_count()) {
    return nullptr;
  }

  return pool.alloc_element<CaseElement>(in->entries().at(0).src, entries, cond->else_ir);
  return nullptr;
}

bool var_name_equal(const Env& env, const std::string& a, std::optional<RegisterAccess> b) {
  ASSERT(b);
  return env.get_variable_name(*b) == a;
}

Form* match_ja_set(const Env& env,
                   const std::string& ch_var_name,
                   const std::string& field_name,
                   int arr_idx,
                   Form* in,
                   int* idx,
                   bool* bad) {
  ASSERT(idx);
  ASSERT(bad);
  if (*idx >= in->size()) {
    // *bad = true;
    return nullptr;
  }

  auto deref_matcher =
      arr_idx == -1
          ? Matcher::deref(Matcher::any_reg(0), false, {DerefTokenMatcher::string(field_name)})
          : Matcher::deref(
                Matcher::any_reg(0), false,
                {DerefTokenMatcher::string(field_name), DerefTokenMatcher::integer(arr_idx)});
  auto mr = match(Matcher::set(deref_matcher, Matcher::any(1)), in->at(*idx));
  if (!mr.matched) {
    // TODO what if didn't match but it was some weird thing?? i guess later size checks fix that.
    return nullptr;
  }

  if (!var_name_equal(env, ch_var_name, mr.maps.regs.at(0))) {
    lg::error("[{}] JA MACRO ERROR channel var not {} in set {} {}", env.func->name(), ch_var_name,
              field_name, arr_idx);
    *bad = true;
    return nullptr;
  }

  *idx += 1;
  return mr.maps.forms.at(1);
}

void ja_push_form_to_args(FormPool& pool,
                          std::vector<Form*>& args,
                          Form* form,
                          const std::string& key_name) {
  if (form) {
    args.push_back(pool.form<ConstantTokenElement>(fmt::format(":{}", key_name)));
    args.push_back(form);
  }
}

Form* strip_cast(const std::string& type, Form* in) {
  auto casted = dynamic_cast<CastElement*>(in->try_as_single_element());
  if (casted && casted->type() == TypeSpec(type)) {
    in = casted->source();
  }
  return in;
}

FormElement* rewrite_joint_macro(LetElement* in, const Env& env, FormPool& pool) {
  // this function checks for the behemoth (ja) macro.

  // TODO this should honestly just use its own custom element instead of GenericElement
  // since it would make analyzing after rewriting it MUCH easier, but I am kind of lazy right now

  const static std::unordered_map<std::string, std::string> num_func_remap = {
      {"num-func-identity", "identity"},   {"num-func-+!", "+!"},       {"num-func--!", "-!"},
      {"num-func-loop!", "loop!"},         {"num-func-seek!", "seek!"}, {"num-func-chan", "chan"},
      {"num-func-blend-in!", "blend-in!"}, {"num-func-none", "none"}};

  // should have this anyway, but double check so we don't throw this away.
  if (in->entries().size() != 1) {
    return nullptr;
  }

  // look for setting a var to (-> self skel root-channel ,channel).
  // yes, channel is actually not always just 0!
  auto ra = in->entries().at(0).dest;
  auto var = env.get_variable_name(ra);
  auto mr_chan = match(
      Matcher::deref(Matcher::s6(), false,
                     {DerefTokenMatcher::string("skel"), DerefTokenMatcher::string("root-channel"),
                      DerefTokenMatcher::any_expr_or_int(0)}),
      in->entries().at(0).src);
  if (!mr_chan.matched) {
    return nullptr;
  }
  auto channel_form = mr_chan.int_or_form_to_form(pool, 0);

  // now we checks for set!'s. the actual contents of the macro are not very complicated to match.
  // there is just a LOT to match. and then to write!
  bool bad = false;
  int idx = 0;
  auto set_fi = match_ja_set(env, var, "frame-interp", -1, in->body(), &idx, &bad);
  auto set_dist = match_ja_set(env, var, "dist", -1, in->body(), &idx, &bad);
  auto set_fg = match_ja_set(env, var, "frame-group", -1, in->body(), &idx, &bad);
  auto set_p0 = match_ja_set(env, var, "param", 0, in->body(), &idx, &bad);
  auto set_p1 = match_ja_set(env, var, "param", 1, in->body(), &idx, &bad);
  auto set_nf = match_ja_set(env, var, "num-func", -1, in->body(), &idx, &bad);
  auto set_fn = match_ja_set(env, var, "frame-num", -1, in->body(), &idx, &bad);

  // lastly, match the function call.
  enum { NO_FUNC, EVAL, NO_EVAL } func_status = NO_FUNC;
  Form* arg_group = nullptr;
  std::string arg_num_func;
  if (idx < in->body()->size()) {
    auto mr_func =
        match(Matcher::op(GenericOpMatcher::func(Matcher::any_symbol(3)),
                          {Matcher::any_reg(0), Matcher::any(1), Matcher::any_symbol(2)}),
              in->body()->at(idx));
    if (mr_func.matched) {
      // NOTE : it's actually fine for there to be no func. we just forgo setting the num! param.
      if (!var_name_equal(env, var, mr_func.maps.regs.at(0))) {
        return nullptr;
      }

      arg_group = mr_func.maps.forms.at(1);
      arg_num_func = mr_func.maps.strings.at(2);
      const auto& func_name = mr_func.maps.strings.at(3);
      if (func_name == "joint-control-channel-group!") {
        func_status = NO_EVAL;
      } else if (func_name == "joint-control-channel-group-eval!") {
        func_status = EVAL;
      } else {
        // wtf happened?
        lg::error("[{}] JA MACRO ERROR func name: {}", env.func->name(), func_name);
        return nullptr;
      }

      if (num_func_remap.find(arg_num_func) == num_func_remap.end()) {
        lg::error("[{}] JA MACRO ERROR unknown num func: {}", env.func->name(), arg_num_func);
        return nullptr;
      }

      auto group_lisp = strip_cast("art-joint-anim", arg_group)->to_form(env);
      if (group_lisp.is_symbol() && group_lisp.as_symbol()->name == "#f") {
        arg_group = nullptr;
      }

      idx++;
    }
  }

  // PSYCHE! there may actually be a second frame-num set. for some reason. sigh...
  auto set_fn2 = match_ja_set(env, var, "frame-num", -1, in->body(), &idx, &bad);

  if (bad) {
    // an error occurred
    return nullptr;
  }

  // check that we have nothing more
  if (in->body()->size() > idx) {
    lg::error("[{}] JA MACRO ERROR elts matched: {}/{}", env.func->name(), idx, in->body()->size());
    return nullptr;
  }

  // now check the arguments.
  if (set_fn && set_fn2) {
    lg::error("[{}] JA MACRO ERROR both frame nums: {}", env.func->name(),
              set_fn->to_form(env).print(), set_fn2->to_form(env).print());
    ASSERT(false);
    return nullptr;
  }

  if (func_status != NO_FUNC && set_fg && arg_group) {
    ASSERT(set_fg->to_form(env) == arg_group->to_form(env));
    // lg::info("p0: {} p1: {} nf: {} fn: {}", !!set_p0, !!set_p1, !!set_nf, !!set_fn);
  }

  auto form_fg = set_fg ? set_fg : arg_group;
  auto matcher_max_num = Matcher::cast(
      "float",
      Matcher::op_fixed(
          FixedOperatorKind::ADDITION,
          {form_fg
               ? Matcher::deref(Matcher::any(1), false,
                                {DerefTokenMatcher::string("data"), DerefTokenMatcher::integer(0),
                                 DerefTokenMatcher::string("length")})
               : Matcher::deref(
                     Matcher::any_reg(0), false,
                     {DerefTokenMatcher::string("frame-group"), DerefTokenMatcher::string("data"),
                      DerefTokenMatcher::integer(0), DerefTokenMatcher::string("length")}),
           Matcher::integer(-1)}));

  // DONE CHECKING EVERYTHING!!! Now write the goddamn macro.
  std::vector<Form*> args;

  // check the channel arg
  auto channel_arg = channel_form->to_form(env);
  if (!channel_arg.is_int(0)) {
    ja_push_form_to_args(pool, args, channel_form, "chan");
  }

  if (func_status != NO_FUNC) {
    // check the group! arg
    if (form_fg) {
      ja_push_form_to_args(pool, args, strip_cast("art-joint-anim", form_fg), "group!");
    }

    const std::string prelim_num =
        num_func_remap.count(arg_num_func) == 0 ? "" : num_func_remap.at(arg_num_func);
    Form* num_form = nullptr;
    // check the num! arg
    if (prelim_num == "identity") {
      if (set_fn2) {
        auto obj_fn2 = set_fn2->to_form(env);
        if (obj_fn2.is_float(0.0)) {
          num_form = pool.form<ConstantTokenElement>("min");
        } else {
          auto mr = match(matcher_max_num, set_fn2);
          if (mr.matched &&
              ((form_fg && mr.maps.forms.at(1)->to_form(env) == form_fg->to_form(env)) ||
               (!form_fg && var_name_equal(env, var, mr.maps.regs.at(0))))) {
            num_form = pool.form<ConstantTokenElement>("max");
          } else {
            num_form = pool.form<GenericElement>(
                GenericOperator::make_function(pool.form<ConstantTokenElement>(prelim_num)),
                set_fn2);
          }
        }
        set_fn2 = nullptr;
      }
    } else if (prelim_num == "loop!" || prelim_num == "+!" || prelim_num == "-!") {
      if (set_p0) {
        auto obj_p0 = set_p0->to_form(env);
        if (obj_p0.is_float(1.0)) {
          num_form = pool.form<GenericElement>(
              GenericOperator::make_function(pool.form<ConstantTokenElement>(prelim_num)));
        } else {
          auto mr = match(matcher_max_num, set_p0);
          if (mr.matched &&
              ((form_fg && mr.maps.forms.at(1)->to_form(env) == form_fg->to_form(env)) ||
               (!form_fg && var_name_equal(env, var, mr.maps.regs.at(0))))) {
            num_form = pool.form<GenericElement>(
                GenericOperator::make_function(pool.form<ConstantTokenElement>(prelim_num)),
                pool.form<ConstantTokenElement>("max"));
          } else {
            num_form = pool.form<GenericElement>(
                GenericOperator::make_function(pool.form<ConstantTokenElement>(prelim_num)),
                set_p0);
          }
        }
        set_p0 = nullptr;
      }
    } else if (prelim_num == "chan") {
      if (set_p0) {
        auto obj_p0 = set_p0->to_form(env);
        if (obj_p0.is_float((goos::FloatType)((goos::IntType)obj_p0.as_float()))) {
          num_form = pool.form<GenericElement>(
              GenericOperator::make_function(pool.form<ConstantTokenElement>("chan")),
              pool.form<SimpleAtomElement>(
                  SimpleAtom::make_int_constant((goos::IntType)obj_p0.as_float())));
        } else {
          lg::error("[{}] JA MACRO ERROR bad chan arg: {}", env.func->name(), obj_p0.print());
          ASSERT_MSG(false, "chan case");
        }
        set_p0 = nullptr;
      }
    } else if (prelim_num == "seek!") {
      ASSERT(set_p0 && set_p1);
      std::vector<Form*> seek_args;

      // (the float (1- (-> (the art-joint-anim ,group!) data 0 length)))
      // (the float (1- (-> ja-ch frame-group data 0 length)))
      auto mr = match(matcher_max_num, set_p0);
      if (!mr.matched || (form_fg && mr.maps.forms.at(1)->to_form(env) != form_fg->to_form(env)) ||
          (!form_fg && !var_name_equal(env, var, mr.maps.regs.at(0)))) {
        // did not match default
        seek_args.push_back(set_p0);
      }

      auto obj_p1 = set_p1->to_form(env);
      if (!obj_p1.is_float(1.0)) {
        // did not match default
        if (seek_args.size() < 1) {
          seek_args.push_back(mr.matched ? pool.form<ConstantTokenElement>("max") : set_p0);
        }
        seek_args.push_back(set_p1);
      }

      // do not print :param0 and :param1 keys
      set_p0 = nullptr;
      set_p1 = nullptr;

      num_form = pool.form<GenericElement>(
          GenericOperator::make_function(pool.form<ConstantTokenElement>("seek!")), seek_args);
    }

    if (!num_form) {
      num_form = pool.form<ConstantTokenElement>(prelim_num);
    }

    ja_push_form_to_args(pool, args, num_form, "num!");
  } else if (form_fg) {
    ja_push_form_to_args(pool, args, strip_cast("art-joint-anim", form_fg), "group!");
  }

  if (set_fn) {
    auto mr = match(matcher_max_num, set_fn);
    if (mr.matched && ((form_fg && mr.maps.forms.at(1)->to_form(env) == form_fg->to_form(env)) ||
                       (!form_fg && var_name_equal(env, var, mr.maps.regs.at(0))))) {
      set_fn = pool.form<ConstantTokenElement>("max");
    }
  }

  // other generic args
  ja_push_form_to_args(pool, args, set_fi, "frame-interp");
  ja_push_form_to_args(pool, args, set_dist, "dist");
  // ja_push_form_to_args(pool, args, form_fg, "frame-group");
  ja_push_form_to_args(pool, args, set_p0, "param0");
  ja_push_form_to_args(pool, args, set_p1, "param1");
  ja_push_form_to_args(pool, args, set_nf, "num-func");
  ja_push_form_to_args(pool, args, set_fn, "frame-num");

  // TODO
  if (set_fn2) {
    lg::error("[{}] JA MACRO ERROR ignoring frame-num 2: {}", env.func->name(),
              set_fn2->to_form(env).print());
    ASSERT(func_status != NO_FUNC && arg_num_func == "num-func-identity");
    return nullptr;
  }
  if (set_p0 || set_p1) {
    lg::error("[{}] JA MACRO ERROR something still using params: {}", env.func->name(),
              in->to_form(env).print());
    ASSERT(false);
    return nullptr;
  }

  return pool.alloc_element<GenericElement>(
      GenericOperator::make_function(
          pool.form<ConstantTokenElement>(func_status == NO_EVAL ? "ja-no-eval" : "ja")),
      args);
}

FormElement* rewrite_proc_new(LetElement* in, const Env& env, FormPool& pool) {
  // this function checks for the process-spawn macros.
  // it uses recursive form scanning to wrap the macro inside a potential "shell"
  // which means this is probably slow and bad

  auto is_full_let = in->entries().size() == 1 && in->body()->size() == 1;
  if (!(is_full_let || (in->entries().size() == 2))) {
    return nullptr;
  }

  auto test = in->to_form(env).print();

  // look for setting a var to (get-process *default-dead-pool* logo-slave #x4000)
  auto ra = in->entries().at(0).dest;
  auto var = env.get_variable_name(ra);
  auto mr_get_proc = match(
      Matcher::func("get-process", {Matcher::any(0), Matcher::any_symbol(1), Matcher::any(2)}),
      in->entries().at(0).src);
  if (!mr_get_proc.matched) {
    return nullptr;
  }

  const auto& proc_type = mr_get_proc.maps.strings.at(1);
  auto macro_form =
      is_full_let ? in->body()->at(0) : in->entries().at(1).src->try_as_single_element();

  auto cast_type = TypeSpec("pointer", {TypeSpec(proc_type)});
  auto mr_with_shell = rewrite_shelled_return_form(
      Matcher::if_no_else(
          Matcher::op(GenericOpMatcher::condition(IR2_Condition::Kind::TRUTHY),
                      {Matcher::reg(ra.reg())}),
          Matcher::begin(
              {Matcher::any(0),
               Matcher::func_with_rest(
                   Matcher::match_or({Matcher::constant_token("run-now-in-process"),
                                      Matcher::constant_token("run-next-time-in-process")}),
                   {Matcher::reg(ra.reg()), Matcher::any(1)}),
               Matcher::any(2)})),
      macro_form, env, pool,
      [&](FormElement* s_in, const MatchResult& /*mr*/, const Env& env, FormPool& pool) {
        auto as_when = dynamic_cast<CondNoElseElement*>(s_in);
        const auto& when_body = as_when->entries.front().body->elts();

        // there's a let with an activate inside it that we want to check first
        auto as_let = dynamic_cast<LetElement*>(when_body.at(0));
        if (!as_let || as_let->entries().size() != 1 || as_let->body()->size() != 1) {
          return (Form*)nullptr;
        }

        auto as_func = dynamic_cast<GenericElement*>(when_body.at(1));
        if (!as_func) {
          return (Form*)nullptr;
        }
        bool is_next_time =
            as_func->op().func()->to_form(env).is_symbol("run-next-time-in-process");

        auto has_unused_let = dynamic_cast<LetElement*>(when_body.at(2));
        auto matcher_unused_let =
            Matcher::deref(Matcher::reg(ra.reg()), false, {DerefTokenMatcher::string("ppointer")});
        if (has_unused_let) {
          auto new_pptr = rewrite_empty_let(has_unused_let, env, pool);
          if (!new_pptr || !match(matcher_unused_let, new_pptr).matched) {
            return (Form*)nullptr;
          }
        } else if (!match(matcher_unused_let, when_body.at(2)).matched) {
          return (Form*)nullptr;
        }

        auto mr_activate_func = match(
            Matcher::op_fixed(FixedOperatorKind::METHOD_OF_TYPE,
                              {Matcher::symbol(proc_type), Matcher::constant_token("activate")}),
            as_let->entries().at(0).src);

        if (!mr_activate_func.matched) {
          return (Form*)nullptr;
        }

        auto activate_func_ra = as_let->entries().at(0).dest;

        auto mr_ac_call =
            match(Matcher::func(
                      Matcher::reg(activate_func_ra.reg()),
                      {proc_type == "process" ? Matcher::reg(ra.reg())
                                              : Matcher::cast(proc_type, Matcher::reg(ra.reg())),
                       Matcher::any(0), Matcher::any(1), Matcher::any(2)}),
                  as_let->body()->at(0));

        if (!mr_ac_call.matched) {
          return (Form*)nullptr;
        }

        // ok done checking that stuff, now make the macro!
        // once again, I should just make a new element for it. but... so lazy...

        std::string head = is_next_time ? "process-spawn-function" : "process-spawn";
        std::vector<Form*> args;

        // type and init func
        if (is_next_time) {
          args.push_back(pool.form<ConstantTokenElement>(proc_type));
          args.push_back(as_func->elts().at(1));
        } else {
          auto init_func = as_func->elts().at(1)->to_form(env);
          if (init_func.is_symbol("manipy-init") && proc_type == "manipy") {
            head = "manipy-spawn";
          } else {
            args.push_back(pool.form<ConstantTokenElement>(proc_type));
            if (!init_func.is_symbol(fmt::format("{}-init-by-other", proc_type))) {
              ja_push_form_to_args(pool, args, as_func->elts().at(1), "init");
            }
          }
        }

        // args
        // this would be a great place to do some hacky stuff! hmm, maybe i will...
        for (size_t i = 2; i < as_func->elts().size(); ++i) {
          args.push_back(as_func->elts().at(i));
        }

        if (mr_ac_call.maps.forms.at(1)->to_string(env) != fmt::format("'{}", proc_type)) {
          ja_push_form_to_args(pool, args, mr_ac_call.maps.forms.at(1), "name");
        }
        if (!mr_get_proc.maps.forms.at(0)->to_form(env).is_symbol("*default-dead-pool*")) {
          args.push_back(pool.form<ConstantTokenElement>(":from"));
          args.push_back(mr_get_proc.maps.forms.at(0));
        }
        if (!mr_ac_call.maps.forms.at(0)->to_form(env).is_symbol("*default-pool*")) {
          args.push_back(pool.form<ConstantTokenElement>(":to"));
          args.push_back(mr_ac_call.maps.forms.at(0));
        }
        auto stack_arg = mr_ac_call.maps.forms.at(2)->to_string(env);
        if (stack_arg == "(&-> *dram-stack* 14336)") {
          // special case!
          ja_push_form_to_args(pool, args, pool.form<ConstantTokenElement>("*kernel-dram-stack*"),
                               "stack");
        } else if (stack_arg != "(the-as pointer #x70004000)") {
          ja_push_form_to_args(pool, args, mr_ac_call.maps.forms.at(2), "stack");
        }
        if (!mr_get_proc.maps.forms.at(2)->to_form(env).is_int(0x4000)) {
          ja_push_form_to_args(pool, args, mr_get_proc.maps.forms.at(2), "stack-size");
        }

        return pool.form<GenericElement>(
            GenericOperator::make_function(pool.form<ConstantTokenElement>(head)), args);
      },
      &cast_type);

  if (!std::get<0>(mr_with_shell).matched) {
    // lg::error("shell match failed, dont know this form: {}", macro_form->to_string(env));
    return nullptr;
  }

  if (!is_full_let) {
    // we're actually just editing entries in a let here.

    // delete us.
    in->entries().erase(in->entries().begin());
    // set the value of the new first entry to the macro
    in->entries().at(0).src = std::get<1>(mr_with_shell);
    in->entries().at(0).src->parent_element = in;

    // return us but modified?
    return in;
  }

  auto elt = std::get<1>(mr_with_shell)->try_as_single_element();
  ASSERT(elt);
  return elt;
}

FormElement* rewrite_attack_info(LetElement* in, const Env& env, FormPool& pool) {
  // this function checks for the static-attack-info macro.

  if (in->entries().size() != 1) {
    return nullptr;
  }

  auto test = in->to_form(env).print();

  // (let ((block-var (new 'static 'attack-info))
  auto block_src = in->entries().at(0).src->try_as_element<DecompiledDataElement>();
  if (!block_src) {
    return nullptr;
  }

  auto e = block_src->label_info();
  if (e && (*e).result_type != TypeSpec("attack-info")) {
    return nullptr;
  }

  const auto& label = block_src->label();
  const auto& words = env.file->words_by_seg.at(label.target_segment);
  u32 mask = words.at((label.offset + 64) / 4).data;
  u32 mask_implicit = 0;

  auto block_var = in->entries().at(0).dest;
  const auto& block_var_reg = block_var.reg();
  auto block_var_name = env.get_variable_name(block_var);

  const static std::map<std::string, std::pair<int, bool>> possible_args = {
      {"vector", {1, true}},    {"mode", {5, false}},     {"shove-back", {6, false}},
      {"shove-up", {7, false}}, {"control", {10, false}}, {"angle", {11, false}}};

  std::vector<std::pair<std::string, Form*>> args_in_info;
  for (int i = 0; i < in->body()->size() - 1; ++i) {
    auto s_elt = dynamic_cast<SetFormFormElement*>(in->body()->at(i));
    if (!s_elt) {
      lg::error("attack info err elt {} not a set!: {}", i, in->body()->at(i)->to_string(env));
      return nullptr;
    }

    auto d_elt = s_elt->dst()->try_as_element<DerefElement>();
    if (!d_elt) {
      lg::error("attack info err elt {} dst not a deref: {}", i, s_elt->dst()->to_string(env));
      return nullptr;
    }
    if (d_elt->tokens().size() != 1 && d_elt->tokens().size() != 2) {
      lg::error("attack info err elt {} invalid token len: {}", i, d_elt->to_string(env));
      return nullptr;
    }
    if (d_elt->tokens().at(0).kind() != DerefToken::Kind::FIELD_NAME) {
      lg::error("attack info err elt {} invalid token kind: {}", i, d_elt->to_string(env));
      return nullptr;
    }
    const auto& field_name = d_elt->tokens().at(0).field_name();
    auto arg_it = possible_args.find(field_name);
    if (arg_it == possible_args.end()) {
      lg::error("attack info unknown field {}", field_name);
      return nullptr;
    }
    if (arg_it->second.second &&
        (d_elt->tokens().size() != 2 || d_elt->tokens().at(1).field_name() != "quad")) {
      lg::error("attack info err elt {} invalid vector field: {}", i, d_elt->to_string(env));
      return nullptr;
    } else if (!arg_it->second.second && d_elt->tokens().size() != 1) {
      lg::error("attack info err elt {} invalid field: {}", i, d_elt->to_string(env));
      break;
    }

    if (arg_it->second.second) {
      auto d_src = s_elt->src()->try_as_element<DerefElement>();
      if (d_src) {
        if (d_src->tokens().size() == 1 && d_src->tokens().at(0).is_field_name("quad")) {
          args_in_info.push_back({field_name, d_src->base()});
        } else {
          d_src->tokens().pop_back();
          args_in_info.push_back({field_name, s_elt->src()});
        }
      }
    } else {
      if ((field_name == "shove-back" || field_name == "shove-up") &&
          s_elt->src()->to_form(env).is_float()) {
        args_in_info.push_back(
            {field_name, pool.form<GenericElement>(GenericOperator::make_function(
                                                       pool.form<ConstantTokenElement>("meters")),
                                                   pool.form<ConstantTokenElement>(meters_to_string(
                                                       s_elt->src()->to_form(env).as_float())))});
      } else {
        args_in_info.push_back({field_name, s_elt->src()});
      }
    }
    mask_implicit |= 1 << arg_it->second.first;
  }

  if ((mask & mask_implicit) != mask_implicit) {
    lg::error("attack info bad mask: #x{:x} but needed #x{:x}", mask, mask_implicit);
    ASSERT(false);
    return nullptr;
  }
  mask ^= mask_implicit;

  // (static-attack-info :mask <mask> etc)
  auto mr_with_shell = rewrite_shelled_return_form(
      Matcher::reg(block_var_reg), in->body()->at(in->body()->size() - 1), env, pool,
      [&](FormElement* /*s_in*/, const MatchResult& /*mr*/, const Env& env, FormPool& pool) {
        // time to build the macro!
        std::vector<Form*> macro_args;

        if (mask) {
          ja_push_form_to_args(
              pool, macro_args,
              decompiler::cast_to_bitfield_enum(env.dts->ts.try_enum_lookup("attack-mask"), pool,
                                                env, mask, true),
              "mask");
        }

        if (args_in_info.size() > 0) {
          std::vector<Form*> info_args;
          for (auto& [name, val] : args_in_info) {
            info_args.push_back(pool.form<GenericElement>(
                GenericOperator::make_function(pool.form<ConstantTokenElement>(name)), val));
          }
          auto head = GenericOperator::make_function(info_args.at(0));
          info_args.erase(info_args.begin());
          macro_args.push_back(pool.form<GenericElement>(head, info_args));
        }

        return pool.form<GenericElement>(
            GenericOperator::make_function(pool.form<ConstantTokenElement>("static-attack-info")),
            macro_args);
      },
      nullptr);

  if (!std::get<0>(mr_with_shell).matched) {
    // lg::error("shell match failed, dont know this form: {}",
    //           in->body()->at(in->body()->size() - 1)->to_string(env));
    return nullptr;
  }

  auto elt = std::get<1>(mr_with_shell)->try_as_single_element();
  ASSERT(elt);
  return elt;
}

/*!
 * Attempt to rewrite a let as another form.  If it cannot be rewritten, this will return nullptr.
 */
FormElement* rewrite_let(LetElement* in, const Env& env, FormPool& pool, LetRewriteStats& stats) {
  auto as_unused = rewrite_empty_let(in, env, pool);
  if (as_unused) {
    stats.unused++;
    return as_unused;
  }

  auto as_joint_macro = rewrite_joint_macro(in, env, pool);
  if (as_joint_macro) {
    stats.ja++;
    return as_joint_macro;
  }

  auto as_set_vector = rewrite_set_vector(in, env, pool);
  if (as_set_vector) {
    stats.set_vector++;
    return as_set_vector;
  }

  auto as_dotimes = rewrite_as_dotimes(in, env, pool);
  if (as_dotimes) {
    stats.dotimes++;
    return as_dotimes;
  }

  auto as_send_event = rewrite_as_send_event(in, env, pool, stats);
  if (as_send_event) {
    stats.send_event++;
    return as_send_event;
  }

  auto as_countdown = rewrite_as_countdown(in, env, pool);
  if (as_countdown) {
    stats.countdown++;
    return as_countdown;
  }

  auto as_case_no_else = rewrite_as_case_no_else(in, env, pool);
  if (as_case_no_else) {
    stats.case_no_else++;
    return as_case_no_else;
  }

  auto as_case_with_else = rewrite_as_case_with_else(in, env, pool);
  if (as_case_with_else) {
    stats.case_with_else++;
    return as_case_with_else;
  }

  auto as_set_vector2 = rewrite_set_vector_2(in, env, pool);
  if (as_set_vector2) {
    stats.set_vector2++;
    return as_set_vector2;
  }

  auto as_abs_2 = fix_up_abs_2(in, env, pool);
  if (as_abs_2) {
    stats.abs2++;
    return as_abs_2;
  }

  auto as_abs = fix_up_abs(in, env, pool);
  if (as_abs) {
    stats.abs++;
    return as_abs;
  }

  auto as_proc_new = rewrite_proc_new(in, env, pool);
  if (as_proc_new) {
    stats.proc_new++;
    return as_proc_new;
  }

  auto as_attack_info = rewrite_attack_info(in, env, pool);
  if (as_attack_info) {
    stats.attack_info++;
    return as_attack_info;
  }

  // nothing matched.
  return nullptr;
}

FormElement* rewrite_rand_float_gen(LetElement* in, const Env& env, FormPool& pool) {
  // this function checks for the (rand-float-gen) macro

  bool ret_in_body = in->entries().size() == 2;

  if (ret_in_body && in->body()->size() != 1) {
    return nullptr;
  }

  auto test = in->to_string(env);

  auto mr_div = match(
      Matcher::op_fixed(FixedOperatorKind::DIVISION,
                        {Matcher::cast("int", Matcher::func(Matcher::symbol("rand-uint31-gen"),
                                                            {Matcher::any(0)})),
                         Matcher::integer(256)}),
      in->entries().at(0).src);
  if (!mr_div.matched) {
    return nullptr;
  }

  const auto& reg1 = in->entries().at(0).dest.reg();
  auto gen = mr_div.maps.forms.at(0);
  if (gen->to_form(env).is_symbol("*random-generator*")) {
    gen = nullptr;
  }

  auto mr_or =
      match(Matcher::cast("number",
                          Matcher::op_fixed(FixedOperatorKind::LOGIOR,
                                            {Matcher::integer(0x3f800000), Matcher::reg(reg1)})),
            in->entries().at(1).src);
  if (!mr_or.matched) {
    return nullptr;
  }

  const auto& reg2 = in->entries().at(1).dest.reg();

  auto matcher_res =
      Matcher::op_fixed(FixedOperatorKind::ADDITION, {Matcher::single(-1.f), Matcher::reg(reg2)});

  if (!ret_in_body) {
    // simpler case

    auto mr_res = match(matcher_res, in->entries().at(2).src);

    if (!mr_res.matched) {
      return nullptr;
    }

    // delete us.
    in->entries().erase(in->entries().begin());
    in->entries().erase(in->entries().begin());

    auto head = GenericOperator::make_function(pool.form<ConstantTokenElement>("rand-float-gen"));

    // set the value of the new first entry to the macro
    in->entries().at(0).src =
        gen ? pool.form<GenericElement>(head, gen) : pool.form<GenericElement>(head);

    // return us but modified?
    in->clear_let_star();
    return in;
  } else {
    auto mr_res = rewrite_shelled_return_form(
        matcher_res, in->body()->at(0), env, pool,
        [&](FormElement* /*s_in*/, const MatchResult& /*mr*/, const Env& /*env*/, FormPool& pool) {
          auto head =
              GenericOperator::make_function(pool.form<ConstantTokenElement>("rand-float-gen"));
          return gen ? pool.form<GenericElement>(head, gen) : pool.form<GenericElement>(head);
        },
        nullptr);

    if (!std::get<0>(mr_res).matched) {
      // lg::error("shell match failed, dont know this form: {}",
      // in->body()->at(0)->to_string(env));
      return nullptr;
    }

    auto elt = std::get<1>(mr_res)->try_as_single_element();
    elt->parent_form = in->parent_form;
    return elt;
  }
}

FormElement* rewrite_multi_let_as_vector_dot(LetElement* in, const Env& env, FormPool& pool) {
  if (in->body()->size() != 3) {
    return nullptr;
  }

  /* The body:
      (.mula.s f1-11 f4-0)
      (.madda.s f2-1 f5-0)
      (.madd.s f1-12 f3-0 f6-0)
   */

  // get asm ops
  std::array<RegisterAccess[2], 3> vars;
  std::array<InstructionKind, 3> kinds = {InstructionKind::MULAS, InstructionKind::MADDAS,
                                          InstructionKind::MADDS};
  for (int i = 0; i < 3; i++) {
    auto as_op = dynamic_cast<AsmOpElement*>(in->body()->at(i));
    if (!as_op) {
      return nullptr;
    }
    if (as_op->op()->instruction().kind != kinds[i]) {
      return nullptr;
    }
    for (int j = 0; j < 2; j++) {
      ASSERT(as_op->op()->src(j).has_value());
      vars[i][j] = *as_op->op()->src(j);
    }
  }

  RegisterAccess output = *dynamic_cast<AsmOpElement*>(in->body()->at(2))->op()->dst();
  int start_idx = in->entries().size() - 6;

  std::optional<RegisterAccess> in_vars[2];
  for (int in_var = 0; in_var < 2; in_var++) {
    for (int axis = 0; axis < 3; axis++) {
      int idx = start_idx + in_var * 3 + axis;
      std::string axis_name(1, "xyz"[axis]);
      auto matcher =
          Matcher::deref(Matcher::any_reg(0), false, {DerefTokenMatcher::string(axis_name)});
      auto mr = match(matcher, in->entries().at(idx).src);
      if (!mr.matched) {
        return nullptr;
      }
      auto this_var = mr.maps.regs.at(0);
      ASSERT(this_var.has_value());
      if (in_vars[in_var].has_value()) {
        // seen it before
        if (env.get_variable_name(*this_var) != env.get_variable_name(*in_vars[in_var])) {
          return nullptr;
        }
      } else {
        ASSERT(axis == 0);
        // first time seeing it.
        in_vars[in_var] = this_var;
      }

      if (env.get_variable_name(vars[axis][in_var]) !=
          env.get_variable_name(in->entries().at(idx).dest)) {
        return nullptr;
      }
    }
  }

  // don't inline in the actual function...
  if (env.func->name() == "vector-dot") {
    return nullptr;
  }

  auto dot_op = alloc_generic_token_op(
      "vector-dot", {alloc_var_form(*in_vars[0], pool), alloc_var_form(*in_vars[1], pool)}, pool);
  auto dot_set = pool.alloc_element<SetVarElement>(output, pool.alloc_single_form(nullptr, dot_op),
                                                   true, TypeSpec("float"));

  // remove let forms:
  for (int i = 0; i < 6; i++) {
    in->entries().pop_back();
  }

  if (in->entries().empty()) {
    dot_set->parent_form = in->parent_form;
    return dot_set;
  }
  // replace body:
  in->body()->elts().clear();
  in->body()->push_back(dot_set);
  return in;
}

FormElement* rewrite_multi_let(LetElement* in,
                               const Env& env,
                               FormPool& pool,
                               LetRewriteStats& stats) {
  if (in->entries().size() >= 2) {
    auto as_rand_float_gen = rewrite_rand_float_gen(in, env, pool);
    if (as_rand_float_gen) {
      stats.rand_float_gen++;
      return as_rand_float_gen;
    }

    auto as_proc_new = rewrite_proc_new(in, env, pool);
    if (as_proc_new) {
      stats.proc_new++;
      return as_proc_new;
    }
  }

  if (in->entries().size() >= 6) {
    auto as_vector_dot = rewrite_multi_let_as_vector_dot(in, env, pool);
    if (as_vector_dot) {
      stats.vector_dot++;
      return as_vector_dot;
    }
  }

  return in;
}

Form* insert_cast_for_let(RegisterAccess dst,
                          const TypeSpec& src_type,
                          Form* src,
                          FormPool& pool,
                          const Env& env) {
  auto dst_type = env.get_variable_type(dst, true);

  if (src_type != dst_type) {
    /*auto src_as_cast = dynamic_cast<CastElement*>(src->try_as_single_element());
    if (src_as_cast) {
      if (env.dts->ts.tc(dst_type, src_as_cast->type())) {
        return src;  // no need to cast again.
      } else {
        // don't nest casts
        src_as_cast->set_type(dst_type);
        return src;
      }
    }*/
    auto as_single = src->try_as_single_element();
    if (as_single) {
      return pool.alloc_single_form(nullptr, make_cast_using_existing(as_single, dst_type, pool));
    }

    return pool.alloc_single_element_form<CastElement>(nullptr, dst_type, src);
  }

  return src;
}

bool register_can_hold_var(const Register& reg) {
  return reg.get_kind() == Reg::FPR || reg.get_kind() == Reg::GPR;
}
}  // namespace

LetStats insert_lets(const Function& func,
                     Env& env,
                     FormPool& pool,
                     Form* top_level_form,
                     LetRewriteStats& let_rewrite_stats) {
  (void)func;
  //    if (func.name() != "(method 4 pair)") {
  //      return {};
  //    }
  LetStats stats;

  // Stored per variable.
  struct PerVarInfo {
    std::string var_name;  // name used to uniquely identify
    RegisterAccess access;
    std::unordered_set<FormElement*> elts_using_var;  // all FormElements using var
    Form* lca_form = nullptr;  // the lowest common form that contains all the above elts
    int start_idx = -1;        // in the above form, first FormElement using var's index
    int end_idx = -1;          // in the above form, 1 + last FormElement using var's index
  };

  std::unordered_map<std::string, PerVarInfo> var_info;

  // Part 1, figure out which forms reference each var
  top_level_form->apply([&](FormElement* elt) {
    // for each element, figure out what vars we reference:
    RegAccessSet reg_accesses;
    elt->collect_vars(reg_accesses, false);

    //    if (!reg_accesses.empty()) {
    //      Form* f = elt->parent_form;
    //      while (f && f != top_level_form) {
    //        auto pe = f->parent_element;
    //        if (pe) {
    //          f = pe->parent_form;
    //        } else {
    //          f = nullptr;
    //        }
    //      }
    //
    //      ASSERT(f);
    //    }

    // and add it.
    for (auto& access : reg_accesses) {
      if (register_can_hold_var(access.reg())) {
        auto name = env.get_variable_name(access);
        var_info[name].elts_using_var.insert(elt);
        var_info[name].var_name = name;
        var_info[name].access = access;
      }
    }
  });

  stats.total_vars = var_info.size();

  // Part 2, figure out the lca form which contains all uses of a var
  for (auto& kv : var_info) {
    // fmt::print("--------------------- {}\n", kv.first);
    Form* lca = nullptr;
    for (auto fe : kv.second.elts_using_var) {
      lca = lca_form(lca, fe->parent_form, env);
    }
    ASSERT(lca);
    var_info[kv.first].lca_form = lca;
  }

  // Part 3, find the minimum range of FormElement's within the lca form that contain
  // all uses. This is the minimum possible range for a set!
  for (auto& kv : var_info) {
    // fmt::print("Setting range for let {}\n", kv.first);
    kv.second.start_idx = std::numeric_limits<int>::max();
    kv.second.end_idx = std::numeric_limits<int>::min();

    bool got_one = false;
    for (int i = 0; i < kv.second.lca_form->size(); i++) {
      RegAccessSet ras;
      kv.second.lca_form->at(i)->collect_vars(ras, true);
      bool uses = false;
      for (auto& ra : ras) {
        if ((ra.reg().get_kind() == Reg::FPR || ra.reg().get_kind() == Reg::GPR) &&
            env.get_variable_name(ra) == kv.second.var_name) {
          uses = true;
        }
      }
      if (uses) {
        //      if (kv.second.elts_using_var.find(kv.second.lca_form->at(i)) !=
        //          kv.second.elts_using_var.end()) {
        got_one = true;
        kv.second.start_idx = std::min(kv.second.start_idx, i);
        kv.second.end_idx = std::max(kv.second.end_idx, i + 1);
        // fmt::print("update range {} to {} because of {}\n", kv.second.start_idx,
        // kv.second.end_idx, kv.second.lca_form->at(i)->to_string(env));
      }
    }
    ASSERT(got_one);
  }

  // fmt::print("\n");

  // Part 4, sort the var infos in descending size.
  // this simplifies future passes.
  std::vector<PerVarInfo> sorted_info;
  for (auto& kv : var_info) {
    sorted_info.push_back(kv.second);
  }
  std::sort(sorted_info.begin(), sorted_info.end(), [](const PerVarInfo& a, const PerVarInfo& b) {
    return (a.end_idx - a.start_idx) > (b.end_idx - b.start_idx);
  });

  // Part 5, find where we want to insert lets.  But don't actually do any insertions.
  // Only variables that begin with a set! var value can be used in a let, so we may discard
  // some variables here.  Though I suspect most reasonable functions will not discard any.
  struct LetInsertion {
    Form* form = nullptr;
    int start_elt = -1;  // this is the set!
    SetVarElement* set_form = nullptr;
    int end_elt = -1;
    std::string name;
  };

  // stored per containing form.
  std::unordered_map<Form*, std::vector<LetInsertion>> possible_insertions;
  for (auto& info : sorted_info) {
    auto first_form = info.lca_form->at(info.start_idx);
    auto first_form_as_set = dynamic_cast<SetVarElement*>(first_form);
    if (first_form_as_set && register_can_hold_var(first_form_as_set->dst().reg()) &&
        env.get_variable_name(first_form_as_set->dst()) == env.get_variable_name(info.access) &&
        !first_form_as_set->info().is_eliminated_coloring_move) {
      bool allowed = true;

      RegAccessSet ras;
      first_form_as_set->src()->collect_vars(ras, true);
      for (auto ra : ras) {
        if (ra.reg() == first_form_as_set->dst().reg()) {
          if (env.get_variable_name(ra) == env.get_variable_name(first_form_as_set->dst())) {
            allowed = false;
            break;
          }
        }
      }
      // success!
      // fmt::print("Want let for {} range {} to {}\n",
      // env.get_variable_name(first_form_as_set->dst()), info.start_idx, info.end_idx);
      if (allowed) {
        LetInsertion li;
        li.form = info.lca_form;
        li.start_elt = info.start_idx;
        li.end_elt = info.end_idx;
        li.set_form = first_form_as_set;
        li.name = info.var_name;
        possible_insertions[li.form].push_back(li);
        stats.vars_in_lets++;
      }
    } else {
      // fmt::print("fail for {} : {}\n", info.var_name, first_form->to_string(env));
    }
  }

  // Part 6, expand ends of intervals to prevent "tangled lets"
  for (auto& group : possible_insertions) {
    // Note : this algorithm is not efficient.
    bool changed = true;
    while (changed) {
      changed = false;
      for (auto& let_a : group.second) {
        for (auto& let_b : group.second) {
          // If b starts within a and ends after a, expand a.
          if (let_b.start_elt > let_a.start_elt && let_b.start_elt < let_a.end_elt &&
              let_b.end_elt > let_a.end_elt) {
            changed = true;
            // fmt::print("Resized {}'s end to {}\n", let_a.set_form->dst().to_string(env),
            // let_b.end_elt);
            let_a.end_elt = let_b.end_elt;
          }
        }
      }
    }
  }

  // Part 7: insert lets!
  for (auto& group : possible_insertions) {
    // sort decreasing size.
    std::sort(group.second.begin(), group.second.end(),
              [](const LetInsertion& a, const LetInsertion& b) {
                return (a.end_elt - a.start_elt) > (b.end_elt - b.start_elt);
              });

    // ownership[elt_idx] = the let which actually has this.
    std::vector<int> ownership;
    ownership.resize(group.first->size(), -1);
    for (int let_idx = 0; let_idx < int(group.second.size()); let_idx++) {
      for (int elt_idx = group.second.at(let_idx).start_elt;
           elt_idx < group.second.at(let_idx).end_elt; elt_idx++) {
        ownership.at(elt_idx) = let_idx;
      }
    }

    // build lets
    std::vector<LetElement*> lets;
    lets.resize(group.first->size(), nullptr);
    // start at the smallest.
    for (size_t let_idx = group.second.size(); let_idx-- > 0;) {
      auto& let_desc = group.second.at(let_idx);
      std::vector<FormElement*> body;
      int elt_idx = let_desc.start_elt + 1;  // plus one to skip the variable def.
      while (elt_idx < let_desc.end_elt) {
        if (ownership.at(elt_idx) == int(let_idx)) {
          body.push_back(let_desc.form->at(elt_idx));
          elt_idx++;
        } else {
          auto existing_let = lets.at(ownership[elt_idx]);
          ASSERT(existing_let);
          auto& existing_let_info = group.second.at(ownership[elt_idx]);
          ASSERT(existing_let_info.start_elt == elt_idx);
          body.push_back(existing_let);
          elt_idx = existing_let_info.end_elt;
        }
      }
      ASSERT(elt_idx == let_desc.end_elt);
      auto new_let = pool.alloc_element<LetElement>(pool.alloc_sequence_form(nullptr, body));
      // insert a cast, if needed.
      auto casted_src = insert_cast_for_let(let_desc.set_form->dst(), let_desc.set_form->src_type(),
                                            let_desc.set_form->src(), pool, env);
      new_let->add_def(let_desc.set_form->dst(), casted_src);
      env.set_defined_in_let(let_desc.name);
      lets.at(let_idx) = new_let;
    }

    // now rebuild form
    int elt_idx = 0;
    std::vector<FormElement*> new_body;
    while (elt_idx < group.first->size()) {
      if (ownership.at(elt_idx) == -1) {
        new_body.push_back(group.first->at(elt_idx));
        elt_idx++;
      } else {
        auto existing_let = lets.at(ownership[elt_idx]);
        ASSERT(existing_let);
        auto& existing_let_info = group.second.at(ownership[elt_idx]);
        ASSERT(existing_let_info.start_elt == elt_idx);
        new_body.push_back(existing_let);
        elt_idx = existing_let_info.end_elt;
      }
    }
    ASSERT(elt_idx == group.first->size());

    group.first->elts() = new_body;
    group.first->claim_all_children();
  }

  // Part 8: recognize loop forms
  top_level_form->apply_form([&](Form* f) {
    for (auto& elt : f->elts()) {
      auto as_let = dynamic_cast<LetElement*>(elt);
      if (as_let) {
        auto rewritten = rewrite_let(as_let, env, pool, let_rewrite_stats);
        if (rewritten) {
          rewritten->parent_form = f;
          elt = rewritten;
        }
      }
    }
  });

  // Part 9: compact recursive lets:
  bool changed = true;
  while (changed) {
    changed = false;
    top_level_form->apply_form([&](Form* form) {
      for (int idx = 0; idx < form->size(); idx++) {
        auto* f = form->at(idx);
        auto as_let = dynamic_cast<LetElement*>(f);
        if (!as_let) {
          continue;
        }

        auto inner_let = dynamic_cast<LetElement*>(as_let->body()->try_as_single_element());
        if (!inner_let) {
          continue;
        }

        for (auto& e : inner_let->entries()) {
          as_let->add_entry(e);
        }

        as_let->set_body(inner_let->body());

        // rewrite:
        form->at(idx) = rewrite_multi_let(as_let, env, pool, let_rewrite_stats);
        ASSERT(form->at(idx)->parent_form == form);

        // check star
        for (auto& e : inner_let->entries()) {
          if (!as_let->is_star()) {
            RegAccessSet used;
            e.src->collect_vars(used, true);
            std::unordered_set<std::string> used_by_name;
            for (auto used_var : used) {
              used_by_name.insert(env.get_variable_name(used_var));
            }
            for (auto& old_entry : as_let->entries()) {
              if (used_by_name.find(env.get_variable_name(old_entry.dest)) != used_by_name.end()) {
                as_let->make_let_star();
                break;
              }
            }
          }
        }

        changed = true;
      }
    });
  }

  return stats;
}

}  // namespace decompiler
