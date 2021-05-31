#include <algorithm>
#include <limits>

#include "insert_lets.h"
#include "decompiler/IR2/GenericElementMatcher.h"
#include "decompiler/util/DecompilerTypeSystem.h"

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
std::vector<Form*> path_up_tree(Form* in) {
  std::vector<Form*> path;

  while (in) {
    path.push_back(in);
    // lg::warn("In: {}", in->to_string(env));
    if (in->parent_element) {
      // lg::warn("  {}", in->parent_element->to_string(env));
      in = in->parent_element->parent_form;
    } else {
      in = nullptr;
    }
  }
  // lg::warn("DONE\n");
  return path;
}

Form* lca_form(Form* a, Form* b, const Env& env) {
  (void)env;
  if (!a) {
    return b;
  }

  // fmt::print("lca {} ({}) and {} ({})\n", a->to_string(env), (void*)a, b->to_string(env),
  // (void*)b);

  auto a_up = path_up_tree(a);
  auto b_up = path_up_tree(b);

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
  assert(result);

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
  Matcher while_matcher =
      Matcher::while_loop(Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::LT),
                                      {Matcher::any_reg(0), Matcher::any(1)}),
                          Matcher::any(2));

  auto mr = match(while_matcher, in->body());
  if (!mr.matched) {
    return nullptr;
  }

  // check the lt operation:
  auto lt_var = mr.maps.regs.at(0);
  assert(lt_var);
  if (env.get_variable_name(*lt_var) != var) {
    return nullptr;  // wrong variable checked
  }

  // check the body
  auto body = mr.maps.forms.at(2);
  auto last_in_body = body->elts().back();

  // kind hacky
  Form fake_form;
  fake_form.elts().push_back(last_in_body);
  Matcher increment_matcher =
      Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::ADDITION_IN_PLACE),
                  {Matcher::any_reg(0), Matcher::integer(1)});

  auto int_mr = match(increment_matcher, &fake_form);
  if (!int_mr.matched) {
    return nullptr;
  }

  auto inc_var = int_mr.maps.regs.at(0);
  assert(inc_var);
  if (env.get_variable_name(*inc_var) != var) {
    return nullptr;  // wrong variable incremented
  }

  // success! here we commit to modifying this:

  // first, remove the increment
  body->pop_back();

  return pool.alloc_element<DoTimesElement>(in->entries().at(0).dest, *lt_var, *inc_var,
                                            mr.maps.forms.at(1), body);
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

  auto body_matcher =
      Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::ABS), {Matcher::any_reg(0)});
  auto mr = match(body_matcher, in->body());
  if (!mr.matched) {
    return nullptr;
  }

  assert(mr.maps.regs.at(0));
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

  auto matcher =
      Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::ABS), {Matcher::any_reg(0)});

  auto mr = match(matcher, first_as_set->src());
  if (!mr.matched) {
    return nullptr;
  }

  assert(mr.maps.regs.at(0));

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

FormElement* fix_up_vector_inline_zero(LetElement* in, const Env& env, FormPool& pool) {
  /*
   * (let ((local-trans (new 'stack-no-clear 'vector)))
   *   (set! (-> local-trans quad) (the-as uint128 0))
   */

  if (in->entries().size() != 1) {
    return nullptr;
  }

  if (in->body()->elts().empty()) {
    return nullptr;
  }

  Form* src = in->entries().at(0).src;
  auto src_as_stackvar = src->try_as_element<StackVarDefElement>();
  if (!src_as_stackvar) {
    return nullptr;
  }

  bool is_vector = src_as_stackvar->type() == TypeSpec("vector");
  bool is_matrix = src_as_stackvar->type() == TypeSpec("matrix");

  if (is_vector) {
    auto first_elt = in->body()->elts().at(0);

    auto matcher = Matcher::set(
        Matcher::deref(Matcher::any_reg(0), false, {DerefTokenMatcher::string("quad")}),
        Matcher::cast("uint128", Matcher::integer(0)));

    Form hack;
    hack.elts().push_back(first_elt);
    auto mr = match(matcher, &hack);

    if (mr.matched) {
      auto var = in->entries().at(0).dest;
      auto var_name = env.get_variable_name(var);

      if (var_name != env.get_variable_name(*mr.maps.regs.at(0))) {
        return nullptr;
      }

      auto new_op = pool.alloc_single_element_form<GenericElement>(
          nullptr,
          GenericOperator::make_function(
              pool.alloc_single_element_form<ConstantTokenElement>(nullptr, "new-stack-vector0")));
      src->parent_element = in;
      in->entries().at(0).src = new_op;
      in->body()->elts().erase(in->body()->elts().begin());
      return in;
    }
  } else if (is_matrix) {
    if (in->body()->elts().size() < 4) {
      return nullptr;
    }

    auto var = in->entries().at(0).dest;
    auto var_name = env.get_variable_name(var);

    for (int i = 0; i < 4; i++) {
      auto elt = in->body()->elts().at(i);

      auto matcher = Matcher::set(
          Matcher::deref(Matcher::any_reg(0), false,
                         {DerefTokenMatcher::string("vector"), DerefTokenMatcher::integer(i),
                          DerefTokenMatcher::string("quad")}),
          Matcher::cast("uint128", Matcher::integer(0)));

      Form hack;
      hack.elts().push_back(elt);
      auto mr = match(matcher, &hack);

      if (mr.matched) {
        if (var_name != env.get_variable_name(*mr.maps.regs.at(0))) {
          return nullptr;
        }
      } else {
        return nullptr;
      }
    }

    auto new_op = pool.alloc_single_element_form<GenericElement>(
        nullptr,
        GenericOperator::make_function(
            pool.alloc_single_element_form<ConstantTokenElement>(nullptr, "new-stack-matrix0")));
    src->parent_element = in;
    in->entries().at(0).src = new_op;
    in->body()->elts().erase(in->body()->elts().begin(), in->body()->elts().begin() + 4);
    return in;
  }

  return nullptr;
}

/*!
 * Attempt to rewrite a let as another form.  If it cannot be rewritten, this will return nullptr.
 */
FormElement* rewrite_let(LetElement* in, const Env& env, FormPool& pool) {
  auto as_dotimes = rewrite_as_dotimes(in, env, pool);
  if (as_dotimes) {
    return as_dotimes;
  }

  auto as_abs = fix_up_abs(in, env, pool);
  if (as_abs) {
    return as_abs;
  }

  auto as_abs_2 = fix_up_abs_2(in, env, pool);
  if (as_abs_2) {
    return as_abs_2;
  }

  auto as_vector = fix_up_vector_inline_zero(in, env, pool);
  if (as_vector) {
    return as_vector;
  }

  // nothing matched.
  return nullptr;
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

LetStats insert_lets(const Function& func, Env& env, FormPool& pool, Form* top_level_form) {
  (void)func;
  //    if (func.guessed_name.to_string() != "(method 4 pair)") {
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
    assert(lca);
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
    assert(got_one);
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
          assert(existing_let);
          auto& existing_let_info = group.second.at(ownership[elt_idx]);
          assert(existing_let_info.start_elt == elt_idx);
          body.push_back(existing_let);
          elt_idx = existing_let_info.end_elt;
        }
      }
      assert(elt_idx == let_desc.end_elt);
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
        assert(existing_let);
        auto& existing_let_info = group.second.at(ownership[elt_idx]);
        assert(existing_let_info.start_elt == elt_idx);
        new_body.push_back(existing_let);
        elt_idx = existing_let_info.end_elt;
      }
    }
    assert(elt_idx == group.first->size());

    group.first->elts() = new_body;
    group.first->claim_all_children();
  }

  // Part 8: recognize loop forms
  top_level_form->apply_form([&](Form* f) {
    for (auto& elt : f->elts()) {
      auto as_let = dynamic_cast<LetElement*>(elt);
      if (as_let) {
        auto rewritten = rewrite_let(as_let, env, pool);
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
    top_level_form->apply([&](FormElement* f) {
      auto as_let = dynamic_cast<LetElement*>(f);
      if (!as_let) {
        return;
      }

      auto inner_let = dynamic_cast<LetElement*>(as_let->body()->try_as_single_element());
      if (!inner_let) {
        return;
      }

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
        as_let->add_entry(e);
      }

      as_let->set_body(inner_let->body());
      changed = true;
    });
  }

  return stats;
}

}  // namespace decompiler
