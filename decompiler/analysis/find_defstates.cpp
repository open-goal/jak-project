

#include "find_defstates.h"
#include "common/goos/PrettyPrinter.h"
#include "common/type_system/state.h"
#include "decompiler/IR2/Form.h"
#include "decompiler/IR2/GenericElementMatcher.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"

namespace decompiler {

constexpr bool debug_defstates = false;
constexpr bool print_renames = false;

namespace {

/*!
 * Given the (set! <the-state> <reg-with-static-state>), returns the name of the state and the
 * more specific type.
 */
std::pair<std::string, TypeSpec> get_state_info(FormElement* state_set, const Env& env) {
  auto sff = dynamic_cast<SetFormFormElement*>(state_set);
  if (!sff) {
    env.func->warnings.error_and_throw(
        "Failed to identify defstate. The state symbol set was supposed to be: {}, but "
        "this doesn't look like a set.",
        state_set->to_string(env));
  }

  auto atom = form_as_atom(sff->dst());
  if (!atom || atom->get_kind() != SimpleAtom::Kind::SYMBOL_VAL) {
    env.func->warnings.error_and_throw(
        "Failed to identify defstate. The state symbol set was: {}, which doesn't set a symbol",
        state_set->to_string(env));
  }

  std::string state_name = atom->get_str();

  auto type = env.dts->symbol_types.find(state_name);
  if (type == env.dts->symbol_types.end()) {
    env.func->warnings.error_and_throw(
        "Identified a defstate for state {}, but there is no type information for this state.",
        state_name);
  }

  if (type->second.base_type() != "state") {
    env.func->warnings.error_and_throw(
        "Identified a defstate for state {}, but our type information thinks it is a {}, not a "
        "state.",
        state_name, type->second.print());
  }

  if (type->second.arg_count() == 0) {
    env.func->warnings.error_and_throw(
        "Identified a defstate for state {}, but there is no argument information.", state_name);
  }

  if (type->second.last_arg() == TypeSpec("none")) {
    env.func->warnings.error_and_throw(
        "Identified a defstate for state {}, but the process type is none. You must "
        "provide a process type as the final argument of a state",
        state_name);
  }

  return {atom->get_str(), type->second};
}

std::vector<DefstateElement::Entry> get_defstate_entries(
    Form* body,
    int body_index,
    const Env& env,
    const std::string& state_name,
    const RegisterAccess& let_dest_var,
    const TypeSpec& state_type,
    FormPool& pool,
    const std::optional<std::string>& virtual_child = {},
    const std::unordered_map<std::string, std::unordered_set<std::string>>& skip_states = {}) {
  std::vector<DefstateElement::Entry> entries;

  // next, all the handlers
  for (; body_index < body->size(); body_index++) {
    DefstateElement::Entry this_entry;
    auto matcher =
        Matcher::set(Matcher::deref(Matcher::any_reg(0), false, {DerefTokenMatcher::any_string(1)}),
                     Matcher::any(2));
    Form temp;
    temp.elts().push_back(body->at(body_index));
    auto mr = match(matcher, &temp);

    if (!mr.matched) {
      env.func->warnings.error_and_throw(
          "In defstate for state {}, failed to recognize handler set: {}", state_name,
          temp.to_string(env));
    }

    auto var = mr.maps.regs.at(0);
    auto name = mr.maps.strings.at(1);
    auto val = mr.maps.forms.at(2);

    auto handler_kind = handler_name_to_kind(name);
    while (val->try_as_element<CastElement>()) {
      val = val->try_as_element<CastElement>()->source();
    }
    this_entry.val = val;
    this_entry.kind = handler_kind;
    this_entry.is_behavior = false;

    if (!var || env.get_variable_name(*var) != env.get_variable_name(let_dest_var)) {
      if (var) {
        env.func->warnings.error_and_throw("Messed up defstate. State is in {}, but we set {}",
                                           env.get_variable_name(let_dest_var),
                                           env.get_variable_name(*var));
      } else {
        ASSERT(false);
      }
    }

    if (debug_defstates) {
      fmt::print("SET: {} to {}\n", name, val->to_string(env));
    }

    // now we try to find a function

    auto handler_atom = form_as_atom(val);
    if (handler_atom && handler_atom->is_label()) {
      auto handler_func = env.file->try_get_function_at_label(handler_atom->label());
      if (!handler_func) {
        env.func->warnings.error_and_throw("Failed to find handler function.");
      }

      this_entry.is_behavior = true;
      if (print_renames) {
        fmt::print("RENAME: {} to ", handler_func->name());
      }

      if (virtual_child) {
        handler_func->guessed_name.set_as_v_state(*virtual_child, state_name, handler_kind);
      } else {
        handler_func->guessed_name.set_as_nv_state(state_name, handler_kind);
      }
      if (print_renames) {
        fmt::print("{}\n", handler_func->name());
      }

      // scary part - modify the function type!
      handler_func->type = get_state_handler_type(handler_kind, state_type);
    } else if (handler_atom && handler_atom->is_sym_val()) {
      auto sym_type = env.dts->lookup_symbol_type(handler_atom->get_str());
      auto expected_type = get_state_handler_type(handler_kind, state_type);
      if (!env.dts->ts.tc(expected_type, sym_type)) {
        this_entry.val =
            pool.alloc_single_element_form<CastElement>(nullptr, expected_type, this_entry.val);
      }
    }
    // name = code/event/etc
    std::string name_to_check_for_skip = state_name;
    if (skip_states.count(name_to_check_for_skip) == 0) {
      name_to_check_for_skip =
          fmt::format("({} {})", state_name, state_type.last_arg().base_type());
    }
    if (skip_states.count(name_to_check_for_skip) > 0) {
      if (skip_states.at(name_to_check_for_skip).find(name) !=
          skip_states.at(name_to_check_for_skip).end()) {
        env.func->warnings.warning("SKIP: skipping '{}' handler for state '{}'", name,
                                   name_to_check_for_skip);
        continue;
      }
    }
    entries.push_back(this_entry);
  }
  return entries;
}

FormElement* rewrite_nonvirtual_defstate(
    LetElement* elt,
    const Env& env,
    const std::string& expected_state_name,
    FormPool& pool,
    const std::unordered_map<std::string, std::unordered_set<std::string>>& skip_states = {}) {
  // first thing in the body should be something like:
  //  (set! teetertotter-idle (the-as (state none) v1-3))
  ASSERT(elt->body()->size() > 0);
  int body_index = 0;

  // the setup
  auto first_in_body = elt->body()->at(body_index);
  auto info = get_state_info(first_in_body, env);
  if (info.first != expected_state_name) {
    env.func->warnings.error_and_throw(
        "Inconsistent defstate name. code has {}, static state has {}", info.first,
        expected_state_name);
  }
  if (debug_defstates) {
    fmt::print("State: {} Type: {}\n", info.first, info.second.print());
  }
  body_index++;

  auto entries =
      get_defstate_entries(elt->body(), body_index, env, info.first, elt->entries().at(0).dest,
                           info.second, pool, {}, skip_states);

  return pool.alloc_element<DefstateElement>(info.second.last_arg().base_type(), info.first,
                                             entries, false, false);
}

struct VirtualStateInfo {
  TypeSpec type_from_ts;
};

FormElement* strip_cast(FormElement* in) {
  auto casted = dynamic_cast<CastElement*>(in);
  while (casted) {
    in = casted->source()->try_as_single_element();
    casted = dynamic_cast<CastElement*>(in);
  }
  return in;
}

std::string verify_empty_state_and_get_name(DecompiledDataElement* state, const Env& env) {
  auto lab = state->label();
  // should have:
  /*
    .type state
  L25:
    .symbol teetertotter-launch
    .symbol #f
    .symbol #f
    .symbol #f
    .symbol #f
    .symbol #f
    .symbol #f
    .symbol #f
   */

  int start_word_idx = (lab.offset / 4) - 1;
  auto& words = env.file->words_by_seg.at(lab.target_segment);

  auto first_word = words.at(start_word_idx);
  if (first_word.kind() != LinkedWord::TYPE_PTR || first_word.symbol_name() != "state") {
    env.func->warnings.error_and_throw("Reference to state bad: invalid type pointer");
  }

  auto name_word = words.at(start_word_idx + 1);
  if (name_word.kind() != LinkedWord::SYM_PTR) {
    env.func->warnings.error_and_throw("Reference to state bad: invalid name");
  }

  for (int i = 0; i < 7; i++) {
    auto& word = words.at(start_word_idx + 2 + i);
    if (word.kind() != LinkedWord::SYM_PTR || word.symbol_name() != "#f") {
      env.func->warnings.error_and_throw(
          "Reference to state bad: got a non #f in the initial fields");
    }
  }

  return name_word.symbol_name();
}

FormElement* rewrite_virtual_defstate(
    LetElement* elt,
    const Env& env,
    const std::string& expected_state_name,
    FormPool& pool,
    const std::unordered_map<std::string, std::unordered_set<std::string>>& skip_states = {}) {
  ASSERT(elt->body()->size() > 1);
  // variable at the top of let, contains the static state with name exptected_state_name
  auto state_var_from_let_def = elt->entries().at(0).dest;
  // our index into the let body
  int body_idx = 0;

  // see if the first thing is an inherit-state.
  auto maybe_inherit_form = elt->body()->at(body_idx);
  //  fmt::print("first is {}\n", maybe_inherit_form->to_string(env));
  Form temp;
  temp.elts().push_back(maybe_inherit_form);
  // (inherit-state gp-1 (method-of-type plat-button dummy-24))
  auto inherit_matcher = Matcher::op(GenericOpMatcher::func(Matcher::symbol("inherit-state")),
                                     {Matcher::any_reg(0), Matcher::any(1)});

  struct InheritInfo {
    std::string parent_type_name;
    std::string method_name;
  };
  std::optional<InheritInfo> inherit_info;

  auto inherit_mr = match(inherit_matcher, &temp);
  if (!inherit_mr.matched) {
    // no inherit. This means that we should be the first in the type tree to define this state.
    inherit_info = {};
  } else {
    // found the inherit. advance body_idx so we move on to the next form.
    body_idx++;

    // expect this to match the variable in the top let
    auto state_var = *inherit_mr.maps.regs.at(0);
    // this expression should be the thing we inherit from.
    auto parent_state = inherit_mr.maps.forms.at(1);

    if (env.get_variable_name(state_var_from_let_def) != env.get_variable_name(state_var)) {
      env.func->warnings.error_and_throw(
          "Variable name disagreement in virtual defstate: began with {}, but did method "
          "set using {}",
          env.get_variable_name(state_var_from_let_def), env.get_variable_name(state_var));
    }

    // if there's a cast here, it probably means that there's no :state in the deftype.
    // let's warn here instead of trying to go on.
    auto parent_state_cast = parent_state->try_as_element<CastElement>();
    if (parent_state_cast) {
      env.func->warnings.error_and_throw(
          "virtual defstate attempted on something that isn't a state: {}\nDid you "
          "forget to put :state in the method definition?",
          parent_state_cast->to_string(env));
    }

    // identify the (method-of-type ...) form that grabs the parent state.
    auto mot_matcher = Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::METHOD_OF_TYPE),
                                   {Matcher::any_symbol(0), Matcher::any_constant_token(1)});
    auto mot_mr = match(mot_matcher, parent_state);
    if (!mot_mr.matched) {
      env.func->warnings.error_and_throw(
          "Failed to recognize virtual defstate. Got a {} as the parent to inherit from.",
          parent_state->to_string(env));
    }

    inherit_info = {{mot_mr.maps.strings.at(0), mot_mr.maps.strings.at(1)}};
  }

  // checks to check: method type is a state
  // if inherit matches expected.

  // next, find (method-set! sunken-elevator 22 (the-as function gp-0))
  auto method_set_form = elt->body()->at(body_idx);
  temp = Form();
  temp.elts().push_back(method_set_form);
  auto mset_matcher =
      Matcher::op(GenericOpMatcher::func(Matcher::symbol("method-set!")),
                  {Matcher::any_symbol(0), Matcher::any_integer(1), Matcher::any(2)});
  auto mset_mr = match(mset_matcher, &temp);
  if (!mset_mr.matched) {
    env.func->warnings.error_and_throw(
        "Failed to recognize virtual defstate. Got a {} as the second thing, but was "
        "expecting method-set! call",
        temp.to_string(env));
  }

  // the actual type that gets this as a state
  auto type_name = mset_mr.maps.strings.at(0);
  auto method_id = mset_mr.maps.ints.at(1);

  // should be the state again.
  auto val = strip_cast(mset_mr.maps.forms.at(2)->try_as_single_element());
  if (val->to_string(env) != env.get_variable_name(state_var_from_let_def)) {
    env.func->warnings.error_and_throw(
        "Variable name disagreement in virtual defstate: began with {}, but did method "
        "set using {}",
        val->to_string(env), env.get_variable_name(state_var_from_let_def));
  }

  // we should double check that the type in the defstate is correct
  auto method_info = env.dts->ts.lookup_method(type_name, method_id);
  if (method_info.type.base_type() != "state" ||
      method_info.type.last_arg().base_type() != "_type_") {
    env.func->warnings.error_and_throw(
        "Virtual defstate is defining a virtual state in method {} of {}, but the type "
        "of this method is {}, which is not a valid virtual state type (must be "
        "\"(state ... _type_)\")",
        method_info.name, type_name, method_info.type.print());
  }

  bool state_override = false;
  {
    MethodInfo parent_method_info;
    auto parent_type_name = env.dts->ts.lookup_type(type_name)->get_parent();
    if (env.dts->ts.try_lookup_method(parent_type_name, method_id, &parent_method_info)) {
      if (!inherit_info) {
        // did NOT inherit parent state, this is an override!
        state_override = true;
        // env.func->warnings.warn_and_throw(
        //     "Virtual defstate for state {} in type {}: the state was defined in the "
        //     "parent but wasn't inherited.",
        //     expected_state_name, type_name);
      }
    } else {
      if (inherit_info) {
        env.func->warnings.error_and_throw(
            "Virtual defstate for state {} in type {}: the state wasn't defined in the "
            "parent but was inherited.",
            expected_state_name, type_name);
      }
    }
  }

  // checks: parent_type_name is the parent
  if (inherit_info) {
    auto child_type_info = env.dts->ts.lookup_type(type_name);
    if (child_type_info->get_parent() != inherit_info->parent_type_name) {
      env.func->warnings.error_and_throw(
          "Parent type disagreement in virtual defstate. The state is inherited from {}, but the "
          "parent is {}",
          inherit_info->parent_type_name, child_type_info->get_parent());
    }

    auto parent_method_info = env.dts->ts.lookup_method(inherit_info->parent_type_name, method_id);
    if (parent_method_info.name != inherit_info->method_name) {
      env.func->warnings.error_and_throw(
          "Disagreement between inherit and define. We inherited from method {}, but redefine {}",
          inherit_info->method_name, parent_method_info.name);
    }
  }

  // name matches

  if (expected_state_name != method_info.name) {
    env.func->warnings.error_and_throw(
        "Disagreement between state name and type system name. The state is named {}, "
        "but the slot is named {}, defined in type {}",
        expected_state_name, method_info.name, method_info.defined_in_type);
  }

  // fmt::print("is a {}\n", typeid(*parent_state->try_as_single_element()).name());

  auto entries = get_defstate_entries(
      elt->body(), body_idx + 1, env, expected_state_name, elt->entries().at(0).dest,
      method_info.type.substitute_for_method_call(type_name), pool, type_name, skip_states);

  return pool.alloc_element<DefstateElement>(type_name, expected_state_name, entries, true,
                                             state_override);
}

bool is_nonvirtual_state(LetElement* elt) {
  return dynamic_cast<SetFormFormElement*>(elt->body()->at(0));
}

}  // namespace

void run_defstate(
    Function& top_level_func,
    const std::unordered_map<std::string, std::unordered_set<std::string>>& skip_states) {
  auto& env = top_level_func.ir2.env;
  auto& pool = *top_level_func.ir2.form_pool;
  if (!top_level_func.ir2.top_form) {
    return;
  }
  top_level_func.ir2.top_form->apply_form([&](Form* form) {
    for (auto& fe : form->elts()) {
      auto as_let = dynamic_cast<LetElement*>(fe);
      if (as_let && as_let->entries().size() == 1) {
        /* Looks something like this:
            (let ((v1-3 <static-data L28>))
              (set! teetertotter-idle (the-as (state none) v1-3))
              (set! (-> v1-3 event) L17)
              (set! (-> v1-3 code) L15)
              (set! (-> v1-3 post) transform-post)
              )
         */

        // first, see if we get a label:
        auto src_as_label = as_let->entries().at(0).src->try_as_element<DecompiledDataElement>();
        if (src_as_label &&
            env.get_variable_type(as_let->entries().at(0).dest, false) == TypeSpec("state")) {
          std::string expected_state_name = verify_empty_state_and_get_name(src_as_label, env);
          if (debug_defstates) {
            fmt::print("got state let:\n{}\n", pretty_print::to_string(as_let->to_form(env)));
          }

          if (is_nonvirtual_state(as_let)) {
            auto rewritten =
                rewrite_nonvirtual_defstate(as_let, env, expected_state_name, pool, skip_states);
            if (rewritten) {
              fe = rewritten;
            }
          } else {
            auto rewritten =
                rewrite_virtual_defstate(as_let, env, expected_state_name, pool, skip_states);
            if (rewritten) {
              fe = rewritten;
            }
          }
        }
      }
    }
  });
}
}  // namespace decompiler
