

#include "find_defstates.h"
#include "decompiler/IR2/Form.h"
#include "common/goos/PrettyPrinter.h"
#include "decompiler/IR2/GenericElementMatcher.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "common/type_system/state.h"

namespace decompiler {

constexpr bool debug_defstates = false;

namespace {

/*!
 * Given the (set! <the-state> <reg-with-static-state>), returns the name of the state and the
 * more specific type.
 */
std::pair<std::string, TypeSpec> get_state_info(FormElement* state_set, const Env& env) {
  auto sff = dynamic_cast<SetFormFormElement*>(state_set);
  if (!sff) {
    throw std::runtime_error(
        fmt::format("Failed to identify defstate. The state symbol set was supposed to be: {}, but "
                    "this doesn't look like a set.",
                    state_set->to_string(env)));
  }

  auto atom = form_as_atom(sff->dst());
  if (!atom || atom->get_kind() != SimpleAtom::Kind::SYMBOL_VAL) {
    throw std::runtime_error(fmt::format(
        "Failed to identify defstate. The state symbol set was: {}, which doesn't set a symbol",
        state_set->to_string(env)));
  }

  std::string state_name = atom->get_str();

  auto type = env.dts->symbol_types.find(state_name);
  if (type == env.dts->symbol_types.end()) {
    throw std::runtime_error(fmt::format(
        "Identified a defstate for state {}, but there is no type information for this state.",
        state_name));
  }

  if (type->second.base_type() != "state") {
    throw std::runtime_error(
        fmt::format("Identified a defstate for state {}, but our type information thinks it is a "
                    "{}, not a state.",
                    type->second.print()));
  }

  if (type->second.arg_count() == 0) {
    throw std::runtime_error(fmt::format(
        "Identified a defstate for state {}, but there is no argument information.", state_name));
  }

  if (type->second.last_arg() == TypeSpec("none")) {
    throw std::runtime_error(
        fmt::format("Identified a defstate for state {}, but the process type is none. You must "
                    "provide a process type as the final argument of a state",
                    state_name));
  }

  return {atom->get_str(), type->second};
}

FormElement* rewrite_nonvirtual_defstate(LetElement* elt,
                                         const Env& env,
                                         const std::string& expected_state_name,
                                         FormPool& pool) {
  // first thing in the body should be something like:
  //  (set! teetertotter-idle (the-as (state none) v1-3))
  assert(elt->body()->size() > 0);
  int body_index = 0;

  // the setup
  auto first_in_body = elt->body()->at(body_index);
  auto info = get_state_info(first_in_body, env);
  if (info.first != expected_state_name) {
    throw std::runtime_error(
        fmt::format("Inconsistent defstate name. code has {}, static state has {}", info.first,
                    expected_state_name));
  }
  if (debug_defstates) {
    fmt::print("State: {} Type: {}\n", info.first, info.second.print());
  }
  body_index++;

  std::vector<NonVirtualDefstateElement::Entry> entries;

  // next, all the handlers
  for (; body_index < elt->body()->size(); body_index++) {
    NonVirtualDefstateElement::Entry this_entry;
    auto matcher =
        Matcher::set(Matcher::deref(Matcher::any_reg(0), false, {DerefTokenMatcher::any_string(1)}),
                     Matcher::any(2));
    Form temp;
    temp.elts().push_back(elt->body()->at(body_index));
    auto mr = match(matcher, &temp);

    if (!mr.matched) {
      throw std::runtime_error(
          fmt::format("In defstate for state {}, failed to recognize handler set: {}", info.first,
                      temp.to_string(env)));
    }

    auto var = mr.maps.regs.at(0);
    auto name = mr.maps.strings.at(1);
    auto val = mr.maps.forms.at(2);

    auto handler_kind = handler_name_to_kind(name);
    this_entry.val = val;
    this_entry.kind = handler_kind;
    this_entry.is_behavior = false;

    if (!var || env.get_variable_name(*var) != env.get_variable_name(elt->entries().at(0).dest)) {
      if (var) {
        throw std::runtime_error(fmt::format("Messed up defstate. State is in {}, but we set {}",
                                             env.get_variable_name(elt->entries().at(0).dest),
                                             env.get_variable_name(*var)));
      } else {
        assert(false);
      }
    }

    if (debug_defstates) {
      fmt::print("SET: {} to {}\n", name, val->to_string(env));
    }

    // now we try to find a function
    while (val->try_as_element<CastElement>()) {
      val = val->try_as_element<CastElement>()->source();
    }
    auto handler_atom = form_as_atom(val);
    if (handler_atom && handler_atom->is_label()) {
      auto handler_func = env.file->try_get_function_at_label(handler_atom->label());
      if (!handler_func) {
        throw std::runtime_error("Failed to find handler function.");
      }

      this_entry.is_behavior = true;
      handler_func->guessed_name.set_as_nv_state(info.first, handler_kind);
      // scary part - modify the function type!
      handler_func->type = get_state_handler_type(handler_kind, info.second);
    }
    entries.push_back(this_entry);
  }

  return pool.alloc_element<NonVirtualDefstateElement>(info.second.last_arg().base_type(),
                                                       info.first, entries);
}

bool is_nonvirtual_state(LetElement* elt) {
  return dynamic_cast<SetFormFormElement*>(elt->body()->at(0));
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
  if (first_word.kind != LinkedWord::TYPE_PTR || first_word.symbol_name != "state") {
    throw std::runtime_error("Reference to state bad: invalid type pointer");
  }

  auto name_word = words.at(start_word_idx + 1);
  if (name_word.kind != LinkedWord::SYM_PTR) {
    throw std::runtime_error("Reference to state bad: invalid name");
  }

  for (int i = 0; i < 7; i++) {
    auto& word = words.at(start_word_idx + 2 + i);
    if (word.kind != LinkedWord::SYM_PTR || word.symbol_name != "#f") {
      throw std::runtime_error("Reference to state bad: got a non #f in the initial fields");
    }
  }

  return name_word.symbol_name;
}
}  // namespace

void run_defstate(DecompilerTypeSystem& dts, Function& top_level_func) {
  auto& env = top_level_func.ir2.env;
  auto& pool = *top_level_func.ir2.form_pool;
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
            auto rewritten = rewrite_nonvirtual_defstate(as_let, env, expected_state_name, pool);
            if (rewritten) {
              fe = rewritten;
            }
          } else {
            // TODO virtual state
          }
        }
      }
    }
  });
}
}  // namespace decompiler