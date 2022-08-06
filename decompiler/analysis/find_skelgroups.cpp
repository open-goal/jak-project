

#include "find_skelgroups.h"
#include "common/goos/PrettyPrinter.h"
#include "common/math/Vector.h"
#include "decompiler/IR2/Form.h"
#include "decompiler/IR2/GenericElementMatcher.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"

namespace decompiler {

namespace {

std::string get_skelgroup_name(FormElement* skel_set, const Env& env) {
  auto sff = dynamic_cast<SetFormFormElement*>(skel_set);
  if (!sff || !skel_set) {
    env.func->warnings.error_and_throw("Failed to identify defskelgroup.");
  }

  auto atom = form_as_atom(sff->dst());
  if (!atom || atom->get_kind() != SimpleAtom::Kind::SYMBOL_VAL) {
    env.func->warnings.error_and_throw(
        "Failed to identify defskelgroup. The skeleton-group symbol set was: {}, which doesn't set "
        "a symbol",
        skel_set->to_string(env));
  }

  return atom->get_str();
}

static const std::vector<int> empty_words = {1, 2, 7, 8, 10, 11, 12, 13, 16};
DefskelgroupElement::StaticInfo inspect_skel_group_data(DecompiledDataElement* skel,
                                                        const Env& env) {
  DefskelgroupElement::StaticInfo result;

  auto lab = skel->label();
  // should have:
  /*
    .type skeleton-group
  L52:
    .word L53 // name (string)
    .word 0x0 // jgeo
    .word 0x0 // janim
    .word 0x0 // bounds x
    .word 0x0 // bounds y
    .word 0x0 // bounds z
    .word 0x46400000 // bounds w/radius
    .word 0x0 // mgeo 0/1
    .word 0x0 // mgeo 2/3
    .word 0x2 // max-lod
    .word 0x0 // lod dist 0
    .word 0x0 // lod dist 1
    .word 0x0 // lod dist 2
    .word 0x0 // lod dist 3
    .word 0x45800000 // longest-edge
    .word 0x40600 // texture-level/version/shadow/sort
    .word 0x0 // pad
   */

  int start_word_idx = lab.offset / 4;
  auto& words = env.file->words_by_seg.at(lab.target_segment);

  auto& type_word = words.at(start_word_idx - 1);
  if (type_word.kind() != LinkedWord::TYPE_PTR || type_word.symbol_name() != "skeleton-group") {
    env.func->warnings.error_and_throw("Reference to skelgroup bad: invalid type pointer");
  }
  auto& string_word = words.at(start_word_idx);
  if (string_word.kind() != LinkedWord::PTR) {
    env.func->warnings.error_and_throw("Reference to skelgroup bad: invalid name label");
  }
  result.art_name = env.file->get_goal_string_by_label(
      env.file->get_label_by_name(env.file->get_label_name(string_word.label_id())));
  for (int i = 0; i < 4; i++) {
    auto& word = words.at(start_word_idx + 3 + i);
    if (word.kind() != LinkedWord::PLAIN_DATA) {
      env.func->warnings.error_and_throw("Reference to skelgroup bad: invalid bounds");
    }
    result.bounds[i] = *reinterpret_cast<float*>(&word.data);
  }
  auto& lod_word = words.at(start_word_idx + 9);
  if (lod_word.kind() != LinkedWord::PLAIN_DATA) {
    env.func->warnings.error_and_throw("Reference to skelgroup bad: invalid max-lod");
  }
  result.max_lod = lod_word.data;
  auto& edge_word = words.at(start_word_idx + 14);
  if (edge_word.kind() != LinkedWord::PLAIN_DATA) {
    env.func->warnings.error_and_throw("Reference to skelgroup bad: invalid longest-edge");
  }
  result.longest_edge = *reinterpret_cast<float*>(&edge_word.data);
  auto& other_word = words.at(start_word_idx + 15);
  if (other_word.kind() != LinkedWord::PLAIN_DATA) {
    env.func->warnings.error_and_throw("Reference to skelgroup bad: invalid other data");
  }
  result.tex_level = other_word.get_byte(0);
  result.version = other_word.get_byte(1);
  result.shadow = other_word.get_byte(2);
  result.sort = other_word.get_byte(3);

  for (auto i : empty_words) {
    auto& word = words.at(start_word_idx + i);
    if (word.data != LinkedWord::PLAIN_DATA || word.data != 0) {
      env.func->warnings.error_and_throw(fmt::format("Reference to skelgroup bad: set word {}", i));
    }
  }

  return result;
}

DefskelgroupElement::Info get_defskelgroup_entries(Form* body,
                                                   const Env& env,
                                                   const RegisterAccess& let_dest_var) {
  DefskelgroupElement::Info out_info;

  // next, all the handlers
  for (int i = 0; i < body->size() - 1; ++i) {
    auto matcher = i < 2 ? Matcher::set(Matcher::deref(Matcher::any_reg(0), false,
                                                       {DerefTokenMatcher::any_string(1)}),
                                        Matcher::any(2))
                         : Matcher::set(Matcher::deref(Matcher::any_reg(0), false,
                                                       {DerefTokenMatcher::any_string(1),
                                                        DerefTokenMatcher::integer(i / 2 - 1)}),
                                        Matcher::any(3));
    Form temp;
    temp.elts().push_back(body->at(i));
    auto mr = match(matcher, &temp);

    if (!mr.matched) {
      env.func->warnings.error_and_throw("defskelgroup set no match");
    }

    auto& var = mr.maps.regs.at(0);
    auto& name = mr.maps.strings.at(1);
    auto val = i < 2 ? mr.maps.forms.at(2) : mr.maps.forms.at(3);

    while (val->try_as_element<CastElement>()) {
      val = val->try_as_element<CastElement>()->source();
    }

    if (!var || env.get_variable_name(*var) != env.get_variable_name(let_dest_var)) {
      if (var) {
        env.func->warnings.error_and_throw("Messed up defskelgroup. It is in {}, but we set {}",
                                           env.get_variable_name(let_dest_var),
                                           env.get_variable_name(*var));
      } else {
        ASSERT(false);
      }
    }

    if (name == "jgeo") {
      out_info.jgeo = val;
    } else if (name == "janim") {
      out_info.janim = val;
    } else if (name == "mgeo") {
      auto& this_entry = out_info.lods.emplace_back();
      this_entry.mgeo = val;
    } else if (name == "lod-dist") {
      auto& this_entry = out_info.lods.back();
      this_entry.lod_dist = val;
    }
  }
  return out_info;
}

FormElement* rewrite_defskelgroup(LetElement* elt,
                                  const Env& env,
                                  DefskelgroupElement::StaticInfo& skelgroup_info,
                                  FormPool& pool) {
  // last thing in the body should be something like:
  //  (set! *hopper-sg* v1-1)
  ASSERT(elt->body()->size() > 0);

  int last_lod = (elt->body()->size() - 3) / 2 - 1;
  if (last_lod > skelgroup_info.max_lod) {
    env.func->warnings.error_and_throw("defskelgroup exceeds max-lod of {} ({})",
                                       skelgroup_info.max_lod, last_lod);
  }

  auto rest_info = get_defskelgroup_entries(elt->body(), env, elt->entries().at(0).dest);

  return pool.alloc_element<DefskelgroupElement>(get_skelgroup_name(elt->body()->back(), env),
                                                 rest_info, skelgroup_info);
}
}  // namespace

void run_defskelgroups(Function& top_level_func) {
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
            (let ((v1-1 <static-data L57>))
              (set! (-> v1-1 jgeo) 0)
              (set! (-> v1-1 janim) 5)
              (set! (-> v1-1 mgeo 0) 1)
              (set! (-> v1-1 lod-dist 0) 81920.0)
              (set! (-> v1-1 mgeo 1) 2)
              (set! (-> v1-1 lod-dist 1) 163840.0)
              (set! (-> v1-1 mgeo 2) 3)
              (set! (-> v1-1 lod-dist 2) 4095996000.0)
              (set! *hopper-sg* v1-1)
              )
         */

        // first, see if we get a label:
        auto src_as_label = as_let->entries().at(0).src->try_as_element<DecompiledDataElement>();
        if (src_as_label && env.get_variable_type(as_let->entries().at(0).dest, false) ==
                                TypeSpec("skeleton-group")) {
          auto sg = inspect_skel_group_data(src_as_label, env);
          auto rewritten = rewrite_defskelgroup(as_let, env, sg, pool);
          if (rewritten) {
            fe = rewritten;
          }
        }
      }
    }
  });
}
}  // namespace decompiler
