

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

static const std::vector<int> empty_words_jak1 = {1, 2, 7, 8, 10, 11, 12, 13, 16};
DefskelgroupElement::StaticInfo inspect_skel_group_data_jak1(DecompiledDataElement* skel,
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
    env.func->warnings.error_and_throw("Reference to skelgroup bad: invalid art-group-name label");
  }
  result.art_group_name = env.file->get_goal_string_by_label(
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

  for (auto i : empty_words_jak1) {
    auto& word = words.at(start_word_idx + i);
    if (word.data != LinkedWord::PLAIN_DATA || word.data != 0) {
      env.func->warnings.error_and_throw(fmt::format("Reference to skelgroup bad: set word {}", i));
    }
  }

  return result;
}

static const std::vector<int> empty_words_jak2 = {2,  4,  5,  6,  8,  9,  10, 15,
                                                  16, 17, 19, 20, 21, 22, 23, 24};
DefskelgroupElement::StaticInfo inspect_skel_group_data_jak2(DecompiledDataElement* skel,
                                                             const Env& env) {
  DefskelgroupElement::StaticInfo result;

  auto lab = skel->label();
  // example from "crates"
  /*
    .type skeleton-group
    L30:
    .symbol #f        // info                                                  // 4
    .word L263        // name (string)                                         // 8
    .word 0x0         // length                                                // 12
    .symbol #f        // extra                                                 // 16
    .word 0x0         // ? (word 4)                                            // 20
    .word 0x0         // ? (word 5)                                            // 24
    .word 0x0         // ? (word 6)                                            // 28
    .word L262        // art-group-name (string)                               // 32
    .word 0x0         // jgeo                                                  // 36
    .word 0x0         // janim                                                 // 40
    .word 0x0         // ? (word 10)                                           // 44
    .word 0x0         // bounds x                                              // 48
    .word 0x45800000  // bounds y                                              // 52
    .word 0x0         // bounds z                                              // 56
    .word 0x45cccccd  // bounds w/radius                                       // 60
    .word 0x0         // mgeo 0/1                                              // 64
    .word 0x0         // mgeo 2/3                                              // 68
    .word 0x0         // mgeo 4/5                                              // 72
    .word 0x1         // max-lod                                               // 76
    .word 0x0         // lod-dist 0                                            // 80
    .word 0x0         // lod-dist 1                                            // 84
    .word 0x0         // lod-dist 2                                            // 88
    .word 0x0         // lod-dist 3                                            // 92
    .word 0x0         // lod-dist 4                                            // 96
    .word 0x0         // lod-dist 5                                            // 100
    .word 0x0         // longest-edge                                          // 104
    .word 0x706       // texture-level/version/shadow/sort                     // 108
    .word 0x0         // origin-joint-index/shadow-joint-index/light-index/pad // 112
   */

  int start_word_idx = lab.offset / 4;
  auto& words = env.file->words_by_seg.at(lab.target_segment);

  auto& type_word = words.at(start_word_idx - 1);
  if (type_word.kind() != LinkedWord::TYPE_PTR || type_word.symbol_name() != "skeleton-group") {
    env.func->warnings.error_and_throw("Reference to skelgroup bad: invalid type pointer");
  }
  auto& name_word = words.at(start_word_idx + 1);
  if (name_word.kind() != LinkedWord::PTR) {
    env.func->warnings.error_and_throw("Reference to skelgroup bad: invalid name label");
  }
  result.name = env.file->get_goal_string_by_label(
      env.file->get_label_by_name(env.file->get_label_name(name_word.label_id())));
  auto& art_name_word = words.at(start_word_idx + 7);
  if (art_name_word.kind() != LinkedWord::PTR) {
    env.func->warnings.error_and_throw("Reference to skelgroup bad: invalid art-group-name label");
  }
  result.art_group_name = env.file->get_goal_string_by_label(
      env.file->get_label_by_name(env.file->get_label_name(art_name_word.label_id())));
  for (int i = 0; i < 4; i++) {
    auto& word = words.at(start_word_idx + 11 + i);
    if (word.kind() != LinkedWord::PLAIN_DATA) {
      env.func->warnings.error_and_throw("Reference to skelgroup bad: invalid bounds");
    }
    result.bounds[i] = *reinterpret_cast<float*>(&word.data);
  }
  auto& lod_word = words.at(start_word_idx + 18);
  if (lod_word.kind() != LinkedWord::PLAIN_DATA) {
    env.func->warnings.error_and_throw("Reference to skelgroup bad: invalid max-lod");
  }
  result.max_lod = lod_word.data;
  auto& edge_word = words.at(start_word_idx + 25);
  if (edge_word.kind() != LinkedWord::PLAIN_DATA) {
    env.func->warnings.error_and_throw("Reference to skelgroup bad: invalid longest-edge");
  }
  result.longest_edge = *reinterpret_cast<float*>(&edge_word.data);
  auto& other_word = words.at(start_word_idx + 26);
  if (other_word.kind() != LinkedWord::PLAIN_DATA) {
    env.func->warnings.error_and_throw("Reference to skelgroup bad: invalid other data");
  }
  result.tex_level = other_word.get_byte(0);
  result.version = other_word.get_byte(1);
  result.shadow = other_word.get_byte(2);
  result.sort = other_word.get_byte(3);
  auto& index_word = words.at(start_word_idx + 27);
  if (index_word.kind() != LinkedWord::PLAIN_DATA) {
    env.func->warnings.error_and_throw("Reference to skelgroup bad: invalid index data");
  }
  result.origin_joint_index = index_word.get_byte(0);
  result.shadow_joint_index = index_word.get_byte(1);
  result.light_index = index_word.get_byte(2);

  for (auto i : empty_words_jak2) {
    auto& word = words.at(start_word_idx + i);
    if (word.data != LinkedWord::PLAIN_DATA || word.data != 0) {
      env.func->warnings.error_and_throw(fmt::format("Reference to skelgroup bad: set word {}", i));
    }
  }

  return result;
}

static const std::vector<int> empty_words_jak3 = {2,  4,  5,  6,  8,  9,  10, 15, 16,
                                                  17, 19, 20, 21, 22, 23, 24, 30};
DefskelgroupElement::StaticInfo inspect_skel_group_data_jak3(DecompiledDataElement* skel,
                                                             const Env& env) {
  DefskelgroupElement::StaticInfo result;

  auto lab = skel->label();
  // example from target-util
  /*
    .type skeleton-group
L460:
    .symbol #f       // info                                                  // 4
    .word L462       // name (string)                                         // 8
    .word 0x0        // length                                                // 12
    .symbol #f       // extra                                                 // 16
    .word 0x0        // ? (word 4)                                            // 20
    .word 0x0        // ? (word 5)                                            // 24
    .word 0x0        // ? (word 6)                                            // 28
    .word L461       // art-group-name (string)                               // 32
    .word 0x0        // jgeo                                                  // 36
    .word 0x0        // janim                                                 // 40
    .word 0x0        // ? (word 10)                                           // 44
    .word 0x0        // bounds x                                              // 48   11
    .word 0x0        // bounds y                                              // 52   12
    .word 0x0        // bounds z                                              // 56   13
    .word 0x464ccccd // bounds w/radius                                       // 60   14
    .word 0x0        // mgeo 0/1                                              // 64   15
    .word 0x0        // mgeo 2/3                                              // 68   16
    .word 0x0        // mgeo 4/5                                              // 72   17
    .word 0x0        // max-lod                                               // 76   18
    .word 0x0        // lod-dist 0                                            // 80   19
    .word 0x0        // lod-dist 1                                            // 84   20
    .word 0x0        // lod-dist 2                                            // 88   21
    .word 0x0        // lod-dist 3                                            // 92   22
    .word 0x0        // lod-dist 4                                            // 96   23
    .word 0x0        // lod-dist 5                                            // 100  24
    .word 0x45800000 // longest-edge                                          // 104  25
    (texture-level      int8                 :offset-assert 108) ;; word 26
    (version            int8                 :offset-assert 109)
    (shadow             int16                :offset-assert 110)
    (shadow-joint-index int8                 :offset-assert 112) ;; word 27
    (origin-joint-index int8                 :offset-assert 113)
    (sort               int8                 :offset-assert 114)
    (light-index        uint8                :offset-assert 115)
    (clothing           (array cloth-params) :offset-assert 116) ;; word 28
    (global-effects     uint8                :offset-assert 120) ;; word 29
    .word 0x0        // pad
   */

  int start_word_idx = lab.offset / 4;
  auto& words = env.file->words_by_seg.at(lab.target_segment);

  auto& type_word = words.at(start_word_idx - 1);
  if (type_word.kind() != LinkedWord::TYPE_PTR || type_word.symbol_name() != "skeleton-group") {
    env.func->warnings.error_and_throw("Reference to skelgroup bad: invalid type pointer");
  }
  auto& name_word = words.at(start_word_idx + 1);
  if (name_word.kind() != LinkedWord::PTR) {
    env.func->warnings.error_and_throw("Reference to skelgroup bad: invalid name label");
  }
  result.name = env.file->get_goal_string_by_label(
      env.file->get_label_by_name(env.file->get_label_name(name_word.label_id())));
  auto& art_name_word = words.at(start_word_idx + 7);
  if (art_name_word.kind() != LinkedWord::PTR) {
    env.func->warnings.error_and_throw("Reference to skelgroup bad: invalid art-group-name label");
  }
  result.art_group_name = env.file->get_goal_string_by_label(
      env.file->get_label_by_name(env.file->get_label_name(art_name_word.label_id())));
  for (int i = 0; i < 4; i++) {
    auto& word = words.at(start_word_idx + 11 + i);
    if (word.kind() != LinkedWord::PLAIN_DATA) {
      env.func->warnings.error_and_throw("Reference to skelgroup bad: invalid bounds");
    }
    result.bounds[i] = *reinterpret_cast<float*>(&word.data);
  }
  auto& lod_word = words.at(start_word_idx + 18);
  if (lod_word.kind() != LinkedWord::PLAIN_DATA) {
    env.func->warnings.error_and_throw("Reference to skelgroup bad: invalid max-lod");
  }
  result.max_lod = lod_word.data;
  auto& edge_word = words.at(start_word_idx + 25);
  if (edge_word.kind() != LinkedWord::PLAIN_DATA) {
    env.func->warnings.error_and_throw("Reference to skelgroup bad: invalid longest-edge");
  }
  result.longest_edge = *reinterpret_cast<float*>(&edge_word.data);
  auto& other_word = words.at(start_word_idx + 26);
  if (other_word.kind() != LinkedWord::PLAIN_DATA) {
    env.func->warnings.error_and_throw("Reference to skelgroup bad: invalid other data");
  }
  result.tex_level = other_word.get_byte(0);
  result.version = other_word.get_byte(1);
  result.shadow = ((u32)other_word.get_byte(2)) | (((u32)other_word.get_byte(3)) << 8);

  auto& index_word = words.at(start_word_idx + 27);
  if (index_word.kind() != LinkedWord::PLAIN_DATA) {
    env.func->warnings.error_and_throw("Reference to skelgroup bad: invalid index data");
  }
  result.shadow_joint_index = index_word.get_byte(0);
  result.origin_joint_index = index_word.get_byte(1);
  result.sort = index_word.get_byte(2);
  result.light_index = index_word.get_byte(3);

  auto& clothing = words.at(start_word_idx + 28);
  if (clothing.kind() != LinkedWord::SYM_PTR && clothing.symbol_name() != "#f") {
    env.func->warnings.error_and_throw(
        "Reference to skelgroup bad: invalid clothing (should be #f)");
  }

  result.global_effects = words.at(start_word_idx + 29).get_byte(0);

  for (auto i : empty_words_jak3) {
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

DefskelgroupElement::Info get_defskelgroup_entries_jak3(Form* body,
                                                        const Env& env,
                                                        const RegisterAccess& let_dest_var) {
  DefskelgroupElement::Info out_info;

  // next, all the handlers
  for (int i = 1; i < body->size() - 1; ++i) {
    auto matcher =
        i < 3 ? Matcher::set(
                    Matcher::deref(Matcher::any_reg(0), false, {DerefTokenMatcher::any_string(1)}),
                    Matcher::any(2))
              : Matcher::set(Matcher::deref(Matcher::any_reg(0), false,
                                            {DerefTokenMatcher::any_string(1),
                                             DerefTokenMatcher::integer((i - 1) / 2 - 1)}),
                             Matcher::any(3));
    Form temp;
    temp.elts().push_back(body->at(i));
    auto mr = match(matcher, &temp);

    if (!mr.matched) {
      env.func->warnings.error_and_throw("defskelgroup set no match");
    }

    auto& var = mr.maps.regs.at(0);
    auto& name = mr.maps.strings.at(1);
    auto val = i < 3 ? mr.maps.forms.at(2) : mr.maps.forms.at(3);

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

  int last_lod = env.version == GameVersion::Jak3 ? (elt->body()->size() - 4) / 2 - 1
                                                  : (elt->body()->size() - 3) / 2 - 1;
  if (last_lod > skelgroup_info.max_lod) {
    env.func->warnings.error_and_throw("defskelgroup exceeds max-lod of {} ({})",
                                       skelgroup_info.max_lod, last_lod);
  }

  auto rest_info = env.version == GameVersion::Jak3
                       ? get_defskelgroup_entries_jak3(elt->body(), env, elt->entries().at(0).dest)
                       : get_defskelgroup_entries(elt->body(), env, elt->entries().at(0).dest);

  if (env.version != GameVersion::Jak1) {
    return pool.alloc_element<DefskelgroupElement>(skelgroup_info.name, rest_info, skelgroup_info);
  } else {
    return pool.alloc_element<DefskelgroupElement>(get_skelgroup_name(elt->body()->back(), env),
                                                   rest_info, skelgroup_info);
  }
}
}  // namespace

DefskelgroupElement::ClothParams get_cloth_params(const DecompilerLabel& label, Env& env) {
  /*
L446:
    .word 0xb
    .word 0x48000000
    .word 0x3f400000
    .word 0x6
    .word 0x0
    .word 0x0
    .word 0xa
    .word 0x0
    .word L449
    .word L449
    .word L449
    .word L448
    .word L448
    .word L448
    .word 0x3f800000
    .word 0x3
    .word 0x3e808312
    .word 0x434ccccd
    .word 0x1
    .word 0x0
    .word 0x0
    .word 0x0
    .word 0x0
    .word 0x0
   */
  DefskelgroupElement::ClothParams params;
  int start_word_idx = label.offset / 4;
  auto& words = env.file->words_by_seg.at(label.target_segment);
  params.mesh = words.at(start_word_idx).data;
  params.gravity = *reinterpret_cast<float*>(&words.at(start_word_idx + 1).data);
  params.wind = *reinterpret_cast<float*>(&words.at(start_word_idx + 2).data);
  auto word3 = words.at(start_word_idx + 3).data;
  params.width = word3 & 0xffff;
  params.sphere_constraints = (word3 >> 16) & 0xffff;
  auto word4 = words.at(start_word_idx + 4).data;
  auto disc = word4 & 0xffff;
  auto anchor = (word4 >> 16) & 0xffff;
  params.disc_constraints = disc;
  params.anchor_points = anchor;
  auto flags_hi = words.at(start_word_idx + 5).data;
  auto flags_lo = words.at(start_word_idx + 6).data;
  params.flags = static_cast<u64>(flags_hi) << 32 | flags_lo;
  // these can be #f
  auto& tex_name_word = words.at(start_word_idx + 8);
  if (tex_name_word.kind() != LinkedWord::SYM_PTR) {
    params.tex_name = env.file->get_goal_string_by_label(
        env.file->get_label_by_name(env.file->get_label_name(tex_name_word.label_id())));
  }
  auto& tex_name2_word = words.at(start_word_idx + 9);
  if (tex_name2_word.kind() != LinkedWord::SYM_PTR) {
    params.tex_name2 = env.file->get_goal_string_by_label(
        env.file->get_label_by_name(env.file->get_label_name(tex_name2_word.label_id())));
  }
  auto& tex_name3_word = words.at(start_word_idx + 10);
  if (tex_name3_word.kind() != LinkedWord::SYM_PTR) {
    params.tex_name3 = env.file->get_goal_string_by_label(
        env.file->get_label_by_name(env.file->get_label_name(tex_name3_word.label_id())));
  }
  auto& alt_tex_name_word = words.at(start_word_idx + 11);
  if (alt_tex_name_word.kind() != LinkedWord::SYM_PTR) {
    params.alt_tex_name = env.file->get_goal_string_by_label(
        env.file->get_label_by_name(env.file->get_label_name(alt_tex_name_word.label_id())));
  }
  auto& alt_tex_name2_word = words.at(start_word_idx + 12);
  if (alt_tex_name2_word.kind() != LinkedWord::SYM_PTR) {
    params.alt_tex_name2 = env.file->get_goal_string_by_label(
        env.file->get_label_by_name(env.file->get_label_name(alt_tex_name2_word.label_id())));
  }
  auto& alt_tex_name3_word = words.at(start_word_idx + 13);
  if (alt_tex_name3_word.kind() != LinkedWord::SYM_PTR) {
    params.alt_tex_name3 = env.file->get_goal_string_by_label(
        env.file->get_label_by_name(env.file->get_label_name(alt_tex_name3_word.label_id())));
  }
  params.thickness = *reinterpret_cast<float*>(&words.at(start_word_idx + 14).data);
  params.xform = words.at(start_word_idx + 15).data;
  params.drag = *reinterpret_cast<float*>(&words.at(start_word_idx + 16).data);
  params.ball_collision_radius = *reinterpret_cast<float*>(&words.at(start_word_idx + 17).data);
  params.iterations = words.at(start_word_idx + 18).get_byte(0);
  params.timestep_freq = words.at(start_word_idx + 18).get_byte(1);
  auto secret_hi = words.at(start_word_idx + 21).data;
  auto secret_lo = words.at(start_word_idx + 20).data;
  params.secret = static_cast<u64>(secret_hi) << 32 | secret_lo;
  return params;
}

void inspect_cloth_data_jak3(LetElement* let, DefskelgroupElement::StaticInfo& info, Env& env) {
  auto body = let->body();
  auto when = dynamic_cast<CondNoElseElement*>(body->at(0));
  if (when) {
    auto cond = when->entries.at(0).condition;
    if (cond) {
      auto matcher = Matcher::op(GenericOpMatcher::condition(IR2_Condition::Kind::TRUTHY),
                                 {Matcher::symbol("#f")});
      auto mr = match(matcher, cond);
      if (mr.matched) {
        // no cloth data
        return;
      } else {
        // get the cloth array
        auto when_body = when->entries.at(0).body;
        auto array_set = dynamic_cast<SetFormFormElement*>(when_body->at(0));
        if (array_set) {
          // get elements
          auto elts = (int)when_body->elts().size() - 2;
          for (int i = 0; i < elts; i++) {
            auto parms_form = dynamic_cast<SetFormFormElement*>(when_body->at(i + 1));
            if (parms_form) {
              auto parms_data = dynamic_cast<DecompiledDataElement*>(parms_form->src()->at(0));
              if (parms_data) {
                info.clothing.push_back(get_cloth_params(parms_data->label(), env));
              }
            }
          }
        }
        return;
      }
    }
  }
}

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
          DefskelgroupElement::StaticInfo sg;
          switch (env.version) {
            case GameVersion::Jak1:
              sg = inspect_skel_group_data_jak1(src_as_label, env);
              break;
            case GameVersion::Jak2:
              sg = inspect_skel_group_data_jak2(src_as_label, env);
              break;
            case GameVersion::Jak3:
              sg = inspect_skel_group_data_jak3(src_as_label, env);
              inspect_cloth_data_jak3(as_let, sg, env);
              break;
          }
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
