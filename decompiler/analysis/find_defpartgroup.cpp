#include "find_defpartgroup.h"

#include "common/goos/PrettyPrinter.h"
#include "common/util/BitUtils.h"

#include "decompiler/IR2/Form.h"
#include "decompiler/IR2/GenericElementMatcher.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "decompiler/util/data_decompile.h"

namespace decompiler {

namespace {

const goos::Object& car(const goos::Object* x) {
  return x->as_pair()->car;
}

const goos::Object* cdr(const goos::Object* x) {
  return &x->as_pair()->cdr;
}

void read_static_group_data(DecompiledDataElement* src,
                            const Env& env,
                            DefpartgroupElement::StaticInfo& group) {
  auto lab = src->label();
  // looks like:
  /*
    .type sparticle-launch-group
L81:
    .word 0xbb80042
    .word 0x405dc
    .word L83
    .word L82
    .word 0x0
    .word 0x0
    .word 0x0
    .word 0x0
    .word 0x0
    .word 0x0
    .word 0x47800000
L82:
   */

  int word_idx = (lab.offset / 4) - 1;
  auto& words = env.file->words_by_seg.at(lab.target_segment);

  auto& first_word = words.at(word_idx++);
  if (first_word.kind() != LinkedWord::TYPE_PTR ||
      first_word.symbol_name() != "sparticle-launch-group") {
    env.func->warnings.error_and_throw(
        "Reference to sparticle-launch-group bad: invalid type pointer");
  }

  auto& word_1 = words.at(word_idx++);
  s16 len = word_1.data & 0xffff;
  group.duration = (word_1.data >> 16) & 0xffff;
  auto& word_2 = words.at(word_idx++);
  group.linger = word_2.data & 0xffff;
  group.flags = (word_2.data >> 16) & 0xffff;

  auto& string_word = words.at(word_idx++);
  if (string_word.kind() != LinkedWord::PTR) {
    env.func->warnings.error_and_throw(
        "Reference to sparticle-launch-group bad: invalid name label");
  }
  group.name = env.file->get_goal_string_by_label(
      env.file->get_label_by_name(env.file->get_label_name(string_word.label_id())));

  auto& array_word = words.at(word_idx++);
  if (array_word.kind() != LinkedWord::PTR) {
    env.func->warnings.error_and_throw(
        "Reference to sparticle-launch-group bad: invalid array label");
  }
  auto& array_lab = env.file->get_label_by_name(env.file->get_label_name(array_word.label_id()));
  auto& array_words = env.file->words_by_seg.at(array_lab.target_segment);
  int array_start_word_idx = array_lab.offset / 4;
  group.elts.clear();
  for (int i = 0; i < len; ++i) {
    int item_idx = i * 8 + array_start_word_idx;
    auto& item = group.elts.emplace_back();
    item.part_id = array_words.at(item_idx + 0).data;
    item.fade = *reinterpret_cast<float*>(&array_words.at(item_idx + 1).data);
    item.falloff = *reinterpret_cast<float*>(&array_words.at(item_idx + 2).data);
    item.flags = array_words.at(item_idx + 3).data & 0xffff;
    item.period = (array_words.at(item_idx + 3).data >> 16) & 0xffff;
    item.length = array_words.at(item_idx + 4).data & 0xffff;
    item.offset = (array_words.at(item_idx + 4).data >> 16) & 0xffff;
    item.hour_mask = array_words.at(item_idx + 5).data;
    item.binding = array_words.at(item_idx + 6).data;
  }

  if (env.version != GameVersion::Jak1) {
    // added fields in jak 2
    for (int i = 0; i < 3; i++) {
      auto& word = words.at(word_idx++);
      if (word.kind() != LinkedWord::PLAIN_DATA) {
        env.func->warnings.error_and_throw("Reference to sparticle-launch-group bad: invalid rot");
      }
      group.rot[i] = *reinterpret_cast<float*>(&word.data);
    }
    for (int i = 0; i < 3; i++) {
      auto& word = words.at(word_idx++);
      if (word.kind() != LinkedWord::PLAIN_DATA) {
        env.func->warnings.error_and_throw(
            "Reference to sparticle-launch-group bad: invalid scale");
      }
      group.scale[i] = *reinterpret_cast<float*>(&word.data);
    }
  }

  word_idx = align4(word_idx);
  for (int i = 0; i < 4; i++) {
    auto& word = words.at(word_idx + i);
    if (word.kind() != LinkedWord::PLAIN_DATA) {
      env.func->warnings.error_and_throw("Reference to sparticle-launch-group bad: invalid bounds");
    }
    group.bounds[i] = *reinterpret_cast<float*>(&word.data);
  }
  word_idx += 4;
}

void read_static_part_data(DecompiledDataElement* src,
                           const Env& env,
                           DefpartElement::StaticInfo& part) {
  auto lab = src->label();
  // looks like:
  /*
    .type sparticle-launcher
L79:
    .word 0x0
    .word 0x0
    .word L80
L80:
    .word 0x1
    .word 0x201200
    .word 0x0
    .word 0x0
    .word 0x10006
    .word 0x3dcccccd
    .word 0x0
    .word 0x3f800000
   */

  int start_word_idx = (lab.offset / 4) - 1;
  auto& words = env.file->words_by_seg.at(lab.target_segment);

  auto& first_word = words.at(start_word_idx);
  if (first_word.kind() != LinkedWord::TYPE_PTR ||
      first_word.symbol_name() != "sparticle-launcher") {
    env.func->warnings.error_and_throw("Reference to sparticle-launcher bad: invalid type pointer");
  }

  auto& empty1 = words.at(start_word_idx + 1);
  auto& empty2 = words.at(start_word_idx + 2);
  if (empty1.kind() != LinkedWord::PLAIN_DATA || empty1.data != 0 ||
      empty2.kind() != LinkedWord::PLAIN_DATA || empty2.data != 0) {
    env.func->warnings.error_and_throw("Reference to sparticle-launcher bad: accums not empty");
  }

  auto& array_word = words.at(start_word_idx + 3);
  if (array_word.kind() != LinkedWord::PTR) {
    env.func->warnings.error_and_throw("Reference to sparticle-launcher bad: invalid array label");
  }
  auto& array_lab = env.file->get_label_by_name(env.file->get_label_name(array_word.label_id()));
  auto& array_words = env.file->words_by_seg.at(array_lab.target_segment);
  int array_start_word_idx = array_lab.offset / 4;
  part.fields.clear();
  src->do_decomp(env, env.file);
  auto obj = src->to_form(env);
  obj = car(cdr(cdr(&obj)));
  auto cur_field = cdr(&obj);
  for (int i = 0; true; ++i) {
    int field_idx = i * 4 + array_start_word_idx;
    auto& item = part.fields.emplace_back();
    item.field_id = array_words.at(field_idx + 0).data & 0xffff;
    item.flags = (array_words.at(field_idx + 0).data >> 16) & 0xffff;
    item.data.push_back(array_words.at(field_idx + 0));
    item.data.push_back(array_words.at(field_idx + 1));
    item.data.push_back(array_words.at(field_idx + 2));
    item.data.push_back(array_words.at(field_idx + 3));
    if (item.flags == 4) {
      auto& fld = car(cur_field);
      item.sound_spec = cdr(cdr(cdr(cdr(&fld))))->as_pair()->car;
    }
    item.userdata = car(cur_field);
    if (item.is_sp_end(env.version)) {
      // sp-end
      break;
    }
    cur_field = cdr(cur_field);
  }
}

}  // namespace

void run_defpartgroup(Function& top_level_func) {
  auto& env = top_level_func.ir2.env;
  auto& pool = *top_level_func.ir2.form_pool;
  if (!top_level_func.ir2.top_form) {
    return;
  }
  top_level_func.ir2.top_form->apply_form([&](Form* form) {
    for (auto& fe : form->elts()) {
      auto as_set = dynamic_cast<SetFormFormElement*>(fe);
      if (as_set) {
        /* Looks something like this:
            (set! (-> *part-group-id-table* 188) (new 'static 'sparticle-launch-group
         */
        if (as_set->dst()->elts().size() != 1) {
          continue;
        }
        auto dest = dynamic_cast<DerefElement*>(as_set->dst()->elts().at(0));
        if (!dest)
          continue;
        if (dest->tokens().size() != 1)
          continue;
        if (dest->tokens().at(0).kind() != DerefToken::Kind::INTEGER_CONSTANT)
          continue;
        if (dest->base()->elts().size() != 1)
          continue;
        auto dest_base = dynamic_cast<SimpleExpressionElement*>(dest->base()->elts().at(0));
        if (!dest_base || !dest_base->expr().is_identity() || dest_base->expr().args() < 1)
          continue;
        auto src = dynamic_cast<DecompiledDataElement*>(as_set->src()->elts().at(0));
        if (!src)
          continue;
        auto& sym = dest_base->expr().get_arg(0);
        if (!sym.is_sym_val())
          continue;

        int id = dest->tokens().at(0).int_constant();
        if (sym.get_str() == "*part-group-id-table*") {
          DefpartgroupElement::StaticInfo group;
          read_static_group_data(src, env, group);
          auto rewritten = pool.alloc_element<DefpartgroupElement>(group, id);
          if (rewritten) {
            fe = rewritten;
          }
        } else if (sym.get_str() == "*part-id-table*") {
          DefpartElement::StaticInfo part;
          read_static_part_data(src, env, part);
          auto rewritten = pool.alloc_element<DefpartElement>(part, id);
          if (rewritten) {
            fe = rewritten;
          }
        }
      }
    }
  });
}
}  // namespace decompiler
