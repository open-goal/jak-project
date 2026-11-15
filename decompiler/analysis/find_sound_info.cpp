#include "find_sound_info.h"

#include <cstring>
#include <stdexcept>
#include <unordered_set>

#include "decompiler/IR2/Form.h"
#include "decompiler/IR2/GenericElementMatcher.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "decompiler/util/DecompilerTypeSystem.h"

#include "fmt/format.h"

namespace decompiler {

namespace {

constexpr int SOUND_NAME_SIZE = 16;

std::string read_sound_name(const LinkedObjectFile& file,
                            const DecompilerLabel& sound_info,
                            int name_offset) {
  auto& words = file.words_by_seg.at(sound_info.target_segment);
  int word_idx = (sound_info.offset + name_offset) / 4;
  char name[SOUND_NAME_SIZE + 1] = {};
  for (int i = 0; i < SOUND_NAME_SIZE / 4; i++) {
    auto& word = words.at(word_idx + i);
    if (word.kind() != LinkedWord::PLAIN_DATA) {
      throw std::runtime_error("Reference to sound-info bad: invalid name");
    }
    memcpy(name + i * 4, &word.data, 4);
  }
  return name;
}

std::vector<std::string> read_static_sound_info_array(const DecompilerLabel& lab, const Env& env) {
  // looks like:
  /*
    .type array
L2:
    .word 0x288
    .word 0x288
    .type sound-info
    .word L650
    .word L649
   */
  if ((lab.offset % 8) != 4) {
    throw std::runtime_error("Reference to *sound-info* bad: invalid alignment");
  }

  auto& words = env.file->words_by_seg.at(lab.target_segment);
  int word_idx = (lab.offset / 4) - 1;

  auto& type_word = words.at(word_idx);
  auto& content_type_word = words.at(word_idx + 3);
  if (type_word.kind() != LinkedWord::TYPE_PTR || type_word.symbol_name() != "array" ||
      content_type_word.kind() != LinkedWord::TYPE_PTR ||
      content_type_word.symbol_name() != "sound-info") {
    throw std::runtime_error("Reference to *sound-info* bad: invalid type pointer");
  }

  int len = words.at(word_idx + 1).data;
  int name_offset = env.dts->ts.lookup_field_info("sound-info", "name").field.offset();

  std::vector<std::string> result;
  std::unordered_set<std::string> used_names;
  for (int i = 0; i < len; i++) {
    auto& elt_word = words.at(word_idx + 4 + i);
    if (elt_word.kind() != LinkedWord::PTR) {
      throw std::runtime_error("Reference to *sound-info* bad: invalid element");
    }
    auto name = fmt::format(
        "snd-{}",
        read_sound_name(*env.file, env.file->labels.at(elt_word.label_id()), name_offset));
    // some sounds have duplicate entries
    if (!used_names.insert(name).second) {
      name += fmt::format("-{}", i);
    }
    result.push_back(name);
  }
  return result;
}

}  // namespace

void run_sound_info(Function& top_level_func, std::vector<std::string>& sound_info_names) {
  auto& env = top_level_func.ir2.env;
  if (!top_level_func.ir2.top_form) {
    return;
  }
  for (auto fe : top_level_func.ir2.top_form->elts()) {
    /* Looks something like this:
        (set! *sound-info* (new 'static 'boxed-array :type sound-info
     */
    auto as_set = dynamic_cast<SetFormFormElement*>(fe);
    if (!as_set || !match(Matcher::symbol("*sound-info*"), as_set->dst()).matched) {
      continue;
    }
    auto src = as_set->src()->try_as_element<DecompiledDataElement>();
    if (src) {
      sound_info_names = read_static_sound_info_array(src->label(), env);
    }
  }
}
}  // namespace decompiler
