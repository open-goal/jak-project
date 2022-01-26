#include "third-party/fmt/core.h"
#include "decompiler/ObjectFile/ObjectFileDB.h"
#include "dir_tpages.h"

namespace decompiler {
std::string DirTpageResult::to_source() const {
  std::string result;
  int i = 0;
  for (auto len : lengths) {
    result += fmt::format("  {:6s} ;; entry {}\n", fmt::format("#x{:x}", len), i);
    i++;
  }
  return result;
}

DirTpageResult process_dir_tpages(ObjectFileData& data) {
  DirTpageResult result;

  auto& words = data.linked_data.words_by_seg.at(0);

  int word_idx = 0;
  // first is type
  assert(words.at(word_idx).kind() == LinkedWord::TYPE_PTR);
  assert(words.at(word_idx).symbol_name() == "texture-page-dir");
  word_idx++;
  // next is length
  assert(words.at(word_idx).kind() == LinkedWord::PLAIN_DATA);
  int dir_length = words.at(word_idx).data;
  word_idx++;

  for (int i = 0; i < dir_length; i++) {
    assert(words.at(word_idx).kind() == LinkedWord::PLAIN_DATA);
    u32 entry = words.at(word_idx).data;
    assert((entry & 0xffff7000) == 0);  // 7 checks for sign bit.
    word_idx++;
    result.lengths.push_back(entry & 0xffff);

    assert(words.at(word_idx).kind() == LinkedWord::SYM_PTR);
    assert(words.at(word_idx).symbol_name() == "#f");
    word_idx++;
    assert(words.at(word_idx).kind() == LinkedWord::SYM_PTR);
    assert(words.at(word_idx).symbol_name() == "#f");
    word_idx++;
  }

  assert(word_idx == (int)words.size());

  return result;
}
}  // namespace decompiler
