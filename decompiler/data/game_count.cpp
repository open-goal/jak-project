#include "game_count.h"

#include "LinkedWordReader.h"

#include "decompiler/ObjectFile/ObjectFileDB.h"

#include "fmt/core.h"

namespace decompiler {
GameCountResult process_game_count(ObjectFileData& data) {
  GameCountResult result;
  auto& words = data.linked_data.words_by_seg.at(0);
  auto reader = LinkedWordReader(&words);
  auto type = reader.get_type_tag();
  ASSERT(type == "game-count-info");
  auto length = reader.get_word<s32>();

  for (s32 i = 0; i < length; i++) {
    GameCountResult::CountInfo info;
    info.money_count = reader.get_word<s32>();
    info.buzzer_count = reader.get_word<s32>();
    result.info.push_back(info);
  }

  result.mystery_data[0] = reader.get_word<u32>();
  result.mystery_data[1] = reader.get_word<u32>();
  ASSERT(reader.words_left() == 0);
  return result;
}

std::string write_game_count(const GameCountResult& result) {
  std::string str;
  str +=
      ";; this file contains money/buzzer counts for each level.\n;; The last pair is unknown data "
      "and possibly a bug that it is included\n\n";

  for (auto& x : result.info) {
    str += fmt::format("(:money {} :buzzer {})\n", x.money_count, x.buzzer_count);
  }

  str += fmt::format("(:unknown-1 {} :unknown-2 {})\n", result.mystery_data[0],
                     result.mystery_data[1]);
  return str;
}
}  // namespace decompiler
