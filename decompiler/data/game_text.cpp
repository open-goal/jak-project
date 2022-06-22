#include <cstring>
#include <map>
#include <vector>
#include <algorithm>
#include "third-party/fmt/core.h"
#include "game_text.h"
#include "decompiler/ObjectFile/ObjectFileDB.h"
#include "common/goos/Reader.h"
#include "common/util/BitUtils.h"

namespace decompiler {
namespace {
template <typename T>
T get_word(const LinkedWord& word) {
  T result;
  ASSERT(word.kind() == LinkedWord::PLAIN_DATA);
  static_assert(sizeof(T) == 4, "bad get_word size");
  memcpy(&result, &word.data, 4);
  return result;
}

DecompilerLabel get_label(ObjectFileData& data, const LinkedWord& word) {
  ASSERT(word.kind() == LinkedWord::PTR);
  return data.linked_data.labels.at(word.label_id());
}

static const std::unordered_map<GameTextVersion, std::pair<int, int>> sTextCreditsIDs = {
    {GameTextVersion::JAK1_V1, {0xb00, 0xf00}},
    {GameTextVersion::JAK1_V2, {0xb00, 0xf00}}};

}  // namespace

/*
(deftype game-text (structure)
  ((id   uint32  :offset-assert 0)
   (text basic   :offset-assert 4)
   )
  )

(deftype game-text-info (basic)
  ((length      int32            :offset-assert 4)
   (language-id int32            :offset-assert 8)
   (group-name  basic            :offset-assert 12)
   (data        game-text :dynamic :offset-assert 16)
   )
  )
 */

GameTextResult process_game_text(ObjectFileData& data, GameTextVersion version) {
  GameTextResult result;
  auto& words = data.linked_data.words_by_seg.at(0);
  std::vector<int> read_words(words.size(), false);

  int offset = 0;

  // type tag for game-text-info
  if (words.at(offset).kind() != LinkedWord::TYPE_PTR ||
      words.front().symbol_name() != "game-text-info") {
    ASSERT(false);
  }
  read_words.at(offset)++;
  offset++;

  // length field
  read_words.at(offset)++;
  u32 text_count = get_word<u32>(words.at(offset++));

  // language-id field
  read_words.at(offset)++;
  u32 language = get_word<u32>(words.at(offset++));
  result.language = language;

  // group-name field
  read_words.at(offset)++;
  auto group_label = get_label(data, words.at(offset++));
  auto group_name = data.linked_data.get_goal_string_by_label(group_label);
  ASSERT(group_name == "common");
  // remember that we read these bytes
  auto group_start = (group_label.offset / 4) - 1;
  for (int j = 0; j < align16(8 + 1 + (int)group_name.length()) / 4; j++) {
    read_words.at(group_start + j)++;
  }

  // read each text...
  for (u32 i = 0; i < text_count; i++) {
    // id number
    read_words.at(offset)++;
    auto text_id = get_word<u32>(words.at(offset++));

    // label to string
    read_words.at(offset)++;
    auto text_label = get_label(data, words.at(offset++));

    // actual string
    auto text = data.linked_data.get_goal_string_by_label(text_label);
    result.total_text++;
    result.total_chars += text.length();

    // no duplicate ids
    if (result.text.find(text_id) != result.text.end()) {
      ASSERT(false);
    }

    // escape characters
    if (font_bank_exists(version)) {
      result.text[text_id] = get_font_bank(version)->convert_game_to_utf8(text.c_str());
    } else {
      result.text[text_id] = goos::get_readable_string(text.c_str());  // HACK!
    }

    // remember what we read (-1 for the type tag)
    auto string_start = (text_label.offset / 4) - 1;
    // 8 for type tag and length fields, 1 for null char.
    for (int j = 0, m = align16(8 + 1 + (int)text.length()) / 4;
         j < m && string_start + j < (int)read_words.size(); j++) {
      read_words.at(string_start + j)++;
    }
  }

  // alignment to the string section.
  while (offset & 3) {
    read_words.at(offset)++;
    offset++;
  }

  // make sure we read each thing at least once.
  // reading more than once is ok, some text is duplicated.
  for (int i = 0; i < int(words.size()); i++) {
    if (read_words[i] < 1) {
      std::string debug;
      data.linked_data.append_word_to_string(debug, words.at(i));
      ASSERT_MSG(false, fmt::format("[{}] {} 0x{}", i, int(read_words[i]), debug.c_str()));
    }
  }

  return result;
}

std::string write_game_text(
    GameTextVersion version,
    const std::unordered_map<int, std::unordered_map<int, std::string>>& data) {
  // first sort languages:
  std::vector<int> languages;
  for (const auto& lang : data) {
    languages.push_back(lang.first);
  }
  std::sort(languages.begin(), languages.end());

  // build map
  std::map<int, std::vector<std::string>> text_by_id;
  std::map<int, std::vector<std::string>> text_by_id_credits;
  std::map<int, std::vector<std::string>> text_by_id_post_credits;
  int last_credits_id = 0;
  int credits_begin = 0;
  int credits_end = 0;
  if (auto it = sTextCreditsIDs.find(version); it != sTextCreditsIDs.end()) {
    credits_begin = it->second.first;
    credits_end = it->second.second;
  }
  for (auto lang : languages) {
    for (auto& [id, text] : data.at(lang)) {
      if (id < credits_begin) {
        // comes before credits
        text_by_id[id].push_back(text);
      } else if (id < credits_end) {
        // comes before credits
        text_by_id_credits[id].push_back(text);
        if (id > last_credits_id) {
          last_credits_id = id;
        }
      } else {
        // comes after credits
        text_by_id_post_credits[id].push_back(text);
      }
    }
  }

  // write!
  std::string result;  // = "\xEF\xBB\xBF";  // UTF-8 encode (don't need this anymore)
  result += "(group-name \"common\")\n";
  result += "(language-id";
  for (auto lang : languages) {
    result += fmt::format(" {}", lang);
  }
  result += ")\n";
  for (auto& x : text_by_id) {
    result += fmt::format("(#x{:04x}\n  ", x.first);
    for (auto& y : x.second) {
      result += fmt::format("\"{}\"\n  ", y);
    }
    result += ")\n\n";
  }
  if (text_by_id_credits.size() > 0) {
    result += fmt::format("(credits :begin #x{:04x}\n  ", sTextCreditsIDs.at(version).first);

    for (int id = sTextCreditsIDs.at(version).first; id <= last_credits_id; ++id) {
      // check if the line exists first
      if (text_by_id_credits.count(id) == 0) {
        result += fmt::format("\"\"\n  ");
        continue;
      }
      // check if all lines are identical first
      bool diff_langs = false;
      bool is_first = true;
      std::string last_lang;
      for (auto& y : text_by_id_credits.at(id)) {
        if (is_first) {
          is_first = false;
        } else if (last_lang != y) {
          diff_langs = true;
          break;
        }
        last_lang = y;
      }
      // now write them
      if (!diff_langs) {
        result += fmt::format("\"{}\"\n  ", last_lang);
      } else {
        result += fmt::format("(");
        for (auto& y : text_by_id_credits.at(id)) {
          result += fmt::format("\"{}\"\n   ", y);
        }
        result += ")\n  ";
      }
    }
    result += ")\n\n";
  }
  for (auto& x : text_by_id_post_credits) {
    result += fmt::format("(#x{:04x}\n  ", x.first);
    for (auto& y : x.second) {
      result += fmt::format("\"{}\"\n  ", y);
    }
    result += ")\n\n";
  }

  return result;
}
}  // namespace decompiler
