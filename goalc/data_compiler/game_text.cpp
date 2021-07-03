/*!
 * @file game_text.cpp
 * Builds the XCOMMON.TXT text files.  Each file contains all the strings that appear in the game
 * translated into a language.
 *
 * The decompiler/data/game_text.cpp file extracts text from the game and creates a file that
 * can be read with these functions.
 */

#include <algorithm>
#include "game_text.h"
#include "common/goos/Reader.h"
#include "DataObjectGenerator.h"
#include "common/util/FileUtil.h"
#include "third-party/fmt/core.h"

namespace {
template <typename T>
void for_each_in_list(const goos::Object& list, const T& f) {
  const goos::Object* iter = &list;
  while (iter->is_pair()) {
    auto lap = iter->as_pair();
    f(lap->car);
    iter = &lap->cdr;
  }

  if (!iter->is_empty_list()) {
    throw std::runtime_error("Invalid list");
  }
}

int64_t get_int(const goos::Object& obj) {
  if (obj.is_int()) {
    return obj.integer_obj.value;
  }
  throw std::runtime_error(obj.print() + " was supposed to be an integer, but isn't");
}

const goos::Object& car(const goos::Object& x) {
  if (!x.is_pair()) {
    throw std::runtime_error("invalid pair");
  }

  return x.as_pair()->car;
}

const goos::Object& cdr(const goos::Object& x) {
  if (!x.is_pair()) {
    throw std::runtime_error("invalid pair");
  }

  return x.as_pair()->cdr;
}

std::string get_string(const goos::Object& x) {
  if (x.is_string()) {
    return x.as_string()->data;
  }
  throw std::runtime_error(x.print() + " was supposed to be an string, but isn't");
}

std::string uppercase(const std::string& in) {
  std::string result;
  result.reserve(in.size());
  for (auto c : in) {
    if (c >= 'a' && c <= 'z') {
      c -= ('a' - 'A');
    }
    result.push_back(c);
  }
  return result;
}

/*!
 * Parse a game text file for all languages.
 * The result is a vector<map<text_id, string>>
 *  so result[lang_id][text_id] gets you the text in the given language.
 *
 * The file should begin with (language-count x) with the given number of languages.
 * Each entry should be (text-id "text-in-lang-0" "text-in-lang-1" ... )
 * The text id's can be out of order or missing entries.
 */
std::vector<std::unordered_map<int, std::string>> parse(const goos::Object& data,
                                                        std::string* group_name) {
  std::vector<std::unordered_map<int, std::string>> text;
  bool languages_set = false;
  bool group_name_set = false;
  std::string possible_group_name;

  for_each_in_list(data.as_pair()->cdr, [&](const goos::Object& obj) {
    if (obj.is_pair()) {
      auto head = obj.as_pair()->car;
      if (head.is_symbol() && head.as_symbol()->name == "language-count") {
        if (languages_set) {
          throw std::runtime_error("Languages has been set multiple times.");
        }

        text.resize(get_int(car(cdr(obj))));
        if (!cdr(cdr(obj)).is_empty_list()) {
          throw std::runtime_error("language-count has too many arguments");
        }
      } else if (head.is_symbol() && head.as_symbol()->name == "group-name") {
        if (group_name_set) {
          throw std::runtime_error("group-name has been set multiple times.");
        }
        group_name_set = true;

        possible_group_name = get_string(car(cdr(obj)));
        if (!cdr(cdr(obj)).is_empty_list()) {
          throw std::runtime_error("group-name has too many arguments");
        }
      }

      else if (head.is_int()) {
        int i = 0;
        int id = head.as_int();
        for_each_in_list(cdr(obj), [&](const goos::Object& entry) {
          if (i >= int(text.size())) {
            throw std::runtime_error(
                "String has too many entries. There should be one per language");
          }

          if (entry.is_string()) {
            auto& map = text.at(i);
            if (map.find(id) != map.end()) {
              throw std::runtime_error("Entry appears more than once");
            }

            map[id] = entry.as_string()->data;
          } else {
            throw std::runtime_error("Each entry must be a string");
          }

          i++;
        });
        if (i != int(text.size())) {
          throw std::runtime_error("String did not have an entry for each language");
        }
      } else {
        throw std::runtime_error("Invalid game text file entry: " + head.print());
      }
    } else {
      throw std::runtime_error("Invalid game text file");
    }
  });

  if (!group_name_set) {
    throw std::runtime_error("group-name not set.");
  }
  *group_name = possible_group_name;
  return text;
}

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

/*!
 * Write game text data to a file. Uses the V2 object format which is identical between GOAL and
 * OpenGOAL, so this should produce exactly identical files to what is found in the game.
 */
void compile(const std::vector<std::unordered_map<int, std::string>>& text,
             const std::string& group_name) {
  if (text.empty()) {
    return;
  }
  // get all text ID's we know
  std::vector<int> add_order;
  add_order.reserve(text.front().size());
  for (auto& x : text.front()) {
    add_order.push_back(x.first);
  }
  // and sort them to be added in order. This matches the game.
  std::sort(add_order.begin(), add_order.end());

  for (int lang = 0; lang < int(text.size()); lang++) {
    DataObjectGenerator gen;
    gen.add_type_tag("game-text-info");  // type
    gen.add_word(text.front().size());   // length
    gen.add_word(lang);                  // language-id
    // this string is found in the string pool.
    gen.add_ref_to_string_in_pool(group_name);  // group-name

    // now add all the datas:
    for (auto id : add_order) {
      gen.add_word(id);  // id
      // these strings must be in the string pool, as sometimes there are duplicate
      // strings in a single language, and these strings should be stored once and have multiple
      // references to them.
      gen.add_ref_to_string_in_pool(text.at(lang).at(id));  // text
    }
    auto data = gen.generate_v2();

    file_util::create_dir_if_needed(file_util::get_file_path({"out", "iso"}));
    file_util::write_binary_file(
        file_util::get_file_path(
            {"out", "iso", fmt::format("{}{}.TXT", lang, uppercase(group_name))}),
        data.data(), data.size());
  }
}
}  // namespace

/*!
 * Read a game text description file and generate GOAL objects.
 */
void compile_game_text(const std::string& filename) {
  goos::Reader reader;
  auto code = reader.read_from_file({filename});
  printf("[Build Game Text] %s\n", filename.c_str());
  std::string group_name;
  auto text_map = parse(code, &group_name);
  compile(text_map, group_name);
}
