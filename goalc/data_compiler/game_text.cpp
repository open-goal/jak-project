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
#include "common/util/FontUtils.h"
#include "common/goos/ParseHelpers.h"
#include "third-party/fmt/core.h"

namespace {
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
  throw std::runtime_error(x.print() + " was supposed to be a string, but isn't");
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
 * Parse a game text file.
 * Information is added to the game text database.
 *
 * The file should begin with (language-id x y z...) with the given language IDs.
 * Each entry should be (id "line for 1st language" "line for 2nd language" ...)
 * This adds the text line to each of the specified languages.
 */
void parse(const goos::Object& data, GameTextVersion text_ver, GameTextDB& db) {
  auto font = get_font_bank(text_ver);
  std::vector<std::shared_ptr<GameTextBank>> banks;
  bool group_name_set = false;
  std::string possible_group_name;

  for_each_in_list(data.as_pair()->cdr, [&](const goos::Object& obj) {
    if (obj.is_pair()) {
      auto& head = car(obj);
      if (head.is_symbol() && head.as_symbol()->name == "language-id") {
        if (banks.size() != 0) {
          throw std::runtime_error("Languages have been set multiple times.");
        }

        if (cdr(obj).is_empty_list()) {
          throw std::runtime_error("At least one language must be set.");
        }

        if (!group_name_set) {
          throw std::runtime_error("Text group must be set before languages.");
        }

        for_each_in_list(cdr(obj), [&](const goos::Object& obj) {
          auto lang = get_int(obj);
          if (!db.bank_exists(possible_group_name, lang)) {
            // database has no lang in this group yet
            banks.push_back(db.add_bank(possible_group_name, std::make_shared<GameTextBank>(lang)));
          } else {
            banks.push_back(db.bank_by_id(possible_group_name, lang));
          }
        });
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
        if (banks.size() == 0) {
          throw std::runtime_error("At least one language must be set before defining entries.");
        }
        int i = 0;
        int id = head.as_int();
        for_each_in_list(cdr(obj), [&](const goos::Object& entry) {
          if (entry.is_string()) {
            if (i >= int(banks.size())) {
              throw std::runtime_error(fmt::format("Too many strings in text id #x{:x}", id));
            }

            auto line = font->convert_utf8_to_game(entry.as_string()->data);
            banks[i++]->set_line(id, line);
          } else {
            throw std::runtime_error(fmt::format("Non-string value in text id #x{:x}", id));
          }
        });
        if (i != int(banks.size())) {
          throw std::runtime_error(
              fmt::format("Not enough strings specified in text id #x{:x}", id));
        }
      } else {
        throw std::runtime_error("Invalid game text file entry: " + head.print());
      }
    } else {
      throw std::runtime_error("Invalid game text file");
    }
  });
  if (banks.size() == 0) {
    throw std::runtime_error("At least one language must be set.");
  }
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
void compile(GameTextDB& db) {
  for (const auto& [group_name, banks] : db.groups()) {
    for (const auto& [lang, bank] : banks) {
      DataObjectGenerator gen;
      gen.add_type_tag("game-text-info");  // type
      gen.add_word(bank->lines().size());  // length
      gen.add_word(lang);                  // language-id
      // this string is found in the string pool.
      gen.add_ref_to_string_in_pool(group_name);  // group-name

      // now add all the datas: (the lines are already sorted by id)
      for (auto& [id, line] : bank->lines()) {
        gen.add_word(id);  // id
        // these strings must be in the string pool, as sometimes there are duplicate
        // strings in a single language, and these strings should be stored once and have multiple
        // references to them.
        gen.add_ref_to_string_in_pool(line);  // text
      }

      auto data = gen.generate_v2();

      file_util::create_dir_if_needed(file_util::get_file_path({"out", "iso"}));
      file_util::write_binary_file(
          file_util::get_file_path(
              {"out", "iso", fmt::format("{}{}.TXT", lang, uppercase(group_name))}),
          data.data(), data.size());
    }
  }
}
}  // namespace

/*!
 * Read a game text description file and generate GOAL objects.
 */
void compile_game_text(const std::vector<std::string>& filenames, GameTextVersion text_ver) {
  GameTextDB db;
  goos::Reader reader;
  for (auto& filename : filenames) {
    fmt::print("[Build Game Text] {}\n", filename.c_str());
    auto code = reader.read_from_file({filename});
    parse(code, text_ver, db);
  }
  compile(db);
}
