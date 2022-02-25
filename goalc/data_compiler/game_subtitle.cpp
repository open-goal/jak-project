/*!
 * @file game_subtitle.cpp
 * Builds the XSUBTIT.TXT text files.  Each file contains all the strings used as subtitles for
 * cutscenes, hints and ambient speech, along with the timings.
 *
 * This kind of file is completely custom.
 */

#include <algorithm>
#include <new>
#include <queue>
#include "game_subtitle.h"
#include "DataObjectGenerator.h"
#include "common/goos/Reader.h"
#include "common/util/FileUtil.h"
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
 * Parse a game subtitle file.
 * Information is added to the game subtitles database.
 *
 * The file should begin with (language-id x y z...) with the given language IDs.
 * Each entry should be (name (frame "line-text-0" "line-text-1") ... )
 * This adds the subtitle to each of the specified languages.
 */
void parse(const goos::Object& data, GameTextVersion text_ver, GameSubtitleDB& db) {
  auto font = get_font_bank(text_ver);
  std::map<int, GameSubtitleBank*> banks;
  bool languages_set = false;

  for_each_in_list(data.as_pair()->cdr, [&](const goos::Object& obj) {
    if (obj.is_pair()) {
      auto& head = car(obj);
      if (head.is_symbol() && head.as_symbol()->name == "language-id") {
        if (languages_set) {
          throw std::runtime_error("Languages have been set multiple times.");
        }

        if (cdr(obj).is_empty_list()) {
          throw std::runtime_error("At least one language must be set.");
        }

        for_each_in_list(cdr(obj), [&](const goos::Object& obj) {
          auto lang = get_int(obj);
          if (!db.bank_exists(lang)) {
            // database has no lang yet
            banks[lang] = db.new_bank(lang);
          } else {
            banks[lang] = db.bank_by_id(lang);
          }
        });

        languages_set = true;
      }

      else if (head.is_string()) {
        if (!languages_set) {
          throw std::runtime_error("At least one language must be set before defining entries.");
        }
        GameSubtitleSceneInfo scene(head.as_string()->data);
        for_each_in_list(cdr(obj), [&](const goos::Object& entry) {
          if (entry.is_pair()) {
            if (!car(entry).is_int() || !car(cdr(entry)).is_symbol() ||
                !car(cdr(cdr(entry))).is_string() || !car(cdr(cdr(cdr(entry)))).is_string()) {
              throw std::runtime_error(
                  "Each entry must be of format (number symbol \"string\" \"string\")");
            }

            auto line = font->convert_utf8_to_game(car(cdr(cdr(cdr(entry)))).as_string()->data);
            auto speaker = font->convert_utf8_to_game(car(cdr(cdr(entry))).as_string()->data);
            auto offscreen = car(cdr(entry)).as_symbol()->name != "#f";
            scene.add_line(car(entry).as_int(), line, speaker, offscreen);
          } else {
            throw std::runtime_error("Each entry must be a list");
          }
        });
        for (auto& [lang, bank] : banks) {
          if (!bank->scene_exists(scene.name())) {
            bank->add_scene(scene);
          } else {
            // this should copy the data, so it's safe to delete the new one afterwards.
            auto& old_scene = bank->scene_by_name(scene.name());
            old_scene.from_other_scene(scene);
          }
        }
      } else {
        throw std::runtime_error("Invalid game subtitles file entry: " + head.print());
      }
    } else {
      throw std::runtime_error("Invalid game subtitles file");
    }
  });
  if (!languages_set) {
    throw std::runtime_error("At least one language must be set.");
  }
}

/*!
 * Write game subtitle data to a file. Uses the V2 object format which is identical between GOAL and
 * OpenGOAL.
 */
void compile(GameSubtitleDB& db) {
  for (const auto& [lang, bank] : db.banks()) {
    DataObjectGenerator gen;
    gen.add_type_tag("subtitle-text-info");  // type
    gen.add_word(bank->scenes().size());     // length
    gen.add_word(lang);                      // lang
    gen.add_word(0);                         // dummy

    // fifo queue for scene data arrays
    std::queue<int> array_link_sources;
    // now add all the scene infos
    for (auto& [name, scene] : bank->scenes()) {
      gen.add_word(0 |
                   (scene.lines().size() << 16));  // kind (lower 16 bits), length (upper 16 bits)

      array_link_sources.push(gen.words());
      gen.add_word(0);  // keyframes (linked later)

      gen.add_ref_to_string_in_pool(scene.name());  // name

      gen.add_word(0);  // pad (string is 4 bytes but we have 8)
    }
    // now add all the scene *data!* (keyframes)
    for (auto& [name, scene] : bank->scenes()) {
      // link inline-array with reference from earlier
      gen.link_word_to_word(array_link_sources.front(), gen.words());
      array_link_sources.pop();

      for (auto& subtitle : scene.lines()) {
        gen.add_word(subtitle.frame);                           // frame
        gen.add_ref_to_string_in_pool(subtitle.line);           // line
        gen.add_ref_to_string_in_pool(subtitle.speaker);        // speaker
        gen.add_symbol_link(subtitle.offscreen ? "#t" : "#f");  // offscreen
      }
    }

    auto data = gen.generate_v2();

    file_util::create_dir_if_needed(file_util::get_file_path({"out", "iso"}));
    file_util::write_binary_file(
        file_util::get_file_path(
            {"out", "iso", fmt::format("{}{}.TXT", lang, uppercase("subtit"))}),
        data.data(), data.size());
  }
}
}  // namespace

void compile_game_subtitle(const std::vector<std::string>& filenames,
                           GameTextVersion text_ver,
                           GameSubtitleDB& db) {
  goos::Reader reader;
  for (auto& filename : filenames) {
    fmt::print("[Build Game Subtitle] {}\n", filename.c_str());
    auto code = reader.read_from_file({filename});
    parse(code, text_ver, db);
  }
  compile(db);
}
