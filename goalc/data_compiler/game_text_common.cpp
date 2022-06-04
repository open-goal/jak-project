/*!
 * @file game_text.cpp
 * Builds the XCOMMON.TXT text files.  Each file contains all the strings that appear in the game
 * translated into a language.
 *
 * The decompiler/data/game_text.cpp file extracts text from the game and creates a file that
 * can be read with these functions.
 *
 * Also builds the XSUBTIT.TXT text files.  Each file contains all the strings used as subtitles for
 * cutscenes, hints and ambient speech, along with the timings.
 * This kind of file is completely custom.
 */

#include <algorithm>
#include <queue>
#include "game_text_common.h"
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
void parse_text(const goos::Object& data, GameTextVersion text_ver, GameTextDB& db) {
  auto font = get_font_bank(text_ver);
  std::vector<std::shared_ptr<GameTextBank>> banks;
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

        if (possible_group_name.empty()) {
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
        if (!possible_group_name.empty()) {
          throw std::runtime_error("group-name has been set multiple times.");
        }

        possible_group_name = get_string(car(cdr(obj)));

        if (possible_group_name.empty()) {
          throw std::runtime_error("invalid group-name.");
        }

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
void compile_text(GameTextDB& db) {
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

/*!
 * Parse a game subtitle file.
 * Information is added to the game subtitles database.
 *
 * The file should begin with (language-id x y z...) for the given language IDs.
 * Each scene should be (scene-name <entry 1> <entry 2> ... )
 * This adds the subtitle to each of the specified languages.
 */
void parse_subtitle(const goos::Object& data, GameTextVersion text_ver, GameSubtitleDB& db) {
  auto font = get_font_bank(text_ver);
  std::map<int, std::shared_ptr<GameSubtitleBank>> banks;

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

        for_each_in_list(cdr(obj), [&](const goos::Object& obj) {
          auto lang = get_int(obj);
          if (!db.bank_exists(lang)) {
            // database has no lang yet
            banks[lang] = db.add_bank(std::make_shared<GameSubtitleBank>(lang));
          } else {
            banks[lang] = db.bank_by_id(lang);
          }
        });
      }

      else if (head.is_string() || head.is_int()) {
        if (banks.size() == 0) {
          throw std::runtime_error("At least one language must be set before defining scenes.");
        }
        auto kind = SubtitleSceneKind::Movie;
        int id = 0;
        auto entries = cdr(obj);
        if (head.is_int()) {
          kind = SubtitleSceneKind::Hint;
        } else if (car(entries).is_symbol()) {
          const auto& parm = car(entries).as_symbol()->name;
          if (parm == ":hint") {
            entries = cdr(entries);
            id = car(entries).as_int();
            kind = SubtitleSceneKind::HintNamed;
          } else {
            throw std::runtime_error("Unknown parameter for subtitle scene");
          }
          entries = cdr(entries);
        }

        GameSubtitleSceneInfo scene(kind);
        if (kind == SubtitleSceneKind::Movie || kind == SubtitleSceneKind::HintNamed) {
          scene.set_name(head.as_string()->data);
        } else if (kind == SubtitleSceneKind::Hint) {
          id = head.as_int();
        }
        scene.set_id(id);

        for_each_in_list(entries, [&](const goos::Object& entry) {
          if (entry.is_pair()) {
            // expected formats:
            // (time <args>)
            // all arguments have default values. the arguments are:
            // "speaker" "line" - two strings. one for the speaker's name and one for the actual
            //                    line. speaker can be empty. default is just empty string.
            // :offscreen - speaker is offscreen. default is not offscreen.

            if (!car(entry).is_int()) {
              throw std::runtime_error("Each entry must start with a timestamp (number)");
            }

            auto time = car(entry).as_int();
            goos::StringObject *speaker = nullptr, *line = nullptr;
            bool offscreen = false;
            if (scene.kind() == SubtitleSceneKind::Hint ||
                scene.kind() == SubtitleSceneKind::HintNamed) {
              offscreen = true;
            }
            for_each_in_list(cdr(entry), [&](const goos::Object& arg) {
              if (arg.is_string()) {
                if (!speaker) {
                  speaker = arg.as_string();
                } else if (!line) {
                  line = arg.as_string();
                } else {
                  throw std::runtime_error("Invalid string in subtitle entry");
                }
              } else if (speaker && !line) {
                throw std::runtime_error(
                    "Invalid object in subtitle entry, expecting actual line string after speaker");
              } else if (arg.is_symbol()) {
                if (scene.kind() == SubtitleSceneKind::Movie &&
                    arg.as_symbol()->name == ":offscreen") {
                  offscreen = true;
                } else {
                  throw std::runtime_error(
                      fmt::format("Unknown parameter {} in subtitle", arg.as_symbol()->name));
                }
              }
            });
            auto line_str = font->convert_utf8_to_game(line ? line->data : "");
            auto speaker_str = font->convert_utf8_to_game(speaker ? speaker->data : "");
            scene.add_line(time, line_str, speaker_str, offscreen);
          } else {
            throw std::runtime_error("Each entry must be a list");
          }
        });
        for (auto& [lang, bank] : banks) {
          if (!bank->scene_exists(scene.name())) {
            bank->add_scene(scene);
          } else {
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
  if (banks.size() == 0) {
    throw std::runtime_error("At least one language must be set.");
  }
}

/*!
 * Write game subtitle data to a file. Uses the V2 object format which is identical between GOAL and
 * OpenGOAL.
 */
void compile_subtitle(GameSubtitleDB& db) {
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
      gen.add_word((u16)scene.kind() |
                   (scene.lines().size() << 16));  // kind (lower 16 bits), length (upper 16 bits)

      array_link_sources.push(gen.words());
      gen.add_word(0);  // keyframes (linked later)

      if (scene.kind() == SubtitleSceneKind::Movie ||
          scene.kind() == SubtitleSceneKind::HintNamed) {
        gen.add_ref_to_string_in_pool(scene.name());  // name
      } else if (scene.kind() == SubtitleSceneKind::Hint) {
        gen.add_word(0);  // nothing
      }
      gen.add_word(scene.id());
    }
    // now add all the scene *data!* (keyframes)
    for (auto& [name, scene] : bank->scenes()) {
      // link inline-array with reference from earlier
      gen.link_word_to_word(array_link_sources.front(), gen.words());
      array_link_sources.pop();

      for (auto& subtitle : scene.lines()) {
        gen.add_word(subtitle.frame);                     // frame
        gen.add_ref_to_string_in_pool(subtitle.line);     // line
        gen.add_ref_to_string_in_pool(subtitle.speaker);  // speaker
        gen.add_word(subtitle.offscreen);                 // offscreen
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

/*!
 * Read a game text description file and generate GOAL objects.
 */
void compile_game_text(const std::vector<std::string>& filenames,
                       GameTextVersion text_ver,
                       GameTextDB& db) {
  goos::Reader reader;
  for (auto& filename : filenames) {
    fmt::print("[Build Game Text] {}\n", filename.c_str());
    auto code = reader.read_from_file({filename});
    parse_text(code, text_ver, db);
  }
  compile_text(db);
}

void compile_game_subtitle(const std::vector<std::string>& filenames,
                           GameTextVersion text_ver,
                           GameSubtitleDB& db) {
  goos::Reader reader;
  for (auto& filename : filenames) {
    fmt::print("[Build Game Subtitle] {}\n", filename.c_str());
    auto code = reader.read_from_file({filename});
    parse_subtitle(code, text_ver, db);
  }
  compile_subtitle(db);
}

static const std::unordered_map<std::string, GameTextVersion> s_text_ver_enum_map = {
    {"jak1-v1", GameTextVersion::JAK1_V1}};

void open_text_project(const std::string& kind,
                       const std::string& filename,
                       std::unordered_map<GameTextVersion, std::vector<std::string>>& inputs) {
  goos::Reader reader;
  auto& proj = reader.read_from_file({filename}).as_pair()->cdr.as_pair()->car;
  if (!proj.is_pair() || !proj.as_pair()->car.is_symbol() ||
      proj.as_pair()->car.as_symbol()->name != kind) {
    throw std::runtime_error(fmt::format("invalid {} project", kind));
  }

  goos::for_each_in_list(proj.as_pair()->cdr, [&](const goos::Object& o) {
    if (!o.is_pair()) {
      throw std::runtime_error(fmt::format("invalid entry in {} project", kind));
    }

    auto& ver = o.as_pair()->car.as_symbol()->name;
    auto& in = o.as_pair()->cdr.as_pair()->car.as_string()->data;

    inputs[s_text_ver_enum_map.at(ver)].push_back(in);
  });
}
