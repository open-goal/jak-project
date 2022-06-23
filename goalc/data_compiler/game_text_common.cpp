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

#include "game_text_common.h"

#include <algorithm>
#include <queue>

#include "DataObjectGenerator.h"

#include "common/goos/ParseHelpers.h"
#include "common/goos/Reader.h"
#include "common/serialization/subtitles/subtitles.h"
#include "common/util/FileUtil.h"
#include "common/util/FontUtils.h"

#include "third-party/fmt/core.h"

namespace {

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
    parse_subtitle(code, text_ver, db, filename);
  }
  compile_subtitle(db);
}
