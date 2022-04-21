/*!
 * @file game_subtitle.cpp
 * Builds the XSUBTIT.TXT text files.  Each file contains all the strings used as subtitles for
 * cutscenes, hints and ambient speech, along with the timings.
 *
 * This kind of file is completely custom.
 */

#include "game_subtitle.h"

#include <algorithm>
#include <new>
#include <queue>
#include "common/goos/Reader.h"
#include "common/util/FileUtil.h"
#include "common/goos/ParseHelpers.h"
#include "third-party/fmt/core.h"

#include "common/serialization/DataObjectGenerator.h"
#include "common/serialization/subtitles/subtitles.h"

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

void compile_game_subtitle(const std::vector<std::string>& filenames, GameTextVersion text_ver) {
  GameSubtitleDB db;
  goos::Reader reader;
  for (auto& filename : filenames) {
    fmt::print("[Build Game Subtitle] {}\n", filename.c_str());
    auto code = reader.read_from_file({filename});
    parse_subtitle_files(code, text_ver, db);
  }
  compile(db);
}
