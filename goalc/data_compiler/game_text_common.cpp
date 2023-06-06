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
#include "common/util/FileUtil.h"
#include "common/util/FontUtils.h"
#include "common/util/json_util.h"

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
void compile_text(GameTextDB& db, const std::string& output_prefix) {
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

      file_util::create_dir_if_needed(file_util::get_file_path({"out", output_prefix, "iso"}));
      file_util::write_binary_file(
          file_util::get_file_path(
              {"out", output_prefix, "iso", fmt::format("{}{}.TXT", lang, uppercase(group_name))}),
          data.data(), data.size());
    }
  }
}

/*!
 * Write game subtitle data to a file. Uses the V2 object format which is identical between GOAL and
 * OpenGOAL.
 */
void compile_subtitle(GameSubtitleDB& db, const std::string& output_prefix) {
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

    file_util::create_dir_if_needed(file_util::get_file_path({"out", output_prefix, "iso"}));
    file_util::write_binary_file(
        file_util::get_file_path(
            {"out", output_prefix, "iso", fmt::format("{}{}.TXT", lang, uppercase("subtit"))}),
        data.data(), data.size());
  }
}

/*!
 * Write game subtitle2 data to a file. Uses the V2 object format which is identical between GOAL
 * and OpenGOAL.
 */
void compile_subtitle2(GameSubtitle2DB& db, const std::string& output_prefix) {
  auto& speaker_names = get_speaker_names(db.version());
  for (const auto& [lang, bank] : db.banks()) {
    auto font = get_font_bank(bank->text_version);
    DataObjectGenerator gen;
    gen.add_type_tag("subtitle2-text-info");                   // type
    gen.add_word((bank->scenes.size() & 0xffff) | (1 << 16));  // length (lo) + version (hi)
    // note: we add 1 because "none" isn't included
    gen.add_word((lang & 0xffff) | ((speaker_names.size() + 1) << 16));  // lang + speaker-length
    int speaker_array_link = gen.add_word(0);  // speaker array (dummy for now)

    auto speaker_index_by_name = [&speaker_names](const std::string& name) {
      for (int i = 0; i < speaker_names.size(); ++i) {
        if (speaker_names.at(i) == name) {
          return i + 1;
        }
      }
      return 0;
    };

    // fifo queue for scene data arrays
    std::queue<int> array_link_sources;
    // now add all the scenes inline
    for (auto& [name, scene] : bank->scenes) {
      gen.add_ref_to_string_in_pool(name);  // scene name
      gen.add_word(scene.lines.size());     // line amount
      array_link_sources.push(gen.words());
      gen.add_word(0);  // line array (linked later)
    }
    // now add all the line arrays and link them to their scene
    for (auto& [name, scene] : bank->scenes) {
      // link inline-array with reference from earlier
      gen.link_word_to_word(array_link_sources.front(), gen.words());
      array_link_sources.pop();

      for (auto& line : scene.lines) {
        gen.add_word_float(line.start);  // start frame
        gen.add_word_float(line.end);    // end frame
        if (!line.merge) {
          gen.add_ref_to_string_in_pool(font->convert_utf8_to_game(line.text));  // line text
        } else {
          gen.add_symbol_link("#f");
        }
        u16 speaker = speaker_index_by_name(line.speaker);
        u16 flags = 0;
        flags |= line.offscreen << 0;
        flags |= line.merge << 1;
        gen.add_word(speaker | (flags << 16));  // speaker (lo) + flags (hi)
      }
    }
    // now write the array of strings for the speakers
    gen.link_word_to_word(speaker_array_link, gen.words());
    // we write #f for invalid entries, including the "none" at the start
    gen.add_symbol_link("#f");
    for (auto& speaker_name : speaker_names) {
      if (bank->speakers.count(speaker_name) == 0) {
        // no speaker for this
        gen.add_symbol_link("#f");
      } else {
        gen.add_ref_to_string_in_pool(font->convert_utf8_to_game(bank->speakers.at(speaker_name)));
      }
    }

    auto data = gen.generate_v2();

    file_util::create_dir_if_needed(file_util::get_file_path({"out", output_prefix, "iso"}));
    file_util::write_binary_file(
        file_util::get_file_path(
            {"out", output_prefix, "iso", fmt::format("{}{}.TXT", lang, uppercase("subti2"))}),
        data.data(), data.size());
  }
}
}  // namespace

/*!
 * Read a game text description file and generate GOAL objects.
 */
void compile_game_text(const std::vector<GameTextDefinitionFile>& files,
                       GameTextDB& db,
                       const std::string& output_prefix) {
  goos::Reader reader;
  for (auto& file : files) {
    if (file.format == GameTextDefinitionFile::Format::GOAL) {
      lg::print("[Build Game Text] GOAL {}\n", file.file_path);
      auto code = reader.read_from_file({file.file_path});
      parse_text(code, db, file);
    } else if (file.format == GameTextDefinitionFile::Format::JSON) {
      lg::print("[Build Game Text] JSON {}\n", file.file_path);
      auto file_path = file_util::get_jak_project_dir() / file.file_path;
      auto json = parse_commented_json(file_util::read_text_file(file_path), file.file_path);
      parse_text_json(json, db, file);
    }
  }
  compile_text(db, output_prefix);
}

void compile_game_subtitle(const std::vector<GameSubtitleDefinitionFile>& files,
                           GameSubtitleDB& db,
                           const std::string& output_prefix) {
  goos::Reader reader;
  for (auto& file : files) {
    if (file.format == GameSubtitleDefinitionFile::Format::GOAL) {
      lg::print("[Build Game Subtitle] GOAL {}\n", file.lines_path);
      auto code = reader.read_from_file({file.lines_path});
      parse_subtitle(code, db, file.lines_path);
    } else if (file.format == GameSubtitleDefinitionFile::Format::JSON) {
      lg::print("[Build Game Subtitle] JSON {}:{}\n", file.lines_path, file.meta_path);
      parse_subtitle_json(db, file);
    }
  }
  compile_subtitle(db, output_prefix);
}

void compile_game_subtitle2(const std::vector<GameSubtitle2DefinitionFile>& files,
                            GameSubtitle2DB& db,
                            const std::string& output_prefix) {
  goos::Reader reader;
  for (auto& file : files) {
    lg::print("[Build Game Subtitle] JSON {}\n", file.file_path);
    parse_subtitle2_json(db, file);
  }
  compile_subtitle2(db, output_prefix);
}
