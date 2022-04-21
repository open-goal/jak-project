

#include "subtitles.h"
#include <common/goos/ParseHelpers.h>
#include "common/goos/Reader.h"
#include <common/util/FileUtil.h>

// TODO - break these out properly, duplicate code right now
static const std::unordered_map<std::string, GameTextVersion> s_text_ver_enum_map = {
    {"jak1-v1", GameTextVersion::JAK1_V1}};

std::unordered_map<GameTextVersion, std::vector<std::string>> open_text_project(
    const std::string& kind,
    const std::string& filename) {
  goos::Reader reader;
  auto& proj = reader.read_from_file({filename}).as_pair()->cdr.as_pair()->car;
  if (!proj.is_pair() || !proj.as_pair()->car.is_symbol() ||
      proj.as_pair()->car.as_symbol()->name != kind) {
    // throw std::runtime_error(fmt::format("invalid {} project", kind));
  }

  std::unordered_map<GameTextVersion, std::vector<std::string>> inputs;
  goos::for_each_in_list(proj.as_pair()->cdr, [&](const goos::Object& o) {
    if (!o.is_pair()) {
      // throw std::runtime_error(fmt::format("invalid entry in {} project", kind));
    }

    auto& ver = o.as_pair()->car.as_symbol()->name;
    auto& in = o.as_pair()->cdr.as_pair()->car.as_string()->data;

    inputs[s_text_ver_enum_map.at(ver)].push_back(in);
  });

  return inputs;
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

void parse_subtitle_files(const goos::Object& data, GameTextVersion text_ver, GameSubtitleDB& db) {
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

      else if (head.is_string()) {
        if (banks.size() == 0) {
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
  if (banks.size() == 0) {
    throw std::runtime_error("At least one language must be set.");
  }
}

GameSubtitleDB load_subtitle_project() {
  // Load the subtitle files
  GameSubtitleDB db;
  goos::Reader reader;
  std::string subtitle_project =
      (file_util::get_jak_project_dir() / "game" / "assets" / "game_subtitle.gp").string();
  for (auto& [ver, in] : open_text_project("subtitle", subtitle_project)) {
    for (auto& filename : in) {
      auto code = reader.read_from_file({filename});
      parse_subtitle_files(code, ver, db);
    }
  }
  return db;
}

// TODO - write a deserializer, the compiler still can do the compiling!

