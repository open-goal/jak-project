#include "subtitles2_ser.h"

#include "common/goos/ParseHelpers.h"
#include "common/goos/Reader.h"
#include "common/util/FileUtil.h"
#include "common/util/json_util.h"

#include "common/log/log.h"


void parse_subtitle2_json(GameSubtitle2DB& db, const GameSubtitle2DefinitionFile& file_info) {
  // TODO - some validation
  // Init Settings
  std::shared_ptr<GameSubtitle2Bank> bank;
  try {

     if (!db.bank_exists(file_info.language_id)) {
      // database has no lang yet
      bank = db.add_bank(std::make_shared<GameSubtitle2Bank>(file_info.language_id));
    }
    else {
      bank = db.bank_by_id(file_info.language_id);
    }
    bank->text_version = file_info.text_version;
    bank->file_path = file_info.file_path;
    const GameTextFontBank* font = get_font_bank(file_info.text_version);
    // Parse the file
    auto file = parse_commented_json(
          file_util::read_text_file(file_util::get_jak_project_dir() / file_info.file_path),
          "subtitle2_json");
    from_json(file, *bank);
  } catch (std::exception& e) {
    lg::error("Unable to parse subtitle json entry, couldn't successfully load files - {}",
              e.what());
    throw;
  }
}

void to_json(json & j, const Subtitle2Line& obj) {
  j = json{{"start", obj.start},
           {"end", obj.end},
           {"offscreen", obj.offscreen},
           {"speaker", obj.speaker},
           {"text", obj.text}};
}
void from_json(const json& j, Subtitle2Line& obj) {
  json_deserialize_if_exists(start);
  json_deserialize_if_exists(end);
  json_deserialize_if_exists(offscreen);
  json_deserialize_if_exists(speaker);
  json_deserialize_if_exists(text);
}
void to_json(json& j, const Subtitle2Scene& obj) {
  j = json{{"name", obj.name}, {"lines", obj.lines}};
}
void from_json(const json& j, Subtitle2Scene& obj) {
  json_deserialize_if_exists(name);
  for (auto& kv : j.at("lines").items()) {
    auto& line = obj.lines.emplace_back();
    from_json(kv.value(), line);
  }
}
void to_json(json& j, const GameSubtitle2Bank& obj) {
  j = json{{"speakers", obj.speakers}, {"scenes", obj.scenes}, {"lang", obj.lang}};
}
void from_json(const json& j, GameSubtitle2Bank& obj) {
  json_deserialize_if_exists(speakers);
  for (auto& kv : j.at("scenes").items()) {
    Subtitle2Scene scene;
    from_json(kv.value(), scene);
    obj.scenes[kv.key()] = scene;
  }
  json_deserialize_if_exists(lang);
}

void open_subtitle2_project(const std::string& kind,
                            const std::string& filename,
                            std::vector<GameSubtitle2DefinitionFile>& subtitle_files) {
  goos::Reader reader;
  auto& proj = reader.read_from_file({filename}).as_pair()->cdr.as_pair()->car;
  if (!proj.is_pair() || !proj.as_pair()->car.is_symbol() ||
      proj.as_pair()->car.as_symbol()->name != kind) {
    throw std::runtime_error(fmt::format("invalid {} project", kind));
  }

  goos::for_each_in_list(proj.as_pair()->cdr, [&](const goos::Object& o) {
    if (o.is_pair() && o.as_pair()->cdr.is_pair()) {
      auto args = o.as_pair();
      auto& action = args->car.as_symbol()->name;
      args = args->cdr.as_pair();

      if (action == "file-json") {
        GameSubtitle2DefinitionFile new_file;
        while (true) {
          const auto& kwarg = args->car.as_symbol()->name;
          args = args->cdr.as_pair();
          if (kwarg == ":language-id") {
            new_file.language_id = args->car.as_int();
          } else if (kwarg == ":text-version") {
            new_file.text_version = get_text_version_from_name(args->car.as_string()->data);
          } else if (kwarg == ":data") {
            new_file.file_path = args->car.as_string()->data;
          }
          if (args->cdr.is_empty_list()) {
            break;
          }
          args = args->cdr.as_pair();
        }
        subtitle_files.push_back(new_file);
      } else {
        throw std::runtime_error(fmt::format("unknown action {} in {} project", action, kind));
      }
    } else {
      throw std::runtime_error(fmt::format("invalid entry in {} project", kind));
    }
  });
}

GameSubtitle2DB load_subtitle2_project(GameVersion game_version) {
  // Load the subtitle files
  GameSubtitle2DB db;
  try {
    goos::Reader reader;
    std::vector<GameSubtitle2DefinitionFile> files;
    std::string subtitle_project = (file_util::get_jak_project_dir() / "game" / "assets" /
                                    version_to_game_name(game_version) / "game_subtitle.gp")
                                       .string();
    open_subtitle2_project("subtitle2", subtitle_project, files);
    for (auto& file : files) {
      parse_subtitle2_json(db, file);
    }
  } catch (std::runtime_error& e) {
    lg::error("error loading subtitle project: {}", e.what());
  }

  return db;
}
