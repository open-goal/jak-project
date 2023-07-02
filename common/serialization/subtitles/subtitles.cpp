#include "subtitles.h"

#include "common/goos/ParseHelpers.h"
#include "common/goos/Reader.h"
#include "common/util/FileUtil.h"
#include "common/util/json_util.h"

#include "third-party/fmt/core.h"

// TODO - why is this needed if it's also stored in the json file...where is the actual source of
// truth?

// matches enum in `subtitle2.gc` with "none" (first) and "max" (last) removed
const std::vector<std::string> s_speakers_jak2 = {
    "computer",
    "jak",
    "darkjak",
    "daxter",
    "samos",
    "keira",
    "keira-before-class-3",
    "kid",
    "kor",
    "metalkor",
    "baron",
    "errol",
    "torn",
    "tess",
    "guard",
    "guard-a",
    "guard-b",
    "krew",
    "sig",
    "brutter",
    "vin",
    "youngsamos",
    "youngsamos-before-rescue",
    "pecker",
    "onin",
    "ashelin",
    "jinx",
    "mog",
    "grim",
    "agent",
    "citizen-male",
    "citizen-female",
    "oracle",
    "precursor",
};

const std::vector<std::string> get_speaker_names(GameVersion version) {
  switch (version) {
    case GameVersion::Jak2:
      return s_speakers_jak2;
    default:
      break;
  }
  throw std::runtime_error(
      fmt::format("no speakers for game version {} project", version_to_game_name(version)));
}

int64_t get_int(const goos::Object& obj) {
  if (obj.is_int()) {
    return obj.integer_obj.value;
  }
  throw std::runtime_error(obj.print() + " was supposed to be an integer, but isn't");
}

const goos::Object& car(const goos::Object& x) {
  if (!x.is_pair()) {
    throw std::runtime_error("car: invalid pair");
  }

  return x.as_pair()->car;
}

const goos::Object& cdr(const goos::Object& x) {
  if (!x.is_pair()) {
    throw std::runtime_error("cdr: invalid pair");
  }

  return x.as_pair()->cdr;
}

std::string get_string(const goos::Object& x) {
  if (x.is_string()) {
    return x.as_string()->data;
  }
  throw std::runtime_error(x.print() + " was supposed to be a string, but isn't");
}

// Do the converting at compile time, can simplify all of this
void parse_subtitle(GameSubtitleDB& db, const GameSubtitleDefinitionFile& file_info) {
  // TODO - some validation
  // Init Settings
  std::shared_ptr<GameSubtitleBank> bank;
  if (!db.bank_exists(file_info.language_id)) {
    // database has no lang yet
    bank = db.add_bank(std::make_shared<GameSubtitleBank>(file_info.language_id));
  } else {
    bank = db.bank_by_id(file_info.language_id);
  }
  bank->m_text_version = file_info.text_version;
  bank->m_file_path = file_info.lines_path;
  const GameTextFontBank* font = get_font_bank(file_info.text_version);
  // Parse the file
  SubtitleMetadataFile meta_file;
  SubtitleFile lines_file;
  try {
    // If we have a base file defined, load that and merge it
    if (file_info.meta_base_path) {
      auto base_data =
          parse_commented_json(file_util::read_text_file(file_util::get_jak_project_dir() /
                                                         file_info.meta_base_path.value()),
                               "subtitle_meta_base_path");
      auto data = parse_commented_json(
          file_util::read_text_file(file_util::get_jak_project_dir() / file_info.meta_path),
          "subtitle_meta_path");
      base_data.at("cutscenes").update(data.at("cutscenes"));
      base_data.at("hints").update(data.at("hints"));
      meta_file = base_data;

    } else {
      meta_file = parse_commented_json(
          file_util::read_text_file(file_util::get_jak_project_dir() / file_info.meta_path),
          "subtitle_meta_path");
    }
    if (file_info.lines_base_path) {
      auto base_data =
          parse_commented_json(file_util::read_text_file(file_util::get_jak_project_dir() /
                                                         file_info.lines_base_path.value()),
                               "subtitle_line_base_path");

      auto data = parse_commented_json(
          file_util::read_text_file(file_util::get_jak_project_dir() / file_info.lines_path),
          "subtitle_line_path");
      base_data.at("cutscenes").update(data.at("cutscenes"));
      base_data.at("hints").update(data.at("hints"));
      base_data.at("speakers").update(data.at("speakers"));
      auto test = base_data.dump();
      lines_file = base_data;
    } else {
      lines_file = parse_commented_json(
          file_util::read_text_file(file_util::get_jak_project_dir() / file_info.lines_path),
          "subtitle_line_path");
    }
  } catch (std::exception& e) {
    lg::error("Unable to parse subtitle json entry, couldn't successfully load files - {}",
              e.what());
    throw;
  }
  // Iterate through the metadata file as blank lines are now omitted from the lines file now
  // Cutscenes First
  for (const auto& [cutscene_name, cutscene_lines] : meta_file.cutscenes) {
    GameSubtitleSceneInfo scene(SubtitleSceneKind::Movie);
    scene.set_name(cutscene_name);
    // Iterate the lines, grab the actual text from the lines file if it's not a clear screen entry
    int line_idx = 0;
    int lines_added = 0;
    for (const auto& line : cutscene_lines) {
      if (line.clear) {
        scene.add_clear_entry(line.frame);
        lines_added++;
      } else {
        if (lines_file.speakers.find(line.speaker) == lines_file.speakers.end() ||
            lines_file.cutscenes.find(cutscene_name) == lines_file.cutscenes.end() ||
            int(lines_file.cutscenes.at(cutscene_name).size()) < line_idx) {
          lg::warn(
              "{} Couldn't find {} in line file, or line list is too small, or speaker could not "
              "be resolved {}!",
              file_info.language_id, cutscene_name, line.speaker);
        } else {
          // NOTE - the convert_utf8_to_game function is really really slow (about 80-90% of the
          // time loading the subtitle files)
          // TODO - improve that as a follow up sometime in the future
          scene.add_line(
              line.frame,
              font->convert_utf8_to_game(lines_file.cutscenes.at(cutscene_name).at(line_idx)),
              font->convert_utf8_to_game(lines_file.speakers.at(line.speaker)), line.offscreen);
          lines_added++;
        }
        line_idx++;
      }
    }
    // Verify we added the amount of lines we expected to
    if (lines_added != int(cutscene_lines.size())) {
      throw std::runtime_error(
          fmt::format("Cutscene: '{}' has a mismatch in metadata lines vs text lines. Expected {} "
                      "only added {} lines",
                      cutscene_name, cutscene_lines.size(), lines_added));
    }

    // TODO - add scene, can't we just use an emplace here?
    if (!bank->scene_exists(scene.name())) {
      bank->add_scene(scene);
    } else {
      auto& old_scene = bank->scene_by_name(scene.name());
      old_scene.from_other_scene(scene);
    }
  }
  // Now hints
  for (const auto& [hint_name, hint_info] : meta_file.hints) {
    GameSubtitleSceneInfo scene(SubtitleSceneKind::HintNamed);
    scene.set_name(hint_name);
    /*scene.m_sorting_group = db.m_subtitle_groups->find_group(hint_name);
    scene.m_sorting_group_idx = db.m_subtitle_groups->find_group_index(scene.m_sorting_group);*/
    if (hint_info.id == "0") {
      scene.m_kind = SubtitleSceneKind::HintNamed;
    } else {
      scene.set_id(std::stoi(hint_info.id, nullptr, 16));
    }
    // Iterate the lines, grab the actual text from the lines file if it's not a clear screen entry
    int line_idx = 0;
    int lines_added = 0;
    for (const auto& line : hint_info.lines) {
      if (line.clear) {
        scene.add_clear_entry(line.frame);
        lines_added++;
      } else {
        if (lines_file.speakers.find(line.speaker) == lines_file.speakers.end() ||
            lines_file.hints.find(hint_name) == lines_file.hints.end() ||
            int(lines_file.hints.at(hint_name).size()) < line_idx) {
          lg::warn(
              "{} Couldn't find {} in line file, or line list is too small, or speaker could not "
              "be resolved {}!",
              file_info.language_id, hint_name, line.speaker);
        } else {
          // NOTE - the convert_utf8_to_game function is really really slow (about 80-90% of the
          // time loading the subtitle files)
          // TODO - improve that as a follow up sometime in the future
          scene.add_line(line.frame,
                         font->convert_utf8_to_game(lines_file.hints.at(hint_name).at(line_idx)),
                         font->convert_utf8_to_game(lines_file.speakers.at(line.speaker)), true);
          lines_added++;
        }
        line_idx++;
      }
    }
    // Verify we added the amount of lines we expected to
    if (lines_added != int(hint_info.lines.size())) {
      throw std::runtime_error(
          fmt::format("Hint: '{}' has a mismatch in metadata lines vs text lines. Expected {} "
                      "only added {} lines",
                      hint_name, hint_info.lines.size(), lines_added));
    }

    // TODO - add scene, can't we just use an emplace here?
    if (!bank->scene_exists(scene.name())) {
      bank->add_scene(scene);
    } else {
      auto& old_scene = bank->scene_by_name(scene.name());
      old_scene.from_other_scene(scene);
    }
  }
}

void open_subtitle_project(const std::string& kind,
                           const std::string& filename,
                           std::vector<GameSubtitleDefinitionFile>& subtitle_files) {
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
        auto new_file = GameSubtitleDefinitionFile();
        while (true) {
          const auto& kwarg = args->car.as_symbol()->name;
          args = args->cdr.as_pair();
          if (kwarg == ":language-id") {
            new_file.language_id = args->car.as_int();
          } else if (kwarg == ":text-version") {
            new_file.text_version = args->car.as_string()->data;
          } else if (kwarg == ":lines") {
            new_file.lines_path = args->car.as_string()->data;
          } else if (kwarg == ":meta") {
            new_file.meta_path = args->car.as_string()->data;
          } else if (kwarg == ":lines-base") {
            new_file.lines_base_path = args->car.as_string()->data;
          } else if (kwarg == ":meta-base") {
            new_file.meta_base_path = args->car.as_string()->data;
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

void to_json(json& j, const SubtitleCutsceneLineMetadata& obj) {
  j = json{{"frame", obj.frame},
           {"offscreen", obj.offscreen},
           {"speaker", obj.speaker},
           {"clear", obj.clear}};
}
void from_json(const json& j, SubtitleCutsceneLineMetadata& obj) {
  json_deserialize_if_exists(frame);
  json_deserialize_if_exists(offscreen);
  json_deserialize_if_exists(speaker);
  json_deserialize_if_exists(clear);
}
void to_json(json& j, const SubtitleHintLineMetadata& obj) {
  j = json{{"frame", obj.frame}, {"speaker", obj.speaker}, {"clear", obj.clear}};
}
void from_json(const json& j, SubtitleHintLineMetadata& obj) {
  json_deserialize_if_exists(frame);
  json_deserialize_if_exists(speaker);
  json_deserialize_if_exists(clear);
}
void to_json(json& j, const SubtitleHintMetadata& obj) {
  j = json{{"id", obj.id}, {"lines", obj.lines}};
}
void from_json(const json& j, SubtitleHintMetadata& obj) {
  json_deserialize_if_exists(id);
  json_deserialize_if_exists(lines);
}
void to_json(json& j, const SubtitleMetadataFile& obj) {
  j = json{{"cutscenes", obj.cutscenes}, {"hints", obj.hints}};
}

void from_json(const json& j, SubtitleMetadataFile& obj) {
  json_deserialize_if_exists(cutscenes);
  json_deserialize_if_exists(hints);
}
void to_json(json& j, const SubtitleFile& obj) {
  j = json{{"speakers", obj.speakers}, {"cutscenes", obj.cutscenes}, {"hints", obj.hints}};
}
void from_json(const json& j, SubtitleFile& obj) {
  json_deserialize_if_exists(speakers);
  json_deserialize_if_exists(cutscenes);
  json_deserialize_if_exists(hints);
}

GameSubtitleDB load_subtitle_project(GameVersion game_version) {
  // Load the subtitle files
  GameSubtitleDB db;
  try {
    goos::Reader reader;
    std::vector<GameSubtitleDefinitionFile> files;
    std::string subtitle_project = (file_util::get_jak_project_dir() / "game" / "assets" /
                                    version_to_game_name(game_version) / "game_subtitle.gp")
                                       .string();
    open_subtitle_project("subtitle", subtitle_project, files);
    for (auto& file : files) {
      parse_subtitle(db, file);
    }
  } catch (std::runtime_error& e) {
    lg::error("error loading subtitle project: {}", e.what());
  }

  return db;
}
