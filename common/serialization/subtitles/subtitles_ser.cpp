#include "subtitles_ser.h"

#include "common/goos/ParseHelpers.h"
#include "common/goos/Reader.h"
#include "common/util/FileUtil.h"
#include "common/util/json_util.h"

#include "third-party/fmt/core.h"

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

/*!
 * Parse a game text file (GOAL format).
 * Information is added to the game text database.
 *
 * The file should begin with (language-id x y z...) with the given language IDs.
 * Each entry should be (id "line for 1st language" "line for 2nd language" ...)
 * This adds the text line to each of the specified languages.
 */
void parse_text(const goos::Object& data,
                GameTextDB& db,
                const GameTextDefinitionFile& /*file_info*/) {
  const GameTextFontBank* font = nullptr;
  std::vector<std::shared_ptr<GameTextBank>> banks;
  std::string possible_group_name;

  for_each_in_list(data.as_pair()->cdr, [&](const goos::Object& obj) {
    if (obj.is_pair()) {
      auto& head = car(obj);
      if (head.is_symbol("language-id")) {
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
      } else if (head.is_symbol("group-name")) {
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
      } else if (head.is_symbol("credits")) {
        // parse a "credits" object. it's a list of lines where the ID automatically increments, and
        // empty lines are skipped
        if (banks.size() == 0) {
          throw std::runtime_error("At least one language must be set before defining entries.");
        }

        if (cdr(obj).is_empty_list() || cdr(cdr(obj)).is_empty_list() ||
            !car(cdr(obj)).is_symbol(":begin") || !car(cdr(cdr(obj))).is_int()) {
          throw std::runtime_error("Invalid credits begin param");
        }

        const auto& it = cdr(cdr(obj));
        int begin_id = car(it).as_int();
        int id = begin_id - 1;
        for_each_in_list(cdr(it), [&](const goos::Object& entry) {
          ++id;
          if (entry.is_string()) {
            if (entry.as_string()->data.empty()) {
              // empty string! just advance
              return;
            }

            auto line = font->convert_utf8_to_game(entry.as_string()->data);
            // add to all langs
            for (auto& bank : banks) {
              bank->set_line(id, line);
            }
          } else if (entry.is_pair()) {
            int b_i = 0;
            for_each_in_list(entry, [&](const goos::Object& entry) {
              if (entry.is_string()) {
                if (b_i >= int(banks.size())) {
                  throw std::runtime_error(fmt::format("Too many strings in text id #x{:x}", id));
                }

                auto line = font->convert_utf8_to_game(entry.as_string()->data);
                banks[b_i++]->set_line(id, line);
              } else {
                throw std::runtime_error(fmt::format("Non-string value in text id #x{:x}", id));
              }
            });
            if (b_i != int(banks.size())) {
              throw std::runtime_error(
                  fmt::format("Not enough strings specified in text id #x{:x}", id));
            }
          } else {
            throw std::runtime_error(fmt::format("Non-string value in text id #x{:x}", id));
          }
        });
      } else if (head.is_symbol("text-version")) {
        if (font) {
          throw std::runtime_error("text version is already set");
        }

        const auto& ver_name = car(cdr(obj));
        if (!ver_name.is_symbol()) {
          throw std::runtime_error("invalid text version entry");
        }

        font = get_font_bank(ver_name.as_symbol()->name);
      }

      else if (head.is_int()) {
        if (!font) {
          throw std::runtime_error("Text version must be set before defining entries.");
        }
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

/*!
 * Parse a game text file (JSON format).
 * Information is added to the game text database.
 *
 * Each single text entry should follow this format:
 *    <text_id>: <text_value>
 * For example, for (display #x1043) you would add this entry:
 *    "1043": "DISPLAY"
 *
 * You can also set multiple sequential lines starting from an initial text_id:
 *    <text_id>: [<text_value_1>, <text_value_2>, etc]
 * For example, Jak 1 credits start at #x0b00, so this would override the first 3 lines:
 *    "0b00": [
 *      "JAK AND DAXTER: CREDITS LINE 1",
 *      "CREDITS LINE 2",
 *      "CREDITS LINE 3"
 *    ]
 */
void parse_text_json(const nlohmann::json& json,
                     GameTextDB& db,
                     const GameTextDefinitionFile& file_info) {
  // Verify we have all data that we need
  if (!file_info.group_name.has_value()) {
    throw std::runtime_error(
        fmt::format("Can't parse {}, did not provide group_name", file_info.file_path));
  }
  if (file_info.language_id == -1) {
    throw std::runtime_error(
        fmt::format("Can't parse {}, did not provide language_id", file_info.file_path));
  }
  if (file_info.text_version.empty()) {
    throw std::runtime_error(
        fmt::format("Can't parse {}, did not provide text_version", file_info.file_path));
  }
  // Init Settings
  std::shared_ptr<GameTextBank> bank;
  if (!db.bank_exists(file_info.group_name.value(), file_info.language_id)) {
    // database has no lang in this group yet
    bank = db.add_bank(file_info.group_name.value(),
                       std::make_shared<GameTextBank>(file_info.language_id));
  } else {
    bank = db.bank_by_id(file_info.group_name.value(), file_info.language_id);
  }
  const GameTextFontBank* font = get_font_bank(file_info.text_version);
  // Parse the file
  for (const auto& [text_id, text_value] : json.items()) {
    auto line_id = std::stoi(text_id, nullptr, 16);
    if (text_value.is_string()) {
      // single line replacement
      auto line = font->convert_utf8_to_game(text_value);
      // TODO - lint duplicate line definitions across text files
      bank->set_line(line_id, line);
    } else if (text_value.is_array()) {
      // multi-line replacement starting from line_id
      //  (e.g. for Jak 1 credits, start from x0b00)
      for (const auto& [idx, raw_line] : text_value.items()) {
        if (!raw_line.is_string()) {
          throw std::runtime_error(fmt::format(
              "Non string provided for line {} / text id #x{} of _credits", idx, line_id));
        }
        auto line = font->convert_utf8_to_game(raw_line);
        bank->set_line(line_id++, line);  // increment line_id
      }
    } else {
      // Unexpected value type
      throw std::runtime_error(
          fmt::format("Must provide string or array for text id #x{}", text_id));
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
void parse_subtitle(const goos::Object& data,
                    GameSubtitleDB& db,
                    const std::string& /*file_path*/) {
  const GameTextFontBank* font = nullptr;
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
      } else if (head.is_symbol("text-version")) {
        if (font) {
          throw std::runtime_error("text version is already set");
        }

        const auto& ver_name = car(cdr(obj));
        if (!ver_name.is_symbol()) {
          throw std::runtime_error("invalid text version entry");
        }
        font = get_font_bank(ver_name.as_symbol()->name);
      }

      else if (head.is_string() || head.is_int()) {
        if (!font) {
          throw std::runtime_error("Text version must be set before defining entries.");
        }
        if (banks.size() == 0) {
          throw std::runtime_error("At least one language must be set before defining scenes.");
        }
        auto kind = SubtitleSceneKind::Movie;
        int id = 0;
        auto entries = cdr(obj);
        if (head.is_int()) {
          kind = SubtitleSceneKind::Hint;
        } else if (entries.is_pair() && car(entries).is_symbol()) {
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
            throw std::runtime_error(
                fmt::format("{} | Each entry must be a non-empty list", scene.name()));
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

void parse_subtitle_json(GameSubtitleDB& db, const GameSubtitleDefinitionFile& file_info) {
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
  // Iterate through the metadata file as blank lines are no omitted from the lines file now
  // Cutscenes First
  for (const auto& [cutscene_name, cutscene_lines] : meta_file.cutscenes) {
    GameSubtitleSceneInfo scene(SubtitleSceneKind::Movie);
    scene.set_name(cutscene_name);
    scene.m_sorting_group = db.m_subtitle_groups->find_group(cutscene_name);
    scene.m_sorting_group_idx = db.m_subtitle_groups->find_group_index(scene.m_sorting_group);
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

GameTextVersion parse_text_only_version(const std::string& filename) {
  goos::Reader reader;
  return parse_text_only_version(reader.read_from_file({filename}));
}

GameTextVersion parse_text_only_version(const goos::Object& data) {
  const GameTextFontBank* font = nullptr;

  for_each_in_list(data.as_pair()->cdr, [&](const goos::Object& obj) {
    if (obj.is_pair()) {
      auto& head = car(obj);
      if (head.is_symbol("text-version")) {
        if (font) {
          throw std::runtime_error("text version is already set");
        }

        const auto& ver_name = car(cdr(obj));
        if (!ver_name.is_symbol()) {
          throw std::runtime_error("invalid text version entry");
        }

        font = get_font_bank(ver_name.as_symbol()->name);
      }
    }
  });

  if (!font) {
    throw std::runtime_error("text version not found");
  }

  return font->version();
}

void GameSubtitleGroups::hydrate_from_asset_file() {
  std::string file_path = (file_util::get_jak_project_dir() / "game" / "assets" / "jak1" /
                           "subtitle" / "subtitle-groups.json")
                              .string();
  auto config_str = file_util::read_text_file(file_path);
  auto group_data = parse_commented_json(config_str, file_path);

  for (const auto& [key, val] : group_data.items()) {
    try {
      if (key == group_order_key) {
        m_group_order = val.get<std::vector<std::string>>();
      } else {
        m_groups[key] = val.get<std::vector<std::string>>();
      }
    } catch (std::exception& ex) {
      lg::print("Bad subtitle group entry - {} - {}", key, ex.what());
    }
  }
}

std::string GameSubtitleGroups::find_group(const std::string& scene_name) {
  for (auto const& [group, scenes] : m_groups) {
    for (auto const& name : scenes) {
      if (name == scene_name) {
        return group;
      }
    }
  }
  // Add to the uncategorized group if it wasn't found
  m_groups[uncategorized_group].push_back(scene_name);
  return uncategorized_group;
}

int GameSubtitleGroups::find_group_index(const std::string& group_name) {
  auto it = find(m_group_order.begin(), m_group_order.end(), group_name);
  if (it != m_group_order.end()) {
    return it - m_group_order.begin();
  } else {
    return m_group_order.size() - 1;
  }
}

void GameSubtitleGroups::remove_scene(const std::string& group_name,
                                      const std::string& scene_name) {
  if (m_groups.count(group_name) == 0) {
    lg::error("Subtitle group {} doesn't exist! Abort.", group_name);
    return;
  }
  m_groups[group_name].erase(
      std::remove(m_groups[group_name].begin(), m_groups[group_name].end(), scene_name),
      m_groups[group_name].end());
}

void GameSubtitleGroups::add_scene(const std::string& group_name, const std::string& scene_name) {
  std::string group = group_name;
  if (m_groups.count(group_name) == 0) {
    lg::error("Subtitle group {} doesn't exist! Add to uncategorized.", group_name);
    group = uncategorized_group;
  }
  auto it = std::find(m_groups[group].begin(), m_groups[group].end(), scene_name);
  if (it != m_groups[group].end()) {
    lg::error("Scene {} already exists in group {}", scene_name, group);
  } else {
    m_groups[group].push_back(scene_name);
  }
}

void open_text_project(const std::string& kind,
                       const std::string& filename,
                       std::vector<GameTextDefinitionFile>& text_files) {
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

      if (action == "file") {
        auto& file_path = args->car.as_string()->data;
        auto new_file = GameTextDefinitionFile();
        new_file.format = GameTextDefinitionFile::Format::GOAL;
        new_file.file_path = file_path;
        text_files.push_back(new_file);
      } else if (action == "file-json") {
        auto& language_id = args->car.as_int();
        args = args->cdr.as_pair();
        auto& text_version = args->car.as_symbol()->name;
        args = args->cdr.as_pair();
        std::optional<std::string> group_name = std::nullopt;
        group_name = args->car.as_string()->data;
        args = args->cdr.as_pair()->car.as_pair();
        goos::for_each_in_list(args->cdr.as_pair()->car, [&](const goos::Object& o) {
          text_files.push_back({GameTextDefinitionFile::Format::JSON, o.as_string()->data,
                                (int)language_id, text_version, group_name});
        });
      } else {
        throw std::runtime_error(fmt::format("unknown action {} in {} project", action, kind));
      }
    } else {
      throw std::runtime_error(fmt::format("invalid entry in {} project", kind));
    }
  });
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

      if (action == "file") {
        auto& file_path = args->car.as_string()->data;
        auto new_file = GameSubtitleDefinitionFile();
        new_file.format = GameSubtitleDefinitionFile::Format::GOAL;
        new_file.lines_path = file_path;
        subtitle_files.push_back(new_file);
      } else if (action == "file-json") {
        auto new_file = GameSubtitleDefinitionFile();
        new_file.format = GameSubtitleDefinitionFile::Format::JSON;
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

// TODO - temporary code for migration
SubtitleMetadataFile dump_bank_as_meta_json(
    std::shared_ptr<GameSubtitleBank> bank,
    std::unordered_map<std::string, std::string> speaker_lookup) {
  auto meta_file = SubtitleMetadataFile();
  auto font = get_font_bank("jak1-v2");
  for (const auto& [scene_name, scene_info] : bank->m_scenes) {
    if (scene_info.m_kind == SubtitleSceneKind::Movie) {
      std::vector<SubtitleCutsceneLineMetadata> lines;
      for (const auto& line : scene_info.m_lines) {
        auto line_meta = SubtitleCutsceneLineMetadata();
        line_meta.frame = line.frame;
        if (line.line.empty()) {
          line_meta.clear = true;
        } else {
          auto line_speaker = font->convert_game_to_utf8(line.speaker.c_str());
          for (const auto& [speaker, speaker_localized] : speaker_lookup) {
            if (line_speaker == speaker_localized) {
              line_speaker = speaker;
            }
          }
          line_meta.offscreen = line.offscreen;
          line_meta.speaker = line_speaker;
        }
        lines.push_back(line_meta);
      }
      meta_file.cutscenes[scene_name] = lines;
    } else if (scene_info.m_kind == SubtitleSceneKind::Hint ||
               scene_info.m_kind == SubtitleSceneKind::HintNamed) {
      SubtitleHintMetadata hint;
      hint.id = fmt::format("{:x}", scene_info.m_id);
      std::vector<SubtitleHintLineMetadata> lines;
      for (const auto& line : scene_info.m_lines) {
        auto line_meta = SubtitleHintLineMetadata();
        line_meta.frame = line.frame;
        if (line.line.empty()) {
          line_meta.clear = true;
        } else {
          auto line_speaker = font->convert_game_to_utf8(line.speaker.c_str());
          for (const auto& [speaker, speaker_localized] : speaker_lookup) {
            if (line_speaker == speaker_localized) {
              line_speaker = speaker;
            }
          }
          line_meta.speaker = line_speaker;
        }
        lines.push_back(line_meta);
      }
      hint.lines = lines;
      meta_file.hints[scene_name] = hint;
    }
  }
  return meta_file;
}

// TODO - temporary code for migration
SubtitleFile dump_bank_as_json(std::shared_ptr<GameSubtitleBank> bank,
                               std::shared_ptr<GameSubtitleBank> base_bank,
                               std::unordered_map<std::string, std::string> speaker_lookup) {
  SubtitleFile file;
  file.speakers = speaker_lookup;
  auto font = get_font_bank("jak1-v2");
  // Figure out speakers
  for (const auto& [scene_name, scene_info] : bank->m_scenes) {
    for (const auto& line : scene_info.m_lines) {
      if (line.line.empty()) {
        continue;
      }
      auto line_speaker = font->convert_game_to_utf8(line.speaker.c_str());
      bool new_speaker = true;
      for (const auto& [speaker, speaker_localized] : file.speakers) {
        if (line_speaker == speaker_localized) {
          new_speaker = false;
          break;
        }
      }
      if (new_speaker) {
        // if the speaker is in the english speaker map, append it
        if (speaker_lookup.find(line_speaker) != speaker_lookup.end()) {
          file.speakers[line_speaker] = line_speaker;
        } else {
          // otherwise, go figure it out manually, most names are the same so this isn't worth
          // writing code for
          file.speakers[fmt::format("unknown-{}", scene_info.m_name)] = line_speaker;
        }
      }
    }
  }
  // Hints
  for (const auto& [scene_name, scene_info] : bank->m_scenes) {
    if (scene_info.m_kind == SubtitleSceneKind::Hint ||
        scene_info.m_kind == SubtitleSceneKind::HintNamed) {
      // Check if the number of hints in the translated language match that of the base language
      if (base_bank->m_scenes.find(scene_name) == base_bank->m_scenes.end()) {
        lg::warn("scene not found in base language - {}:{}", bank->m_lang_id, scene_name);
      } else {
        if (scene_info.m_lines.size() > base_bank->m_scenes.at(scene_name).m_lines.size()) {
          lg::info("hint - translation has more lines than base - {}:{}", bank->m_lang_id,
                   scene_name);
        }
        file.hints[scene_name] = {};
        for (const auto& scene_line : scene_info.m_lines) {
          if (scene_line.line.empty()) {
            continue;
          }
          auto line_utf8 = font->convert_game_to_utf8(scene_line.line.c_str());
          file.hints[scene_name].push_back(line_utf8);
        }
      }
    }
  }

  // Cutscenes
  for (const auto& [scene_name, scene_info] : bank->m_scenes) {
    if (scene_info.m_kind == SubtitleSceneKind::Movie) {
      // Check if the number of hints in the translated language match that of the base language
      if (base_bank->m_scenes.find(scene_name) == base_bank->m_scenes.end()) {
        lg::warn("scene not found in base language - {}:{}", bank->m_lang_id, scene_name);
      } else {
        if (scene_info.m_lines.size() > base_bank->m_scenes.at(scene_name).m_lines.size()) {
          lg::info("cutscene - translation has more lines than base - {}:{}", bank->m_lang_id,
                   scene_name);
        }
        file.cutscenes[scene_name] = {};
        for (const auto& scene_line : scene_info.m_lines) {
          if (scene_line.line.empty()) {
            continue;
          }
          auto line_utf8 = font->convert_game_to_utf8(scene_line.line.c_str());
          file.cutscenes[scene_name].push_back(line_utf8);
        }
      }
    }
  }

  return file;
}

GameSubtitleDB load_subtitle_project(GameVersion game_version) {
  // Load the subtitle files
  GameSubtitleDB db;
  db.m_subtitle_groups = std::make_unique<GameSubtitleGroups>();
  db.m_subtitle_groups->hydrate_from_asset_file();
  try {
    goos::Reader reader;
    std::vector<GameSubtitleDefinitionFile> files;
    std::string subtitle_project = (file_util::get_jak_project_dir() / "game" / "assets" /
                                    version_to_game_name(game_version) / "game_subtitle.gp")
                                       .string();
    open_subtitle_project("subtitle", subtitle_project, files);
    for (auto& file : files) {
      if (file.format == GameSubtitleDefinitionFile::Format::GOAL) {
        auto code = reader.read_from_file({file.lines_path});
        parse_subtitle(code, db, file.lines_path);
      } else if (file.format == GameSubtitleDefinitionFile::Format::JSON) {
        parse_subtitle_json(db, file);
      }
    }
  } catch (std::runtime_error& e) {
    lg::error("error loading subtitle project: {}", e.what());
  }

  // Dump new JSON format (uncomment if you need it)
  // TODO -- TEMPORARY CODE FOR MIGRATION -- REMOVE LATER
  // auto speaker_json = parse_commented_json(
  //    file_util::read_text_file((file_util::get_jak_project_dir() / "game" / "assets" /
  //                               version_to_game_name(game_version) / "subtitle" /
  //                               "_speaker_lookup.jsonc")),
  //    "_speaker_lookup.jsonc");
  // auto speaker_lookup =
  //    speaker_json
  //        .get<std::unordered_map<std::string, std::unordered_map<std::string, std::string>>>();

  // std::vector<std::string> locale_lookup = {"en-US", "fr-FR", "de-DE", "es-ES", "it-IT",
  //                                           "jp-JP", "en-GB", "pt-PT", "fi-FI", "sv-SE",
  //                                           "da-DK", "no-NO", "nl-NL", "pt-BR", "hu-HU", "ca-ES",
  //                                           "is-IS"};

  // for (const auto& [language_id, bank] : db.m_banks) {
  //   auto meta_file =
  //       dump_bank_as_meta_json(bank, speaker_lookup.at(fmt::format("{}", language_id)));
  //   std::string dump_path =
  //       (file_util::get_jak_project_dir() / "game" / "assets" /
  //       version_to_game_name(game_version) /
  //        "subtitle" / fmt::format("subtitle_meta_{}.json", locale_lookup.at(language_id)))
  //           .string();
  //   json data = meta_file;
  //   try {
  //     std::string str = data.dump(2);
  //     file_util::write_text_file(dump_path, str);
  //   } catch (std::exception& ex) {
  //     lg::error(ex.what());
  //   }
  //   // Now dump the actual subtitles
  //   auto subtitle_file = dump_bank_as_json(bank, db.m_banks.at(0),
  //                                          speaker_lookup.at(fmt::format("{}", language_id)));
  //   dump_path =
  //       (file_util::get_jak_project_dir() / "game" / "assets" /
  //       version_to_game_name(game_version) /
  //        "subtitle" / fmt::format("subtitle_lines_{}.json", locale_lookup.at(language_id)))
  //           .string();
  //   data = subtitle_file;
  //   try {
  //     std::string str = data.dump(2);
  //     file_util::write_text_file(dump_path, str);
  //   } catch (std::exception& ex) {
  //     lg::error(ex.what());
  //   }
  // }

  return db;
}
