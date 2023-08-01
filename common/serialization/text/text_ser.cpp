#include "text_ser.h"

#include "common/goos/ParseHelpers.h"
#include "common/goos/Reader.h"

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
void parse_text_goal(const goos::Object& data,
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
