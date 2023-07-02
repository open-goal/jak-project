#include "text_ser.h"

#include "common/goos/ParseHelpers.h"
#include "common/goos/Reader.h"

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

      if (action == "file-json") {
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
