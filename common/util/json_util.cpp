#include "json_util.h"

#include "common/log/log.h"
#include "common/util/Assert.h"

/*!
 * Strip out // and / * comments
 * Does not strip comments from within a string.
 * Assumes \" is used to escape quotes inside of a string.
 */
std::string strip_cpp_style_comments(const std::string& input) {
  std::string output;

  enum State { NORMAL, BLOCK_COMMENT, LINE_COMMENT, STRING } state = NORMAL;

  for (size_t i = 0; i < input.size(); i++) {
    char c = input[i];
    char next = i < (input.size() - 1) ? input[i + 1] : '\0';
    char prev = i > 0 ? input[i - 1] : '\0';

    switch (state) {
      case NORMAL:
        if (c == '/' && next == '*') {
          // Normal -> block
          state = BLOCK_COMMENT;
          i++;  // skip over *
        } else if (c == '/' && next == '/') {
          // Normal -> line
          state = LINE_COMMENT;
          i++;
        } else if (c == '"') {
          // Normal -> string
          state = STRING;
          output.push_back(c);
        } else {
          // Normal -> normal
          output.push_back(c);
        }
        break;
      case BLOCK_COMMENT:
        if (c == '*' && next == '/') {
          // Block -> normal
          state = NORMAL;
          i++;
        }
        // otherwise stay in block comment
        break;
      case LINE_COMMENT:
        if (c == '\n') {
          state = NORMAL;
          output.push_back(c);
        }
        break;
      case STRING:
        if (c == '"' && prev != '\\') {
          state = NORMAL;
        }
        output.push_back(c);
        break;
      default:
        ASSERT(false);
    }
  }

  if (state == BLOCK_COMMENT) {
    throw std::runtime_error("strip_cpp_style_comments ended in a block comment");
  }

  if (state == STRING) {
    throw std::runtime_error("strip_cpp_style_comments ended in a string.");
  }

  return output;
}

std::optional<nlohmann::json> safe_parse_json(const std::string& input) {
  json j = json::parse(input, nullptr, false);
  if (j.is_discarded()) {
    lg::error("Failed to parse json");
    return {};
  }
  return j;
}

/*!
 * Parse JSON file with comments stripped. Unlike the default comment stripping feature
 * of nlohmann::json, this allows you to have multiple comments in a row!
 */
nlohmann::json parse_commented_json(const std::string& input, const std::string& source_name) {
  try {
    return nlohmann::json::parse(strip_cpp_style_comments(input));
  } catch (const std::exception& e) {
    if (source_name.empty()) {
      lg::error("Error parsing JSON file: {}", e.what());
    } else {
      lg::error("Error parsing JSON in file {}: {}", source_name, e.what());
    }
    throw;
  }
}

/*!
 * Parse something like:
 * 2 -> Range(2, 3)
 * [2, 3] -> Range(2, 3)
 * [2, 10] -> Range(2, 10)
 */
Range<int> parse_json_optional_integer_range(const nlohmann::json& json) {
  if (json.is_number_integer()) {
    auto first = json.get<int>();
    return Range<int>(first, first + 1);
  } else if (json.is_array()) {
    if (json.size() != 2) {
      throw std::runtime_error("Invalid array size in parse_json_optional_integer_range");
    }
    return {json[0].get<int>(), json[1].get<int>()};
  } else {
    throw std::runtime_error("Invalid json as input to parse_json_optional_integer_range");
  }
}
