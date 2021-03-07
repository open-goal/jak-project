#include <cassert>
#include "json_comment_strip.h"

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
        assert(false);
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

/*!
 * Parse JSON file with comments stripped. Unlike the default comment stripping feature
 * of nlohmann::json, this allows you to have multiple comments in a row!
 */
nlohmann::json parse_commented_json(const std::string& input) {
  return nlohmann::json::parse(strip_cpp_style_comments(input));
}