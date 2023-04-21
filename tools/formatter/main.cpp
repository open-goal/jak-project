// Iterates through the `all-types` DTS to find types that meet a variety of criteria, such as:
// - type size
// - field types at given offsets
// - parent-types
// - ...

#include <queue>

#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/json_util.h"
#include "common/util/string_util.h"
#include "common/util/unicode_util.h"

#include "decompiler/util/DecompilerTypeSystem.h"
#include "tree_sitter/api.h"

#include "third-party/CLI11.hpp"
#include "third-party/fmt/core.h"
#include "third-party/json.hpp"

// Declare the `tree_sitter_opengoal` function, which is
// implemented by the `tree-sitter-opengoal` library.
extern "C" {
extern const TSLanguage* tree_sitter_opengoal();
}

void walk_tree(TSTreeCursor* cursor, std::string& output, const std::string& source_code) {
  // an imperative breadth-first-search
  while (true) {
    // Process the node
    const auto curr_node = ts_tree_cursor_current_node(cursor);
    const std::string curr_node_type = ts_node_type(curr_node);
    std::string curr_node_field_name;
    if (ts_tree_cursor_current_field_name(cursor)) {
      curr_node_field_name = ts_tree_cursor_current_field_name(cursor);
    }
    if (curr_node_field_name == "open") {
      output += "(";
    } else if (curr_node_field_name == "close") {
      output.pop_back();
      output += ") ";
    }
    if (curr_node_type == "sym_name" || curr_node_type == "num_lit" ||
        curr_node_type == "str_lit") {
      uint32_t start = ts_node_start_byte(curr_node);
      uint32_t end = ts_node_end_byte(curr_node);
      const char* type = ts_node_type(curr_node);
      // TODO - if it's a string literal, take out any newlines and reflow the string to the
      // line-length
      const auto contents = source_code.substr(start, end - start);
      output += contents + " ";
    }

    if (ts_tree_cursor_goto_first_child(cursor)) {
      continue;
    }

    if (ts_tree_cursor_goto_next_sibling(cursor)) {
      continue;
    }

    while (true) {
      if (!ts_tree_cursor_goto_parent(cursor)) {
        if (output.at(output.length() - 1) == ' ') {
          output.pop_back();
        }
        return;
      }
      if (ts_tree_cursor_goto_next_sibling(cursor)) {
        break;
      }
    }
  }
}

int main(int argc, char** argv) {
  ArgumentGuard u8_guard(argc, argv);

  bool check = false;
  bool write_inplace = false;
  bool write_newfile = false;
  std::string file_path = "";
  std::string config_path = "";

  lg::initialize();

  // TODO - write a simple test framework for this stuff

  CLI::App app{"OpenGOAL Formatter"};
  app.add_flag("-c,--check", check,
               "If on, will just do a dry-run and fail if something isn't formatted correctly");
  app.add_flag("-w,--write", write_inplace,
               "Whether to write the formatted results directly back into the original file");
  app.add_flag("-n,--new", write_newfile,
               "Whether to write the formatted results into a new file in the same directory, "
               "useful for testing");
  app.add_option("-f,--file", file_path, "Input file path");
  app.add_option("--config", config_path, "Config file path");
  app.validate_positionals();
  CLI11_PARSE(app, argc, argv);

  // Create a parser.
  TSParser* parser = ts_parser_new();

  // Set the parser's language (JSON in this case).
  ts_parser_set_language(parser, tree_sitter_opengoal());

  // TODO - support recursing directories
  // Read in source code
  const auto source_code = file_util::read_text_file(file_path);

  // Build a syntax tree based on source code stored in a string.
  TSTree* tree = ts_parser_parse_string(parser, NULL, source_code.c_str(), source_code.length());

  // Get the root node of the syntax tree.
  TSNode root_node = ts_tree_root_node(tree);
  TSNode source_node = ts_node_named_child(root_node, 0);

  // Make a cursor, it's the fastest way to traverse the tree
  TSTreeCursor cursor = ts_tree_cursor_new(source_node);
  std::string output = "";
  lg::info(ts_node_string(source_node));
  walk_tree(&cursor, output, source_code);

  if (write_newfile) {
    // TODO - i don't like this implementation, return a new string instead
    if (str_util::replace(file_path, ".gc", ".new.gc")) {
      file_util::write_text_file(file_path, output);
    }
  }

  return 0;
}
