// Iterates through the `all-types` DTS to find types that meet a variety of criteria, such as:
// - type size
// - field types at given offsets
// - parent-types
// - ...

#include <queue>

#include "common/formatter/formatter.h"
#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/json_util.h"
#include "common/util/string_util.h"
#include "common/util/term_util.h"
#include "common/util/unicode_util.h"

#include "decompiler/util/DecompilerTypeSystem.h"
#include "tree_sitter/api.h"

#include "fmt/core.h"
#include "third-party/CLI11.hpp"
#include "third-party/json.hpp"

int main(int argc, char** argv) {
  ArgumentGuard u8_guard(argc, argv);

  bool check = false;
  bool write_inplace = false;
  bool write_newfile = false;
  std::string file_path = "";
  std::string config_path = "";

  lg::initialize();

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
  define_common_cli_arguments(app);
  CLI11_PARSE(app, argc, argv);

  if (_cli_flag_disable_ansi) {
    lg::disable_ansi_colors();
  }

  // TODO - support recursing directories
  // Read in source code
  const auto source_code = file_util::read_text_file(file_path);
  const auto result = formatter::format_code(source_code);

  if (result) {
    // TODO - i don't like this implementation, return a new string instead
    if (write_inplace) {
      file_util::write_text_file(file_path, result.value());
    } else if (write_newfile) {
      if (str_util::replace(file_path, ".gc", ".new.gc")) {
        file_util::write_text_file(file_path, result.value());
      }
    }
    return 0;
  } else {
    lg::error("Could not format file");
    return 1;
  }
}
