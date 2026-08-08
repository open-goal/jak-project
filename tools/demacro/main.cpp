#include <iostream>
#include <string>

#include "common/demacro/demacro.h"
#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/term_util.h"
#include "common/util/unicode_util.h"

#include "third-party/CLI11.hpp"

int main(int argc, char** argv) {
  ArgumentGuard u8_guard(argc, argv);

  std::string input_path;
  std::string pattern_path;
  bool check = false;
  bool report = false;
  bool write = false;

  lg::initialize();

  CLI::App app{"Recognize expanded OpenGOAL macros using configurable LISP-form patterns"};
  app.add_option("-f,--file", input_path, "OpenGOAL .gc file to transform")->required();
  app.add_option("-p,--patterns", pattern_path, "Demacro JSONC pattern file")->required();
  app.add_flag("-w,--write", write, "Write the transformed source back to --file");
  app.add_flag("-c,--check", check,
               "Do not print source; return failure when the file contains recognized expansions");
  app.add_flag("-r,--report", report, "Report rewrite counts by rule");
  app.validate_positionals();
  define_common_cli_arguments(app);
  CLI11_PARSE(app, argc, argv);

  if (_cli_flag_disable_ansi) {
    lg::disable_ansi_colors();
  }
  if (write && check) {
    lg::error("--write and --check cannot be used together");
    return 2;
  }

  try {
    const auto source = file_util::read_text_file(input_path);
    const auto result = demacro::rewrite(source, fs::path(pattern_path));
    if (report) {
      for (const auto& stat : result.stats) {
        if (stat.rewrites) {
          lg::info("{}: {}", stat.name, stat.rewrites);
        }
      }
    }
    if (check) {
      if (result.rewrite_count() != 0) {
        lg::error("{} contains {} recognized macro expansion(s)", input_path,
                  result.rewrite_count());
        return 1;
      }
      return 0;
    }

    if (write) {
      if (result.source != source) {
        file_util::write_binary_file(input_path, result.source.data(), result.source.size());
      }
      lg::info("Rewrote {} macro expansion(s) in {}", result.rewrite_count(), input_path);
    } else {
      std::cout << result.source;
    }
    return 0;
  } catch (const std::exception& e) {
    lg::error("Demacro failed: {}", e.what());
    return 1;
  }
}
