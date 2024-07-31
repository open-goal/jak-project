#include <string>

#include "config.h"
#include "decompilation_process.h"

#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/set_util.h"
#include "common/util/term_util.h"
#include "common/util/unicode_util.h"

#include "third-party/CLI11.hpp"

int main(int argc, char** argv) {
  ArgumentGuard u8_guard(argc, argv);

  fs::path config_path;
  fs::path in_folder;
  fs::path out_folder;

  std::string config_game_version = "";
  std::string config_override = "{}";

  CLI::App app{"OpenGOAL Decompiler"};
  app.add_option("config-path", config_path,
                 "Path to the decompiler config .jsonc file. ie. "
                 "./decompiler/config/jak1/jak1_config.jsonc")
      ->required();
  app.add_option("in-folder", in_folder,
                 "The path containing the iso_data folders. ie. ./iso_data/. Assumes the "
                 "'gameName' from the config as a sub-directory")
      ->required();
  app.add_option("out-folder", out_folder,
                 "The path for where the decompiler should place it's outputs. Assumes the "
                 "'gameName' from the config as a sub-directory")
      ->required();

  app.add_option("--version", config_game_version,
                 "The name of the game version to update the config with, ie. ntsc_v2")
      ->required();
  app.add_option("--config-override", config_override,
                 "JSON provided will be merged with the specified config, use to override options");
  define_common_cli_arguments(app);
  app.validate_positionals();
  CLI11_PARSE(app, argc, argv);

  if (!file_util::setup_project_path(std::nullopt)) {
    lg::error("Unable to setup project path");
    return 1;
  }

  try {
    lg::set_file("decompiler");
    lg::set_file_level(lg::level::info);
    lg::set_stdout_level(lg::level::info);
    lg::set_flush_level(lg::level::info);
    lg::initialize();
    if (_cli_flag_disable_ansi) {
      lg::disable_ansi_colors();
    }
  } catch (const std::exception& e) {
    lg::error("Failed to setup logging: {}", e.what());
    return 1;
  }

  decompiler::Config config;
  try {
    config = decompiler::read_config_file(config_path, config_game_version, config_override);
  } catch (const std::exception& e) {
    lg::error("Failed to parse config: {}", e.what());
    return 1;
  }

  // these options imply read_spools
  config.read_spools |= config.process_subtitle_text || config.process_subtitle_images;

  // Check if any banned objects are also in the allowed objects list
  // if so, throw an error as this can be a confusing situation
  auto intersection = set_util::intersection(config.allowed_objects, config.banned_objects);
  if (!intersection.empty()) {
    lg::error("Aborting - There is an overlap between 'allowed_objects' and 'banned_objects'");
    return 1;
  }

  in_folder = in_folder / config.game_name;
  // Verify the in_folder is correct
  if (!exists(in_folder)) {
    lg::error("Aborting - 'in_folder' does not exist '{}'", in_folder.string());
    return 1;
  }

  out_folder = out_folder / config.game_name;
  file_util::create_dir_if_needed(out_folder);
  file_util::create_dir_if_needed(out_folder / "assets");

  // Warning message if expected ELF isn't found, user could be using bad assets / didn't extract
  // the ISO properly
  if (!config.expected_elf_name.empty() && !fs::exists(in_folder / config.expected_elf_name)) {
    lg::error(
        "WARNING - '{}' does not contain the expected ELF file '{}'.  Was the ISO extracted "
        "properly or is there a version mismatch?",
        in_folder.string(), config.expected_elf_name);
  }

  // -- Begin the Decompilation!
  return run_decompilation_process(config, in_folder, out_folder, false);
}
