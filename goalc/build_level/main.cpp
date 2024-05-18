#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"
#include "common/versions/versions.h"

#include "goalc/build_level/jak1/build_level.h"
#include "goalc/build_level/jak2/build_level.h"
#include "goalc/build_level/jak3/build_level.h"

#include "third-party/CLI11.hpp"

// debug tool to run only build_level.
int main(int argc, char** argv) {
  // logging
  lg::set_stdout_level(lg::level::info);
  lg::set_flush_level(lg::level::info);
  lg::initialize();

  // game version
  std::string game, input_json, output_file;
  fs::path project_path_override;

  // path
  if (!file_util::setup_project_path(std::nullopt)) {
    return 1;
  }

  lg::info("Build Level Tool", versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR);

  CLI::App app{"OpenGOAL Compiler / REPL"};
  app.add_option(
         "input-json", input_json,
         "Input JSON file (for example, custom_assets/jak2/levels/test-zone/test-zone.jsonc)")
      ->required();
  app.add_option("output-file", output_file,
                 "Output .go file, (for example out/jak2/obj/test-zone.go)")
      ->required();
  app.add_option("-g,--game", game, "Game version (jak1, jak2 or jak3)")->required();
  app.add_option("--proj-path", project_path_override,
                 "Specify the location of the 'data/' folder");
  app.validate_positionals();
  CLI11_PARSE(app, argc, argv);

  GameVersion game_version = game_name_to_version(game);

  if (!project_path_override.empty()) {
    if (!fs::exists(project_path_override)) {
      lg::error("Error: project path override '{}' does not exist", project_path_override.string());
      return 1;
    }
    if (!file_util::setup_project_path(project_path_override)) {
      lg::error("Could not setup project path!");
      return 1;
    }
  } else if (!file_util::setup_project_path(std::nullopt)) {
    return 1;
  }

  switch (game_version) {
    case GameVersion::Jak1:
      jak1::run_build_level(input_json, output_file, "jak1/");
      break;
    case GameVersion::Jak2:
      jak2::run_build_level(input_json, output_file, "jak2/");
      break;
    case GameVersion::Jak3:
      jak3::run_build_level(input_json, output_file, "jak3/");
      break;
    default:
      ASSERT_NOT_REACHED_MSG("unsupported game version");
  }

  return 0;
}
