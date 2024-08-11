#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"

#include "goalc/build_actor/jak1/build_actor.h"

#include "third-party/CLI11.hpp"

int main(int argc, char** argv) {
  // logging
  lg::set_stdout_level(lg::level::info);
  lg::set_flush_level(lg::level::info);
  lg::initialize();

  // game version
  std::string game, mdl_name, output_file;
  bool gen_collide_mesh = false;
  fs::path project_path_override;

  // path
  if (!file_util::setup_project_path(std::nullopt)) {
    return 1;
  }

  lg::info("Build Actor Tool", versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR);

  CLI::App app{"OpenGOAL Compiler / REPL"};
  app.add_option("input-model", mdl_name,
                 "Input model file (for example: custom_assets/jak1/models/test.glb)")
      ->required();
  app.add_option("output-file", output_file,
                 "Output *-ag.go file (for example: out/jak1/obj/test-ag.go)")
      ->required();
  app.add_option("-g,--game", game, "Game version (only jak1 for now)")->required();
  app.add_option("--proj-path", project_path_override,
                 "Specify the location of the 'data/' folder");
  app.add_flag("-m,--mesh", gen_collide_mesh, "Whether to generate a collide-mesh for this model");
  app.validate_positionals();
  CLI11_PARSE(app, argc, argv)

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
      jak1::run_build_actor(mdl_name, output_file, gen_collide_mesh);
      break;
    default:
      ASSERT_NOT_REACHED_MSG("unsupported game version");
  }

  return 0;
}
