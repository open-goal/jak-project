#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"

#include "jak1/build_actor.h"
#include "jak2/build_actor.h"
#include "jak3/build_actor.h"

#include "third-party/CLI11.hpp"

int main(int argc, char** argv) {
  // logging
  lg::set_stdout_level(lg::level::info);
  lg::set_flush_level(lg::level::info);
  lg::initialize();

  // game version
  std::string game, mdl_name, output_file;
  fs::path project_path_override;
  bool gen_collide_mesh = false;
  s8 texture_bucket = -1;
  s32 texture_level = -1;
  s32 joint_channel = -1;

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
  app.add_option("-g,--game", game, "Game version (jak1, jak2, jak3)")->required();
  app.add_option("--proj-path", project_path_override,
                 "Specify the location of the 'data/' folder");
  app.add_flag("-m,--mesh", gen_collide_mesh, "Whether to generate a collide-mesh for this model");
  app.add_option("--texture-bucket", texture_bucket, "Texture bucket to set for this model");
  app.add_option("--texture-level", texture_level, "Texture level to set for this model");
  app.add_option("--joint-channel", joint_channel,
                 "Amount of joint channels to set for this model");
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
    case GameVersion::Jak1: {
      jak1::BuildActorParams params;
      params.gen_collide_mesh = gen_collide_mesh;
      params.texture_bucket = texture_bucket;
      params.texture_level = texture_level;
      params.joint_channel = joint_channel;
      jak1::run_build_actor(mdl_name, output_file, params);
    } break;
    case GameVersion::Jak2: {
      jak2::BuildActorParams params;
      params.gen_collide_mesh = gen_collide_mesh;
      params.texture_bucket = texture_bucket;
      params.texture_level = texture_level;
      params.joint_channel = joint_channel;
      jak2::run_build_actor(mdl_name, output_file, params);
    } break;
    case GameVersion::Jak3: {
      jak3::BuildActorParams params;
      params.gen_collide_mesh = gen_collide_mesh;
      params.texture_bucket = texture_bucket;
      params.texture_level = texture_level;
      params.joint_channel = joint_channel;
      jak3::run_build_actor(mdl_name, output_file, params);
    } break;
    default:
      ASSERT_NOT_REACHED_MSG("unsupported game version");
  }

  return 0;
}
