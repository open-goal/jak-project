#include "third-party/fmt/core.h"
#include "common/util/FileUtil.h"
#include "decompiler/Disasm/OpcodeInfo.h"
#include "decompiler/ObjectFile/ObjectFileDB.h"
#include "decompiler/level_extractor/extract_level.h"
#include "decompiler/config.h"
#include "goalc/compiler/Compiler.h"

void setup_global_decompiler_stuff() {
  file_util::init_crc();
  decompiler::init_opcode_info();
  file_util::setup_project_path();
}

int main(int argc, char** argv) {
  using namespace decompiler;
  fmt::print("OpenGOAL Level Extraction Tool\n");
  if (argc != 2) {
    fmt::print(" usage: extractor <path-to-jak1-files>\n");
    return 1;
  }

  // todo: print revision here.
  setup_global_decompiler_stuff();

  std::filesystem::path jak1_input_files(argv[1]);
  // make sure the input looks right
  if (!std::filesystem::exists(jak1_input_files)) {
    fmt::print("Error: input folder {} does not exist\n", jak1_input_files.string());
    return 1;
  }

  if (!std::filesystem::is_directory(jak1_input_files)) {
    fmt::print("Error: input folder {} is not a folder.\n", jak1_input_files.string());
    return 1;
  }

  if (!std::filesystem::exists(jak1_input_files / "DGO")) {
    fmt::print("Error: input folder doesn't have a DGO folder. Is this the right input?\n");
    return 1;
  }

  Config config = read_config_file(
      (file_util::get_jak_project_dir() / "decompiler" / "config" / "jak1_ntsc_black_label.jsonc")
          .string(),
      {});

  std::vector<std::string> dgos, objs;

  // grab all DGOS we need (level + common)
  for (const auto& dgo_name : config.dgo_names) {
    std::string common_name = "GAME.CGO";
    if (dgo_name.length() > 3 && dgo_name.substr(dgo_name.length() - 3) == "DGO") {
      // ends in DGO, it's a level
      dgos.push_back((jak1_input_files / dgo_name).string());
    } else if (dgo_name.length() >= common_name.length() &&
               dgo_name.substr(dgo_name.length() - common_name.length()) == common_name) {
      // it's COMMON.CGO, we need that too.
      dgos.push_back((jak1_input_files / dgo_name).string());
    }
  }

  // grab all the object files we need (just text)
  for (const auto& obj_name : config.object_file_names) {
    if (obj_name.length() > 3 && obj_name.substr(obj_name.length() - 3) == "TXT") {
      // ends in DGO, it's a level
      objs.push_back((jak1_input_files / obj_name).string());
    }
  }

  // set up objects
  ObjectFileDB db(dgos, config.obj_file_name_map_file, objs, {}, config);

  // save object files
  auto out_folder = (file_util::get_jak_project_dir() / "decompiler_out" / "jak1").string();
  auto raw_obj_folder = file_util::combine_path(out_folder, "raw_obj");
  file_util::create_dir_if_needed(raw_obj_folder);
  db.dump_raw_objects(raw_obj_folder);

  // analyze object file link data
  db.process_link_data(config);
  db.find_code(config);
  db.process_labels();

  // text files
  {
    auto result = db.process_game_text_files(config);
    if (!result.empty()) {
      file_util::write_text_file(file_util::get_file_path({"assets", "game_text.txt"}), result);
    }
  }

  // textures
  decompiler::TextureDB tex_db;
  file_util::write_text_file(file_util::get_file_path({"assets", "tpage-dir.txt"}),
                             db.process_tpages(tex_db));
  // texture replacements
  auto replacements_path = file_util::get_file_path({"texture_replacements"});
  if (std::filesystem::exists(replacements_path)) {
    tex_db.replace_textures(replacements_path);
  }

  // game count
  {
    auto result = db.process_game_count_file();
    if (!result.empty()) {
      file_util::write_text_file(file_util::get_file_path({"assets", "game_count.txt"}), result);
    }
  }

  // levels
  {
    extract_common(db, tex_db, "GAME.CGO");
    for (auto& lev : config.levels_to_extract) {
      extract_from_level(db, tex_db, lev, config.hacks, config.rip_levels);
    }
  }

  // Compile!
  Compiler compiler;
  compiler.make_system().set_constant("*iso-data*", absolute(jak1_input_files).string());
  compiler.make_system().set_constant("*use-iso-data-path*", true);

  compiler.make_system().load_project_file(
      (file_util::get_jak_project_dir() / "goal_src" / "game.gp").string());
  compiler.run_front_end_on_string("(mi)");

  system((file_util::get_jak_project_dir() / "../gk").string().c_str());
}