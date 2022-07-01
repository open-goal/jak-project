#include <cstdio>
#include <string>
#include <vector>

#include "config.h"

#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/Timer.h"
#include "common/util/diff.h"
#include "common/util/os.h"
#include "common/versions.h"

#include "ObjectFile/ObjectFileDB.h"
#include "decompiler/data/TextureDB.h"
#include "decompiler/data/streamed_audio.h"
#include "decompiler/level_extractor/extract_level.h"

int main(int argc, char** argv) {
  Timer decomp_timer;

  fmt::print("[Mem] Top of main: {} MB\n", get_peak_rss() / (1024 * 1024));
  using namespace decompiler;
  if (!file_util::setup_project_path(std::nullopt)) {
    return 1;
  }
  lg::set_file(file_util::get_file_path({"log/decompiler.txt"}));
  lg::set_file_level(lg::level::info);
  lg::set_stdout_level(lg::level::info);
  lg::set_flush_level(lg::level::info);
  lg::initialize();

  init_opcode_info();

  if (argc < 4) {
    printf(
        "Usage: decompiler <config_file> <in_folder> <out_folder> "
        "[bool_flag_name=true/false...]\n");
    return 1;
  }
  fmt::print("[Mem] After init: {} MB\n", get_peak_rss() / (1024 * 1024));

  // collect all files to process
  Config config;
  try {
    // Allow overriding config boolean flags via CLI
    // There are very minimum guard-rails here
    //
    // "<key>=<override>"
    //
    // This allows us to run scripts that deviate from the defaults
    std::map<std::string, bool> overrides;
    if (argc > 4) {
      for (int i = 4; i < argc; i++) {
        std::string val = argv[i];
        if (val.find('=') == std::string::npos) {
          printf("Aborting - invalid flag override syntax\n");
          printf(
              "Usage: decompiler <config_file> <in_folder> <out_folder> "
              "[bool_flag_name=true/false...]\n");
          return 1;
        }
        auto pair = split_string(argv[i], '=');
        if (pair.size() > 2) {
          printf("Aborting - invalid flag override syntax, provide pairs!\n");
          printf(
              "Usage: decompiler <config_file> <in_folder> <out_folder> "
              "[bool_flag_name=true/false...]\n");
          return 1;
        }
        if (pair.at(1) != "true" && pair.at(1) != "false") {
          printf("Aborting - invalid flag override syntax, true|false only!\n");
          printf(
              "Usage: decompiler <config_file> <in_folder> <out_folder> "
              "[bool_flag_name=true/false...]\n");
          return 1;
        }
        overrides.insert({pair.at(0), pair.at(0) == "true"});
      }
    }

    config = read_config_file(argv[1], overrides);

  } catch (const std::exception& e) {
    lg::error("Failed to parse config: {}", e.what());
    return 1;
  }

  // std::string in_folder = file_util::combine_path(argv[2], config.game_name);
  std::filesystem::path in_folder = std::filesystem::path(argv[2]) / config.game_name;
  std::filesystem::path out_folder = std::filesystem::path(argv[3]) / config.game_name;

  // Verify the in_folder is correct
  if (!exists(in_folder)) {
    fmt::print("Aborting - 'in_folder' does not exist '{}'\n", in_folder.string());
    return 1;
  }

  // Warning message if expected ELF isn't found, user could be using bad assets / didn't extract
  // the ISO properly
  if (!config.expected_elf_name.empty() && !exists(in_folder / config.expected_elf_name)) {
    fmt::print(
        "WARNING - '{}' does not contain the expected ELF file '{}'.  Was the ISO extracted "
        "properly or is there a version mismatch?\n",
        in_folder.string(), config.expected_elf_name);
  }

  std::vector<std::filesystem::path> dgos, objs, strs;
  for (const auto& dgo_name : config.dgo_names) {
    dgos.push_back(in_folder / dgo_name);
  }

  for (const auto& obj_name : config.object_file_names) {
    objs.push_back(in_folder / obj_name);
  }

  for (const auto& str_name : config.str_file_names) {
    strs.push_back(in_folder / str_name);
  }

  file_util::create_dir_if_needed(out_folder);
  file_util::create_dir_if_needed(out_folder / "assets");

  if (config.rip_levels) {
    file_util::create_dir_if_needed(file_util::get_jak_project_dir() / "debug_out");
  }

  fmt::print("[Mem] After config read: {} MB\n", get_peak_rss() / (1024 * 1024));

  // build file database
  lg::info("Setting up object file DB...");
  ObjectFileDB db(dgos, std::filesystem::path(config.obj_file_name_map_file), objs, strs, config);

  fmt::print("[Mem] After DB setup: {} MB\n", get_peak_rss() / (1024 * 1024));

  // write out DGO file info
  file_util::write_text_file(out_folder / "dgo.txt", db.generate_dgo_listing());
  // write out object file map (used for future decompilations, if desired)
  file_util::write_text_file(out_folder / "obj.txt",
                             db.generate_obj_listing(config.merged_objects));

  // dump raw objs
  if (config.dump_objs) {
    auto path = out_folder / "raw_obj";
    file_util::create_dir_if_needed(path);
    db.dump_raw_objects(path);
  }

  // process files (required for all analysis)
  db.process_link_data(config);
  fmt::print("[Mem] After link data: {} MB\n", get_peak_rss() / (1024 * 1024));
  db.find_code(config);
  db.process_labels();
  fmt::print("[Mem] After code: {} MB\n", get_peak_rss() / (1024 * 1024));

  // top level decompile (do this before printing asm so we get function names)
  if (config.find_functions) {
    db.ir2_top_level_pass(config);
  }

  // print disassembly
  if (config.disassemble_code || config.disassemble_data) {
    db.write_disassembly(out_folder, config.disassemble_data, config.disassemble_code,
                         config.write_hex_near_instructions);
  }

  // process art groups (used in decompilation)
  if (config.decompile_code || config.process_art_groups) {
    db.extract_art_info();
  }

  // main decompile.
  if (config.decompile_code) {
    db.analyze_functions_ir2(out_folder, config, {});
  }

  if (config.generate_all_types) {
    ASSERT_MSG(config.decompile_code, "Must decompile code to generate all-types");
    db.ir2_analyze_all_types(out_folder / "new-all-types.gc", config.old_all_types_file,
                             config.hacks.types_with_bad_inspect_methods);
  }

  fmt::print("[Mem] After decomp: {} MB\n", get_peak_rss() / (1024 * 1024));

  // write out all symbols
  file_util::write_text_file(out_folder / "all-syms.gc", db.dts.dump_symbol_types());

  // write art groups
  if (config.process_art_groups) {
    db.dump_art_info(out_folder);
  }

  if (config.hexdump_code || config.hexdump_data) {
    db.write_object_file_words(out_folder, config.hexdump_data, config.hexdump_code);
  }

  // data stuff
  if (config.write_scripts) {
    db.find_and_write_scripts(out_folder);
  }

  if (config.process_game_text) {
    auto result = db.process_game_text_files(config);
    if (!result.empty()) {
      file_util::write_text_file(out_folder / "assets" / "game_text.txt", result);
    }
  }

  fmt::print("[Mem] After text: {} MB\n", get_peak_rss() / (1024 * 1024));

  decompiler::TextureDB tex_db;
  if (config.process_tpages || config.levels_extract) {
    auto textures_out = out_folder / "textures";
    file_util::create_dir_if_needed(textures_out);
    auto result = db.process_tpages(tex_db, textures_out);
    if (!result.empty() && config.process_tpages) {
      file_util::write_text_file(textures_out / "tpage-dir.txt", result);
    }
  }

  fmt::print("[Mem] After textures: {} MB\n", get_peak_rss() / (1024 * 1024));
  auto replacements_path = file_util::get_jak_project_dir() / "texture_replacements";
  if (std::filesystem::exists(replacements_path)) {
    tex_db.replace_textures(replacements_path);
  }

  if (config.process_game_count) {
    auto result = db.process_game_count_file();
    if (!result.empty()) {
      file_util::write_text_file(out_folder / "assets" / "game_count.txt", result);
    }
  }

  if (config.levels_extract) {
    auto level_out_path =
        file_util::get_jak_project_dir() / "out" / game_version_names[config.game_version] / "fr3";
    file_util::create_dir_if_needed(level_out_path);
    extract_all_levels(db, tex_db, config.levels_to_extract, "GAME.CGO", config.hacks,
                       config.rip_levels, config.extract_collision, level_out_path);
  }

  fmt::print("[Mem] After extraction: {} MB\n", get_peak_rss() / (1024 * 1024));

  if (!config.audio_dir_file_name.empty()) {
    auto streaming_audio_in = in_folder / "VAG";
    auto streaming_audio_out = out_folder / "assets" / "streaming_audio";
    file_util::create_dir_if_needed(streaming_audio_out);
    process_streamed_audio(streaming_audio_out, in_folder, config.streamed_audio_file_names);
  }

  lg::info("Decompiler has finished successfully in {:.2f} seconds.", decomp_timer.getSeconds());
  return 0;
}
