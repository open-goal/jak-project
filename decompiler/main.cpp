#include <cstdio>
#include <string>
#include <vector>
#include "ObjectFile/ObjectFileDB.h"
#include "common/log/log.h"
#include "config.h"
#include "common/util/FileUtil.h"
#include "common/versions.h"
#include "decompiler/data/streamed_audio.h"
#include "decompiler/level_extractor/extract_level.h"
#include "decompiler/data/TextureDB.h"
#include "common/util/os.h"
#include "common/util/diff.h"

int main(int argc, char** argv) {
  fmt::print("[Mem] Top of main: {} MB\n", get_peak_rss() / (1024 * 1024));
  using namespace decompiler;
  if (!file_util::setup_project_path()) {
    return 1;
  }
  lg::set_file(file_util::get_file_path({"log/decompiler.txt"}));
  lg::set_file_level(lg::level::info);
  lg::set_stdout_level(lg::level::info);
  lg::set_flush_level(lg::level::info);
  lg::initialize();
  lg::info("GOAL Decompiler version {}\n", versions::DECOMPILER_VERSION);

  file_util::init_crc();
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

  std::string in_folder = file_util::combine_path(argv[2], config.game_name);
  std::string out_folder = file_util::combine_path(argv[3], config.game_name);

  // Verify the in_folder is correct
  // TODO - refactor to use ghc::filesystem, cleanup file_util
  if (!file_util::file_exists(in_folder)) {
    fmt::print("Aborting - 'in_folder' does not exist '{}'\n", in_folder);
    return 1;
  }

  // Warning message if expected ELF isn't found, user could be using bad assets / didn't extract
  // the ISO properly
  if (!config.expected_elf_name.empty() &&
      !file_util::file_exists(file_util::combine_path(in_folder, config.expected_elf_name))) {
    fmt::print(
        "WARNING - '{}' does not contain the expected ELF file '{}'.  Was the ISO extracted "
        "properly or is there a version mismatch?\n",
        in_folder, config.expected_elf_name);
  }

  std::vector<std::string> dgos, objs, strs;
  for (const auto& dgo_name : config.dgo_names) {
    dgos.push_back(file_util::combine_path(in_folder, dgo_name));
  }

  for (const auto& obj_name : config.object_file_names) {
    objs.push_back(file_util::combine_path(in_folder, obj_name));
  }

  for (const auto& str_name : config.str_file_names) {
    strs.push_back(file_util::combine_path(in_folder, str_name));
  }

  file_util::create_dir_if_needed(out_folder);
  if (config.rip_levels) {
    file_util::create_dir_if_needed(file_util::get_file_path({"debug_out"}));
  }

  fmt::print("[Mem] After config read: {} MB\n", get_peak_rss() / (1024 * 1024));

  // build file database
  lg::info("Setting up object file DB...");
  ObjectFileDB db(dgos, config.obj_file_name_map_file, objs, strs, config);

  fmt::print("[Mem] After DB setup: {} MB\n", get_peak_rss() / (1024 * 1024));

  // write out DGO file info
  file_util::write_text_file(file_util::combine_path(out_folder, "dgo.txt"),
                             db.generate_dgo_listing());
  // write out object file map (used for future decompilations, if desired)
  file_util::write_text_file(file_util::combine_path(out_folder, "obj.txt"),
                             db.generate_obj_listing(config.merged_objects));

  // dump raw objs
  if (config.dump_objs) {
    auto path = file_util::combine_path(out_folder, "raw_obj");
    file_util::create_dir_if_needed(path);
    db.dump_raw_objects(path);
  }

  // process files (required for all analysis)
  db.process_link_data(config);
  fmt::print("[Mem] After link data: {} MB\n", get_peak_rss() / (1024 * 1024));
  db.find_code(config);
  db.process_labels();
  fmt::print("[Mem] After code: {} MB\n", get_peak_rss() / (1024 * 1024));

  // print disassembly
  if (config.disassemble_code || config.disassemble_data) {
    db.write_disassembly(out_folder, config.disassemble_data, config.disassemble_code,
                         config.write_hex_near_instructions);
  }

  // main decompile.
  if (config.decompile_code) {
    db.analyze_functions_ir2(out_folder, config, {});
  }

  fmt::print("[Mem] After decomp: {} MB\n", get_peak_rss() / (1024 * 1024));

  // write out all symbols
  file_util::write_text_file(file_util::combine_path(out_folder, "all-syms.gc"),
                             db.dts.dump_symbol_types());

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
      file_util::write_text_file(file_util::get_file_path({"assets", "game_text.txt"}), result);
    }
  }

  fmt::print("[Mem] After text: {} MB\n", get_peak_rss() / (1024 * 1024));

  decompiler::TextureDB tex_db;
  if (config.process_tpages || config.levels_extract) {
    auto result = db.process_tpages(tex_db);
    if (!result.empty() && config.process_tpages) {
      file_util::write_text_file(file_util::get_file_path({"assets", "tpage-dir.txt"}), result);
    }
  }

  fmt::print("[Mem] After textures: {} MB\n", get_peak_rss() / (1024 * 1024));
  // todo config
  auto replacements_path = file_util::get_file_path({"texture_replacements"});
  if (std::filesystem::exists(replacements_path)) {
    tex_db.replace_textures(replacements_path);
  }

  if (config.process_game_count) {
    auto result = db.process_game_count_file();
    if (!result.empty()) {
      file_util::write_text_file(file_util::get_file_path({"assets", "game_count.txt"}), result);
    }
  }

  if (config.levels_extract) {
    extract_common(db, tex_db, "GAME.CGO");
    for (auto& lev : config.levels_to_extract) {
      extract_from_level(db, tex_db, lev, config.hacks, config.rip_levels);
    }
  }

  fmt::print("[Mem] After extraction: {} MB\n", get_peak_rss() / (1024 * 1024));

  if (!config.audio_dir_file_name.empty()) {
    process_streamed_audio(config.audio_dir_file_name, config.streamed_audio_file_names);
  }

  lg::info("Disassembly has completed successfully.");
  return 0;
}
