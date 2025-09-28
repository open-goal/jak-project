#include "decompilation_process.h"

#include <string>
#include <vector>

#include "config.h"

#include "common/log/log.h"
#include "common/util/Timer.h"
#include "common/util/os.h"

#include "Disasm/OpcodeInfo.h"
#include "ObjectFile/ObjectFileDB.h"
#include "data/streamed_audio.h"
#include "level_extractor/extract_level.h"

int run_decompilation_process(decompiler::Config config,
                              const fs::path& in_folder,
                              const fs::path& out_folder,
                              const bool minimal_for_extractor) {
  using namespace decompiler;
  Timer decomp_timer;

  lg::info("[Mem] Top of main: {} MB\n", get_peak_rss() / (1024 * 1024));

  init_opcode_info();

  lg::info("[Mem] After init: {} MB\n", get_peak_rss() / (1024 * 1024));

  std::vector<fs::path> dgos, objs, strs, tex_strs, art_strs;
  if (minimal_for_extractor) {
    // TODO - does this even matter, or can we just make the DGOs lazily loaded (does it already
    // happen?)
    for (const auto& dgo_name : config.dgo_names) {
      std::string common_name = "GAME.CGO";
      if (dgo_name.length() > 3 && dgo_name.substr(dgo_name.length() - 3) == "DGO") {
        // ends in DGO, it's a level
        dgos.push_back(in_folder / dgo_name);
      } else if (dgo_name.length() >= common_name.length() &&
                 dgo_name.substr(dgo_name.length() - common_name.length()) == common_name) {
        // it's COMMON.CGO, we need that too.
        dgos.push_back(in_folder / dgo_name);
      }
    }
  } else {
    for (const auto& dgo_name : config.dgo_names) {
      dgos.push_back(in_folder / dgo_name);
    }
  }

  if (minimal_for_extractor) {
    // TODO - does this even matter, or can we just make the DGOs lazily loaded (does it already
    // happen?)
    for (const auto& obj_name : config.object_file_names) {
      if (obj_name.length() > 3 && obj_name.substr(obj_name.length() - 3) == "TXT") {
        // ends in TXT
        objs.push_back(in_folder / obj_name);
      }
    }
  } else {
    for (const auto& obj_name : config.object_file_names) {
      objs.push_back(in_folder / obj_name);
    }
  }

  if (!minimal_for_extractor) {
    for (const auto& str_name : config.str_file_names) {
      strs.push_back(in_folder / str_name);
    }
  }

  for (const auto& str_name : config.str_texture_file_names) {
    tex_strs.push_back(in_folder / str_name);
  }

  for (const auto& str_name : config.str_art_file_names) {
    art_strs.push_back(in_folder / str_name);
  }

  lg::info("[Mem] After config read: {} MB", get_peak_rss() / (1024 * 1024));

  // build file database
  lg::info("Setting up object file DB...");
  ObjectFileDB db(dgos, fs::path(config.obj_file_name_map_file), objs, strs, tex_strs, art_strs,
                  config);

  // Explicitly fail if a file in the 'allowed_objects' list wasn't found in the DB
  // as this is another silent error that can be confusing
  if (!config.allowed_objects.empty()) {
    for (const auto& expected_obj : config.allowed_objects) {
      if (db.obj_files_by_name.count(expected_obj) == 0) {
        // TODO - this is wrong for jak1, fix eventually as this is now done in 3 places
        lg::error(
            "Expected to find '{}' in the ObjectFileDB but did not. Check "
            "./decompiler/config/{}/inputs.jsonc",
            expected_obj, config.game_name);
        return 1;
      }
    }
  }

  lg::info("[Mem] After DB setup: {} MB", get_peak_rss() / (1024 * 1024));

  // write out DGO file info
  file_util::create_dir_if_needed(out_folder);
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
  lg::info("[Mem] After link data: {} MB", get_peak_rss() / (1024 * 1024));
  db.find_code(config);
  db.process_labels();
  lg::info("[Mem] After code: {} MB", get_peak_rss() / (1024 * 1024));

  // top level decompile (do this before printing asm so we get function names)
  if (config.find_functions) {
    db.ir2_top_level_pass(config);
  }

  // print disassembly
  if (config.disassemble_code || config.disassemble_data) {
    db.write_disassembly(out_folder, config.disassemble_data, config.disassemble_code,
                         config.write_hex_near_instructions);
  }

  if (config.process_art_groups) {
    db.extract_art_info();
    // dump art info to json if requested
    if (config.dump_art_group_info) {
      auto ag_file_name = out_folder / "dump" / "art-group-info.min.json";
      nlohmann::json ag_json = db.dts.art_group_info;
      file_util::create_dir_if_needed_for_file(ag_file_name);
      file_util::write_text_file(ag_file_name, ag_json.dump(-1));
      lg::info("[DUMP] Dumped art group info to {}", ag_file_name.string());
    }
    if (config.dump_joint_geo_info) {
      auto jg_file_name = out_folder / "dump" / "joint-node-info.min.json";
      nlohmann::json jg_json = db.dts.jg_info;
      file_util::create_dir_if_needed_for_file(jg_file_name);
      file_util::write_text_file(jg_file_name, jg_json.dump(-1));
      lg::info("[DUMP] Dumped joint node info to {}", jg_file_name.string());
    }
  } else if (!config.art_group_info_dump.empty() || !config.jg_info_dump.empty()) {
    // process art groups (used in decompilation)
    // - if the config has a path to the art info dump, just use that
    // - otherwise (or if we want to dump it fresh) extract it
    if (!config.art_group_info_dump.empty()) {
      db.dts.art_group_info = config.art_group_info_dump;
    }
    if (!config.jg_info_dump.empty()) {
      db.dts.jg_info = config.jg_info_dump;
    }
  } else {
    lg::error("`process_art_groups` was false and no art-group-info dump was provided!");
    return 1;
  }

  if (config.process_tpages && !config.texture_info_dump.empty()) {
    db.dts.textures = config.texture_info_dump;
  }

  // main decompile.
  if (config.decompile_code) {
    db.analyze_functions_ir2(out_folder, config, {}, {}, {});
  }

  if (config.generate_all_types) {
    ASSERT_MSG(config.decompile_code, "Must decompile code to generate all-types");
    db.ir2_analyze_all_types(out_folder / "new-all-types.gc", config.old_all_types_file,
                             config.hacks.types_with_bad_inspect_methods);
  }

  lg::info("[Mem] After decomp: {} MB", get_peak_rss() / (1024 * 1024));

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

  // ensure asset dir exists
  file_util::create_dir_if_needed(out_folder / "assets");

  if (config.process_game_text) {
    auto result = db.process_game_text_files(config);
    if (!result.empty()) {
      file_util::write_text_file(out_folder / "assets" / "game_text.txt", result);
    }
  }

  lg::info("[Mem] After text: {} MB", get_peak_rss() / (1024 * 1024));

  if (config.process_subtitle_text || config.process_subtitle_images) {
    auto result = db.process_all_spool_subtitles(
        config, config.process_subtitle_images ? out_folder / "assets" / "subtitle-images" : "");
    if (!result.empty()) {
      file_util::write_text_file(out_folder / "assets" / "game_subs.txt", result);
    }
  }

  lg::info("[Mem] After spool handling: {} MB", get_peak_rss() / (1024 * 1024));

  TextureDB tex_db;
  if (config.process_tpages || config.levels_extract) {
    auto textures_out = out_folder / "textures";
    auto dump_out = out_folder / "import";
    file_util::create_dir_if_needed(textures_out);
    auto result = db.process_tpages(tex_db, textures_out, config, dump_out);
    if (!result.empty() && config.process_tpages) {
      file_util::write_text_file(textures_out / "tpage-dir.txt", result);
      file_util::write_text_file(textures_out / "tex-remap.txt",
                                 tex_db.generate_texture_dest_adjustment_table());
    }
    if (config.dump_tex_info) {
      auto texture_file_name = out_folder / "dump" / "tex-info.min.json";
      nlohmann::json texture_json = db.dts.textures;
      file_util::create_dir_if_needed_for_file(texture_file_name);
      file_util::write_text_file(texture_file_name, texture_json.dump(-1));
      lg::info("[DUMP] Dumped texture info to {}", texture_file_name.string());
    }
  }

  lg::info("[Mem] After textures: {} MB", get_peak_rss() / (1024 * 1024));

  // Merge textures before replacing them, in other words, replacements take priority
  auto texture_merge_path = file_util::get_jak_project_dir() / "game" / "assets" /
                            game_version_names[config.game_version] / "texture_merges";
  if (fs::exists(texture_merge_path)) {
    tex_db.merge_textures(texture_merge_path);
  }

  auto replacements_path = file_util::get_jak_project_dir() / "custom_assets" /
                           game_version_names[config.game_version] / "texture_replacements";
  if (fs::exists(replacements_path)) {
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
    extract_all_levels(db, tex_db, config.levels_to_extract, "GAME.CGO", config, level_out_path);
  }

  lg::info("[Mem] After extraction: {} MB", get_peak_rss() / (1024 * 1024));

  if (config.rip_streamed_audio) {
    auto streaming_audio_out = out_folder / "audio";
    file_util::create_dir_if_needed(streaming_audio_out);
    process_streamed_audio(config, streaming_audio_out, in_folder,
                           config.streamed_audio_file_names);
  }

  lg::info("Decompiler has finished successfully in {:.2f} seconds.", decomp_timer.getSeconds());
  return 0;
}
