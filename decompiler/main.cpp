#include <cstdio>
#include <string>
#include <vector>
#include "ObjectFile/ObjectFileDB.h"
#include "common/log/log.h"
#include "config.h"
#include "common/util/FileUtil.h"
#include "common/versions.h"

int main(int argc, char** argv) {
  using namespace decompiler;
  lg::set_file(file_util::get_file_path({"log/decompiler.txt"}));
  lg::set_file_level(lg::level::info);
  lg::set_stdout_level(lg::level::info);
  lg::set_flush_level(lg::level::info);
  lg::initialize();
  lg::info("GOAL Decompiler version {}\n", versions::DECOMPILER_VERSION);

  file_util::init_crc();
  init_opcode_info();

  if (argc != 4) {
    printf("Usage: decompiler <config_file> <in_folder> <out_folder>\n");
    return 1;
  }

  // collect all files to process
  set_config(argv[1]);
  std::string in_folder = argv[2];
  std::string out_folder = argv[3];

  std::vector<std::string> dgos, objs, strs;
  for (const auto& dgo_name : get_config().dgo_names) {
    dgos.push_back(file_util::combine_path(in_folder, dgo_name));
  }

  for (const auto& obj_name : get_config().object_file_names) {
    objs.push_back(file_util::combine_path(in_folder, obj_name));
  }

  for (const auto& str_name : get_config().str_file_names) {
    strs.push_back(file_util::combine_path(in_folder, str_name));
  }

  file_util::create_dir_if_needed(out_folder);

  // build file database
  lg::info("Setting up object file DB...");
  ObjectFileDB db(dgos, get_config().obj_file_name_map_file, objs, strs);
  file_util::write_text_file(file_util::combine_path(out_folder, "dgo.txt"),
                             db.generate_dgo_listing());
  file_util::write_text_file(file_util::combine_path(out_folder, "obj.txt"),
                             db.generate_obj_listing());

  if (get_config().dump_objs) {
    auto path = file_util::combine_path(out_folder, "raw_obj");
    file_util::create_dir_if_needed(path);
    db.dump_raw_objects(path);
  }

  // process files (basic)
  db.process_link_data();
  db.find_code();
  db.process_labels();

  // IR1 or IR2 function analysis
  if (get_config().run_ir2) {
    db.analyze_functions_ir2(out_folder);
  } else {
    if (get_config().analyze_functions) {
      db.analyze_functions_ir1();
    }

    if (get_config().write_disassembly) {
      db.write_disassembly(out_folder, get_config().disassemble_objects_without_functions,
                           get_config().write_func_json);
    }
  }

  // common IR1 and IR2 function stuff:
  file_util::write_text_file(file_util::combine_path(out_folder, "all-syms.gc"),
                             db.dts.dump_symbol_types());

  // data stuff
  if (get_config().write_scripts) {
    db.find_and_write_scripts(out_folder);
  }

  if (get_config().write_hexdump) {
    db.write_object_file_words(out_folder, get_config().write_hexdump_on_v3_only);
  }

  if (get_config().process_game_text) {
    auto result = db.process_game_text_files();
    file_util::write_text_file(file_util::get_file_path({"assets", "game_text.txt"}), result);
  }

  if (get_config().process_tpages) {
    db.process_tpages();
  }

  if (get_config().process_game_count) {
    auto result = db.process_game_count_file();
    file_util::write_text_file(file_util::get_file_path({"assets", "game_count.txt"}), result);
  }

  // todo print type summary
  // printf("%s\n", get_type_info().get_summary().c_str());

  lg::info("Disassembly has completed successfully.");
  return 0;
}
