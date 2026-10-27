#include "decompiler_setup.h"

#include "common/link_types.h"

#include "decompiler/Disasm/OpcodeInfo.h"
#include "decompiler/analysis/find_defpartgroup.h"
#include "decompiler/analysis/find_defstates.h"
#include "decompiler/analysis/find_skelgroups.h"

#include "fmt/format.h"

using namespace decompiler;

std::unique_ptr<ObjectFileDB> load_object_file_db(Config& config, const fs::path& game_folder) {
  init_opcode_info();

  std::vector<fs::path> dgos, objs;
  for (const auto& dgo_name : config.dgo_names) {
    dgos.push_back(game_folder / dgo_name);
  }
  for (const auto& obj_name : config.object_file_names) {
    objs.push_back(game_folder / obj_name);
  }

  auto db = std::make_unique<ObjectFileDB>(dgos, fs::path(config.obj_file_name_map_file), objs,
                                           std::vector<fs::path>{}, std::vector<fs::path>{},
                                           std::vector<fs::path>{}, config);
  db->process_link_data(config);
  db->find_code(config);
  db->process_labels();
  db->ir2_top_level_pass(config);

  if (config.process_art_groups) {
    db->extract_art_info();
  } else {
    if (!config.art_group_info_dump.empty()) {
      db->dts.art_group_info = config.art_group_info_dump;
    }
    if (!config.jg_info_dump.empty()) {
      db->dts.jg_info = config.jg_info_dump;
    }
  }
  if (config.process_part_group_table && !config.part_group_table.empty()) {
    db->dts.part_group_table = config.part_group_table;
  }
  if (config.process_tpages && !config.texture_info_dump.empty()) {
    db->dts.textures = config.texture_info_dump;
  }
  return db;
}

void run_object_type_analysis(ObjectFileDB& db, Config& config, ObjectFileData& data) {
  db.ir2_do_segment_analysis_phase1(TOP_LEVEL_SEGMENT, config, data);
  db.ir2_do_segment_analysis_phase1(DEBUG_SEGMENT, config, data);
  db.ir2_do_segment_analysis_phase1(MAIN_SEGMENT, config, data);
  db.ir2_setup_labels(config, data);
  db.ir2_do_segment_analysis_phase2(TOP_LEVEL_SEGMENT, config, data);
  if (data.linked_data.functions_by_seg.size() == 3) {
    auto& top_level = data.linked_data.functions_by_seg.at(TOP_LEVEL_SEGMENT).front();
    try {
      run_defpartgroup(top_level, config.part_group_table);
      run_defstate(top_level);
      run_defskelgroups(top_level);
    } catch (const std::exception&) {
    }
  }
  db.ir2_type_analysis_pass(DEBUG_SEGMENT, config, data);
  db.ir2_type_analysis_pass(MAIN_SEGMENT, config, data);
}

bool type_exists(const TypeSystem& ts, const std::string& name) {
  return ts.fully_defined_type_exists(name) || ts.partially_defined_type_exists(name);
}

std::string closest_label(const LinkedObjectFile& file, const Function& func, int seg, int idx) {
  int byte_offset = (func.start_word + idx) * 4;
  const DecompilerLabel* best = nullptr;
  for (const auto& label : file.labels) {
    if (label.target_segment == seg && label.offset >= func.start_word * 4 &&
        label.offset <= byte_offset && (!best || label.offset > best->offset)) {
      best = &label;
    }
  }
  if (!best) {
    return "";
  }
  int delta = (byte_offset - best->offset) / 4;
  return delta ? fmt::format("{}+{}", best->name, delta) : best->name;
}
