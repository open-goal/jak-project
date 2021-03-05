#pragma once

/*!
 * @file ObjectFileDB.h
 * A "database" of object files found in DGO files.
 * Eliminates duplicate object files, and also assigns unique names to all object files
 * (there may be different object files with the same name sometimes)
 */

#ifndef JAK2_DISASSEMBLER_OBJECTFILEDB_H
#define JAK2_DISASSEMBLER_OBJECTFILEDB_H

#include <cassert>
#include <string>
#include <unordered_map>
#include <vector>
#include "LinkedObjectFile.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "common/common_types.h"

namespace decompiler {
/*!
 * A "record" which can be used to identify an object file.
 */
struct ObjectFileRecord {
  std::string name;  // including -ag, not including dgo suffix
  int version = -1;
  uint32_t hash = 0;
};

/*!
 * All of the data for a single object file
 */
struct ObjectFileData {
  std::vector<uint8_t> data;     // raw bytes
  LinkedObjectFile linked_data;  // data including linking annotations
  ObjectFileRecord record;       // name
  std::vector<std::string> dgo_names;
  int obj_version = -1;
  bool has_multiple_versions = false;
  std::string name_in_dgo;
  std::string name_from_map;
  std::string to_unique_name() const;
  uint32_t reference_count = 0;  // number of times its used.
};

class ObjectFileDB {
 public:
  ObjectFileDB(const std::vector<std::string>& _dgos,
               const std::string& obj_file_name_map_file,
               const std::vector<std::string>& object_files,
               const std::vector<std::string>& str_files);
  std::string generate_dgo_listing();
  std::string generate_obj_listing();
  void process_link_data();
  void process_labels();
  void find_code();
  void find_and_write_scripts(const std::string& output_dir);
  void dump_raw_objects(const std::string& output_dir);

  void write_object_file_words(const std::string& output_dir, bool dump_v3_only);
  void write_disassembly(const std::string& output_dir,
                         bool disassemble_objects_without_functions,
                         bool write_json,
                         const std::string& file_suffix = "");

  void analyze_functions_ir1();
  void analyze_functions_ir2(const std::string& output_dir);
  void ir2_top_level_pass();
  void ir2_basic_block_pass();
  void ir2_atomic_op_pass();
  void ir2_type_analysis_pass();
  void ir2_register_usage_pass();
  void ir2_variable_pass();
  void ir2_cfg_build_pass();
  void ir2_store_current_forms();
  void ir2_build_expressions();
  void ir2_insert_lets();
  void ir2_write_results(const std::string& output_dir);
  std::string ir2_to_file(ObjectFileData& data);
  std::string ir2_function_to_string(ObjectFileData& data, Function& function, int seg);
  std::string ir2_final_out(ObjectFileData& data,
                            const std::unordered_set<std::string>& skip_functions = {});

  void process_tpages();
  std::string process_game_count_file();
  std::string process_game_text_files();

  ObjectFileData& lookup_record(const ObjectFileRecord& rec);
  DecompilerTypeSystem dts;
  std::string all_type_defs;

  bool lookup_function_type(const FunctionName& name,
                            const std::string& obj_name,
                            TypeSpec* result);

 public:
  void load_map_file(const std::string& map_data);
  void get_objs_from_dgo(const std::string& filename);
  void add_obj_from_dgo(const std::string& obj_name,
                        const std::string& name_in_dgo,
                        const uint8_t* obj_data,
                        uint32_t obj_size,
                        const std::string& dgo_name);

  /*!
   * Apply f to all ObjectFileData's. Does it in the right order.
   */
  template <typename Func>
  void for_each_obj(Func f) {
    assert(obj_files_by_name.size() == obj_file_order.size());
    for (const auto& name : obj_file_order) {
      for (auto& obj : obj_files_by_name.at(name)) {
        f(obj);
      }
    }
  }

  /*!
   * Apply f to all functions
   * takes (Function, segment, linked_data)
   * Does it in the right order.
   */
  template <typename Func>
  void for_each_function(Func f) {
    for_each_obj([&](ObjectFileData& data) {
      //      printf("IN %s\n", data.record.to_unique_name().c_str());
      for (int i = 0; i < int(data.linked_data.segments); i++) {
        //        printf("seg %d\n", i);
        int fn = 0;
        for (auto& goal_func : data.linked_data.functions_by_seg.at(i)) {
          //          printf("fn %d\n", fn);
          f(goal_func, i, data);
          fn++;
        }
      }
    });
  }

  template <typename Func>
  void for_each_function_def_order(Func f) {
    for_each_obj([&](ObjectFileData& data) {
      //      printf("IN %s\n", data.record.to_unique_name().c_str());
      for (int i = 0; i < int(data.linked_data.segments); i++) {
        //        printf("seg %d\n", i);
        int fn = 0;
        //        for (auto& goal_func : data.linked_data.functions_by_seg.at(i)) {
        for (size_t j = data.linked_data.functions_by_seg.at(i).size(); j-- > 0;) {
          //          printf("fn %d\n", fn);
          f(data.linked_data.functions_by_seg.at(i).at(j), i, data);
          fn++;
        }
      }
    });
  }

  // Danger: after adding all object files, we assume that the vector never reallocates.
  std::unordered_map<std::string, std::vector<ObjectFileData>> obj_files_by_name;
  std::unordered_map<std::string, std::vector<ObjectFileRecord>> obj_files_by_dgo;

  std::vector<std::string> obj_file_order;
  std::unordered_map<std::string, std::unordered_map<std::string, std::string>> dgo_obj_name_map;

  struct {
    uint32_t total_dgo_bytes = 0;
    uint32_t total_obj_files = 0;
    uint32_t unique_obj_files = 0;
    uint32_t unique_obj_bytes = 0;
  } stats;
};
}  // namespace decompiler

#endif  // JAK2_DISASSEMBLER_OBJECTFILEDB_H
