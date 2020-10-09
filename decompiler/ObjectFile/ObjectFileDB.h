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

/*!
 * A "record" which can be used to identify an object file.
 */
struct ObjectFileRecord {
  std::string name;
  int version = -1;
  uint32_t hash = 0;
  std::string to_unique_name() const;
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
  std::string to_unique_name() const;
  uint32_t reference_count = 0;  // number of times its used.
};

class ObjectFileDB {
 public:
  ObjectFileDB(const std::vector<std::string>& _dgos);
  std::string generate_dgo_listing();
  std::string generate_obj_listing();
  void process_link_data();
  void process_labels();
  void find_code();
  void find_and_write_scripts(const std::string& output_dir);

  void write_object_file_words(const std::string& output_dir, bool dump_v3_only);
  void write_disassembly(const std::string& output_dir, bool disassemble_objects_without_functions);
  void analyze_functions();
  ObjectFileData& lookup_record(ObjectFileRecord rec);
  DecompilerTypeSystem dts;

 private:
  void get_objs_from_dgo(const std::string& filename);
  void add_obj_from_dgo(const std::string& obj_name,
                        const std::string& name_in_dgo,
                        uint8_t* obj_data,
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

  // Danger: after adding all object files, we assume that the vector never reallocates.
  std::unordered_map<std::string, std::vector<ObjectFileData>> obj_files_by_name;
  std::unordered_map<std::string, std::vector<ObjectFileRecord>> obj_files_by_dgo;

  std::vector<std::string> obj_file_order;

  struct {
    uint32_t total_dgo_bytes = 0;
    uint32_t total_obj_files = 0;
    uint32_t unique_obj_files = 0;
    uint32_t unique_obj_bytes = 0;
  } stats;
};

#endif  // JAK2_DISASSEMBLER_OBJECTFILEDB_H
