#pragma once

/*!
 * @file ObjectFileDB.h
 * A "database" of object files found in DGO files.
 * Eliminates duplicate object files, and also assigns unique names to all object files
 * (there may be different object files with the same name sometimes)
 */

#include <string>
#include <unordered_map>
#include <vector>

#include "LinkedObjectFile.h"

#include "common/common_types.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"

#include "decompiler/analysis/symbol_def_map.h"
#include "decompiler/data/TextureDB.h"
#include "decompiler/util/DecompilerTypeSystem.h"

#include "third-party/fmt/core.h"

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
  ObjectFileData(GameVersion version) : linked_data(version) {}
  std::vector<uint8_t> data;     // raw bytes
  LinkedObjectFile linked_data;  // data including linking annotations
  ObjectFileRecord record;       // name
  std::vector<std::string> dgo_names;
  int obj_version = -1;
  bool has_multiple_versions = false;
  std::string name_in_dgo;
  std::string name_from_map;
  std::string to_unique_name() const;
  std::string base_name_from_chunk;
  uint32_t reference_count = 0;  // number of times its used.

  std::string full_output;
  std::string output_with_skips;
};

/*!
 * Stats structure for let rewriting.
 */
struct LetRewriteStats {
  int dotimes = 0;
  int countdown = 0;
  int abs = 0;
  int abs2 = 0;
  int unused = 0;
  int ja = 0;
  int case_no_else = 0;
  int case_with_else = 0;
  int set_vector = 0;
  int set_vector2 = 0;
  int set_vector3 = 0;
  int send_event = 0;
  int font_context_meth = 0;
  int proc_new = 0;
  int attack_info = 0;
  int vector_dot = 0;
  int rand_float_gen = 0;
  int set_let = 0;
  int with_dma_buf_add_bucket = 0;
  int dma_buffer_add_gs_set = 0;

  int total() const {
    return dotimes + countdown + abs + abs2 + unused + ja + case_no_else + case_with_else +
           set_vector + set_vector2 + send_event + font_context_meth + proc_new + attack_info +
           vector_dot + rand_float_gen + set_let + with_dma_buf_add_bucket + dma_buffer_add_gs_set;
  }

  std::string print() const {
    std::string out;
    out += fmt::format("LET REWRITE STATS: {} total\n", total());
    out += fmt::format("  dotimes: {}\n", dotimes);
    out += fmt::format("  countdown: {}\n", countdown);
    out += fmt::format("  abs: {}\n", abs);
    out += fmt::format("  abs2: {}\n", abs2);
    out += fmt::format("  ja: {}\n", ja);
    out += fmt::format("  set_vector: {}\n", set_vector);
    out += fmt::format("  set_vector2: {}\n", set_vector2);
    out += fmt::format("  set_vector3: {}\n", set_vector3);
    out += fmt::format("  case_no_else: {}\n", case_no_else);
    out += fmt::format("  case_with_else: {}\n", case_with_else);
    out += fmt::format("  unused: {}\n", unused);
    out += fmt::format("  send_event: {}\n", send_event);
    // out += fmt::format("  font_context_meth: {}\n", font_context_meth);
    out += fmt::format("  proc_new: {}\n", proc_new);
    out += fmt::format("  attack_info: {}\n", attack_info);
    out += fmt::format("  vector_dot: {}\n", vector_dot);
    out += fmt::format("  rand_float_gen: {}\n", rand_float_gen);
    out += fmt::format("  set_let: {}\n", set_let);
    out += fmt::format("  with_dma_buf_add_bucket: {}\n", with_dma_buf_add_bucket);
    out += fmt::format("  dma_buffer_add_gs_set: {}\n", dma_buffer_add_gs_set);
    return out;
  }

  LetRewriteStats operator+(const LetRewriteStats& other) {
    LetRewriteStats result;
    result.dotimes = dotimes + other.dotimes;
    result.countdown = countdown + other.countdown;
    result.abs = abs + other.abs;
    result.abs2 = abs2 + other.abs2;
    result.ja = ja + other.ja;
    result.set_vector = set_vector + other.set_vector;
    result.set_vector2 = set_vector2 + other.set_vector2;
    result.case_no_else = case_no_else + other.case_no_else;
    result.case_with_else = case_with_else + other.case_with_else;
    result.unused = unused + other.unused;
    result.send_event = send_event + other.send_event;
    result.font_context_meth = font_context_meth + other.font_context_meth;
    result.proc_new = proc_new + other.proc_new;
    result.attack_info = attack_info + other.attack_info;
    result.vector_dot = vector_dot + other.vector_dot;
    result.rand_float_gen = rand_float_gen + other.rand_float_gen;
    result.set_let = rand_float_gen + other.set_let;
    result.with_dma_buf_add_bucket = rand_float_gen + other.with_dma_buf_add_bucket;
    return result;
  }

  LetRewriteStats& operator+=(const LetRewriteStats& other) {
    dotimes += other.dotimes;
    countdown += other.countdown;
    abs += other.abs;
    abs2 += other.abs2;
    ja += other.ja;
    set_vector += other.set_vector;
    set_vector2 += other.set_vector2;
    case_no_else += other.case_no_else;
    case_with_else += other.case_with_else;
    unused += other.unused;
    send_event += other.send_event;
    font_context_meth += other.font_context_meth;
    proc_new += other.proc_new;
    attack_info += other.attack_info;
    vector_dot += other.vector_dot;
    rand_float_gen += other.rand_float_gen;
    set_let += other.set_let;
    with_dma_buf_add_bucket += other.with_dma_buf_add_bucket;
    return *this;
  }
};

class ObjectFileDB {
 public:
  ObjectFileDB(const std::vector<fs::path>& _dgos,
               const fs::path& obj_file_name_map_file,
               const std::vector<fs::path>& object_files,
               const std::vector<fs::path>& str_files,
               const std::vector<fs::path>& str_tex_files,
               const Config& config);
  std::string generate_dgo_listing();
  std::string generate_obj_listing(const std::unordered_set<std::string>& merged_objs);
  void process_link_data(const Config& config);
  void process_labels();
  void find_code(const Config& config);
  void find_and_write_scripts(const fs::path& output_dir);
  void extract_art_info();
  void dump_art_info(const fs::path& output_dir);
  void dump_raw_objects(const fs::path& output_dir);
  void write_object_file_words(const fs::path& output_dir, bool dump_data, bool dump_code);
  void write_disassembly(const fs::path& output_dir,
                         bool disassemble_data,
                         bool disassemble_code,
                         bool print_hex);

  void process_object_file_data(
      ObjectFileData& data,
      const fs::path& output_dir,
      const Config& config,
      const std::unordered_set<std::string>& skip_functions,
      const std::unordered_map<std::string, std::unordered_set<std::string>>& skip_states);
  void analyze_functions_ir2(
      const fs::path& output_dir,
      const Config& config,
      const std::optional<std::function<void(std::string)>> prefile_callback,
      const std::optional<std::function<void()>> postfile_callback,
      const std::unordered_set<std::string>& skip_functions,
      const std::unordered_map<std::string, std::unordered_set<std::string>>& skip_states = {});
  void ir2_top_level_pass(const Config& config);
  void ir2_stack_spill_slot_pass(int seg, ObjectFileData& data);
  void ir2_basic_block_pass(int seg, const Config& config, ObjectFileData& data);
  void ir2_atomic_op_pass(int seg, const Config& config, ObjectFileData& data);
  void ir2_type_analysis_pass(int seg, const Config& config, ObjectFileData& data);
  void ir2_register_usage_pass(int seg, ObjectFileData& data);
  void ir2_variable_pass(int seg, ObjectFileData& data);
  void ir2_cfg_build_pass(int seg, ObjectFileData& data);
  // void ir2_store_current_forms(int seg);
  void ir2_build_expressions(int seg, const Config& config, ObjectFileData& data);
  void ir2_insert_lets(int seg, ObjectFileData& data);
  void ir2_add_store_errors(int seg, ObjectFileData& data);
  void ir2_rewrite_inline_asm_instructions(int seg, ObjectFileData& data);
  void ir2_insert_anonymous_functions(int seg, ObjectFileData& data);
  void ir2_symbol_definition_map(ObjectFileData& data);
  void ir2_write_results(const fs::path& output_dir,
                         const Config& config,
                         const std::vector<std::string>& imports,
                         ObjectFileData& data);
  void ir2_do_segment_analysis_phase1(int seg, const Config& config, ObjectFileData& data);
  void ir2_do_segment_analysis_phase2(int seg, const Config& config, ObjectFileData& data);
  void ir2_setup_labels(const Config& config, ObjectFileData& data);
  void ir2_run_mips2c(const Config& config, ObjectFileData& data);
  struct PerObjectAllTypeInfo {
    std::string object_name;
    std::unordered_set<std::string> already_seen_symbols;

    // type-name : { method id : state name }
    std::unordered_map<std::string, std::unordered_map<int, std::string>> state_methods;
    // symbol-name : type-name
    std::unordered_map<std::string, std::string> symbol_types;

    struct TypeInfo {
      bool from_inspect_method = false;  // does this come from an inspect method?
      // if from inspect method:
      std::string type_definition;  // the deftype generated from the inspect method.
      // if not from inspect method:
      u32 flags = 0;
      std::string parent;
    };

    std::vector<std::string> type_names_in_order;
    std::unordered_map<std::string, TypeInfo> type_info;

    std::string symbol_defs;
  };
  void ir2_analyze_all_types(const fs::path& output_file,
                             const std::optional<std::string>& previous_game_types,
                             const std::unordered_set<std::string>& bad_types);
  std::string ir2_to_file(ObjectFileData& data, const Config& config);
  std::string ir2_function_to_string(ObjectFileData& data, Function& function, int seg);
  std::string ir2_final_out(ObjectFileData& data,
                            const std::vector<std::string>& imports,
                            const std::unordered_set<std::string>& skip_functions);

  std::string process_tpages(TextureDB& tex_db, const fs::path& output_path, const Config& cfg);
  std::string process_game_count_file();
  std::string process_game_text_files(const Config& cfg);
  std::string process_all_spool_subtitles(const Config& cfg, const fs::path& image_out);

  const ObjectFileData& lookup_record(const ObjectFileRecord& rec) const;
  DecompilerTypeSystem dts;

  bool lookup_function_type(const FunctionName& name,
                            const std::string& obj_name,
                            const Config& config,
                            TypeSpec* result);

  void load_map_file(const std::string& map_data);
  void get_objs_from_dgo(const fs::path& filename, const Config& config);
  void add_obj_from_dgo(const std::string& obj_name,
                        const std::string& name_in_dgo,
                        const uint8_t* obj_data,
                        uint32_t obj_size,
                        const std::string& dgo_name,
                        const Config& config,
                        const std::string& cut_name = "");

  /*!
   * Apply f to all ObjectFileData's. Does it in the right order.
   */
  template <typename Func>
  void for_each_obj(Func f) {
    ASSERT(obj_files_by_name.size() == obj_file_order.size());
    for (const auto& name : obj_file_order) {
      for (auto& obj : obj_files_by_name.at(name)) {
        f(obj);
      }
    }
  }

  /*!
   * Apply f to all ObjectFileData's in a specific DGO. Does it in the right order.
   */
  template <typename Func>
  void for_each_obj_in_dgo(const std::string& dgo_name, Func f) {
    ASSERT(obj_files_by_name.size() == obj_file_order.size());
    if (obj_files_by_dgo.count(dgo_name) > 0) {
      const auto& dgo_objs = obj_files_by_dgo.at(dgo_name);
      for (const auto& rec : dgo_objs) {
        for (auto& obj : obj_files_by_name.at(rec.name)) {
          f(obj);
        }
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
      for (int i = 0; i < int(data.linked_data.segments); i++) {
        [[maybe_unused]] int fn = 0;
        for (auto& goal_func : data.linked_data.functions_by_seg.at(i)) {
          f(goal_func, i, data);
          fn++;
        }
      }
    });
  }

  template <typename Func>
  void for_each_function_def_order(Func f) {
    for_each_obj([&](ObjectFileData& data) {
      for (int i = 0; i < int(data.linked_data.segments); i++) {
        int fn = 0;
        for (size_t j = data.linked_data.functions_by_seg.at(i).size(); j-- > 0;) {
          f(data.linked_data.functions_by_seg.at(i).at(j), i, data);
          fn++;
        }
      }
    });
  }

  template <typename Func>
  void for_each_function_def_order_in_obj(ObjectFileData& data, Func f) {
    for (int i = 0; i < int(data.linked_data.segments); i++) {
      [[maybe_unused]] int fn = 0;
      for (size_t j = data.linked_data.functions_by_seg.at(i).size(); j-- > 0;) {
        f(data.linked_data.functions_by_seg.at(i).at(j), i);
        fn++;
      }
    }
  }

  template <typename Func>
  void for_each_function_in_seg(int seg, Func f) {
    for_each_obj([&](ObjectFileData& data) {
      int fn = 0;
      if (data.linked_data.segments == 3) {
        for (size_t j = data.linked_data.functions_by_seg.at(seg).size(); j-- > 0;) {
          f(data.linked_data.functions_by_seg.at(seg).at(j), data);
          fn++;
        }
      }
    });
  }

  template <typename Func>
  void for_each_function_in_seg_in_obj(int seg, ObjectFileData& data, Func f) {
    [[maybe_unused]] int fn = 0;
    if (data.linked_data.segments == 3) {
      for (size_t j = data.linked_data.functions_by_seg.at(seg).size(); j-- > 0;) {
        f(data.linked_data.functions_by_seg.at(seg).at(j));
        fn++;
      }
    }
  }

  // Danger: after adding all object files, we assume that the vector never reallocates.
  std::unordered_map<std::string, std::vector<ObjectFileData>> obj_files_by_name;
  std::unordered_map<std::string, std::vector<ObjectFileRecord>> obj_files_by_dgo;

  std::vector<std::string> obj_file_order;
  std::unordered_map<std::string, std::unordered_map<std::string, std::string>> dgo_obj_name_map;

  SymbolMapBuilder map_builder;

  struct {
    LetRewriteStats let;
    uint32_t total_dgo_bytes = 0;
    uint32_t total_obj_files = 0;
    uint32_t unique_obj_files = 0;
    uint32_t unique_obj_bytes = 0;
  } stats;

  GameVersion version() const { return m_version; }

 private:
  GameVersion m_version;
};

std::string print_art_elt_for_dump(const std::string& group_name, const std::string& name, int idx);
}  // namespace decompiler
