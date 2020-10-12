/*!
 * @file ObjectFileDB.cpp
 * A "database" of object files found in DGO files.
 * Eliminates duplicate object files, and also assigns unique names to all object files
 * (there may be different object files with the same name sometimes)
 */

#include "ObjectFileDB.h"
#include <algorithm>
#include <set>
#include <cstring>
#include <map>
#include "LinkedObjectFileCreation.h"
#include "decompiler/config.h"
#include "third-party/minilzo/minilzo.h"
#include "common/util/BinaryReader.h"
#include "decompiler/util/FileIO.h"
#include "common/util/Timer.h"
#include "common/util/FileUtil.h"
#include "decompiler/Function/BasicBlocks.h"
#include "decompiler/IR/BasicOpBuilder.h"
#include "decompiler/IR/CfgBuilder.h"

/*!
 * Get a unique name for this object file.
 */
std::string ObjectFileRecord::to_unique_name() const {
  return name + "-v" + std::to_string(version);
}

std::string ObjectFileData::to_unique_name() const {
  if (has_multiple_versions) {
    std::string result = record.name + "-";
    auto dgo_names_sorted = dgo_names;
    std::sort(dgo_names_sorted.begin(), dgo_names_sorted.end());
    for (auto x : dgo_names_sorted) {
      auto ext = x.substr(x.length() - 4, 4);
      if (ext == ".CGO" || ext == ".cgo" || ext == ".DGO" || ext == ".dgo") {
        x = x.substr(0, x.length() - 4);
      }
      result += x + "-";
    }
    result.pop_back();
    return result;
  } else {
    return record.name;
  }
}
ObjectFileData& ObjectFileDB::lookup_record(ObjectFileRecord rec) {
  ObjectFileData* result = nullptr;

  for (auto& x : obj_files_by_name[rec.name]) {
    if (x.record.version == rec.version) {
      assert(x.record.hash == rec.hash);
      assert(!result);
      result = &x;
    }
  }

  assert(result);
  return *result;
}

/*!
 * Build an object file DB for the given list of DGOs.
 */
ObjectFileDB::ObjectFileDB(const std::vector<std::string>& _dgos) {
  Timer timer;
  printf("- Loading Types...\n");
  dts.parse_type_defs({"decompiler", "config", "all-types.gc"});

  printf("- Initializing ObjectFileDB...\n");
  for (auto& dgo : _dgos) {
    get_objs_from_dgo(dgo);
  }

  printf("ObjectFileDB Initialized:\n");
  printf(" total dgos: %d\n", int(_dgos.size()));
  printf(" total data: %d bytes\n", stats.total_dgo_bytes);
  printf(" total objs: %d\n", stats.total_obj_files);
  printf(" unique objs: %d\n", stats.unique_obj_files);
  printf(" unique data: %d bytes\n", stats.unique_obj_bytes);
  printf(" total %.1f ms (%.3f MB/sec, %.3f obj/sec)\n", timer.getMs(),
         stats.total_dgo_bytes / ((1u << 20u) * timer.getSeconds()),
         stats.total_obj_files / timer.getSeconds());
  printf("\n");
}

// Header for a DGO file
struct DgoHeader {
  uint32_t size;
  char name[60];
};

namespace {
/*!
 * Assert false if the char[] has non-null data after the null terminated string.
 * Used to sanity check the sizes of strings in DGO/object file headers.
 */
void assert_string_empty_after(const char* str, int size) {
  auto ptr = str;
  while (*ptr)
    ptr++;
  while (ptr - str < size) {
    assert(!*ptr);
    ptr++;
  }
}
}  // namespace

namespace {
std::string get_object_file_name(const std::string& original_name, uint8_t* data, int size) {
  const char art_group_text[] =
      "/src/next/data/art-group6/";  // todo, this may change in other games
  const char suffix[] = "-ag.go";

  int len = int(strlen(art_group_text));
  for (int start = 0; start < size; start++) {
    bool failed = false;
    for (int i = 0; i < len; i++) {
      if (start + i >= size || data[start + i] != art_group_text[i]) {
        failed = true;
        break;
      }
    }

    if (!failed) {
      for (int i = 0; i < int(original_name.length()); i++) {
        if (start + len + i >= size || data[start + len + i] != original_name[i]) {
          assert(false);
        }
      }

      assert(int(strlen(suffix)) + start + len + int(original_name.length()) < size);
      assert(!memcmp(data + start + len + original_name.length(), suffix, strlen(suffix) + 1));

      return original_name + "-ag";
    }
  }

  return original_name;
}
}  // namespace

constexpr int MAX_CHUNK_SIZE = 0x8000;
/*!
 * Load the objects stored in the given DGO into the ObjectFileDB
 */
void ObjectFileDB::get_objs_from_dgo(const std::string& filename) {
  auto dgo_data = file_util::read_binary_file(filename);
  stats.total_dgo_bytes += dgo_data.size();

  const char jak2_header[] = "oZlB";
  bool is_jak2 = true;
  for (int i = 0; i < 4; i++) {
    if (jak2_header[i] != dgo_data[i]) {
      is_jak2 = false;
    }
  }

  if (is_jak2) {
    if (lzo_init() != LZO_E_OK) {
      assert(false);
    }
    BinaryReader compressed_reader(dgo_data);
    // seek past oZlB
    compressed_reader.ffwd(4);
    auto decompressed_size = compressed_reader.read<uint32_t>();
    std::vector<uint8_t> decompressed_data;
    decompressed_data.resize(decompressed_size);
    size_t output_offset = 0;
    while (true) {
      // seek past alignment bytes and read the next chunk size
      uint32_t chunk_size = 0;
      while (!chunk_size) {
        chunk_size = compressed_reader.read<uint32_t>();
      }

      if (chunk_size < MAX_CHUNK_SIZE) {
        lzo_uint bytes_written;
        auto lzo_rv =
            lzo1x_decompress(compressed_reader.here(), chunk_size,
                             decompressed_data.data() + output_offset, &bytes_written, nullptr);
        assert(lzo_rv == LZO_E_OK);
        compressed_reader.ffwd(chunk_size);
        output_offset += bytes_written;
      } else {
        // nope - sometimes chunk_size is bigger than MAX, but we should still use max.
        //        assert(chunk_size == MAX_CHUNK_SIZE);
        memcpy(decompressed_data.data() + output_offset, compressed_reader.here(), MAX_CHUNK_SIZE);
        compressed_reader.ffwd(MAX_CHUNK_SIZE);
        output_offset += MAX_CHUNK_SIZE;
      }

      if (output_offset >= decompressed_size)
        break;
      while (compressed_reader.get_seek() % 4) {
        compressed_reader.ffwd(1);
      }
    }
    dgo_data = decompressed_data;
  }

  BinaryReader reader(dgo_data);
  auto header = reader.read<DgoHeader>();

  auto dgo_base_name = base_name(filename);
  assert(header.name == dgo_base_name);
  assert_string_empty_after(header.name, 60);

  // get all obj files...
  for (uint32_t i = 0; i < header.size; i++) {
    auto obj_header = reader.read<DgoHeader>();
    assert(reader.bytes_left() >= obj_header.size);
    assert_string_empty_after(obj_header.name, 60);

    auto name = get_object_file_name(obj_header.name, reader.here(), obj_header.size);

    add_obj_from_dgo(name, obj_header.name, reader.here(), obj_header.size, dgo_base_name);
    reader.ffwd(obj_header.size);
  }

  // check we're at the end
  assert(0 == reader.bytes_left());
}

/*!
 * Add an object file to the ObjectFileDB
 */
void ObjectFileDB::add_obj_from_dgo(const std::string& obj_name,
                                    const std::string& name_in_dgo,
                                    uint8_t* obj_data,
                                    uint32_t obj_size,
                                    const std::string& dgo_name) {
  stats.total_obj_files++;
  assert(obj_size > 128);
  uint16_t version = *(uint16_t*)(obj_data + 8);
  auto hash = crc32(obj_data, obj_size);

  bool duplicated = false;
  // first, check to see if we already got it...
  for (auto& e : obj_files_by_name[obj_name]) {
    if (e.data.size() == obj_size && e.record.hash == hash) {
      // just to make sure we don't have a hash collision.
      assert(!memcmp(obj_data, e.data.data(), obj_size));

      // already got it!
      e.reference_count++;
      auto& rec = e.record;
      assert(name_in_dgo == e.name_in_dgo);
      e.dgo_names.push_back(dgo_name);
      obj_files_by_dgo[dgo_name].push_back(rec);
      duplicated = true;
      break;
    } else {
      e.has_multiple_versions = true;
    }
  }

  if (duplicated) {
    return;
  }

  // nope, have to add a new one.
  ObjectFileData data;
  data.data.resize(obj_size);
  memcpy(data.data.data(), obj_data, obj_size);
  data.record.hash = hash;
  data.record.name = obj_name;
  data.dgo_names.push_back(dgo_name);
  if (obj_files_by_name[obj_name].empty()) {
    // if this is the first time we've seen this object file name, add it in the order.
    obj_file_order.push_back(obj_name);
  }
  data.record.version = obj_files_by_name[obj_name].size();
  data.name_in_dgo = name_in_dgo;
  data.obj_version = version;
  obj_files_by_dgo[dgo_name].push_back(data.record);
  obj_files_by_name[obj_name].emplace_back(std::move(data));
  stats.unique_obj_files++;
  stats.unique_obj_bytes += obj_size;

  if (obj_files_by_name[obj_name].size() > 1) {
    for (auto& e : obj_files_by_name[obj_name]) {
      e.has_multiple_versions = true;
    }
  }
}

/*!
 * Generate a listing of what object files go in which dgos
 */
std::string ObjectFileDB::generate_dgo_listing() {
  std::string result = ";; DGO File Listing\n\n";
  std::vector<std::string> dgo_names;
  for (auto& kv : obj_files_by_dgo) {
    dgo_names.push_back(kv.first);
  }

  std::sort(dgo_names.begin(), dgo_names.end());

  for (const auto& name : dgo_names) {
    result += "(\"" + name + "\"\n";
    for (auto& obj_rec : obj_files_by_dgo[name]) {
      auto obj = lookup_record(obj_rec);
      std::string extension = ".o";
      if (obj.obj_version == 4 || obj.obj_version == 2) {
        extension = ".go";
      }
      result += "  (\"" + obj.to_unique_name() + extension + "\" \"" + obj.name_in_dgo + "\")\n";
    }
    result += "  )\n\n";
  }

  return result;
}

namespace {
std::string pad_string(const std::string& in, size_t length) {
  assert(in.length() < length);
  return in + std::string(length - in.length(), ' ');
}
}  // namespace

std::string ObjectFileDB::generate_obj_listing() {
  std::string result;
  std::set<std::string> all_unique_names;
  int unique_count = 0;
  for (auto& obj_file : obj_file_order) {
    for (auto& x : obj_files_by_name.at(obj_file)) {
      std::string dgos = "[";
      for (auto& y : x.dgo_names) {
        assert(y.length() >= 5);
        dgos += "\"" + y.substr(0, y.length() - 4) + "\", ";
      }
      dgos.pop_back();
      dgos.pop_back();
      dgos += "]";
      result += "[\"" + pad_string(x.to_unique_name() + "\", ", 40) + "\"" +
                pad_string(x.name_in_dgo + "\", ", 30) + std::to_string(x.obj_version) + ", " +
                dgos + ", \"\"],\n";
      unique_count++;
      all_unique_names.insert(x.to_unique_name());
    }
  }

  // this check is extremely important. It makes sure we don't have any repeat names. This could
  // be caused by two files with the same name, in the same DGOs, but different data.
  assert(int(all_unique_names.size()) == unique_count);
  return result;
}

/*!
 * Process all of the linking data of all objects.
 */
void ObjectFileDB::process_link_data() {
  printf("- Processing Link Data...\n");
  Timer process_link_timer;

  LinkedObjectFile::Stats combined_stats;

  for_each_obj([&](ObjectFileData& obj) {
    obj.linked_data = to_linked_object_file(obj.data, obj.record.name, dts);
    combined_stats.add(obj.linked_data.stats);
  });

  printf("Processed Link Data:\n");
  printf(" code %d bytes\n", combined_stats.total_code_bytes);
  printf(" v2 code %d bytes\n", combined_stats.total_v2_code_bytes);
  printf(" v2 link data %d bytes\n", combined_stats.total_v2_link_bytes);
  printf(" v2 pointers %d\n", combined_stats.total_v2_pointers);
  printf(" v2 pointer seeks %d\n", combined_stats.total_v2_pointer_seeks);
  printf(" v2 symbols %d\n", combined_stats.total_v2_symbol_count);
  printf(" v2 symbol links %d\n", combined_stats.total_v2_symbol_links);

  printf(" v3 code %d bytes\n", combined_stats.v3_code_bytes);
  printf(" v3 link data %d bytes\n", combined_stats.v3_link_bytes);
  printf(" v3 pointers %d\n", combined_stats.v3_pointers);
  printf("   split %d\n", combined_stats.v3_split_pointers);
  printf("   word  %d\n", combined_stats.v3_word_pointers);
  printf(" v3 pointer seeks %d\n", combined_stats.v3_pointer_seeks);
  printf(" v3 symbols %d\n", combined_stats.v3_symbol_count);
  printf(" v3 offset symbol links %d\n", combined_stats.v3_symbol_link_offset);
  printf(" v3 word symbol links %d\n", combined_stats.v3_symbol_link_word);

  printf(" total %.3f ms\n", process_link_timer.getMs());
  printf("\n");
}

/*!
 * Process all of the labels generated from linking and give them reasonable names.
 */
void ObjectFileDB::process_labels() {
  printf("- Processing Labels...\n");
  Timer process_label_timer;
  uint32_t total = 0;
  for_each_obj([&](ObjectFileData& obj) { total += obj.linked_data.set_ordered_label_names(); });

  printf("Processed Labels:\n");
  printf(" total %d labels\n", total);
  printf(" total %.3f ms\n", process_label_timer.getMs());
  printf("\n");
}

/*!
 * Dump object files and their linking data to text files for debugging
 */
void ObjectFileDB::write_object_file_words(const std::string& output_dir, bool dump_v3_only) {
  if (dump_v3_only) {
    printf("- Writing object file dumps (v3 only)...\n");
  } else {
    printf("- Writing object file dumps (all)...\n");
  }

  Timer timer;
  uint32_t total_bytes = 0, total_files = 0;

  for_each_obj([&](ObjectFileData& obj) {
    if (obj.linked_data.segments == 3 || !dump_v3_only) {
      auto file_text = obj.linked_data.print_words();
      auto file_name = combine_path(output_dir, obj.record.to_unique_name() + ".txt");
      total_bytes += file_text.size();
      file_util::write_text_file(file_name, file_text);
      total_files++;
    }
  });

  printf("Wrote object file dumps:\n");
  printf(" total %d files\n", total_files);
  printf(" total %.3f MB\n", total_bytes / ((float)(1u << 20u)));
  printf(" total %.3f ms (%.3f MB/sec)\n", timer.getMs(),
         total_bytes / ((1u << 20u) * timer.getSeconds()));
  printf("\n");
}

/*!
 * Dump disassembly for object files containing code.  Data zones will also be dumped.
 */
void ObjectFileDB::write_disassembly(const std::string& output_dir,
                                     bool disassemble_objects_without_functions) {
  printf("- Writing functions...\n");
  Timer timer;
  uint32_t total_bytes = 0, total_files = 0;

  for_each_obj([&](ObjectFileData& obj) {
    if (obj.linked_data.has_any_functions() || disassemble_objects_without_functions) {
      auto file_text = obj.linked_data.print_disassembly();
      auto file_name = combine_path(output_dir, obj.record.to_unique_name() + ".func");
      total_bytes += file_text.size();
      file_util::write_text_file(file_name, file_text);
      total_files++;
    }
  });

  printf("Wrote functions dumps:\n");
  printf(" total %d files\n", total_files);
  printf(" total %.3f MB\n", total_bytes / ((float)(1u << 20u)));
  printf(" total %.3f ms (%.3f MB/sec)\n", timer.getMs(),
         total_bytes / ((1u << 20u) * timer.getSeconds()));
  printf("\n");
}

/*!
 * Find code/data zones, identify functions, and disassemble
 */
void ObjectFileDB::find_code() {
  printf("- Finding code in object files...\n");
  LinkedObjectFile::Stats combined_stats;
  Timer timer;

  for_each_obj([&](ObjectFileData& obj) {
    //      printf("fc %s\n", obj.record.to_unique_name().c_str());
    obj.linked_data.find_code();
    obj.linked_data.find_functions();
    obj.linked_data.disassemble_functions();

    if (get_config().game_version == 1 || obj.record.to_unique_name() != "effect-control-v0") {
      obj.linked_data.process_fp_relative_links();
    } else {
      printf("skipping process_fp_relative_links in %s\n", obj.record.to_unique_name().c_str());
    }

    auto& obj_stats = obj.linked_data.stats;
    if (obj_stats.code_bytes / 4 > obj_stats.decoded_ops) {
      printf("Failed to decode all in %s (%d / %d)\n", obj.record.to_unique_name().c_str(),
             obj_stats.decoded_ops, obj_stats.code_bytes / 4);
    }
    combined_stats.add(obj.linked_data.stats);
  });

  printf("Found code:\n");
  printf(" code %.3f MB\n", combined_stats.code_bytes / (float)(1 << 20));
  printf(" data %.3f MB\n", combined_stats.data_bytes / (float)(1 << 20));
  printf(" functions: %d\n", combined_stats.function_count);
  printf(" fp uses resolved: %d / %d (%.3f %%)\n", combined_stats.n_fp_reg_use_resolved,
         combined_stats.n_fp_reg_use,
         100.f * (float)combined_stats.n_fp_reg_use_resolved / combined_stats.n_fp_reg_use);
  auto total_ops = combined_stats.code_bytes / 4;
  printf(" decoded %d / %d (%.3f %%)\n", combined_stats.decoded_ops, total_ops,
         100.f * (float)combined_stats.decoded_ops / total_ops);
  printf(" total %.3f ms\n", timer.getMs());
  printf("\n");
}

/*!
 * Finds and writes all scripts into a file named all_scripts.lisp.
 * Doesn't change any state in ObjectFileDB.
 */
void ObjectFileDB::find_and_write_scripts(const std::string& output_dir) {
  printf("- Finding scripts in object files...\n");
  Timer timer;
  std::string all_scripts;

  for_each_obj([&](ObjectFileData& obj) {
    auto scripts = obj.linked_data.print_scripts();
    if (!scripts.empty()) {
      all_scripts += ";--------------------------------------\n";
      all_scripts += "; " + obj.record.to_unique_name() + "\n";
      all_scripts += ";---------------------------------------\n";
      all_scripts += scripts;
    }
  });

  auto file_name = combine_path(output_dir, "all_scripts.lisp");
  file_util::write_text_file(file_name, all_scripts);

  printf("Found scripts:\n");
  printf(" total %.3f ms\n", timer.getMs());
  printf("\n");
}

void ObjectFileDB::analyze_functions() {
  printf("- Analyzing Functions...\n");
  Timer timer;

  int total_functions = 0;
  int resolved_cfg_functions = 0;
  const auto& config = get_config();

  {
    timer.start();
    for_each_obj([&](ObjectFileData& data) {
      if (data.linked_data.segments == 3) {
        // the top level segment should have a single function
        assert(data.linked_data.functions_by_seg.at(2).size() == 1);

        auto& func = data.linked_data.functions_by_seg.at(2).front();
        assert(func.guessed_name.empty());
        func.guessed_name.set_as_top_level();
        func.find_global_function_defs(data.linked_data, dts);
        func.find_method_defs(data.linked_data);
      }
    });

    // check for function uniqueness.
    std::unordered_set<std::string> unique_names;
    std::unordered_map<std::string, std::unordered_set<std::string>> duplicated_functions;

    int uid = 1;
    for_each_function([&](Function& func, int segment_id, ObjectFileData& data) {
      (void)segment_id;
      func.guessed_name.unique_id = uid++;
      auto name = func.guessed_name.to_string();
      if (func.guessed_name.expected_unique()) {
        if (unique_names.find(name) != unique_names.end()) {
          duplicated_functions[name].insert(data.record.to_unique_name());
        }

        unique_names.insert(name);
      }

      if (config.asm_functions_by_name.find(name) != config.asm_functions_by_name.end()) {
        func.warnings += "flagged as asm by config\n";
        func.suspected_asm = true;
      }
    });

    for_each_function([&](Function& func, int segment_id, ObjectFileData& data) {
      (void)segment_id;
      auto name = func.guessed_name.to_string();
      if (func.guessed_name.expected_unique()) {
        if (duplicated_functions.find(name) != duplicated_functions.end()) {
          duplicated_functions[name].insert(data.record.to_unique_name());
          func.warnings += "this function exists in multiple non-identical object files";
        }
      }
    });

    //    for(const auto& kv : duplicated_functions) {
    //      printf("Function %s is found in non-identical object files:\n", kv.first.c_str());
    //      for(const auto& obj : kv.second) {
    //        printf(" %s\n", obj.c_str());
    //      }
    //    }
  }

  int total_trivial_cfg_functions = 0;
  int total_named_functions = 0;
  int total_basic_ops = 0;
  int total_failed_basic_ops = 0;

  int asm_funcs = 0;
  int non_asm_funcs = 0;
  int successful_cfg_irs = 0;

  std::map<int, std::vector<std::string>> unresolved_by_length;
  if (get_config().find_basic_blocks) {
    timer.start();
    int total_basic_blocks = 0;
    for_each_function([&](Function& func, int segment_id, ObjectFileData& data) {
      //      printf("in %s\n", func.guessed_name.to_string().c_str());
      auto blocks = find_blocks_in_function(data.linked_data, segment_id, func);
      total_basic_blocks += blocks.size();
      func.basic_blocks = blocks;

      total_functions++;

      if (!func.suspected_asm) {
        func.analyze_prologue(data.linked_data);
        func.cfg = build_cfg(data.linked_data, segment_id, func);
        for (auto& block : func.basic_blocks) {
          if (block.end_word > block.start_word) {
            add_basic_ops_to_block(&func, block, &data.linked_data);
          }
        }
        total_basic_ops += func.get_basic_op_count();
        total_failed_basic_ops += func.get_failed_basic_op_count();

        func.ir = build_cfg_ir(func, *func.cfg, data.linked_data);
        non_asm_funcs++;
        if (func.ir) {
          successful_cfg_irs++;
        }

        if (func.cfg->is_fully_resolved()) {
          resolved_cfg_functions++;
        }
      } else {
        asm_funcs++;
      }

      if (func.basic_blocks.size() > 1 && !func.suspected_asm) {
        if (func.cfg->is_fully_resolved()) {
        } else {
          unresolved_by_length[func.end_word - func.start_word].push_back(
              func.guessed_name.to_string());
        }
      }

      if (!func.suspected_asm && func.basic_blocks.size() <= 1) {
        total_trivial_cfg_functions++;
      }

      if (!func.guessed_name.empty()) {
        total_named_functions++;
      }

      //      if (func.guessed_name.to_string() == "inspect") {
      //        assert(false);
      //      }
    });

    printf("Found %d functions (%d with no control flow)\n", total_functions,
           total_trivial_cfg_functions);
    printf("Named %d/%d functions (%.2f%%)\n", total_named_functions, total_functions,
           100.f * float(total_named_functions) / float(total_functions));
    printf("Excluding %d asm functions\n", asm_funcs);
    printf("Found %d basic blocks in %.3f ms\n", total_basic_blocks, timer.getMs());
    printf(" %d/%d functions passed cfg analysis stage (%.2f%%)\n", resolved_cfg_functions,
           non_asm_funcs, 100.f * float(resolved_cfg_functions) / float(non_asm_funcs));
    int successful_basic_ops = total_basic_ops - total_failed_basic_ops;
    printf(" %d/%d basic ops converted successfully (%.2f%%)\n", successful_basic_ops,
           total_basic_ops, 100.f * float(successful_basic_ops) / float(total_basic_ops));
    printf(" %d/%d cfgs converted to ir (%.2f%%)\n", successful_cfg_irs, non_asm_funcs,
           100.f * float(successful_cfg_irs) / float(non_asm_funcs));

    for (auto& kv : unresolved_by_length) {
      printf("LEN %d\n", kv.first);
      for (auto& x : kv.second) {
        printf("  %s\n", x.c_str());
      }
    }
  }
}
