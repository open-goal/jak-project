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
#include "decompiler/data/tpage.h"
#include "decompiler/data/game_text.h"
#include "decompiler/data/StrFileReader.h"
#include "decompiler/data/game_count.h"
#include "LinkedObjectFileCreation.h"
#include "decompiler/config.h"
#include "third-party/minilzo/minilzo.h"
#include "common/util/BinaryReader.h"
#include "common/util/Timer.h"
#include "common/util/FileUtil.h"
#include "decompiler/Function/BasicBlocks.h"
#include "decompiler/IR/BasicOpBuilder.h"
#include "decompiler/IR/CfgBuilder.h"
#include "decompiler/Function/TypeInspector.h"
#include "common/log/log.h"
#include "third-party/json.hpp"

namespace decompiler {
namespace {
std::string strip_dgo_extension(const std::string& x) {
  auto ext = x.substr(x.length() - 4, 4);
  if (ext == ".CGO" || ext == ".cgo" || ext == ".DGO" || ext == ".dgo") {
    return x.substr(0, x.length() - 4);
  }
  return x;
}

/*!
 * Get an object name from a file name.
 * Strips off the file extension and anything before the last slash.
 */
std::string obj_filename_to_name(const std::string& x) {
  auto end = x.length();

  // find last dot
  auto last_dot = end;
  for (; last_dot-- > 0;) {
    if (x.at(last_dot) == '.') {
      break;
    }
  }

  if (last_dot == 0) {
    last_dot = end;
  }

  auto last_slash = end;
  for (; last_slash-- > 0;) {
    if (x.at(last_slash) == '\\' || x.at(last_slash) == '/') {
      break;
    }
  }

  assert(last_dot > last_slash + 1);
  assert(last_slash + 1 < x.length());
  return x.substr(last_slash + 1, last_dot - last_slash - 1);
}
}  // namespace

std::string ObjectFileData::to_unique_name() const {
  if (!name_from_map.empty()) {
    return name_from_map;
  }

  if (has_multiple_versions) {
    std::string result = record.name + "-";
    auto dgo_names_sorted = dgo_names;
    std::sort(dgo_names_sorted.begin(), dgo_names_sorted.end());
    for (auto x : dgo_names_sorted) {
      x = strip_dgo_extension(x);
      result += x + "-";
    }
    result.pop_back();
    return result;
  } else {
    return record.name;
  }
}

ObjectFileData& ObjectFileDB::lookup_record(const ObjectFileRecord& rec) {
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
ObjectFileDB::ObjectFileDB(const std::vector<std::string>& _dgos,
                           const std::string& obj_file_name_map_file,
                           const std::vector<std::string>& object_files,
                           const std::vector<std::string>& str_files) {
  Timer timer;

  lg::info("-Loading types...");
  dts.parse_type_defs({"decompiler", "config", "all-types.gc"});

  if (!obj_file_name_map_file.empty()) {
    lg::info("-Loading obj name map file...");
    load_map_file(file_util::read_text_file(file_util::get_file_path({obj_file_name_map_file})));
  } else {
    lg::warn(
        "Not using an obj name map file! The decompiler will automatically generate object file "
        "names and write them to out/objs.txt. It is recommended to reuse this map file to get "
        "consistent naming when doing a partial decompilation.");
  }

  lg::info("-Loading DGOs...");
  for (auto& dgo : _dgos) {
    get_objs_from_dgo(dgo);
  }

  lg::info("-Loading plain object files...");
  for (auto& obj : object_files) {
    auto data = file_util::read_binary_file(obj);
    auto name = obj_filename_to_name(obj);
    add_obj_from_dgo(name, name, data.data(), data.size(), "NO-XGO");
  }

  lg::info("-Loading streaming object files...");
  for (auto& obj : str_files) {
    StrFileReader reader(obj);
    // name from the file name
    std::string base_name = obj_filename_to_name(obj);
    // name from inside the file (this does a lot of sanity checking)
    auto obj_name = reader.get_full_name(base_name + ".STR");
    for (int i = 0; i < reader.chunk_count(); i++) {
      // append the chunk ID to the full name
      std::string name = obj_name + fmt::format("+{}", i);
      auto& data = reader.get_chunk(i);
      add_obj_from_dgo(name, name, data.data(), data.size(), "NO-XGO");
    }
  }

  lg::info("ObjectFileDB Initialized:");
  lg::info("Total DGOs: {}", int(_dgos.size()));
  lg::info("Total data: {} bytes", stats.total_dgo_bytes);
  lg::info("Total objs: {}", stats.total_obj_files);
  lg::info("Unique objs: {}", stats.unique_obj_files);
  lg::info("Unique data: {} bytes", stats.unique_obj_bytes);
  lg::info("Total {:.2f} ms ({:.3f} MB/sec, {:.2f} obj/sec)", timer.getMs(),
           stats.total_dgo_bytes / ((1u << 20u) * timer.getSeconds()),
           stats.total_obj_files / timer.getSeconds());
}

void ObjectFileDB::load_map_file(const std::string& map_data) {
  auto j = nlohmann::json::parse(map_data, nullptr, true, true);

  for (auto& x : j) {
    auto mapped_name = x[0].get<std::string>();
    auto game_name = x[1].get<std::string>();
    auto dgo_names = x[3].get<std::vector<std::string>>();
    bool is_ag = mapped_name.find("-ag") != std::string::npos;
    auto game_name_with_ag = game_name;
    if (is_ag) {
      game_name_with_ag += "-ag";
    }

    // add dgo
    for (auto& dgo : dgo_names) {
      auto kv = dgo_obj_name_map[dgo].find(game_name_with_ag);
      if (kv != dgo_obj_name_map[dgo].end()) {
        lg::error("Object {} in dgo {} occurs more than one time.", game_name_with_ag, dgo);
        assert(false);
      }
      dgo_obj_name_map[dgo][game_name_with_ag] = mapped_name;
    }
  }
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

  auto dgo_base_name = file_util::base_name(filename);
  assert(header.name == dgo_base_name);
  assert_string_empty_after(header.name, 60);

  // get all obj files...
  for (uint32_t i = 0; i < header.size; i++) {
    auto obj_header = reader.read<DgoHeader>();
    assert(reader.bytes_left() >= obj_header.size);
    assert_string_empty_after(obj_header.name, 60);

    if (std::string(obj_header.name).find("-ag") != std::string::npos) {
      lg::error(
          "Object file {} has \"-ag\" in its name. This will break any tools which use this to "
          "detect an art group",
          obj_header.name);
      assert(false);
    }

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
                                    const uint8_t* obj_data,
                                    uint32_t obj_size,
                                    const std::string& dgo_name) {
  stats.total_obj_files++;
  assert(obj_size > 128);
  uint16_t version = *(const uint16_t*)(obj_data + 8);
  auto hash = file_util::crc32(obj_data, obj_size);

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
  if (!dgo_obj_name_map.empty()) {
    auto dgo_kv = dgo_obj_name_map.find(strip_dgo_extension(dgo_name));
    if (dgo_kv == dgo_obj_name_map.end()) {
      lg::error("Object {} is from DGO {}, but this DGO wasn't in the map.", obj_name, dgo_name);
      assert(false);
    }

    auto name_kv = dgo_kv->second.find(obj_name);
    if (name_kv == dgo_kv->second.end()) {
      lg::error("Object {} from DGO {} wasn't found in the name map.", obj_name, dgo_name);
      assert(false);
    }
    data.name_from_map = name_kv->second;
  }
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
  std::string result = "[";
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
      result += "[\"" + pad_string(x.to_unique_name() + "\", ", 50) + "\"" +
                pad_string(x.name_in_dgo + "\", ", 50) + std::to_string(x.obj_version) + ", " +
                dgos + ", \"\"],\n";
      unique_count++;
      all_unique_names.insert(x.to_unique_name());
    }
    // this check is extremely important. It makes sure we don't have any repeat names. This could
    // be caused by two files with the same name, in the same DGOs, but different data.
    assert(int(all_unique_names.size()) == unique_count);
  }

  result.pop_back();  // kill last new line
  result.pop_back();  // kill last comma
  return result + "]";
}

/*!
 * Process all of the linking data of all objects.
 */
void ObjectFileDB::process_link_data() {
  lg::info("- Processing Link Data...");
  Timer process_link_timer;

  LinkedObjectFile::Stats combined_stats;

  for_each_obj([&](ObjectFileData& obj) {
    obj.linked_data = to_linked_object_file(obj.data, obj.record.name, dts);
    combined_stats.add(obj.linked_data.stats);
  });

  lg::info("Processed Link Data:");
  lg::info(" Code {} bytes", combined_stats.total_code_bytes);
  lg::info(" v2 Code {} bytes", combined_stats.total_v2_code_bytes);
  lg::info(" v2 Link Data {} bytes", combined_stats.total_v2_link_bytes);
  lg::info(" v2 Pointers {}", combined_stats.total_v2_pointers);
  lg::info(" v2 Pointer Seeks {}", combined_stats.total_v2_pointer_seeks);
  lg::info(" v2 Symbols {}", combined_stats.total_v2_symbol_count);
  lg::info(" v2 Symbol Links {}", combined_stats.total_v2_symbol_links);

  lg::info(" v3 Code {} bytes", combined_stats.v3_code_bytes);
  lg::info(" v3 Link Data {} bytes", combined_stats.v3_link_bytes);
  lg::info(" v3 Pointers {}", combined_stats.v3_pointers);
  lg::info("   Split {}", combined_stats.v3_split_pointers);
  lg::info("   Word  {}", combined_stats.v3_word_pointers);
  lg::info(" v3 Pointer Seeks {}", combined_stats.v3_pointer_seeks);
  lg::info(" v3 Symbols {}", combined_stats.v3_symbol_count);
  lg::info(" v3 Offset Symbol Links {}", combined_stats.v3_symbol_link_offset);
  lg::info(" v3 Word Symbol Links {}", combined_stats.v3_symbol_link_word);

  lg::info(" Total {} ms\n", process_link_timer.getMs());
  // printf("\n");
}

/*!
 * Process all of the labels generated from linking and give them reasonable names.
 */
void ObjectFileDB::process_labels() {
  lg::info("- Processing Labels...");
  Timer process_label_timer;
  uint32_t total = 0;
  for_each_obj([&](ObjectFileData& obj) { total += obj.linked_data.set_ordered_label_names(); });

  lg::info("Processed Labels:");
  lg::info(" Total {} labels", total);
  lg::info(" Total {} ms", process_label_timer.getMs());
  // printf("\n");
}

/*!
 * Dump object files and their linking data to text files for debugging
 */
void ObjectFileDB::write_object_file_words(const std::string& output_dir, bool dump_v3_only) {
  if (dump_v3_only) {
    lg::info("- Writing object file dumps (v3 only)...");
  } else {
    lg::info("- Writing object file dumps (all)...");
  }

  Timer timer;
  uint32_t total_bytes = 0, total_files = 0;

  for_each_obj([&](ObjectFileData& obj) {
    if (obj.linked_data.segments == 3 || !dump_v3_only) {
      auto file_text = obj.linked_data.print_words();
      auto file_name = file_util::combine_path(output_dir, obj.to_unique_name() + ".txt");
      total_bytes += file_text.size();
      file_util::write_text_file(file_name, file_text);
      total_files++;
    }
  });

  lg::info("Wrote object file dumps:");
  lg::info(" Total {} files", total_files);
  lg::info(" Total {:.3f} MB", total_bytes / ((float)(1u << 20u)));
  lg::info(" Total {} ms ({:.3f} MB/sec)", timer.getMs(),
           total_bytes / ((1u << 20u) * timer.getSeconds()));
  // printf("\n");
}

void ObjectFileDB::write_debug_type_analysis(const std::string& output_dir,
                                             const std::string& suffix) {
  lg::info("- Writing debug type analysis...");
  Timer timer;
  uint32_t total_bytes = 0, total_files = 0;

  for_each_obj([&](ObjectFileData& obj) {
    if (obj.linked_data.has_any_functions()) {
      auto file_text = obj.linked_data.print_type_analysis_debug();
      auto file_name =
          file_util::combine_path(output_dir, obj.to_unique_name() + suffix + "_dbt.asm");

      total_bytes += file_text.size();
      file_util::write_text_file(file_name, file_text);
      total_files++;
    }
  });

  lg::info("Wrote functions dumps:");
  lg::info(" Total {} files", total_files);
  lg::info(" Total {} MB", total_bytes / ((float)(1u << 20u)));
  lg::info(" Total {} ms ({:.3f} MB/sec)", timer.getMs(),
           total_bytes / ((1u << 20u) * timer.getSeconds()));
}

/*!
 * Dump disassembly for object files containing code.  Data zones will also be dumped.
 */
void ObjectFileDB::write_disassembly(const std::string& output_dir,
                                     bool disassemble_objects_without_functions,
                                     bool write_json,
                                     const std::string& file_suffix) {
  lg::info("- Writing functions...");
  Timer timer;
  uint32_t total_bytes = 0, total_files = 0;

  std::string asm_functions;

  for_each_obj([&](ObjectFileData& obj) {
    if (obj.linked_data.has_any_functions() || disassemble_objects_without_functions) {
      auto file_text = obj.linked_data.print_disassembly();
      asm_functions += obj.linked_data.print_asm_function_disassembly(obj.to_unique_name());
      auto file_name =
          file_util::combine_path(output_dir, obj.to_unique_name() + file_suffix + ".asm");

      if (get_config().analyze_functions && write_json) {
        auto json_asm_text = obj.linked_data.to_asm_json(obj.to_unique_name());
        auto json_asm_file_name =
            file_util::combine_path(output_dir, obj.to_unique_name() + "_asm.json");
        file_util::write_text_file(json_asm_file_name, json_asm_text);
        total_files++;
        total_bytes += json_asm_text.size();
      }

      total_bytes += file_text.size();
      file_util::write_text_file(file_name, file_text);
      total_files++;
    }
  });

  total_bytes += asm_functions.size();
  total_files++;
  file_util::write_text_file(file_util::combine_path(output_dir, "asm_functions.func"),
                             asm_functions);

  lg::info("Wrote functions dumps:");
  lg::info(" Total {} files", total_files);
  lg::info(" Total {} MB", total_bytes / ((float)(1u << 20u)));
  lg::info(" Total {} ms ({:.3f} MB/sec)", timer.getMs(),
           total_bytes / ((1u << 20u) * timer.getSeconds()));
}

/*!
 * Find code/data zones, identify functions, and disassemble
 */
void ObjectFileDB::find_code() {
  lg::info("- Finding code in object files...");
  LinkedObjectFile::Stats combined_stats;
  Timer timer;

  for_each_obj([&](ObjectFileData& obj) {
    //      printf("fc %s\n", obj.record.to_unique_name().c_str());
    obj.linked_data.find_code();
    obj.linked_data.find_functions();
    obj.linked_data.disassemble_functions();

    if (get_config().game_version == 1 || obj.to_unique_name() != "effect-control-v0") {
      obj.linked_data.process_fp_relative_links();
    } else {
      lg::warn("Skipping process_fp_relative_links in {}", obj.to_unique_name().c_str());
    }

    auto& obj_stats = obj.linked_data.stats;
    if (obj_stats.code_bytes / 4 > obj_stats.decoded_ops) {
      lg::warn("Failed to decode all in {} ({} / {})", obj.to_unique_name().c_str(),
               obj_stats.decoded_ops, obj_stats.code_bytes / 4);
    }
    combined_stats.add(obj.linked_data.stats);
  });

  lg::info("Found code:");
  lg::info(" Code {:.3f} MB", combined_stats.code_bytes / (float)(1 << 20));
  lg::info(" Data {:.3f} MB", combined_stats.data_bytes / (float)(1 << 20));
  lg::info(" Functions: {}", combined_stats.function_count);
  lg::info(" fp uses resolved: {} / {} ({:.3f} %)", combined_stats.n_fp_reg_use_resolved,
           combined_stats.n_fp_reg_use,
           100.f * (float)combined_stats.n_fp_reg_use_resolved / combined_stats.n_fp_reg_use);
  auto total_ops = combined_stats.code_bytes / 4;
  lg::info(" Decoded {} / {} ({:.3f} %)", combined_stats.decoded_ops, total_ops,
           100.f * (float)combined_stats.decoded_ops / total_ops);
  lg::info(" Total {:.3f} ms", timer.getMs());
  // printf("\n");
}

/*!
 * Finds and writes all scripts into a file named all_scripts.lisp.
 * Doesn't change any state in ObjectFileDB.
 */
void ObjectFileDB::find_and_write_scripts(const std::string& output_dir) {
  lg::info("- Finding scripts in object files...");
  Timer timer;
  std::string all_scripts;

  for_each_obj([&](ObjectFileData& obj) {
    auto scripts = obj.linked_data.print_scripts();
    if (!scripts.empty()) {
      all_scripts += ";--------------------------------------\n";
      all_scripts += "; " + obj.to_unique_name() + "\n";
      all_scripts += ";---------------------------------------\n";
      all_scripts += scripts;
    }
  });

  auto file_name = file_util::combine_path(output_dir, "all_scripts.lisp");
  file_util::write_text_file(file_name, all_scripts);

  lg::info("Found scripts:");
  lg::info(" Total {:.3f} ms\n", timer.getMs());
}

void ObjectFileDB::process_tpages() {
  lg::info("- Finding textures in tpages...");
  std::string tpage_string = "tpage-";
  int total = 0, success = 0;
  Timer timer;
  for_each_obj([&](ObjectFileData& data) {
    if (data.name_in_dgo.substr(0, tpage_string.length()) == tpage_string) {
      auto statistics = process_tpage(data);
      total += statistics.total_textures;
      success += statistics.successful_textures;
    }
  });
  lg::info("Processed {} / {} textures {:.2f}% in {:.2f} ms", success, total,
           100.f * float(success) / float(total), timer.getMs());
}

std::string ObjectFileDB::process_game_text_files() {
  lg::info("- Finding game text...");
  std::string text_string = "COMMON";
  Timer timer;
  int file_count = 0;
  int string_count = 0;
  int char_count = 0;
  std::unordered_map<int, std::unordered_map<int, std::string>> text_by_language_by_id;

  for_each_obj([&](ObjectFileData& data) {
    if (data.name_in_dgo.substr(1) == text_string) {
      file_count++;
      auto statistics = process_game_text(data);
      string_count += statistics.total_text;
      char_count += statistics.total_chars;
      if (text_by_language_by_id.find(statistics.language) != text_by_language_by_id.end()) {
        assert(false);
      }
      text_by_language_by_id[statistics.language] = std::move(statistics.text);
    }
  });

  lg::info("Processed {} text files ({} strings, {} characters) in {:.2f} ms", file_count,
           string_count, char_count, timer.getMs());

  return write_game_text(text_by_language_by_id);
}

std::string ObjectFileDB::process_game_count_file() {
  lg::info("- Finding game count file...");
  bool found = false;
  std::string result;

  for_each_obj([&](ObjectFileData& data) {
    if (data.name_in_dgo == "game-cnt") {
      assert(!found);
      found = true;
      result = write_game_count(process_game_count(data));
    }
  });

  if (!found) {
    lg::warn("did not find game-cnt file");
  }

  return result;
}

/*!
 * This is the main decompiler routine which runs after we've identified functions.
 */
void ObjectFileDB::analyze_functions() {
  lg::info("- Analyzing Functions...");
  Timer timer;

  int total_functions = 0;
  int resolved_cfg_functions = 0;
  const auto& config = get_config();

  // Step 1 - analyze the "top level" or "login" code for each object file.
  // this will give us type definitions, method definitions, and function definitions...
  lg::info("  - Processing top levels...");

  timer.start();
  for_each_obj([&](ObjectFileData& data) {
    if (data.linked_data.segments == 3) {
      // the top level segment should have a single function
      assert(data.linked_data.functions_by_seg.at(2).size() == 1);

      auto& func = data.linked_data.functions_by_seg.at(2).front();
      assert(func.guessed_name.empty());
      func.guessed_name.set_as_top_level();
      func.find_global_function_defs(data.linked_data, dts);
      func.find_type_defs(data.linked_data, dts);
      func.find_method_defs(data.linked_data, dts);
    }
  });

  // check for function uniqueness.
  std::unordered_set<std::string> unique_names;
  std::unordered_map<std::string, std::unordered_set<std::string>> duplicated_functions;

  int uid = 1;
  for_each_obj([&](ObjectFileData& data) {
    int func_in_obj = 0;
    for (int segment_id = 0; segment_id < int(data.linked_data.segments); segment_id++) {
      for (auto& func : data.linked_data.functions_by_seg.at(segment_id)) {
        func.guessed_name.unique_id = uid++;
        func.guessed_name.id_in_object = func_in_obj++;
        func.guessed_name.object_name = data.to_unique_name();
        auto name = func.guessed_name.to_string();

        if (unique_names.find(name) != unique_names.end()) {
          duplicated_functions[name].insert(data.to_unique_name());
        }

        unique_names.insert(name);

        if (config.asm_functions_by_name.find(name) != config.asm_functions_by_name.end()) {
          func.warnings += ";; flagged as asm by config\n";
          func.suspected_asm = true;
        }
      }
    }
  });

  for_each_function([&](Function& func, int segment_id, ObjectFileData& data) {
    (void)segment_id;
    auto name = func.guessed_name.to_string();

    if (duplicated_functions.find(name) != duplicated_functions.end()) {
      duplicated_functions[name].insert(data.to_unique_name());
      func.warnings += ";; this function exists in multiple non-identical object files\n";
    }
  });
  /*
      for (const auto& kv : duplicated_functions) {
        printf("Function %s is found in non-identical object files:\n", kv.first.c_str());
        for (const auto& obj : kv.second) {
          printf(" %s\n", obj.c_str());
        }
      }
      */

  int total_trivial_cfg_functions = 0;
  int total_named_functions = 0;
  int total_basic_ops = 0;
  int total_failed_basic_ops = 0;
  int total_reginfo_ops = 0;

  int asm_funcs = 0;
  int non_asm_funcs = 0;
  int successful_cfg_irs = 0;
  int successful_type_analysis = 0;
  int attempted_type_analysis = 0;
  int bad_type_analysis = 0;  // didn't attempt because we didn't know how + attempted but failed

  std::map<int, std::vector<std::string>> unresolved_by_length;

  timer.start();
  int total_basic_blocks = 0;

  // Main Pass over each function...
  for_each_function_def_order([&](Function& func, int segment_id, ObjectFileData& data) {
    total_functions++;
    //        if (func.guessed_name.to_string() != "sort") {
    //          return;
    //        }
    //          printf("in %s from %s\n", func.guessed_name.to_string().c_str(),
    //                 data.to_unique_name().c_str());

    // first, find basic blocks.
    auto blocks = find_blocks_in_function(data.linked_data, segment_id, func);
    total_basic_blocks += blocks.size();
    func.basic_blocks = blocks;

    // analyze the proluge
    if (!func.suspected_asm) {
      // first, find the prologue/epilogue
      func.analyze_prologue(data.linked_data);
    }

    if (!func.suspected_asm) {
      // run analysis

      // build a control flow graph, just looking at branch instructions.
      func.cfg = build_cfg(data.linked_data, segment_id, func);

      // convert individual basic blocks to sequences of IR Basic Ops
      for (auto& block : func.basic_blocks) {
        if (block.end_word > block.start_word) {
          auto label_id =
              data.linked_data.get_label_at(segment_id, (func.start_word + block.start_word) * 4);
          if (label_id != -1) {
            block.label_name = data.linked_data.get_label_name(label_id);
          }

          block.start_basic_op = func.basic_ops.size();
          add_basic_ops_to_block(&func, block, &data.linked_data);
          block.end_basic_op = func.basic_ops.size();
        }
      }
      total_basic_ops += func.get_basic_op_count();
      total_failed_basic_ops += func.get_failed_basic_op_count();
      total_reginfo_ops += func.get_reginfo_basic_op_count();

      // if we got an inspect method, inspect it.
      if (func.is_inspect_method) {
        auto result = inspect_inspect_method(func, func.method_of_type, dts, data.linked_data);
        all_type_defs += ";; " + data.to_unique_name() + "\n";
        all_type_defs += result.print_as_deftype() + "\n";
      }

      // Combine basic ops + CFG to build a nested IR
      // register usage first, so we can tell if the SC's if's are used by value.
      func.run_reg_usage();
      func.ir = build_cfg_ir(func, *func.cfg, data.linked_data);
      non_asm_funcs++;
      if (func.ir) {
        successful_cfg_irs++;
      }

      if (func.cfg->is_fully_resolved()) {
        resolved_cfg_functions++;
      } else {
        lg::warn("Function {} from {} failed cfg ir", func.guessed_name.to_string(),
                 data.to_unique_name());
      }

      // type analysis

      if (get_config().function_type_prop) {
        auto hints = get_config().type_hints_by_function_by_idx[func.guessed_name.to_string()];
        if (get_config().no_type_analysis_functions_by_name.find(func.guessed_name.to_string()) ==
            get_config().no_type_analysis_functions_by_name.end()) {
          if (func.guessed_name.kind == FunctionName::FunctionKind::GLOBAL) {
            // we're a global named function. This means we're stored in a symbol
            auto kv = dts.symbol_types.find(func.guessed_name.function_name);
            if (kv != dts.symbol_types.end() && kv->second.arg_count() >= 1) {
              if (kv->second.base_type() != "function") {
                lg::error("Found a function named {} but the symbol has type {}",
                          func.guessed_name.to_string(), kv->second.print());
                assert(false);
              }
              // GOOD!
              func.type = kv->second;
              func.attempted_type_analysis = true;
              attempted_type_analysis++;
              //            lg::info("Type Analysis on {} {}", func.guessed_name.to_string(),
              //                         kv->second.print());
              if (func.run_type_analysis(kv->second, dts, data.linked_data, hints)) {
                successful_type_analysis++;
              } else {
                // bad, failed.
                bad_type_analysis++;
              }
            } else {
              // bad, don't know global type
              bad_type_analysis++;
            }
          } else if (func.guessed_name.kind == FunctionName::FunctionKind::METHOD) {
            // it's a method.
            try {
              auto info =
                  dts.ts.lookup_method(func.guessed_name.type_name, func.guessed_name.method_id);
              if (info.type.arg_count() >= 1) {
                if (info.type.base_type() != "function") {
                  lg::error("Found a method named {} but the symbol has type {}",
                            func.guessed_name.to_string(), info.type.print());
                  assert(false);
                }
                // GOOD!
                func.type = info.type.substitute_for_method_call(func.guessed_name.type_name);
                func.attempted_type_analysis = true;
                attempted_type_analysis++;
                //              lg::info("Type Analysis on {} {}",
                //              func.guessed_name.to_string(),
                //                           func.type.print());
                if (func.run_type_analysis(func.type, dts, data.linked_data, hints)) {
                  successful_type_analysis++;
                } else {
                  bad_type_analysis++;
                }
              } else {
                // not enough type info
                bad_type_analysis++;
              }

            } catch (std::runtime_error& e) {
              // failed to lookup method info
              bad_type_analysis++;
            }
          } else if (func.guessed_name.kind == FunctionName::FunctionKind::TOP_LEVEL_INIT) {
            attempted_type_analysis++;
            func.type = dts.ts.make_function_typespec({}, "none");
            func.attempted_type_analysis = true;
            if (func.run_type_analysis(func.type, dts, data.linked_data, hints)) {
              successful_type_analysis++;
            } else {
              // failed
              bad_type_analysis++;
            }
          } else if (func.guessed_name.kind == FunctionName::FunctionKind::UNIDENTIFIED) {
            auto obj_name = data.to_unique_name();
            // try looking up the object
            const auto& map = get_config().anon_function_types_by_obj_by_id;
            auto obj_kv = map.find(obj_name);
            if (obj_kv != map.end()) {
              auto func_kv = obj_kv->second.find(func.guessed_name.get_anon_id());
              if (func_kv != obj_kv->second.end()) {
                attempted_type_analysis++;
                func.type = dts.parse_type_spec(func_kv->second);
                func.attempted_type_analysis = true;
                if (func.run_type_analysis(func.type, dts, data.linked_data, hints)) {
                  successful_type_analysis++;
                } else {
                  // tried, but failed.
                  bad_type_analysis++;
                }
              } else {
                // no id
                bad_type_analysis++;
              }
            } else {
              // no object in map
              bad_type_analysis++;
            }
          } else {
            // unsupported function kind
            bad_type_analysis++;
          }

          if (!func.attempted_type_analysis) {
            func.warnings.append(";; Failed to try type analysis\n");
          }
        } else {
          func.warnings.append(";; Marked as no type analysis in config\n");
        }
      }
    } else {
      asm_funcs++;
      func.warnings.append(";; Assembly Function. Analysis passes were not attempted.\n");
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

    //    if (func.guessed_name.to_string() == "reset-and-call") {
    //      assert(false);
    //    }
  });

  lg::info("Found {} functions ({} with no control flow)", total_functions,
           total_trivial_cfg_functions);
  lg::info("Named {}/{} functions ({:.3f}%)", total_named_functions, total_functions,
           100.f * float(total_named_functions) / float(total_functions));
  lg::info("Excluding {} asm functions", asm_funcs);
  lg::info("Found {} basic blocks in {:.3f} ms", total_basic_blocks, timer.getMs());
  lg::info(" {}/{} functions passed cfg analysis stage ({:.3f}%)", resolved_cfg_functions,
           non_asm_funcs, 100.f * float(resolved_cfg_functions) / float(non_asm_funcs));
  int successful_basic_ops = total_basic_ops - total_failed_basic_ops;
  lg::info(" {}/{} basic ops converted successfully ({:.3f}%)", successful_basic_ops,
           total_basic_ops, 100.f * float(successful_basic_ops) / float(total_basic_ops));
  lg::info(" {}/{} basic ops with reginfo ({:.3f}%)", total_reginfo_ops, total_basic_ops,
           100.f * float(total_reginfo_ops) / float(total_basic_ops));
  lg::info(" {}/{} cfgs converted to ir ({:.3f}%)", successful_cfg_irs, non_asm_funcs,
           100.f * float(successful_cfg_irs) / float(non_asm_funcs));
  lg::info(" {}/{} functions attempted type analysis ({:.2f}%)", attempted_type_analysis,
           non_asm_funcs, 100.f * float(attempted_type_analysis) / float(non_asm_funcs));
  lg::info(" {}/{} functions that attempted type analysis succeeded ({:.2f}%)",
           successful_type_analysis, attempted_type_analysis,
           100.f * float(successful_type_analysis) / float(attempted_type_analysis));
  lg::info(" {}/{} functions passed type analysis ({:.2f}%)", successful_type_analysis,
           non_asm_funcs, 100.f * float(successful_type_analysis) / float(non_asm_funcs));
  lg::info(
      " {} functions were supposed to do type analysis but either failed or didn't know their "
      "types.\n",
      bad_type_analysis);

  //    for (auto& kv : unresolved_by_length) {
  //      printf("LEN %d\n", kv.first);
  //      for (auto& x : kv.second) {
  //        printf("  %s\n", x.c_str());
  //      }
  //    }
}

void ObjectFileDB::analyze_expressions() {
  lg::info("- Analyzing Expressions...");
  Timer timer;
  int attempts = 0;
  int success = 0;
  bool had_failure = false;
  for_each_function_def_order([&](Function& func, int segment_id, ObjectFileData& data) {
    (void)segment_id;

    if (/*!had_failure &&*/ func.attempted_type_analysis) {
      attempts++;
      lg::info("Analyze {}", func.guessed_name.to_string());
      if (func.build_expression(data.linked_data)) {
        success++;
      } else {
        func.warnings.append(";; Expression analysis failed.\n");
        had_failure = true;
      }
    }
  });

  lg::info(" {}/{} functions passed expression building ({:.2f}%)\n", success, attempts,
           100.f * float(success) / float(attempts));
}

void ObjectFileDB::dump_raw_objects(const std::string& output_dir) {
  for_each_obj([&](ObjectFileData& data) {
    auto dest = output_dir + "/" + data.to_unique_name();
    file_util::write_binary_file(dest, data.data.data(), data.data.size());
  });
}
}  // namespace decompiler