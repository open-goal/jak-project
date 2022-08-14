/*!
 * @file ObjectFileDB.cpp
 * A "database" of object files found in DGO files.
 * Eliminates duplicate object files, and also assigns unique names to all object files
 * (there may be different object files with the same name sometimes)
 */

#include "ObjectFileDB.h"

#include <algorithm>
#include <cstring>
#include <map>
#include <set>

#include "LinkedObjectFileCreation.h"

#include "common/link_types.h"
#include "common/log/log.h"
#include "common/util/BinaryReader.h"
#include "common/util/BitUtils.h"
#include "common/util/FileUtil.h"
#include "common/util/Timer.h"
#include "common/util/crc32.h"
#include "common/util/dgo_util.h"
#include "common/util/json_util.h"

#include "decompiler/Function/BasicBlocks.h"
#include "decompiler/config.h"
#include "decompiler/data/StrFileReader.h"
#include "decompiler/data/dir_tpages.h"
#include "decompiler/data/game_count.h"
#include "decompiler/data/game_text.h"
#include "decompiler/data/tpage.h"

#include "third-party/xdelta3/xdelta3.h"

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

  ASSERT(last_dot > last_slash + 1);
  ASSERT(last_slash + 1 < x.length());
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
    for (const auto& x : dgo_names_sorted) {
      result += strip_dgo_extension(x) + "-";
    }
    result.pop_back();
    return result;
  } else {
    return record.name;
  }
}

const ObjectFileData& ObjectFileDB::lookup_record(const ObjectFileRecord& rec) const {
  const ObjectFileData* result = nullptr;

  for (auto& x : obj_files_by_name.at(rec.name)) {
    if (x.record.version == rec.version) {
      ASSERT(x.record.hash == rec.hash);
      ASSERT(!result);
      result = &x;
    }
  }

  ASSERT(result);
  return *result;
}

/*!
 * Build an object file DB for the given list of DGOs.
 */
ObjectFileDB::ObjectFileDB(const std::vector<fs::path>& _dgos,
                           const fs::path& obj_file_name_map_file,
                           const std::vector<fs::path>& object_files,
                           const std::vector<fs::path>& str_files,
                           const Config& config)
    : dts(config.game_version) {
  Timer timer;

  lg::info("-Loading types...");
  dts.parse_type_defs({config.all_types_file});

  if (!obj_file_name_map_file.empty()) {
    lg::info("-Loading obj name map file...");
    load_map_file(
        file_util::read_text_file(file_util::get_jak_project_dir() / obj_file_name_map_file));
  } else {
    lg::warn(
        "Not using an obj name map file! The decompiler will automatically generate object file "
        "names and write them to out/objs.txt. It is recommended to reuse this map file to get "
        "consistent naming when doing a partial decompilation.");
  }

  lg::info("-Loading {} DGOs...", _dgos.size());
  for (auto& dgo : _dgos) {
    try {
      get_objs_from_dgo(dgo, config);
    } catch (std::runtime_error& e) {
      lg::warn("Error when reading DGOs: {} on {}", e.what(), dgo.string());
    }
  }

  lg::info("-Loading {} plain object files...", object_files.size());
  for (auto& obj : object_files) {
    auto data = file_util::read_binary_file(obj);
    auto name = obj_filename_to_name(obj.string());
    if (auto it = config.object_patches.find(name); it != config.object_patches.end()) {
      // print the file CRC
      fmt::print("CRC for {} is: 0x{:X}\n", name, crc32(data.data(), data.size()));
      // write patch file if necessary
      if (config.write_patches) {
        // this is the "target" file we want to patch to
        auto data2 = file_util::read_binary_file(
            (file_util::get_jak_project_dir() / it->second.target_file).string());
        // we need to allocate a buffer to store the delta patch
        // we make it 2x the source file's size because... it seems like a good size?
        // ideally the delta patch should never be that big.
        int buf_sz = data.size() * 2;
        u8* out_buf = (u8*)malloc(buf_sz);
        size_t out_sz = 0;  // this is where the actual used size of the delta patch will be stored
        int xd3_res = xd3_encode_memory(data2.data(), data2.size(), data.data(), data.size(),
                                        out_buf, &out_sz, buf_sz, 0);
        if (xd3_res) {
          lg::error("error patching {} with {} (out {}): {}", name, it->second.target_file,
                    it->second.patch_file, xd3_strerror(xd3_res));
        } else {
          std::vector<u8> patch_data(out_sz);
          memcpy(patch_data.data(), out_buf, patch_data.size());
          file_util::write_binary_file(
              (file_util::get_jak_project_dir() / it->second.patch_file).string(),
              patch_data.data(), patch_data.size());
        }
        free(out_buf);
      }

      // apply patch file if necessary
      // note that xd3 doesnt really have any safety against bad files so we check crc ourselves
      if (config.apply_patches && it->second.crc == crc32(data.data(), data.size())) {
        // this is the delta patch file
        auto data2 = file_util::read_binary_file(
            (file_util::get_jak_project_dir() / it->second.patch_file).string());
        // we need to allocate a buffer to store the patched file
        // the delta patch isn't gonna be this big but we allocate 2x the source file's size + the
        // delta patch's size
        int buf_sz = data.size() * 2 + data2.size();
        u8* out_buf = (u8*)malloc(buf_sz);
        size_t out_sz = 0;
        int xd3_res = xd3_decode_memory(data2.data(), data2.size(), data.data(), data.size(),
                                        out_buf, &out_sz, buf_sz, 0);
        if (xd3_res) {
          lg::error("error patching {} with {} (out {}): {}", name, it->second.target_file,
                    it->second.patch_file, xd3_strerror(xd3_res));
        } else {
          data.resize(out_sz);
          memcpy(data.data(), out_buf, data.size());
        }
        free(out_buf);
      }
    }
    add_obj_from_dgo(name, name, data.data(), data.size(), "NO-XGO", config);
  }

  lg::info("-Loading {} streaming object files...", str_files.size());
  for (auto& obj : str_files) {
    StrFileReader reader(obj);
    // name from the file name
    std::string base_name = obj_filename_to_name(obj.string());
    // name from inside the file (this does a lot of sanity checking)
    auto obj_name = reader.get_full_name(base_name + ".STR");
    for (int i = 0; i < reader.chunk_count(); i++) {
      // append the chunk ID to the full name
      std::string name = obj_name + fmt::format("+{}", i);
      auto& data = reader.get_chunk(i);
      add_obj_from_dgo(name, name, data.data(), data.size(), "NO-XGO", config);
    }
  }

  lg::info("ObjectFileDB Initialized\n");
  if (obj_files_by_name.empty()) {
    lg::error(
        "No object files have been added. Check that there are input files and the allowed_objects "
        "list.");
  }

  dts.bad_format_strings = config.bad_format_strings;
  dts.format_ops_with_dynamic_string_by_func_name =
      config.hacks.format_ops_with_dynamic_string_by_func_name;
}

void ObjectFileDB::load_map_file(const std::string& map_data) {
  auto j = parse_commented_json(map_data, "ObjectFileDB Map File");

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
        ASSERT(false);
      }
      dgo_obj_name_map[dgo][game_name_with_ag] = mapped_name;
    }
  }
}

/*!
 * Load the objects stored in the given DGO into the ObjectFileDB
 */
void ObjectFileDB::get_objs_from_dgo(const fs::path& filename, const Config& config) {
  auto dgo_data = file_util::read_binary_file(filename);
  stats.total_dgo_bytes += dgo_data.size();

  if (file_util::dgo_header_is_compressed(dgo_data)) {
    dgo_data = file_util::decompress_dgo(dgo_data);
  }

  BinaryReader reader(dgo_data);
  auto header = reader.read<DgoHeader>();

  auto dgo_base_name = filename.filename().string();
  ASSERT(header.name == dgo_base_name);
  assert_string_empty_after(header.name, 60);

  // get all obj files...
  for (uint32_t i = 0; i < header.object_count; i++) {
    auto obj_header = reader.read<DgoHeader>();
    assert_string_empty_after(obj_header.name, 60);
    if (i == header.object_count - 1) {
      if (reader.bytes_left() == obj_header.object_count - 0x30) {
        if (config.is_pal) {
          lg::warn("Skipping {} in {} because it is a broken PAL object", obj_header.name,
                   dgo_base_name);
          reader.ffwd(reader.bytes_left());
          continue;
        } else {
          ASSERT(false);
        }
      }
    } else {
      ASSERT(reader.bytes_left() >= obj_header.object_count);
    }

    if (std::string(obj_header.name).find("-ag") != std::string::npos) {
      lg::error(
          "Object file {} has \"-ag\" in its name. This will break any tools which use this to "
          "detect an art group",
          obj_header.name);
      ASSERT(false);
    }

    auto name = get_object_file_name(obj_header.name, reader.here(), obj_header.object_count);

    add_obj_from_dgo(name, obj_header.name, reader.here(), obj_header.object_count, dgo_base_name,
                     config);
    reader.ffwd(align16(obj_header.object_count));
  }

  // check we're at the end
  ASSERT(0 == reader.bytes_left());
}

/*!
 * Add an object file to the ObjectFileDB
 */
void ObjectFileDB::add_obj_from_dgo(const std::string& obj_name,
                                    const std::string& name_in_dgo,
                                    const uint8_t* obj_data,
                                    uint32_t obj_size,
                                    const std::string& dgo_name,
                                    const Config& config) {
  if (config.banned_objects.find(obj_name) != config.banned_objects.end()) {
    return;
  }
  if (!config.allowed_objects.empty()) {
    if (config.allowed_objects.find(obj_name) == config.allowed_objects.end()) {
      return;
    }
  }
  stats.total_obj_files++;
  ASSERT(obj_size > 128);
  uint16_t version = *(const uint16_t*)(obj_data + 8);
  auto hash = crc32(obj_data, obj_size);

  bool duplicated = false;
  // first, check to see if we already got it...
  for (auto& e : obj_files_by_name[obj_name]) {
    if (e.data.size() == obj_size && e.record.hash == hash) {
      // just to make sure we don't have a hash collision.
      ASSERT(!memcmp(obj_data, e.data.data(), obj_size));

      // already got it!
      e.reference_count++;
      auto& rec = e.record;
      ASSERT(name_in_dgo == e.name_in_dgo);
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
  ObjectFileData data(config.game_version);
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
      lg::error("Object {} is from DGO {}, but this DGO was not in the map.", obj_name, dgo_name);
      ASSERT(false);
    }

    auto name_kv = dgo_kv->second.find(obj_name);
    if (name_kv == dgo_kv->second.end()) {
      lg::error("Object {} from DGO {} was not found in the name map.", obj_name, dgo_name);
      ASSERT(false);
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
      auto& obj = lookup_record(obj_rec);
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
  if (in.length() < length) {
    return in + std::string(length - in.length(), ' ');
  } else {
    return in;
  }
}
}  // namespace

std::string ObjectFileDB::generate_obj_listing(const std::unordered_set<std::string>& merged_objs) {
  std::string result = "[";
  std::set<std::string> all_unique_names;
  int unique_count = 0;
  for (auto& obj_file : obj_file_order) {
    for (auto& x : obj_files_by_name.at(obj_file)) {
      std::string dgos = "[";
      for (auto& y : x.dgo_names) {
        ASSERT(y.length() >= 5);
        std::string new_str = y == "NO-XGO" ? y : y.substr(0, y.length() - 4);
        dgos += "\"" + new_str + "\", ";
      }
      dgos.pop_back();
      dgos.pop_back();
      dgos += "]";
      auto name = x.to_unique_name();
      result += "[\"" + pad_string(name + "\", ", 50) + "\"" +
                pad_string(x.name_in_dgo + "\", ", 50) + std::to_string(x.obj_version) + ", " +
                dgos + ", \"\"],\n";
      if (all_unique_names.find(name) != all_unique_names.end() &&
          merged_objs.find(name) == merged_objs.end()) {
        lg::error("Object file {} appears multiple times with the same name.", name);
      }
      if (merged_objs.find(name) == merged_objs.end() ||
          all_unique_names.find(name) == all_unique_names.end()) {
        unique_count++;
      }
      all_unique_names.insert(name);
    }
  }
  // this check is extremely important. It makes sure we don't have any repeat names. This could
  // be caused by two files with the same name, in the same DGOs, but different data.
  if (int(all_unique_names.size()) != unique_count) {
    lg::error(
        "Object files are not named properly, data will be lost! Got {} objs, but only {} names\n",
        unique_count, all_unique_names.size());
  }

  if (unique_count > 0) {
    result.pop_back();  // kill last new line
    result.pop_back();  // kill last comma
  }
  return result + "]";
}

/*!
 * Process all of the linking data of all objects.
 */
void ObjectFileDB::process_link_data(const Config& config) {
  lg::info("Processing Link Data...");
  Timer process_link_timer;

  LinkedObjectFile::Stats combined_stats;

  for_each_obj([&](ObjectFileData& obj) {
    obj.linked_data = to_linked_object_file(obj.data, obj.record.name, dts, config.game_version);
    combined_stats.add(obj.linked_data.stats);
  });

  lg::info("Processed Link Data");
  lg::info(" Total {:.2f} ms\n", process_link_timer.getMs());
  // printf("\n");
}

/*!
 * Process all of the labels generated from linking and give them reasonable names.
 */
void ObjectFileDB::process_labels() {
  lg::info("Processing Labels...");
  Timer process_label_timer;
  uint32_t total = 0;
  for_each_obj([&](ObjectFileData& obj) { total += obj.linked_data.set_ordered_label_names(); });

  lg::info("Processed Labels:");
  lg::info(" Total {} labels", total);
  lg::info(" Total {:.2f} ms\n", process_label_timer.getMs());
}

/*!
 * Dump object files and their linking data to text files for debugging
 */
void ObjectFileDB::write_object_file_words(const fs::path& output_dir,
                                           bool dump_data,
                                           bool dump_code) {
  lg::info("- Writing object file dumps (code? {} data? {})...", dump_code, dump_data);

  Timer timer;
  uint32_t total_bytes = 0, total_files = 0;

  for_each_obj([&](ObjectFileData& obj) {
    if ((obj.linked_data.segments == 3 && dump_code) ||
        (obj.linked_data.segments != 3 && dump_data)) {
      auto file_text = obj.linked_data.print_words();
      auto file_name = output_dir / (obj.to_unique_name() + ".txt");
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
}

/*!
 * Dump disassembly for object files containing code.  Data zones will also be dumped.
 */
void ObjectFileDB::write_disassembly(const fs::path& output_dir,
                                     bool disassemble_data,
                                     bool disassemble_code,
                                     bool print_hex) {
  lg::info("- Writing functions...");
  Timer timer;
  uint32_t total_bytes = 0, total_files = 0;

  std::string asm_functions;

  for_each_obj([&](ObjectFileData& obj) {
    if ((obj.obj_version == 3 && disassemble_code) || (obj.obj_version != 3 && disassemble_data)) {
      auto file_text = obj.linked_data.print_disassembly(print_hex);
      asm_functions += obj.linked_data.print_asm_function_disassembly(obj.to_unique_name());
      auto file_name = output_dir / (obj.to_unique_name() + ".asm");

      total_bytes += file_text.size();
      file_util::write_text_file(file_name, file_text);
      total_files++;
    }
  });

  total_bytes += asm_functions.size();
  total_files++;
  file_util::write_text_file(output_dir / "asm_functions.func", asm_functions);

  lg::info("Wrote functions dumps:");
  lg::info(" Total {} files", total_files);
  lg::info(" Total {} MB", total_bytes / ((float)(1u << 20u)));
  lg::info(" Total {} ms ({:.3f} MB/sec)", timer.getMs(),
           total_bytes / ((1u << 20u) * timer.getSeconds()));
}

/*!
 * Find code/data zones, identify functions, and disassemble
 */
void ObjectFileDB::find_code(const Config& config) {
  lg::info("Finding code in object files...");
  LinkedObjectFile::Stats combined_stats;
  Timer timer;

  for_each_obj([&](ObjectFileData& obj) {
    //      printf("fc %s\n", obj.record.to_unique_name().c_str());
    obj.linked_data.find_code();
    obj.linked_data.find_functions(config.game_version);
    obj.linked_data.disassemble_functions();

    if (config.game_version == GameVersion::Jak1 || obj.to_unique_name() != "effect-control-v0") {
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
  lg::info(" Total {:.3f} ms\n", timer.getMs());
}

/*!
 * Finds and writes all scripts into a file named all_scripts.lisp.
 * Doesn't change any state in ObjectFileDB.
 */
void ObjectFileDB::find_and_write_scripts(const fs::path& output_dir) {
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

  auto file_name = output_dir / "all_scripts.lisp";
  file_util::write_text_file(file_name, all_scripts);

  lg::info("Found scripts:");
  lg::info(" Total {:.3f} ms\n", timer.getMs());
}

std::string ObjectFileDB::process_tpages(TextureDB& tex_db, const fs::path& output_path) {
  lg::info("- Finding textures in tpages...");
  std::string tpage_string = "tpage-";
  int total = 0, success = 0;
  int tpage_dir_count = 0;
  u64 total_px = 0;
  Timer timer;

  std::string result;
  for_each_obj([&](ObjectFileData& data) {
    if (data.name_in_dgo.substr(0, tpage_string.length()) == tpage_string) {
      auto statistics = process_tpage(data, tex_db, output_path);
      total += statistics.total_textures;
      success += statistics.successful_textures;
      total_px += statistics.num_px;
    } else if (data.name_in_dgo == "dir-tpages") {
      result = process_dir_tpages(data).to_source();
      tpage_dir_count++;
    }
  });

  ASSERT(tpage_dir_count <= 1);

  lg::info("Processed {} / {} textures ({} px) {:.2f}% in {:.2f} ms", success, total, total_px,
           100.f * float(success) / float(total), timer.getMs());

  if (tpage_dir_count == 0) {
    lg::warn("Did not find tpage-dir.");
    return {};
  }
  return result;
}

std::string ObjectFileDB::process_game_text_files(const Config& cfg) {
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
      auto statistics = process_game_text(data, cfg.text_version);
      string_count += statistics.total_text;
      char_count += statistics.total_chars;
      if (text_by_language_by_id.find(statistics.language) != text_by_language_by_id.end()) {
        ASSERT(false);
      }
      text_by_language_by_id[statistics.language] = std::move(statistics.text);
    }
  });

  lg::info("Processed {} text files ({} strings, {} characters) in {:.2f} ms", file_count,
           string_count, char_count, timer.getMs());

  if (text_by_language_by_id.empty()) {
    return {};
  }
  return write_game_text(cfg.text_version, text_by_language_by_id);
}

std::string ObjectFileDB::process_game_count_file() {
  lg::info("- Finding game count file...");
  bool found = false;
  std::string result;

  for_each_obj([&](ObjectFileData& data) {
    if (data.name_in_dgo == "game-cnt") {
      ASSERT(!found);
      found = true;
      result = write_game_count(process_game_count(data));
    }
  });

  if (!found) {
    lg::warn("did not find game-cnt file");
  }

  return result;
}

namespace {
void get_art_info(ObjectFileDB& db, ObjectFileData& obj) {
  if (obj.obj_version == 4) {
    const auto& words = obj.linked_data.words_by_seg.at(MAIN_SEGMENT);
    if (words.at(0).kind() == LinkedWord::Kind::TYPE_PTR &&
        words.at(0).symbol_name() == "art-group") {
      // fmt::print("art-group {}:\n", obj.to_unique_name());
      auto name = obj.linked_data.get_goal_string_by_label(words.at(2).label_id());
      int length = words.at(3).data;
      // fmt::print("  length: {}\n", length);
      std::unordered_map<int, std::string> art_group_elts;
      for (int i = 0; i < length; ++i) {
        const auto& word = words.at(8 + i);
        if (word.kind() == LinkedWord::Kind::SYM_PTR && word.symbol_name() == "#f") {
          continue;
        }
        const auto& label = obj.linked_data.labels.at(word.label_id());
        auto elt_name =
            obj.linked_data.get_goal_string_by_label(words.at(label.offset / 4 + 1).label_id());
        std::string elt_type = words.at(label.offset / 4 - 1).symbol_name();
        std::string unique_name = elt_name;
        if (elt_type == "art-joint-geo") {
          // the skeleton!
          unique_name += "-jg";
        } else if (elt_type == "merc-ctrl" || elt_type == "shadow-geo") {
          // (maybe mesh-geo as well but that doesnt exist)
          // the skin!
          unique_name += "-mg";
        } else if (elt_type == "art-joint-anim") {
          // the animations!
          unique_name += "-ja";
        } else {
          // the something idk!
          throw std::runtime_error(
              fmt::format("unknown art elt type {} in {}", elt_type, obj.to_unique_name()));
        }
        art_group_elts[i] = unique_name;
        // fmt::print("  {}: {} ({}) -> {}\n", i, elt_name, elt_type, unique_name);
      }
      db.dts.art_group_info[obj.to_unique_name()] = art_group_elts;
    }
  }
}
}  // namespace

/*!
 * Get information from art groups.
 */
void ObjectFileDB::extract_art_info() {
  lg::info("Processing art groups...");
  Timer timer;

  for_each_obj([&](ObjectFileData& obj) {
    try {
      get_art_info(*this, obj);
    } catch (std::runtime_error& e) {
      lg::warn("Error when extracting art group info: {}", e.what());
    }
  });

  lg::info("Processed art groups: in {:.2f} ms\n", timer.getMs());
}

/*!
 * Write out the art group information.
 */
void ObjectFileDB::dump_art_info(const fs::path& output_dir) {
  lg::info("Writing art group info...");
  Timer timer;

  if (!dts.art_group_info.empty()) {
    file_util::create_dir_if_needed(output_dir / "import");
  }
  for (const auto& [ag_name, info] : dts.art_group_info) {
    auto ag_fname = ag_name + ".gc";
    auto filename = output_dir / "import" / ag_fname;
    std::string result = ";;-*-Lisp-*-\n";
    result += "(in-package goal)\n\n";
    result += fmt::format(";; {} - art group OpenGOAL import file\n", ag_fname);
    result += ";; THIS FILE IS AUTOMATICALLY GENERATED!\n\n";
    for (const auto& [idx, elt_name] : info) {
      result += print_art_elt_for_dump(ag_name, elt_name, idx);
    }
    result += "\n";
    file_util::write_text_file(filename, result);
  }

  lg::info("Written art group info: in {:.2f} ms\n", timer.getMs());
}

void ObjectFileDB::dump_raw_objects(const fs::path& output_dir) {
  for_each_obj([&](ObjectFileData& data) {
    auto dest = output_dir / data.to_unique_name();
    if (data.obj_version != 3) {
      dest += ".go";
    }
    file_util::write_binary_file(dest, data.data.data(), data.data.size());
  });
}

std::string print_art_elt_for_dump(const std::string& group_name,
                                   const std::string& name,
                                   int idx) {
  return fmt::format("(def-art-elt {} {} {})\n", group_name, name, idx);
}
}  // namespace decompiler
