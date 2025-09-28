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
#include "common/texture/texture_slots.h"
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
#include "decompiler/data/game_subs.h"
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
                           const std::vector<fs::path>& str_tex_files,
                           const std::vector<fs::path>& str_art_files,
                           const Config& config)
    : dts(config.game_version), m_version(config.game_version) {
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
      lg::print("CRC for {} is: 0x{:X}\n", name, crc32(data.data(), data.size()));
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

  if (config.read_spools) {
    lg::info("-Loading {} streaming object files...", str_files.size());
    for (auto& obj : str_files) {
      StrFileReader reader(obj, version());
      // name from the file name
      std::string base_name = obj_filename_to_name(obj.string());
      // name from inside the file (this does a lot of sanity checking)
      auto obj_name = reader.get_full_name(base_name + ".STR");
      for (int i = 0; i < reader.chunk_count(); i++) {
        // append the chunk ID to the full name
        std::string name = obj_name + fmt::format("+{}", i);
        auto& data = reader.get_chunk(i);
        add_obj_from_dgo(name, name, data.data(), data.size(), "ALLSPOOL", config, obj_name);
      }
    }
  }

  if (!str_tex_files.empty()) {
    lg::info("-Loading {} streaming texture files...", str_tex_files.size());
    for (auto& obj : str_tex_files) {
      StrFileReader reader(obj, version());
      // name from the file name
      std::string base_name = obj_filename_to_name(obj.string());
      for (int i = 0; i < reader.chunk_count(); i++) {
        auto name = reader.get_chunk_texture_name(i);
        add_obj_from_dgo(name, name, reader.get_chunk(i).data(), reader.get_chunk(i).size(),
                         "TEXSPOOL", config, name);
      }
    }
  }

  if (!str_art_files.empty()) {
    lg::info("-Loading {} streaming art files...", str_art_files.size());
    for (auto& obj : str_art_files) {
      StrFileReader reader(obj, version());
      for (int i = 0; i < reader.chunk_count(); i++) {
        auto name = reader.get_chunk_art_name(i);
        add_obj_from_dgo(name, name, reader.get_chunk(i).data(), reader.get_chunk(i).size(),
                         "ARTSPOOL", config, name);
      }
    }
  }

  lg::info("ObjectFileDB Initialized");
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
 * Are two object files the same?
 * Unfortunately they seemed to have a memory bug in their art-group generator, so there's some
 * uninitialized padding bytes.
 */
bool are_objects_the_same(const std::string& obj_name,
                          size_t size_a,
                          const u8* a,
                          size_t size_b,
                          const u8* b) {
  if (size_a != size_b) {
    return false;
  }

  // if they are byte-for-byte the same, it's a match
  if (!memcmp(a, b, size_a)) {
    return true;
  }

  // if it's an art group...
  if (obj_name.size() > 3 && !obj_name.compare(obj_name.length() - 3, 3, "-ag")) {
    // count up the number of differing bytes, and the location of the first one.
    size_t first_diff = 0;
    size_t last_diff = 0;
    int num_diffs = 0;
    bool found_first_diff = false;
    for (size_t i = 0; i < size_a; i++) {
      if (a[i] != b[i]) {
        num_diffs++;
        last_diff = i;
        if (!found_first_diff) {
          first_diff = i;
          found_first_diff = true;
        }
      }
    }

    // find the gap between "code" (really data here) and link table. This has up to 15 bytes of
    // uninitialized memory.
    const auto* header = (const LinkHeaderV4*)a;
    int link_data_offset = header->code_size + sizeof(LinkHeaderV4);
    int start_off_diff_from_code_end = link_data_offset - (int)first_diff;
    if (num_diffs < 16 && start_off_diff_from_code_end < 16 && (int)last_diff < link_data_offset) {
      return true;
    }
  }
  return false;
}

/*!
 * Add an object file to the ObjectFileDB
 */
void ObjectFileDB::add_obj_from_dgo(const std::string& obj_name,
                                    const std::string& name_in_dgo,
                                    const uint8_t* obj_data,
                                    uint32_t obj_size,
                                    const std::string& dgo_name,
                                    const Config& config,
                                    const std::string& cut_name) {
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
    if (are_objects_the_same(obj_name, e.data.size(), e.data.data(), obj_size, obj_data)) {
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
  data.base_name_from_chunk = cut_name;
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
      for (auto& name : x.dgo_names) {
        ASSERT(name.length() >= 5);
        std::string new_str =
            (name == "NO-XGO" || name == "ALLSPOOL") ? name : name.substr(0, name.length() - 4);
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
  lg::info(" Total {:.2f} ms", process_link_timer.getMs());
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
  lg::info(" Total {:.2f} ms", process_label_timer.getMs());
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
    if (((obj.obj_version == 3 || (obj.obj_version == 5 && obj.linked_data.has_any_functions())) &&
         disassemble_code) ||
        (obj.obj_version != 3 && disassemble_data)) {
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
  lg::info(" Total {:.3f} ms", timer.getMs());
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
  lg::info(" Total {:.3f} ms", timer.getMs());
}

std::string ObjectFileDB::process_tpages(TextureDB& tex_db,
                                         const fs::path& output_path,
                                         const Config& cfg,
                                         const fs::path& dump_out) {
  lg::info("- Finding textures in tpages...");
  std::string tpage_string = "tpage-";
  int total = 0, success = 0;
  int tpage_dir_count = 0;
  u64 total_px = 0;
  Timer timer;

  std::vector<std::string> animated_slots;
  switch (m_version) {
    case GameVersion::Jak1:  // no animated
      break;
    case GameVersion::Jak2:
      animated_slots = jak2_animated_texture_slots();
      break;
    case GameVersion::Jak3:
      animated_slots = jak3_animated_texture_slots();
      break;
    default:
      ASSERT_NOT_REACHED();
  }

  for (size_t i = 0; i < animated_slots.size(); i++) {
    tex_db.animated_tex_output_to_anim_slot[animated_slots[i]] = i;
  }

  std::string result;
  for_each_obj([&](ObjectFileData& data) {
    if (data.name_in_dgo.substr(0, tpage_string.length()) == tpage_string) {
      auto statistics =
          process_tpage(data, tex_db, output_path, cfg.animated_textures, cfg.save_texture_pngs);
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

  if (cfg.write_tpage_imports) {
    file_util::create_dir_if_needed(dump_out);
    std::string tpage_dump;
    std::string tex_dump;
    for (auto& tpage : tex_db.tpage_names) {
      tpage_dump += print_tpage_for_dump(tpage.second, tpage.first);
    }
    for (auto& tex : tex_db.textures) {
      auto tpage_name = tex_db.tpage_names[tex.second.page];
      dts.textures.emplace(tex.first, TexInfo{tex.second.name, tpage_name, tex.first & 0x0000ffff});
      tex_dump += print_tex_for_dump(tex.second.name, tpage_name, tex.first & 0x0000ffff);
    }

    auto tpage_dump_out = dump_out / "tpages.gc";
    auto tex_dump_out = dump_out / "textures.gc";

    file_util::write_text_file(tpage_dump_out, tpage_dump);
    file_util::write_text_file(tex_dump_out, tex_dump);
  }

  return result;
}

std::string ObjectFileDB::process_all_spool_subtitles(const Config& cfg,
                                                      const fs::path& image_out) {
  try {
    lg::info("- Finding spool subtitles...");
    Timer timer;
    int obj_count = 0;
    int string_count = 0;
    int image_count = 0;
    int subs_count = 0;
    std::unordered_map<std::string, std::vector<SpoolSubtitleRange>> all_subs;

    for_each_obj_in_dgo("ALLSPOOL", [&](ObjectFileData& data) {
      int this_string_count = 0;
      int this_image_count = 0;
      obj_count++;
      auto this_spool_subs = process_spool_subtitles(data, cfg.text_version);
      if (!this_spool_subs.empty()) {
        for (auto& s : this_spool_subs) {
          subs_count++;
          for (int i = 0; i < 8; ++i) {
            if (s.message[i].kind == SpoolSubtitleMessage::Kind::IMAGE) {
              this_image_count++;
            } else if (s.message[i].kind == SpoolSubtitleMessage::Kind::STRING) {
              this_string_count++;
            }
          }
        }
        auto& spool_subs = all_subs[data.base_name_from_chunk];
        for (auto& x : this_spool_subs) {
          bool skip = false;
          for (auto& other : spool_subs) {
            skip |= other == x;
          }
          if (!skip) {
            all_subs[data.base_name_from_chunk].push_back(x);
            image_count += this_image_count;
            string_count += this_string_count;
          }
        }
      }
    });

    lg::info("Processed {} subtitles in {} spool objects ({} strings, {} images) in {:.2f} ms",
             subs_count, obj_count, string_count, image_count, timer.getMs());

    return write_spool_subtitles(cfg.text_version, image_out, all_subs);
  } catch (std::runtime_error& e) {
    lg::warn("Error when extracting spool subtitles: {}", e.what());
    return {};
  }
}

std::string ObjectFileDB::process_game_text_files(const Config& cfg) {
  try {
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
  } catch (std::runtime_error& e) {
    lg::warn("Error when extracting game text: {}", e.what());
    return {};
  }
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
struct JointGeo {
  u32 offset{};
  std::string name;
  u32 length{};
};

void get_joint_info(ObjectFileDB& db, ObjectFileData& obj, JointGeo jg) {
  const auto& words = obj.linked_data.words_by_seg.at(MAIN_SEGMENT);
  for (size_t i = 0; i < jg.length; ++i) {
    u32 label = 0x0;
    if (db.version() == GameVersion::Jak3) {
      label = words.at((jg.offset / 4) + 11 + i).label_id();
    } else {
      label = words.at((jg.offset / 4) + 7 + i).label_id();
    }
    const auto& joint = obj.linked_data.labels.at(label);
    const auto& name =
        obj.linked_data.get_goal_string_by_label(words.at(joint.offset / 4).label_id());
    // lg::print("{} joint idx {}/{}: {}\n", jg.name, i + 1, jg.length, name);
    db.dts.add_joint_node(jg.name, name, i + 1);
  }
}

void get_art_info(ObjectFileDB& db, ObjectFileData& obj) {
  // jak 1/2
  if (obj.obj_version == 4) {
    const auto& words = obj.linked_data.words_by_seg.at(MAIN_SEGMENT);
    if (words.at(0).kind() == LinkedWord::Kind::TYPE_PTR &&
        words.at(0).symbol_name() == "art-group") {
      auto obj_unique_name = obj.to_unique_name();

      // lg::print("art-group {}:\n", obj.to_unique_name());
      auto name = obj.linked_data.get_goal_string_by_label(words.at(2).label_id());
      int length = words.at(3).data;
      // lg::print("  length: {}\n", length);
      for (int i = 0; i < length; ++i) {
        const auto& word = words.at(8 + i);
        if (word.kind() == LinkedWord::Kind::SYM_PTR && word.symbol_name() == "#f") {
          continue;
        }
        const auto& label = obj.linked_data.labels.at(word.label_id());
        auto elt_name =
            obj.linked_data.get_goal_string_by_label(words.at(label.offset / 4 + 1).label_id());
        auto unique_name = elt_name;

        auto ag_name = obj_unique_name;
        int elt_index = i;
        auto& word_master_ag = words.at(label.offset / 4 + 7);
        auto& word_master_idx = words.at(label.offset / 4 + 8);
        if (word_master_ag.kind() == LinkedWord::Kind::PTR &&
            word_master_idx.kind() == LinkedWord::Kind::PLAIN_DATA) {
          ag_name = obj.linked_data.get_goal_string_by_label(word_master_ag.label_id()) + "-ag";
          elt_index = word_master_idx.data;
        }

        std::string elt_type = words.at(label.offset / 4 - 1).symbol_name();
        if (elt_type == "art-joint-geo") {
          // the skeleton!
          unique_name += "-jg";
          JointGeo jg;
          jg.offset = label.offset;
          jg.name = unique_name;
          jg.length = words.at(label.offset / 4 + 2).data;
          get_joint_info(db, obj, jg);
        } else if (elt_type == "merc-ctrl" || elt_type == "shadow-geo") {
          // (maybe mesh-geo as well but that doesnt exist)
          // the skin!
          unique_name += "-mg";
        } else if (elt_type == "art-joint-anim") {
          // the animations!
          unique_name += "-ja";
        } else if (elt_type == "art-cloth-geo") {
          // cloth geometry (jak 3)
          unique_name += "-cg";
        } else {
          // the something idk!
          throw std::runtime_error(
              fmt::format("unknown art elt type {} in {}", elt_type, obj.to_unique_name()));
        }
        // lg::print("  {}: {} ({}) -> {} @ {}\n", i, elt_name, elt_type, unique_name, elt_index);
        db.dts.add_art_group_elt(ag_name, unique_name, elt_index);
      }
    }
  }
  // jak 3
  if (obj.obj_version == 5 && obj.linked_data.segments == 1) {
    const auto& words = obj.linked_data.words_by_seg.at(MAIN_SEGMENT);
    if (words.at(0).kind() == LinkedWord::Kind::TYPE_PTR &&
        words.at(0).symbol_name() == "art-group") {
      auto obj_unique_name = obj.to_unique_name();

      // lg::print("art-group {}:\n", obj.to_unique_name());
      auto name = obj.linked_data.get_goal_string_by_label(words.at(2).label_id());
      int length = words.at(3).data;
      // lg::print("  length: {}\n", length);
      for (int i = 0; i < length; ++i) {
        const auto& word = words.at(8 + i);
        if (word.kind() == LinkedWord::Kind::SYM_PTR && word.symbol_name() == "#f") {
          continue;
        }
        const auto& label = obj.linked_data.labels.at(word.label_id());
        auto elt_name =
            obj.linked_data.get_goal_string_by_label(words.at(label.offset / 4 + 1).label_id());
        auto unique_name = elt_name;

        auto ag_name = obj_unique_name;
        int elt_index = i;
        std::string elt_type = words.at(label.offset / 4 - 1).symbol_name();
        auto& word_master_ag = words.at(label.offset / 4 + 4);
        auto& word_master_idx = words.at(label.offset / 4 + 5);
        if (word_master_ag.kind() == LinkedWord::Kind::PTR &&
            word_master_idx.kind() == LinkedWord::Kind::PLAIN_DATA) {
          ag_name = obj.linked_data.get_goal_string_by_label(word_master_ag.label_id()) + "-ag";
          if (elt_type != "art-cloth-geo") {
            elt_index = word_master_idx.data;
          }
        }

        if (elt_type == "art-joint-geo") {
          // the skeleton!
          unique_name += "-jg";
          JointGeo jg;
          jg.offset = label.offset;
          jg.name = unique_name;
          jg.length = words.at(label.offset / 4 + 2).data;
          get_joint_info(db, obj, jg);
        } else if (elt_type == "merc-ctrl" || elt_type == "shadow-geo") {
          // (maybe mesh-geo as well but that doesnt exist)
          // the skin!
          unique_name += "-mg";
        } else if (elt_type == "art-joint-anim") {
          // the animations!
          unique_name += "-ja";
        } else if (elt_type == "art-cloth-geo") {
          // cloth geometry (jak 3)
          unique_name += "-cg";
        } else {
          // the something idk!
          throw std::runtime_error(
              fmt::format("unknown art elt type {} in {}", elt_type, obj.to_unique_name()));
        }
        // lg::print("  {}: {} ({}) -> {} @ {}\n", i, elt_name, elt_type, unique_name, elt_index);
        db.dts.add_art_group_elt(ag_name, unique_name, elt_index);
      }
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

  lg::info("Processed art groups: in {:.2f} ms", timer.getMs());
}

/*!
 * Write out the art group information.
 */
void ObjectFileDB::dump_art_info(const fs::path& output_dir) {
  lg::info("Writing art group info...");
  Timer timer;

  if (!dts.art_group_info.empty() || !dts.jg_info.empty()) {
    file_util::create_dir_if_needed(output_dir / "import");
  }

  auto ag_fpath = output_dir / "import" / "art-elts.gc";
  std::string ag_result;

  for (const auto& [ag_name, info] : dts.art_group_info) {
    // auto ag_fname = ag_name + ".gc";
    // auto filename = output_dir / "import" / ag_fname;
    // std::string result = ";;-*-Lisp-*-\n";
    // result += "(in-package goal)\n\n";
    // result += fmt::format(";; {} - art group OpenGOAL import file\n", ag_fname);
    // result += ";; THIS FILE IS AUTOMATICALLY GENERATED!\n\n";
    for (const auto& [idx, elt_name] : info) {
      ag_result += print_art_elt_for_dump(ag_name, elt_name, idx);
    }
    ag_result += "\n";
  }

  file_util::write_text_file(ag_fpath, ag_result);

  auto jg_fpath = output_dir / "import" / "joint-nodes.gc";
  std::string jg_result;

  for (const auto& [jg_name, info] : dts.jg_info) {
    for (const auto& [idx, joint] : info) {
      jg_result += print_jg_for_dump(jg_name, joint, idx);
    }
    jg_result += "\n";
  }

  file_util::write_text_file(jg_fpath, jg_result);

  lg::info("Written art group info: in {:.2f} ms", timer.getMs());
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
std::string print_jg_for_dump(const std::string& jg_name, const std::string& joint_name, int idx) {
  return fmt::format("(def-joint-node {} \"{}\" {})\n", jg_name, joint_name, idx);
}
std::string print_tpage_for_dump(const std::string& debug_name, u32 id) {
  return fmt::format("(defconstant {} {})\n", debug_name, id);
}
std::string print_tex_for_dump(const std::string& name, const std::string& page_name, u32 idx) {
  return fmt::format("(def-tex {} {} {})\n", name, page_name, idx);
}
}  // namespace decompiler
