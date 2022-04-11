#include <set>

#include "extract_level.h"
#include "decompiler/level_extractor/BspHeader.h"
#include "decompiler/level_extractor/extract_tfrag.h"
#include "decompiler/level_extractor/extract_tie.h"
#include "decompiler/level_extractor/extract_shrub.h"
#include "common/util/compress.h"
#include "common/util/FileUtil.h"

namespace decompiler {

/*!
 * Look through files in a DGO and find the bsp-header file (the level)
 */
std::optional<ObjectFileRecord> get_bsp_file(const std::vector<ObjectFileRecord>& records) {
  std::optional<ObjectFileRecord> result;
  bool found = false;
  for (auto& file : records) {
    if (file.name.length() > 4 && file.name.substr(file.name.length() - 4) == "-vis") {
      ASSERT(!found);
      found = true;
      result = file;
    }
  }
  return result;
}

/*!
 * Make sure a file is a valid bsp-header.
 */
bool is_valid_bsp(const decompiler::LinkedObjectFile& file) {
  if (file.segments != 1) {
    fmt::print("Got {} segments, but expected 1\n", file.segments);
    return false;
  }

  auto& first_word = file.words_by_seg.at(0).at(0);
  if (first_word.kind() != decompiler::LinkedWord::TYPE_PTR) {
    fmt::print("Expected the first word to be a type pointer, but it wasn't.\n");
    return false;
  }

  if (first_word.symbol_name() != "bsp-header") {
    fmt::print("Expected to get a bsp-header, but got {} instead.\n", first_word.symbol_name());
    return false;
  }

  return true;
}

void print_memory_usage(const tfrag3::Level& lev, int uncompressed_data_size) {
  int total_accounted = 0;
  auto memory_use_by_category = lev.get_memory_usage();

  std::vector<std::pair<std::string, int>> known_categories = {
      {"texture", memory_use_by_category[tfrag3::MemoryUsageCategory::TEXTURE]},
      {"tie-deinst-vis", memory_use_by_category[tfrag3::MemoryUsageCategory::TIE_DEINST_VIS]},
      {"tie-deinst-idx", memory_use_by_category[tfrag3::MemoryUsageCategory::TIE_DEINST_INDEX]},
      {"tie-inst-vis", memory_use_by_category[tfrag3::MemoryUsageCategory::TIE_INST_VIS]},
      {"tie-inst-idx", memory_use_by_category[tfrag3::MemoryUsageCategory::TIE_INST_INDEX]},
      {"tie-bvh", memory_use_by_category[tfrag3::MemoryUsageCategory::TIE_BVH]},
      {"tie-verts", memory_use_by_category[tfrag3::MemoryUsageCategory::TIE_VERTS]},
      {"tie-colors", memory_use_by_category[tfrag3::MemoryUsageCategory::TIE_TIME_OF_DAY]},
      {"tie-wind-inst-info",
       memory_use_by_category[tfrag3::MemoryUsageCategory::TIE_WIND_INSTANCE_INFO]},
      {"tie-cidx", memory_use_by_category[tfrag3::MemoryUsageCategory::TIE_CIDX]},
      {"tie-mats", memory_use_by_category[tfrag3::MemoryUsageCategory::TIE_MATRICES]},
      {"tie-grps", memory_use_by_category[tfrag3::MemoryUsageCategory::TIE_GRPS]},
      {"tfrag-vis", memory_use_by_category[tfrag3::MemoryUsageCategory::TFRAG_VIS]},
      {"tfrag-idx", memory_use_by_category[tfrag3::MemoryUsageCategory::TFRAG_INDEX]},
      {"tfrag-vert", memory_use_by_category[tfrag3::MemoryUsageCategory::TFRAG_VERTS]},
      {"tfrag-colors", memory_use_by_category[tfrag3::MemoryUsageCategory::TFRAG_TIME_OF_DAY]},
      {"tfrag-cluster", memory_use_by_category[tfrag3::MemoryUsageCategory::TFRAG_CLUSTER]},
      {"tfrag-bvh", memory_use_by_category[tfrag3::MemoryUsageCategory::TFRAG_BVH]},
      {"shrub-colors", memory_use_by_category[tfrag3::MemoryUsageCategory::SHRUB_TIME_OF_DAY]},
      {"shrub-vert", memory_use_by_category[tfrag3::MemoryUsageCategory::SHRUB_VERT]},
      {"shrub-ind", memory_use_by_category[tfrag3::MemoryUsageCategory::SHRUB_IND]}};
  for (auto& known : known_categories) {
    total_accounted += known.second;
  }

  known_categories.push_back({"unknown", uncompressed_data_size - total_accounted});

  std::sort(known_categories.begin(), known_categories.end(),
            [](const auto& a, const auto& b) { return a.second > b.second; });

  for (const auto& x : known_categories) {
    fmt::print("{:30s} : {:6d} kB {:3.1f}%\n", x.first, x.second / 1024,
               100.f * (float)x.second / uncompressed_data_size);
  }
}

void add_all_textures_from_level(tfrag3::Level& lev,
                                 const std::string& level_name,
                                 TextureDB& tex_db) {
  ASSERT(lev.textures.empty());
  for (auto id : tex_db.texture_ids_per_level[level_name]) {
    const auto& tex = tex_db.textures.at(id);
    lev.textures.emplace_back();
    auto& new_tex = lev.textures.back();
    new_tex.combo_id = id;
    new_tex.w = tex.w;
    new_tex.h = tex.h;
    new_tex.debug_tpage_name = tex_db.tpage_names.at(tex.page);
    new_tex.debug_name = new_tex.debug_tpage_name + tex.name;
    new_tex.data = tex.rgba_bytes;
    new_tex.combo_id = id;
    new_tex.load_to_pool = true;
  }
}

void confirm_textures_identical(TextureDB& tex_db) {
  std::unordered_map<std::string, std::vector<u32>> tex_dupl;
  for (auto& tex : tex_db.textures) {
    auto name = tex_db.tpage_names[tex.second.page] + tex.second.name;
    auto it = tex_dupl.find(name);
    if (it == tex_dupl.end()) {
      tex_dupl.insert({name, tex.second.rgba_bytes});
    } else {
      bool ok = it->second == tex.second.rgba_bytes;
      if (!ok) {
        ASSERT_MSG(false, fmt::format("BAD duplicate: {} {} vs {}", name,
                                      tex.second.rgba_bytes.size(), it->second.size()));
      }
    }
  }
}

/*!
 * Extract common textures found in GAME.CGO
 */
void extract_common(ObjectFileDB& db, TextureDB& tex_db, const std::string& dgo_name) {
  if (db.obj_files_by_dgo.count(dgo_name) == 0) {
    lg::warn("Skipping common extract for {} because the DGO was not part of the input", dgo_name);
    return;
  }

  if (tex_db.textures.size() == 0) {
    lg::warn("Skipping common extract because there were no textures in the input");
    return;
  }

  confirm_textures_identical(tex_db);

  tfrag3::Level tfrag_level;
  add_all_textures_from_level(tfrag_level, dgo_name, tex_db);
  Serializer ser;
  tfrag_level.serialize(ser);
  auto compressed =
      compression::compress_zstd(ser.get_save_result().first, ser.get_save_result().second);
  print_memory_usage(tfrag_level, ser.get_save_result().second);
  fmt::print("compressed: {} -> {} ({:.2f}%)\n", ser.get_save_result().second, compressed.size(),
             100.f * compressed.size() / ser.get_save_result().second);
  file_util::write_binary_file(file_util::get_file_path({fmt::format(
                                   "assets/{}.fr3", dgo_name.substr(0, dgo_name.length() - 4))}),
                               compressed.data(), compressed.size());
}

void extract_from_level(ObjectFileDB& db,
                        TextureDB& tex_db,
                        const std::string& dgo_name,
                        const DecompileHacks& hacks,
                        bool dump_level) {
  if (db.obj_files_by_dgo.count(dgo_name) == 0) {
    lg::warn("Skipping extract for {} because the DGO was not part of the input", dgo_name);
    return;
  }

  auto bsp_rec = get_bsp_file(db.obj_files_by_dgo.at(dgo_name));
  if (!bsp_rec) {
    lg::warn("Skipping extract for {} because the BSP file was not found", dgo_name);
    return;
  }
  std::string level_name = bsp_rec->name.substr(0, bsp_rec->name.length() - 4);

  fmt::print("Processing level {} ({})\n", dgo_name, level_name);
  auto& bsp_file = db.lookup_record(*bsp_rec);
  bool ok = is_valid_bsp(bsp_file.linked_data);
  ASSERT(ok);

  level_tools::DrawStats draw_stats;
  // draw_stats.debug_print_dma_data = true;
  level_tools::BspHeader bsp_header;
  bsp_header.read_from_file(bsp_file.linked_data, db.dts, &draw_stats);
  ASSERT((int)bsp_header.drawable_tree_array.trees.size() == bsp_header.drawable_tree_array.length);

  /*
  level_tools::PrintSettings settings;
  settings.expand_shrub = true;
  fmt::print("{}\n", bsp_header.print(settings));
  */

  const std::set<std::string> tfrag_trees = {
      "drawable-tree-tfrag",     "drawable-tree-trans-tfrag",  "drawable-tree-dirt-tfrag",
      "drawable-tree-ice-tfrag", "drawable-tree-lowres-tfrag", "drawable-tree-lowres-trans-tfrag"};
  int i = 0;
  tfrag3::Level tfrag_level;

  add_all_textures_from_level(tfrag_level, dgo_name, tex_db);

  for (auto& draw_tree : bsp_header.drawable_tree_array.trees) {
    if (tfrag_trees.count(draw_tree->my_type())) {
      auto as_tfrag_tree = dynamic_cast<level_tools::DrawableTreeTfrag*>(draw_tree.get());
      ASSERT(as_tfrag_tree);
      std::vector<std::pair<int, int>> expected_missing_textures;
      auto it = hacks.missing_textures_by_level.find(level_name);
      if (it != hacks.missing_textures_by_level.end()) {
        expected_missing_textures = it->second;
      }
      extract_tfrag(as_tfrag_tree, fmt::format("{}-{}", dgo_name, i++),
                    bsp_header.texture_remap_table, tex_db, expected_missing_textures, tfrag_level,
                    dump_level);
    } else if (draw_tree->my_type() == "drawable-tree-instance-tie") {
      auto as_tie_tree = dynamic_cast<level_tools::DrawableTreeInstanceTie*>(draw_tree.get());
      ASSERT(as_tie_tree);
      extract_tie(as_tie_tree, fmt::format("{}-{}-tie", dgo_name, i++),
                  bsp_header.texture_remap_table, tex_db, tfrag_level, dump_level);
    } else if (draw_tree->my_type() == "drawable-tree-instance-shrub") {
      auto as_shrub_tree =
          dynamic_cast<level_tools::shrub_types::DrawableTreeInstanceShrub*>(draw_tree.get());
      ASSERT(as_shrub_tree);
      extract_shrub(as_shrub_tree, fmt::format("{}-{}-shrub", dgo_name, i++),
                    bsp_header.texture_remap_table, tex_db, {}, tfrag_level, dump_level);
    } else {
      // fmt::print("  unsupported tree {}\n", draw_tree->my_type());
    }
  }

  Serializer ser;
  tfrag_level.serialize(ser);
  auto compressed =
      compression::compress_zstd(ser.get_save_result().first, ser.get_save_result().second);
  print_memory_usage(tfrag_level, ser.get_save_result().second);
  fmt::print("compressed: {} -> {} ({:.2f}%)\n", ser.get_save_result().second, compressed.size(),
             100.f * compressed.size() / ser.get_save_result().second);
  file_util::write_binary_file(file_util::get_file_path({fmt::format(
                                   "assets/{}.fr3", dgo_name.substr(0, dgo_name.length() - 4))}),
                               compressed.data(), compressed.size());
}
}  // namespace decompiler
