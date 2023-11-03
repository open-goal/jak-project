#include "extract_level.h"

#include <set>
#include <thread>

#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/SimpleThreadGroup.h"
#include "common/util/compress.h"
#include "common/util/string_util.h"

#include "decompiler/level_extractor/BspHeader.h"
#include "decompiler/level_extractor/extract_actors.h"
#include "decompiler/level_extractor/extract_collide_frags.h"
#include "decompiler/level_extractor/extract_joint_group.h"
#include "decompiler/level_extractor/extract_merc.h"
#include "decompiler/level_extractor/extract_shrub.h"
#include "decompiler/level_extractor/extract_tfrag.h"
#include "decompiler/level_extractor/extract_tie.h"
#include "decompiler/level_extractor/fr3_to_gltf.h"

namespace decompiler {

/*!
 * Look through files in a DGO and find the bsp-header file (the level)
 */
std::optional<ObjectFileRecord> get_bsp_file(const std::vector<ObjectFileRecord>& records,
                                             const std::string& dgo_name) {
  std::optional<ObjectFileRecord> result;
  bool found = false;
  for (auto& file : records) {
    if (file.name.length() > 4 && file.name.substr(file.name.length() - 4) == "-vis") {
      ASSERT(!found);
      found = true;
      result = file;
    }
  }

  if (!result) {
    if (str_util::ends_with(dgo_name, ".DGO") || str_util::ends_with(dgo_name, ".CGO")) {
      auto expected_name = dgo_name.substr(0, dgo_name.length() - 4);
      for (auto& c : expected_name) {
        c = tolower(c);
      }
      if (!records.empty() && expected_name == records.back().name) {
        return records.back();
      }
    }
  }
  return result;
}

/*!
 * Make sure a file is a valid bsp-header.
 */
bool is_valid_bsp(const decompiler::LinkedObjectFile& file) {
  if (file.segments != 1) {
    lg::error("Got {} segments, but expected 1", file.segments);
    return false;
  }

  auto& first_word = file.words_by_seg.at(0).at(0);
  if (first_word.kind() != decompiler::LinkedWord::TYPE_PTR) {
    lg::error("Expected the first word to be a type pointer, but it wasn't.");
    return false;
  }

  if (first_word.symbol_name() != "bsp-header") {
    lg::error("Expected to get a bsp-header, but got {} instead.", first_word.symbol_name());
    return false;
  }

  return true;
}

tfrag3::Texture make_texture(u32 id,
                             const TextureDB::TextureData& tex,
                             const std::string& tpage_name,
                             bool pool_load) {
  tfrag3::Texture new_tex;
  new_tex.combo_id = id;
  new_tex.w = tex.w;
  new_tex.h = tex.h;
  new_tex.debug_tpage_name = tpage_name;
  new_tex.debug_name = tex.name;
  new_tex.data = tex.rgba_bytes;
  new_tex.combo_id = id;
  new_tex.load_to_pool = pool_load;
  return new_tex;
}

void add_all_textures_from_level(tfrag3::Level& lev,
                                 const std::string& level_name,
                                 const TextureDB& tex_db) {
  ASSERT(lev.textures.empty());
  const auto& level_it = tex_db.texture_ids_per_level.find(level_name);
  if (level_it != tex_db.texture_ids_per_level.end()) {
    for (auto id : level_it->second) {
      const auto& tex = tex_db.textures.at(id);
      lev.textures.push_back(make_texture(id, tex, tex_db.tpage_names.at(tex.page), true));
    }
  }
}

void confirm_textures_identical(const TextureDB& tex_db) {
  std::unordered_map<std::string, std::vector<u32>> tex_dupl;
  for (auto& tex : tex_db.textures) {
    auto name = tex_db.tpage_names.at(tex.second.page) + tex.second.name;
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

void extract_art_groups_from_level(const ObjectFileDB& db,
                                   const TextureDB& tex_db,
                                   const std::vector<level_tools::TextureRemap>& tex_remap,
                                   const std::string& dgo_name,
                                   tfrag3::Level& level_data,
                                   std::map<std::string, level_tools::ArtData>& art_group_data) {
  const auto& files = db.obj_files_by_dgo.at(dgo_name);
  for (const auto& file : files) {
    if (file.name.length() > 3 && !file.name.compare(file.name.length() - 3, 3, "-ag")) {
      const auto& ag_file = db.lookup_record(file);
      extract_merc(ag_file, tex_db, db.dts, tex_remap, level_data, false, db.version());
      extract_joint_group(ag_file, db.dts, db.version(), art_group_data);
    }
  }
}

std::vector<level_tools::TextureRemap> extract_tex_remap(const ObjectFileDB& db,
                                                         const std::string& dgo_name) {
  auto bsp_rec = get_bsp_file(db.obj_files_by_dgo.at(dgo_name), dgo_name);
  if (!bsp_rec) {
    lg::warn("Skipping extract for {} because the BSP file was not found", dgo_name);
    return {};
  }
  std::string level_name = bsp_rec->name.substr(0, bsp_rec->name.length() - 4);

  lg::info("Processing level {} ({})", dgo_name, level_name);
  const auto& bsp_file = db.lookup_record(*bsp_rec);
  bool ok = is_valid_bsp(bsp_file.linked_data);
  ASSERT(ok);

  level_tools::DrawStats draw_stats;
  level_tools::BspHeader bsp_header;
  bsp_header.read_from_file(bsp_file.linked_data, db.dts, &draw_stats, db.version(), true);

  return bsp_header.texture_remap_table;
}

level_tools::BspHeader extract_bsp_from_level(const ObjectFileDB& db,
                                              const TextureDB& tex_db,
                                              const std::string& dgo_name,
                                              const DecompileHacks& hacks,
                                              bool extract_collision,
                                              tfrag3::Level& level_data) {
  auto bsp_rec = get_bsp_file(db.obj_files_by_dgo.at(dgo_name), dgo_name);
  if (!bsp_rec) {
    lg::warn("Skipping extract for {} because the BSP file was not found", dgo_name);
    return {};
  }
  std::string level_name = bsp_rec->name.substr(0, bsp_rec->name.length() - 4);

  lg::info("Processing level {} ({})", dgo_name, level_name);
  const auto& bsp_file = db.lookup_record(*bsp_rec);
  bool ok = is_valid_bsp(bsp_file.linked_data);
  ASSERT(ok);

  level_tools::DrawStats draw_stats;
  // draw_stats.debug_print_dma_data = true;
  level_tools::BspHeader bsp_header;
  bsp_header.read_from_file(bsp_file.linked_data, db.dts, &draw_stats, db.version());
  ASSERT((int)bsp_header.drawable_tree_array.trees.size() == bsp_header.drawable_tree_array.length);

  /*
  level_tools::PrintSettings settings;
  settings.expand_collide = true;
  lg::print("{}\n", bsp_header.print(settings));
   */

  const std::set<std::string> tfrag_trees = {
      "drawable-tree-tfrag",        "drawable-tree-trans-tfrag",       "drawable-tree-tfrag-trans",
      "drawable-tree-dirt-tfrag",   "drawable-tree-tfrag-water",       "drawable-tree-ice-tfrag",
      "drawable-tree-lowres-tfrag", "drawable-tree-lowres-trans-tfrag"};
  int i = 0;

  std::vector<const level_tools::DrawableTreeInstanceTie*> all_ties;
  for (auto& draw_tree : bsp_header.drawable_tree_array.trees) {
    auto as_tie_tree = dynamic_cast<level_tools::DrawableTreeInstanceTie*>(draw_tree.get());
    if (as_tie_tree) {
      all_ties.push_back(as_tie_tree);
    }
  }

  bool got_collide = false;
  for (auto& draw_tree : bsp_header.drawable_tree_array.trees) {
    if (tfrag_trees.count(draw_tree->my_type())) {
      auto as_tfrag_tree = dynamic_cast<level_tools::DrawableTreeTfrag*>(draw_tree.get());
      ASSERT(as_tfrag_tree);
      std::vector<std::pair<int, int>> expected_missing_textures;
      auto it = hacks.missing_textures_by_level.find(level_name);
      if (it != hacks.missing_textures_by_level.end()) {
        expected_missing_textures = it->second;
      }
      bool atest_disable_flag = false;
      if (db.version() == GameVersion::Jak2) {
        if (bsp_header.texture_flags[0] & 1) {
          atest_disable_flag = true;
        }
      }
      extract_tfrag(as_tfrag_tree, fmt::format("{}-{}", dgo_name, i++),
                    bsp_header.texture_remap_table, tex_db, expected_missing_textures, level_data,
                    false, level_name, atest_disable_flag);
    } else if (draw_tree->my_type() == "drawable-tree-instance-tie") {
      auto as_tie_tree = dynamic_cast<level_tools::DrawableTreeInstanceTie*>(draw_tree.get());
      ASSERT(as_tie_tree);
      extract_tie(as_tie_tree, fmt::format("{}-{}-tie", dgo_name, i++),
                  bsp_header.texture_remap_table, tex_db, level_data, false, db.version());
    } else if (draw_tree->my_type() == "drawable-tree-instance-shrub") {
      auto as_shrub_tree =
          dynamic_cast<level_tools::shrub_types::DrawableTreeInstanceShrub*>(draw_tree.get());
      ASSERT(as_shrub_tree);
      extract_shrub(as_shrub_tree, fmt::format("{}-{}-shrub", dgo_name, i++),
                    bsp_header.texture_remap_table, tex_db, {}, level_data, false, db.version());
    } else if (draw_tree->my_type() == "drawable-tree-collide-fragment" && extract_collision) {
      auto as_collide_frags =
          dynamic_cast<level_tools::DrawableTreeCollideFragment*>(draw_tree.get());
      ASSERT(as_collide_frags);
      ASSERT(!got_collide);
      got_collide = true;
      extract_collide_frags(as_collide_frags, all_ties, fmt::format("{}-{}-collide", dgo_name, i++),
                            level_data, false);
    } else {
      lg::print("  unsupported tree {}\n", draw_tree->my_type());
    }
  }

  if (bsp_header.collide_hash.num_items) {
    ASSERT(!got_collide);
    extract_collide_frags(bsp_header.collide_hash, all_ties, db.dts, level_data);
  }
  level_data.level_name = level_name;

  return bsp_header;
}

/*!
 * Extract stuff found in GAME.CGO.
 * Even though GAME.CGO isn't technically a level, the decompiler/loader treat it like one,
 * but the bsp stuff is just empty. It will contain only textures/art groups.
 */
void extract_common(const ObjectFileDB& db,
                    const TextureDB& tex_db,
                    const std::string& dgo_name,
                    bool dump_levels,
                    const fs::path& output_folder,
                    const Config& config) {
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
  std::map<std::string, level_tools::ArtData> art_group_data;
  add_all_textures_from_level(tfrag_level, dgo_name, tex_db);
  extract_art_groups_from_level(db, tex_db, {}, dgo_name, tfrag_level, art_group_data);

  std::set<std::string> textures_we_have;

  // put _all_ index textures in common.
  for (const auto& [id, tex] : tex_db.index_textures_by_combo_id) {
    tfrag_level.index_textures.push_back(tex);
  }

  for (const auto& t : tfrag_level.textures) {
    textures_we_have.insert(t.debug_name);
  }

  for (const auto& [id, normal_texture] : tex_db.textures) {
    if (config.common_tpages.count(normal_texture.page) &&
        !textures_we_have.count(normal_texture.name)) {
      textures_we_have.insert(normal_texture.name);
      tfrag_level.textures.push_back(
          make_texture(id, normal_texture, tex_db.tpage_names.at(normal_texture.page), true));
    }
  }

  // add animated textures that are missing.
  for (const auto& [id, normal_texture] : tex_db.textures) {
    if (config.animated_textures.count(normal_texture.name) &&
        !textures_we_have.count(normal_texture.name)) {
      textures_we_have.insert(normal_texture.name);
      tfrag_level.textures.push_back(
          make_texture(id, normal_texture, tex_db.tpage_names.at(normal_texture.page), false));
    }
  }

  Serializer ser;
  tfrag_level.serialize(ser);
  auto compressed =
      compression::compress_zstd(ser.get_save_result().first, ser.get_save_result().second);

  lg::info("stats for {}", dgo_name);
  print_memory_usage(tfrag_level, ser.get_save_result().second);
  lg::info("compressed: {} -> {} ({:.2f}%)", ser.get_save_result().second, compressed.size(),
           100.f * compressed.size() / ser.get_save_result().second);
  file_util::write_binary_file(
      output_folder / fmt::format("{}.fr3", dgo_name.substr(0, dgo_name.length() - 4)),
      compressed.data(), compressed.size());

  if (dump_levels) {
    auto file_path = file_util::get_jak_project_dir() / "glb_out" / "common.glb";
    file_util::create_dir_if_needed_for_file(file_path);
    save_level_foreground_as_gltf(tfrag_level, art_group_data, file_path);
  }
}

void extract_from_level(const ObjectFileDB& db,
                        const TextureDB& tex_db,
                        const std::string& dgo_name,
                        const Config& config,
                        bool dump_level,
                        bool extract_collision,
                        const fs::path& output_folder,
                        const fs::path& entities_folder) {
  if (db.obj_files_by_dgo.count(dgo_name) == 0) {
    lg::warn("Skipping extract for {} because the DGO was not part of the input", dgo_name);
    return;
  }
  tfrag3::Level level_data;
  std::map<std::string, level_tools::ArtData> art_group_data;
  add_all_textures_from_level(level_data, dgo_name, tex_db);

  // the bsp header file data
  auto bsp_header =
      extract_bsp_from_level(db, tex_db, dgo_name, config.hacks, extract_collision, level_data);
  extract_art_groups_from_level(db, tex_db, bsp_header.texture_remap_table, dgo_name, level_data,
                                art_group_data);

  Serializer ser;
  level_data.serialize(ser);
  auto compressed =
      compression::compress_zstd(ser.get_save_result().first, ser.get_save_result().second);
  lg::info("stats for {}", dgo_name);
  print_memory_usage(level_data, ser.get_save_result().second);
  lg::info("compressed: {} -> {} ({:.2f}%)", ser.get_save_result().second, compressed.size(),
           100.f * compressed.size() / ser.get_save_result().second);
  file_util::write_binary_file(
      output_folder / fmt::format("{}.fr3", dgo_name.substr(0, dgo_name.length() - 4)),
      compressed.data(), compressed.size());

  if (dump_level) {
    auto back_file_path = file_util::get_jak_project_dir() / "glb_out" /
                          fmt::format("{}_background.glb", level_data.level_name);
    file_util::create_dir_if_needed_for_file(back_file_path);
    save_level_background_as_gltf(level_data, back_file_path);
    auto fore_file_path = file_util::get_jak_project_dir() / "glb_out" /
                          fmt::format("{}_foreground.glb", level_data.level_name);
    file_util::create_dir_if_needed_for_file(fore_file_path);
    save_level_foreground_as_gltf(level_data, art_group_data, fore_file_path);
  }
  file_util::write_text_file(entities_folder / fmt::format("{}_actors.json", level_data.level_name),
                             extract_actors_to_json(bsp_header.actors));
}

void extract_all_levels(const ObjectFileDB& db,
                        const TextureDB& tex_db,
                        const std::vector<std::string>& dgo_names,
                        const std::string& common_name,
                        const Config& config,
                        bool debug_dump_level,
                        bool extract_collision,
                        const fs::path& output_path) {
  extract_common(db, tex_db, common_name, debug_dump_level, output_path, config);
  auto entities_dir = file_util::get_jak_project_dir() / "decompiler_out" /
                      game_version_names[config.game_version] / "entities";
  file_util::create_dir_if_needed(entities_dir);
  SimpleThreadGroup threads;
  threads.run(
      [&](int idx) {
        extract_from_level(db, tex_db, dgo_names[idx], config, debug_dump_level, extract_collision,
                           output_path, entities_dir);
      },
      dgo_names.size());
  threads.join();
}

}  // namespace decompiler
