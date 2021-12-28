#include <set>

#include "extract_level.h"
#include "decompiler/level_extractor/BspHeader.h"
#include "decompiler/level_extractor/extract_tfrag.h"
#include "decompiler/level_extractor/extract_tie.h"
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
      assert(!found);
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

void extract_from_level(ObjectFileDB& db,
                        TextureDB& tex_db,
                        const std::string& dgo_name,
                        const DecompileHacks& hacks) {
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
  assert(ok);

  level_tools::DrawStats draw_stats;
  // draw_stats.debug_print_dma_data = true;
  level_tools::BspHeader bsp_header;
  bsp_header.read_from_file(bsp_file.linked_data, db.dts, &draw_stats);
  assert((int)bsp_header.drawable_tree_array.trees.size() == bsp_header.drawable_tree_array.length);

  const std::set<std::string> tfrag_trees = {
      "drawable-tree-tfrag",     "drawable-tree-trans-tfrag",  "drawable-tree-dirt-tfrag",
      "drawable-tree-ice-tfrag", "drawable-tree-lowres-tfrag", "drawable-tree-lowres-trans-tfrag"};
  int i = 0;
  tfrag3::Level tfrag_level;

  for (auto& draw_tree : bsp_header.drawable_tree_array.trees) {
    if (tfrag_trees.count(draw_tree->my_type())) {
      auto as_tfrag_tree = dynamic_cast<level_tools::DrawableTreeTfrag*>(draw_tree.get());
      fmt::print("  extracting tree {}\n", draw_tree->my_type());
      assert(as_tfrag_tree);
      std::vector<std::pair<int, int>> expected_missing_textures;
      auto it = hacks.missing_textures_by_level.find(level_name);
      if (it != hacks.missing_textures_by_level.end()) {
        expected_missing_textures = it->second;
      }
      extract_tfrag(as_tfrag_tree, fmt::format("{}-{}", dgo_name, i++),
                    bsp_header.texture_remap_table, tex_db, expected_missing_textures, tfrag_level);
    } else if (draw_tree->my_type() == "drawable-tree-instance-tie") {
      fmt::print("  extracting TIE\n");
      auto as_tie_tree = dynamic_cast<level_tools::DrawableTreeInstanceTie*>(draw_tree.get());
      assert(as_tie_tree);
      extract_tie(as_tie_tree, fmt::format("{}-{}-tie", dgo_name, i++),
                  bsp_header.texture_remap_table, tex_db, tfrag_level);
    } else {
      fmt::print("  unsupported tree {}\n", draw_tree->my_type());
    }
  }

  Serializer ser;
  tfrag_level.serialize(ser);
  file_util::write_binary_file(file_util::get_file_path({fmt::format(
                                   "assets/{}.fr3", dgo_name.substr(0, dgo_name.length() - 4))}),
                               ser.get_save_result().first, ser.get_save_result().second);
}
}  // namespace decompiler
