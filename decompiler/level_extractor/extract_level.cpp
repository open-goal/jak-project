#include <set>

#include "extract_level.h"
#include "decompiler/level_extractor/BspHeader.h"
#include "decompiler/level_extractor/extract_tfrag.h"

namespace decompiler {

/*!
 * Look through files in a DGO and find the bsp-header file (the level)
 */
ObjectFileRecord get_bsp_file(const std::vector<ObjectFileRecord>& records) {
  ObjectFileRecord result;
  bool found = false;
  for (auto& file : records) {
    if (file.name.length() > 4 && file.name.substr(file.name.length() - 4) == "-vis") {
      assert(!found);
      found = true;
      result = file;
    }
  }
  assert(found);
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
  if (first_word.kind != decompiler::LinkedWord::TYPE_PTR) {
    fmt::print("Expected the first word to be a type pointer, but it wasn't.\n");
    return false;
  }

  if (first_word.symbol_name != "bsp-header") {
    fmt::print("Expected to get a bsp-header, but got {} instead.\n", first_word.symbol_name);
    return false;
  }

  return true;
}

void extract_from_level(ObjectFileDB& db, const std::string& dgo_name) {
  fmt::print("Extract from {}\n", dgo_name);
  auto bsp_rec = get_bsp_file(db.obj_files_by_dgo.at(dgo_name));
  fmt::print("found bsp file: {}\n", bsp_rec.name);
  auto& bsp_file = db.lookup_record(bsp_rec);
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
  for (auto& draw_tree : bsp_header.drawable_tree_array.trees) {
    fmt::print("tree: {}\n", draw_tree->my_type());
    if (tfrag_trees.count(draw_tree->my_type())) {
      auto as_tfrag_tree = dynamic_cast<level_tools::DrawableTreeTfrag*>(draw_tree.get());
      fmt::print("  Is a tfrag tree!\n");
      assert(as_tfrag_tree);
      extract_tfrag(as_tfrag_tree, fmt::format("{}-{}", dgo_name, i++));
    }
  }
}
}  // namespace decompiler
