#include "extract_merc.h"
#include "decompiler/util/goal_data_reader.h"
#include "decompiler/level_extractor/MercData.h"

namespace decompiler {


void extract_merc_ctrl(const LinkedObjectFile& file,
                       const DecompilerTypeSystem& dts,
                       int word_idx) {
  Ref ref;
  ref.data = &file;
  ref.seg = 0;
  ref.byte_offset = word_idx * 4;

  auto tr = typed_ref_from_basic(ref, dts);

  MercCtrl ctrl;
  ctrl.from_ref(tr, dts);
  fmt::print("{}\n", ctrl.print());
}

/*!
 * Find the word indices for the merc ctrls (the type tags)
 */
std::vector<int> find_merc_ctrls(const LinkedObjectFile& file) {
  std::vector<int> result;
  for (size_t i = 0; i < file.words_by_seg.at(0).size(); i++) {
    const auto& word = file.words_by_seg[0][i];
    if (word.kind() == LinkedWord::TYPE_PTR && word.symbol_name() == "merc-ctrl") {
      result.push_back(i);
    }
  }
  return result;
}

/*!
 * Top-level merc extraction
 */
void extract_merc(const ObjectFileData& ag_data,
                  const TextureDB& tex_db,
                  const DecompilerTypeSystem& dts,
                  tfrag3::Level& out,
                  bool dump_level) {
  fmt::print("MERC extract for: {}\n", ag_data.name_in_dgo);
  auto ctrls = find_merc_ctrls(ag_data.linked_data);
  fmt::print(" found {} merc ctrls\n", ctrls.size());
  for (auto ctrl : ctrls) {
    extract_merc_ctrl(ag_data.linked_data, dts, ctrl);
  }
}
}  // namespace decompiler
