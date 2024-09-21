#include "Tfrag.h"

#include <iostream>

#include "gltf_mesh_extract.h"

#include "common/custom_data/pack_helpers.h"
#include "common/util/gltf_util.h"

#include "goalc/data_compiler/DataObjectGenerator.h"

void add_tree(std::vector<tfrag3::TfragTree>& out_pc,
              const gltf_mesh_extract::TfragOutput& mesh_extract_out,
              const std::vector<tfrag3::StripDraw>& draws,
              tfrag3::TFragmentTreeKind kind) {
  auto& normal = out_pc.emplace_back();
  normal.kind = kind;
  normal.draws = draws;
  pack_tfrag_vertices(&normal.packed_vertices, mesh_extract_out.tfrag_vertices);
  normal.colors = gltf_util::pack_time_of_day(mesh_extract_out.color_palette);
  normal.use_strips = false;
}

void tfrag_from_gltf(const gltf_mesh_extract::TfragOutput& mesh_extract_out,
                     std::vector<tfrag3::TfragTree>& out_pc) {
  if (!mesh_extract_out.normal_strip_draws.empty()) {
    add_tree(out_pc, mesh_extract_out, mesh_extract_out.normal_strip_draws,
             tfrag3::TFragmentTreeKind::NORMAL);
  }

  if (!mesh_extract_out.trans_strip_draws.empty()) {
    add_tree(out_pc, mesh_extract_out, mesh_extract_out.trans_strip_draws,
             tfrag3::TFragmentTreeKind::TRANS);
  }
}

/*

(deftype drawable-group (drawable)
  ((length  int16       :offset 6)
   (data    drawable 1  :offset-assert 32)
   )
  (:methods
    (new (symbol type int) _type_)
    )
  :flag-assert #x1200000024
  )

 (deftype drawable-tree (drawable-group)
  ()
  :flag-assert #x1200000024
  )

(deftype drawable-inline-array (drawable)
  ((length  int16          :offset 6) ;; this is kinda weird.
   )
  :method-count-assert 18
  :size-assert         #x20
  :flag-assert         #x1200000020
  )

(deftype drawable-inline-array-tfrag (drawable-inline-array)
  ((data tfragment 1 :inline :offset-assert 32)
   (pad uint32))
  :method-count-assert 18
  :size-assert         #x64
  :flag-assert         #x1200000064
  )

(deftype drawable-tree-tfrag (drawable-tree)
  ((time-of-day-pal time-of-day-palette :offset 12)
   (arrays    drawable-inline-array 1  :offset 32 :score 100) ;; either drawable-inline-array-node
or drawable-inline-array-tfrag
   )
  :method-count-assert #x12
  :size-assert #x24
  :flag-assert #x1200000024
  )


 */

size_t add_empty_dia(const std::string& name, DataObjectGenerator& gen, int total_size) {
  gen.align_to_basic();
  gen.add_type_tag(name);
  size_t result = gen.current_offset_bytes();
  total_size -= 4;
  while (total_size > 0) {
    gen.add_word(0);
    total_size -= 4;
  }

  return result;
}

size_t DrawableTreeTfrag::add_to_object_file(DataObjectGenerator& gen) const {
  gen.align_to_basic();
  gen.add_type_tag(m_type);
  size_t result = gen.current_offset_bytes();
  gen.add_word(1 << 16);
  for (int i = 0; i < 6; i++) {
    gen.add_word(0);
  }
  size_t slot = gen.add_word(0);
  ASSERT(slot * 4 - result == 28);
  gen.link_word_to_byte(slot, add_empty_dia(m_array_type, gen, 0x64));

  return result;
}