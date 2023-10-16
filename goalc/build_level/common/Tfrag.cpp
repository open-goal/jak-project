#include "Tfrag.h"

#include <iostream>

#include "gltf_mesh_extract.h"

#include "common/custom_data/pack_helpers.h"

#include "goalc/data_compiler/DataObjectGenerator.h"

void tfrag_from_gltf(const gltf_mesh_extract::TfragOutput& mesh_extract_out,
                     DrawableTreeTfrag& /*out*/,
                     tfrag3::TfragTree& out_pc) {
  out_pc.kind = tfrag3::TFragmentTreeKind::NORMAL;  // todo more types?
  out_pc.draws = std::move(mesh_extract_out.strip_draws);
  pack_tfrag_vertices(&out_pc.packed_vertices, mesh_extract_out.vertices);

  for (auto& col : mesh_extract_out.color_palette) {
    tfrag3::TimeOfDayColor todc;
    for (auto& rgba : todc.rgba) {
      rgba = col;
    }
    out_pc.colors.push_back(todc);
  }
  out_pc.use_strips = false;
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
  gen.add_type_tag("drawable-tree-tfrag");
  size_t result = gen.current_offset_bytes();
  gen.add_word(1 << 16);
  for (int i = 0; i < 6; i++) {
    gen.add_word(0);
  }
  size_t slot = gen.add_word(0);
  ASSERT(slot * 4 - result == 28);
  gen.link_word_to_byte(slot, add_empty_dia("drawable-inline-array-tfrag", gen, 0x64));

  return result;
}