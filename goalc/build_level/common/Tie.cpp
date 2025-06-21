#include "Tie.h"

void tie_from_gltf(const gltf_mesh_extract::TieOutput& mesh_extract_out,
                   std::vector<tfrag3::TieTree>& out_pc) {
  auto& out = out_pc.emplace_back();
  // bvh: leave default
  // draws
  // categories
  out.category_draw_indices[0] = 0;
  for (int category_idx = 0; category_idx < tfrag3::kNumTieCategories; category_idx++) {
    switch ((tfrag3::TieCategory)category_idx) {
      case tfrag3::TieCategory::NORMAL_ENVMAP:
        out.static_draws.insert(out.static_draws.end(), mesh_extract_out.base_draws.begin(),
                                mesh_extract_out.base_draws.end());
        break;
      case tfrag3::TieCategory::NORMAL_ENVMAP_SECOND_DRAW:
        out.static_draws.insert(out.static_draws.end(), mesh_extract_out.envmap_draws.begin(),
                                mesh_extract_out.envmap_draws.end());
        break;
      default:
        break;
    }
    out.category_draw_indices[category_idx + 1] = out.static_draws.size();
  }
  // packed
  out.packed_vertices.color_indices = mesh_extract_out.color_indices;
  out.packed_vertices.vertices = mesh_extract_out.vertices;
  auto& matrix_group = out.packed_vertices.matrix_groups.emplace_back();
  matrix_group.matrix_idx = -1;
  matrix_group.start_vert = 0;
  matrix_group.end_vert = out.packed_vertices.vertices.size();
  matrix_group.has_normals = true;

  // colors
  out.colors = gltf_util::pack_time_of_day(mesh_extract_out.color_palette);
  // wind (none)
  // proto vis toggle
  out.has_per_proto_visibility_toggle = false;

  out.use_strips = false;
}

/*

(deftype drawable (basic)
  ((id      int16          :offset-assert 4)
   (bsphere vector :inline :offset-assert 16)
   )

(deftype drawable-tree (drawable-group)
  ()
  :flag-assert #x1200000024
  )

(deftype drawable-group (drawable)
  ((length  int16       :offset 6)
   (data    drawable 1  :offset-assert 32)
   )
  (:methods
    (new (symbol type int) _type_)
    )
  :flag-assert #x1200000024
  )

(deftype drawable-tree-instance-tie (drawable-tree)
  ((prototypes proxy-prototype-array-tie           :offset 8)
   )
  :method-count-assert 18
  :size-assert         #x24
  :flag-assert         #x1200000024
  )

(deftype proxy-prototype-array-tie (basic)
  ((prototype-array-tie prototype-array-tie   :offset-assert 4)
   (wind-vectors        uint32  :offset-assert 8) ; likely a pointer
   )
  :method-count-assert 9
  :size-assert         #xc
  :flag-assert         #x90000000c
  )

(deftype prototype-array-tie (array)
  ((array-data prototype-bucket-tie :dynamic :offset 16)
   )
  :method-count-assert 10
  :size-assert         #x10
  :flag-assert         #xa00000010
  (:methods
    (login (_type_) none) ;; 9
    )
  )
*/

size_t add_prototype_array_tie(DataObjectGenerator& gen) {
  gen.align_to_basic();
  gen.add_type_tag("prototype-array-tie");  // 0
  size_t ret = gen.current_offset_bytes();
  gen.add_word(0);                           // 4 length
  gen.add_word(0);                           // 8 allocated-length
  gen.add_type_tag("prototype-bucket-tie");  // 12 content type (might be wrong?)
  return ret;
}

size_t add_proxy_prototype_array_tie(DataObjectGenerator& gen) {
  const size_t array_offset = add_prototype_array_tie(gen);

  gen.align_to_basic();
  gen.add_type_tag("proxy-prototype-array-tie");  // 0
  const size_t result = gen.current_offset_bytes();
  const size_t array_slot = gen.add_word(0);  // 4 prototype-array-tie
  gen.link_word_to_byte(array_slot, array_offset);
  gen.add_word(0);  // 8 wind-vectors
  return result;
}

size_t add_tie_tree_to_object_file(DataObjectGenerator& gen) {
  const size_t proxy = add_proxy_prototype_array_tie(gen);
  gen.align_to_basic();
  gen.add_type_tag("drawable-tree-instance-tie");  // 0
  size_t result = gen.current_offset_bytes();
  gen.add_word(0);                      // 4 (id = 0, length = 0)
  const size_t slot = gen.add_word(0);  // 8
  gen.link_word_to_byte(slot, proxy);
  return result;
}

size_t DrawableTreeInstanceTie::add_to_object_file(DataObjectGenerator& gen) const {
  return add_tie_tree_to_object_file(gen);
}
