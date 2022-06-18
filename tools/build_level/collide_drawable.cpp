#include "collide_drawable.h"
#include "goalc/data_compiler/DataObjectGenerator.h"
#include "common/util/Assert.h"

/*
(deftype drawable (basic)
  ((id      int16          :offset-assert 4)
   (bsphere vector :inline :offset-assert 16)
   )

(deftype drawable-group (drawable)
  ((length  int16       :offset 6)
   (data    drawable 1  :offset-assert 32)
   )

(deftype drawable-tree (drawable-group)
  ()

(deftype drawable-inline-array (drawable)
  ((length  int16          :offset 6) ;; this is kinda weird.
   )

(deftype drawable-tree-collide-fragment (drawable-tree)
  ((data-override drawable-inline-array 1 :offset 32)) ;; should be 1 there

(deftype drawable-inline-array-collide-fragment (drawable-inline-array)
  ((data    collide-fragment 1 :inline      :offset-assert 32)

 (deftype collide-fragment (drawable)
  ((mesh    collide-frag-mesh          :offset 8)
   )
  :method-count-assert 18
  :size-assert         #x20

(deftype collide-frag-mesh (basic)
  ((packed-data     uint32         :offset-assert 4)
   (pat-array       uint32         :offset-assert 8)
   (strip-data-len  uint16         :offset-assert 12)
   (poly-count      uint16         :offset-assert 14)
   (base-trans      vector :inline :offset-assert 16)
   ;; these go in the w of the vector above.
   (vertex-count    uint8          :offset 28)
   (vertex-data-qwc uint8          :offset 29)
   (total-qwc       uint8          :offset 30)
   (unused          uint8          :offset 31)
   )
  :method-count-assert 9
  :size-assert         #x20

(deftype draw-node (drawable)
  ((child-count uint8          :offset 6)
   (flags       uint8          :offset 7)
   (child       drawable        :offset 8)
   (distance    float          :offset 12)
   )
   :size-assert         #x20

 (deftype drawable-inline-array-node (drawable-inline-array)
  ((data draw-node 1 :inline)
   (pad uint32)
   )
  :method-count-assert 18
  :size-assert         #x44
 */

size_t generate_pat_array(DataObjectGenerator& gen, const std::vector<PatSurface>& pats) {
  gen.align_to_basic();
  size_t result = gen.current_offset_bytes();
  for (auto& pat : pats) {
    gen.add_word(pat.val);
  }
  return result;
}

size_t generate_packed_collide_data(DataObjectGenerator& gen, const std::vector<u8>& data) {
  gen.align_to_basic();
  size_t result = gen.current_offset_bytes();
  ASSERT((data.size() % 4) == 0);
  for (size_t i = 0; i < data.size(); i += 4) {
    u32 word;
    memcpy(&word, data.data() + i, 4);
    gen.add_word(word);
  }
  return result;
}

size_t generate_collide_frag_mesh(DataObjectGenerator& gen,
                                  const CollideFragMeshData& mesh,
                                  size_t packed_data_loc,
                                  size_t pat_array_loc) {
  gen.align_to_basic();
  gen.add_type_tag("collide-frag-mesh");  // 0
  size_t result = gen.current_offset_bytes();
  gen.link_word_to_byte(gen.add_word(0), packed_data_loc);      // 4
  gen.link_word_to_byte(gen.add_word(0), pat_array_loc);        // 8
  gen.add_word(mesh.strip_data_len | (mesh.poly_count << 16));  // 12
  gen.add_word(mesh.base_trans_xyz_s32.x());                    // 16
  gen.add_word(mesh.base_trans_xyz_s32.y());                    // 20
  gen.add_word(mesh.base_trans_xyz_s32.z());                    // 24
  u32 packed = 0;
  packed |= mesh.vertex_count;
  packed |= ((u32)mesh.vertex_data_qwc) << 8;
  packed |= ((u32)mesh.total_qwc) << 16;
  gen.add_word(packed);  // 28
  return result;
}

size_t generate_collide_draw_node_array(DataObjectGenerator& gen,
                                        const std::vector<collide::DrawNode>& nodes,
                                        u32 flag,
                                        const std::vector<size_t>& children,
                                        std::vector<size_t>& parent_ref_out) {
  gen.align_to_basic();
  gen.add_type_tag("drawable-inline-array-node");  // 0
  size_t result = gen.current_offset_bytes();
  gen.add_word(nodes.size() << 16);  // 4, 6
  gen.add_word(0);                   // 8
  gen.add_word(0);                   // 12
  gen.add_word(0);                   // 16
  gen.add_word(0);                   // 20
  gen.add_word(0);                   // 24
  gen.add_word(0);                   // 28

  ASSERT(nodes.size() == children.size());
  for (size_t i = 0; i < nodes.size(); i++) {
    auto& node = nodes[i];
    // should be 8 words here:
    gen.add_type_tag("draw-node");  // 1
    size_t me = gen.current_offset_bytes();
    u32 packed_flags = 0;
    packed_flags |= (8 << 16);  // TODO hard-coded size here
    packed_flags |= (flag << 24);
    gen.add_word(packed_flags);                           // 2
    gen.link_word_to_byte(gen.add_word(0), children[i]);  // 3
    gen.add_word(0);                                      // 4
    if ((i % 8) == 0) {
      parent_ref_out.push_back(me);
    }
    gen.add_word_float(node.bsphere.x());  // 5
    gen.add_word_float(node.bsphere.y());  // 6
    gen.add_word_float(node.bsphere.z());  // 7
    gen.add_word_float(node.bsphere.w());  // 8
  }

  return result;
}

size_t DrawableTreeCollideFragment::add_to_object_file(DataObjectGenerator& gen) const {
  // don't forget pats
  return 0;
}
