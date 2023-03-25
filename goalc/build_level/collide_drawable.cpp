#include "collide_drawable.h"

#include <unordered_map>
#include <unordered_set>

#include "common/util/Assert.h"

#include "goalc/data_compiler/DataObjectGenerator.h"

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

size_t generate_collide_fragment(DataObjectGenerator& gen,
                                 const CollideFragMeshData& mesh,
                                 size_t frag_mesh_loc) {
  /*
    .type collide-fragment
    .word 0x10000
    .word L705
    .word 0x0
    .word 0x46bf480a
    .word 0x43dc730b
    .word 0xb71ed4fe
    .word 0x46c42e44
   */
  gen.align_to_basic();
  gen.add_type_tag("collide-fragment");
  size_t result = gen.current_offset_bytes();
  gen.add_word(0x10000);  // ???
  gen.link_word_to_byte(gen.add_word(0), frag_mesh_loc);
  gen.add_word(0);
  for (int i = 0; i < 4; i++) {
    gen.add_word_float(mesh.bsphere[i]);
  }

  return result;
}

size_t generate_collide_fragment_array(DataObjectGenerator& gen,
                                       const std::vector<CollideFragMeshData>& meshes,
                                       const std::vector<size_t>& frag_mesh_locs,
                                       std::vector<size_t>& loc_out) {
  gen.align_to_basic();
  gen.add_type_tag("drawable-inline-array-collide-fragment");  // 0
  size_t result = gen.current_offset_bytes();
  ASSERT(meshes.size() < UINT16_MAX);
  gen.add_word(meshes.size() << 16);  // 4, 6
  gen.add_word(0);                    // 8
  gen.add_word(0);                    // 12
  gen.add_word(0);                    // 16
  gen.add_word(0);                    // 20
  gen.add_word(0);                    // 24
  gen.add_word(0);                    // 28

  ASSERT(meshes.size() == frag_mesh_locs.size());
  for (size_t i = 0; i < meshes.size(); i++) {
    auto& mesh = meshes[i];
    // should be 8 words here:
    gen.add_type_tag("collide-fragment");  // 1
    size_t me = gen.current_offset_bytes();
    gen.add_word(0x10000);  // ???
    gen.link_word_to_byte(gen.add_word(0), frag_mesh_locs[i]);
    gen.add_word(0);
    for (int j = 0; j < 4; j++) {
      gen.add_word_float(mesh.bsphere[j]);
    }
    loc_out.push_back(me);
  }

  return result;
}

int child_count(const collide::DrawNode* node) {
  if (node->frag_children.empty()) {
    return node->draw_node_children.size();
  } else {
    return node->frag_children.size();
  }
}

std::unordered_map<const collide::DrawNode*, size_t> add_draw_nodes(
    DataObjectGenerator& gen,
    const std::vector<const collide::DrawNode*>& nodes,
    const std::vector<size_t>& frag_locs,
    size_t& array_out) {
  std::unordered_map<const collide::DrawNode*, size_t> result;
  std::unordered_map<size_t, const collide::DrawNode*> back_map;

  gen.align_to_basic();
  gen.add_type_tag("drawable-inline-array-node");  // 0
  array_out = gen.current_offset_bytes();
  ASSERT(nodes.size() < UINT16_MAX);
  gen.add_word(nodes.size() << 16);  // 4, 6
  gen.add_word(0);                   // 8
  gen.add_word(0);                   // 12
  gen.add_word(0);                   // 16
  gen.add_word(0);                   // 20
  gen.add_word(0);                   // 24
  gen.add_word(0);                   // 28

  for (auto& node : nodes) {
    bool is_draw_node = node->frag_children.empty();

    // should be 8 words here:
    gen.add_type_tag("draw-node");  // 1
    size_t me = gen.current_offset_bytes();
    u32 packed_flags = 0;
    packed_flags |=
        ((is_draw_node ? node->draw_node_children.size() : node->frag_children.size()) << 16);
    packed_flags |= ((is_draw_node ? 1 : 0) << 24);
    gen.add_word(packed_flags);  // 2
    if (is_draw_node) {
      // gen.link_word_to_byte(gen.add_word(0), result.at(&node->draw_node_children.at(0)));  // 3
      back_map[gen.add_word(0)] = &node->draw_node_children.at(0);
    } else {
      gen.link_word_to_byte(gen.add_word(0), frag_locs.at(node->frag_children.at(0)));
    }

    result[node] = me;

    gen.add_word(0);  // 4

    gen.add_word_float(node->bsphere.x());  // 5
    gen.add_word_float(node->bsphere.y());  // 6
    gen.add_word_float(node->bsphere.z());  // 7
    gen.add_word_float(node->bsphere.w());  // 8
  }

  for (const auto& [loc, node] : back_map) {
    gen.link_word_to_byte(loc, result.at(node));
  }

  return result;
}

std::vector<const collide::DrawNode*> bfs_nodes(const collide::DrawNode& fake_root) {
  std::vector<const collide::DrawNode*> out;
  std::unordered_set<const collide::DrawNode*> added;
  std::vector<const collide::DrawNode*> frontier;
  for (auto& dnc : fake_root.draw_node_children) {
    frontier.push_back(&dnc);
  }

  while (!frontier.empty()) {
    std::vector<const collide::DrawNode*> next_frontier;

    for (auto x : frontier) {
      if (added.find(x) != added.end()) {
        continue;
      }
      added.insert(x);
      out.push_back(x);

      for (auto& child : x->draw_node_children) {
        if (added.find(&child) == added.end()) {
          next_frontier.push_back(&child);
        }
      }
    }

    frontier = std::move(next_frontier);
  }

  return out;
}

size_t DrawableTreeCollideFragment::add_to_object_file(DataObjectGenerator& gen) const {
  // generate pat array
  size_t pat_array_loc = generate_pat_array(gen, packed_frags.pats);

  // generated packed data
  std::vector<size_t> packed_data_locs;
  for (auto& mesh : packed_frags.packed_frag_data) {
    packed_data_locs.push_back(generate_packed_collide_data(gen, mesh.packed_data));
  }

  // generate collide frag meshes
  std::vector<size_t> collide_frag_meshes;
  for (size_t i = 0; i < packed_data_locs.size(); i++) {
    collide_frag_meshes.push_back(generate_collide_frag_mesh(gen, packed_frags.packed_frag_data[i],
                                                             packed_data_locs[i], pat_array_loc));
  }

  std::vector<size_t> children_refs;
  generate_collide_fragment_array(gen, packed_frags.packed_frag_data, collide_frag_meshes,
                                  children_refs);

  auto order = bfs_nodes(bvh.fake_root_node);
  size_t others_array;
  auto located_nodes = add_draw_nodes(gen, order, children_refs, others_array);

  size_t root_dian = -1;
  {
    gen.align_to_basic();
    gen.add_type_tag("drawable-inline-array-node");  // 0
    root_dian = gen.current_offset_bytes();
    gen.add_word(bvh.fake_root_node.draw_node_children.size() << 16);  // 4, 6
    gen.add_word(0);                                                   // 8
    gen.add_word(0);                                                   // 12
    gen.add_word(0);                                                   // 16
    gen.add_word(0);                                                   // 20
    gen.add_word(0);                                                   // 24
    gen.add_word(0);                                                   // 28

    for (auto& node : bvh.fake_root_node.draw_node_children) {
      bool is_draw_node = node.frag_children.empty();

      // should be 8 words here:
      gen.add_type_tag("draw-node");  // 1

      u32 packed_flags = 0;
      packed_flags |=
          ((is_draw_node ? node.draw_node_children.size() : node.frag_children.size()) << 16);
      packed_flags |= ((is_draw_node ? 1 : 0) << 24);
      gen.add_word(packed_flags);  // 2
      if (is_draw_node) {
        gen.link_word_to_byte(gen.add_word(0),
                              located_nodes.at(&node.draw_node_children.at(0)));  // 3
      } else {
        gen.link_word_to_byte(gen.add_word(0), children_refs.at(node.frag_children.at(0)));
      }

      gen.add_word(0);                       // 4
      gen.add_word_float(node.bsphere.x());  // 5
      gen.add_word_float(node.bsphere.y());  // 6
      gen.add_word_float(node.bsphere.z());  // 7
      gen.add_word_float(node.bsphere.w());  // 8
    }
  }

  {
    gen.align_to_basic();
    gen.add_type_tag("drawable-tree-collide-fragment");
    size_t result = gen.current_offset_bytes();
    gen.add_word(2 << 16);  // todo the minus one here??
    for (int i = 0; i < 6; i++) {
      gen.add_word(0);
    }

    gen.link_word_to_byte(gen.add_word(0), root_dian);
    gen.link_word_to_byte(gen.add_word(0), others_array);

    return result;
  }
}
