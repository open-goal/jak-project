#include "build_actor.h"

#include "common/log/log.h"

#include "third-party/tiny_gltf/tiny_gltf.h"

using namespace gltf_util;
namespace jak1 {

void extract(MercExtractData& out,
             const tinygltf::Model& model,
             const std::vector<NodeWithTransform>& all_nodes,
             u32 index_offset,
             u32 vertex_offset,
             u32 tex_offset) {
  ASSERT(out.new_vertices.empty());
  std::map<int, tfrag3::MercDraw> draw_by_material;
  int mesh_count = 0;
  int prim_count = 0;

  for (const auto& n : all_nodes) {
    const auto& node = model.nodes[n.node_idx];
    if (node.mesh >= 0) {
      const auto& mesh = model.meshes[node.mesh];
      mesh_count++;
      for (const auto& prim : mesh.primitives) {
        prim_count++;
        // extract index buffer
        std::vector<u32> prim_indices =
            gltf_index_buffer(model, prim.indices, out.new_vertices.size() + vertex_offset);
        ASSERT_MSG(prim.mode == TINYGLTF_MODE_TRIANGLES, "Unsupported triangle mode");
        // extract vertices
        auto verts = gltf_vertices(model, prim.attributes, n.w_T_node, true, true, mesh.name);
        out.new_vertices.insert(out.new_vertices.end(), verts.vtx.begin(), verts.vtx.end());
        out.new_colors.insert(out.new_colors.end(), verts.vtx_colors.begin(),
                              verts.vtx_colors.end());
        ASSERT(out.new_colors.size() == out.new_vertices.size());

        // TODO: just putting it all in one material
        auto& draw = draw_by_material[prim.material];
        draw.mode = make_default_draw_mode();  // todo rm
        draw.tree_tex_id = 0;                  // todo rm
        draw.num_triangles += prim_indices.size() / 3;
        //        if (draw.vis_groups.empty()) {
        //          auto& grp = draw.vis_groups.emplace_back();
        //          grp.num_inds += prim_indices.size();
        //          grp.num_tris += draw.num_triangles;
        //          grp.vis_idx_in_pc_bvh = UINT32_MAX;
        //        } else {
        //          auto& grp = draw.vis_groups.back();
        //          grp.num_inds += prim_indices.size();
        //          grp.num_tris += draw.num_triangles;
        //          grp.vis_idx_in_pc_bvh = UINT32_MAX;
        //        }
        draw.index_count = prim_indices.size();
        draw.first_index = index_offset + out.new_indices.size();

        out.new_indices.insert(out.new_indices.end(), prim_indices.begin(), prim_indices.end());
      }
    }
  }

  tfrag3::MercEffect e;
  out.new_model.max_bones = 120;  // idk
  out.new_model.max_draws = 200;
  for (const auto& [mat_idx, d_] : draw_by_material) {
    e.all_draws.push_back(d_);
    auto& draw = e.all_draws.back();
    draw.mode = make_default_draw_mode();

    if (mat_idx == -1) {
      lg::warn("Draw had a material index of -1, using default texture.");
      draw.tree_tex_id = 0;
      continue;
    }
    const auto& mat = model.materials[mat_idx];
    int tex_idx = mat.pbrMetallicRoughness.baseColorTexture.index;
    if (tex_idx == -1) {
      lg::warn("Material {} has no texture, using default texture.", mat.name);
      draw.tree_tex_id = 0;
      continue;
    }

    const auto& tex = model.textures[tex_idx];
    ASSERT(tex.sampler >= 0);
    ASSERT(tex.source >= 0);
    draw.mode = draw_mode_from_sampler(model.samplers.at(tex.sampler));

    const auto& img = model.images[tex.source];
    draw.tree_tex_id = tex_offset + texture_pool_add_texture(&out.tex_pool, img);
  }
  lg::info("total of {} unique materials", e.all_draws.size());
  out.new_model.effects.push_back(e);
  out.new_model.effects.push_back(e);
  out.new_model.effects.push_back(e);
  out.new_model.effects.push_back(e);

  lg::info("Merged {} meshes and {} prims into {} vertices", mesh_count, prim_count,
           out.new_vertices.size());
}

const tfrag3::MercVertex& find_closest(const std::vector<tfrag3::MercVertex>& old,
                                       float x,
                                       float y,
                                       float z) {
  float best_dist = 1e10;
  int best_idx = 0;

  for (int i = 0; i < old.size(); i++) {
    auto& v = old[i];
    float dx = v.pos[0] - x;
    float dy = v.pos[1] - y;
    float dz = v.pos[2] - z;
    float dist = (dx * dx) + (dy * dy) + (dz * dz);
    if (dist < best_dist) {
      best_dist = dist;
      best_idx = i;
    }
  }

  return old[best_idx];
}

void merc_convert(MercSwapData& out,
                  const MercExtractData& in,
                  const std::vector<tfrag3::MercVertex>& old_verts) {
  /*
   *   std::vector<u32> new_indices;
  std::vector<tfrag3::MercVertex> new_vertices;
  std::vector<decompiler::TextureDB::TextureData> new_textures;
  tfrag3::MercModel new_model;
   */

  // easy
  out.new_model = in.new_model;
  out.new_indices = in.new_indices;
  out.new_textures = in.tex_pool.textures_by_idx;

  // convert vertices
  for (size_t i = 0; i < in.new_vertices.size(); i++) {
    const auto& y = in.new_vertices[i];
    const auto& copy_from = find_closest(old_verts, y.x, y.y, y.z);
    auto& x = out.new_vertices.emplace_back();
    x.pos[0] = y.x;
    x.pos[1] = y.y;
    x.pos[2] = y.z;
    x.normal[0] = copy_from.normal[0];
    x.normal[1] = copy_from.normal[1];
    x.normal[2] = copy_from.normal[2];
    x.weights[0] = copy_from.weights[0];
    x.weights[1] = copy_from.weights[1];
    x.weights[2] = copy_from.weights[2];
    x.st[0] = y.s;
    x.st[1] = y.t;
    x.rgba[0] = in.new_colors[i][0];
    x.rgba[1] = in.new_colors[i][1];
    x.rgba[2] = in.new_colors[i][2];
    x.rgba[3] = in.new_colors[i][3];
    x.mats[0] = copy_from.mats[0];
    x.mats[1] = copy_from.mats[1];
    x.mats[2] = copy_from.mats[2];
  }
}

MercSwapData load_merc_model(u32 current_idx_count,
                             u32 current_vtx_count,
                             u32 current_tex_count,
                             const std::string& path) {
  MercSwapData result;
  lg::info("Reading gltf mesh: {}", path);
  tinygltf::TinyGLTF loader;
  tinygltf::Model model;
  std::string err, warn;
  bool res = loader.LoadBinaryFromFile(&model, &err, &warn, path);
  ASSERT_MSG(warn.empty(), warn.c_str());
  ASSERT_MSG(err.empty(), err.c_str());
  ASSERT_MSG(res, "Failed to load GLTF file!");
  auto all_nodes = flatten_nodes_from_all_scenes(model);

  MercExtractData extract_data;
  extract(extract_data, model, all_nodes, current_idx_count, current_vtx_count, current_tex_count);
  // merc_convert(result, extract_data, old_verts);

  return result;
}

size_t Joint::generate(DataObjectGenerator& gen) const {
  gen.align_to_basic();
  gen.add_type_tag("joint");
  size_t result = gen.current_offset_bytes();
  gen.add_ref_to_string_in_pool(name);
  gen.add_word(number);
  if (parent == -1) {
    gen.add_symbol_link("#f");
  }
  for (int i = 0; i < 4; i++) {
    for (int j = 0; j < 4; j++) {
      gen.add_word_float(bind_pose(i, j));
    }
  }
  return result;
}

size_t JointAnimCompressed::generate(DataObjectGenerator& gen) const {
  gen.align_to_basic();
  gen.add_type_tag("joint-anim-compressed");
  size_t result = gen.current_offset_bytes();
  gen.add_ref_to_string_in_pool(name);
  gen.add_word((length << 16) + number);
  for (auto& word : data) {
    gen.add_word(word);
  }
  return result;
}

size_t JointAnimCompressedFrame::generate(DataObjectGenerator& gen) const {
  size_t result = gen.current_offset_bytes();
  gen.add_word(offset_64);  // 0
  gen.add_word(offset_32);  // 4
  gen.add_word(offset_16);  // 8
  gen.add_word(reserved);   // 12
  return result;
}

size_t JointAnimCompressedHDR::generate(DataObjectGenerator& gen) const {
  size_t result = gen.current_offset_bytes();
  for (auto& bit : control_bits) {
    gen.add_word(bit);
  }
  gen.add_word(num_joints);
  gen.add_word(matrix_bits);
  return result;
}

size_t JointAnimCompressedFixed::generate(DataObjectGenerator& gen) const {
  size_t result = gen.current_offset_bytes();
  hdr.generate(gen);        // 0-64 (inline)
  gen.add_word(offset_64);  // 64
  gen.add_word(offset_32);  // 68
  gen.add_word(offset_16);  // 72
  gen.add_word(reserved);   // 76
  // default joint poses
  for (size_t i = 0; i < 8; i++) {
    gen.add_word_float(data[i].x());
    gen.add_word_float(data[i].y());
    gen.add_word_float(data[i].z());
    gen.add_word_float(data[i].w());
  }
  gen.add_word(0);
  gen.add_word(0x7fff0000);
  gen.add_word(0);
  gen.add_word(0x10001000);
  gen.add_word(0x10000000);
  gen.add_word(0);
  gen.add_word(0);
  gen.add_word(0);
  return result;
}

size_t JointAnimCompressedControl::generate(DataObjectGenerator& gen) const {
  size_t result = gen.current_offset_bytes();
  gen.add_word(num_frames);  // 0
  gen.add_word(fixed_qwc);   // 4
  gen.add_word(frame_qwc);   // 8
  gen.link_word_to_byte(gen.add_word(0), fixed.generate(gen));
  gen.link_word_to_byte(gen.add_word(0), frame[0].generate(gen));
  gen.align(4);
  return result;
}

size_t ArtJointGeo::generate(DataObjectGenerator& gen) const {
  gen.align_to_basic();
  gen.add_type_tag("art-joint-geo");
  size_t result = gen.current_offset_bytes();
  gen.add_word(0);                      // 4
  gen.add_ref_to_string_in_pool(name);  // 8
  gen.add_word(length);                 // 12
  gen.add_symbol_link("#f");            // 16
  gen.add_word(0);                      // 20
  gen.add_word(0);
  gen.add_word(0);
  for (auto& joint : data) {
    gen.link_word_to_byte(gen.add_word(0), joint.generate(gen));
  }
  gen.align(4);
  return result;
}

size_t ArtJointAnim::generate(DataObjectGenerator& gen) const {
  gen.align_to_basic();
  gen.add_type_tag("art-joint-anim");
  size_t result = gen.current_offset_bytes();
  gen.add_symbol_link("#f");                             // 4 (eye block)
  gen.add_ref_to_string_in_pool(name);                   // 8
  gen.add_word(length);                                  // 12
  gen.add_symbol_link("#f");                             // 16 (res-lump)
  gen.add_word_float(speed);                             // 20
  gen.add_word_float(artist_base);                       // 24
  gen.add_word_float(artist_step);                       // 28
  gen.add_ref_to_string_in_pool(master_art_group_name);  // 32
  gen.add_word(master_art_group_index);                  // 36
  gen.add_symbol_link("#f");                             // 40 (blerc)
  gen.link_word_to_byte(gen.add_word(0), frames.generate(gen));
  // for (size_t i = 0; i < length; i++) {
  //   gen.link_word_to_byte(gen.add_word(0), data[i].generate(gen));
  // }
  return result;
}

size_t generate_dummy_merc_ctrl(DataObjectGenerator& gen, const std::string& name) {
  gen.align_to_basic();
  gen.add_type_tag("merc-ctrl");
  size_t result = gen.current_offset_bytes();
  gen.add_word(0);
  gen.add_ref_to_string_in_pool(name + "-lod0");
  gen.add_word(0);
  gen.add_symbol_link("#f");
  for (int i = 0; i < 31; i++) {
    gen.add_word(0);
  }
  return result;
}

std::vector<u8> ArtGroup::save_object_file() const {
  DataObjectGenerator gen;
  gen.add_type_tag("art-group");
  auto ag_words = 8 + length;
  while (gen.words() < ag_words) {
    gen.add_word(0);
  }
  auto file_info_slot = info.add_to_object_file(gen);
  gen.link_word_to_byte(4 / 4, file_info_slot);  // 4 (file-info)
  gen.link_word_to_string_in_pool(name, 8 / 4);  // 8 (name)
  gen.set_word(12 / 4, length);                  // 12 (ag length)
  gen.link_word_to_symbol("#f", 16 / 4);         // 16 (res-lump)
  gen.set_word(20 / 4, 0);                       // 20 (pad)
  gen.set_word(24 / 4, 0);
  gen.set_word(28 / 4, 0);
  if (!elts.empty()) {
    if (elts.at(0)) {
      auto jgeo = (ArtJointGeo*)elts.at(0);
      gen.link_word_to_byte(32 / 4, jgeo->generate(gen));
    }
    if (!elts.at(1)) {
      gen.link_word_to_byte(36 / 4, generate_dummy_merc_ctrl(gen, name));
    }
    if (elts.at(2)) {
      auto ja = (ArtJointAnim*)elts.at(2);
      gen.link_word_to_byte(40 / 4, ja->generate(gen));
    }
  }

  return gen.generate_v4();
}

bool run_build_actor(const std::string& input_model,
                     const std::string& ag_out,
                     const std::string& output_prefix) {
  std::string ag_name;
  // ag_name = fs::path(input_model).stem().string();
  if (fs::exists(file_util::get_jak_project_dir() / input_model)) {
    ag_name = fs::path(input_model).stem().string();
  } else {
    ASSERT_MSG(false, "Model file not found: " + input_model);
  }

  ArtGroup ag(ag_name, GameVersion::Jak1);
  ArtJointGeo jgeo(ag.name);
  ArtJointAnim ja(ag.name);

  ag.elts.emplace_back(&jgeo);
  // dummy merc-ctrl
  ag.elts.emplace_back(nullptr);
  ag.elts.emplace_back(&ja);

  ag.length = ag.elts.size();

  auto ag_file = ag.save_object_file();
  lg::print("ag file size {} bytes\n", ag_file.size());
  auto save_path = fs::path(ag_out);
  file_util::create_dir_if_needed_for_file(ag_out);
  lg::print("Saving to {}\n", save_path.string());
  file_util::write_binary_file(file_util::get_jak_project_dir() / save_path, ag_file.data(),
                               ag_file.size());
  return true;
}
}  // namespace jak1