#include "extract_hfrag.h"

#include "decompiler/level_extractor/extract_common.h"

namespace decompiler {

constexpr int kCornersPerEdge = level_tools::HFragment::kCornersPerEdge;
constexpr int kVertsPerEdge = level_tools::HFragment::kVertsPerEdge;
constexpr int kVertsPerCorner = kVertsPerEdge / kCornersPerEdge;

int vertex_xz_to_index(int vx, int vz) {
  return vz * kVertsPerEdge + vx;
}

int corner_xz_to_index(int x, int z) {
  return z * kCornersPerEdge + x;
}

void extract_hfrag(const level_tools::BspHeader& bsp, const TextureDB& tex_db, tfrag3::Level* out) {
  ASSERT(bsp.hfrag.has_value());
  const auto& hfrag = bsp.hfrag.value();
  auto& hfrag_out = out->hfrag;

  hfrag_out.occlusion_offset = bsp.visible_list_length - bsp.extra_vis_list_length;

  // create corners
  hfrag_out.buckets.resize(hfrag.num_buckets_near);
  for (int cz = 0; cz < kCornersPerEdge; cz++) {
    for (int cx = 0; cx < kCornersPerEdge; cx++) {
      const int ci = corner_xz_to_index(cx, cz);
      const int vi = vertex_xz_to_index(cx * kVertsPerCorner, cz * kVertsPerCorner);

      auto& corner_out = hfrag_out.corners.emplace_back();
      const auto& corner = hfrag.spheres.at(ci);

      for (int i = 0; i < 4; i++) {
        corner_out.bsphere[i] = corner.data[i];
      }
      corner_out.vis_id = hfrag.vis_ids.at(ci);
      const u32 v_data = hfrag.verts.at(vi);
      const u16 v_packed = v_data >> 16;
      const u16 bucket = v_packed >> 11;
      hfrag_out.buckets.at(bucket).corners.push_back(ci);
    }
  }

  // create vertices and indices
  // loop over each corner
  for (int cz = 0; cz < kCornersPerEdge; cz++) {
    const int vz_corner_base = cz * kVertsPerCorner;
    for (int cx = 0; cx < kCornersPerEdge; cx++) {
      const int vx_corner_base = cx * kVertsPerCorner;
      const int ci = corner_xz_to_index(cx, cz);
      auto& corner = hfrag_out.corners.at(ci);
      corner.index_start = hfrag_out.indices.size();

      // loop over quad rows which have lower vertex in vz
      for (int vz_offset = 0; vz_offset < kVertsPerCorner; vz_offset++) {
        const int vz = vz_corner_base + vz_offset;
        // loop over quads which have lower vertex in vx, vz
        for (int vx_offset = 0; vx_offset < kVertsPerCorner; vx_offset++) {
          const int vx = vx_corner_base + vx_offset;

          // skip out of bound quads
          if (vx + 1 < kVertsPerEdge && vz + 1 < kVertsPerEdge) {
            corner.num_tris += 2;
            for (int qx = 0; qx < 2; qx++) {
              for (int qz = 0; qz < 2; qz++) {
                hfrag_out.indices.push_back(hfrag_out.vertices.size());
                int vi = vertex_xz_to_index(vx + qx, vz + qz);
                const u32 data = hfrag.verts.at(vi);
                auto& vert = hfrag_out.vertices.emplace_back();
                vert.height = 8.f * (data & 0xffff);
                vert.color_index = (data >> 16) & 0b111'1111'1111;
                vert.u = qx;
                vert.v = qz;
                vert.vi = vi;
              }
            }
            hfrag_out.indices.push_back(UINT32_MAX);
          }
        }
      }
      corner.index_length = hfrag_out.indices.size() - corner.index_start;
    }
  }

  for (int i = 0; i < 3; i++) {
    ASSERT(hfrag.start_corner.data[i] == 0);
  }

  // colors
  hfrag_out.time_of_day_colors = pack_colors(hfrag.colors);

  // shaders
  DrawMode mode;
  mode.set_at(false);  // I think this is just the default and hfrag doesn't set it
  mode.set_ab(false);  // see prim regs set up in hfrag-vu1
  mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_SRC_SRC_SRC);  // unused
  mode.set_zt(true);                                            // default ztest
  mode.set_depth_test(GsTest::ZTest::GEQUAL);
  mode.set_depth_write_enable(true);
  mode.enable_fog();
  mode.set_decal(false);
  mode.set_clamp_s_enable(true);
  mode.set_clamp_t_enable(true);

  // adgif0 should be tex0
  const auto& shader = hfrag.shaders[2];
  ASSERT((u8)shader.tex0_addr == (u32)GsRegisterAddress::TEX0_1);
  ASSERT(shader.tex0_data == 0);  // no decal
  // adgif1 should be tex1
  ASSERT((u8)shader.tex1_addr == (u32)GsRegisterAddress::TEX1_1);
  u32 tpage = shader.tex1_addr >> 20;
  u32 tidx = (shader.tex1_addr >> 8) & 0b1111'1111'1111;
  u32 tex_combo = (((u32)tpage) << 16) | tidx;
  auto tex = tex_db.textures.find(tex_combo);
  ASSERT(tex != tex_db.textures.end());
  ASSERT(tex->second.name == "wang_0");
  ASSERT((u8)shader.mip_addr == (u32)GsRegisterAddress::MIPTBP1_1);
  ASSERT((u8)shader.clamp_addr == (u32)GsRegisterAddress::CLAMP_1);
  bool clamp_s = shader.clamp_data & 0b001;
  bool clamp_t = shader.clamp_data & 0b100;
  ASSERT(clamp_t && clamp_s);
  ASSERT((u8)shader.alpha_addr == (u32)GsRegisterAddress::ALPHA_1);
  GsAlpha reg(shader.alpha_data);
  ASSERT(reg.a_mode() == GsAlpha::BlendMode::SOURCE && reg.b_mode() == GsAlpha::BlendMode::DEST &&
         reg.c_mode() == GsAlpha::BlendMode::SOURCE && reg.d_mode() == GsAlpha::BlendMode::DEST);
  hfrag_out.draw_mode = mode;

  // find texture (hack, until we have texture animations)
  u32 idx_in_lev_data = UINT32_MAX;
  for (u32 i = 0; i < out->textures.size(); i++) {
    if (out->textures[i].combo_id == tex_combo) {
      idx_in_lev_data = i;
      break;
    }
  }
  ASSERT(idx_in_lev_data != UINT32_MAX);
  hfrag_out.wang_tree_tex_id[0] = idx_in_lev_data;
  hfrag_out.wang_tree_tex_id[1] = -1;
  hfrag_out.wang_tree_tex_id[2] = -1;
  hfrag_out.wang_tree_tex_id[3] = -1;

  // montage table
  for (int bi = 0; bi < 17; bi++) {
    for (int mi = 0; mi < 16; mi++) {
      // the game stores this as a memory offset, but we convert to an index for convenience.
      u32 montage_mem_offset = hfrag.montage[bi].table[mi];
      ASSERT((montage_mem_offset & 31) == 0);
      hfrag_out.buckets.at(bi).montage_table.at(mi) = montage_mem_offset / 32;
    }
  }

  //  std::string result = fmt::format(
  //      "ply\nformat ascii 1.0\nelement vertex {}\nproperty float x\nproperty float y\nproperty
  //      " "float z\nproperty uchar red\nproperty uchar green\nproperty uchar blue\nelement face
  //      "
  //      "{}\nproperty list uchar int vertex_index\nend_header\n",
  //      kVertsPerEdge * kVertsPerEdge, 2 * (kVertsPerEdge - 1) * (kVertsPerEdge - 1));
  //
  //  // build vertices
  //  for (int vz = 0; vz < kVertsPerEdge; vz++) {
  //    for (int vx = 0; vx < kVertsPerEdge; vx++) {
  //      // total size is 524288 * 32
  //
  //      const int v_idx = vertex_xz_to_index(vx, vz);
  //      const u32 v_data = hfrag.verts.at(v_idx);
  //      const u16 v_height_u16 = v_data & 0xffff;
  //      const int cx = vx / kVertsPerCorner;
  //      const int cz = vz / kVertsPerCorner;
  //      const int c_idx = corner_xz_to_index(cx, cz);
  //      const int bucket_v_idx = vertex_xz_to_index(cx * kVertsPerCorner, cz * kVertsPerCorner);
  //      const u32 cv_data = hfrag.verts.at(bucket_v_idx);
  //
  //      // const float height_offset = hfrag.
  //      const float v_height = ((float)v_height_u16) * 8;
  //      const u16 cv_packed = cv_data >> 16;
  //      const u16 bucket = cv_packed >> 11;
  //      const u16 bucket_color = bucket * 10;
  //      if (cx * kVertsPerCorner == vx && cz * kVertsPerCorner == vz) {
  //        printf("bucket %d\n", bucket);
  //      }
  //
  //      math::Vector3f v(vx * kVertSpacing, v_height, vz * kVertSpacing);
  //      result += fmt::format("{} {} {} {} {} {}\n", v.x() / 1024.f, v.y() / 1024.f, v.z() /
  //      1024.f,
  //                            bucket_color, 128, 128);
  //    }
  //  }
  //
  //  for (int vz = 0; vz < kVertsPerEdge - 1; vz++) {
  //    for (int vx = 0; vx < kVertsPerEdge - 1; vx++) {
  //      result += fmt::format("3 {} {} {}\n", vertex_xz_to_index(vx, vz),
  //                            vertex_xz_to_index(vx + 1, vz), vertex_xz_to_index(vx, vz + 1));
  //      result += fmt::format("3 {} {} {}\n", vertex_xz_to_index(vx + 1, vz + 1),
  //                            vertex_xz_to_index(vx, vz + 1), vertex_xz_to_index(vx + 1, vz));
  //    }
  //  }
  //
  //  auto file_path = file_util::get_file_path({"debug_out/hfrag", fmt::format("{}.ply",
  //  debug_name)}); file_util::create_dir_if_needed_for_file(file_path);
  //  file_util::write_text_file(file_path, result);
}
}  // namespace decompiler