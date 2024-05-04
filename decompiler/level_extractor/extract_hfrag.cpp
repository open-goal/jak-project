#include "extract_hfrag.h"

#include "decompiler/level_extractor/extract_common.h"

namespace decompiler {

constexpr int kCornersPerEdge = level_tools::HFragment::kCornersPerEdge;
constexpr int kVertsPerEdge = level_tools::HFragment::kVertsPerEdge;
constexpr int kVertsPerCorner = kVertsPerEdge / kCornersPerEdge;

constexpr float kVertSpacing = 32768;

int vertex_xz_to_index(int vx, int vz) {
  return vz * kVertsPerEdge + vx;
}

int corner_xz_to_index(int x, int z) {
  return z * kCornersPerEdge + x;
}

//  X----X----X---X
//  |    |    |   |
//  X----X----X---X
//  |    |    |
//  X----X----X---

void extract_hfrag(const level_tools::HFragment& hfrag,
                   const std::string& debug_name,
                   const std::vector<level_tools::TextureRemap>& map,
                   const TextureDB& tex_db,
                   tfrag3::Level* out) {
  auto& hfrag_out = out->hfrag;

  // create vertices
  for (int vz = 0; vz < kVertsPerEdge; vz++) {
    for (int vx = 0; vx < kVertsPerEdge; vx++) {
      const int v_idx = vertex_xz_to_index(vx, vz);
      const u32 v_data = hfrag.verts.at(v_idx);
      const u16 v_height_u16 = v_data & 0xffff;
      const u16 v_packed_index = v_data >> 16;
      const u16 color_idx = v_packed_index & 0b111'1111'1111;
      auto& vert_out = hfrag_out.vertices.emplace_back();
      vert_out.color_index = color_idx;
      vert_out.height = v_height_u16 * 8.f;
    }
  }

  for (int cz = 0; cz < kCornersPerEdge; cz++) {
    const int vz_corner_base = cz * kVertsPerCorner;
    for (int cx = 0; cx < kCornersPerEdge; cx++) {
      const int vx_corner_base = cx * kVertsPerCorner;
      for (int vz_offset = 0; vz_offset < kVertsPerCorner; vz_offset++) {
        const int vz = vz_corner_base + vz_offset;
        for (int vx_offset = 0; vx_offset < kVertsPerCorner + 1; vx_offset++) {
          const int vx = vx_corner_base + vx_offset;
          const int vi = vertex_xz_to_index(vx, vz);
          const int vi_z = vertex_xz_to_index(vx, vz + 1);
          hfrag_out.indices.push_back(vi);
          hfrag_out.indices.push_back(vi_z);
        }
        hfrag_out.indices.push_back(UINT32_MAX);
      }
    }
  }

  for (int i = 0; i < 3; i++) {
    ASSERT(hfrag.start_corner.data[i] == 0);
  }

  hfrag_out.buckets.resize(hfrag.num_buckets_near);

  // create corners
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
      printf("corner %d in bucket %d\n", ci, bucket);
      hfrag_out.buckets.at(bucket).corners.push_back(ci);
    }
  }

  // colors
  hfrag_out.time_of_day_colors = pack_colors(hfrag.colors);

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