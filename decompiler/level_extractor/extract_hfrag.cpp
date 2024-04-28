#include "extract_hfrag.h"

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

//  X----X----X---
//  |    |    |
//  X----X----X---
//  |    |    |

void extract_hfrag(const level_tools::HFragment& hfrag,
                   const std::string& debug_name,
                   const std::vector<level_tools::TextureRemap>& map,
                   const TextureDB& tex_db,
                   tfrag3::Level* out) {
  std::string result = fmt::format(
      "ply\nformat ascii 1.0\nelement vertex {}\nproperty float x\nproperty float y\nproperty "
      "float z\nproperty uchar red\nproperty uchar green\nproperty uchar blue\nelement face "
      "{}\nproperty list uchar int vertex_index\nend_header\n",
      kVertsPerEdge * kVertsPerEdge, 2 * (kVertsPerEdge - 1) * (kVertsPerEdge - 1));

  // build vertices
  for (int vz = 0; vz < kVertsPerEdge; vz++) {
    for (int vx = 0; vx < kVertsPerEdge; vx++) {
      // total size is 524288 * 32

      const int v_idx = vertex_xz_to_index(vx, vz);
      const u32 v_data = hfrag.verts.at(v_idx);
      const u16 v_height_u16 = v_data & 0xffff;
      const int cx = vx / kVertsPerCorner;
      const int cz = vz / kVertsPerCorner;
      const int c_idx = corner_xz_to_index(cx, cz);
      const int bucket_v_idx = vertex_xz_to_index(cx * kVertsPerCorner, cz * kVertsPerCorner);
      const u32 cv_data = hfrag.verts.at(bucket_v_idx);

      // const float height_offset = hfrag.
      const float v_height = ((float)v_height_u16) * 8;
      const u16 cv_packed = cv_data >> 16;
      const u16 bucket = cv_packed >> 11;
      const u16 bucket_color = bucket * 10;
      if (cx * kVertsPerCorner == vx && cz * kVertsPerCorner == vz) {
        printf("bucket %d\n", bucket);
      }

      math::Vector3f v(vx * kVertSpacing, v_height, vz * kVertSpacing);
      result += fmt::format("{} {} {} {} {} {}\n", v.x() / 1024.f, v.y() / 1024.f, v.z() / 1024.f,
                            bucket_color, 128, 128);
    }
  }

  for (int vz = 0; vz < kVertsPerEdge - 1; vz++) {
    for (int vx = 0; vx < kVertsPerEdge - 1; vx++) {
      result += fmt::format("3 {} {} {}\n", vertex_xz_to_index(vx, vz),
                            vertex_xz_to_index(vx + 1, vz), vertex_xz_to_index(vx, vz + 1));
      result += fmt::format("3 {} {} {}\n", vertex_xz_to_index(vx + 1, vz + 1),
                            vertex_xz_to_index(vx, vz + 1), vertex_xz_to_index(vx + 1, vz));
    }
  }

  auto file_path = file_util::get_file_path({"debug_out/hfrag", fmt::format("{}.ply", debug_name)});
  file_util::create_dir_if_needed_for_file(file_path);
  file_util::write_text_file(file_path, result);
}
}  // namespace decompiler