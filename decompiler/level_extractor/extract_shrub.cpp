#include <array>

#include "extract_shrub.h"

#include "decompiler/ObjectFile/LinkedObjectFile.h"

#include "common/util/FileUtil.h"

namespace decompiler {
using namespace level_tools;

std::array<math::Vector4f, 4> extract_shrub_matrix(const u16* data) {
  std::array<math::Vector4f, 4> result;
  for (int i = 0; i < 4; i++) {
    s32 x = data[12 + i];
    x <<= 16;
    x >>= 10;
    result[3][i] = x;
  }

  for (int vec = 0; vec < 3; vec++) {
    for (int i = 0; i < 4; i++) {
      s32 x = data[vec * 4 + i];
      x <<= 16;
      x >>= 16;
      result[vec][i] = (float)x / 4096.f;
    }
  }

  return result;
}

struct ShrubVertex {
  math::Vector<float, 3> xyz;
  math::Vector<float, 2> st;
  math::Vector<u8, 4> rgba_generic;
  bool adc = false;
};

struct ShrubDraw {
  u32 start_vtx_idx = -1;
  AdGifData adgif;
  std::vector<ShrubVertex> vertices;
};

struct ShrubFrag {
  std::vector<ShrubDraw> draws;
};

struct ShrubInstanceInfo {
  u32 proto_idx;
  u32 color_idx;
  std::array<math::Vector4f, 4> mat;
  math::Vector4f bsphere;
};

struct ShrubProtoInfo {
  std::vector<ShrubFrag> frags;
  std::vector<ShrubInstanceInfo> instances;
};

std::string debug_dump_proto_to_obj(const ShrubProtoInfo& proto) {
  std::vector<math::Vector<float, 3>> verts;
  std::vector<math::Vector<float, 2>> tcs;
  std::vector<math::Vector<int, 3>> faces;

  for (auto& frag : proto.frags) {
    for (auto& strip : frag.draws) {
      // add verts...
      ASSERT(strip.vertices.size() >= 3);

      int vert_idx = 0;

      int vtx_idx_queue[3];

      int q_idx = 0;
      int startup = 0;
      while (vert_idx < (int)strip.vertices.size()) {
        verts.push_back(strip.vertices.at(vert_idx).xyz / 65536);  // no idea
        tcs.push_back(math::Vector<float, 2>{strip.vertices.at(vert_idx).st.x(),
                                             strip.vertices.at(vert_idx).st.y()});

        vtx_idx_queue[q_idx++] = verts.size();

        // wrap the index
        if (q_idx == 3) {
          q_idx = 0;
        }

        // bump the startup
        if (startup < 3) {
          startup++;
        }

        if (startup >= 3 && strip.vertices.at(vert_idx).adc) {
          faces.push_back(
              math::Vector<int, 3>{vtx_idx_queue[0], vtx_idx_queue[1], vtx_idx_queue[2]});
        }
        vert_idx++;
      }
    }
  }

  std::string result;
  for (auto& vert : verts) {
    result += fmt::format("v {} {} {}\n", vert.x(), vert.y(), vert.z());
  }
  for (auto& tc : tcs) {
    result += fmt::format("vt {} {}\n", tc.x(), tc.y());
  }
  for (auto& face : faces) {
    result += fmt::format("f {}/{} {}/{} {}/{}\n", face.x(), face.x(), face.y(), face.y(), face.z(),
                          face.z());
  }

  return result;
}

ShrubProtoInfo extract_proto(const shrub_types::PrototypeBucketShrub& proto) {
  ShrubProtoInfo result;
  for (int frag_idx = 0; frag_idx < proto.generic_geom.length; frag_idx++) {
    auto& frag_out = result.frags.emplace_back();
    auto& frag = proto.generic_geom.shrubs.at(frag_idx);

    std::vector<AdGifData> adgif_data;
    adgif_data.resize(frag.textures.size() / sizeof(AdGifData));
    memcpy(adgif_data.data(), frag.textures.data(), frag.textures.size());
    for (size_t i = 0; i < adgif_data.size(); i++) {
      auto& draw = frag_out.draws.emplace_back();

      const auto& ag = adgif_data[i];
      int count = (ag.tex1_addr >> 32) & 0xfff;  // drop the eop flag
      draw.start_vtx_idx = ((ag.tex0_addr >> 32) & 0xffff) / 3;

      if (i > 0) {
        auto& prev_draw = frag_out.draws[frag_out.draws.size() - 2];
        ASSERT(prev_draw.start_vtx_idx + prev_draw.vertices.size() + 3 == draw.start_vtx_idx);
      }

      for (int vert_idx = 0; vert_idx < count; vert_idx++) {
        auto& vert_out = draw.vertices.emplace_back();
        s16 vert_data[3];
        memcpy(vert_data, frag.vtx.data() + sizeof(u16) * 3 * (vert_idx + draw.start_vtx_idx),
               3 * sizeof(u16));
        vert_out.xyz = math::Vector3f(vert_data[0], vert_data[1], vert_data[2]);

        u16 st_data[2];
        memcpy(st_data, frag.stq.data() + sizeof(u16) * 2 * (vert_idx + draw.start_vtx_idx),
               2 * sizeof(u16));
        vert_out.st = math::Vector2f(st_data[0], st_data[1]);
        vert_out.adc = (st_data[0] & 1) == 0;  // adc in the low bit of texture coordinate

        memcpy(vert_out.rgba_generic.data(), frag.col.data() + 4 * (vert_idx + draw.start_vtx_idx),
               4);
      }
    }

    ASSERT(frag.vtx_cnt * 3 * sizeof(u16) <= frag.vtx.size());
  }

  /*
  file_util::write_text_file(
      file_util::get_file_path({fmt::format("debug_out/shrub/{}.obj", proto.name)}),
      debug_dump_proto_to_obj(result));
      */
  return result;
}

void extract_instance(const shrub_types::InstanceShrubbery& inst,
                      std::vector<ShrubProtoInfo>& protos) {
  ShrubInstanceInfo result;
  result.proto_idx = inst.bucket_index;
  for (int i = 0; i < 4; i++) {
    result.bsphere[i] = inst.bsphere.data[i];
  }

  // from ee asm
  result.mat = extract_shrub_matrix(inst.origin.data);
  result.mat[3][0] += result.bsphere[0];
  result.mat[3][1] += result.bsphere[1];
  result.mat[3][2] += result.bsphere[2];
  // result.wind_index = instance.wind_index;

  result.mat[0][3] = 0.f;

  protos.at(result.proto_idx).instances.push_back(result);
}

/*!
 * Transform a point in a prototype to the actual point location in the game world.
 */
math::Vector<float, 3> transform_shrub(const std::array<math::Vector4f, 4> mat,
                                       const math::Vector3f& pt) {
  auto temp = mat[0] * pt.x() + mat[1] * pt.y() + mat[2] * pt.z() + mat[3];
  math::Vector3f result;
  result.x() = temp.x();
  result.y() = temp.y();
  result.z() = temp.z();
  return result;
}

/*!
 * Dump the entire tie tree to an obj. Used to debug the transform_tie function. If we get this
 * right, it should fit in with .obj's produced from the tfrag debug.
 */
std::string dump_full_to_obj(const std::vector<ShrubProtoInfo>& protos) {
  std::vector<math::Vector<float, 3>> verts;
  std::vector<math::Vector<int, 3>> faces;

  for (auto& proto : protos) {
    for (auto& inst : proto.instances) {
      auto& mat = inst.mat;
      for (auto& frag : proto.frags) {
        for (auto& strip : frag.draws) {
          // add verts...
          ASSERT(strip.vertices.size() >= 3);

          int vert_idx = 0;

          int vtx_idx_queue[3];

          int q_idx = 0;
          int startup = 0;
          while (vert_idx < (int)strip.vertices.size()) {
            verts.push_back(transform_shrub(mat, strip.vertices.at(vert_idx).xyz) /
                            65536);  // no idea

            vtx_idx_queue[q_idx++] = verts.size();

            // wrap the index
            if (q_idx == 3) {
              q_idx = 0;
            }

            // bump the startup
            if (startup < 3) {
              startup++;
            }

            if (startup >= 3 && strip.vertices.at(vert_idx).adc) {
              faces.push_back(
                  math::Vector<int, 3>{vtx_idx_queue[0], vtx_idx_queue[1], vtx_idx_queue[2]});
            }
            vert_idx++;
          }
        }
      }
    }
  }

  std::string result;
  for (auto& vert : verts) {
    result += fmt::format("v {} {} {}\n", vert.x(), vert.y(), vert.z());
  }

  for (auto& face : faces) {
    result += fmt::format("f {}/{} {}/{} {}/{}\n", face.x(), face.x(), face.y(), face.y(), face.z(),
                          face.z());
  }

  return result;
}

void extract_shrub(const shrub_types::DrawableTreeInstanceShrub* tree,
                   const std::string& debug_name,
                   const std::vector<level_tools::TextureRemap>& /*map*/,
                   const TextureDB& /*tex_db*/,
                   const std::vector<std::pair<int, int>>& /*expected_missing_textures*/,
                   tfrag3::Level& /*out*/,
                   bool dump_level) {
  auto& protos = tree->info.prototype_inline_array_shrub;
  std::vector<ShrubProtoInfo> proto_info;
  for (auto& proto : protos.data) {
    proto_info.push_back(extract_proto(proto));
  }

  for (auto& arr : tree->discovered_arrays) {
    auto as_shrubs = dynamic_cast<shrub_types::DrawableInlineArrayInstanceShrub*>(arr.get());
    if (as_shrubs) {
      for (auto& inst : as_shrubs->instances) {
        extract_instance(inst, proto_info);
      }
    }
  }

  if (dump_level) {
    auto path = file_util::get_file_path({fmt::format("debug_out/shrub_all/{}.obj", debug_name)});
    file_util::create_dir_if_needed_for_file(path);
    file_util::write_text_file(path, dump_full_to_obj(proto_info));
  }
}
}  // namespace decompiler
