

#include "tfrag_common.h"
#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/pipelines/opengl.h"

#include <immintrin.h>

DoubleDraw setup_tfrag_shader(const TfragRenderSettings& /*settings*/,
                              SharedRenderState* render_state,
                              DrawMode mode) {
  glActiveTexture(GL_TEXTURE0);

  if (mode.get_zt_enable()) {
    glEnable(GL_DEPTH_TEST);
    switch (mode.get_depth_test()) {
      case GsTest::ZTest::NEVER:
        glDepthFunc(GL_NEVER);
        break;
      case GsTest::ZTest::ALWAYS:
        glDepthFunc(GL_ALWAYS);
        break;
      case GsTest::ZTest::GEQUAL:
        glDepthFunc(GL_GEQUAL);
        break;
      case GsTest::ZTest::GREATER:
        glDepthFunc(GL_GREATER);
        break;
      default:
        assert(false);
    }
  } else {
    glDisable(GL_DEPTH_TEST);
  }

  if (mode.get_ab_enable() && mode.get_alpha_blend() != DrawMode::AlphaBlend::DISABLED) {
    glEnable(GL_BLEND);
    switch (mode.get_alpha_blend()) {
      case DrawMode::AlphaBlend::SRC_DST_SRC_DST:
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        break;
      case DrawMode::AlphaBlend::SRC_0_SRC_DST:
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
        break;
      case DrawMode::AlphaBlend::SRC_0_FIX_DST:
        glBlendFunc(GL_ONE, GL_ONE);
        break;
      case DrawMode::AlphaBlend::SRC_DST_FIX_DST:
        // Cv = (Cs - Cd) * FIX + Cd
        // Cs * FIX * 0.5
        // Cd * FIX * 0.5
        glBlendFunc(GL_CONSTANT_COLOR, GL_CONSTANT_COLOR);
        glBlendColor(0.5, 0.5, 0.5, 0.5);
        break;
      default:
        assert(false);
    }
  } else {
    glDisable(GL_BLEND);
  }

  if (mode.get_clamp_s_enable()) {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  } else {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  }

  if (mode.get_clamp_t_enable()) {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  } else {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  }

  if (mode.get_filt_enable()) {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  } else {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  }

  // for some reason, they set atest NEVER + FB_ONLY to disable depth writes
  bool alpha_hack_to_disable_z_write = false;
  DoubleDraw double_draw;

  float alpha_min = 0.;
  if (mode.get_at_enable()) {
    switch (mode.get_alpha_test()) {
      case DrawMode::AlphaTest::ALWAYS:
        break;
      case DrawMode::AlphaTest::GEQUAL:
        alpha_min = mode.get_aref() / 127.f;
        switch (mode.get_alpha_fail()) {
          case GsTest::AlphaFail::KEEP:
            // ok, no need for double draw
            break;
          case GsTest::AlphaFail::FB_ONLY:
            // darn, we need to draw twice
            double_draw.kind = DoubleDrawKind::AFAIL_NO_DEPTH_WRITE;
            double_draw.aref = alpha_min;
            break;
          default:
            assert(false);
        }
        break;
      case DrawMode::AlphaTest::NEVER:
        if (mode.get_alpha_fail() == GsTest::AlphaFail::FB_ONLY) {
          alpha_hack_to_disable_z_write = true;
        } else {
          assert(false);
        }
        break;
      default:
        assert(false);
    }
  }

  if (mode.get_depth_write_enable()) {
    glDepthMask(GL_TRUE);
  } else {
    glDepthMask(GL_FALSE);
  }

  glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "alpha_min"),
              alpha_min);
  glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "alpha_max"),
              10.f);

  return double_draw;
}

void first_tfrag_draw_setup(const TfragRenderSettings& settings, SharedRenderState* render_state) {
  render_state->shaders[ShaderId::TFRAG3].activate();
  glUniform1i(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "tex_T0"), 0);
  glUniform1i(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "tex_T1"), 1);
  glUniformMatrix4fv(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "camera"),
                     1, GL_FALSE, settings.math_camera.data());
  glUniform4f(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "hvdf_offset"),
              settings.hvdf_offset[0], settings.hvdf_offset[1], settings.hvdf_offset[2],
              settings.hvdf_offset[3]);
  glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "fog_constant"),
              settings.fog_x);
}

void interp_time_of_day_slow(const float weights[8],
                             const std::vector<tfrag3::TimeOfDayColor>& in,
                             math::Vector<u8, 4>* out) {
  // Timer interp_timer;
  for (size_t color = 0; color < in.size(); color++) {
    math::Vector4f result = math::Vector4f::zero();
    for (int component = 0; component < 8; component++) {
      result += in[color].rgba[component].cast<float>() * weights[component];
    }
    result[0] = std::min(result[0], 255.f);
    result[1] = std::min(result[1], 255.f);
    result[2] = std::min(result[2], 255.f);
    result[3] = std::min(result[3], 128.f);  // note: different for alpha!
    out[color] = result.cast<u8>();
  }
}

// we want to absolutely minimize the number of time we have to "cross lanes" in AVX (meaning X
// component of one vector interacts with Y component of another).  We can make this a lot better by
// taking groups of 4 time of day colors (each containing 8x RGBAs) and rearranging them with this
// pattern.  We want to compute:
// [rgba][0][0] * weights[0] + [rgba][0][1] * weights[1] + [rgba][0][2]... + rgba[0][7] * weights[7]
// RGBA is already a vector of 4 components, but with AVX we have vectors with 32 bytes which fit
// 16 colors in them.

// This makes each vector have:
// colors0 = [rgba][0][0], [rgba][1][0], [rgba][2][0], [rgba][3][0]
// colors1 = [rgba][0][1], [rgba][1][1], [rgba][2][1], [rgba][3][1]
// ...
// so we can basically add up the columns (multiplying by weights in between)
// and we'll end up with [final0, final1, final2, final3, final4]

// the swizzle function below rearranges to get this pattern.
// it's not the most efficient way to do it, but it just runs during loading and not on every frame.

SwizzledTimeOfDay swizzle_time_of_day(const std::vector<tfrag3::TimeOfDayColor>& in) {
  SwizzledTimeOfDay out;
  out.data.resize(in.size() * 8 * 4);

  // we're rearranging per 4 colors (groups of 32 * 4 = 128)
  // color (lots of these)
  // component (8 of these)
  // channel (4 of these, rgba)

  for (u32 color_quad = 0; color_quad < in.size() / 4; color_quad++) {
    u8* quad_out = out.data.data() + color_quad * 128;
    for (u32 component = 0; component < 8; component++) {
      for (u32 color = 0; color < 4; color++) {
        for (u32 channel = 0; channel < 4; channel++) {
          *quad_out = in.at(color_quad * 4 + color).rgba[component][channel];
          quad_out++;
        }
      }
    }
  }
  out.color_count = in.size();
  return out;
}

// This does the same thing as interp_time_of_day_slow, but is faster.
// Due to using integers instead of floats, it may be a tiny bit different.
// TODO: it might be possible to reorder the loop into two blocks of loads and avoid spilling xmms.
// It's ~8x faster than the slow version.
void interp_time_of_day_fast(const float weights[8],
                             const SwizzledTimeOfDay& in,
                             math::Vector<u8, 4>* out) {
  // even though the colors are 8 bits, we'll use 16 bits so we can saturate correctly

  // weight multipliers
  __m256i weights0 = _mm256_set1_epi16(weights[0] * 128.f);
  __m256i weights1 = _mm256_set1_epi16(weights[1] * 128.f);
  __m256i weights2 = _mm256_set1_epi16(weights[2] * 128.f);
  __m256i weights3 = _mm256_set1_epi16(weights[3] * 128.f);
  __m256i weights4 = _mm256_set1_epi16(weights[4] * 128.f);
  __m256i weights5 = _mm256_set1_epi16(weights[5] * 128.f);
  __m256i weights6 = _mm256_set1_epi16(weights[6] * 128.f);
  __m256i weights7 = _mm256_set1_epi16(weights[7] * 128.f);

  // saturation: note that alpha is saturated to 128 but the rest are 255.
  // TODO: maybe we should saturate to 255 for everybody (can do this using a single packus) and
  // change the shader to deal with this.
  __m256i sat = _mm256_set_epi16(128, 255, 255, 255, 128, 255, 255, 255, 128, 255, 255, 255, 128,
                                 255, 255, 255);

  for (u32 color_quad = 0; color_quad < in.color_count / 4; color_quad++) {
    // first, load colors. We put 16 bytes / register and don't touch the upper half because we will
    // convert u8s to u16s.
    const u8* base = in.data.data() + color_quad * 128;
    __m128i color0_p = _mm_loadu_si128((const __m128i*)(base + 0));
    __m128i color1_p = _mm_loadu_si128((const __m128i*)(base + 16));
    __m128i color2_p = _mm_loadu_si128((const __m128i*)(base + 32));
    __m128i color3_p = _mm_loadu_si128((const __m128i*)(base + 48));
    __m128i color4_p = _mm_loadu_si128((const __m128i*)(base + 64));
    __m128i color5_p = _mm_loadu_si128((const __m128i*)(base + 80));
    __m128i color6_p = _mm_loadu_si128((const __m128i*)(base + 96));
    __m128i color7_p = _mm_loadu_si128((const __m128i*)(base + 112));

    // unpack to 16-bits. each has 16x 16 bit colors.
    __m256i color0 = _mm256_cvtepu8_epi16(color0_p);
    __m256i color1 = _mm256_cvtepu8_epi16(color1_p);
    __m256i color2 = _mm256_cvtepu8_epi16(color2_p);
    __m256i color3 = _mm256_cvtepu8_epi16(color3_p);
    __m256i color4 = _mm256_cvtepu8_epi16(color4_p);
    __m256i color5 = _mm256_cvtepu8_epi16(color5_p);
    __m256i color6 = _mm256_cvtepu8_epi16(color6_p);
    __m256i color7 = _mm256_cvtepu8_epi16(color7_p);

    // multiply by weights
    color0 = _mm256_mullo_epi16(color0, weights0);
    color1 = _mm256_mullo_epi16(color1, weights1);
    color2 = _mm256_mullo_epi16(color2, weights2);
    color3 = _mm256_mullo_epi16(color3, weights3);
    color4 = _mm256_mullo_epi16(color4, weights4);
    color5 = _mm256_mullo_epi16(color5, weights5);
    color6 = _mm256_mullo_epi16(color6, weights6);
    color7 = _mm256_mullo_epi16(color7, weights7);

    // add. This order minimizes dependencies.
    color0 = _mm256_add_epi16(color0, color1);
    color2 = _mm256_add_epi16(color2, color3);
    color4 = _mm256_add_epi16(color4, color5);
    color6 = _mm256_add_epi16(color6, color7);

    color0 = _mm256_add_epi16(color0, color2);
    color4 = _mm256_add_epi16(color4, color6);

    color0 = _mm256_add_epi16(color0, color4);

    // divide, because we multiplied our weights by 2^7.
    color0 = _mm256_srli_epi16(color0, 7);

    // saturate
    color0 = _mm256_min_epu16(sat, color0);

    // back to u8s.
    auto hi = _mm256_extracti128_si256(color0, 1);
    auto result = _mm_packus_epi16(_mm256_castsi256_si128(color0), hi);

    // store result
    _mm_storeu_si128((__m128i*)(&out[color_quad * 4]), result);
  }
}

bool sphere_in_view_ref(const math::Vector4f& sphere, const math::Vector4f* planes) {
  math::Vector4f acc =
      planes[0] * sphere.x() + planes[1] * sphere.y() + planes[2] * sphere.z() - planes[3];

  return acc.x() > -sphere.w() && acc.y() > -sphere.w() && acc.z() > -sphere.w() &&
         acc.w() > -sphere.w();
}

// this isn't super efficient, but we spend so little time here it's not worth it to go faster.
void cull_check_all_slow(const math::Vector4f* planes,
                         const std::vector<tfrag3::VisNode>& nodes,
                         u8* out) {
  for (size_t i = 0; i < nodes.size(); i++) {
    out[i] = sphere_in_view_ref(nodes[i].bsphere, planes);
  }
}

u32 make_all_visible_index_list(std::pair<int, int>* group_out,
                                u32* idx_out,
                                const std::vector<tfrag3::StripDraw>& draws) {
  int idx_buffer_ptr = 0;
  for (size_t i = 0; i < draws.size(); i++) {
    const auto& draw = draws[i];
    std::pair<int, int> ds;
    ds.first = idx_buffer_ptr;
    memcpy(&idx_out[idx_buffer_ptr], draw.vertex_index_stream.data(),
           draw.vertex_index_stream.size() * sizeof(u32));
    idx_buffer_ptr += draw.vertex_index_stream.size();
    ds.second = idx_buffer_ptr;
    group_out[i] = ds;
  }
  return idx_buffer_ptr;
}

u32 make_index_list_from_vis_string(std::pair<int, int>* group_out,
                                    u32* idx_out,
                                    const std::vector<tfrag3::StripDraw>& draws,
                                    const std::vector<u8>& vis_data) {
  int idx_buffer_ptr = 0;
  for (size_t i = 0; i < draws.size(); i++) {
    const auto& draw = draws[i];
    int vtx_idx = 0;
    std::pair<int, int> ds;
    ds.first = idx_buffer_ptr;
    bool building_run = false;
    int run_start_out = 0;
    int run_start_in = 0;
    for (auto& grp : draw.vis_groups) {
      bool vis = grp.vis_idx == 0xffffffff || vis_data[grp.vis_idx];
      if (building_run) {
        if (vis) {
          idx_buffer_ptr += grp.num;
        } else {
          building_run = false;
          idx_buffer_ptr += grp.num;
          memcpy(&idx_out[run_start_out], &draw.vertex_index_stream[run_start_in],
                 (idx_buffer_ptr - run_start_out) * sizeof(u32));
        }
      } else {
        if (vis) {
          building_run = true;
          run_start_out = idx_buffer_ptr;
          run_start_in = vtx_idx;
          idx_buffer_ptr += grp.num;
        } else {
        }
      }
      vtx_idx += grp.num;
    }
    if (building_run) {
      memcpy(&idx_out[run_start_out], &draw.vertex_index_stream[run_start_in],
             (idx_buffer_ptr - run_start_out) * sizeof(u32));
    }

    ds.second = idx_buffer_ptr;
    group_out[i] = ds;
  }
  return idx_buffer_ptr;
}