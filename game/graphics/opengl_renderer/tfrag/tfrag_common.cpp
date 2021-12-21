

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
  // about 70 us, not bad.
  // fmt::print("interp {} colors {:.2f} ms\n", in.size(), interp_timer.getMs());
}

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

std::string print_16s(__m256 vals) {
  u16 mem[16];
  _mm256_storeu_si256((__m256i*)mem, vals);
  std::string result = "[";
  for (int i = 0; i < 16; i++) {
    result += fmt::format("0x{:4x} ", mem[i]);
    if (i && (i % 4) == 0) {
      result += fmt::format("| ");
    }
  }
  result.back() = ']';
  return result;
};

std::string print_8s(__m256 vals) {
  u8 mem[32];
  _mm256_storeu_si256((__m256i*)mem, vals);
  std::string result = "[";
  for (int i = 0; i < 32; i++) {
    result += fmt::format("0x{:2x} ", mem[i]);
    if (i && (i % 8) == 0) {
      result += fmt::format("| ");
    }
  }
  result.back() = ']';
  return result;
};

void interp_time_of_day_fast(const float weights[8],
                             const SwizzledTimeOfDay& in,
                             math::Vector<u8, 4>* out) {
  __m256 zero = _mm256_set1_epi16(0);
  __m256 weights0 = _mm256_set1_epi16(weights[0] * 128.f);
  __m256 weights1 = _mm256_set1_epi16(weights[1] * 128.f);
  __m256 weights2 = _mm256_set1_epi16(weights[2] * 128.f);
  __m256 weights3 = _mm256_set1_epi16(weights[3] * 128.f);
  __m256 weights4 = _mm256_set1_epi16(weights[4] * 128.f);
  __m256 weights5 = _mm256_set1_epi16(weights[5] * 128.f);
  __m256 weights6 = _mm256_set1_epi16(weights[6] * 128.f);
  __m256 weights7 = _mm256_set1_epi16(weights[7] * 128.f);
  //
  //  fmt::print("weights\n");
  //  fmt::print("{}\n{}\n{}\n{}\n{}\n{}\n{}\n{}\n", print_16s(weights0), print_16s(weights1))

    __m256 sat = _mm256_set_epi16(128, 255, 255, 255, 128, 255, 255, 255, 128, 255, 255, 255, 128,
                                  255, 255, 255);
  __m256 hack = _mm256_set_epi16(128, 255, 255, 255, 128, 255, 255, 255, 128, 255, 255, 255, 128,
                               255, 255, 255);
//  __m256 sat = _mm256_set_epi16(255, 255, 255, 128, 255, 255, 255, 128, 255, 255, 255, 128, 255,
//                                255, 255, 128);

  for (u32 color_quad = 0; color_quad < in.color_count / 4; color_quad++) {
    // first, load colors. We put 16 bytes / register because we will unpack eventually
    const u8* base = in.data.data() + color_quad * 128;
    __m256 color0 = _mm256_castsi128_si256(_mm_loadu_si128((const __m128i*)(base + 0)));
    __m256 color1 = _mm256_castsi128_si256(_mm_loadu_si128((const __m128i*)(base + 16)));
    __m256 color2 = _mm256_castsi128_si256(_mm_loadu_si128((const __m128i*)(base + 32)));
    __m256 color3 = _mm256_castsi128_si256(_mm_loadu_si128((const __m128i*)(base + 48)));
    __m256 color4 = _mm256_castsi128_si256(_mm_loadu_si128((const __m128i*)(base + 64)));
    __m256 color5 = _mm256_castsi128_si256(_mm_loadu_si128((const __m128i*)(base + 80)));
    __m256 color6 = _mm256_castsi128_si256(_mm_loadu_si128((const __m128i*)(base + 96)));
    __m256 color7 = _mm256_castsi128_si256(_mm_loadu_si128((const __m128i*)(base + 112)));

    // unpack to 16-bits. each has 16.
    color0 = _mm256_unpacklo_epi8(color0, zero);
    color1 = _mm256_unpacklo_epi8(color1, zero);
    color2 = _mm256_unpacklo_epi8(color2, zero);
    color3 = _mm256_unpacklo_epi8(color3, zero);
    color4 = _mm256_unpacklo_epi8(color4, zero);
    color5 = _mm256_unpacklo_epi8(color5, zero);
    color6 = _mm256_unpacklo_epi8(color6, zero);
    color7 = _mm256_unpacklo_epi8(color7, zero);

    color0 = _mm256_mullo_epi16(color0, weights0);
    color1 = _mm256_mullo_epi16(color1, weights1);
    color2 = _mm256_mullo_epi16(color2, weights2);
    color3 = _mm256_mullo_epi16(color3, weights3);
    color4 = _mm256_mullo_epi16(color4, weights4);
    color5 = _mm256_mullo_epi16(color5, weights5);
    color6 = _mm256_mullo_epi16(color6, weights6);
    color7 = _mm256_mullo_epi16(color7, weights7);

    color0 = _mm256_add_epi16(color0, color1);
    color2 = _mm256_add_epi16(color2, color3);
    color4 = _mm256_add_epi16(color4, color5);
    color6 = _mm256_add_epi16(color6, color7);

    color0 = _mm256_add_epi16(color0, color2);
    color4 = _mm256_add_epi16(color4, color6);

    color0 = _mm256_add_epi16(color0, color4);
    color0 = _mm256_srli_epi16(color0, 7);

    __m256 mask = _mm256_cmpgt_epi16( sat, color0);
    color0 = _mm256_blendv_epi8(sat, color0, mask);
    __m128i result = _mm256_castsi256_si128(_mm256_packus_epi16(color0, _mm256_permute2f128_si256(color0, color0, 1)));
    _mm_storeu_si128((__m128i_u*)(&out[color_quad * 4]), result);
  }
}

bool sphere_in_view_ref(const math::Vector4f& sphere, const math::Vector4f* planes) {
  /*
   *(let ((v1-0 *math-camera*))
    (.lvf vf6 (&-> arg0 quad))
    (.lvf vf1 (&-> v1-0 plane 0 quad))
    (.lvf vf2 (&-> v1-0 plane 1 quad))
    (.lvf vf3 (&-> v1-0 plane 2 quad))
    (.lvf vf4 (&-> v1-0 plane 3 quad))
    )
   (.mul.x.vf acc vf1 vf6)
   (.add.mul.y.vf acc vf2 vf6 acc)
   (.add.mul.z.vf acc vf3 vf6 acc)
   (.sub.mul.w.vf vf5 vf4 vf0 acc)
   (.add.w.vf vf5 vf5 vf6)
   (.mov v1-1 vf5)
   (.pcgtw v1-2 r0-0 v1-1)
   (.ppach v1-3 r0-0 v1-2)
   (zero? (the-as int v1-3))
   */

  math::Vector4f acc =
      planes[0] * sphere.x() + planes[1] * sphere.y() + planes[2] * sphere.z() - planes[3];

  return acc.x() > -sphere.w() && acc.y() > -sphere.w() && acc.z() > -sphere.w() &&
         acc.w() > -sphere.w();
}


void cull_check_all_slow(const math::Vector4f* planes,
                  const std::vector<tfrag3::VisNode>& nodes,
                  u8* out) {
  for (size_t i = 0; i < nodes.size(); i++) {
    out[i] = sphere_in_view_ref(nodes[i].bsphere, planes);
  }
}