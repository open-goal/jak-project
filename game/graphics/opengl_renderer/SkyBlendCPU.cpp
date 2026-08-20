#include "SkyBlendCPU.h"

#include "common/util/os.h"
#include "common/util/simd_util.h"

#include "game/graphics/opengl_renderer/AdgifHandler.h"

#ifdef __aarch64__
#include <arm_neon.h>
#endif

SkyBlendCPU::SkyBlendCPU() {
  for (int i = 0; i < 2; i++) {
    glGenTextures(1, &m_textures[i].gl);
    glBindTexture(GL_TEXTURE_2D, m_textures[i].gl);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, m_sizes[i], m_sizes[i], 0, GL_RGBA,
                 GL_UNSIGNED_INT_8_8_8_8_REV, 0);
    m_texture_data[i].resize(4 * m_sizes[i] * m_sizes[i]);
  }
}

SkyBlendCPU::~SkyBlendCPU() {
  for (auto& tex : m_textures) {
    glDeleteTextures(1, &tex.gl);
  }
}

/*!
 * out[i] = saturate_u8((in[i] * intensity) >> 7)
 */
void blend_sky_initial_fast(u8 intensity, u8* out, const u8* in, u32 size) {
#ifdef __aarch64__
  // widen u8 to u16, multiply, shift, then narrow with saturation. 255*255 fits in a u16 so
  // the multiply can't overflow, and vqmovn_u16 clamps at 255 like packus does.
  const uint16x8_t intensity_vec = vdupq_n_u16(intensity);
  u32 i = 0;
  for (; i + 16 <= size; i += 16) {
    const uint8x16_t tex = vld1q_u8(in + i);
    const uint16x8_t lo = vshrq_n_u16(vmulq_u16(vmovl_u8(vget_low_u8(tex)), intensity_vec), 7);
    const uint16x8_t hi = vshrq_n_u16(vmulq_u16(vmovl_u8(vget_high_u8(tex)), intensity_vec), 7);
    vst1q_u8(out + i, vcombine_u8(vqmovn_u16(lo), vqmovn_u16(hi)));
  }
  for (; i < size; i++) {
    const u32 v = ((u32)in[i] * intensity) >> 7;
    out[i] = v > 255 ? 255 : (u8)v;
  }
#else
  if (get_cpu_info().has_avx2) {
#ifdef __AVX2__
    __m256i intensity_vec = _mm256_set1_epi16(intensity);
    for (u32 i = 0; i < size / 16; i++) {
      __m128i tex_data8 = _mm_loadu_si128((const __m128i*)(in + (i * 16)));
      __m256i tex_data16 = _mm256_cvtepu8_epi16(tex_data8);
      tex_data16 = _mm256_mullo_epi16(tex_data16, intensity_vec);
      tex_data16 = _mm256_srli_epi16(tex_data16, 7);
      auto hi = _mm256_extracti128_si256(tex_data16, 1);
      auto result = _mm_packus_epi16(_mm256_castsi256_si128(tex_data16), hi);
      _mm_storeu_si128((__m128i*)(out + (i * 16)), result);
    }
#else
    ASSERT(false);
#endif
  } else {
    __m128i intensity_vec = _mm_set1_epi16(intensity);
    for (u32 i = 0; i < size / 8; i++) {
      __m128i tex_data8 = _mm_loadu_si64((const __m128i*)(in + (i * 8)));
      __m128i tex_data16 = _mm_cvtepu8_epi16(tex_data8);
      tex_data16 = _mm_mullo_epi16(tex_data16, intensity_vec);
      tex_data16 = _mm_srli_epi16(tex_data16, 7);
      auto result = _mm_packus_epi16(tex_data16, tex_data16);
      _mm_storel_epi64((__m128i*)(out + (i * 8)), result);
    }
  }
#endif
}

/*!
 * out[i] = saturating_add_u8(out[i], saturate_u8((in[i] * intensity) >> 7))
 */
void blend_sky_fast(u8 intensity, u8* out, const u8* in, u32 size) {
#ifdef __aarch64__
  const uint16x8_t intensity_vec = vdupq_n_u16(intensity);
  u32 i = 0;
  for (; i + 16 <= size; i += 16) {
    const uint8x16_t tex = vld1q_u8(in + i);
    const uint8x16_t cur = vld1q_u8(out + i);
    const uint16x8_t lo = vshrq_n_u16(vmulq_u16(vmovl_u8(vget_low_u8(tex)), intensity_vec), 7);
    const uint16x8_t hi = vshrq_n_u16(vmulq_u16(vmovl_u8(vget_high_u8(tex)), intensity_vec), 7);
    // vqmovn_u16 clamps at 255, so it covers both the min and the packus the x86 path does
    vst1q_u8(out + i, vqaddq_u8(cur, vcombine_u8(vqmovn_u16(lo), vqmovn_u16(hi))));
  }
  for (; i < size; i++) {
    u32 v = ((u32)in[i] * intensity) >> 7;
    if (v > 255)
      v = 255;
    const u32 sum = out[i] + v;
    out[i] = sum > 255 ? 255 : (u8)sum;
  }
#else
  if (get_cpu_info().has_avx2) {
#ifdef __AVX2__
    __m256i intensity_vec = _mm256_set1_epi16(intensity);
    __m256i max_intensity = _mm256_set1_epi16(255);
    for (u32 i = 0; i < size / 16; i++) {
      __m128i tex_data8 = _mm_loadu_si128((const __m128i*)(in + (i * 16)));
      __m128i out_val = _mm_loadu_si128((const __m128i*)(out + (i * 16)));
      __m256i tex_data16 = _mm256_cvtepu8_epi16(tex_data8);
      tex_data16 = _mm256_mullo_epi16(tex_data16, intensity_vec);
      tex_data16 = _mm256_srli_epi16(tex_data16, 7);
      tex_data16 = _mm256_min_epi16(max_intensity, tex_data16);
      auto hi = _mm256_extracti128_si256(tex_data16, 1);
      auto result = _mm_packus_epi16(_mm256_castsi256_si128(tex_data16), hi);
      out_val = _mm_adds_epu8(out_val, result);
      _mm_storeu_si128((__m128i*)(out + (i * 16)), out_val);
    }
#else
    ASSERT(false);
#endif
  } else {
    __m128i intensity_vec = _mm_set1_epi16(intensity);
    __m128i max_intensity = _mm_set1_epi16(255);
    for (u32 i = 0; i < size / 8; i++) {
      __m128i tex_data8 = _mm_loadu_si64((const __m128i*)(in + (i * 8)));
      __m128i out_val = _mm_loadu_si64((const __m128i*)(out + (i * 8)));
      __m128i tex_data16 = _mm_cvtepu8_epi16(tex_data8);
      tex_data16 = _mm_mullo_epi16(tex_data16, intensity_vec);
      tex_data16 = _mm_srli_epi16(tex_data16, 7);
      tex_data16 = _mm_min_epi16(max_intensity, tex_data16);
      auto result = _mm_packus_epi16(tex_data16, tex_data16);
      out_val = _mm_adds_epu8(out_val, result);
      _mm_storel_epi64((__m128i*)(out + (i * 8)), out_val);
    }
  }
#endif
}

SkyBlendStats SkyBlendCPU::do_sky_blends(DmaFollower& dma,
                                         SharedRenderState* render_state,
                                         ScopedProfilerNode& /*prof*/) {
  SkyBlendStats stats;

  while (dma.current_tag().qwc == 6) {
    // assuming that the vif and gif-tag is correct
    auto setup_data = dma.read_and_advance();

    // first is an adgif
    AdgifHelper adgif(setup_data.data + 16);
    ASSERT(adgif.is_normal_adgif());
    ASSERT(adgif.alpha().data == 0x8000000068);  // Cs + Cd

    // next is the actual draw
    auto draw_data = dma.read_and_advance();
    ASSERT(draw_data.size_bytes == 6 * 16);

    GifTag draw_or_blend_tag(draw_data.data);

    // the first draw overwrites the previous frame's draw by disabling alpha blend (ABE = 0)
    bool is_first_draw = !GsPrim(draw_or_blend_tag.prim()).abe();

    // here's we're relying on the format of the drawing to get the alpha/offset.
    u32 coord;
    u32 intensity;
    memcpy(&coord, draw_data.data + (5 * 16), 4);
    memcpy(&intensity, draw_data.data + 16, 4);

    // we didn't parse the render-to-texture setup earlier, so we need a way to tell sky from
    // clouds. we can look at the drawing coordinates to tell - the sky is smaller than the clouds.
    int buffer_idx = 0;
    if (coord == 0x200) {
      // sky
      buffer_idx = 0;
    } else if (coord == 0x400) {
      buffer_idx = 1;
    } else {
      ASSERT(false);  // bad data
    }

    // look up the source texture
    auto tex = render_state->texture_pool->lookup_gpu_texture(adgif.tex0().tbp0());
    ASSERT(tex);

    // slow version
    /*
    if (is_first_draw) {
      memset(m_texture_data[buffer_idx].data(), 0, m_texture_data[buffer_idx].size());
    }

    // intensities should be 0-128 (maybe higher is okay, but I don't see how this could be
    // generated with the GOAL code.)
    ASSERT(intensity <= 128);
    ASSERT(m_texture_data[buffer_idx].size() == tex->data.size());
    for (size_t i = 0; i < m_texture_data[buffer_idx].size(); i++) {
      u32 val = tex->data[i] * intensity;
      val >>= 7;
      m_texture_data[buffer_idx][i] += val;
    }
     */
    if (tex->get_data_ptr()) {
      if (m_texture_data[buffer_idx].size() == tex->data_size()) {
        if (is_first_draw) {
          blend_sky_initial_fast(intensity, m_texture_data[buffer_idx].data(), tex->get_data_ptr(),
                                 m_texture_data[buffer_idx].size());
        } else {
          blend_sky_fast(intensity, m_texture_data[buffer_idx].data(), tex->get_data_ptr(),
                         m_texture_data[buffer_idx].size());
        }
      }

      if (buffer_idx == 0) {
        if (is_first_draw) {
          stats.sky_draws++;
        } else {
          stats.sky_blends++;
        }
      } else {
        if (is_first_draw) {
          stats.cloud_draws++;
        } else {
          stats.cloud_blends++;
        }
      }
      glBindTexture(GL_TEXTURE_2D, m_textures[buffer_idx].gl);
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, m_sizes[buffer_idx], m_sizes[buffer_idx], 0, GL_RGBA,
                   GL_UNSIGNED_INT_8_8_8_8_REV, m_texture_data[buffer_idx].data());

      render_state->texture_pool->move_existing_to_vram(m_textures[buffer_idx].tex,
                                                        m_textures[buffer_idx].tbp);
    }
  }

  return stats;
}

void SkyBlendCPU::init_textures(TexturePool& tex_pool, GameVersion version) {
  for (int i = 0; i < 2; i++) {
    // update it
    glBindTexture(GL_TEXTURE_2D, m_textures[i].gl);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, m_sizes[i], m_sizes[i], 0, GL_RGBA,
                 GL_UNSIGNED_INT_8_8_8_8_REV, m_texture_data[i].data());
    TextureInput in;

    in.gpu_texture = m_textures[i].gl;
    in.w = m_sizes[i];
    in.h = m_sizes[i];
    in.debug_name = fmt::format("PC-SKY-CPU-{}", i);
    in.id = tex_pool.allocate_pc_port_texture(version);
    u32 tbp = SKY_TEXTURE_VRAM_ADDRS[i];
    m_textures[i].tex = tex_pool.give_texture_and_load_to_vram(in, tbp);
    m_textures[i].tbp = tbp;
  }
}