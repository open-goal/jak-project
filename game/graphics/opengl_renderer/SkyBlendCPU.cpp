#include "SkyBlendCPU.h"
#include "game/graphics/opengl_renderer/AdgifHandler.h"

#include <immintrin.h>

SkyBlendCPU::SkyBlendCPU() {
  glGenTextures(2, m_textures);
  for (int i = 0; i < 2; i++) {
    glBindTexture(GL_TEXTURE_2D, m_textures[i]);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, m_sizes[i], m_sizes[i], 0, GL_RGBA,
                 GL_UNSIGNED_INT_8_8_8_8_REV, 0);
    m_texture_data[i].resize(4 * m_sizes[i] * m_sizes[i]);
  }
}

SkyBlendCPU::~SkyBlendCPU() {
  glDeleteTextures(2, m_textures);
}

void blend_sky_initial_fast(u8 intensity, u8* out, const u8* in, u32 size) {
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
}

void blend_sky_fast(u8 intensity, u8* out, const u8* in, u32 size) {
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
}

SkyBlendStats SkyBlendCPU::do_sky_blends(DmaFollower& dma,
                                         SharedRenderState* render_state,
                                         ScopedProfilerNode& /*prof*/) {
  SkyBlendStats stats;

  Timer sky_timer;
  while (dma.current_tag().qwc == 6) {
    // assuming that the vif and gif-tag is correct
    auto setup_data = dma.read_and_advance();
    if (render_state->dump_playback) {
      // continue;
    }

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
    auto tex = render_state->texture_pool->lookup(adgif.tex0().tbp0());
    ASSERT(tex);
    ASSERT(!tex->only_on_gpu);  // we need the actual data!!

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
    if (is_first_draw) {
      blend_sky_initial_fast(intensity, m_texture_data[buffer_idx].data(), tex->data.data(),
                             tex->data.size());
    } else {
      blend_sky_fast(intensity, m_texture_data[buffer_idx].data(), tex->data.data(),
                     tex->data.size());
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
  }

  // put in pool.
  if (render_state->dump_playback) {
    return stats;
  }
  for (int i = 0; i < 2; i++) {
    // todo - these are hardcoded and rely on the vram layout.
    u32 tbp = i == 0 ? 8064 : 8096;

    // lookup existing, or create a new entry
    TextureRecord* tex = render_state->texture_pool->lookup(tbp);
    if (!tex) {
      auto tsp = std::make_shared<TextureRecord>();
      render_state->texture_pool->set_texture(tbp, tsp);
      tex = tsp.get();
    }

    // update it
    glBindTexture(GL_TEXTURE_2D, m_textures[i]);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, m_sizes[i], m_sizes[i], 0, GL_RGBA,
                 GL_UNSIGNED_INT_8_8_8_8_REV, m_texture_data[i].data());

    tex->gpu_texture = m_textures[i];
    tex->on_gpu = true;
    tex->only_on_gpu = true;
    tex->do_gc = false;
    tex->w = m_sizes[i];
    tex->h = m_sizes[i];
    tex->name = fmt::format("PC-SKY-{}", i);
  }
  //  fmt::print("sky blend took {:.2f} ms\n", sky_timer.getMs());

  return stats;
}