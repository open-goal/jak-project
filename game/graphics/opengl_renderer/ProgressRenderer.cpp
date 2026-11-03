#include "ProgressRenderer.h"

#include <algorithm>
#include <cmath>

ProgressRenderer::ProgressRenderer(const std::string& name, int my_id, int batch_size)
    : DirectRenderer(name, my_id, batch_size),
      m_minimap_fb(kMinimapWidth, kMinimapHeight, GL_UNSIGNED_INT_8_8_8_8_REV) {}

void ProgressRenderer::pre_render() {
  m_current_fbp = kScreenFbp;
}

void ProgressRenderer::post_render() {
  m_fb_ctxt.reset();
  m_offscreen_mode = false;
}

void ProgressRenderer::init_textures(TexturePool& texture_pool, GameVersion version) {
  TextureInput in;
  in.gpu_texture = m_minimap_fb.texture();
  in.w = kMinimapWidth;
  in.h = kMinimapHeight;
  in.debug_page_name = "PC-MAP";
  in.debug_name = "map";
  in.id = texture_pool.allocate_pc_port_texture(version);
  m_minimap_gpu_tex = texture_pool.give_texture_and_load_to_vram(in, kMinimapVramAddr);
  // the game always treats the minimap as 128x128, even if we render it at a higher resolution.
  m_minimap_gpu_tex->ps2_w = kMinimapWidth;
  m_minimap_gpu_tex->ps2_h = kMinimapHeight;
}

void ProgressRenderer::handle_frame(u64 val,
                                    SharedRenderState* render_state,
                                    ScopedProfilerNode& prof) {
  GsFrame f(val);
  u32 fbp = f.fbp();
  bool flushed = false;
  if (fbp != m_current_fbp) {
    flush_pending(render_state, prof);
    flushed = true;
    m_prim_gl_state_needs_gl_update = true;
    m_current_fbp = fbp;
    switch (f.fbp()) {
      case kScreenFbp:  // 408
        m_fb_ctxt.reset();
        m_offscreen_mode = false;
        break;
      case kMinimapFbp:  // 126
        update_minimap_resolution(render_state);
        m_fb_ctxt.emplace(m_minimap_fb);
        // replace any other texture that the game loaded to this slot with our PC with the GPU
        // one that we assume will get written to now.
        render_state->texture_pool->move_existing_to_vram(m_minimap_gpu_tex, kMinimapVramAddr);
        m_offscreen_mode = true;
        break;
      default:
        fmt::print("Unknown fbp in ProgressRenderer: {}\n", f.fbp());
        ASSERT(false);
    }
  }

  bool write_rgb = f.fbmsk() != 0xffffff;
  if (write_rgb != m_test_state.write_rgb) {
    if (!flushed) {
      m_stats.flush_from_test++;
      flush_pending(render_state, prof);
    }

    m_test_state.write_rgb = write_rgb;
    m_test_state_needs_gl_update = true;
    m_prim_gl_state_needs_gl_update = true;
  }
}

/*!
 * The game draws the minimap to a 128x128 texture, then draws that on screen. If the minimap was
 * drawn with higher resolution textures from a texture pack, render it at a higher resolution
 * so the detail isn't lost. With the original textures, this stays at 128x128.
 */
void ProgressRenderer::update_minimap_resolution(SharedRenderState* render_state) {
  // only grow, so we don't keep resizing as textures change.
  m_minimap_tex_upscale = std::max(m_minimap_tex_upscale, m_max_offscreen_uv_upscale);
  m_max_offscreen_uv_upscale = 1.f;
  int scale = (int)std::ceil(m_minimap_tex_upscale);

  // no point going much bigger than the minimap is on screen (112 of the 416 lines).
  if (render_state->draw_region_h > 0) {
    const float screen_px = render_state->draw_region_h * (112.f / 416.f);
    scale = std::min(scale, (int)std::ceil(screen_px / kMinimapHeight));
  }
  scale = std::max(scale, 1);
  if (scale == m_minimap_scale) {
    return;
  }

  m_minimap_scale = scale;
  const int w = kMinimapWidth * scale;
  const int h = kMinimapHeight * scale;
  // resize the existing texture. It's the same OpenGL texture, so nothing else needs updating.
  glBindTexture(GL_TEXTURE_2D, m_minimap_fb.texture());
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV, nullptr);
  m_minimap_fb.update_texture_size(w, h);
  m_minimap_gpu_tex->w = w;
  m_minimap_gpu_tex->h = h;
}
