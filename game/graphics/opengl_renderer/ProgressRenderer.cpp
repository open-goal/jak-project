#include "ProgressRenderer.h"

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