#include "Warp.h"

Warp::Warp(const std::string& name, int id, std::shared_ptr<Generic2> generic)
    : BucketRenderer(name, id), m_generic(generic) {}

void Warp::draw_debug_window() {
  m_generic->draw_debug_window();
}

void Warp::render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) {
  m_fb_copier.copy_now(render_state->render_fb_w, render_state->render_fb_h,
                       render_state->render_fb_x, render_state->render_fb_y,
                       render_state->render_fb);
  render_state->texture_pool->move_existing_to_vram(m_warp_src_tex, m_tbp);
  m_generic->render_in_mode(dma, render_state, prof, Generic2::Mode::WARP);
}

void Warp::init_textures(TexturePool& tex_pool, GameVersion version) {
  TextureInput in;
  // point to fb copier's texture.
  in.gpu_texture = m_fb_copier.texture();
  in.w = 32;
  in.h = 32;
  in.debug_page_name = "PC-WARP";
  in.debug_name = "PC-WARP";
  in.id = tex_pool.allocate_pc_port_texture(version);
  m_warp_src_tex = tex_pool.give_texture_and_load_to_vram(in, m_tbp);
}