#include "BlitDisplays.h"

#include "common/log/log.h"

#include "game/graphics/opengl_renderer/Fbo.h"

BlitDisplays::BlitDisplays(const std::string& name, int my_id) : BucketRenderer(name, my_id) {}

void BlitDisplays::init_textures(TexturePool& texture_pool, GameVersion version) {
  // set up target texture
  u32 tbp = 0;
  switch (version) {
    case GameVersion::Jak2:
      tbp = 0x3300;
      break;
    default:
      ASSERT_NOT_REACHED();
  }
  m_copier = std::make_unique<FramebufferCopier>();
  TextureInput in;
  in.gpu_texture = m_copier->texture();
  in.w = 32;
  in.h = 32;
  in.debug_page_name = "PC-BLIT";
  in.debug_name = fmt::format("blit-display");
  in.id = texture_pool.allocate_pc_port_texture(version);
  m_gpu_tex = texture_pool.give_texture_and_load_to_vram(in, tbp);
  m_tbp = tbp;
}

void BlitDisplays::render(DmaFollower& dma,
                          SharedRenderState* render_state,
                          ScopedProfilerNode& /*prof*/) {
  // loop through all data
  while (dma.current_tag_offset() != render_state->next_bucket) {
    auto data = dma.read_and_advance();

    if (data.vifcode0().kind == VifCode::Kind::PC_PORT) {
      switch (data.vifcode0().immediate) {
        case 0x10: {  // copy buffer->texture (tbp in vif1)
          u32 tbp = data.vifcode1().immediate;
          ASSERT_MSG(tbp == m_tbp, fmt::format("unexpected tbp {}", tbp));
          // copy buffer texture -> custom texture
          m_copier->copy_now(render_state->render_fb_w, render_state->render_fb_h,
                             render_state->render_fb);
          m_gpu_tex->w = render_state->render_fb_w;
          m_gpu_tex->h = render_state->render_fb_h;
          render_state->texture_pool->move_existing_to_vram(m_gpu_tex, m_tbp);

        } break;
        case 0x11: {
          // this case is "do nothing" on PS2 because the countdown is nonzero.
          // On the PS2, there were two framebuffers. The first was where triangles were directly
          // rendered to. Then, this was copied to the final buffer. These buffers were slightly
          // different resolution.

          // On PC, we don't imitate this 2-buffer setup. So in cases where the original game
          // skipped doing a copy, it would render to the first buffer but never copy to the second.
          // On PC, we set this flag which means that after rendering is finished we copy the saved
          // buffer back.
          m_copy_back_pending = true;
        } break;
      }
    }
  }

  // after reading the framebuffer, clear it.
  // The upcoming sky renderer bucket will clear it, but can't clear stuff in the letterbox regions,
  // so we manually do the clear here. Note that we need to clear the window (framebuffer 0) in case
  // the resolution/aspect/size changed, and there is a different letterbox than last frame.
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClearDepth(0.0);
  glDepthMask(GL_TRUE);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
  glDisable(GL_BLEND);

  glBindFramebuffer(GL_FRAMEBUFFER, render_state->render_fb);
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClearDepth(0.0);
  glClearStencil(0);
  glDepthMask(GL_TRUE);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
  glDisable(GL_BLEND);
  render_state->stencil_dirty = false;
}

void BlitDisplays::do_copy_back(SharedRenderState* render_state) {
  if (m_copy_back_pending) {
    if (render_state->render_fb_w == m_copier->width() &&
        render_state->render_fb_h == m_copier->height()) {
      m_copier->copy_back_now(render_state->render_fb_w, render_state->render_fb_h,
                              render_state->render_fb);
    }
    m_copy_back_pending = false;
  }
}

void BlitDisplays::draw_debug_window() {
  glBindTexture(GL_TEXTURE_2D, m_copier->texture());
  int w, h;
  glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, &w);
  glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, &h);
  ImGui::Image((void*)(u64)m_copier->texture(), ImVec2(w, h));
}
