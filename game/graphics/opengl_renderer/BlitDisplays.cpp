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
    case GameVersion::Jak3:
      tbp = 0x3300;  // assuming this for now...
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
                          ScopedProfilerNode& prof) {
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
        case 0x13: {
          // copy from render buffer
          m_blur_old_copier.copy_now(render_state->render_fb_w, render_state->render_fb_h,
                                     render_state->render_fb);

          memcpy(&m_zoom_blur, data.data, sizeof(PcZoomBlur));
          m_zoom_blur_pending = true;
        } break;
        case 0x14: {
          m_color_filter_pending = true;
          memcpy(m_color_filter.data(), data.data, sizeof(math::Vector4f));
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

void BlitDisplays::do_copy_back(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  if (m_copy_back_pending) {
    if (render_state->render_fb_w == m_copier->width() &&
        render_state->render_fb_h == m_copier->height()) {
      m_copier->copy_back_now(render_state->render_fb_w, render_state->render_fb_h,
                              render_state->render_fb);
    }
    m_copy_back_pending = false;
  } else if (m_zoom_blur_pending) {
    do_zoom_blur(render_state, prof);
    m_zoom_blur_pending = false;
  }
}

void BlitDisplays::do_zoom_blur(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  const float texels = m_zoom_blur.texels;
  float xmin, xmax, ymin, ymax;
  if (m_zoom_blur.is_2d) {
    const float f2_0 = (texels / 512.f) * m_zoom_blur.pos.x();
    const float f0_6 = 512.f - (texels - f2_0);
    const float f3_1 = (texels / 416.f) * m_zoom_blur.pos.y();
    const float f1_4 = 416.f - (texels - f3_1);

    xmin = f2_0 / 512.f;
    xmax = (f0_6 - 1.f) / 512.f;
    ymin = f3_1 / 416.f;
    ymax = (f1_4 - 1.f) / 416.f;

  } else {
    const float f1_10 = (texels / 512.f) * m_zoom_blur.pos.x();
    const float f2_8 = std::max(0.f, std::min(f1_10, texels));
    const float f0_22 = 512.f - (texels - f2_8);
    const float f3_4 = (texels / 416.f) * m_zoom_blur.pos.y();
    const float f3_6 = std::max(0.f, std::min(f3_4, texels));
    const float f1_16 = 416.f - (texels - f3_6);

    xmin = f2_8 / 512.f;
    xmax = (f0_22 - 1.f) / 512.f;
    ymin = f3_6 / 416.f;
    ymax = (f1_16 - 1.f) / 416.f;
  }

  m_blur_new_copier.copy_now(render_state->render_fb_w, render_state->render_fb_h,
                             render_state->render_fb);

  // clear screen
  glBindFramebuffer(GL_FRAMEBUFFER, render_state->render_fb);
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT);

  // GL Setup
  glDisable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glBindTexture(GL_TEXTURE_2D, m_blur_old_copier.texture());

  // zoom blur draw
  m_fullscreen_tex_draw.draw(m_zoom_blur.color.cast<float>() / 128.f, math::Vector2f{xmin, ymin},
                             math::Vector2f{xmax, ymax}, render_state, prof);

  // screen draw
  glBindTexture(GL_TEXTURE_2D, m_blur_new_copier.texture());
  m_fullscreen_tex_draw.draw(math::Vector4f{1.f, 1.f, 1.f, m_zoom_blur.alpha_current},
                             math::Vector2f{0, 0}, math::Vector2f{1, 1}, render_state, prof);
}

void BlitDisplays::draw_debug_window() {
  glBindTexture(GL_TEXTURE_2D, m_copier->texture());
  int w, h;
  glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, &w);
  glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, &h);
  ImGui::Image((ImTextureID)(intptr_t)m_copier->texture(), ImVec2(w, h));
}

void BlitDisplays::apply_color_filter(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  if (m_color_filter_pending) {
    glEnable(GL_BLEND);
    glBlendEquation(GL_FUNC_ADD);
    glBlendFunc(GL_DST_COLOR, GL_ZERO);
    m_color_draw.draw(m_color_filter, render_state, prof);
    m_color_filter_pending = false;
  }
}
