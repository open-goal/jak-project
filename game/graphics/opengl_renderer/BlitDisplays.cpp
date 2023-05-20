#include "BlitDisplays.h"

#include "common/log/log.h"

#include "game/graphics/opengl_renderer/OpenGLRenderer.h"
#include "game/graphics/pipelines/opengl.h"

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
  TextureInput in;
  glGenTextures(1, &in.gpu_texture);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
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
  auto back = render_state->back_fbo;
  bool valid = back && back->valid;

  // loop through all data
  while (dma.current_tag_offset() != render_state->next_bucket) {
    auto data = dma.read_and_advance();

    if (data.vifcode0().kind == VifCode::Kind::PC_PORT) {
      switch (data.vifcode0().immediate) {
        case 0x10: {  // copy buffer->texture (tbp in vif1)
          u32 tbp = data.vifcode1().immediate;
          ASSERT_MSG(tbp == m_tbp, fmt::format("unexpected tbp {}", tbp));
          if (valid) {
            // copy buffer texture -> custom texture
            auto my_tex_id = m_gpu_tex->gpu_textures.at(0).gl;
            int w = back->width, h = back->height;

            glBindTexture(GL_TEXTURE_2D, my_tex_id);
            glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, nullptr);

            glBindFramebuffer(GL_READ_FRAMEBUFFER, back->fbo_id);
            glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, 0, 0, w, h, 0);
            glBindFramebuffer(GL_READ_FRAMEBUFFER, 0);

            m_gpu_tex->w = w;
            m_gpu_tex->h = h;
            render_state->texture_pool->move_existing_to_vram(m_gpu_tex, m_tbp);
          } else {
            lg::error("no valid back buffer to blit!");
          }
        } break;
      }
    }
  }
}

void BlitDisplays::draw_debug_window() {}
