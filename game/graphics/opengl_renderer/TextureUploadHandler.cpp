#include "TextureUploadHandler.h"

#include "common/global_profiler/GlobalProfiler.h"
#include "common/log/log.h"

#include "game/graphics/opengl_renderer/EyeRenderer.h"
#include "game/graphics/pipelines/opengl.h"

#include "third-party/fmt/core.h"
#include "third-party/imgui/imgui.h"

TextureUploadHandler::TextureUploadHandler(const std::string& name,
                                           int my_id,
                                           std::shared_ptr<TextureAnimator> texture_animator,
                                           bool add_direct)
    : BucketRenderer(name, my_id), m_texture_animator(texture_animator) {
  if (add_direct) {
    m_direct = std::make_unique<DirectRenderer>(name, my_id, 1024 * 6);
    m_direct->set_mipmap(false);  // try rm
  }
}

void TextureUploadHandler::render(DmaFollower& dma,
                                  SharedRenderState* render_state,
                                  ScopedProfilerNode& prof) {
  // this is the data we get from the PC Port modification.
  m_upload_count = 0;
  std::vector<TextureUpload> uploads;
  if (m_direct) {
    m_direct->reset_state();
  }
  // loop through all data, grabbing buckets
  while (dma.current_tag_offset() != render_state->next_bucket) {
    auto dma_tag = dma.current_tag();

    auto vif0 = dma.current_tag_vifcode0();
    if (vif0.kind == VifCode::Kind::PC_PORT) {
      if (vif0.immediate == 12) {
        dma.read_and_advance();
        auto p = scoped_prof("texture-animator");
        // note: if both uploads and animator write to the pool, do uploads before the animator.
        flush_uploads(uploads, render_state);
        uploads.clear();
        if (m_direct) {
          m_direct->flush_pending(render_state, prof);
        }
        m_texture_animator->handle_texture_anim_data(dma, (const u8*)render_state->ee_main_memory,
                                                     render_state->texture_pool.get(),
                                                     render_state->frame_idx);
        if (m_direct) {
          m_direct->lookup_textures_again(render_state);
          m_direct->reinitialize_gl_state();
        }
      }
    }
    // does it look like data to do eye rendering?
    if (dma_tag.qwc == (128 / 16)) {
      // note: these uploads may have texture that we need for eye rendering.
      flush_uploads(uploads, render_state);
      render_state->eye_renderer->handle_eye_dma2(dma, render_state, prof);
      uploads.clear();
    }

    auto data = dma.read_and_advance();
    if (data.size_bytes == 0 && data.vif0() == 0 && data.vif1() == 0) {
      continue;
    }

    if (data.size_bytes == 16 && data.vifcode0().kind == VifCode::Kind::PC_PORT &&
        data.vif1() == 3) {
      TextureUpload upload_data;
      memcpy(&upload_data, data.data, sizeof(upload_data));
      uploads.push_back(upload_data);
      continue;
    }

    if (dma_tag.kind == DmaTag::Kind::CALL) {
      dma.read_and_advance();  // call
      dma.read_and_advance();  // cnt
      dma.read_and_advance();  // ret
      // on next
      ASSERT(dma.current_tag_offset() == render_state->next_bucket);
    } else if (m_direct) {
      if (data.vifcode0().kind == VifCode::Kind::DIRECT ||
          data.vifcode1().kind == VifCode::Kind::DIRECT) {
        m_direct->render_vif(data.vif0(), data.vif1(), data.data, data.size_bytes, render_state,
                             prof);
      }
    }
  }

  flush_uploads(uploads, render_state);
  if (m_direct) {
    m_direct->flush_pending(render_state, prof);
  }
}

void TextureUploadHandler::flush_uploads(std::vector<TextureUpload>& uploads,
                                         SharedRenderState* render_state) {
  auto p = scoped_prof("flush-uploads");
  if (m_fake_uploads) {
    uploads.clear();
  } else {
    m_upload_count += uploads.size();
    // NOTE: we don't actually copy the textures in the dma chain copying because they aren't
    // reference by DMA tag.  So there's the potential for race conditions if the game gets messed
    // up and corrupts the texture memory.
    const u8* ee_mem = (const u8*)render_state->ee_main_memory;
    for (auto& upload : uploads) {
      render_state->texture_pool->handle_upload_now(ee_mem + upload.page, upload.mode, ee_mem,
                                                    render_state->offset_of_s7, m_my_id == 999);
    }
  }
}
void TextureUploadHandler::draw_debug_window() {
  ImGui::Checkbox("Fake Uploads", &m_fake_uploads);
  ImGui::Text("Uploads: %d", m_upload_count);
  if (m_direct) {
    m_direct->draw_debug_window();
  }
}
