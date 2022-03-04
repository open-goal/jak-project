#include "third-party/fmt/core.h"
#include "third-party/imgui/imgui.h"

#include "TextureUploadHandler.h"
#include "game/graphics/pipelines/opengl.h"
#include "game/graphics/opengl_renderer/EyeRenderer.h"

TextureUploadHandler::TextureUploadHandler(const std::string& name, BucketId my_id)
    : BucketRenderer(name, my_id) {}

void TextureUploadHandler::render(DmaFollower& dma,
                                  SharedRenderState* render_state,
                                  ScopedProfilerNode& prof) {
  // this is the data we get from the PC Port modification.
  std::vector<TextureUpload> uploads;

  // loop through all data, grabbing buckets
  while (dma.current_tag_offset() != render_state->next_bucket) {
    auto dma_tag = dma.current_tag();

    // does it look like data to do eye rendering?
    if (dma_tag.qwc == (128 / 16)) {
      // note: these uploads may have texture that we need for eye rendering.
      flush_uploads(uploads, render_state);
      render_state->eye_renderer->handle_eye_dma2(dma, render_state, prof);
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
    }
  }

  flush_uploads(uploads, render_state);
}

void TextureUploadHandler::flush_uploads(std::vector<TextureUpload>& uploads,
                                         SharedRenderState* render_state) {
  if (m_fake_uploads) {
    uploads.clear();
  } else {
    // NOTE: we don't actually copy the textures in the dma chain copying because they aren't
    // reference by DMA tag.  So there's the potential for race conditions if the game gets messed
    // up and corrupts the texture memory.
    const u8* ee_mem = (const u8*)render_state->ee_main_memory;
    if (uploads.size() == 2 && uploads[0].mode == 2 && uploads[1].mode == -2 &&
        uploads[0].page == uploads[1].page) {
      render_state->texture_pool->handle_upload_now(ee_mem + uploads[0].page, -2, ee_mem,
                                                    render_state->offset_of_s7);
      render_state->texture_pool->handle_upload_now(ee_mem + uploads[0].page, 2, ee_mem,
                                                    render_state->offset_of_s7);
    } else if (uploads.size() == 1 && uploads[0].mode == -1) {
      render_state->texture_pool->handle_upload_now(ee_mem + uploads[0].page, -1, ee_mem,
                                                    render_state->offset_of_s7);
    } else if (uploads.size() == 1 && uploads[0].mode == -2) {
      render_state->texture_pool->handle_upload_now(ee_mem + uploads[0].page, -2, ee_mem,
                                                    render_state->offset_of_s7);
    } else if (uploads.size() == 1 && uploads[0].mode == 0) {
      render_state->texture_pool->handle_upload_now(ee_mem + uploads[0].page, 0, ee_mem,
                                                    render_state->offset_of_s7);
    }

    else if (uploads.empty()) {
      // do nothing.
    } else {
      fmt::print("unhandled upload sequence in {}:\n", m_name);
      for (auto& upload : uploads) {
        fmt::print(" page: 0x{:x} mode: {}\n", upload.page, upload.mode);
      }
      ASSERT(false);
    }
  }
}
void TextureUploadHandler::draw_debug_window() {
  ImGui::Checkbox("Fake Uploads", &m_fake_uploads);
}
