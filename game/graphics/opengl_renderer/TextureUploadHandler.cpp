#include "third-party/fmt/core.h"
#include "third-party/imgui/imgui.h"

#include "TextureUploadHandler.h"
#include "game/graphics/pipelines/opengl.h"

TextureUploadHandler::TextureUploadHandler(const std::string& name, BucketId my_id)
    : BucketRenderer(name, my_id) {}

void TextureUploadHandler::render(DmaFollower& dma,
                                  SharedRenderState* render_state,
                                  ScopedProfilerNode& /*prof*/) {
  // this is the data we get from the PC Port modification.
  struct TextureUpload {
    u64 page;
    s64 mode;
  };

  std::vector<TextureUpload> uploads;

  // loop through all data, grabbing buckets
  while (dma.current_tag_offset() != render_state->next_bucket) {
    auto dma_tag = dma.current_tag();
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

  // if we're replaying a graphics dump, don't try to read ee memory
  // TODO, we might still want to grab stuff from the cache
  if (render_state->dump_playback) {
    return;
  }

  // NOTE: we don't actually copy the textures in the dma chain copying because they aren't
  // reference by DMA tag.  So there's the potential for race conditions if the game gets messed
  // up and corrupts the texture memory.
  const u8* ee_mem = (const u8*)render_state->ee_main_memory;

  // The logic here is a bit confusing. It works around an issue where higher LODs are uploaded
  // before their CLUT in some cases.
  if (uploads.size() == 2 && uploads[0].mode == 2 && uploads[1].mode == -2 &&
      uploads[0].page == uploads[1].page) {
    // couldn't find this texture in cache, need to convert it
    render_state->texture_pool->handle_upload_now(ee_mem + uploads[0].page, -2, ee_mem,
                                                  render_state->offset_of_s7);
    render_state->texture_pool->handle_upload_now(ee_mem + uploads[0].page, 2, ee_mem,
                                                  render_state->offset_of_s7);
    // after conversion, we should be able to populate the texture pool.
  } else if (uploads.size() == 1 && uploads[0].mode == -1) {
    // look at the texture page and determine if we have it in cache.
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

void TextureUploadHandler::draw_debug_window() {

}

void TextureUploadHandler::serialize(Serializer& ser) {

}