#include "third-party/fmt/core.h"
#include "third-party/imgui/imgui.h"

#include "TextureUploadHandler.h"
#include "game/graphics/pipelines/opengl.h"

TextureUploadHandler::TextureUploadHandler(const std::string& name, BucketId my_id)
    : BucketRenderer(name, my_id) {}

void TextureUploadHandler::render(DmaFollower& dma, SharedRenderState* render_state) {
  m_stats = {};

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
      assert(dma.current_tag_offset() == render_state->next_bucket);
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
    bool has_segment[3] = {true, true, true};
    if (!try_to_populate_from_cache(uploads[0].page, has_segment, render_state)) {
      // couldn't find this texture in cache, need to convert it
      populate_cache(render_state->texture_pool->convert_textures(
                         ee_mem + uploads[0].page, -2, ee_mem, render_state->offset_of_s7),
                     render_state);
      populate_cache(render_state->texture_pool->convert_textures(
                         ee_mem + uploads[0].page, 2, ee_mem, render_state->offset_of_s7),
                     render_state);
      // after conversion, we should be able to populate the texture pool.
      bool ok = try_to_populate_from_cache(uploads[0].page, has_segment, render_state);
      assert(ok);
    }

  } else if (uploads.size() == 1 && uploads[0].mode == -1) {
    // look at the texture page and determine if we have it in cache.
    bool has_segment[3] = {true, true, true};
    if (!try_to_populate_from_cache(uploads[0].page, has_segment, render_state)) {
      populate_cache(render_state->texture_pool->convert_textures(
                         ee_mem + uploads[0].page, -1, ee_mem, render_state->offset_of_s7),
                     render_state);
      bool ok = try_to_populate_from_cache(uploads[0].page, has_segment, render_state);
      assert(ok);
    }

  } else if (uploads.size() == 1 && uploads[0].mode == -2) {
    bool has_segment[3] = {true, true, true};
    if (!try_to_populate_from_cache(uploads[0].page, has_segment, render_state)) {
      populate_cache(render_state->texture_pool->convert_textures(
                         ee_mem + uploads[0].page, -2, ee_mem, render_state->offset_of_s7),
                     render_state);
      bool ok = try_to_populate_from_cache(uploads[0].page, has_segment, render_state);
      assert(ok);
    }

  } else if (uploads.empty()) {
    // do nothing.
  } else {
    fmt::print("unhandled upload sequence in {}:\n", m_name);
    for (auto& upload : uploads) {
      fmt::print(" page: 0x{:x} mode: {}\n", upload.page, upload.mode);
    }
    assert(false);
  }
}

void TextureUploadHandler::draw_debug_window() {
  ImGui::Text("Textures this frame: %d", m_stats.textures_provided);
  ImGui::Text("Textures converted: %d", m_stats.textures_converted);
  ImGui::Text("Textures replaced: %d", m_stats.textures_evicted);
}

namespace {
const char* goal_string(u32 ptr, const u8* memory_base) {
  if (ptr == 0) {
    assert(false);
  }
  return (const char*)(memory_base + ptr + 4);
}
}  // namespace

/*!
 * Try to set an entry in the texture pool from a cached texture for the given page (GOAL pointer).
 */
bool TextureUploadHandler::try_to_populate_from_cache(u64 page,
                                                      const bool with_seg[3],
                                                      SharedRenderState* render_state) {
  auto old_tex_provided = m_stats.textures_provided;
  const u8* ee_mem = (const u8*)render_state->ee_main_memory;
  auto tpage = ee_mem + page;
  GoalTexturePage texture_page;
  memcpy(&texture_page, tpage, sizeof(GoalTexturePage));

  // loop over all textures in the page
  for (int tex_idx = 0; tex_idx < texture_page.length; tex_idx++) {
    // we might have some invalid textures, for whatever reason. The PS2 side checks for this.
    GoalTexture tex;
    if (texture_page.try_copy_texture_description(&tex, tex_idx, ee_mem, tpage,
                                                  render_state->offset_of_s7)) {
      // loop over all mip levels of this texture
      for (int mip_idx = 0; mip_idx < tex.num_mips; mip_idx++) {
        // only grab mip levels that we requested (we don't want to overwrite vram that the engine
        // expects us to not touch)
        if (with_seg[tex.segment_of_mip(mip_idx)]) {
          m_stats.textures_provided++;

          // lookup the texture by name!
          auto it = m_tex_cache.find(goal_string(tex.name_ptr, ee_mem));
          if (it == m_tex_cache.end() || !it->second.at(mip_idx)) {
            // failed to find it, reject the entire page load
            m_stats.textures_provided = old_tex_provided;
            return false;
          } else {
            // found it! Set it in the pool (just setting a pointer)
            render_state->texture_pool->set_texture(tex.dest[mip_idx], it->second.at(mip_idx));
          }
        }
      }
    }
  }
  return true;
}

/*!
 * Cache the given textures and set in pool
 */
void TextureUploadHandler::populate_cache(
    const std::vector<std::shared_ptr<TextureRecord>>& textures,
    SharedRenderState* render_state) {
  for (auto& tex : textures) {
    // disable automatic GC of these textures. We need this - even if the texture becomes evicted
    // from PS2 VRAM, we want to hold on to the conversion.  Now this cache will be responsible for
    // managing this texture.
    tex->do_gc = false;
    m_stats.textures_provided++;
    m_stats.textures_converted++;
    // put in pool too
    render_state->texture_pool->set_texture(tex->dest, tex);
    auto it = m_tex_cache.find(tex->name);
    if (it != m_tex_cache.end()) {
      if (it->second.at(tex->mip_level)) {
        // replacing an existing, don't forget to kill the original.
        m_stats.textures_evicted++;
        render_state->texture_pool->discard(it->second.at(tex->mip_level));
      }
      it->second.at(tex->mip_level) = tex;
    } else {
      std::vector<std::shared_ptr<TextureRecord>> recs(7);  // max mip
      recs.at(tex->mip_level) = tex;
      m_tex_cache.insert({tex->name, std::move(recs)});
    }
  }
}

/*!
 * Unload any cached textures from GPU.
 * Remove all textures from this cache.
 * Set do_gc on all textures, as they may be in use in the pool and we may need them.
 *
 * Effectively, this will require all textures to re-converted and re-uploaded next time they are
 * uploaded from the game.
 */
void TextureUploadHandler::evict_all() {
  for (auto& e : m_tex_cache) {
    for (auto& x : e.second) {
      if (x) {
        if (x->on_gpu) {
          x->unload_from_gpu();
        }
        x->do_gc = true;
      }
    }
  }
  m_tex_cache = {};
}

void TextureUploadHandler::serialize(Serializer& ser) {
  if (ser.is_saving()) {
    ser.save<size_t>(m_tex_cache.size());
    for (auto& entry : m_tex_cache) {
      ser.save_str(&entry.first);
      ser.save<size_t>(entry.second.size());
      for (auto& x : entry.second) {
        if (x) {
          ser.save<u8>(1);
          x->serialize(ser);
        } else {
          ser.save<u8>(0);
        }
      }
    }
  } else {
    evict_all();
    auto size = ser.load<size_t>();
    for (size_t i = 0; i < size; i++) {
      auto str = ser.load_string();
      std::vector<std::shared_ptr<TextureRecord>> recs(ser.load<size_t>());
      for (auto& x : recs) {
        if (ser.load<u8>()) {
          x = std::make_shared<TextureRecord>();
          x->serialize(ser);
          x->on_gpu = false;
        }
      }
      m_tex_cache.insert({str, std::move(recs)});
    }
  }
}
