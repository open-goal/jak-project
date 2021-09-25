#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/texture/TexturePool.h"

/*!
 * The TextureUploadHandler receives textures uploads in the DMA chain and updates the TexturePool.
 * It will attempt to cache textures when possible as converting and uploading them to the GPU is
 * pretty expensive.
 *
 * Note that the PC Port sends a somewhat simplified texture upload message and this can't handle
 * any arbitrary PS2 texture transfer.  We rely on the texture metadata in GOAL to simplify this.
 */
class TextureUploadHandler : public BucketRenderer {
 public:
  TextureUploadHandler(const std::string& name, BucketId my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state) override;
  void draw_debug_window() override;
  void serialize(Serializer& ser) override;

 private:
  void evict_all();
  bool try_to_populate_from_cache(u64 page,
                                  const bool with_seg[3],
                                  SharedRenderState* render_state);
  void populate_cache(const std::vector<std::shared_ptr<TextureRecord>>& textures,
                      SharedRenderState* render_state);
  std::unordered_map<std::string, std::vector<std::shared_ptr<TextureRecord>>> m_tex_cache;

  struct {
    u32 textures_provided = 0;
    u32 textures_converted = 0;
    u32 textures_evicted = 0;

  } m_stats;
};
