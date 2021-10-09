#pragma once

#include <string>
#include <memory>
#include "game/graphics/dma/dma_chain_read.h"
#include "game/graphics/opengl_renderer/Shader.h"
#include "game/graphics/texture/TexturePool.h"
#include "game/graphics/opengl_renderer/Profiler.h"

/*!
 * Matches the bucket-id enum in GOAL
 */
enum class BucketId {
  BUCKET0 = 0,
  BUCKET1 = 1,
  SKY_DRAW = 3,
  TFRAG_TEX_LEVEL0 = 5,
  SHRUB_TEX_LEVEL0 = 19,
  ALPHA_TEX_LEVEL0 = 31,
  SKY_LEVEL0 = 32,
  PRIS_TEX_LEVEL0 = 48,
  WATER_TEX_LEVEL0 = 57,
  // ...
  PRE_SPRITE_TEX = 65,  // maybe it's just common textures?
  SPRITE = 66,
  DEBUG_DRAW_0 = 67,
  DEBUG_DRAW_1 = 68,
  MAX_BUCKETS = 69
};

/*!
 * The main renderer will contain a single SharedRenderState that's passed to all bucket renderers.
 * This allows bucket renders to share textures and shaders.
 */
struct SharedRenderState {
  explicit SharedRenderState(std::shared_ptr<TexturePool> _texture_pool)
      : texture_pool(_texture_pool) {}
  ShaderLibrary shaders;
  std::shared_ptr<TexturePool> texture_pool;
  u32 buckets_base = 0;  // address of buckets array.
  u32 next_bucket = 0;   // address of next bucket that we haven't started rendering in buckets
  u32 default_regs_buffer = 0;  // address of the default regs chain.

  void* ee_main_memory = nullptr;
  u32 offset_of_s7;
  bool dump_playback = false;
};

/*!
 * Interface for bucket renders. Each bucket will have its own BucketRenderer.
 */
class BucketRenderer {
 public:
  BucketRenderer(const std::string& name, BucketId my_id) : m_name(name), m_my_id(my_id) {}
  virtual void render(DmaFollower& dma,
                      SharedRenderState* render_state,
                      ScopedProfilerNode& prof) = 0;
  std::string name_and_id() const;
  virtual ~BucketRenderer() = default;
  bool& enabled() { return m_enabled; }
  virtual bool empty() const { return false; }
  virtual void draw_debug_window() = 0;
  virtual void serialize(Serializer&) {}

 protected:
  std::string m_name;
  BucketId m_my_id;
  bool m_enabled = true;
};

/*!
 * Renderer that makes sure the bucket is empty and ignores it.
 */
class EmptyBucketRenderer : public BucketRenderer {
 public:
  EmptyBucketRenderer(const std::string& name, BucketId my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  bool empty() const override { return true; }
  void draw_debug_window() override {}
};