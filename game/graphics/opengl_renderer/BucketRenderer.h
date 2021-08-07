#pragma once

#include <string>
#include "game/graphics/dma/dma_chain_read.h"
#include "game/graphics/opengl_renderer/Shader.h"

/*!
 * Matches the bucket-id enum in GOAL
 */
enum class BucketId {
  BUCKET0 = 0,
  BUCKET1 = 1,
  // ...
  DEBUG_DRAW_0 = 67,
  DEBUG_DRAW_1 = 68,
  MAX_BUCKETS = 69
};

/*!
 * The main renderer will contain a single SharedRenderState that's passed to all bucket renderers.
 * This allows bucket renders to share textures and shaders.
 */
struct SharedRenderState {
  ShaderLibrary shaders;
  u32 buckets_base = 0;  // address of buckets array.
  u32 next_bucket = 0;   // address of next bucket that we haven't started rendering in buckets
  u32 default_regs_buffer = 0;  // address of the default regs chain.
};

/*!
 * Interface for bucket renders. Each bucket will have its own BucketRenderer.
 */
class BucketRenderer {
 public:
  BucketRenderer(const std::string& name, BucketId my_id) : m_name(name), m_my_id(my_id) {}
  virtual void render(DmaFollower& dma, SharedRenderState* render_state) = 0;
  std::string name_and_id() const;
  virtual ~BucketRenderer() = default;

 protected:
  std::string m_name;
  BucketId m_my_id;
};

/*!
 * Renderer that makes sure the bucket is empty and ignores it.
 */
class EmptyBucketRenderer : public BucketRenderer {
 public:
  EmptyBucketRenderer(const std::string& name, BucketId my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state) override;
};