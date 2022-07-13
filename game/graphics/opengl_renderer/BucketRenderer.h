#pragma once

#include <memory>
#include <string>

#include "common/dma/dma_chain_read.h"

#include "game/graphics/opengl_renderer/Profiler.h"
#include "game/graphics/opengl_renderer/Shader.h"
#include "game/graphics/opengl_renderer/buckets.h"
#include "game/graphics/opengl_renderer/loader/Loader.h"
#include "game/graphics/texture/TexturePool.h"

struct LevelVis {
  bool valid = false;
  u8 data[2048];
};

struct FboState {
  GLuint fbo = -1;
  GLuint tex = -1;
  GLuint fbo2 = -1;
  GLuint tex2 = -1;
  GLuint zbuf = -1;
  GLenum render_targets[1] = {GL_COLOR_ATTACHMENT0};
  int width = 640;
  int height = 480;
  int msaa = 4;

  void delete_objects() {
    if (fbo != -1) {
      glDeleteFramebuffers(1, &fbo);
      fbo = -1;
    }
    if (tex != -1) {
      glDeleteTextures(1, &tex);
      tex = -1;
    }
    if (fbo2 != -1) {
      glDeleteFramebuffers(1, &fbo2);
      fbo2 = -1;
    }
    if (tex2 != -1) {
      glDeleteTextures(1, &tex2);
      tex2 = -1;
    }
    if (zbuf != -1) {
      glDeleteRenderbuffers(1, &zbuf);
      zbuf = -1;
    }
  }
};

class EyeRenderer;
/*!
 * The main renderer will contain a single SharedRenderState that's passed to all bucket renderers.
 * This allows bucket renders to share textures and shaders.
 */
struct SharedRenderState {
  explicit SharedRenderState(std::shared_ptr<TexturePool> _texture_pool,
                             std::shared_ptr<Loader> _loader)
      : texture_pool(_texture_pool), loader(_loader) {}
  ShaderLibrary shaders;
  std::shared_ptr<TexturePool> texture_pool;
  std::shared_ptr<Loader> loader;

  u32 buckets_base = 0;  // address of buckets array.
  u32 next_bucket = 0;   // address of next bucket that we haven't started rendering in buckets
  u32 default_regs_buffer = 0;  // address of the default regs chain.

  void* ee_main_memory = nullptr;
  u32 offset_of_s7;

  bool use_sky_cpu = true;
  bool use_occlusion_culling = true;
  bool enable_merc_xgkick = true;
  math::Vector<u8, 4> fog_color;
  float fog_intensity = 1.f;
  bool no_multidraw = false;

  void reset();
  bool has_pc_data = false;
  LevelVis occlusion_vis[2];

  math::Vector4f camera_planes[4];
  math::Vector4f camera_matrix[4];
  math::Vector4f camera_hvdf_off;
  math::Vector4f camera_fog;
  math::Vector4f camera_pos;

  EyeRenderer* eye_renderer = nullptr;

  std::string load_status_debug;

  int window_width_px;
  int window_height_px;
  int window_offset_x_px;
  int window_offset_y_px;

  FboState fbo_state;
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
  virtual void init_shaders(ShaderLibrary&) {}
  virtual void init_textures(TexturePool&) {}

 protected:
  std::string m_name;
  BucketId m_my_id;
  bool m_enabled = true;
};

class RenderMux : public BucketRenderer {
 public:
  RenderMux(const std::string& name,
            BucketId my_id,
            std::vector<std::unique_ptr<BucketRenderer>> renderers);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;
  void init_shaders(ShaderLibrary&) override;
  void init_textures(TexturePool&) override;
  void set_idx(u32 i) { m_render_idx = i; };

 private:
  std::vector<std::unique_ptr<BucketRenderer>> m_renderers;
  int m_render_idx = 0;
  std::vector<std::string> m_name_strs;
  std::vector<const char*> m_name_str_ptrs;
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

class SkipRenderer : public BucketRenderer {
 public:
  SkipRenderer(const std::string& name, BucketId my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  bool empty() const override { return true; }
  void draw_debug_window() override {}
};
