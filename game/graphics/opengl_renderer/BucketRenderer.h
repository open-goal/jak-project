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

class EyeRenderer;
/*!
 * The main renderer will contain a single SharedRenderState that's passed to all bucket renderers.
 * This allows bucket renders to share textures and shaders.
 */
struct SharedRenderState {
  explicit SharedRenderState(std::shared_ptr<TexturePool> _texture_pool,
                             std::shared_ptr<Loader> _loader,
                             GameVersion version)
      : shaders(version), texture_pool(_texture_pool), loader(_loader) {}
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
  math::Vector<u8, 4> fog_color = math::Vector<u8, 4>{0, 0, 0, 0};
  float fog_intensity = 1.f;
  bool no_multidraw = false;

  void reset();
  bool has_pc_data = false;
  LevelVis occlusion_vis[6];

  math::Vector4f camera_planes[4];
  math::Vector4f camera_matrix[4];
  math::Vector4f camera_hvdf_off;
  math::Vector4f camera_fog;
  math::Vector4f camera_pos;

  EyeRenderer* eye_renderer = nullptr;

  std::string load_status_debug;

  // Information for renderers that need to read framebuffers:
  // Most renderers can just use the framebuffer/glViewport set up by OpenGLRenderer, but special
  // effects like sprite distort that read the framebuffer will need to know the details of the
  // framebuffer setup.

  // the framebuffer that bucket renderers should render to.
  int render_fb_x = 0;
  int render_fb_y = 0;
  int render_fb_w = 0;
  int render_fb_h = 0;
  GLuint render_fb = -1;

  // the region within that framebuffer to draw to.
  int draw_region_w = 0;
  int draw_region_h = 0;
  int draw_offset_x = 0;
  int draw_offset_y = 0;

  int bucket_for_vis_copy = 0;
  int num_vis_to_copy = 0;
  GameVersion version;
};

/*!
 * Interface for bucket renders. Each bucket will have its own BucketRenderer.
 */
class BucketRenderer {
 public:
  BucketRenderer(const std::string& name, int my_id) : m_name(name), m_my_id(my_id) {}
  virtual void render(DmaFollower& dma,
                      SharedRenderState* render_state,
                      ScopedProfilerNode& prof) = 0;
  std::string name_and_id() const;
  virtual ~BucketRenderer() = default;
  bool& enabled() { return m_enabled; }
  virtual bool empty() const { return false; }
  virtual void draw_debug_window() = 0;
  virtual void init_shaders(ShaderLibrary&) {}
  virtual void init_textures(TexturePool&, GameVersion) {}

 protected:
  std::string m_name;
  int m_my_id;
  bool m_enabled = true;
};

class RenderMux : public BucketRenderer {
 public:
  RenderMux(const std::string& name,
            int my_id,
            std::vector<std::unique_ptr<BucketRenderer>> renderers);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;
  void init_shaders(ShaderLibrary&) override;
  void init_textures(TexturePool&, GameVersion) override;
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
  EmptyBucketRenderer(const std::string& name, int my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  bool empty() const override { return true; }
  void draw_debug_window() override {}
};

class SkipRenderer : public BucketRenderer {
 public:
  SkipRenderer(const std::string& name, int my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  bool empty() const override { return true; }
  void draw_debug_window() override {}
};
