#pragma once

#include "game/graphics/gfx.h"
#include "game/graphics/opengl_renderer/sprite/sprite_common.h"

class GlowRenderer {
 public:
  GlowRenderer();
  bool at_max_capacity();
  SpriteGlowOutput* alloc_sprite();
  void cancel_sprite();

  void flush(SharedRenderState* render_state, ScopedProfilerNode& prof);
  void draw_debug_window();

  bool new_mode = true;

  // Vertex can hold all possible values for all passes. The total number of vertices is very small
  // so it ends up a lot faster to do a single upload, even if the size is like 50% larger than it
  // could be.
  struct Vertex {
    float x, y, z, w;
    float r, g, b, a;
    float u, v;
    float uu, vv;
  };

 private:
  struct {
    bool show_probes = false;
    bool show_probe_copies = false;
    bool enable_glow_boost = false;
    int num_sprites = 0;
    float glow_boost = 1.f;
  } m_debug;
  void add_sprite_pass_1(const SpriteGlowOutput& data);
  void add_sprite_pass_2(const SpriteGlowOutput& data, int sprite_idx);
  void add_sprite_pass_3(const SpriteGlowOutput& data, int sprite_idx);

  void add_sprite_new(const SpriteGlowOutput& data, int sprite_idx);

  void probe_and_copy_old(SharedRenderState* render_state, ScopedProfilerNode& prof);
  void probe_and_copy_new(SharedRenderState* render_state, ScopedProfilerNode& prof);

  void blit_depth(SharedRenderState* render_state);

  void setup_buffers_for_draws();

  void draw_probes(SharedRenderState* render_state,
                   ScopedProfilerNode& prof,
                   u32 idx_start,
                   u32 idx_end);

  void debug_draw_probes(SharedRenderState* render_state,
                         ScopedProfilerNode& prof,
                         u32 idx_start,
                         u32 idx_end);

  void draw_probe_copies(SharedRenderState* render_state,
                         ScopedProfilerNode& prof,
                         u32 idx_start,
                         u32 idx_end);

  void debug_draw_probe_copies(SharedRenderState* render_state,
                               ScopedProfilerNode& prof,
                               u32 idx_start,
                               u32 idx_end);
  void downsample_chain(SharedRenderState* render_state, ScopedProfilerNode& prof, u32 num_sprites);

  void draw_sprites(SharedRenderState* render_state, ScopedProfilerNode& prof);

  std::vector<Vertex> m_vertex_buffer;
  std::vector<SpriteGlowOutput> m_sprite_data_buffer;
  u32 m_next_sprite = 0;

  u32 m_next_vertex = 0;
  Vertex* alloc_vtx(int num);

  std::vector<u32> m_index_buffer;
  u32 m_next_index = 0;
  u32* alloc_index(int num);

  struct DsFbo {
    int size = -1;
    GLuint fbo;
    GLuint tex;
  };

  // max sprites should be 128 in simple sprite, plus 256 from aux = 384
  // 20 width = 20 * 20 = 400 sprites > 384.
  static constexpr int kDownsampleBatchWidth = 20;
  static constexpr int kMaxSprites = kDownsampleBatchWidth * kDownsampleBatchWidth;
  static constexpr int kMaxVertices = kMaxSprites * 32;  // check.
  static constexpr int kMaxIndices = kMaxSprites * 32;   // check.
  static constexpr int kDownsampleIterations = 5;
  static constexpr int kFirstDownsampleSize = 32;  // should be power of 2.

  struct {
    GLuint vertex_buffer;
    GLuint vao;
    GLuint index_buffer;

    GLuint probe_fbo;
    GLuint probe_fbo_rgba_tex;
    GLuint probe_fbo_depth_tex;
    GLuint first_ds_depth_rb;
    // GLuint probe_fbo_zbuf_rb;
    int probe_fbo_w = 640;
    int probe_fbo_h = 480;

    GLuint depth_texture;

    DsFbo downsample_fbos[kDownsampleIterations];
  } m_ogl;

  struct {
    GLuint vao;
    GLuint index_buffer;
    GLuint vertex_buffer;
  } m_ogl_downsampler;

  DrawMode m_default_draw_mode;

  struct SpriteRecord {
    u32 tbp;
    DrawMode draw_mode;
    u32 idx;
  };

  std::array<SpriteRecord, kMaxSprites> m_sprite_records;
};
