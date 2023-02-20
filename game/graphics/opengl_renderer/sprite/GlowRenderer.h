#pragma once

#include "game/graphics/opengl_renderer/sprite/sprite_common.h"

class GlowRenderer {
 public:
  GlowRenderer();
  SpriteGlowOutput* alloc_sprite();
  void cancel_sprite();

  void flush(SharedRenderState* render_state, ScopedProfilerNode& prof);
  void draw_debug_window();

  // Vertex can hold all possible values for all passes. The total number of vertices is very small
  // so it ends up a lot faster to do a single upload, even if the size is like 50% larger than it
  // could be.
  struct Vertex {
    float x, y, z, w;
    float r, g, b, a;
    float u, v;
    u32 pad[2];
  };

 private:
  struct {
    bool show_probes = false;
    int num_sprites = 0;
  } m_debug;
  void add_sprite_pass_1(const SpriteGlowOutput& data);
  void add_sprite_pass_2(const SpriteGlowOutput& data);

  void blit_depth(SharedRenderState* render_state);


  //  struct SpriteRecord {
  //    int draw1_probe_alpha_clear_idx = -1;
  //  };

  static constexpr int kMaxSprites = 128 * 4;
  static constexpr int kMaxVertices = kMaxSprites * 32;  // check.
  static constexpr int kMaxIndices = kMaxSprites * 32;   // check.

  std::vector<Vertex> m_vertex_buffer;
  std::vector<SpriteGlowOutput> m_sprite_data_buffer;
  u32 m_next_sprite = 0;
  //  std::vector<SpriteRecord> m_sprite_records;

  u32 m_next_vertex = 0;
  Vertex* alloc_vtx(int num);

  std::vector<u32> m_index_buffer;
  u32 m_next_index = 0;
  u32* alloc_index(int num);

  struct {
    GLuint vertex_buffer;
    GLuint vao;
    GLuint index_buffer;

    GLuint probe_fbo;
    GLuint probe_fbo_rgba_tex;
    GLuint probe_fbo_zbuf_rb;
    int probe_fbo_w = 640;
    int probe_fbo_h = 480;

    GLuint downsample_fbo;
    GLuint downsample_fbo_tex;
  } m_ogl;
};
