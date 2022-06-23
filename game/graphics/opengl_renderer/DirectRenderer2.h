#pragma once

#include <vector>

#include "common/common_types.h"
#include "common/dma/gs.h"

#include "game/graphics/opengl_renderer/BucketRenderer.h"

class DirectRenderer2 {
 public:
  DirectRenderer2(u32 max_verts,
                  u32 max_inds,
                  u32 max_draws,
                  const std::string& name,
                  bool use_ftoi_mod);
  void init_shaders(ShaderLibrary& shaders);
  void reset_state();
  void render_gif_data(const u8* data, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void flush_pending(SharedRenderState* render_state, ScopedProfilerNode& prof);
  void draw_debug_window();
  ~DirectRenderer2();

 private:
  static constexpr u8 TEX_UNITS = 10;
  void reset_buffers();

  void draw_call_loop_simple(SharedRenderState* render_state, ScopedProfilerNode& prof);
  void draw_call_loop_grouped(SharedRenderState* render_state, ScopedProfilerNode& prof);

  // the GsState is the state of all Gs Registers.
  struct GsState {
    DrawMode as_mode;
    u16 tbp;
    GsTest gs_test;
    GsTex0 gs_tex0;
    GsPrim gs_prim;
    GsAlpha gs_alpha;
    u8 tex_unit = 0;

    float s, t, Q;
    math::Vector<u8, 4> rgba;
    bool next_vertex_starts_strip = true;
    u8 vertex_flags = 0;
    void set_tcc_flag(bool value) { vertex_flags ^= (-(u8)value ^ vertex_flags) & 1; }
    void set_decal_flag(bool value) { vertex_flags ^= (-(u8)value ^ vertex_flags) & 2; }
    void set_fog_flag(bool value) { vertex_flags ^= (-(u8)value ^ vertex_flags) & 4; }
  } m_state;

  // if this is true, then drawing a vertex can just get pushed directly to the vertex buffer.
  // if not, we need to set up a new draw
  bool m_current_state_has_open_draw = false;

  struct Draw {
    DrawMode mode;
    u32 start_index = -1;
    u16 tbp = UINT16_MAX;
    u8 fix = 0;
    u8 tex_unit = 0;

    std::string to_string() const;
    std::string to_single_line_string() const;
  };

  std::vector<Draw> m_draw_buffer;
  u32 m_next_free_draw = 0;

  struct Vertex {
    math::Vector<float, 3> xyz;
    math::Vector<u8, 4> rgba;
    math::Vector<float, 3> stq;
    u8 tex_unit;
    u8 flags;
    u8 fog;
    u8 pad;

    std::string print() const;
  };
  static_assert(sizeof(Vertex) == 32);

  struct VertexBuffer {
    std::vector<Vertex> vertices;
    std::vector<u32> indices;
    u32 next_vertex = 0;
    u32 next_index = 0;

    void push_reset() { indices[next_index++] = UINT32_MAX; }

    Vertex& push() {
      indices[next_index++] = next_vertex;
      return vertices[next_vertex++];
    }

    bool close_to_full() {
      return (next_vertex + 40 > vertices.size()) || (next_index + 40 > indices.size());
    }
  } m_vertices;

  struct {
    GLuint vertex_buffer;
    GLuint index_buffer;
    GLuint vao;
    GLuint alpha_reject, color_mult, fog_color;
  } m_ogl;

  struct Stats {
    u32 upload_bytes = 0;
    u32 num_uploads = 0;
    u32 flush_due_to_full = 0;
    float upload_wait = 0;
    u32 saved_draws = 0;
  } m_stats;

  struct Debug {
    bool disable_mip = true;
  } m_debug;

  std::string m_name;
  void setup_opengl_for_draw_mode(const Draw& draw, SharedRenderState* render_state);
  void setup_opengl_tex(u16 unit,
                        u16 tbp,
                        bool filter,
                        bool clamp_s,
                        bool clamp_t,
                        SharedRenderState* render_state);

  // gif handlers
  void handle_ad(const u8* data);

  void handle_test1(u64 val);
  void handle_tex0_1(u64 val);
  void handle_tex1_1(u64 val);
  void handle_clamp1(u64 val);
  void handle_prim(u64 val);
  void handle_alpha1(u64 val);
  void handle_zbuf1(u64 val);

  // packed
  void handle_st_packed(const u8* data);
  void handle_rgbaq_packed(const u8* data);

  void handle_xyzf2_packed(const u8* data,
                           SharedRenderState* render_state,
                           ScopedProfilerNode& prof);

  bool m_use_ftoi_mod = false;
  void handle_xyzf2_mod_packed(const u8* data,
                               SharedRenderState* render_state,
                               ScopedProfilerNode& prof);
};
