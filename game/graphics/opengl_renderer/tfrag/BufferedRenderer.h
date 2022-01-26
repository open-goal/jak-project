#pragma once
#include "common/math/Vector.h"
#include "common/common_types.h"
#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "common/dma/gs.h"
#include "game/graphics/pipelines/opengl.h"

namespace BufferedRenderer {

// The buffered renderer performs efficient sorting of primitives to reduce draw calls.

// the settings:

// this is the maximum number of draw "kinds" we can have per textures.
// a draw is a different "kind" if any opengl state changes happen (like z test, etc)
// if this is exceeded, this will return an error code.
// you must flush the group, then try adding it again.
// making this too large will slow down insertion and increase memory usage
constexpr int MAX_DRAW_KINDS_PER_TEX = 4;

// this is the maximum number of textures. If this is exceeded, there will be an error like above.
constexpr int MAX_TEXTURES = 256;

// this is the PS2 maximum TBP value.
constexpr int MAX_TBP = 16384;

// 32-byte vertex.
// the xyz, rgba, and stq are aligned. we have a free 4-bytes at the end.
// there is a single big pool of vertices.
struct Vertex {
  math::Vector<u32, 3> xyz;   // ps2 coords (0)
  math::Vector<u8, 4> rgba;   // 0, 4 (1)
  math::Vector<float, 2> st;  // (2)
  float q;                    // (3)
  u32 pad;
};
static_assert(sizeof(Vertex) == 32);

// a triangle grabs three vertices from the pools.
struct Triangle {
  u32 verts[3];
};

struct Draw {
  DrawMode mode;
  std::vector<Triangle> triangles;  // just indices
  void clear() {
    mode.set_invalid();
    triangles.clear();
  }
};

struct DrawGroup {
  Draw draws[MAX_DRAW_KINDS_PER_TEX];
  u16 tbp = UINT16_MAX;

  // can fail, in which case you should flush the DrawGroup.
  Draw* get_draw_for_mode(DrawMode mode) {
    for (auto& draw : draws) {
      if (draw.mode.is_invalid()) {
        draw.mode = mode;
        return &draw;
      } else if (draw.mode == mode) {
        return &draw;
      }
    }
    return nullptr;
  }

  void clear() {
    for (auto& draw : draws) {
      draw.clear();
      tbp = UINT16_MAX;
    }
  }
};

struct DrawList {
  DrawGroup groups[MAX_TEXTURES];
  u16 tbp_to_tex_id[MAX_TBP];
  u16 current_tex_id = 0;

  DrawList() {
    for (auto& x : tbp_to_tex_id) {
      x = UINT16_MAX;
    }
  }

  void clear() {
    for (auto& group : groups) {
      group.clear();
    }
    for (auto& x : tbp_to_tex_id) {
      x = UINT16_MAX;
    }
    current_tex_id = 0;
  }

  DrawGroup* get_group_for_tbp(u16 tbp) {
    if (tbp_to_tex_id[tbp] != UINT16_MAX) {
      // already have it
      return &groups[tbp_to_tex_id[tbp]];
    } else {
      if (current_tex_id == MAX_TEXTURES) {
        // don't have it, and out of room
        return nullptr;
      } else {
        // don't have it, but we can add it.
        auto group = &groups[current_tex_id];
        group->tbp = tbp;
        tbp_to_tex_id[tbp] = current_tex_id;
        current_tex_id++;
        return group;
      }
    }
  }
};

class Renderer {
 public:
  Renderer(BucketId my_id);
  ~Renderer();
  void render_list(const DrawList& list,
                   SharedRenderState* render_state,
                   ScopedProfilerNode& prof,
                   const std::vector<Vertex>& vertices);
  void render_group(const DrawGroup& group,
                    SharedRenderState* render_state,
                    ScopedProfilerNode& prof,
                    const std::vector<Vertex>& vertices);

  void setup_opengl_excluding_textures(SharedRenderState* render_state, DrawMode mode);

  void draw_debug_window();
  void clear_stats();
  BucketId my_id() const { return m_my_id; }

 private:
  static constexpr int MAX_VERTS = 400000;
  BucketId m_my_id;

  struct {
    int triangles = 0;
    int draw_calls = 0;
    int groups = 0;
  } m_stats;

  struct {
    GLuint vertex_buffer = -1;
    u32 vertex_buffer_size = 0;
    GLuint index_buffer = -1;
    u32 index_buffer_size = 0;
    GLuint vao = -1;
  } m_ogl;
};

class Builder {
 public:
  Builder(BucketId my_id);
  u32 add_gif_data(const void* data);
  void add_gif_data_sized(const void* data, u32 expected_size);
  void flush(SharedRenderState* render_state, ScopedProfilerNode& prof);
  void draw_debug_window();
  void reset_state();

 private:
  void handle_ad(const u8* data);
  void handle_test1(u64 val);
  void handle_tex0_1(u64 val);
  void handle_tex1_1(u64 val);
  void handle_clamp1(u64 val);
  void handle_prim(u64 val);
  void handle_alpha1(u64 val);

  void handle_st_packed(const u8* data);
  void handle_rgbaq_packed(const u8* data);
  void handle_xyzf2_packed(const u8* data);
  void handle_xyzf2_common(u32 x, u32 y, u32 z, u8 f, bool advance);
  bool handle_tri_strip_add(u32 new_vertex, bool advance);
  void add_prim_now(Triangle tri);

  u32 create_vertex_now(u32 x, u32 y, u32 z);
  BucketId my_id() const { return m_my_id; }

  BucketId m_my_id;

  DrawList m_list;
  Renderer m_renderer;
  DrawMode m_current_mode;
  u16 m_current_tbp = 0;

  GsPrim::Kind m_prim_kind = GsPrim::Kind::PRIM_7;

  std::vector<Vertex> m_vertices;

  float m_st_pending_q[3] = {0};  // q goes to real q on rgbaq packed
  float m_q = 0;
  math::Vector<u8, 4> m_rgba;

  // todo maybe add a mode cache?

  struct {
    u32 idx = 0;
    u32 startup = 0;
    u32 verts[3] = {0, 0, 0};
  } m_vertex_queue;

  struct {
    u32 m_dvert = 0;
    u32 m_tri = 0;
  } m_stats;

  struct {
    u32 last_tbp = UINT32_MAX;
    u32 last_mode = UINT32_MAX;
    Draw* draw = nullptr;
  } m_cache;
};
}  // namespace BufferedRenderer
