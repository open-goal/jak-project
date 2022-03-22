#pragma once
#include "game/graphics/opengl_renderer/BucketRenderer.h"

class CommonOceanRenderer {
 public:
  CommonOceanRenderer();
  ~CommonOceanRenderer();

  void init_for_near();
  void kick_from_near(const u8* data);
  void flush_near(SharedRenderState* render_state, ScopedProfilerNode& prof);

  void init_for_mid();
  void kick_from_mid(const u8* data);
  void flush_mid(SharedRenderState* render_state, ScopedProfilerNode& prof);

 private:
  void handle_near_vertex_gif_data_fan(const u8* data, u32 offset, u32 loop);
  void handle_near_vertex_gif_data_strip(const u8* data, u32 offset, u32 loop);

  void handle_near_adgif(const u8* data, u32 offset, u32 count);

  void handle_mid_adgif(const u8* data, u32 offset);

  enum VertexBucket {
    RGB_TEXTURE = 0,
    ALPHA = 1,
    ENV_MAP = 2,
  };
  u32 m_current_bucket = VertexBucket::RGB_TEXTURE;

  struct Vertex {
    math::Vector<float, 3> xyz;
    math::Vector<u8, 4> rgba;
    math::Vector<float, 3> stq;
    u8 fog;
    u8 pad[3];
  };
  static_assert(sizeof(Vertex) == 32);

  static constexpr int NUM_BUCKETS = 3;

  std::vector<Vertex> m_vertices;
  u32 m_next_free_vertex = 0;

  std::vector<u32> m_indices[NUM_BUCKETS];
  u32 m_next_free_index[NUM_BUCKETS] = {0};

  u32 m_envmap_tex = 0;

  struct {
    GLuint vertex_buffer, index_buffer[NUM_BUCKETS], vao;
  } m_ogl;
};
