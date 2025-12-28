#include "SlowTimeEffect.h"

/*
    (set-vector! (-> (the-as (inline-array vector4w) v1-0) 2) 96 96 96 192)
    (set-vector! (-> (the-as (inline-array vector4w) v1-0) 3) 4088 3320 0 0)
    (set-vector! (-> (the-as (inline-array vector4w) v1-0) 4) 5120 (* arg2 8) #xffffff #x10000)

    (set-vector! (-> (the-as (inline-array vector4w) v1-0) 5) 128 128 128 arg3)
    (set-vector! (-> (the-as (inline-array vector4w) v1-0) 6) 8 8 0 0)
    (set-vector! (-> (the-as (inline-array vector4w) v1-0) 7) 0 0 #xffffff 0)

    (set-vector! (-> (the-as (inline-array vector4w) v1-0) 8) 128 128 128 arg3)
    (set-vector! (-> (the-as (inline-array vector4w) v1-0) 9) 8184 8 0 0)
    (set-vector! (-> (the-as (inline-array vector4w) v1-0) 10) #x2800 0 #xffffff #x10000)

    (set-vector! (-> (the-as (inline-array vector4w) v1-0) 11) 128 128 128 arg3)
    (set-vector! (-> (the-as (inline-array vector4w) v1-0) 12) 8184 (+ (* (+ arg1 -1) 16) 8) 0 0)
    (set-vector! (-> (the-as (inline-array vector4w) v1-0) 13) #x2800 (* arg2 16) #xffffff #x10000)

    (set-vector! (-> (the-as (inline-array vector4w) v1-0) 14) 128 128 128 arg3)
    (set-vector! (-> (the-as (inline-array vector4w) v1-0) 15) 8 (+ (* (+ arg1 -1) 16) 8) 0 0)
    (set-vector! (-> (the-as (inline-array vector4w) v1-0) 16) 0 (* arg2 16) #xffffff #x10000)

    (set-vector! (-> (the-as (inline-array vector4w) v1-0) 17) 128 128 128 arg3)
    (set-vector! (-> (the-as (inline-array vector4w) v1-0) 18) 8 8 0 0)
    (set-vector! (-> (the-as (inline-array vector4w) v1-0) 19) 0 0 #xffffff 0)
 */

SlowTimeEffect::SlowTimeEffect() {
  glGenVertexArrays(1, &m_vao);
  glGenBuffers(1, &m_vertex_buffer);
  glBindVertexArray(m_vao);

  // mid, mid <- color is 96, 96, 96, 192
  // low, low <- color is 128, 128, 128, color
  // hi,  low
  // hi,  hi
  // lo,  hi
  // lo,  lo

  std::array<int32_t, 6> vertices = {0, 1, 2, 3, 4, 5};

  glBindBuffer(GL_ARRAY_BUFFER, m_vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(int32_t) * 6, vertices.data(), GL_STATIC_DRAW);

  glEnableVertexAttribArray(0);
  glVertexAttribIPointer(0, 1, GL_INT, sizeof(int32_t), nullptr);

  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
}

SlowTimeEffect::~SlowTimeEffect() {
  glDeleteVertexArrays(1, &m_vao);
  glDeleteBuffers(1, &m_vertex_buffer);
}

void SlowTimeEffect::draw(float amount, SharedRenderState* render_state, ScopedProfilerNode& prof) {
  glBindVertexArray(m_vao);
  glBindBuffer(GL_ARRAY_BUFFER, m_vertex_buffer);
  auto& shader = render_state->shaders[ShaderId::SLOW_TIME];
  shader.activate();
  glUniform1f(glGetUniformLocation(shader.id(), "amount"), amount);

  prof.add_tri(4);
  prof.add_draw_call();
  glDrawArrays(GL_TRIANGLE_FAN, 0, 6);
}
