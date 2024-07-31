#pragma once

#include <string>

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/opengl_utils.h"
#include "game/graphics/pipelines/opengl.h"

constexpr int EYE_BASE_BLOCK_JAK1 = 8160;
constexpr int EYE_BASE_BLOCK_JAK2 = 3968;
constexpr int EYE_BASE_BLOCK_JAK3 = 504;
constexpr int NUM_EYE_PAIRS = 20;
constexpr int SINGLE_EYE_SIZE = 32;

class EyeRenderer : public BucketRenderer {
 public:
  EyeRenderer(const std::string& name, int id);
  ~EyeRenderer();
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;
  void init_textures(TexturePool& texture_pool, GameVersion) override;

  void handle_eye_dma2(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof);
  std::optional<u64> lookup_eye_texture(u8 eye_id);
  std::optional<u64> lookup_eye_texture_hash(u64 hash, bool lr);

  struct SpriteInfo {
    u8 a;
    u64 uv0;  // stores hashed name of merc-ctrl that reads this eye.
    u32 uv1[2];
    u32 xyz0[3];
    u32 xyz1[3];

    std::string print() const;
  };

  struct ScissorInfo {
    int x0, x1;
    int y0, y1;
    std::string print() const;
  };

  struct EyeDraw {
    SpriteInfo sprite;
    ScissorInfo scissor;
    std::string print() const;
  };

 private:
  std::string m_debug;
  float m_average_time_ms = 0;

  struct GpuEyeTex {
    GpuTexture* gpu_tex = nullptr;
    u32 tbp;
    FramebufferTexturePair fb;
    u64 fnv_name_hash = 0;
    bool lr = false;

    // note: eye texture increased to 128x128 (originally 32x32) here.
    GpuEyeTex() : fb(128, 128, GL_UNSIGNED_INT_8_8_8_8_REV) {}
  } m_gpu_eye_textures[NUM_EYE_PAIRS * 2];

  // xyst per vertex, 4 vertices per square, 3 draws per eye, 11 pairs of eyes, 2 eyes per pair.
  static constexpr int VTX_BUFFER_FLOATS = 4 * 4 * 3 * NUM_EYE_PAIRS * 2;
  float m_gpu_vertex_buffer[VTX_BUFFER_FLOATS];
  GLuint m_vao;
  GLuint m_gl_vertex_buffer;

  struct SingleEyeDraws {
    u64 fnv_name_hash = 0;
    int lr;
    int pair;
    bool using_64 = false;

    int tex_slot() const { return pair * 2 + lr; }
    u32 clear_color;
    EyeDraw iris;
    GpuTexture* iris_tex = nullptr;
    u64 iris_gl_tex = 0;

    EyeDraw pupil;
    GpuTexture* pupil_tex = nullptr;
    u64 pupil_gl_tex = 0;

    EyeDraw lid;
    GpuTexture* lid_tex = nullptr;
    u64 lid_gl_tex = 0;
  };

  std::vector<SingleEyeDraws> get_draws(DmaFollower& dma, SharedRenderState* render_state);
  void run_gpu(const std::vector<SingleEyeDraws>& draws, SharedRenderState* render_state);
};
