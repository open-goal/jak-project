#pragma once

#include <string>

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/opengl_utils.h"
#include "game/graphics/pipelines/opengl.h"

constexpr int EYE_BASE_BLOCK = 8160;
constexpr int NUM_EYE_PAIRS = 11;
constexpr int SINGLE_EYE_SIZE = 32;

class EyeRenderer : public BucketRenderer {
 public:
  EyeRenderer(const std::string& name, int id);
  ~EyeRenderer();
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;
  void init_textures(TexturePool& texture_pool, GameVersion) override;

  void handle_eye_dma2(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof);

  struct SpriteInfo {
    u8 a;
    u32 uv0[2];
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

  bool m_use_bilinear = true;
  bool m_alpha_hack = true;

  u32 m_temp_tex[SINGLE_EYE_SIZE * SINGLE_EYE_SIZE];

  bool m_use_gpu = true;

  struct CpuEyeTex {
    u64 gl_tex;
    GpuTexture* gpu_tex;
    u32 tbp;
  };
  CpuEyeTex m_cpu_eye_textures[NUM_EYE_PAIRS * 2];

  struct GpuEyeTex {
    GpuTexture* gpu_tex;
    u32 tbp;
    FramebufferTexturePair fb;

    // note: eye texture increased to 128x128 (originally 32x32) here.
    GpuEyeTex() : fb(128, 128, GL_UNSIGNED_INT_8_8_8_8_REV) {}
  } m_gpu_eye_textures[NUM_EYE_PAIRS * 2];

  // xyst per vertex, 4 vertices per square, 3 draws per eye, 11 pairs of eyes, 2 eyes per pair.
  static constexpr int VTX_BUFFER_FLOATS = 4 * 4 * 3 * NUM_EYE_PAIRS * 2;
  float m_gpu_vertex_buffer[VTX_BUFFER_FLOATS];
  GLuint m_vao;
  GLuint m_gl_vertex_buffer;

  struct SingleEyeDraws {
    int lr;
    int pair;

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
  void run_cpu(const std::vector<SingleEyeDraws>& draws, SharedRenderState* render_state);
  void run_gpu(const std::vector<SingleEyeDraws>& draws, SharedRenderState* render_state);
};
