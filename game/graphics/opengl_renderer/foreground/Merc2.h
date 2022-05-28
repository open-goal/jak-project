#pragma once
#include "game/graphics/opengl_renderer/BucketRenderer.h"

class Merc2 : public BucketRenderer {
 public:
  Merc2(const std::string& name, BucketId my_id);
  void draw_debug_window() override;
  void init_shaders(ShaderLibrary& shaders) override;
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void handle_merc_chain(DmaFollower& dma,
                         SharedRenderState* render_state,
                         ScopedProfilerNode& prof);

 private:
  enum MercDataMemory {
    LOW_MEMORY = 0,
    BUFFER_BASE = 442,
    // this negative offset is what broke jak graphics in Dobiestation for a long time.
    BUFFER_OFFSET = -442
  };

  struct LowMemory {
    u8 tri_strip_tag[16];
    u8 ad_gif_tag[16];
    math::Vector4f hvdf_offset;
    math::Vector4f perspective[4];
    math::Vector4f fog;
  } m_low_memory;
  static_assert(sizeof(LowMemory) == 0x80);

  void init_for_frame(SharedRenderState* render_state);
  void init_pc_model(const DmaTransfer& setup, SharedRenderState* render_state);
  void handle_all_dma(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void handle_setup_dma(DmaFollower& dma,
                        SharedRenderState* render_state,
                        ScopedProfilerNode& prof);
  void handle_lights_dma(const DmaTransfer& dma);
  void handle_matrix_dma(const DmaTransfer& dma);
  void flush_pending_model(SharedRenderState* render_state, ScopedProfilerNode& prof);

  void upload_tbones(int count, float scale);

  std::optional<Loader::MercRef> m_current_model = std::nullopt;
  u16 m_current_effect_enable_bits = 0;
  u16 m_current_ignore_alpha_bits = 0;
  struct VuLights {
    math::Vector3f direction0;
    u32 w0;
    math::Vector3f direction1;
    u32 w1;
    math::Vector3f direction2;
    u32 w2;
    math::Vector3f color0;
    u32 w3;
    math::Vector3f color1;
    u32 w4;
    math::Vector3f color2;
    u32 w5;
    math::Vector3f ambient;
    u32 w6;
  };

  struct MercMat {
    math::Vector4f tmat[4];
    math::Vector4f nmat[3];
  };

  static constexpr int MAX_MERC_MATRICES = 128;
  MercMat m_matrix_buffer[MAX_MERC_MATRICES];

  struct {
    GLuint light_direction[3];
    GLuint light_color[3];
    GLuint light_ambient;

    GLuint hvdf_offset;
    GLuint perspective[4];
    GLuint fog;

    GLuint hmat[4];

    GLuint tbone;
    GLuint nbone;

    GLuint fog_color;
    GLuint perspective_matrix;

    GLuint ignore_alpha;
  } m_uniforms;

  GLuint m_vao;

  struct Stats {
    int num_models = 0;
    int num_chains = 0;
    int num_effects = 0;
    int num_predicted_draws = 0;
    int num_predicted_tris = 0;
  } m_stats;
};