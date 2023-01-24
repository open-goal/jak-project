#pragma once
#include "game/graphics/opengl_renderer/BucketRenderer.h"

class Merc2 : public BucketRenderer {
 public:
  Merc2(const std::string& name, int my_id);
  void draw_debug_window() override;
  void init_shaders(ShaderLibrary& shaders) override;
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;

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

  struct VuLights {
    math::Vector3f direction0;
    u32 w0;
    math::Vector3f direction1;
    u32 w1;
    math::Vector3f direction2;
    u32 w2;
    math::Vector4f color0;
    math::Vector4f color1;
    math::Vector4f color2;
    math::Vector4f ambient;
  };

  void init_pc_model(const DmaTransfer& setup, SharedRenderState* render_state);
  u32 alloc_lights(const VuLights& lights);

  u32 alloc_bones(int count);

  std::optional<MercRef> m_current_model = std::nullopt;
  u16 m_current_effect_enable_bits = 0;
  u16 m_current_ignore_alpha_bits = 0;
  static constexpr int kMaxEffect = 16;
  u8 m_fade_buffer[4 * kMaxEffect];

  struct MercMat {
    math::Vector4f tmat[4];
    math::Vector4f nmat[3];
  };

  struct ShaderMercMat {
    math::Vector4f tmat[4];
    math::Vector4f nmat[3];
    math::Vector4f pad;
    std::string to_string() const;
  };

  static constexpr int MAX_SKEL_BONES = 128;
  static constexpr int BONE_VECTORS_PER_BONE = 7;
  static constexpr int MAX_SHADER_BONE_VECTORS = 1024 * 32;  // ??

  static constexpr int MAX_LEVELS = 3;
  static constexpr int MAX_DRAWS_PER_LEVEL = 1024;
  static constexpr int MAX_ENVMAP_DRAWS_PER_LEVEL = 1024;

  math::Vector4f m_shader_bone_vector_buffer[MAX_SHADER_BONE_VECTORS];
  ShaderMercMat m_skel_matrix_buffer[MAX_SKEL_BONES];

  struct Uniforms {
    GLuint light_direction[3];
    GLuint light_color[3];
    GLuint light_ambient;

    GLuint hvdf_offset;
    GLuint fog;

    GLuint tbone;
    GLuint nbone;

    GLuint fog_color;
    GLuint perspective_matrix;

    GLuint ignore_alpha;
    GLuint decal;

    GLuint gfx_hack_no_tex;

    GLuint fade;
  };

  Uniforms m_merc_uniforms, m_emerc_uniforms;

  void init_shader_common(Shader& shader, Uniforms* uniforms, bool include_lights);
  void init_for_frame(SharedRenderState* render_state, ShaderId shader);
  void handle_setup_dma(DmaFollower& dma, SharedRenderState* render_state);
  void handle_all_dma(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void flush_pending_model(SharedRenderState* render_state, ScopedProfilerNode& prof);
  void handle_merc_chain(DmaFollower& dma,
                         SharedRenderState* render_state,
                         ScopedProfilerNode& prof);

  void switch_to_merc2(SharedRenderState* render_state);
  void switch_to_emerc(SharedRenderState* render_state);

  GLuint m_vao;

  GLuint m_bones_buffer;

  struct Stats {
    int num_models = 0;
    int num_missing_models = 0;
    int num_chains = 0;
    int num_effects = 0;
    int num_predicted_draws = 0;
    int num_predicted_tris = 0;
    int num_bones_uploaded = 0;
    int num_lights = 0;
    int num_draw_flush = 0;

    int num_envmap_effects = 0;
    int num_envmap_tris = 0;
  } m_stats;

  struct Draw {
    u32 first_index;
    u32 index_count;
    DrawMode mode;
    u32 texture;
    u32 num_triangles;
    u16 first_bone;
    u16 light_idx;
    u8 ignore_alpha;
    u8 fade[4];
  };

  struct LevelDrawBucket {
    const LevelData* level = nullptr;
    std::vector<Draw> draws;
    std::vector<Draw> envmap_draws;
    u32 next_free_draw = 0;
    u32 next_free_envmap_draw = 0;

    void reset() {
      level = nullptr;
      next_free_draw = 0;
      next_free_envmap_draw = 0;
    }
  };

  void do_draws(const Draw* draw_array,
                const LevelData* lev,
                u32 num_draws,
                const Uniforms& uniforms,
                ScopedProfilerNode& prof,
                bool set_fade,
                SharedRenderState* render_state);

  static constexpr int MAX_LIGHTS = 1024;
  VuLights m_lights_buffer[MAX_LIGHTS];
  u32 m_next_free_light = 0;
  VuLights m_current_lights;

  std::vector<LevelDrawBucket> m_level_draw_buckets;
  u32 m_next_free_level_bucket = 0;
  u32 m_next_free_bone_vector = 0;
  size_t m_opengl_buffer_alignment = 0;

  void flush_draw_buckets(SharedRenderState* render_state, ScopedProfilerNode& prof);
};
