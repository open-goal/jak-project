#pragma once
#include "game/graphics/opengl_renderer/BucketRenderer.h"

class Merc2 : public BucketRenderer {
 public:
  Merc2(const std::string& name, int my_id);
  ~Merc2();
  void draw_debug_window() override;
  void init_shaders(ShaderLibrary& shaders) override;
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;

 private:
  bool m_debug_mode = false;
  struct DrawDebug {
    DrawMode mode;
    int num_tris;
  };
  struct EffectDebug {
    bool envmap = false;
    DrawMode envmap_mode;
    std::vector<DrawDebug> draws;
  };
  struct ModelDebug {
    std::string name;
    std::string level;
    std::vector<EffectDebug> effects;
  };
  struct {
    std::vector<ModelDebug> model_list;
  } m_debug;
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

  void handle_pc_model(const DmaTransfer& setup,
                       SharedRenderState* render_state,
                       ScopedProfilerNode& prof);
  u32 alloc_lights(const VuLights& lights);

  struct ModBuffers {
    GLuint vao, vertex;
  };

  static constexpr int kMaxEffect = 32;
  bool m_effect_debug_mask[kMaxEffect];

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
  u32 alloc_bones(int count, ShaderMercMat* data);
  static constexpr int MAX_SKEL_BONES = 128;
  static constexpr int BONE_VECTORS_PER_BONE = 7;
  static constexpr int MAX_SHADER_BONE_VECTORS = 1024 * 32;  // ??

  static constexpr int MAX_LEVELS = 3;
  static constexpr int MAX_DRAWS_PER_LEVEL = 1024;
  static constexpr int MAX_ENVMAP_DRAWS_PER_LEVEL = 1024;

  math::Vector4f m_shader_bone_vector_buffer[MAX_SHADER_BONE_VECTORS];

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
  void handle_setup_dma(DmaFollower& dma, SharedRenderState* render_state);
  void handle_all_dma(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void handle_merc_chain(DmaFollower& dma,
                         SharedRenderState* render_state,
                         ScopedProfilerNode& prof);

  void switch_to_merc2(SharedRenderState* render_state);
  void switch_to_emerc(SharedRenderState* render_state);

  GLuint m_vao;

  void setup_merc_vao();

  std::vector<ModBuffers> m_mod_vtx_buffers;
  u32 m_next_mod_vtx_buffer = 0;

  static constexpr int MAX_MOD_VTX = UINT16_MAX;
  std::vector<tfrag3::MercVertex> m_mod_vtx_temp;

  struct UnpackTempVtx {
    float pos[4];
    float nrm[4];
  };
  std::vector<UnpackTempVtx> m_mod_vtx_unpack_temp;

  ModBuffers alloc_mod_vtx_buffer(const LevelData* lev);

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

    int num_upload_bytes = 0;
    int num_uploads = 0;
  } m_stats;

  enum DrawFlags {
    IGNORE_ALPHA = 1,
    MOD_VTX = 2,
  };

  struct Draw {
    u32 first_index;
    u32 index_count;
    DrawMode mode;
    u32 texture;
    u32 num_triangles;
    u16 first_bone;
    u16 light_idx;
    u8 flags;
    ModBuffers mod_vtx_buffer;
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
  Draw* alloc_normal_draw(const tfrag3::MercDraw& mdraw,
                          bool ignore_alpha,
                          LevelDrawBucket* lev_bucket,
                          u32 first_bone,
                          u32 lights);
  Draw* try_alloc_envmap_draw(const tfrag3::MercDraw& mdraw,
                              const DrawMode& envmap_mode,
                              u32 envmap_texture,
                              LevelDrawBucket* lev_bucket,
                              const u8* fade,
                              u32 first_bone,
                              u32 lights);

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

  std::vector<LevelDrawBucket> m_level_draw_buckets;
  u32 m_next_free_level_bucket = 0;
  u32 m_next_free_bone_vector = 0;
  size_t m_opengl_buffer_alignment = 0;

  void flush_draw_buckets(SharedRenderState* render_state, ScopedProfilerNode& prof);
};
