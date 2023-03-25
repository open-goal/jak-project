#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"

class Generic2 : public BucketRenderer {
 public:
  Generic2(const std::string& name,
           int my_id,
           u32 num_verts = 200000,
           u32 num_frags = 2000,
           u32 num_adgif = 6000,
           u32 num_buckets = 800);
  ~Generic2();
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;

  enum class Mode {
    NORMAL,
    LIGHTNING,
  };

  void render_in_mode(DmaFollower& dma,
                      SharedRenderState* render_state,
                      ScopedProfilerNode& prof,
                      Mode mode);

  void draw_debug_window() override;
  void init_shaders(ShaderLibrary& shaders) override;

  struct Vertex {
    math::Vector<float, 3> xyz;
    math::Vector<u8, 4> rgba;
    math::Vector<float, 2> st;  // 16
    u8 tex_unit;
    u8 flags;
    u8 adc;
    u8 pad0;
    u32 pad1;
  };
  static_assert(sizeof(Vertex) == 32);

 private:
  void determine_draw_modes(bool enable_at);
  void build_index_buffer();
  void link_adgifs_back_to_frags();
  void draws_to_buckets();
  void reset_buffers();
  void process_matrices();
  void process_dma(DmaFollower& dma, u32 next_bucket);
  void process_dma_lightning(DmaFollower& dma, u32 next_bucket);
  void setup_draws(bool enable_at);
  void do_draws(SharedRenderState* render_state, ScopedProfilerNode& prof);
  void do_draws_for_alpha(SharedRenderState* render_state,
                          ScopedProfilerNode& prof,
                          DrawMode::AlphaBlend alpha,
                          bool hud);
  void do_hud_draws(SharedRenderState* render_state, ScopedProfilerNode& prof);
  bool check_for_end_of_generic_data(DmaFollower& dma, u32 next_bucket);
  void final_vertex_update();
  bool handle_bucket_setup_dma(DmaFollower& dma, u32 next_bucket);

  void opengl_setup();
  void opengl_cleanup();
  void opengl_bind_and_setup_proj(SharedRenderState* render_state);
  void setup_opengl_for_draw_mode(const DrawMode& draw_mode,
                                  u8 fix,
                                  SharedRenderState* render_state);

  void setup_opengl_tex(u16 unit,
                        u16 tbp,
                        bool filter,
                        bool clamp_s,
                        bool clamp_t,
                        SharedRenderState* render_state);

  struct {
    u32 stcycl;
  } m_dma_unpack;

  struct DrawingConfig {
    bool zmsk = false;
    // horizontal, vertical, depth, fog offsets.
    math::Vector4f hvdf_offset;
    float pfog0;             // scale factor for perspective divide
    float fog_min, fog_max;  // clamp for fog
    math::Vector3f proj_scale;
    float proj_mat_23, proj_mat_32;

    math::Vector3f hud_scale;
    float hud_mat_23, hud_mat_32, hud_mat_33;

    bool uses_hud = false;
  } m_drawing_config;

  struct GsState {
    DrawMode as_mode;
    u16 tbp;
    GsTest gs_test;
    GsTex0 gs_tex0;
    GsPrim gs_prim;
    GsAlpha gs_alpha;
    u8 tex_unit = 0;

    u8 vertex_flags = 0;
    void set_tcc_flag(bool value) { vertex_flags ^= (-(u8)value ^ vertex_flags) & 1; }
    void set_decal_flag(bool value) { vertex_flags ^= (-(u8)value ^ vertex_flags) & 2; }
    void set_fog_flag(bool value) { vertex_flags ^= (-(u8)value ^ vertex_flags) & 4; }

  } m_gs;

  static constexpr u32 FRAG_HEADER_SIZE = 16 * 7;
  struct Fragment {
    u8 header[FRAG_HEADER_SIZE];
    u32 adgif_idx = 0;
    u32 adgif_count = 0;

    u32 vtx_idx = 0;
    u32 vtx_count = 0;
    u8 mscal_addr = 0;
    bool uses_hud;
  };

  struct Adgif {
    AdGifData data;
    DrawMode mode;
    u32 tbp;
    u32 fix;
    u8 vtx_flags;
    u32 frag;
    u32 vtx_idx;
    u32 vtx_count;
    bool uses_hud;

    u32 next = -2;

    u64 key() const {
      u64 result = mode.as_int();
      result |= (((u64)tbp) << 32);
      result |= (((u64)fix) << 48);
      result |= (((u64)uses_hud ? 1ull : 0ull) << 62);
      return result;
    }
  };

  struct Bucket {
    DrawMode mode;
    u32 tbp;
    u32 start = UINT32_MAX;
    u32 last = UINT32_MAX;

    u32 idx_idx;
    u32 idx_count;

    u32 tri_count;  // just for debug
  };

  u32 handle_fragments_after_unpack_v4_32(const u8* data,
                                          u32 off,
                                          u32 first_unpack_bytes,
                                          u32 end_of_vif,
                                          Fragment* frag,
                                          bool loop);

  u32 m_next_free_frag = 0;
  std::vector<Fragment> m_fragments;
  u32 m_max_frags_seen = 0;

  u32 m_next_free_vert = 0;
  std::vector<Vertex> m_verts;
  u32 m_max_verts_seen = 0;

  u32 m_next_free_adgif = 0;
  std::vector<Adgif> m_adgifs;
  u32 m_max_adgifs_seen = 0;

  u32 m_next_free_bucket = 0;
  std::vector<Bucket> m_buckets;
  u32 m_max_buckets_seen = 0;

  u32 m_next_free_idx = 0;
  std::vector<u32> m_indices;
  u32 m_max_indices_seen = 0;

  Fragment& next_frag() {
    ASSERT(m_next_free_frag < m_fragments.size());
    return m_fragments[m_next_free_frag++];
  }

  Adgif& next_adgif() {
    ASSERT(m_next_free_adgif < m_adgifs.size());
    return m_adgifs[m_next_free_adgif++];
  }

  void alloc_vtx(int count) {
    m_next_free_vert += count;
    ASSERT(m_next_free_vert < m_verts.size());
  }

  std::string m_debug;

  struct Stats {
    u32 dma_tags = 0;
  } m_stats;

  static constexpr int ALPHA_MODE_COUNT = 7;
  bool m_alpha_draw_enable[ALPHA_MODE_COUNT] = {true, true, true, true, true, true, true};

  struct {
    GLuint vao;
    GLuint vertex_buffer;
    GLuint index_buffer;
    GLuint alpha_reject, color_mult, fog_color, scale, mat_23, mat_32, mat_33, fog_consts,
        hvdf_offset;
    GLuint gfx_hack_no_tex;
  } m_ogl;
};
