#pragma once

#include "common/dma/gs.h"
#include "common/math/Vector.h"

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"
#include "game/graphics/opengl_renderer/sprite/sprite_common.h"

class SpriteRenderer : public BucketRenderer {
 public:
  SpriteRenderer(const std::string& name, int my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;
  static constexpr int SPRITES_PER_CHUNK = 48;

 private:
  void render_distorter(DmaFollower& dma,
                        SharedRenderState* render_state,
                        ScopedProfilerNode& prof);
  void handle_sprite_frame_setup(DmaFollower& dma);
  void render_3d(DmaFollower& dma);
  void render_2d_group0(DmaFollower& dma,
                        SharedRenderState* render_state,
                        ScopedProfilerNode& prof);
  void render_fake_shadow(DmaFollower& dma);
  void render_2d_group1(DmaFollower& dma,
                        SharedRenderState* render_state,
                        ScopedProfilerNode& prof);
  enum SpriteMode { Mode2D = 1, ModeHUD = 2, Mode3D = 3 };
  void do_block_common(SpriteMode mode,
                       u32 count,
                       SharedRenderState* render_state,
                       ScopedProfilerNode& prof);

  void handle_tex0(u64 val, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void handle_tex1(u64 val, SharedRenderState* render_state, ScopedProfilerNode& prof);
  // void handle_mip(u64 val, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void handle_zbuf(u64 val, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void handle_clamp(u64 val, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void handle_alpha(u64 val, SharedRenderState* render_state, ScopedProfilerNode& prof);

  void update_gl_prim(SharedRenderState* render_state);
  void update_gl_texture(SharedRenderState* render_state, int unit);
  void flush_sprites(SharedRenderState* render_state, ScopedProfilerNode& prof);

  u8 m_sprite_distorter_setup[7 * 16];  // direct data
  u8 m_sprite_direct_setup[3 * 16];
  SpriteFrameData m_frame_data;  // qwa: 980
  Sprite3DMatrixData m_3d_matrix_data;
  SpriteHudMatrixData m_hud_matrix_data;

  SpriteVecData2d m_vec_data_2d[SPRITES_PER_CHUNK];
  AdGifData m_adgif[SPRITES_PER_CHUNK];

  struct DebugStats {
    int blocks_2d_grp0 = 0;
    int count_2d_grp0 = 0;
    int blocks_2d_grp1 = 0;
    int count_2d_grp1 = 0;
  } m_debug_stats;

  bool m_enable_culling = true;

  bool m_2d_enable = true;
  bool m_3d_enable = true;

  struct SpriteVertex3D {
    math::Vector4f xyz_sx;              // position + x scale
    math::Vector4f quat_sy;             // quaternion + y scale
    math::Vector4f rgba;                // color
    math::Vector<u16, 2> flags_matrix;  // flags + matrix... split
    math::Vector<u16, 4> info;
    math::Vector<u8, 4> pad;
  };
  static_assert(sizeof(SpriteVertex3D) == 64);

  std::vector<SpriteVertex3D> m_vertices_3d;

  struct {
    GLuint vertex_buffer;
    GLuint vao;
  } m_ogl;

  int m_sprite_offset = 0;

  // state set through the prim register that requires changing GL stuff.
  struct PrimGlState {
    void from_register(GsPrim reg) {
      current_register = reg;
      gouraud_enable = reg.gouraud();
      texture_enable = reg.tme();
      fogging_enable = reg.fge();
      aa_enable = reg.aa1();
      use_uv = reg.fst();
      ctxt = reg.ctxt();
      fix = reg.fix();
      alpha_blend_enable = reg.abe();
    }

    GsPrim current_register;
    bool gouraud_enable = false;
    bool texture_enable = false;
    bool fogging_enable = false;
    bool alpha_blend_enable = false;

    bool aa_enable = false;
    bool use_uv = false;  // todo: might not require a gl state change
    bool ctxt = false;    // do they ever use ctxt2?
    bool fix = false;     // what does this even do?
  } m_prim_gl_state;

  static constexpr int ADGIF_STATE_COUNT = 10;

  struct AdGifState {
    GsTex0 reg_tex0;
    u32 texture_base_ptr = 0;
    bool using_mt4hh = false;
    bool tcc = false;

    bool enable_tex_filt = false;

    u64 reg_clamp = 0b101;
    bool clamp_s = true;
    bool clamp_t = true;

    GsAlpha reg_alpha;
    GsAlpha::BlendMode a = GsAlpha::BlendMode::SOURCE;
    GsAlpha::BlendMode b = GsAlpha::BlendMode::DEST;
    GsAlpha::BlendMode c = GsAlpha::BlendMode::SOURCE;
    GsAlpha::BlendMode d = GsAlpha::BlendMode::DEST;
    u8 fix = 0;
    void from_register(GsAlpha reg) {
      reg_alpha = reg;
      a = reg.a_mode();
      b = reg.b_mode();
      c = reg.c_mode();
      d = reg.d_mode();
      fix = reg.fix();

      ASSERT(fix == 0);
    }
    bool z_write = false;

    bool used = false;

    bool nontexture_equal(const AdGifState& other) const {
      return reg_alpha == other.reg_alpha && z_write == other.z_write;
    }

    bool operator==(const AdGifState& other) const {
      return reg_tex0 == other.reg_tex0 && enable_tex_filt == other.enable_tex_filt &&
             reg_clamp == other.reg_clamp && nontexture_equal(other);
    }
    bool operator!=(const AdGifState& other) const { return !operator==(other); }
  } m_adgif_state_stack[ADGIF_STATE_COUNT];

  AdGifState m_adgif_state;  // temp state

  int m_adgif_index = 0;

  void update_gl_blend(AdGifState& state);
};
