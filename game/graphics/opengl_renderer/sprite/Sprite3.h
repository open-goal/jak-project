
#pragma once

#include <map>

#include "common/dma/gs.h"
#include "common/math/Vector.h"

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"
#include "game/graphics/opengl_renderer/background/background_common.h"
#include "game/graphics/opengl_renderer/sprite/GlowRenderer.h"
#include "game/graphics/opengl_renderer/sprite/sprite_common.h"

class Sprite3 : public BucketRenderer {
 public:
  Sprite3(const std::string& name, int my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;
  static constexpr int SPRITES_PER_CHUNK = 48;

 private:
  void render_jak1(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void render_jak2(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof);

  void opengl_setup();
  void opengl_setup_normal();
  void opengl_setup_distort();

  void render_distorter(DmaFollower& dma,
                        SharedRenderState* render_state,
                        ScopedProfilerNode& prof);
  void distort_dma(DmaFollower& dma, ScopedProfilerNode& prof);
  void distort_setup(ScopedProfilerNode& prof);
  void distort_setup_instanced(ScopedProfilerNode& prof);
  void distort_draw(SharedRenderState* render_state, ScopedProfilerNode& prof);
  void distort_draw_instanced(SharedRenderState* render_state, ScopedProfilerNode& prof);
  void distort_draw_common(SharedRenderState* render_state, ScopedProfilerNode& prof);
  void distort_setup_framebuffer_dims(SharedRenderState* render_state);
  void handle_sprite_frame_setup(DmaFollower& dma, GameVersion version);
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

  void update_mode_from_alpha1(u64 val, DrawMode& mode);
  void handle_tex0(u64 val, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void handle_tex1(u64 val, SharedRenderState* render_state, ScopedProfilerNode& prof);
  // void handle_mip(u64 val, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void handle_zbuf(u64 val, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void handle_clamp(u64 val, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void handle_alpha(u64 val, SharedRenderState* render_state, ScopedProfilerNode& prof);

  void flush_sprites(SharedRenderState* render_state, ScopedProfilerNode& prof, bool double_draw);

  GlowRenderer m_glow_renderer;
  void glow_dma_and_draw(DmaFollower& dma,
                         SharedRenderState* render_state,
                         ScopedProfilerNode& prof);

  struct SpriteDistorterSetup {
    GifTag gif_tag;
    GsZbuf zbuf;
    u64 zbuf_addr;
    GsTex0 tex0;
    u64 tex0_addr;
    GsTex1 tex1;
    u64 tex1_addr;
    u64 miptbp;
    u64 miptbp_addr;
    u64 clamp;
    u64 clamp_addr;
    GsAlpha alpha;
    u64 alpha_addr;
  };
  static_assert(sizeof(SpriteDistorterSetup) == (7 * 16));

  struct SpriteDistorterSineTables {
    Vector4f entry[128];
    math::Vector<u32, 4> ientry[9];
    GifTag gs_gif_tag;
    math::Vector<u32, 4> color;
  };
  static_assert(sizeof(SpriteDistorterSineTables) == (0x8b * 16));

  struct SpriteDistortFrameData {
    math::Vector3f xyz;  // position
    float num_255;       // always 255.0
    math::Vector2f st;   // texture coords
    float num_1;         // always 1.0
    u32 flag;            // 'resolution' of the sprite
    Vector4f rgba;       // ? (doesn't seem to be color)
  };
  static_assert(sizeof(SpriteDistortFrameData) == 16 * 3);

  struct SpriteDistortVertex {
    math::Vector3f xyz;
    math::Vector2f st;
  };

  struct SpriteDistortInstanceData {
    math::Vector4f x_y_z_s;     // position, S-texture coord
    math::Vector4f sx_sy_sz_t;  // scale, T-texture coord
  };

  struct {
    GLuint vao;
    GLuint vertex_buffer;
    GLuint index_buffer;
    GLuint fbo;
    GLuint fbo_texture;
    int fbo_width = 640;
    int fbo_height = 480;
  } m_distort_ogl;

  struct {
    GLuint vao;
    GLuint vertex_buffer;    // contains vertex data for each possible sprite resolution (3-11)
    GLuint instance_buffer;  // contains all instance specific data for each sprite per frame
    float last_aspect_x = -1.0;
    float last_aspect_y = -1.0;
    bool vertex_data_changed = false;
  } m_distort_instanced_ogl;

  struct {
    int total_sprites;
    int total_tris;
  } m_distort_stats;

  std::vector<SpriteDistortVertex> m_sprite_distorter_vertices;
  std::vector<u32> m_sprite_distorter_indices;
  SpriteDistorterSetup m_sprite_distorter_setup;  // direct data
  math::Vector4f m_sprite_distorter_sine_tables_aspect;
  SpriteDistorterSineTables m_sprite_distorter_sine_tables;
  std::vector<SpriteDistortFrameData> m_sprite_distorter_frame_data;
  std::vector<SpriteDistortVertex> m_sprite_distorter_vertices_instanced;
  std::map<int, std::vector<SpriteDistortInstanceData>> m_sprite_distorter_instances_by_res;

  u8 m_sprite_direct_setup[3 * 16];
  SpriteFrameData m_frame_data;  // qwa: 980
  Sprite3DMatrixData m_3d_matrix_data;
  SpriteHudMatrixData m_hud_matrix_data;
  DirectRenderer m_direct;

  SpriteVecData2d m_vec_data_2d[SPRITES_PER_CHUNK];
  AdGifData m_adgif[SPRITES_PER_CHUNK];

  struct DebugStats {
    int blocks_2d_grp0 = 0;
    int count_2d_grp0 = 0;
    int blocks_2d_grp1 = 0;
    int count_2d_grp1 = 0;
  } m_debug_stats;

  bool m_enable_distort_instancing = true;
  bool m_enable_culling = true;

  bool m_2d_enable = true;
  bool m_3d_enable = true;
  bool m_distort_enable = true;

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
    GLuint index_buffer;
  } m_ogl;

  DrawMode m_current_mode, m_default_mode;
  u32 m_current_tbp = 0;

  struct Bucket {
    std::vector<u32> ids;
    u32 offset_in_idx_buffer = 0;
    u64 key = -1;
  };

  std::map<u64, Bucket> m_sprite_buckets;
  std::vector<Bucket*> m_bucket_list;

  u64 m_last_bucket_key = UINT64_MAX;
  Bucket* m_last_bucket = nullptr;

  u64 m_sprite_idx = 0;

  std::vector<u32> m_index_buffer_data;
};
