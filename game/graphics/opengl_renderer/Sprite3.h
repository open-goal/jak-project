
#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"
#include "common/dma/gs.h"
#include "common/math/Vector.h"
#include "game/graphics/opengl_renderer/sprite_common.h"
#include "game/graphics/opengl_renderer/tfrag/tfrag_common.h"

#include <map>

class Sprite3 : public BucketRenderer {
 public:
  Sprite3(const std::string& name, BucketId my_id);
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

  void flush_sprites(SharedRenderState* render_state, ScopedProfilerNode& prof, bool double_draw);

  u8 m_sprite_distorter_setup[7 * 16];  // direct data
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