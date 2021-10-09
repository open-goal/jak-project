#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"
#include "game/graphics/dma/gs.h"
#include "common/math/Vector.h"

using math::Matrix4f;
using math::Vector4f;

/*!
 * GOAL sprite-frame-data, all the data that's uploaded once per frame for the sprite system.
 */
struct SpriteFrameData {
  Vector4f xy_array[8];
  Vector4f st_array[4];
  Vector4f xyz_array[4];
  Vector4f hmge_scale;
  float pfog0;
  float deg_to_rad;
  float min_scale;
  float inv_area;
  GifTag adgif_giftag;
  GifTag sprite_2d_giftag;
  GifTag sprite_2d_giftag2;
  Vector4f sincos[5];
  Vector4f basis_x;
  Vector4f basis_y;
  GifTag sprite_3d_giftag;
  AdGif screen_shader;
  GifTag clipped_giftag;
  Vector4f inv_hmge_scale;
  Vector4f stq_offset;
  Vector4f stq_scale;
  Vector4f rgba_plain;
  GifTag warp_giftag;
  float fog_min;
  float fog_max;
  float max_scale;
  float bonus;
};

/*!
 * "Matrix Data" for 3D sprites.  This is shared for all 3D sprites
 */
struct Sprite3DMatrixData {
  Matrix4f camera;
  Vector4f hvdf_offset;
};

/*!
 * "Matrix Data" for 2D screen space sprites. These are shared for all 2D HUD sprites
 */
struct SpriteHudMatrixData {
  Matrix4f matrix;

  // the "matrix" field is an index into these 76 quadwords
  Vector4f hvdf_offset;
  Vector4f user_hvdf[75];
};

/*!
 * The "vector data" (sprite-vec-data-2d).  Each sprite has its own vector data.
 */
struct SpriteVecData2d {
  Vector4f xyz_sx;       // position + x scale
  Vector4f flag_rot_sy;  // flags, rotation, and scale y
  Vector4f rgba;         // color

  float sx() const { return xyz_sx.w(); }

  // for HUD, this is the hvdf offset index
  s32 flag() {
    s32 result;
    memcpy(&result, &flag_rot_sy.x(), sizeof(s32));
    return result;
  }

  // unused for HUD
  s32 matrix() {
    s32 result;
    memcpy(&result, &flag_rot_sy.y(), sizeof(s32));
    return result;
  }

  // rotation in degrees
  float rot() const { return flag_rot_sy.z(); }

  // scale y.
  float sy() const { return flag_rot_sy.w(); }
};

/*!
 * The layout of VU1 data memory, in quadword addresses
 * The lower 800 qw's hold two buffers for double buffering drawing/loading.
 */
enum SpriteDataMem {
  // these three can have an offset of 0 or 400 depending on which buffer
  Header = 0,   // number of sprites (updated per chunk)
  Vector = 1,   // vector data (updated per chunk)
  Adgif = 145,  // adgifs (updated per chunk)

  // offset of first buffer
  Buffer0 = 0,
  // offset of second buffer
  Buffer1 = 400,

  GiftagBuilding = 800,  // used to store gs packets for xgkicking
  // matrix data (different depending on group)
  Matrix = 900,
  // frame data (same for the whole frame)
  FrameData = 980
};

/*!
 * The GS packet built by the sprite renderer.
 */
struct SpriteHud2DPacket {
  GifTag adgif_giftag;   // starts the adgif shader. 0
  AdGif user_adgif;      // the adgif shader 16
  GifTag sprite_giftag;  // 96
  math::Vector<s32, 4> color;
  Vector4f st0;
  math::Vector<s32, 4> xy0;
  Vector4f st1;
  math::Vector<s32, 4> xy1;
  Vector4f st2;
  math::Vector<s32, 4> xy2;
  Vector4f st3;
  math::Vector<s32, 4> xy3;
};

/*!
 * The layout of VU1 code memory
 */
enum SpriteProgMem {
  Init = 0,            // the sprite initialization program. runs once per frame.
  Sprites2dGrp0 = 3,   // world space 2d sprites
  Sprites2dHud = 109,  // hud sprites
  Sprites3d = 211      // 3d sprites
};

static_assert(offsetof(SpriteFrameData, hmge_scale) == 256);
static_assert(sizeof(SpriteFrameData) == 0x290, "SpriteFrameData size");

class SpriteRenderer : public BucketRenderer {
 public:
  SpriteRenderer(const std::string& name, BucketId my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state) override;
  void draw_debug_window() override;
  static constexpr int SPRITES_PER_CHUNK = 48;

 private:
  void render_distorter(DmaFollower& dma, SharedRenderState* render_state);
  void handle_sprite_frame_setup(DmaFollower& dma);
  void render_3d(DmaFollower& dma);
  void render_2d_group0(DmaFollower& dma);
  void render_fake_shadow(DmaFollower& dma);
  void render_2d_group1(DmaFollower& dma, SharedRenderState* render_state);
  void do_2d_group1_block_cpu(u32 count, SharedRenderState* render_state);

  u8 m_sprite_distorter_setup[7 * 16];  // direct data
  u8 m_sprite_direct_setup[3 * 16];
  SpriteFrameData m_frame_data;  // qwa: 980
  Sprite3DMatrixData m_3d_matrix_data;
  SpriteHudMatrixData m_hud_matrix_data;

  SpriteVecData2d m_vec_data_2d[SPRITES_PER_CHUNK];
  AdGif m_adgif[SPRITES_PER_CHUNK];

  struct DebugStats {
    int blocks_2d_grp1 = 0;
    int count_2d_grp1 = 0;
  } m_debug_stats;

  bool m_extra_debug = false;

  DirectRenderer m_sprite_renderer;
  DirectRenderer m_direct_renderer;
};
