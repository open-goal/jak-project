#pragma once

#include "common/dma/gs.h"
#include "common/math/Vector.h"

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"

using math::Matrix4f;
using math::Vector4f;

/*!
 * GOAL sprite-frame-data, all the data that's uploaded once per frame for the sprite system.
 */
struct SpriteFrameDataJak1 {
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
  AdGifData screen_shader;
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
  GifTag sprite_3d_giftag_2;
  AdGifData screen_shader;
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

  void from_jak1(const SpriteFrameDataJak1& data) {
    for (int i = 0; i < 8; i++) {
      xy_array[i] = data.xy_array[i];
    }
    for (int i = 0; i < 4; i++) {
      st_array[i] = data.st_array[i];
      xyz_array[i] = data.xyz_array[i];
    }
    hmge_scale = data.hmge_scale;
    pfog0 = data.pfog0;
    deg_to_rad = data.deg_to_rad;
    min_scale = data.min_scale;
    inv_area = data.inv_area;
    adgif_giftag = data.adgif_giftag;
    sprite_2d_giftag = data.sprite_2d_giftag;
    sprite_2d_giftag2 = data.sprite_2d_giftag2;
    for (int i = 0; i < 5; i++) {
      sincos[i] = data.sincos[i];
    }
    basis_x = data.basis_x;
    basis_y = data.basis_y;
    sprite_3d_giftag = data.sprite_3d_giftag;
    screen_shader = data.screen_shader;
    clipped_giftag = data.clipped_giftag;
    inv_hmge_scale = data.inv_hmge_scale;
    stq_offset = data.stq_offset;
    stq_scale = data.stq_scale;
    rgba_plain = data.rgba_plain;
    warp_giftag = data.warp_giftag;
    fog_min = data.fog_min;
    fog_max = data.fog_max;
    max_scale = data.max_scale;
    bonus = data.bonus;
  }
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
static_assert(sizeof(SpriteVecData2d) == 48);

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
  AdGifData user_adgif;  // the adgif shader 16
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
  Init = 0,                 // the sprite initialization program. runs once per frame.
  Sprites2dGrp0 = 3,        // world space 2d sprites
  Sprites2dHud_Jak1 = 109,  // hud sprites
  Sprites2dHud_Jak2 = 115,
  Sprites3d = 211  // 3d sprites
};

static_assert(offsetof(SpriteFrameData, hmge_scale) == 256);
static_assert(sizeof(SpriteFrameDataJak1) == 0x290, "SpriteFrameData size");
static_assert(sizeof(SpriteFrameData) == 0x2a0, "SpriteFrameData size");

/*!
 * Post-transformation description of a sprite glow - passed to the GlowRenderer.
 */
struct SpriteGlowOutput {
  math::Vector4f first_clear_pos[2];   // 8, 9
  math::Vector4f second_clear_pos[2];  // 11, 12 corners for the second clear draw
  math::Vector2f offscreen_uv[2];      // 24, 26
  math::Vector4f flare_xyzw[4];
  AdGifData adgif;                  // 68, 69, 70, 71, 72
  math::Vector4f flare_draw_color;  // 75
  float perspective_q;
};

struct SpriteGlowConsts {
  math::Vector4f camera[4];
  math::Vector4f perspective[4];
  math::Vector4f hvdf;
  math::Vector4f hmge;
  float pfog0;
  float deg_to_rad;
  float min_scale;
  float inv_area;
  math::Vector4f sincos[5];
  math::Vector4f basis_x;
  math::Vector4f basis_y;
  math::Vector4f xy_array[4];
  math::Vector4f clamp_min;
  math::Vector4f clamp_max;
};
static_assert(sizeof(SpriteGlowConsts) == 0x180);