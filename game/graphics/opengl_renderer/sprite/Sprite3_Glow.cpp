#include "game/graphics/opengl_renderer/sprite/Sprite3.h"

struct SpriteGlowData {
  float pos[3];
  float size_x;

  float size_probe;
  float z_offset;
  float rot_angle;
  float size_y;

  float color[4];

  float fade_a;
  float fade_b;
  u32 tex_id;
  u32 dummy;
};
static_assert(sizeof(SpriteGlowData) == 16 * 4);

/*!
 * Transformation math from the sprite-glow vu1 program.
 * Populates the SpriteGlowOutput struct with the same data that would get filled into the
 * output template on VU1. Excludes float to int conversions.
 *
 * Not a particularly efficient implementation, but I think the total number of glow sprites is
 * small, so not a big deal.
 */
bool glow_math(const SpriteGlowConsts* consts,
               const void* vec_data,
               const void* adgif_data,
               SpriteGlowOutput* out) {
  const auto* in = (const SpriteGlowData*)vec_data;
  static_assert(sizeof(out->adgif) == 5 * 16);
  memcpy(&out->adgif, adgif_data, 5 * 16);

  // the transformation here is a bit strange - there's two matrix multiplies.
  // one for camera, and one for perspective. Usually they do one, or when they really need both
  // for stuff like emerc, they optimize knowing which entires of perspective are always 0.
  // But not this time. My guess is that the VU program time is very small compared to actual
  // drawing, so they don't really care.

  // Transform point to camera frame.
  Vector4f p0 = consts->camera[3] + consts->camera[0] * in->pos[0] +
                consts->camera[1] * in->pos[1] + consts->camera[2] * in->pos[2];

  // Compute fade. Interestingly, the fade is computed based on depth, not distance from the camera.
  // I think this is kind of wrong, and it leads to some weird fadeout behavior.
  float fade = in->fade_a * p0.z() + in->fade_b;  // fade_a is negative
  if (fade < 0)
    fade = 0;
  if (fade > 1)
    fade = 1;

  // Adjust color based on fade.
  Vector4f rgba(in->color[0], in->color[1], in->color[2], in->color[3]);
  rgba.x() *= rgba.w() * fade / 128.f;
  rgba.y() *= rgba.w() * fade / 128.f;
  rgba.z() *= rgba.w() * fade / 128.f;
  out->flare_draw_color = rgba;

  // Apply an offset. This moves the point along a line between its original position, and the
  // camera (so this offset doesn't make the thing move up/down/left/right on screen, just "toward"
  // the camera).
  float pscale = 1.f - (in->z_offset / p0.z());
  p0.x() *= pscale;
  p0.y() *= pscale;
  p0.z() *= pscale;

  // Apply perspective transformation (no divide yet)
  p0 = consts->perspective[3] + consts->perspective[0] * p0.x() + consts->perspective[1] * p0.y() +
       consts->perspective[2] * p0.z();

  // HMGE's meaning is unknown, but it's scaling factors for clipping. Apply those, and reject if
  // the origin is off-screen.
  Vector4f pos_hmged = p0.elementwise_multiply(consts->hmge);
  float clip_plus = std::abs(pos_hmged.w());
  float clip_minus = -clip_plus;
  if (pos_hmged.x() > clip_plus || pos_hmged.x() < clip_minus)
    return false;
  if (pos_hmged.y() > clip_plus || pos_hmged.y() < clip_minus)
    return false;
  if (pos_hmged.z() > clip_plus || pos_hmged.z() < clip_minus)
    return false;

  // apply perspective divide. Interestingly using hmge's w here...
  float perspective_q = 1.f / pos_hmged.w();
  p0.x() *= perspective_q;
  p0.y() *= perspective_q;
  p0.z() *= perspective_q;
  out->perspective_q = perspective_q;

  // apply offset to final point. These offsets are applied after perspective divide, and are
  // required for the PS2 screen coordinates (centered at 2048, 2048).
  p0 += consts->hvdf;

  // from this point on, things are in screen coordinates. So our sizes (not screen coordinates)
  // should be scaled by q to become sizes in screen coordinates.
  Vector4f vf02(in->size_probe, in->z_offset, in->size_x, in->size_y);
  vf02 *= perspective_q;

  // clamp the probe size to be in (1, clamp_max.w)
  if (vf02.x() < 1)
    vf02.x() = 1;  // size_probe
  if (vf02.x() > consts->clamp_max.w())
    vf02.x() = consts->clamp_max.w();  // size probe

  // clamp the maximum size_x/size_y to clamp_max.z
  if (vf02.z() > consts->clamp_max.z())
    vf02.z() = consts->clamp_max.z();  // size x
  if (vf02.w() > consts->clamp_max.z())
    vf02.w() = consts->clamp_max.z();  // size y

  // compute the minimum safe position for the center of the probe, so corner ends up at min/max
  math::Vector2f vf09_min_probe_center(consts->clamp_min.x() + vf02.x(),
                                       consts->clamp_min.y() + vf02.x());
  math::Vector2f vf10_max_probe_center(consts->clamp_max.x() - vf02.x(),
                                       consts->clamp_max.y() - vf02.x());

  // clear corners. these don't have rotation applied, I guess (vf11, vf12)
  out->second_clear_pos[0] = Vector4f(p0.x() - vf02.x(), p0.y() - vf02.x(), p0.z(), p0.w());
  out->second_clear_pos[1] = Vector4f(p0.x() + vf02.x(), p0.y() + vf02.x(), p0.z(), p0.w());

  // compute offset from center of sprite to corner. This includes the rotation
  math::Vector2f basis_x(consts->basis_x[0], 0);  // x scaling factor
  math::Vector2f basis_y(0, consts->basis_y[1]);  // y scarling factor
  // rotate them
  float rot_rad = in->rot_angle * consts->deg_to_rad;
  float rot_sin = std::sin(rot_rad);
  float rot_cos = std::cos(rot_rad);
  math::Vector2f vf15_rotated_basis_x = basis_x * rot_sin - basis_y * rot_cos;
  math::Vector2f vf16_rotated_basis_y = basis_x * rot_cos + basis_y * rot_sin;
  vf15_rotated_basis_x *= vf02.z();  // scale x
  vf16_rotated_basis_y *= vf02.w();  // scale y

  // limit position so the clear doesn't go out of bounds
  // max.xy vf20, vf01, vf09 -> is this bugged? I think the x broadcast here is wrong
  // this breaks fadeout as the sprite moves off the top of the screen. I've fixed it here because
  // I'm pretty sure this is just a mistake.
  math::Vector2f vf20_pos(std::max(p0.x(), vf09_min_probe_center.x()),
                          std::max(p0.y(), vf09_min_probe_center.y()));
  vf20_pos.min_in_place(vf10_max_probe_center);

  // vf17 thing, vf18 thing
  math::Vector2f vf17(consts->clamp_min.x() - 1, consts->clamp_min.y() - 1);
  math::Vector2f vf18(consts->clamp_min.x() + 1, consts->clamp_min.y() + 1);
  vf17 = vf20_pos - vf17;
  vf17 -= vf02.x();
  vf18 = vf20_pos - vf18;
  vf18 += vf02.x();
  out->offscreen_uv[0] = vf17;
  out->offscreen_uv[1] = vf18;

  out->first_clear_pos[0] =
      Vector4f(vf20_pos.x() - vf02.x() - 1, vf20_pos.y() - vf02.x() - 1, 0xffffff, p0.w());
  out->first_clear_pos[1] =
      Vector4f(vf20_pos.x() + vf02.x() + 1, vf20_pos.y() + vf02.x() + 1, 0xffffff, p0.w());

  // mulaw.xyzw ACC, vf01, vf00
  // maddax.xyzw ACC, vf15, vf11
  // maddy.xyzw vf11, vf16, vf11
  for (int i = 0; i < 4; i++) {
    out->flare_xyzw[i] = p0;
    math::Vector2f off = (vf15_rotated_basis_x * consts->xy_array[i].x()) +
                         (vf16_rotated_basis_y * consts->xy_array[i].y());
    out->flare_xyzw[i].x() += off.x();
    out->flare_xyzw[i].y() += off.y();
  }
  return true;
}

/*!
 * Handle glow dma and draw glow sprites using GlowRenderer
 */
void Sprite3::glow_dma_and_draw(DmaFollower& dma,
                                SharedRenderState* render_state,
                                ScopedProfilerNode& prof) {
  auto maybe_consts_setup = dma.read_and_advance();
  if (maybe_consts_setup.size_bytes != sizeof(SpriteGlowConsts)) {
    fmt::print("no consts...\n");
    return;
  }
  SpriteGlowConsts consts;
  memcpy(&consts, maybe_consts_setup.data, sizeof(SpriteGlowConsts));

  auto templ_1 = dma.read_and_advance();
  ASSERT(templ_1.size_bytes == 16 * 0x54);

  auto templ_2 = dma.read_and_advance();
  ASSERT(templ_2.size_bytes == 16 * 0x54);

  auto bo = dma.read_and_advance();
  ASSERT(bo.size_bytes == 0);

  auto flushe = dma.read_and_advance();
  ASSERT(flushe.size_bytes == 0);

  auto control_xfer = dma.read_and_advance();
  while (control_xfer.size_bytes == 0 && control_xfer.vifcode0().kind == VifCode::Kind::NOP &&
         control_xfer.vifcode1().kind == VifCode::Kind::NOP) {
    control_xfer = dma.read_and_advance();
  }
  while (control_xfer.size_bytes == 16) {
    auto vecdata_xfer = dma.read_and_advance();
    auto shader_xfer = dma.read_and_advance();
    auto call = dma.read_and_advance();
    (void)call;

    u32 num_sprites;
    memcpy(&num_sprites, control_xfer.data, 4);
    ASSERT(num_sprites == 1);  // always, for whatever reason.

    ASSERT(vecdata_xfer.size_bytes == 4 * 16);
    ASSERT(shader_xfer.size_bytes == 5 * 16);

    auto* out = m_glow_renderer.alloc_sprite();
    if (!glow_math(&consts, vecdata_xfer.data, shader_xfer.data, out)) {
      m_glow_renderer.cancel_sprite();
    }
    control_xfer = dma.read_and_advance();
    while (control_xfer.size_bytes == 0 && control_xfer.vifcode0().kind == VifCode::Kind::NOP &&
           control_xfer.vifcode1().kind == VifCode::Kind::NOP) {
      control_xfer = dma.read_and_advance();
    }
  }

  m_glow_renderer.flush(render_state, prof);
}
