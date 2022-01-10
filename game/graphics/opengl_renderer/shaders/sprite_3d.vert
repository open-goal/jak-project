#version 430 core

layout (location = 0) in vec4 xyz_sx;
layout (location = 1) in vec4 quat_sy;
layout (location = 2) in vec4 rgba;
layout (location = 3) in uvec2 flags_matrix;
layout (location = 4) in uint vert_id;

uniform vec4 hvdf_offset;
uniform mat4 camera;
uniform float pfog0;
uniform float fog_min;
uniform float fog_max;
uniform float max_scale;
uniform float bonus;
uniform float deg_to_rad;
uniform float inv_area;
uniform vec4 hmge_scale;
uniform vec4 xy_array[8];
uniform vec4 xyz_array[4];
uniform vec4 st_array[4];

out vec4 fragment_color;
out vec3 tex_coord;

// putting all texture info stuff here so it's easier to copy-paste
// layout (location = 3) in uvec2 tex_info_in;
// out flat uvec2 tex_info;

vec4 matrix_transform(mat4 mtx, vec4 pt) {
  return mtx[3]
      + mtx[0] * pt.x
      + mtx[1] * pt.y
      + mtx[2] * pt.z;
}

int clip_xyz_plus_minus(vec4 pt) {
  float pw = abs(pt.w);
  float mw = -pw;
  if (pt.x > pw || pt.x < mw) return 1;
  if (pt.y > pw || pt.y < mw) return 1;
  if (pt.z > pw || pt.z < mw) return 1;
  if (pt.w > pw || pt.w < mw) return 1;
  return 0;
}

mat3 sprite_quat_to_rot(float qi, float qj, float qk) {
  mat3 result;
  float qr = sqrt(abs(1.0 - (qi * qi + qj * qj + qk * qk)));
  result[0][0] = 1.0 - 2.0 * (qj * qj + qk * qk);
  result[1][0] = 2.0 * (qi * qj - qk * qr);
  result[2][0] = 2.0 * (qi * qk + qj * qr);
  result[0][1] = 2.0 * (qi * qj + qk * qr);
  result[1][1] = 1.0 - 2.0 * (qi * qi + qk * qk);
  result[2][1] = 2.0 * (qj * qk - qi * qr);
  result[0][2] = 2.0 * (qi * qk - qj * qr);
  result[1][2] = 2.0 * (qj * qk + qi * qr);
  result[2][2] = 1.0 - 2.0 * (qi * qi + qj * qj);
  return result;
}

vec4 sprite_transform2(vec4 root, vec4 off, mat4 cam, mat3 sprite_rot, float sx, float sy, vec4 hvdf_off, float pfog0, float fog_min, float fog_max) {
  vec4 pos = root;

  vec3 offset = sprite_rot[0] * off.x * sx + sprite_rot[1] * off.y + sprite_rot[2] * off.z * sy;

  pos.xyz += offset.xyz;
  vec4 transformed_pos = matrix_transform(cam, pos);
  float Q = pfog0 / transformed_pos.w;
  transformed_pos.xyz *= Q;
  vec4 offset_pos = transformed_pos + hvdf_off;
  offset_pos.w = max(offset_pos.w, fog_max);
  offset_pos.w = min(offset_pos.w, fog_min);

  return offset_pos;
}

void main() {

// STEP 1: UNPACK DATA AND CREATE READABLE VARIABLES

  vec4 position = vec4(xyz_sx.xyz, 1.0);
  float sx = xyz_sx.w;
  float sy = quat_sy.y;
  vec4 quat = vec4(quat_sy.xyz, 1.0);
  fragment_color = rgba;


// STEP 2: TRANSFORM TRANS

  //  lqi.xyzw vf01, vi02        |  nop
  // vec4 pos_vf01 = xyz_sx;
  //  lqi.xyzw vf05, vi02        |  nop
  // vec4 flags_vf05 = flag_rot_sy;
  //  lqi.xyzw vf11, vi02        |  nop
  // vec4 color_vf11 = rgba;

  // multiplications from the right column
  position = matrix_transform(camera, position);
  float Q = pfog0 / position.w;


// STEP 3: SET UP SCALES AND AREA
// This is used at the end for alpha mod?

  // vec4 scales_vf01 = pos_vf01;  // now used for something else.
  //  lq.xyzw vf12, 1020(vi00)   |  mulaw.xyzw ACC, vf28, vf00
  // vf12 is fog consts
  // vec4 fog_consts_vf12 = vec4(fog_min, fog_max, max_scale, bonus);
  //  ilw.y vi08, 1(vi02)        |  maddax.xyzw ACC, vf25, vf01
  // load offset selector for the next round.
  //  nop                        |  madday.xyzw ACC, vf26, vf01
  //  nop                        |  maddz.xyzw vf02, vf27, vf01

  //  move.w vf05, vf00          |  addw.z vf01, vf00, vf05
  // scales_vf01.z = sy
  // scales_vf01.z = rot_sy.y;  // start building the scale vector
  // rot_sy.y = 1.f;              // what are we building in flags right now??
  
  // scales_vf01.x = scales_vf01.z;  // = sy
  // scales_vf01.x *= scales_vf01.w;  // x = sx * sy
  // scales_vf01.x *= m_frame_data.inv_area;  // x = sx * sy * inv_area (area ratio)
  sx *= Q;
  sy *= Q;
  float final_area = min(sx * sy * inv_area, 1.0);


// STEP 4

  //  nop                        |  nop
  //  div Q, vf31.x, vf02.w      |  muly.z vf05, vf05, vf31
  // float Q = pfog0 / position.w;
  quat.z *= deg_to_rad; // ?
  //  nop                        |  mul.xyzw vf03, vf02, vf29
  // vec4 scaled_pos_vf03 = position * hmge_scale;
  //  nop                        |  nop
  //  nop                        |  nop
  //  nop                        |  mulz.z vf04, vf05, vf05 (ts)

  //  the load is for rotation stuff,
  //  lq.xyzw vf14, 1001(vi00)   |  clipw.xyz vf03, vf03      (used for fcand)
  //  iaddi vi06, vi00, 0x1      |  adda.xyzw ACC, vf11, vf11 (used for fmand)

  // upcoming fcand with 0x3f, that checks all of them.
  // int fcand_result = clip_xyz_plus_minus(position * hmge_scale);
  // bool fmand_result = color_vf11.w == 0;  // (really w+w, but I don't think it matters?)

  //  L8:
  //  xgkick double buffer setup
  //  ior vi05, vi15, vi00       |  mul.zw vf01, vf01, Q
  // scales_vf01.z *= Q;  // sy
  // scales_vf01.w *= Q;  // sx

  //  lq.xyzw vf14, 1002(vi00) ts|  mula.xyzw ACC, vf05, vf14 (ts)

  //  fmand vi01, vi06           |  mul.xyz vf02, vf02, Q
  position.xyz *= Q;

  //  ibne vi00, vi01, L10       |  addz.x vf01, vf00, vf01
  // scales_vf01.x = scales_vf01.z;  // = sy
  // if (fmand_result) {
  //   continue;  // reject!
  // }

  //  lqi.xyzw vf07, vi03        |  mulz.xyzw vf16, vf15, vf04 (ts)
  // vf07 is first use adgif

  //  lq.xyzw vf14, 1003(vi00)   |  madda.xyzw ACC, vf15, vf14 (ts both)

  //  lqi.xyzw vf08, vi03        |  add.xyzw vf10, vf02, vf30
  // vf08 is second user adgif
  vec4 offset = position + hvdf_offset;

  //  lqi.xyzw vf09, vi03        |  mulw.x vf01, vf01, vf01
  // vf09 is third user adgif
  // scales_vf01.x *= scales_vf01.w;  // x = sx * sy

  //  sqi.xyzw vf06, vi05        |  mulz.xyzw vf15, vf16, vf04 (ts)
  // FIRST ADGIF IS adgif_vf06
  // packet.adgif_giftag = adgif_vf06;

  // just do all 5 now.
  // packet.user_adgif = m_adgif[sprite_idx];

  offset.w = max(offset.w, fog_max);

  // scales_vf01.z = max(scales_vf01.z, m_frame_data.min_scale);
  // scales_vf01.w = max(scales_vf01.w, m_frame_data.min_scale);

  // scales_vf01.x *= m_frame_data.inv_area;  // x = sx * sy * inv_area (area ratio)

  offset.w = min(offset.w, fog_min);

  // scales_vf01.z = min(scales_vf01.z, max_scale);
  // scales_vf01.w = min(scales_vf01.w, max_scale);
  // bool use_first_giftag = offset_selector == 0;

  // auto flag_vi07 = flags;
  
  // scales_vf01.x = min(scales_vf01.x, 1.f);

  position.w = offset.w - fog_max;

  fragment_color.a *= final_area;  // is this right? doesn't this stall??

  //    ibne vi00, vi09, L6        |  nop
  // if (position.w != 0) {
    // use_first_giftag = false;
  // }

  // flag_vi07 = 0;  // todo hack
  // vec4* xy_array = m_frame_data.xyz_array + flag_vi07;
  // math::Vector<s32, 4> color_integer_vf11 = color_vf11.cast<s32>();
  fragment_color /= 255.0;

  // packet.color = color_integer_vf11;

  // if (fcand_result) {
    // continue;  // reject (could move earlier)
  // }

  vec4 transformed = sprite_transform2(position, xyz_array[vert_id], camera, sprite_quat_to_rot(quat.x, quat.y, quat.z), sx, sy, hvdf_offset, pfog0, fog_min, fog_max);

  // packet.sprite_giftag = use_first_giftag ? m_frame_data.sprite_2d_giftag : m_frame_data.sprite_2d_giftag2;

  tex_coord = st_array[vert_id].xyz;
  
  // correct xy offset
  transformed.xy -= (2048.);

  // correct z scale
  transformed.z /= (8388608);
  transformed.z -= 1;

  // correct xy scale
  transformed.x /= (256);
  transformed.y /= -(128);

  gl_Position = transformed;
  // scissoring area adjust
  // gl_Position.y *= 512.0/448.0;
  // fragment_color = vec4(rgba_in.x, rgba_in.y, rgba_in.z, rgba_in.w * 2.);
  // tex_coord = tex_coord_in;
  // tex_info = tex_info_in;
}
