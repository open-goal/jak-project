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
uniform float min_scale;
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
  float sy = quat_sy.w;
  vec4 quat = vec4(quat_sy.xyz, 1.0);
  fragment_color = rgba;


// STEP 2

  vec4 transformed_pos_vf02 = matrix_transform(camera, xyz_sx);

  vec4 scales_vf01 = xyz_sx;  // now used for something else.
  vec4 fog_consts_vf12 = vec4(fog_min, fog_max, max_scale, bonus);

  scales_vf01.z = sy;  // start building the scale vector

  float Q = pfog0 / transformed_pos_vf02.w;
  // quat.z *= deg_to_rad;
  scales_vf01.z *= Q;  // sy
  scales_vf01.w *= Q;  // sx

  transformed_pos_vf02.x *= Q;
  transformed_pos_vf02.y *= Q;
  transformed_pos_vf02.z *= Q;

  scales_vf01.x = scales_vf01.z;  // = sy

  vec4 offset_pos_vf10 = transformed_pos_vf02 + hvdf_offset;

  scales_vf01.x *= scales_vf01.w;  // x = sx * sy

  offset_pos_vf10.w = max(offset_pos_vf10.w, fog_max);

  scales_vf01.z = max(scales_vf01.z, min_scale);
  scales_vf01.w = max(scales_vf01.w, min_scale);

  scales_vf01.x *= inv_area;  // x = sx * sy * inv_area (area ratio)

  offset_pos_vf10.w = min(offset_pos_vf10.w, fog_min);

  scales_vf01.z = min(scales_vf01.z, fog_consts_vf12.z);
  scales_vf01.w = min(scales_vf01.w, fog_consts_vf12.z);

  scales_vf01.x = min(scales_vf01.x, 1.0);

  transformed_pos_vf02.w = offset_pos_vf10.w - fog_consts_vf12.y;

  fragment_color.w *= scales_vf01.x;  // is this right? doesn't this stall??

  mat3 rot = sprite_quat_to_rot(quat.x, quat.y, quat.z);
  vec4 transformed = sprite_transform2(position, xyz_array[vert_id], camera, rot, sx, sy, hvdf_offset, pfog0, fog_min, fog_max);

  tex_coord = st_array[vert_id].xyz;
  
  // correct xy offset
  transformed.xy -= (2048.);

  // correct z scale
  transformed.z /= (8388608);
  transformed.z -= 1;

  // correct xy scale
  transformed.x /= (256);
  transformed.y /= -(128);

  // hack
  transformed.xyz *= transformed.w;

  gl_Position = transformed;
  // scissoring area adjust
  gl_Position.y *= 512.0/448.0;
  // fragment_color = vec4(rgba_in.x, rgba_in.y, rgba_in.z, rgba_in.w * 2.);
  // tex_coord = tex_coord_in;
  // tex_info = tex_info_in;
}
