#version 430 core

layout (location = 0) in vec4 xyz_sx;
layout (location = 1) in vec4 quat_sy;
layout (location = 2) in vec4 rgba;
layout (location = 3) in uvec2 flags_matrix;
layout (location = 4) in uvec4 tex_info_in;

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
out flat uvec2 tex_info;

vec4 matrix_transform(mat4 mtx, vec4 pt) {
  return mtx[3]
      + mtx[0] * pt.x
      + mtx[1] * pt.y
      + mtx[2] * pt.z;
}

mat3 sprite_quat_to_rot(vec4 quat) {
  mat3 result;
  float qr = sqrt(abs(1.0 - (quat.x * quat.x + quat.y * quat.y + quat.z * quat.z)));
  result[0][0] = 1.0 - 2.0 * (quat.y * quat.y + quat.z * quat.z);
  result[1][0] = 2.0 * (quat.x * quat.y - quat.z * qr);
  result[2][0] = 2.0 * (quat.x * quat.z + quat.y * qr);
  result[0][1] = 2.0 * (quat.x * quat.y + quat.z * qr);
  result[1][1] = 1.0 - 2.0 * (quat.x * quat.x + quat.z * quat.z);
  result[2][1] = 2.0 * (quat.y * quat.z - quat.x * qr);
  result[0][2] = 2.0 * (quat.x * quat.z - quat.y * qr);
  result[1][2] = 2.0 * (quat.y * quat.z + quat.x * qr);
  result[2][2] = 1.0 - 2.0 * (quat.x * quat.x + quat.y * quat.y);
  return result;
}

vec4 sprite_transform2(vec4 root, vec4 off, mat3 sprite_rot, float sx, float sy) {
  vec4 pos = root;

  vec3 offset = sprite_rot[0] * off.x * sx + sprite_rot[1] * off.y + sprite_rot[2] * off.z * sy;

  pos.xyz += offset.xyz;
  vec4 transformed_pos = -matrix_transform(camera, pos);
  float Q = pfog0 / transformed_pos.w;
  transformed_pos.xyz *= Q;
  transformed_pos.xyz += hvdf_offset.xyz;
  // transformed_pos.w = max(offset_pos.w, fog_max);
  // transformed_pos.w = min(offset_pos.w, fog_min);

  return transformed_pos;
}

void main() {

// STEP 1: UNPACK DATA AND CREATE READABLE VARIABLES

  vec4 position = vec4(xyz_sx.xyz, 1.0);
  float sx = xyz_sx.w;
  float sy = quat_sy.w;
  vec4 quat = vec4(quat_sy.xyz, 1.0);
  fragment_color = rgba;
  uint vert_id = tex_info_in.z;


// STEP 2: perspective transform for distance

  vec4 transformed_pos_vf02 = matrix_transform(camera, xyz_sx);
  float Q = pfog0 / transformed_pos_vf02.w;


// STEP 3: fade out sprite!

  // quat.z *= deg_to_rad;
  vec4 scales_vf01 = xyz_sx;  // now used for something else.
  scales_vf01.z = sy;  // start building the scale vector
  scales_vf01.zw *= Q;  // sy sx
  scales_vf01.x = scales_vf01.z;  // = sy
  scales_vf01.x *= scales_vf01.w;  // x = sx * sy
  scales_vf01.x *= inv_area;  // x = sx * sy * inv_area (area ratio)
  scales_vf01.x = min(scales_vf01.x, 1.0);
  fragment_color.w *= scales_vf01.x;  // is this right? doesn't this stall??


// STEP 4: actual vertex transformation

  mat3 rot = sprite_quat_to_rot(quat);
  vec4 transformed = sprite_transform2(position, xyz_array[vert_id], rot, sx, sy);

  tex_coord = st_array[vert_id].xyz;


// STEP 5: final adjustments

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

  fragment_color.w *= 2;

  tex_info = tex_info_in.xy;
}
