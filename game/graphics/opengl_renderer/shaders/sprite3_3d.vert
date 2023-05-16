#version 430 core

layout (location = 0) in vec4 xyz_sx;
layout (location = 1) in vec4 quat_sy;
layout (location = 2) in vec4 rgba;
layout (location = 3) in uvec2 flags_matrix;
layout (location = 4) in uvec4 tex_info_in;

uniform vec4 hvdf_offset;
uniform mat4 camera;
uniform mat4 hud_matrix;
uniform vec4 hud_hvdf_offset;
uniform vec4 hud_hvdf_user[75];
uniform float pfog0;
uniform float fog_min;
uniform float fog_max;
uniform float min_scale;
uniform float max_scale;
uniform float deg_to_rad;
uniform float inv_area;
uniform vec4 basis_x;
uniform vec4 basis_y;
uniform vec4 xy_array[8];
uniform vec4 xyz_array[4];
uniform vec4 st_array[4];

out flat vec4 fragment_color;
out vec3 tex_coord;
out flat uvec2 tex_info;

vec4 matrix_transform(mat4 mtx, vec3 pt) {
  return mtx[3]
       + mtx[0] * pt.x
       + mtx[1] * pt.y
       + mtx[2] * pt.z;
}

mat3 sprite_quat_to_rot(vec3 quat) {
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

vec4 sprite_transform2(vec3 root, vec4 off, mat3 sprite_rot, float sx, float sy) {
  vec3 pos = root;

  vec3 offset = sprite_rot[0] * off.x * sx + sprite_rot[1] * off.y + sprite_rot[2] * off.z * sy;

  pos += offset;
  vec4 transformed_pos = -matrix_transform(camera, pos);
  float Q = pfog0 / transformed_pos.w;
  transformed_pos.xyz *= Q;
  transformed_pos.xyz += hvdf_offset.xyz;

  return transformed_pos;
}

void main() {

  // STEP 1: UNPACK DATA AND CREATE READABLE VARIABLES

  vec3 position = xyz_sx.xyz;
  float sx = xyz_sx.w;
  float sy = quat_sy.w;
  fragment_color = rgba;
  uint vert_id = tex_info_in.z;
  uint rendermode = tex_info_in.w; // 2D, HUD, 3D
  vec3 quat = quat_sy.xyz;
  uint matrix = flags_matrix.y;

  vec4 transformed;

  // STEP 2: perspective transform for distance
  vec4 transformed_pos_vf02 = matrix_transform(rendermode == 2 ? hud_matrix : camera, position);
  float Q = pfog0 / transformed_pos_vf02.w;


  // STEP 3: fade out sprite!
  vec4 scales_vf01 = xyz_sx;  // now used for something else.
  scales_vf01.z = sy;  // start building the scale vector
  scales_vf01.zw *= Q;  // sy sx
  scales_vf01.x = scales_vf01.z;  // = sy
  scales_vf01.x *= scales_vf01.w;  // x = sx * sy
  scales_vf01.x *= inv_area;  // x = sx * sy * inv_area (area ratio)
  fragment_color.w *= min(scales_vf01.x, 1.0);  // is this right? doesn't this stall??


  // STEP 4: actual vertex transformation
  if (rendermode == 3) { // 3D sprites

    mat3 rot = sprite_quat_to_rot(quat);
    transformed = sprite_transform2(position, xyz_array[vert_id], rot, sx, sy);

  } else if (rendermode == 1) { // 2D sprites

    transformed_pos_vf02.xyz *= Q;
    vec4 offset_pos_vf10 = transformed_pos_vf02 + hvdf_offset;
    offset_pos_vf10.w = max(offset_pos_vf10.w, fog_min);

    /* transformed_pos_vf02.w = offset_pos_vf10.w - fog_max;
    int fge = matrix == 0;
    if (transformed_pos_vf02.w != 0) {
      fge = false;
    } */

    scales_vf01.z = clamp(scales_vf01.z, min_scale, max_scale);
    scales_vf01.w = clamp(scales_vf01.w, min_scale, max_scale);

    quat.z *= deg_to_rad;
    float sp_sin = sin(quat.z);
    float sp_cos = cos(quat.z);

    vec4 xy0_vf19 = xy_array[vert_id + flags_matrix.x];
    vec4 vf12_rotated = (basis_x * sp_cos) - (basis_y * sp_sin);
    vec4 vf13_rotated_trans = (basis_x * sp_sin) + (basis_y * sp_cos);

    vf12_rotated *= scales_vf01.w;
    vf13_rotated_trans *= scales_vf01.z;

    transformed = offset_pos_vf10 + vf12_rotated * xy0_vf19.x + vf13_rotated_trans * xy0_vf19.y;

  } else if (rendermode == 2) { // hud sprites
  
    transformed_pos_vf02.xyz *= Q;
    vec4 offset_pos_vf10 = transformed_pos_vf02 + (matrix == 0 ? hud_hvdf_offset : hud_hvdf_user[matrix - 1]);

    // NOTE: no max scale for hud
    scales_vf01.z = max(scales_vf01.z, min_scale);
    scales_vf01.w = max(scales_vf01.w, min_scale);

    quat.z *= deg_to_rad;
    float sp_sin = sin(quat.z);
    float sp_cos = cos(quat.z);

    vec4 xy0_vf19 = xy_array[vert_id + flags_matrix.x];
    vec4 vf12_rotated = (basis_x * sp_cos) - (basis_y * sp_sin);
    vec4 vf13_rotated_trans = (basis_x * sp_sin) + (basis_y * sp_cos);

    vf12_rotated *= scales_vf01.w;
    vf13_rotated_trans *= scales_vf01.z;

    transformed = offset_pos_vf10 + vf12_rotated * xy0_vf19.x + vf13_rotated_trans * xy0_vf19.y;

  }

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
  // scissoring area adjust
  transformed.y *= SCISSOR_ADJUST * HEIGHT_SCALE;
  gl_Position = transformed;

  fragment_color *= 2;
  fragment_color.w *= 2;

  tex_info = tex_info_in.xy;
}
