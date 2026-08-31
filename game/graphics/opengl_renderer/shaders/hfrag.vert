#version 410 core

layout (location = 0) in float position_in;
layout (location = 1) in int time_of_day_index;
layout (location = 2) in ivec2 uv;
layout (location = 3) in int vi;

uniform vec4 hvdf_offset;
uniform vec4 cam_trans;
uniform mat4 pc_camera;
uniform float fog_constant;
uniform float fog_min;
uniform float fog_max;
// uniform int decal;
uniform float fog_hack_threshold;
// GLES has no 1D textures/samplers at all -- the C++ side now creates this as a GL_TEXTURE_2D
// with height 1, so indexing needs an explicit y=0 via ivec2 below.
uniform sampler2D tex_T10;

out vec4 fragment_color;
out vec2 tex_coord;
out float fogginess;

void main() {
  int vx = vi % 512;
  int vz = vi / 512;

  tex_coord.x = (uv.x == 1) ? 1.f : 0.f;
  tex_coord.y = (uv.y == 1) ? 1.f : 0.f;

  vec3 vert = position_in - cam_trans.xyz;
  vec4 transformed = -pc_camera[3];
  transformed -= pc_camera[0] * (32768.f * float(vx) - cam_trans.x);
  transformed -= pc_camera[1] * (position_in - cam_trans.y);
  transformed -= pc_camera[2] * (32768.f * float(vz) - cam_trans.z);

  fogginess = 255. - clamp(-transformed.w + hvdf_offset.w, fog_min, fog_max);

  // scissoring area adjust
  transformed.y *= SCISSOR_ADJUST * HEIGHT_SCALE;
  gl_Position = transformed;

  // time of day lookup
  fragment_color = texelFetch(tex_T10, ivec2(time_of_day_index, 0), 0);
  fragment_color.a = 1.0;
}
