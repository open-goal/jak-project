#version 410 core

layout (location = 0) in float position_in;
layout (location = 1) in int time_of_day_index;
layout (location = 2) in ivec2 uv;
layout (location = 3) in int vi;

uniform vec4 hvdf_offset;
uniform mat4 camera;
uniform float fog_constant;
uniform float fog_min;
uniform float fog_max;
uniform sampler1D tex_T10; // note, sampled in the vertex shader on purpose.
// uniform int decal;
uniform float fog_hack_threshold;

out vec4 fragment_color;
out vec2 tex_coord;
out float fogginess;

void main() {
  int vx = vi % 512;
  int vz = vi / 512;

  tex_coord.x = (uv.x == 1) ? 1.f : 0.f;
  tex_coord.y = (uv.y == 1) ? 1.f : 0.f;

  vec4 transformed = -camera[3];
  transformed -= camera[0] * 32768.f * vx;
  transformed -= camera[1] * position_in;
  transformed -= camera[2] * 32768.f * vz;

  float Q = fog_constant / transformed.w;

   fogginess = 255 - clamp(-transformed.w + hvdf_offset.w, fog_min, fog_max);

  transformed.xyz *= Q;
  // offset
  transformed.xyz += hvdf_offset.xyz;
  // correct xy offset
  transformed.xy -= (2048.);
  // correct z scale
  transformed.z /= (8388608);
  transformed.z -= 1;
  // correct xy scale
  transformed.x /= (256);
  transformed.y /= -(128);
  transformed.xyz *= transformed.w;
  // scissoring area adjust
  transformed.y *= SCISSOR_ADJUST * HEIGHT_SCALE;
  gl_Position = transformed;

  // time of day lookup
  fragment_color = texelFetch(tex_T10, time_of_day_index, 0);
  fragment_color.a = 1.0;
}
