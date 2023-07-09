#version 410 core

out vec4 color;

in vec4 fragment_color;
in vec3 tex_coord;
flat in uint use_uv;
in vec4 gs_scissor;
uniform float alpha_reject;
uniform float color_mult;
uniform float alpha_mult;
uniform float alpha_sub;
uniform float ta0;
uniform int scissor_enable;
// game width, game height, viewport width, viewport height
uniform vec4 game_sizes;

uniform vec4 fog_color;

flat in uvec4 tex_info;
in float fog;

uniform sampler2D tex_T20;

vec4 sample_tex(vec2 coord, uint unit) {
  return texture(tex_T20, coord);
}

vec4 sample_tex_px(vec2 coordf, uint unit) {
  ivec2 coord;
  coord.x = int(coordf.x / 16);
  coord.y = int(coordf.y / 16);
  return texelFetch(tex_T20, coord, 0);
}

void main() {
  if (scissor_enable == 1) {
    float x = gl_FragCoord.x;
    float y = gl_FragCoord.y;
    float w = (game_sizes.z / game_sizes.x);
    float h = (game_sizes.w / game_sizes.y);
    float scax0 = gs_scissor.x * w + 0.5;
    float scax1 = gs_scissor.y * w + 0.5;
    float scay0 = (game_sizes.y - gs_scissor.w) * h + 0.5;
    float scay1 = (game_sizes.y - gs_scissor.z) * h + 0.5;
    if (x < scax0 || x > scax1) discard;
    if (y < scay0 || y > scay1) discard;
  }

  vec4 T0;
  if (use_uv == 1) {
    T0 = sample_tex_px(tex_coord.xy, tex_info.x);
  } else {
    T0 = sample_tex(tex_coord.xy / tex_coord.z, tex_info.x);
  }
  // y is tcc
  // z is decal
  if (T0.w == 0) {
    T0.w = ta0;
  }

  if (tex_info.y == 0) {
    if (tex_info.z == 0) {
      // modulate + no tcc
      color.xyz = fragment_color.xyz * T0.xyz;
      color.w = fragment_color.w;
    } else {
      // decal + no tcc
      color.xyz = T0.xyz * 0.5;
      color.w = fragment_color.w;
    }
  } else {
    if (tex_info.z == 0) {
      // modulate + tcc
      color = fragment_color * T0;
    } else {
      // decal + tcc
      color.xyz = T0.xyz * 0.5;
      color.w = T0.w;
    }
  }
  color *= 2;
  color.xyz *= color_mult;
  color.w *= alpha_mult;
  if (color.a < alpha_reject) {
    discard;
  }
  if (tex_info.w == 1) {
    color.xyz = mix(color.xyz, fog_color.rgb, clamp(fog_color.a * fog, 0, 1));
  }

}
