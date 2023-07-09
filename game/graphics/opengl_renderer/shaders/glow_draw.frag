#version 410 core

out vec4 color;

in vec4 fragment_color;
in float discard_flag;
in vec2 uv_texture;

uniform sampler2D tex_T0;
uniform float glow_boost;


void main() {
  vec4 texture_color = texture(tex_T0, uv_texture);
  color.xyz = texture_color.xyz * fragment_color.xyz * 2.f * discard_flag / 128.f * glow_boost;
  color.w = fragment_color.w * texture_color.w;
}
