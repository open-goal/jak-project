#version 430 core

out vec4 color;

in vec4 fragment_color;
in float discard_flag;
in vec2 uv_texture;

layout (binding = 0) uniform sampler2D tex;


void main() {
  vec4 texture_color = texture(tex, uv_texture);
  color.xyz = texture_color.xyz * fragment_color.xyz * 2.f * discard_flag / 128.f;
  color.w = fragment_color.w * texture_color.w;
}
