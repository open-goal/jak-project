#version 430 core

out vec4 out_color;

uniform sampler2D framebuffer_tex;

in flat vec4 fragment_color;
in vec2 tex_coord;

void main() {
  vec4 color = fragment_color;

  // correct color
  color *= 2;

  // correct texture coordinates
  vec2 texture_coords = vec2(tex_coord.x, (1.0f - tex_coord.y) - (1 - (SCISSOR_HEIGHT / 512.0)) / 2);

  // sample framebuffer texture
  out_color = color * texture(framebuffer_tex, texture_coords);
}
