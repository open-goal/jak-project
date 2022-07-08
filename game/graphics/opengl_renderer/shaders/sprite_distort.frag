#version 430 core

out vec4 out_color;

uniform sampler2D framebuffer_tex;

in flat vec4 fragment_color;
in vec2 tex_coord;

void main() {
  vec4 color = fragment_color;

  // game specifies the color as (0.5, 0.5, 0.5, 0.5), but it doesn't look right unless rgb is 1.0
  color.r = 1.0f;
  color.g = 1.0f;
  color.b = 1.0f;

  // HACK: adjust the texture coords, 0.06 here is arbitrary
  vec2 texture_coords = vec2(tex_coord.x, (1.0f - tex_coord.y) - 0.06f);

  // sample framebuffer texture
  out_color = color * texture(framebuffer_tex, texture_coords);
}
