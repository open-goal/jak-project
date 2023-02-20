#version 430 core

out vec4 out_color;
uniform sampler2D tex;
in flat vec4 fragment_color;
in vec2 tex_coord;

void main() {
  vec2 texture_coords = vec2(tex_coord.x, tex_coord.y);

  // sample framebuffer texture
  out_color = texture(tex, texture_coords);
}
