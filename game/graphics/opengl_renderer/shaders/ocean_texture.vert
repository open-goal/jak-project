#version 410 core

layout (location = 0) in vec2 position_in;
layout (location = 1) in vec4 rgba_in;
layout (location = 2) in vec2 tex_coord_in;

out vec4 fragment_color;
out vec2 tex_coord;

void main() {
  // inputs are 0 - 2048.
  gl_Position = vec4((position_in.x - 1024) / 1024, (position_in.y - 1024) / 1024, 0.5, 1.0);
  fragment_color = vec4(rgba_in.rgb * 2, 1);
  tex_coord = tex_coord_in;
}
