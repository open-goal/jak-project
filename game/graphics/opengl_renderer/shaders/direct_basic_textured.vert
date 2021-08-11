#version 330 core

layout (location = 0) in vec3 position_in;
layout (location = 1) in vec4 rgba_in;
layout (location = 2) in vec2 tex_coord_in;

out vec4 fragment_color;
out vec2 tex_coord;

void main() {
  gl_Position = vec4((position_in.x - 0.5) * 16., -(position_in.y - 0.5) * 32, position_in.z, 1.0);
  fragment_color = vec4(rgba_in.x, rgba_in.y, rgba_in.z, rgba_in.w * 2.);
  tex_coord = tex_coord_in;
}