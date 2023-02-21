#version 430 core

layout (location = 0) in vec4 position_in;

out vec2 tex_coord;

void main() {
  gl_Position = vec4((position_in.xy * 2) - 1.f, 0.f, 1.f);
  tex_coord = position_in.xy;
}
