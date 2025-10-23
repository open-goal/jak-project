#version 410 core

layout (location = 0) in vec2 position_in;

out vec2 tex_coord;

void main() {
  gl_Position = vec4(position_in, 0, 1.0);
  tex_coord = (position_in + vec2(1.0, 1.0)) * 0.5;
}
