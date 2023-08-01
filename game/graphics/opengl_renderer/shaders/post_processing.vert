#version 410 core

layout (location = 0) in vec2 position_in;

out vec2 screen_pos;

void main() {
  gl_Position = vec4(position_in, 0, 1.0);
  screen_pos = (position_in + 1) / 2;
}
