#version 430 core

layout (location = 0) in vec2 position_in;

out vec2 screen_pos;

uniform vec2 game_res;

void main() {
  gl_Position = vec4(position_in, 0, 1.0);
  screen_pos = (position_in + 1) / 2 * game_res;
}
