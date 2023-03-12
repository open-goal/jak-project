#version 430 core

layout (location = 0) in vec4 position_in;
layout (location = 1) in vec4 rgba_in;
layout (location = 2) in vec2 uv;

void main() {
  float x = (uv.x / 512) * 2 - 1;
  float y = 1.f - ((uv.y / SCISSOR_HEIGHT) * 2);
  gl_Position = vec4(x, y, 0, 1.f);
}
