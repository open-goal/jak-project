#version 410 core

layout (location = 0) in vec2 pos;
layout (location = 1) in vec2 tex_coord;

out vec2 uv;

void main() {
  gl_Position = vec4(pos.x * 2 - 1, pos.y * 2 - 1, 0, 1);
  uv = tex_coord;
}
