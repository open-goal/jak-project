#version 430 core

layout (location = 0) in vec4 position_in;
layout (location = 1) in vec4 rgba_in;
layout (location = 2) in vec2 uv;

out vec2 tex_coord;

void main() {
  gl_Position = vec4((position_in.xy * 2) - 1.f, 0.f, 1.f);
  tex_coord.x = uv.x / 512;
  tex_coord.y = 1.f - ((uv.y + 16) / 448);
}
