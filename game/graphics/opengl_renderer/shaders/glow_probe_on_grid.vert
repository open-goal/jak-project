#version 410 core

layout (location = 0) in vec4 position_in;
// layout (location = 1) in vec4 rgba_in;

out vec4 fragment_color;

void main() {
  gl_Position = vec4((position_in.xy * 2) - 1.f, (position_in.z / 8388608.f) - 1.f, 1.f);
  fragment_color = vec4(0, 0.5, 1, 1);
}