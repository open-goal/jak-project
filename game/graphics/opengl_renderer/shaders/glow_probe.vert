#version 410 core

layout (location = 0) in vec4 position_in;
layout (location = 1) in vec4 rgba_in;

out vec4 fragment_color;

void main() {
  vec4 transformed = position_in;
  transformed.xy -= (2048.);
  transformed.z /= (8388608);
  transformed.z -= 1;
  transformed.x /= (256);
  transformed.y /= -(128);
  transformed.xyz *= transformed.w;
  // scissoring area adjust
  transformed.y *= SCISSOR_ADJUST * HEIGHT_SCALE;
  gl_Position = transformed;

  fragment_color = rgba_in;
}
