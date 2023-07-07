#version 410 core

out vec4 color;

in vec4 fragment_color;

void main() {
  if (fragment_color.a <= 0) discard;
  color = fragment_color;
}
