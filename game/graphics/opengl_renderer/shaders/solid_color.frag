#version 410 core

out vec4 color;

uniform vec4 fragment_color;

void main() {
  color = fragment_color;
}
