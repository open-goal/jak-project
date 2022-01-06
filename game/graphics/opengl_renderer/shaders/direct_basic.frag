#version 430 core

out vec4 color;

in vec4 fragment_color;

void main() {
  color = fragment_color;
}
