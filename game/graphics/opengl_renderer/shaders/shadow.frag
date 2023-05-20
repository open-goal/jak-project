#version 430 core

out vec4 color;
uniform vec4 color_uniform;

void main() {
  color = color_uniform * 2;
}
