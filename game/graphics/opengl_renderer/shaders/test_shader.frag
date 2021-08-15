#version 330 core

out vec3 color;

in vec4 fragment_color;

void main() {
  color = fragment_color.xyz;
}
