#version 410 core

out vec4 color;
in vec4 vtx_color;

void main() {
  color = vtx_color;
}
