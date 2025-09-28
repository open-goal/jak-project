#version 410 core

uniform sampler2D tex_T0;
out vec4 color;
in vec2 tex_coord;

void main() {
  color = texture(tex_T0, tex_coord);
}
