#version 430 core

out vec4 color;
in vec2 tex_coord;
in vec4 fragment_color;

uniform sampler2D tex_T0;

void main() {
  vec4 tex = texture(tex_T0, tex_coord);
  color = fragment_color * tex;
}
