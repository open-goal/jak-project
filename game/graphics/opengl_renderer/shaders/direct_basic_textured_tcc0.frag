#version 330 core

out vec4 color;

in vec4 fragment_color;
in vec2 tex_coord;
uniform sampler2D tex_T0;

void main() {
  vec4 T0 = texture(tex_T0, tex_coord);
  T0.w = 1.0;
  color = fragment_color * T0 * 2.0;
}
