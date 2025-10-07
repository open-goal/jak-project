#version 410 core

uniform sampler2D tex_T0;
out vec4 color;
in vec2 tex_coord;

uniform vec4 color_mult;
uniform vec4 color_add;

void main() {
  color = vec4(texture(tex_T0, tex_coord).rgb * color_mult.rgb * color_mult.a, 1.0) + color_add;
}
