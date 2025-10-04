#version 410 core

out vec4 out_color;
in vec2 tex;
uniform vec4 color;
uniform sampler2D tex_T0;

void main() {
  out_color = texture(tex_T0, tex) * color;
}