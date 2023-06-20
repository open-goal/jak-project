#version 430 core

out vec4 color;
in vec2 tex_coord;

uniform float alpha_intensity;
uniform sampler2D tex_T0;

void main() {
  vec4 tex = texture(tex_T0, tex_coord);
  tex.w *= alpha_intensity;
  color = tex;
}
