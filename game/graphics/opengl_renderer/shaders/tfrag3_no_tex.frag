#version 410 core

out vec4 color;

in vec4 fragment_color;
in vec3 tex_coord;
uniform sampler2D tex_T0;

uniform float alpha_min;
uniform float alpha_max;

void main() {
  color = fragment_color;
}
