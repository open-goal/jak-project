#version 430 core

out vec4 color;

in vec4 fragment_color;
noperspective in vec3 tex_coord;
uniform sampler2D tex_T0;

void main() {
  vec4 T0 = texture(tex_T0, tex_coord.xy / tex_coord.z);
  T0.w = 1.0;
  color = fragment_color * T0 * 1.0;
}
