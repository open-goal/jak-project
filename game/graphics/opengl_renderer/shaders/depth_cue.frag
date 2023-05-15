#version 430 core

in flat vec4 fragment_color;
in vec2 tex_coord;

uniform sampler2D tex;

out vec4 out_color;

void main() {
  // sample texture
  out_color = fragment_color * texture(tex, tex_coord);
}
