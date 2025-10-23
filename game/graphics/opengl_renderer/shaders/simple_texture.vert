#version 410 core

layout (location = 0) in int index_in;
layout (location = 1) in vec4 color_in;

uniform vec2 tex_coord_0;
uniform vec2 tex_coord_1;

out vec2 tex;

void main() {
  if (index_in == 0) {
    gl_Position = vec4(-1, -1, 0, 1.0);
    tex = tex_coord_0;
  } else if (index_in == 1) {
    gl_Position = vec4(-1, 1, 0, 1.0);
    tex = vec2(tex_coord_0.x, tex_coord_1.y);
  } else if (index_in == 2) {
    gl_Position = vec4(1, -1, 0, 1);
    tex = vec2(tex_coord_1.x, tex_coord_0.y);
  } else {
    gl_Position = vec4(1, 1, 0, 1);
    tex = vec2(tex_coord_1.x, tex_coord_1.y);
  }
}
