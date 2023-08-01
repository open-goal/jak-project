#version 410 core

layout (location = 0) in vec3 position_in;

out vec3 tex_coord;

void main() {
  gl_Position = vec4(position_in.x*2 -1, position_in.y*2-1, 0.0, 1.0);
  tex_coord = position_in;
}
