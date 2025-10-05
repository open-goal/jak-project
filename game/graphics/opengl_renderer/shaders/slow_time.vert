#version 410 core

layout (location = 0) in int index_in;
layout (location = 1) in vec4 color_in;

uniform float amount;

out vec2 tex;
out vec4 color;

void main() {
  if (index_in == 0) {
    gl_Position = vec4(0, 0, 0, 1.0);
    tex = vec2(0.5, 0.5);
    color = vec4(0.75, 0.75, 0.75, 1.5);
  } else if (index_in == 1) {
    gl_Position = vec4(-1, -1, 0, 1.0);
    tex = vec2(0, 0);
    color = vec4(1, 1, 1, amount);
  } else if (index_in == 2) {
    gl_Position = vec4(1, -1, 0, 1);
    tex = vec2(1, 0);
    color = vec4(1, 1, 1, amount);
  } else if (index_in == 3) {
    gl_Position = vec4(1, 1, 0, 1);
    tex = vec2(1, 1);
    color = vec4(1, 1, 1, amount);
  } else if (index_in == 4) {
    gl_Position = vec4(-1, 1, 0, 1);
    tex = vec2(0, 1);
    color = vec4(1, 1, 1, amount);
  } else if (index_in == 5) {
    gl_Position = vec4(-1, -1, 0, 1.0);
    tex = vec2(0, 0);
    color = vec4(1, 1, 1, amount);
  }
}
