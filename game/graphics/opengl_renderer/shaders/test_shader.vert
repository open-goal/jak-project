#version 430 core

layout (location = 0) in vec3 position_in;
layout (location = 1) in vec4 rgba_in;

out vec4 fragment_color;

void main() {
  gl_Position = vec4(position_in, 1.0);
  fragment_color = rgba_in;
}