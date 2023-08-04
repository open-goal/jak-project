#version 410 core

layout (location = 0) in int vertex_index;

uniform vec3 positions[4];
uniform vec2 uvs[4];
uniform vec4 rgba;
// TODO flags and stuff

out vec2 uv;

void main() {
  gl_Position = vec4(-1. + (positions[vertex_index] * 2), 1);
  uv = uvs[vertex_index];
}
