// Debug shader for drawing things in red. Uses the same conventions as direct_basic, see there for more details

#version 430 core

layout (location = 0) in vec3 position_in;

out vec4 fragment_color;

void main() {
  gl_Position = vec4((position_in.x - 0.5) * 16., -(position_in.y - 0.5) * 32, position_in.z * 2 - 1., 1.0);
  // scissoring area adjust
  gl_Position.y *= 512.0/448.0;
  fragment_color = vec4(1.0, 0, 0, 0.7);
}
