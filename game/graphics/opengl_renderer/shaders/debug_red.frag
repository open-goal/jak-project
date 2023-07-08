// Debug shader for drawing things in red.  Uses the same conventions as direct_basic, see there for more details

#version 410 core

out vec4 color;

in vec4 fragment_color;

void main() {
  color = fragment_color;
}
