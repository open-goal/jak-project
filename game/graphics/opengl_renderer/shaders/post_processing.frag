#version 430 core

in vec2 screen_pos;

out vec4 color;

uniform vec4 fragment_color;

layout (binding = 0) uniform sampler2D screen_tex;

void main() {
  color = vec4(texture(screen_tex, screen_pos).rgb * fragment_color.a, 1.0);
}
