#version 410 core

in vec2 screen_pos;

out vec4 color;

uniform vec4 fragment_color;

uniform sampler2D tex_T0;

void main() {
  color = vec4(texture(tex_T0, screen_pos).rgb * fragment_color.a, 1.0);
}
