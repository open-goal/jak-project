#version 430 core

in vec2 screen_pos;

out vec4 color;

uniform vec4 fragment_color;
uniform sampler2DMS screen_tex;

void main() {
color = vec4(texelFetch(screen_tex, ivec2(screen_pos.x * 640, screen_pos.y * 480), 0).rgb, fragment_color.a);
  for (int i = 1; i < 4; ++i) {
    color += vec4(texelFetch(screen_tex, ivec2(screen_pos.x * 640, screen_pos.y * 480), i).rgb, fragment_color.a);
  }
  color /= 4;
  color *= fragment_color.a;
}
