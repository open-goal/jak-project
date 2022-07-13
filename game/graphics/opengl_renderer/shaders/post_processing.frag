#version 430 core

in vec2 screen_pos;

out vec4 color;

uniform vec4 fragment_color;
uniform sampler2DMS screen_tex;

void main() {
  vec3 smp_color = texelFetch(screen_tex, ivec2(screen_pos.x * 640, screen_pos.y * 480), 0).rgb;
  for (int i = 1; i < 4; ++i) {
    smp_color += texelFetch(screen_tex, ivec2(screen_pos.x * 640, screen_pos.y * 480), i).rgb;
  }
  smp_color /= 4;
  color = vec4(smp_color * fragment_color.a, 1.0);
}
