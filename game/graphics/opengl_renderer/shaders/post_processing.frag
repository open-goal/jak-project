#version 430 core

in vec2 screen_pos;

out vec4 color;

uniform int msaa_samples;
uniform float msaa_inv;
uniform vec4 fragment_color;
uniform sampler2DMS screen_tex;

void main() {
  vec3 smp_color = texelFetch(screen_tex, ivec2(screen_pos.xy), 0).rgb;
  for (int i = 1; i < msaa_samples; ++i) {
    smp_color += texelFetch(screen_tex, ivec2(screen_pos.xy), i).rgb;
  }
  color = vec4(smp_color * msaa_inv * fragment_color.a, 1.0);
}
