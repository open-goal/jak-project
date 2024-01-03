#version 410 core

out vec4 color;

uniform vec4 rgba;
uniform int enable_tex;
uniform int tcc;
uniform ivec4 channel_scramble;
uniform float alpha_multiply;
uniform float minimum;
uniform float maximum;
uniform float slime_scroll;

in vec2 uv;

uniform sampler2D tex_T0;
uniform sampler1D tex_T10;

float cloud_lookup(float v, float minimum, float maximum) {
  maximum = max(minimum, maximum);
  if (v <= minimum) {
    return 0.0;
  }
  if (v >= maximum) {
    return 1.0;
  }

  float alpha = (v - minimum) / (maximum - minimum);
  float sin_alpha = sin(alpha * 3.1415926 / 2.f);
  return sin_alpha * sin_alpha;
}

void main() {
  if (enable_tex == 1) {
    vec4 tex_color = texture(tex_T0, uv);
    vec4 unscambled_tex = vec4(tex_color[channel_scramble[0]],
    tex_color[channel_scramble[1]],
    tex_color[channel_scramble[2]],
    tex_color[channel_scramble[3]]);
    color = rgba / 128.;
    if (tcc == 1) {
      color *= unscambled_tex;
    } else {
      color.xyz *= unscambled_tex.xyz;
    }
  } else if (enable_tex == 2) {
    // cloud version
    vec4 tex_color = texture(tex_T0, uv);
    color.x = 0.5;
    color.y = 0.5;
    color.z = 0.5;
    color.a = 0.5 * cloud_lookup(tex_color.r, minimum, maximum);
  } else if (enable_tex == 3) {
    // cloud version
    vec4 tex_color = texture(tex_T0, uv + vec2(0, slime_scroll));
    color = texelFetch(tex_T10, int(tex_color.r * 255.f), 0);
  } else {
    color = (rgba / 128.);
  }
  color.a *= alpha_multiply;
}