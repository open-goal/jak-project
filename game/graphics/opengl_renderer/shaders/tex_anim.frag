#version 410 core

out vec4 color;

uniform vec4 rgba;
uniform int enable_tex;
uniform int tcc;
uniform ivec4 channel_scramble;
uniform float alpha_multiply;

in vec2 uv;

uniform sampler2D tex_T0;

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
  } else {
    color = (rgba / 128.);
  }

  color.a *= alpha_multiply;
}