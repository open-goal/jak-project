#version 410 core

out vec4 color;

in vec4 fragment_color;
in vec2 tex_coord;
in float fogginess;
uniform sampler2D tex_T0;
uniform vec4 fog_color;

uniform int gfx_hack_no_tex;


void main() {
  if (gfx_hack_no_tex == 0) {
    vec4 T0 = texture(tex_T0, tex_coord);
    color = fragment_color * T0 * 2;
  } else {
    color = fragment_color;
  }

   color.rgb = mix(color.rgb, fog_color.rgb, clamp(fogginess * fog_color.a, 0, 1));
}
