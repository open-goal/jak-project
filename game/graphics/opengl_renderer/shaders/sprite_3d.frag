#version 410 core

out vec4 color;

flat in vec4 fragment_color;
in vec3 tex_coord;
flat in uvec2 tex_info;

uniform sampler2D tex_T20;
uniform sampler2D tex_T21;
uniform sampler2D tex_T22;
uniform sampler2D tex_T23;
uniform sampler2D tex_T24;
uniform sampler2D tex_T25;
uniform sampler2D tex_T26;
uniform sampler2D tex_T27;
uniform sampler2D tex_T28;
uniform sampler2D tex_T29;

vec4 sample_tex(vec2 coord, uint unit) {
  switch (unit) {
    case 0: return texture(tex_T20, coord);
    case 1: return texture(tex_T21, coord);
    case 2: return texture(tex_T22, coord);
    case 3: return texture(tex_T23, coord);
    case 4: return texture(tex_T24, coord);
    case 5: return texture(tex_T25, coord);
    case 6: return texture(tex_T26, coord);
    case 7: return texture(tex_T27, coord);
    case 8: return texture(tex_T28, coord);
    case 9: return texture(tex_T29, coord);
    default: return vec4(1.0, 0, 1.0, 1.0);
  }
}

void main() {
  vec4 T0 = sample_tex(tex_coord.xy, tex_info.x);
  if (tex_info.y == 0) {
    T0.w = 1.0;
  }
  vec4 tex_color = fragment_color * T0 * 2.0;
  if (tex_color.a < 0.016) {
    discard;
  }
  color = tex_color;
}
