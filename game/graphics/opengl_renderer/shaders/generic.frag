#version 430 core


out vec4 color;
in vec2 tex_coord;

uniform float alpha_reject;
uniform float color_mult;
uniform vec4 fog_color;
in float fog;
in vec4 fragment_color;

in flat uvec2 tex_info;

uniform int gfx_hack_no_tex;

layout (binding = 0) uniform sampler2D tex_T0;
layout (binding = 1) uniform sampler2D tex_T1;
layout (binding = 2) uniform sampler2D tex_T2;
layout (binding = 3) uniform sampler2D tex_T3;
layout (binding = 4) uniform sampler2D tex_T4;
layout (binding = 5) uniform sampler2D tex_T5;
layout (binding = 6) uniform sampler2D tex_T6;
layout (binding = 7) uniform sampler2D tex_T7;
layout (binding = 8) uniform sampler2D tex_T8;
layout (binding = 9) uniform sampler2D tex_T9;

vec4 sample_tex(vec2 coord, uint unit) {
    return texture(tex_T0, coord);

//    switch (unit) {
//        case 0: return texture(tex_T0, coord);
//        case 1: return texture(tex_T1, coord);
//        case 2: return texture(tex_T2, coord);
//        case 3: return texture(tex_T3, coord);
//        case 4: return texture(tex_T4, coord);
//        case 5: return texture(tex_T5, coord);
//        case 6: return texture(tex_T6, coord);
//        case 7: return texture(tex_T7, coord);
//        case 8: return texture(tex_T8, coord);
//        case 9: return texture(tex_T9, coord);
//        default : return vec4(1.0, 0, 1.0, 1.0);
//    }
}

void main() {
    // 0x1 is tcc
    // 0x2 is decal
    // 0x4 is fog

    if (gfx_hack_no_tex == 0) {
      vec4 T0 = sample_tex(tex_coord.xy, tex_info.x);
      if ((tex_info.y & 1u) == 0) {
          if ((tex_info.y & 2u) == 0) {
              // modulate + no tcc
              color.rgb = fragment_color.rgb * T0.rgb;
              color.a = fragment_color.a;
          } else {
              // decal + no tcc
              color.rgb = T0.rgb * 0.5;
              color.a = fragment_color.a;
          }
      } else {
          if ((tex_info.y & 2u) == 0) {
              // modulate + tcc
              color = fragment_color * T0;
          } else {
              // decal + tcc
              color.rgb = T0.rgb * 0.5;
              color.a = T0.a;
          }
      }
      color *= 2;
    } else {
      if ((tex_info.y & 1u) == 0) {
          if ((tex_info.y & 2u) == 0) {
              // modulate + no tcc
              color.rgb = fragment_color.rgb;
              color.a = fragment_color.a * 2;
          } else {
              // decal + no tcc
              color.rgb = vec3(1);
              color.a = fragment_color.a * 2;
          }
      } else {
          if ((tex_info.y & 2u) == 0) {
              // modulate + tcc
              color = fragment_color;
          } else {
              // decal + tcc
              color.rgb = vec3(0.5);
              color.a = 1.0;
          }
      }
    }
    color.rgb *= color_mult;

    if (color.a < alpha_reject) {
        discard;
    }
    if ((tex_info.y & 4u) != 0) {
        color.xyz = mix(color.xyz, fog_color.rgb, clamp(fog_color.a * fog, 0, 1));
    }
}
