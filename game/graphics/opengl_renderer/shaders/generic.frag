#version 430 core


out vec4 color;
in vec2 tex_coord;

uniform float alpha_reject;
uniform float color_mult;
uniform vec4 fog_color;
in float fog;
in vec4 fragment_color;

in flat uvec2 tex_info;

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
    vec4 T0 = sample_tex(tex_coord.xy, tex_info.x);
    // y is tcc
    // z is decal

    if ((tex_info.y & 1u) == 0) {
        if ((tex_info.y & 2u) == 0) {
            // modulate + no tcc
            color.xyz = fragment_color.xyz * T0.xyz;
            color.w = fragment_color.w;
        } else {
            // decal + no tcc
            color.xyz = T0.xyz * 0.5;
            color.w = fragment_color.w;
        }
    } else {
        if ((tex_info.y & 2u) == 0) {
            // modulate + tcc
            color = fragment_color * T0;
        } else {
            // decal + tcc
            color.xyz = T0.xyz * 0.5;
            color.w = T0.w;
        }
    }
    color *= 2;
    color.xyz *= color_mult;

    if (color.a < alpha_reject) {
        discard;
    }
    if ((tex_info.y & 4u) != 0) {
        color.xyz = mix(color.xyz, fog_color.rgb, clamp(fog_color.a * fog, 0, 1));
    }
}
