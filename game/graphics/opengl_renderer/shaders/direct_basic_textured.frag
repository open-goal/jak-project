#version 430 core

out vec4 color;

in vec4 fragment_color;
in vec3 tex_coord;
in flat uint use_uv;
uniform float alpha_reject;
uniform float color_mult;
uniform float alpha_mult;
uniform float alpha_sub;
uniform float ta0;

uniform vec4 fog_color;

in flat uvec4 tex_info;
in float fog;

layout (binding = 20) uniform sampler2D tex_T0;
layout (binding = 21) uniform sampler2D tex_T1;
layout (binding = 22) uniform sampler2D tex_T2;
layout (binding = 23) uniform sampler2D tex_T3;
layout (binding = 24) uniform sampler2D tex_T4;
layout (binding = 25) uniform sampler2D tex_T5;
layout (binding = 26) uniform sampler2D tex_T6;
layout (binding = 27) uniform sampler2D tex_T7;
layout (binding = 28) uniform sampler2D tex_T8;
layout (binding = 29) uniform sampler2D tex_T9;

vec4 sample_tex(vec2 coord, uint unit) {
    switch (unit) {
        case 0: return texture(tex_T0, coord);
        case 1: return texture(tex_T1, coord);
        case 2: return texture(tex_T2, coord);
        case 3: return texture(tex_T3, coord);
        case 4: return texture(tex_T4, coord);
        case 5: return texture(tex_T5, coord);
        case 6: return texture(tex_T6, coord);
        case 7: return texture(tex_T7, coord);
        case 8: return texture(tex_T8, coord);
        case 9: return texture(tex_T9, coord);
        default : return vec4(1.0, 0, 1.0, 1.0);
    }
}

vec4 sample_tex_px(vec2 coordf, uint unit) {
    ivec2 coord;
    coord.x = int(coordf.x / 16);
    coord.y = int(coordf.y / 16);
    switch (unit) {
        case 0: return texelFetch(tex_T0, coord, 0);
        case 1: return texelFetch(tex_T1, coord, 0);
        case 2: return texelFetch(tex_T2, coord, 0);
        case 3: return texelFetch(tex_T3, coord, 0);
        case 4: return texelFetch(tex_T4, coord, 0);
        case 5: return texelFetch(tex_T5, coord, 0);
        case 6: return texelFetch(tex_T6, coord, 0);
        case 7: return texelFetch(tex_T7, coord, 0);
        case 8: return texelFetch(tex_T8, coord, 0);
        case 9: return texelFetch(tex_T9, coord, 0);
        default : return vec4(1.0, 0, 1.0, 1.0);
    }
}

void main() {
    vec4 T0;
    if (use_uv == 1) {
        T0 = sample_tex_px(tex_coord.xy, tex_info.x);
    } else {
        T0 = sample_tex(tex_coord.xy / tex_coord.z, tex_info.x);
    }
    // y is tcc
    // z is decal
    if (T0.w == 0) {
        T0.w = ta0;
    }

    if (tex_info.y == 0) {
        if (tex_info.z == 0) {
            // modulate + no tcc
            color.xyz = fragment_color.xyz * T0.xyz;
            color.w = fragment_color.w;
        } else {
            // decal + no tcc
            color.xyz = T0.xyz * 0.5;
            color.w = fragment_color.w;
        }
    } else {
        if (tex_info.z == 0) {
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
    color.w *= alpha_mult;
    if (color.a < alpha_reject) {
        discard;
    }
    if (tex_info.w == 1) {
        color.xyz = mix(color.xyz, fog_color.rgb, clamp(fog_color.a * fog, 0, 1));
    }

}
