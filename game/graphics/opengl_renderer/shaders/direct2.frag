#version 430 core

out vec4 color;

in vec4 fragment_color;
in vec3 tex_coord;
uniform float alpha_reject;
uniform float color_mult;
uniform vec4 fog_color;

in flat uvec4 tex_info;
in float fog;

layout (binding = 0) uniform sampler2D tex_T0;


vec4 sample_tex(vec2 coord) {
    return texture(tex_T0, coord);
}

void main() {
    vec4 T0 = sample_tex(tex_coord.xy / tex_coord.z);
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
        color.xyz = mix(color.xyz, fog_color.xyz / 255., clamp(fog_color.w * (1 - fog), 0, 1));
    }
}
