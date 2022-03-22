#version 430 core

out vec4 color;

in vec4 fragment_color;
in vec3 tex_coord;
uniform float color_mult;
uniform float alpha_mult;

uniform vec4 fog_color;
uniform int bucket;

in float fog;

uniform sampler2D tex_T0;

void main() {
    vec4 T0 = texture(tex_T0, tex_coord.xy / tex_coord.z);
    if (bucket == 0) {
        color.xyz = fragment_color.xyz * T0.xyz;
        color.w = fragment_color.w;
        color.xyz *= 2;
        color.xyz = mix(color.xyz, fog_color.xyz / 255., clamp(fog_color.w * (1 - fog), 0, 1));
    } else if (bucket == 1) {
        color = fragment_color * T0;
        color.xyzw *= 2;
    } else if (bucket == 2) {
        color = fragment_color * T0;
    } else if (bucket == 3) {
        color = fragment_color * T0;
        color.xyzw *= 2;
        color.xyz = mix(color.xyz, fog_color.xyz / 255., clamp(fog_color.w * (1 - fog), 0, 1));
    } else if (bucket == 4) {
        color = fragment_color * T0;
        color.w = 0;
    }

}
