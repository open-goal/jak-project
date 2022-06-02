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
        color.rgb = fragment_color.rgb * T0.rgb;
        color.a = fragment_color.a;
        color.rgb = mix(color.rgb, fog_color.rgb, clamp(fog_color.a * fog, 0, 1));
    } else if (bucket == 1 || bucket == 2 || bucket == 4) {
        color = fragment_color * T0;
    } else if (bucket == 3) {
        color = fragment_color * T0;
        color.rgb = mix(color.rgb, fog_color.rgb, clamp(fog_color.a * fog, 0, 1));
    }

}
