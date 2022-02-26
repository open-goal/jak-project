#version 430 core

out vec4 color;

in vec4 fragment_color;
in vec3 tex_coord;
in float fogginess;
uniform sampler2D tex_T0;

uniform float alpha_min;
uniform float alpha_max;
uniform vec4 fog_color;

void main() {
    //vec4 T0 = texture(tex_T0, tex_coord);
    vec4 T0 = texture(tex_T0, tex_coord.xy);
    color = fragment_color * T0 * 2.0;

    if (color.a < alpha_min) {
        discard;
    }

    if (color.a > alpha_max) {
        discard;
    }

    color.xyz = mix(color.xyz, fog_color.xyz / 255., clamp(fogginess/255 * fog_color.w, 0., 1.));
}
