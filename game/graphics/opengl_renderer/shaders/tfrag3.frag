#version 430 core

out vec4 color;

in vec4 fragment_color;
in vec3 tex_coord;
uniform sampler2D tex_T0;

uniform float alpha_min;
uniform float alpha_max;

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
}
