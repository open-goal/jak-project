#version 330 core

out vec4 color;

in vec4 fragment_color;
in vec3 tex_coord;
uniform sampler2D tex_T0;
uniform float alpha_reject;

void main() {
    //vec4 T0 = texture(tex_T0, tex_coord);
    vec4 T0 = texture(tex_T0, tex_coord.xy / tex_coord.z);
    color = fragment_color * T0 * 2.0;
    if (color.a <= alpha_reject) {
        discard;
    }
}
