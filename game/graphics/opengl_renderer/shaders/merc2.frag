#version 430 core

out vec4 color;
in vec3 vtx_color;
in vec2 vtx_st;
in float fog;


uniform sampler2D tex_T0;

uniform vec4 fog_color;



void main() {
    vec4 T0 = texture(tex_T0, vtx_st);
    color = vec4(vtx_color, 1.0) * T0 * 2.0;
    color.w *= 2;

    if (color.w < 0.128) {
        discard;
    }

    color.xyz = mix(color.xyz, fog_color.xyz / 255., clamp(fog, 0, 1));
}