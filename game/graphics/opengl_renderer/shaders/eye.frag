#version 430 core

out vec4 color;
in vec2 st;
uniform sampler2D tex_T0;

void main() {
    color = texture(tex_T0, st);
    color.w *= 2;
}