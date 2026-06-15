#version 410 core
in vec2 frag_uv;
out vec4 out_color;
uniform sampler2D splash_tex;
void main() {
    out_color = texture(splash_tex, frag_uv);
}