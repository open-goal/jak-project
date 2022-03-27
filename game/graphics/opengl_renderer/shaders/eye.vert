#version 430 core
layout (location = 0) in vec4 xyst_in;

out vec2 st;

void main() {
    gl_Position = vec4((xyst_in.x - 768.f) / 256.f, (xyst_in.y - 768.f) / 256.f, 0, 1);
    st = xyst_in.zw;
}
