#version 410 core
out vec2 frag_uv;
uniform vec2 u_res;
uniform vec2 u_tex;

void main() {
    vec2 pos = vec2(float(gl_VertexID & 1) * 2.0 - 1.0, float(gl_VertexID >> 1) * 2.0 - 1.0);
    frag_uv = vec2(pos.x * 0.5 + 0.5, 0.5 - pos.y * 0.5);

    float tex_aspect = u_tex.x / u_tex.y;
    float fb_aspect  = u_res.x / u_res.y;
    vec2 scale = (fb_aspect > tex_aspect)
    ? vec2(tex_aspect / fb_aspect, 1.0)
    : vec2(1.0, fb_aspect / tex_aspect);

    gl_Position = vec4(pos * scale, 0.0, 1.0);
}