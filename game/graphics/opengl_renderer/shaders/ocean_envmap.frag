#version 410 core
uniform sampler2D tex_T1;
in vec2 tex_coord;
out vec4 color;
void main() {
    const float PI = 3.14159265358979;
    float u = tex_coord.x;
    float v = tex_coord.y;
    float theta = (0.5 - u) * 2.0 * PI;
    float t = 1.0 - abs(2.0 * v - 1.0);
    vec2 st = vec2(0.5) + t * 0.5 * vec2(sin(theta), cos(theta));
    color = texture(tex_T1, st);
}