#version 410 core
out vec4 color;
in vec2 uv;
uniform sampler2D tex_T0;
void main() {
  color = texture(tex_T0, uv);
  color.a = 1;
}