#version 430 core

out vec4 color;

uniform vec4 rgba;
uniform int enable_tex;

in vec2 uv;

layout (binding = 0) uniform sampler2D tex;

void main() {
  if (enable_tex == 1) {
    vec4 tex_color = texture(tex, uv);
    color = rgba * tex_color;
  } else {
    color = rgba;
  }
}