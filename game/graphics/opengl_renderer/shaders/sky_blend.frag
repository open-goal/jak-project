#version 430 core

layout(location = 0) out vec4 color;

in vec3 tex_coord;
layout (binding = 0) uniform sampler2D tex_T0;

void main() {
  vec4 T0 = texture(tex_T0, tex_coord.xy);
  color = vec4(T0 * tex_coord.z);
}
