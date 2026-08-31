#version 410 core

layout (location = 0) in vec3 position_in;
layout (location = 1) in vec4 rgba_in;
layout (location = 2) in vec3 tex_coord_in;
layout (location = 3) in uvec4 byte_info;


out vec4 fragment_color;
out vec3 tex_coord;
out float fog;

// putting all texture info stuff here so it's easier to copy-paste
flat out uvec2 tex_info;

void main() {
  // GLSL hex integer literals (0x8000 etc.) can't take a ".0" suffix to become float -- they're
  // always integer type. Written out as decimal float literals instead: 0x8000=32768,
  // 0x1000=4096, 0x800=2048, 0x800000=8388608.
  gl_Position = vec4((position_in.x - 32768.) / 4096.,
                    -(position_in.y - 32768.) / 2048.,
                    position_in.z / 8388608. - 1., 1.0);
  // scissoring area adjust
  gl_Position.y *= SCISSOR_ADJUST;
  fragment_color = vec4(rgba_in.x, rgba_in.y, rgba_in.z, rgba_in.w * 2.);
  tex_coord = tex_coord_in;
  tex_info = byte_info.xy;
  fog = 255. - float(byte_info.z);
}
