#version 430 core

layout (location = 0) in vec4 position_in;
layout (location = 1) in vec4 rgba_in;
layout (location = 2) in vec3 tex_coord_in;
layout (location = 5) in vec4 gs_scissor_in;

out vec4 fragment_color;
out vec3 tex_coord;
out float fog;
out vec4 gs_scissor;

// putting all texture info stuff here so it's easier to copy-paste
layout (location = 3) in uvec4 tex_info_in;
layout (location = 4) in uint use_uv_in;
out flat uvec4 tex_info;
out flat uint use_uv;
uniform int offscreen_mode;

void main() {
  if (offscreen_mode == 1) {
    gl_Position = vec4((position_in.x - 0.453125) * 64., (position_in.y - 0.5 + (2.25 / 64)) * 64, position_in.z * 2 - 1., 1.0);
  }  else {
    gl_Position = vec4((position_in.x - 0.5) * 16., -(position_in.y - 0.5) * 32 * HEIGHT_SCALE, position_in.z * 2 - 1., 1.0);
    // scissoring area adjust
    gl_Position.y *= SCISSOR_ADJUST;
  }

  fragment_color = vec4(rgba_in.x, rgba_in.y, rgba_in.z, rgba_in.w * 2.);
  tex_coord = tex_coord_in;
  tex_info = tex_info_in;
  fog = 255 - position_in.w;
  use_uv = use_uv_in;
  gs_scissor = gs_scissor_in;
}
