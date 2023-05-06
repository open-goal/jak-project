#version 430 core

layout (location = 0) in vec3 position_in;

uniform int clear_mode;
uniform float fog;
uniform vec4 perspective_x;
uniform vec4 perspective_y;
uniform vec4 perspective_z;
uniform vec4 perspective_w;
uniform vec4 hvdf_offset;


void main() {
  if (clear_mode == 1) {
    // this is just copied from shadow 1, needs revisiting.
    gl_Position = vec4((position_in.x - 0.5) * 16., -(position_in.y - 0.5) * 32, position_in.z * 2 - 1., 1.0);
    gl_Position.y *= SCISSOR_ADJUST;
  } else {
    vec4 transformed = -perspective_w;
    transformed -= perspective_x * position_in.x;
    transformed -= perspective_y * position_in.y;
    transformed -= perspective_z * position_in.z;
    transformed.xyz *= fog / transformed.w;


    transformed.xyz += hvdf_offset.xyz;
    transformed.xy -= (2048.);
    transformed.z /= (8388608);
    transformed.z -= 1;
    transformed.x /= (256);
    transformed.y /= -(128);
    transformed.xyz *= transformed.w;
    transformed.y *= SCISSOR_ADJUST * HEIGHT_SCALE;
    gl_Position = transformed;
  }
}