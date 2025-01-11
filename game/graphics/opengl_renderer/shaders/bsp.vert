#version 410 core

layout (location = 0) in vec3 position_in;
layout (location = 1) in uint bsp_cell;
// layout (location = 2) in vec3 normal_in;
// layout (location = 3) in uint pat;

uniform vec4 hvdf_offset;
uniform mat4 camera;
uniform vec4 camera_position;
uniform float fog_constant;
uniform float fog_min;
uniform float fog_max;
uniform int wireframe;
uniform int min_leaf;
uniform int max_leaf;

out vec4 fragment_color;

void main() {
  // Step 3, the camera transform
  vec4 transformed = -camera[3].xyzw;
  transformed += -camera[0] * position_in.x;
  transformed += -camera[1] * position_in.y;
  transformed += -camera[2] * position_in.z;

  // compute Q
  float Q = fog_constant / transformed[3];

  // perspective divide!
  transformed.xyz *= Q;

  // offset
  transformed.xyz += hvdf_offset.xyz;

  // correct xy offset
  transformed.xy -= (2048.);

  // correct z scale
  transformed.z /= (8388608);
  transformed.z -= 1;

  // correct xy scale
  transformed.x /= (256);
  transformed.y /= -(128);

  // hack
  transformed.xyz *= transformed.w;

  gl_Position = transformed;
  // scissoring area adjust
  gl_Position.y *= SCISSOR_ADJUST * HEIGHT_SCALE;

  //
  fragment_color = vec4(0.12, 0.12, 0.12, 0.5);

  if (bsp_cell < min_leaf || bsp_cell > max_leaf) {
    fragment_color.a = 0;
  }

}
