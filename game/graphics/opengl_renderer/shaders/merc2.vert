#version 410 core

// merc vertex definition
layout (location = 0) in vec3 position_in;
layout (location = 1) in vec3 normal_in;
layout (location = 2) in vec3 weights_in;
layout (location = 3) in vec2 st_in;
layout (location = 4) in vec4 rgba;
layout (location = 5) in uvec3 mats;

// light control
uniform vec4 light_dir0_fade;
uniform vec4 light_dir1_fade_en;
uniform vec3 light_dir2;
uniform vec4 light_col0;
uniform vec4 light_col1;
uniform vec4 light_col2;
uniform vec4 light_ambient;

// camera control
uniform vec4 hvdf_offset;
uniform vec4 fog_constants;

uniform mat4 perspective_matrix;

// output
out vec4 vtx_color;
out vec2 vtx_st;

out float fog;

struct MercMatrixData {
  mat4 X;
  mat3 R;
  vec4 pad;
};

layout (std140) uniform ub_bones {
  MercMatrixData bones[128];
};


/*
The inputs are in registers 8, 10, 12, 25, and the outputs are 9, 11, 13, 26. The output is written over the input.

```
mula.xyzw ACC, vf15, vf08
maddz.xyzw vf09, vf16, vf08
mula.xyzw ACC, vf15, vf10
maddz.xyzw vf11, vf16, vf10
mula.xyzw ACC, vf15, vf12
maddz.xyzw vf13, vf16, vf12
addax.xyzw vf20, vf00
madda.xyzw ACC, vf27, vf25
maddz.xyzw vf26, vf28, vf25
```
*/
void main() {
  //  vec4 transformed = -perspective3.xyzw;
  //  transformed += -perspective0 * position_in.x;
  //  transformed += -perspective1 * position_in.y;
  //  transformed += -perspective2 * position_in.z;


  //  vec4 transformed = -hmat3.xyzw;
  //  transformed += -hmat0 * position_in.x;
  //  transformed += -hmat1 * position_in.y;
  //  transformed += -hmat2 * position_in.z;

  vec4 p = vec4(position_in, 1);
  vec4 vtx_pos = -bones[mats[0]].X * p * weights_in[0];
  vec3 rotated_nrm = bones[mats[0]].R * normal_in * weights_in[0];

  // game may send garbage bones if the weight is 0, don't let NaNs sneak in.
  if (weights_in[1] > 0) {
    vtx_pos += -bones[mats[1]].X * p * weights_in[1];
    rotated_nrm += bones[mats[1]].R * normal_in * weights_in[1];
  }
  if (weights_in[2] > 0) {
    vtx_pos += -bones[mats[2]].X * p * weights_in[2];
    rotated_nrm += bones[mats[2]].R * normal_in * weights_in[2];
  }

  vec4 transformed = perspective_matrix * vtx_pos;

  rotated_nrm = normalize(rotated_nrm);
  vec3 light_intensity = light_dir0_fade.xyz * rotated_nrm.x + light_dir1_fade_en.xyz * rotated_nrm.y + light_dir2 * rotated_nrm.z;
  light_intensity = max(light_intensity, vec3(0, 0, 0));

  vec4 light_color = light_ambient
                   + light_intensity.x * light_col0
                   + light_intensity.y * light_col1
                   + light_intensity.z * light_col2;



  float Q = fog_constants.x / transformed[3];
  fog = 255 - clamp(-transformed.w + hvdf_offset.w, fog_constants.y, fog_constants.z);

  transformed.xyz *= Q;
  transformed.xyz += hvdf_offset.xyz;
  transformed.xy -= (2048.);
  transformed.z /= (8388608);
  transformed.z -= 1;
  transformed.x /= (256);
  transformed.y /= -(128);
  transformed.xyz *= transformed.w;
  transformed.y *= SCISSOR_ADJUST * HEIGHT_SCALE;
  gl_Position = transformed;


  vtx_color = rgba * light_color;
  vtx_st = st_in;
}
