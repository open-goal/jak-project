#version 410 core

// merc vertex definition
layout (location = 0) in vec3 position_in;
layout (location = 1) in float weight_in;
layout (location = 2) in uvec2 mats;
layout (location = 3) in uint flags;

// camera control
uniform vec4 hvdf_offset;
uniform vec4 fog_constants;
uniform mat4 perspective_matrix;
uniform mat4 camera_rot;
uniform vec3 debug_color;
uniform vec4 bottom_plane;
uniform vec4 top_plane;
uniform vec3 origin;
uniform bool bottom_cap;

// output
out vec4 vtx_color;

int offset = 0;

struct MercMatrixData {
  mat4 X;
};

layout (std140) uniform ub_bones {
  MercMatrixData bones[128];
};

/*
- 0 `sub.xyzw vf19, vf01, vf03` : `vf19 = center - vert`
- 1 `mul.xyzw vf11, vf03, vf02` : `vf11 = dot(vert, plane)`
- 2 `mul.xyz vf15, vf19, vf02`  : `vf15 = dot3(center - vert, plane)`
- 3 `move.xyzw vf07, vf03`      : `vf07 = vert`
- 4 `addy.x vf11, vf11, vf11`   : `vf11.x += vf11.y`
- 5 `addy.x vf15, vf15, vf15`   : `vf15.x += vf15.y`
- 6 `addz.x vf11, vf11, vf11`   : `vf11.x += vf11.z`
- 7 `addz.x vf15, vf15, vf15`   : `vf15.x += vf15.z`
- 8 `addw.x vf11, vf11, vf11`   : `vf11.x += vf11.w`
- 9 `div Q, vf11.x, vf15.x`     : `Q = dot(vert, plane) / dot3(center - vert, plane)`
- 10 `mul.xyzw vf19, vf19, Q`   :
- 11 `sub.xyzw vf07, vf07, vf19`:
*/

vec4 dual(vec4 p, vec4 plane) {
  vec4 offset = vec4(origin, 1) - p;
  return p - offset * dot(p, plane) / dot(offset.xyz, plane.xyz);
}

vec4 scissor(vec4 p, vec4 plane) {
  float plane_offset = dot(p, plane);
  if (plane_offset > 0) {
    vec4 offset = vec4(origin, 1) - p;
    return p - offset * plane_offset / dot(offset.xyz, plane.xyz);
  } else {
    return p;
  }
}

void main() {
  vec4 p = vec4(position_in, 1);

  vec4 vtx_pos = -bones[mats[0] + offset].X * p * weight_in;

  if (weight_in > 1) {
    vtx_pos += -bones[mats[1] + offset].X * p * (1.f - weight_in);
  }


  if (bottom_cap) {
    vtx_pos = dual(vtx_pos, bottom_plane);
  } else {
    if ((flags & uint(1)) != 0) {
      vtx_pos = dual(vtx_pos, bottom_plane);
    } else {
      vtx_pos = scissor(vtx_pos, top_plane);
    }
  }



  vec4 transformed = perspective_matrix * vtx_pos;

  float Q = fog_constants.x / transformed[3];

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


  vtx_color = vec4(debug_color, 1.0);
}
