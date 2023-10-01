#version 410 core

layout (location = 0) in vec3 position_in;
layout (location = 1) in uint flags;
layout (location = 2) in vec3 normal_in;
layout (location = 3) in uint pat;

uniform vec4 hvdf_offset;
uniform mat4 camera;
uniform vec4 camera_position;
uniform float fog_constant;
uniform float fog_min;
uniform float fog_max;
uniform int wireframe;
uniform int wireframe_enabled;
uniform int mode;
uniform int version;

out vec4 fragment_color;

const int PAT_MOD_COUNT = 4;
const int PAT_EVT_COUNT = 15;
const int PAT_MAT_COUNT = 29;
uniform uint collision_mode_mask[(PAT_MOD_COUNT + 31) / 32];
uniform uint collision_event_mask[(PAT_EVT_COUNT + 31) / 32];
uniform uint collision_material_mask[(PAT_MAT_COUNT + 31) / 32];
uniform uint collision_skip_mask;
uniform uint collision_skip_hide_mask;
uniform uint collision_skip_nomask_allowed;

const int MODE_NONE = 0;
const int MODE_MODE = 1;
const int MODE_EVENT = 2;
const int MODE_MATERIAL = 3;

uint pat_get_mode(uint p) { return version == 2 ? (p >> 7) & 0x7u : (p >> 3) & 0x7u; }
uint pat_get_material(uint p) { return version == 2 ? (p >> 10) & 0x3fu : (p >> 6) & 0x3fu; }
uint pat_get_event(uint p) { return version == 2 ? (p >> 18) & 0x3fu : (p >> 14) & 0x3fu; }
uint pat_get_skip(uint p) { return version == 2 ? p & 0x1f03007fu : p & 0x3007u; }

bool logtest(uint a, uint b) { return (a & b) != 0; }
bool logtesta(uint a, uint b) { return (a & b) == b; }

layout(std140) uniform PatColors {
  vec4 pat_mode_colors[4];
  vec4 pat_material_colors[32];
  vec4 pat_event_colors[32];
};

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

  // wireframe check
  if (wireframe == 0) {
    // lighting check
    vec3 to_cam = camera_position.xyz - position_in;
    float dist_from_cam = length(to_cam);
    vec3 to_cam_n = to_cam / dist_from_cam;
    float cam_dot = abs(dot(to_cam_n, normal_in));

    // base colors
    fragment_color = vec4(vec3(0.75), 1);
    if (wireframe_enabled == 0) {
      fragment_color.rgb *= (pow(cam_dot, 2) * 0.35 + 0.65);
    } else {
      fragment_color.rgb *= (pow(cam_dot, 2.5) * 0.25 + 0.75);
    }
    
    // pat checks
    uint pMode = pat_get_mode(pat);
    uint pMat = pat_get_material(pat);
    uint pEvent = pat_get_event(pat);
    uint pSkip = pat_get_skip(pat);
    if (logtest(collision_mode_mask[pMode/32], 1 << (pMode & 0x1fu)) &&
        logtest(collision_material_mask[pMat/32], 1 << (pMat & 0x1fu)) &&
        logtest(collision_event_mask[pEvent/32], 1 << (pEvent & 0x1fu)) &&
        !logtest(collision_skip_hide_mask, pSkip) &&
        ((pSkip == 0 && collision_skip_nomask_allowed == 1) || (pSkip != 0 && (collision_skip_mask == 0 || logtest(pSkip, collision_skip_mask))))) {
      // fancy colors
      if (mode == MODE_MODE) {
        fragment_color.rgb *= pat_mode_colors[pMode].rgb;
      } else if (mode == MODE_EVENT) {
        fragment_color.rgb *= pat_event_colors[pEvent].rgb;
      } else if (mode == MODE_MATERIAL) {
        fragment_color.rgb *= pat_material_colors[pMat].rgb;
      } else {
        fragment_color = vec4((normal_in + 1)*.5, 1);
        fragment_color.rgb *= (pow(cam_dot, 2) * 0.5 + 0.5);
      }
      if (fragment_color.r < 0 || fragment_color.g < 0 || fragment_color.b < 0) {
        fragment_color.rgb = vec3(1, 0, 1);
      }
    } else {
      // filtered out. goodbye!
      fragment_color.rgba = vec4(0);
    }
  } else {
    fragment_color = vec4(0.12, 0.12, 0.12, 0.5);
  }
}
