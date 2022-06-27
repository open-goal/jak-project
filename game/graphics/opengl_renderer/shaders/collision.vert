#version 430 core

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
uniform int mode;

out vec4 fragment_color;

const int PAT_MOD_COUNT = 3;
const int PAT_EVT_COUNT = 7;
const int PAT_MAT_COUNT = 23;
uniform uint collision_mode_mask[(PAT_MOD_COUNT + 31) / 32];
uniform uint collision_event_mask[(PAT_EVT_COUNT + 31) / 32];
uniform uint collision_material_mask[(PAT_MAT_COUNT + 31) / 32];
uniform uint collision_skip_mask;

const int MODE_NONE = 0;
const int MODE_MODE = 1;
const int MODE_EVENT = 2;
const int MODE_MATERIAL = 3;

uint pat_get_mode(uint p) { return (p >> 3) & 0x7; }
uint pat_get_material(uint p) { return (p >> 6) & 0x3f; }
uint pat_get_event(uint p) { return (p >> 14) & 0x3f; }

bool logtest(uint a, uint b) { return (a & b) != 0; }
bool logtesta(uint a, uint b) { return (a & b) == b; }

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
    gl_Position.y *= 512.0/448.0;

    // wireframe check
    if (wireframe == 0) {
        // lighting check
        vec3 to_cam = camera_position.xyz - position_in;
        float dist_from_cam = length(to_cam);
        vec3 to_cam_n = to_cam / dist_from_cam;
        float cam_dot = abs(dot(to_cam_n, normal_in));

        // base colors
        fragment_color = vec4(0.5, 0.6, 0.6, 1);
        fragment_color.xyz *= (pow(cam_dot, 3) + 0.3);
        
        // pat checks
        uint pMode = pat_get_mode(pat);
        uint pMat = pat_get_material(pat);
        uint pEvent = pat_get_event(pat);
        if (logtest(collision_mode_mask[pMode/32], 1 << (pMode & 0x1f)) &&
            logtest(collision_material_mask[pMat/32], 1 << (pMat & 0x1f)) &&
            logtest(collision_event_mask[pEvent/32], 1 << (pEvent & 0x1f)) &&
            logtesta(collision_skip_mask, pat)) {
          // fancy colors
          if (mode == MODE_MODE) {
            switch ( pMode ) {
              case 0: fragment_color.rgb *= vec3(1.25, 0.1, 0.1); break; // ground
              case 1: fragment_color.rgb *= vec3(0.1, 0.1, 1.0); break; // wall
              case 2: fragment_color.rgb *= vec3(1.0, 0.1, 1.0); break; // obstacle
              default: fragment_color.rgb = vec3(1, 0, 1); break;
            }
          } else if (mode == MODE_EVENT) {
            switch ( pEvent ) {
              case 0: fragment_color.rgb *= vec3(1.0); break; // none
              case 1: fragment_color.rgb *= vec3(0.2, 1.0, 1.0); break; // deadly
              case 2: fragment_color.rgb *= vec3(0.1, 1.0, 0.1); break; // endlessfall
              case 3: fragment_color.rgb *= vec3(1.0, 1.0, 0.1); break; // burn
              case 4: fragment_color.rgb *= vec3(0.1, 0.1, 1.0); break; // deadlyup
              case 5: fragment_color.rgb *= vec3(1.0, 0.1, 0.5); break; // burnup
              case 6: fragment_color.rgb *= vec3(1.0, 0.1, 0.1); break; // melt
              default: fragment_color.rgb = vec3(1, 0, 1); break;
            }
          } else if (mode == MODE_MATERIAL) {
            switch ( pMat ) {
              case  0: fragment_color.rgb *= vec3(1.0, 0.7, 1.0); break; // stone
              case  1: fragment_color.rgb *= vec3(0.1, 2.0, 2.0); break; // ice
              case  2: fragment_color.rgb *= vec3(0.75, 0.25, 0.1); break; // quicksand
              case  3: fragment_color.rgb *= vec3(0.1, 0.25, 0.75); break; // waterbottom
              case  4: fragment_color.rgb *= vec3(0.5, 0.15, 0.1); break; // tar
              case  5: fragment_color.rgb *= vec3(2.0, 1.5, 0.5); break; // sand
              case  6: fragment_color.rgb *= vec3(1.5, 0.75, 0.1); break; // wood
              case  7: fragment_color.rgb *= vec3(0.1, 1.35, 0.1); break; // grass
              case  8: fragment_color.rgb *= vec3(1.7, 1.3, 0.1); break; // pcmetal
              case  9: fragment_color.rgb *= vec3(1.8); break; // snow
              case 10: fragment_color.rgb *= vec3(1.5, 0.2, 1.0); break; // deepsnow
              case 11: fragment_color.rgb *= vec3(1.2, 0.5, 0.3); break; // hotcoals
              case 12: fragment_color.rgb *= vec3(1.4, 0.1, 0.1); break; // lava
              case 13: fragment_color.rgb *= vec3(0.8, 0.3, 0.1); break; // crwood
              case 14: fragment_color.rgb *= vec3(1.0, 0.4, 1.0); break; // gravel
              case 15: fragment_color.rgb *= vec3(1.5, 0.5, 0.15); break; // dirt
              case 16: fragment_color.rgb *= vec3(0.7, 0.7, 1.0); break; // metal
              case 17: fragment_color.rgb *= vec3(0.1, 0.1, 1.2); break; // straw
              case 18: fragment_color.rgb *= vec3(0.75, 1.75, 0.75); break; // tube
              case 19: fragment_color.rgb *= vec3(0.4, 0.1, 0.8); break; // swamp
              case 20: fragment_color.rgb *= vec3(0.1, 0.4, 0.8); break; // stopproj
              case 21: fragment_color.rgb *= vec3(1.9, 0.1, 1.9); break; // rotate
              case 22: fragment_color.rgb *= vec3(1.0); break; // neutral
              default: fragment_color.rgb = vec3(1, 0, 1); break;
            }
          } else {
            fragment_color = vec4((normal_in + 1)*.5, 1);
            fragment_color.xyz *= (pow(cam_dot, 3) + 0.3);
          }
        } else {
          // filtered out. goodbye!
          fragment_color.rgba = vec4(0);
        }
    } else {
        fragment_color = vec4(0.12, 0.12, 0.12, 0.5);
    }
}
