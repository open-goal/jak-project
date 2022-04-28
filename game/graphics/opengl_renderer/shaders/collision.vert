#version 430 core

layout (location = 0) in vec3 position_in;
layout (location = 1) in uint flags;
layout (location = 2) in vec3 normal_in;

uniform vec4 hvdf_offset;
uniform mat4 camera;
uniform vec4 camera_position;
uniform float fog_constant;
uniform float fog_min;
uniform float fog_max;
uniform int mode;

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

    // ftoi4
    //transformed.xyzw *= 16;

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

    vec3 to_cam = camera_position.xyz - position_in;
    float dist_from_cam = length(to_cam);
    vec3 to_cam_n = to_cam / dist_from_cam;
    float cam_dot = abs(dot(to_cam_n, normal_in));

    // color
    if (mode == 0) {
        fragment_color = vec4((normal_in + 1)*.5, 1);
        fragment_color.xyz *= (pow(cam_dot, 3) + 0.3);
    } else {
        fragment_color = vec4(0.0, 0.3, 0.3, 1);
    }

}
