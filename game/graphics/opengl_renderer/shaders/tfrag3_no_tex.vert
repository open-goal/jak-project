#version 430 core

layout (location = 0) in vec3 position_in;
layout (location = 1) in vec4 rgba_in;

uniform vec4 hvdf_offset;
uniform mat4 camera;
uniform float fog_constant;

out vec4 fragment_color;

// this is just for debugging.
void main() {
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
    transformed.z /= (16777216);
    transformed.z *= 2;
    transformed.z -= 1;

    // correct xy scale
    transformed.x /= (256);
    transformed.y /= -(128);

    // hack
    transformed.xyz *= transformed.w;

    gl_Position = transformed;

    // time of day lookup
    fragment_color = rgba_in;
}