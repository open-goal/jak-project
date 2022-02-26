#version 430 core

layout (location = 0) in vec3 position_in;
layout (location = 1) in vec3 tex_coord_in;
layout (location = 2) in int time_of_day_index;

uniform vec4 hvdf_offset;
uniform mat4 camera;
uniform float fog_constant;
uniform float fog_min;
uniform float fog_max;
layout (binding = 10) uniform sampler1D tex_T1; // note, sampled in the vertex shader on purpose.

out vec4 fragment_color;
out vec3 tex_coord;
out float fogginess;

void main() {


    // old system:
    // - load vf12
    // - itof0 vf12
    // - multiply with camera matrix (add trans)
    // - let Q = fogx / vf12.w
    // - xyz *= Q
    // - xyzw += hvdf_offset
    // - clip w.
    // - ftoi4 vf12
    // use in gs.
    // gs is 12.4 fixed point, set up with 2048.0 as the center.

    // the itof0 is done in the preprocessing step.  now we have floats.

    // Step 3, the camera transform
    vec4 transformed = -camera[3].xyzw;
    transformed += -camera[0] * position_in.x;
    transformed += -camera[1] * position_in.y;
    transformed += -camera[2] * position_in.z;

    // compute Q
    float Q = fog_constant / transformed[3];

    float fog1 = -transformed.w + hvdf_offset.w;
    float fog2 = min(fog1, fog_max);
    float fog3 = max(fog2, fog_min);
    fogginess = 255 - fog3;

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

    // time of day lookup
    fragment_color = texelFetch(tex_T1, time_of_day_index, 0);
    fragment_color.w *= 2;
    tex_coord = tex_coord_in;
}
