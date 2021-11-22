#version 330 core

layout (location = 0) in vec3 position_in;
layout (location = 1) in vec3 tex_coord_in;
layout (location = 2) in float time_of_day_index;

uniform vec4 hvdf_offset;
uniform mat4 camera;
uniform float fog_constant;
uniform sampler1D tex_T1; // note, sampled in the vertex shader on purpose.

out vec4 fragment_color;
out vec3 tex_coord;

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

    // New Version (again)
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
    transformed.z /= (16777216);
    transformed.z *= 2;
    transformed.z -= 1;


    // correct xy scale
    transformed.x /= (256);
    transformed.y /= -(128);

    // hack
    transformed.xyz *= transformed.w;

    gl_Position = transformed;




    // THIS works but has z that's incompatible with others
    /*
    // first, multiply with the camera matrix.
    vec4 transformed = -camera[3].xyzw;
    transformed += -camera[0] * position_in.x;
    transformed += -camera[1] * position_in.y;
    transformed += -camera[2] * position_in.z;

    float Q = fog_constant / transformed[3];

    // openGL will divide by pos[3]. We don't want to "fake" this otherwise clipping doesn't work.
    // so, we leave out the divide here.
    //transformed.xyz *= Q;
    transformed.xyz *= fog_constant;

    transformed.x /= (512);
    transformed.y /= -(256);
    transformed.z /= 16777216;

    transformed.w /= 2.;
    gl_Position = transformed;
    */





    //vec4 transformed = camera * vec4(position_in, 128.) + hvdf_offset;

//    // mulaw.xyzw ACC, vf09, vf00
//    vec4 transformed = camera[3].xyzw;
//
//    // maddax.xyzw ACC, vf04, vf12
//    transformed += camera[0] * position_in.x;
//    transformed += camera[1] * position_in.y;
//    transformed += camera[2] * position_in.z;
//    transformed[3] /= 4 * 1024.f;
//
//    //    float Q = fog_constant / transformed[3];
//    //    transformed.xyz *= Q;
//
//    transformed += hvdf_offset * transformed[3];
//    // then we'd do
//    // ftoi (* 16)
//    // then << 16 (* 65536)
//    // then divide by UINT32_MAX
//    // then * 16 on the GPU
//    transformed.xy *= (1.f / (128.f * 2048.f));
//    transformed.xy -= 1.f;
//    transformed.z *= -(1.f / (1024.f * 2048.f * 64.f * 64.f));
//
//    //gl_Position = vec4(transformed.xyz, 1.); // todo, maybe divide z by 16
//    gl_Position = transformed;


    //fragment_color = vec4(rgba_in.x, rgba_in.y, rgba_in.z, rgba_in.w * 2.);
//    fragment_color = vec4(0.5, 0.5, 0.5, 1.);
    fragment_color = texture(tex_T1, time_of_day_index / 8192.f);
    fragment_color.w *= 2;
    tex_coord = tex_coord_in;
}