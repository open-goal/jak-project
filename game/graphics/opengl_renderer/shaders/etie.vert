#version 410 core

layout (location = 0) in vec3 position_in;
layout (location = 1) in vec3 tex_coord_in;
layout (location = 2) in int time_of_day_index;
layout (location = 3) in vec3 normal;
layout (location = 4) in vec4 proto_tint;

uniform vec4 hvdf_offset;
uniform mat4 camera;
uniform float fog_constant;
uniform float fog_min;
uniform float fog_max;
uniform vec4 envmap_tod_tint;
uniform sampler1D tex_T10; // note, sampled in the vertex shader on purpose.
uniform int decal;

out vec4 fragment_color;
out vec3 tex_coord;
out float fogginess;

// etie stuff
uniform vec4 persp0;
uniform vec4 persp1;
uniform mat4 cam_no_persp;

void main() {
  fogginess = 0;

  // rotate the normal
  vec3 nrm_vf23 = cam_no_persp[0].xyz * normal.x
                + cam_no_persp[1].xyz * normal.y
                + cam_no_persp[2].xyz * normal.z;
  vec3 r_nrm = nrm_vf23;

  // transform the point
  vec4 vf17 = cam_no_persp[3];
  vf17 += cam_no_persp[0] * position_in.x;
  vf17 += cam_no_persp[1] * position_in.y;
  vf17 += cam_no_persp[2] * position_in.z;


  // This is the ETIE math.
  // It seems right only if the nrm_vf23 is normalized first
  // (and in this case it's identical the emerc math below)
  // There is no reason to prefer the emerc version over etie - they are identical.
  // likely on PS2, their normal transformation matrix was scaled correctly.
  // ours isn't. (yes, the camera matrix is a pure rotation and that doesn't matter, but the
  // instance matrix used to de-instance the mesh needs the correction, and we don't have it)
  {
    nrm_vf23 = nrm_vf23;
    // nrm.z -= 1
    //subw.z vf23, vf23, vf00
    nrm_vf23.z -= 1.f;

    // dot = nrm.xyz * pt.xyz
    //mul.xyz vf13, vf17, vf23
    //esum.xyzw P, vf13
    //mfp.x vf13, P
    float nrm_dot = dot(vf17.xyz, nrm_vf23);

    // rfl = pt.xzy * nrm.z
    //mulz.xyz vf14, vf17, vf23
    vec3 rfl_vf14 = vf17.xyz * nrm_vf23.z;

    //;; Q_envmap = vf02.w / norm(rfl.xyz)
    //esadd.xyz P, vf14
    //mfp.x vf30, P
    //rsqrt Q, vf02.w, vf30.x
    float Q_envmap = -0.5 / length(rfl_vf14);

    //
    //;; nrm.xy *= dot.x
    //mulx.xy vf23, vf23, vf13
    nrm_vf23.xy *= nrm_dot;

    //
    //;; nrm.xy += rfl.xy
    //add.xy vf23, vf23, vf14
    nrm_vf23.xy += rfl_vf14.xy;
    //
    //;; nrm.z = 1.0
    //addw.z vf23, vf00, vf00
    nrm_vf23.z = 1.0;
    //
    //;; nrm.xy *= Q_envmap
    //mul.xy vf23, vf23, Q
    nrm_vf23.xy *= Q_envmap;
    //

    //
    //;; nrm.xy += vf03.w
    //addw.xy vf23, vf23, vf03
    nrm_vf23.xy += 0.5;
    tex_coord = nrm_vf23;
  }

  //;; perspective transform
  //mula.xy ACC, vf10, vf17  ;; acc build 1
  //mulaw.zw ACC, vf10, vf00 ;; acc build 2
  vec4 p_proj = vec4(persp1.x * vf17.x, persp1.y * vf17.y, persp1.z, persp1.w);

  //maddz.xyzw vf18, vf09, vf17 ;; acc star
  p_proj += persp0 * vf17.z;

  //;; perspective divide
  //div Q, vf00.w, vf18.w
  float pQ = 1.f / p_proj.w;

  // EMERC version of the math
//  {
//    // emerc hack
//    vec3 vf10 = normalize(r_nrm);
//    //subw.z vf10, vf10, vf00 ;; subtract 1 from z
//    vf10.z -= 1;
//    //addw.z vf09, vf00, vf09 ;; xyww the unperspected thing
//    vec4 vf09 = vf17;
//    //mul.xyz vf15, vf09, vf10 ;;
//    vec3 vf15 = vf09.xyz * vf10.xyz;
//    //adday.xyzw vf15, vf15
//    //maddz.x vf15, vf21, vf15
//    float vf15_x = vf15.x + vf15.y + vf15.z;
//    //div Q, vf15.x, vf10.z
//    float qq = vf15_x / vf10.z;
//    //mulaw.xyzw ACC, vf09, vf00
//    vec3 ACC = vf09.xyz;
//    //madd.xyzw vf10, vf10, Q
//    vf10 = ACC + vf10 * qq;
//    //eleng.xyz P, vf10
//    float P = length(vf10.xyz);
//    //mfp.w vf10, P
//    //div Q, vf23.z, vf10.w
//    float qqq = 0.5 / P;
//    //addaz.xyzw vf00, vf23
//    ACC = vec3(0.5, 0.5, 0.5);
//    //madd.xyzw vf10, vf10, Q
//    vf10 = ACC + vf10 * qqq;
//    tex_coord = vf10.xyz;
//  }


  vec4 transformed = p_proj * pQ;
  transformed.w = p_proj.w;

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
  // scissoring area adjust
  transformed.y *= SCISSOR_ADJUST * HEIGHT_SCALE;
  gl_Position = transformed;


  fragment_color = proto_tint * envmap_tod_tint;
}
