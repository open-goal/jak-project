#version 410 core

out vec4 color;

uniform vec4 rgba;
uniform int enable_tex;
uniform int tcc;
uniform ivec4 channel_scramble;
uniform float alpha_multiply;
uniform float minimum;
uniform float maximum;

in vec2 uv;

uniform sampler2D tex_T0;

float cloud_lookup(float v, float minimum, float maximum) {
  maximum = max(minimum, maximum);
  if (v <= minimum) {
    return 0;
  }
  if (v >= maximum) {
    return 1;
  }

  float alpha = (v - minimum) / (maximum - minimum);
  float sin_alpha = sin(alpha * 3.1415926 / 2.f);
  return sin_alpha * sin_alpha;

//(defun make-cloud-clut ((arg0 (pointer uint32)) (arg1 float) (arg2 float))
//  "Generate clut for the cloud texture."
//  (local-vars (v1-10 int))
//  0
//  (let* ((minimum (the int (* 255.0 arg1)))
//         (maximum (max (the int (* 255.0 arg2)) minimum))
//         (diff (/ 128.0 (the float (- maximum minimum))))
//         )
//    (dotimes (s3-0 256)
//      (let ((s2-0 (-> *clut-translate* s3-0)))
//        (cond
//          ((and (>= minimum s3-0) (nonzero? maximum))
//           (set! v1-10 0)
//           )
//          ((>= s3-0 maximum)
//           (set! v1-10 128)
//           )
//          (else
//            (let* ((f0-9 (sin (* 128.0 (fmin 128.0 (* (the float (- s3-0 minimum)) diff)))))
//                   (f0-11 (* f0-9 f0-9))
//                   )
//              (set! v1-10 (the int (* 128.0 f0-11)))
//              )
//            )
//          )
//        (set! (-> arg0 s2-0) (logior (logand (-> arg0 s2-0) -256) 128))
//        (set! (-> arg0 s2-0) (logior (logand (-> arg0 s2-0) -65281) #x8000))
//        (set! (-> arg0 s2-0) (logior (logand (-> arg0 s2-0) -16711681) #x800000))
//        (set! (-> arg0 s2-0)
//              (logior (logand (-> arg0 s2-0) (the-as uint #xffffffff00ffffff)) (shr (shl v1-10 56) 32))
//              )
//        )
//      )
//    )
//  (none)
//  )
 
}

void main() {

  if (enable_tex == 1) {
    vec4 tex_color = texture(tex_T0, uv);
    vec4 unscambled_tex = vec4(tex_color[channel_scramble[0]],
    tex_color[channel_scramble[1]],
    tex_color[channel_scramble[2]],
    tex_color[channel_scramble[3]]);
    color = rgba / 128.;
    if (tcc == 1) {
      color *= unscambled_tex;
    } else {
      color.xyz *= unscambled_tex.xyz;
    }
  } else if (enable_tex == 2) {
    vec4 tex_color = texture(tex_T0, uv);
    color.x = 1;
    color.y = 1;
    color.z = 1;
    color.a = cloud_lookup(tex_color.r, minimum, maximum);
  } else {
    color = (rgba / 128.);
  }

  color.a *= alpha_multiply;
}