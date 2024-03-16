The `bones.gc` file computes skinning matrices for foreground rendering.

Arguments:
 - `a0`: output matrix area, each matrix is 7x quadwords contaning the vertex and normal transformation matrices.
 - `a1`: input joint array. The joints contain the inverse bind pose.
 - `a2`: input bones array. The world space bone transforms
 - `a3`: num bones

 - `vf28, vf29, vf30, vf31` the camera matrix
 - `vf25, vf26, vf27` the camera matrix again

```asm
    daddiu sp, sp, -96
    sd ra, 0(sp)
    sq s2, 16(sp)
    sq s3, 32(sp)
    sq s4, 48(sp)
    sq s5, 64(sp)
    sq gp, 80(sp)

    lui v1, 4096
    lui t0, 4096
    ori v1, v1, 54272   ;; v1 = DMA reg addr
    ori t0, t0, 53248   ;; t0 = DMA reg addr
    lui t2, 32767       ;; 0x7fff....
    daddiu t1, a3, -16  ;; bone count - 16 (maybe we do 16 bones at a time?)
    ori t2, t2, 65535   ;; 0x7fff'ffff
    lui at, 28672       ;; scratchpad addr
    addiu t4, r0, 64    ;; t4 = 64 (= 16 bones * 4)
    addiu t5, r0, 1280  ;; t5 = 1280 (= 16 bones * 80)
    bgez t1, L17        ;; more than 16 bones?
    addiu t3, r0, 16    ;; t3 = 16

   ;; if first run is under 16 bones, adjust counts
B1:
    or t3, a3, r0       ;; t3 = num bones
    sll r0, r0, 0
    dsll t4, t3, 2      ;; t4 = num bones * 4
    dsll a3, t3, 4      ;; a3 = num bones * 16
    dsll t1, t3, 6      ;; t1 = num bones * 64
    sll r0, r0, 0
    daddu t5, t1, a3    ;; t5 = num bones * 80
    addiu t1, r0, 0     ;; t1 = 0 (remaining bones count)
B2:
L17:
    addiu a3, r0, 0     ;; a3 = 0
    addiu t6, r0, 1     ;; t6 = 1
    and a1, a1, t2      ;; mask off upper bits of address (not sure why, but they do this sometimes)
    sll r0, r0, 0
    daddiu a1, a1, 12   ;; adjustment of joint pointer for the strided dma stuff.
    or a0, a0, r0
    daddiu a1, a1, -80
    sll r0, r0, 0

    ;; wait for DMA to be free...
    <snip>

B5:
L19:
    addiu t6, r0, 80
    addiu t7, r0, 264
    sw t6, 128(v1)   ;; addr in spad = 80 for joints
    sw a1, 16(v1)
    sw t4, 32(v1)    ;; size: num bones * 4
    sw t7, 0(v1)
    daddu a1, a1, t5
    ;; wait for dma to complete
    <snip>

B8:
L21:
    and a2, a2, t2  ;; clean up bones addr
    sll r0, r0, 0
    dsll t2, t3, 2    ;; t2 = bones * 4
    addiu t4, r0, 256 ;; t4 = 256
    daddu t2, t2, t3  ;; t2 = bones * 5 (size of the bone)
    addiu t6, r0, 1104  ;; addr in spad = 1104 for bones.
    dsll t5, t2, 4
    sw t6, 128(v1)
    addiu t8, r0, 0
    sw a2, 16(v1)
    daddu a2, a2, t5
    sw t2, 32(v1)
    addiu t2, r0, 1
    sw t4, 0(v1)
;; wait for dma
;; <snip>

B11:
L23:
    dsll t5, t8, 2     ;; ?? not sure what this is, but always zero?
    daddu t9, t5, at   ;; ptr to bone-work
    sll r0, r0, 0
    lwu t5, 0(t9)      ;; t5 = (-> bone-layout joint)
    or t6, t3, r0
    lwu t7, 8(t9)      ;; t7 = (-> bone-layout bone)
    or ra, t3, r0
    lwu t3, 16(t9)     ;; t3 = (-> bone-layout output)
    sll r0, r0, 0
    sw ra, 44(at)      ;; stash sp-size
    beq ra, r0, L36
    sw t8, 48(at)      ;; stash sp-bufnum

B12:
    daddiu t1, t1, -16   ;; decrement bones count
    addiu t9, r0, 1280   ;; next DMA math stuff
    bgez t1, L24         ;; check if partial bone buffer
    addiu t8, r0, 16     ;; ....

B13:
    daddiu t8, t1, 16
    addiu t1, r0, 0
    dsll t9, t8, 4
    dsll ra, t8, 6
    beq t8, r0, L25
    daddu t9, ra, t9

B14:
L24:
    dsll t4, t8, 2
    dsll ra, t2, 2
    daddu gp, ra, at
    sw a1, 16(v1)
    addiu ra, r0, 264
    lwu gp, 0(gp)
    andi gp, gp, 16383
    sw t4, 32(v1)
    daddu a1, a1, t9
    sw gp, 128(v1)
    addiu t4, r0, 0
    sw ra, 0(v1)

;; and now, for the actual bones.
B15:
L25:
    sll r0, r0, 0
    sw t8, 40(at)   ;; in-count
    sll r0, r0, 0
    lqc2 vf1, 0(t5)  ;; vf1, vf2, vf3, vf4 = inverse bind pose
    sll r0, r0, 0
    lqc2 vf2, 16(t5)
    sll r0, r0, 0
    lqc2 vf3, 32(t5)
    sll r0, r0, 0
    lqc2 vf4, 48(t5)
    sll r0, r0, 0
    lqc2 vf5, 0(t7)  ;; vf5, vf6, vf7, vf8 = input bone matrix.
    sll r0, r0, 0
    lqc2 vf6, 16(t7)
    sll r0, r0, 0
    lqc2 vf7, 32(t7)
    sll r0, r0, 0
    lqc2 vf8, 48(t7)
    vcallms 0         ;; run bone program
    sll r0, r0, 0
B16:
L26:
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu t5, t5, 64  ;; advance joint
    sll r0, r0, 0
    daddiu t7, t7, 80  ;; advance bone.
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    lq t8, 0(t5)   ;; load next joint
    sll r0, r0, 0
    lq t9, 16(t5)
    sll r0, r0, 0
    lq ra, 32(t5)
    sll r0, r0, 0
    lq gp, 48(t5)
    sll r0, r0, 0
    lq s5, 0(t7)   ;; load next bone
    sll r0, r0, 0
    lq s4, 16(t7)
    sll r0, r0, 0
    lq s3, 32(t7)
    sll r0, r0, 0
    lq s2, 48(t7)
    sll r0, r0, 0
    qmtc2.ni vf1, t8 ;; swap in new inputs
    sll r0, r0, 0
    qmtc2.ni vf2, t9
    sll r0, r0, 0
    qmtc2.ni vf3, ra
    sll r0, r0, 0
    qmtc2.ni vf4, gp
    sll r0, r0, 0
    qmtc2.ni vf5, s5
    sll r0, r0, 0
    qmtc2.ni vf6, s4
    sll r0, r0, 0
    qmtc2.ni vf7, s3
    sll r0, r0, 0
    qmtc2.ni vf8, s2
    sll r0, r0, 0
    qmfc2.i t8, vf13   ;; swap out result in (vf13, vf14, vf15, vf16) and (vf9, vf10, vf11)
    sll r0, r0, 0
    qmfc2.ni t9, vf14
    sll r0, r0, 0
    qmfc2.ni ra, vf15
    sll r0, r0, 0
    qmfc2.ni gp, vf16
    sll r0, r0, 0
    qmfc2.ni s5, vf9
    sll r0, r0, 0
    qmfc2.ni s4, vf10
    sll r0, r0, 0
    qmfc2.ni s3, vf11
    vcallms 0
    sll r0, r0, 0
    sll r0, r0, 0
    sq t8, 0(t3)
    sll r0, r0, 0
    sq t9, 16(t3)
    sll r0, r0, 0
    sq ra, 32(t3)
    sll r0, r0, 0
    sq gp, 48(t3)
    sll r0, r0, 0
    sq s5, 64(t3)
    sll r0, r0, 0
    sq s4, 80(t3)
    sll r0, r0, 0
    sq s3, 96(t3)
    sll r0, r0, 0
    sq r0, 112(t3)
    daddiu t3, t3, 128
    daddiu t6, t6, -1
    bgtz t6, L26
    sll r0, r0, 0

B17:
    sll r0, r0, 0
    lw t3, 40(at)
    beq t3, r0, L29
    sll r0, r0, 0

B18:
L27:
    lw t4, 0(v1)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t4, t4, 256
    sll r0, r0, 0
    beq t4, r0, L28
    sll r0, r0, 0

B19:
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    beq r0, r0, L27
    sll r0, r0, 0

B20:
L28:
    dsll t5, t2, 2
    sll r0, r0, 0
    addiu t4, r0, 1
    daddu t5, t5, at
    sll r0, r0, 0
    lwu t6, 8(t5)
    dsll t5, t3, 2
    andi t6, t6, 16383
    daddu t5, t5, t3
    sw t6, 128(v1)
    dsll t6, t5, 4
    sw a2, 16(v1)
    addiu t7, r0, 256
    sw t5, 32(v1)
    daddu a2, a2, t6
    sw t7, 0(v1)
B21:
L29:
    sll r0, r0, 0
    lw t5, 48(at)
    sll r0, r0, 0
    lw t6, 44(at)
B22:
L30:
    lw t7, 0(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t7, t7, 256
    sll r0, r0, 0
    beq t7, r0, L31
    sll r0, r0, 0

B23:
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    beq r0, r0, L30
    sll r0, r0, 0

B24:
L31:
    beq t6, r0, L32
    sll r0, r0, 0

B25:
    dsll t7, t5, 2
    lui t8, 28672
    daddu t7, t7, t8
    lwu t7, 16(t7)
    andi t7, t7, 16383
    sw t7, 128(t0)
    sw a0, 16(t0)
    dsll t7, t6, 3
    sw t7, 32(t0)
    addiu t7, r0, 256
    sw t7, 0(t0)
    dsll t6, t6, 7
    daddu a0, a0, t6
B26:
L32:
    beq t3, r0, L35
    sll r0, r0, 0

B27:
    bne t4, r0, L35
    sll r0, r0, 0

B28:
L33:
    lw t6, 0(v1)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 256
    sll r0, r0, 0
    beq t6, r0, L34
    sll r0, r0, 0

B29:
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    beq r0, r0, L33
    sll r0, r0, 0

B30:
L34:
    dsll t6, t2, 2
    lui t7, 28672
    daddu t6, t6, t7
    lwu t6, 8(t6)
    andi t6, t6, 16383
    sw t6, 128(v1)
    sw a2, 16(v1)
    addiu t6, r0, 5
    mult3 t6, t6, t3
    sw t6, 32(v1)
    addiu t6, r0, 256
    sw t6, 0(v1)
    addiu t6, r0, 80
    mult3 t6, t6, t3
    daddu a2, a2, t6
B31:
L35:
    or t8, t2, r0
    bne t1, r0, L22
    or t2, t5, r0

B32:
    beq a3, r0, L22
    addiu a3, r0, 1

B33:
L36:
    or v0, r0, r0
    ld ra, 0(sp)
    lq gp, 80(sp)
    lq s5, 64(sp)
    lq s4, 48(sp)
    lq s3, 32(sp)
    lq s2, 16(sp)
    jr ra
    daddiu sp, sp, 96
```


# VU0 micoprogram

- vf1, vf2, vf3, vf4 = inverse bind pose
- vf5, vf6, vf7, vf8 = input bone matrix.
- `vf28, vf29, vf30, vf31` the camera matrix
- `vf25, vf26, vf27` the camera matrix again
- (vf13, vf14, vf15, vf16) output point transformation
- (vf09, vf10, vf11) output normal transformation

```
First: multiply bone and bind pose: (vf13, vf14, vf15, vf16) = (vf5, vf6, vf7, vf8) * (vf1, vf2, vf3, vf4).
This is doing a true matrix multiplication.
  nop                        |  mulax.xyzw ACC, vf05, vf01
  nop                        |  madday.xyzw ACC, vf06, vf01
  nop                        |  maddaz.xyzw ACC, vf07, vf01
  nop                        |  maddw.xyzw vf13, vf08, vf01
  nop                        |  mulax.xyzw ACC, vf05, vf02
  nop                        |  madday.xyzw ACC, vf06, vf02
  nop                        |  maddaz.xyzw ACC, vf07, vf02
  nop                        |  maddw.xyzw vf14, vf08, vf02
  nop                        |  mulax.xyzw ACC, vf05, vf03
  nop                        |  madday.xyzw ACC, vf06, vf03
  nop                        |  maddaz.xyzw ACC, vf07, vf03
  nop                        |  maddw.xyzw vf15, vf08, vf03
  nop                        |  mulax.xyzw ACC, vf05, vf04
  nop                        |  madday.xyzw ACC, vf06, vf04
  nop                        |  maddaz.xyzw ACC, vf07, vf04
  nop                        |  maddw.xyzw vf16, vf08, vf04

;; vf09 = cross(y, z)
  nop                        |  opmula.xyz ACC, vf14, vf15
  nop                        |  opmsub.xyz vf09, vf15, vf14

;; vf10 = cross(z, x)
  nop                        |  opmula.xyz ACC, vf15, vf13
  nop                        |  opmsub.xyz vf10, vf13, vf15

;; vf11 = cross(x, y)
  nop                        |  opmula.xyz ACC, vf13, vf14
  nop                        |  opmsub.xyz vf11, vf14, vf13

 ;; vf12 = cross (y, z) * x
  nop                        |  mul.xyz vf12, vf13, vf09

 ;; second multiply: doing (vf13....) = cam * (vf5, vf6, vf7, vf8) * (vf1, vf2, vf3, vf4)
  nop                        |  mulax.xyzw ACC, vf28, vf13
  nop                        |  madday.xyzw ACC, vf29, vf13
  nop                        |  maddaz.xyzw ACC, vf30, vf13
  nop                        |  maddw.xyzw vf13, vf31, vf13

  nop                        |  mulax.w ACC, vf00, vf12
  nop                        |  madday.w ACC, vf00, vf12
  nop                        |  maddz.w vf12, vf00, vf12
  vf12.w = dot (cross(y, z), x)  [before the second multiply]

  nop                        |  mulax.xyzw ACC, vf28, vf14
  nop                        |  madday.xyzw ACC, vf29, vf14
  nop                        |  maddaz.xyzw ACC, vf30, vf14
  div Q, vf00.w, vf12.w      |  maddw.xyzw vf14, vf31, vf14  ;; divide
  nop                        |  mulax.xyzw ACC, vf28, vf15
  nop                        |  madday.xyzw ACC, vf29, vf15
  nop                        |  maddaz.xyzw ACC, vf30, vf15
  nop                        |  maddw.xyzw vf15, vf31, vf15
  nop                        |  mulax.xyzw ACC, vf28, vf16
  nop                        |  madday.xyzw ACC, vf29, vf16
  nop                        |  maddaz.xyzw ACC, vf30, vf16
  nop                        |  maddw.xyzw vf16, vf31, vf16

;; normal scale
  nop                        |  mul.xyzw vf09, vf09, Q
  nop                        |  mul.xyzw vf10, vf10, Q
  nop                        |  mul.xyzw vf11, vf11, Q

;; apply cam to normal matrix too
  nop                        |  mulax.xyzw ACC, vf25, vf09
  nop                        |  madday.xyzw ACC, vf26, vf09
  nop                        |  maddz.xyzw vf09, vf27, vf09
  nop                        |  mulax.xyzw ACC, vf25, vf10
  nop                        |  madday.xyzw ACC, vf26, vf10
  nop                        |  maddz.xyzw vf10, vf27, vf10
  nop                        |  mulax.xyzw ACC, vf25, vf11
  nop                        |  madday.xyzw ACC, vf26, vf11 :e
  nop                        |  maddz.xyzw vf11, vf27, vf11
```