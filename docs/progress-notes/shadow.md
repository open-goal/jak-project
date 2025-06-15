# Shadow Renderer

The shadow renderer works by darkening the intersection between the "shadow volume" and the world. There's a clever trick sometimes called "Carmack's Reverse" to accomplish this, but it requires drawing the "shadow volume".

The game builds the shadow volume mesh in `shadow-cpu.gc`, then submits it to a VU1 renderer. This part is a MIPS2C mess and we want to redo it in C++.

## Drawing Procedure Jak 1

## Setup

The shadow is not drawn is `disable-draw` flag is set.

The `center` stored in `shadow-settings` and `shadow-dcache` have different meanings.

The `center` in `shadow-settings` is set from `draw-bones-shadow`, which is a joint point. The `center` in `shadow-dcache` is

```
dcache.center = settings.center + settings.dir * settings.dist-to-locus
```

There are both top and bottom clipping planes. If the `shdf02` flag is set, the planes in `settings` are treated as global. Otherwise, they are treated as "relative". However, computing the final plane assumes the planes have a y normal:

```
(set! (-> dcache plane w) (- (-> settings bot-plane w) (-> settings center y)))
```

If `shdf00` is set, the shadow is discarded if the camera is below the plane:
```
          (let ((v1-16 (camera-pos)))
            (if (< (+ (* (-> v1-16 x) (-> dcache plane x))
                      (* (-> v1-16 y) (-> dcache plane y))
                      (* (-> v1-16 z) (-> dcache plane z))
                      (-> dcache plane w)
                      )
                   0.0
                   )
                (set! s1-0 #t)
                )
            )
```

The shadow plane is adjusted (again assuming it's +y normal) to make sure the shadow center is inside the volume:
```
          (let ((f0-25 (+ (* (-> dcache center x) (-> dcache plane x))
                          (* (-> dcache center y) (-> dcache plane y))
                          (* (-> dcache center z) (-> dcache plane z))
                          )
                       )
                )
            (if (< 0.0 (+ f0-25 (-> dcache plane w)))
                (set! (-> dcache plane w) (- f0-25))
                )
            )
```

Final setup of dcache:
```
          (set! (-> dcache light-dir quad) (-> settings shadow-dir quad))
          (set! (-> dcache near-plane x) 0.0)
          (set! (-> dcache near-plane y) 0.0)
          (set! (-> dcache near-plane z) 1.0)
          (set! (-> dcache near-plane w) (* -2.0 (-> *math-camera* d)))
          (set! (-> dcache dcache-top) (the-as uint (-> dcache data)))
```

## Stages

The stages are:

- `xform-verts` transform mesh vertices into camera space (no perspective)
- `init-vars` transform settings to camera space
- `calc-dual-verts` project vertices to plane
- `scissor-top` (only executed if shdf03 is set), clip vertices to top plane, if above
- `scissor-edges`, clip vertices to near plane
- `find-facing-single-tris`, set face bit to indicate orientation, cull backward ones
- `find-single-edges`, find edges that, when extruded, should be drawn
- `find-facing-double-tris`, set face bit indicate orientation. double sided tris, so no culling
- `find-double-edges`, find edges to extrude from the double-sided tris
- `add-verts`
- `add-facing-single-tris`
- `add-single-edges`
- `add-double-tris`
- `add-double-edges`


## Transform Verts

this needs access to only the `num-joints` in the header bone matrices.

```asm
L98:
    lw v1, 0(a0)         ;; v1 = qwc-data
    lw a2, 20(a0)        ;; a2 = ofs-verts
    dsll v1, v1, 4       ;; v1 = 16 * qwc-data
    lw t0, 24(a0)        ;; t0 = ofs-refs
    daddu a2, a2, a0     ;; a2 = verts-in-ptr
    lh a3, 8(a0)         ;; a3 = num-verts
    daddu t0, t0, a0     ;; t0 = refs-ptr
    lw t1, 4(a0)         ;; t1 = num-joints
    daddu v1, a0, v1     ;; v1 = dest-start??
    sw a2, 0(a1)         ;; store vtx-table in shadow-dcache
    daddiu v1, v1, 144   ;; v1 = dest-start + 144...
    or a1, t0, r0        ;; a1 = refs-ptr
    lh t0, 10(a0)        ;; t0 = num-twos
    or a2, a2, r0        ;; no effect
    dsubu a3, a3, t0     ;; a3 = num-verts - num-twos
    lui t0, 28672
    ori t0, t0, 2608     ;; 0xa30 offset in spad
    beq a3, r0, L100

;; transform ones
B1:
L99:
    daddiu a3, a3, -1  ;; decrement num-ones counter
    lbu t0, 0(a1)      ;; t0 = ref[0]
    lbu t1, 1(a1)      ;; t1 = ref[1] 
    daddiu a1, a1, 2   ;; increment ref
    dsll t0, t0, 7     ;; t0 = mat0-idx * 128
    daddu t0, t0, v1   ;; t0 = matrix pointer
    lqc2 vf1, 0(t0)    ;; load transformation matrix!
    lqc2 vf2, 16(t0)
    lqc2 vf3, 32(t0)
    lqc2 vf4, 48(t0)
    lqc2 vf9, 0(a2)   ;; load vertex
    vmulaw.xyzw acc, vf4, vf0 ;; transform!!
    vmaddax.xyzw acc, vf1, vf9
    vmadday.xyzw acc, vf2, vf9
    vmaddz.xyz vf9, vf3, vf9
    sqc2 vf9, 0(a2)   ;; store!
    daddiu a2, a2, 16
    bne a3, r0, L99

B2:
L100:
    lh a0, 10(a0)    ;; num-twos
    beq a0, r0, L102
    sll r0, r0, 0

B3:
L101:
    daddiu a0, a0, -1  ;; decrement remaining count
    lbu t0, 0(a1)      ;; load mat0
    lbu a3, 1(a1)      ;; load mat1
    dsll t0, t0, 7     ;; mat0_idx * 128
    daddiu a1, a1, 2   ;; increment refs ptr
    dsll a3, a3, 7     ;; mat0_idx * 128
    daddu t0, t0, v1   ;; t0 = mat0_ptr
    daddu a3, a3, v1   ;; a3 = mat1_ptr
    lqc2 vf1, 0(t0)    ;; load mat0
    lqc2 vf2, 16(t0)
    lqc2 vf3, 32(t0)
    lqc2 vf4, 48(t0)
    lqc2 vf9, 0(a2)   ;; load vertex
    lqc2 vf5, 0(a3)   ;; load mat1
    lqc2 vf6, 16(a3)
    lqc2 vf7, 32(a3)
    lqc2 vf8, 48(a3)
    vsubw.w vf10, vf0, vf9      ;; vf10.w = 1 - vertex.w
    vmulaw.xyzw acc, vf4, vf0   ;; xform 0 to vf10.xyz
    vmaddax.xyzw acc, vf1, vf9
    vmadday.xyzw acc, vf2, vf9
    vmaddz.xyz vf10, vf3, vf9  

    vmulaw.xyzw acc, vf8, vf0 ;; xform 1 to vf9.xyz
    vmaddax.xyzw acc, vf5, vf9
    vmadday.xyzw acc, vf6, vf9
    vmaddz.xyz vf9, vf7, vf9

    vmulaw.xyz acc, vf10, vf9 ;; combine
    vmaddw.xyz vf9, vf9, vf10
    vaddx.w vf9, vf0, vf0     ;; make sure w = 1.

    sqc2 vf9, 0(a2)
    daddiu a2, a2, 16
    bne a0, r0, L101
    sll r0, r0, 0

B4:
    sll r0, r0, 0
    sll r0, r0, 0
B5:
L102:
    or v0, r0, r0
    jr ra
    daddu sp, sp, r0
```

## Init Vars

This function just transforms light-dir, plane, top-plane, and center into the camera frame.
See details of transformation below.

- `vf7 = cam_rot[0]`
- `vf8 = cam_rot[1]`
- `vf9 = cam_rot[2]`
- `vf10 = cam_rot[3]`
- `vf1 = light-dir`
- `vf11 = plane`
- `vf12 = top-plane`
- `vf2 = center`

`vf1`, `vf11`, `vf12` (light-dir, both planes) are rotated by `cam-rot`
`vf2`:`center` is transformed by `cam-rat`

```
    lw v1, *math-camera*(s7)
    or v1, v1, r0
    lqc2 vf7, 364(v1)            
    lqc2 vf8, 380(v1)            
    lqc2 vf9, 396(v1)
    lqc2 vf10, 412(v1)
    lqc2 vf1, 128(a1)
    lqc2 vf11, 80(a1)
    lqc2 vf12, 96(a1)
    lqc2 vf2, 64(a1)

    vmulax.xyzw acc, vf7, vf1    ;; rotate light-dir
    vmadday.xyzw acc, vf8, vf1
    vmaddz.xyzw vf1, vf9, vf1

    vmulax.xyzw acc, vf7, vf11   ;; rotate plane
    vmadday.xyzw acc, vf8, vf11
    vmaddz.xyz vf11, vf9, vf11

    vmulax.xyzw acc, vf7, vf12  ;; rotate top-plane
    vmadday.xyzw acc, vf8, vf12
    vmaddz.xyz vf12, vf9, vf12

    vmul.xyzw vf13, vf10, vf11 ;; vf13 = dot(cam_pos, plane)

    vmulaw.xyzw acc, vf10, vf0 ;; acc = cam_pos
    vmaddax.xyzw acc, vf7, vf2 ;; acc = cam_pos + cam_rot_x*center

    vmul.xyzw vf14, vf10, vf12 ;; vf14 = dot(cam_pos, top-plane)

    vsubx.w vf13, vf13, vf13   ;; vf13 = dot(cam_pos, plane) - [0, 0, 0, cam.x*plane.x]
    vsubx.w vf14, vf14, vf14   ;; vf14 = dot(cam_pos, top-plane) - [0, 0, 0, cam.x*plane.x]

    vmadday.xyzw acc, vf8, vf2 ;; acc = cam_pos + cam_rot_x*center + cam_rot_y*center
    vmaddz.xyzw vf2, vf9, vf2

    vsuby.w vf13, vf13, vf13
    vsuby.w vf14, vf14, vf14
    vsubz.w vf11, vf13, vf13
    vsubz.w vf12, vf14, vf14
    sqc2 vf2, 64(a1)
    sqc2 vf1, 128(a1)
    sqc2 vf11, 80(a1)
    sqc2 vf12, 96(a1)
    or v0, r0, r0
    jr ra
    daddu sp, sp, r0
```

## Calc Dual Verts
This runs each vertex on program 28. It takes two cycles through the program!!
```
  nop                        |  mul.xyzw vf27, vf20, Q            N | V1-10
  div Q, vf13.x, vf17.x      |  sub.xyzw vf19, vf01, vf03      V2-9 | V0-0
  move.xyzw vf23, vf07       |  sub.xyzw vf20, vf01, vf04        ?? | V1-0
  nop                        |  sub.xyzw vf21, vf01, vf05         N | V2-0
  move.xyzw vf25, vf09       |  sub.xyzw vf22, vf01, vf06        ?? | V3-0
  move.xyzw vf26, vf10       |  sub.xyzw vf24, vf08, vf27        ?? | V1-11
  nop                        |  mul.xyzw vf11, vf03, vf02         N | V0-1
  nop                        |  mul.xyz vf15, vf19, vf02          N | V0-2
  div Q, vf14.x, vf18.x      |  mul.xyzw vf12, vf04, vf02      V3-9 | V1-1
  move.xyzw vf07, vf03       |  mul.xyzw vf28, vf28, Q         V0-3 | V2-10
  move.xyzw vf08, vf04       |  mul.xyz vf16, vf20, vf02       V1-3 | V1-2
  move.xyzw vf09, vf05       |  addy.x vf11, vf11, vf11        V2-3 | V0-4
  move.xyzw vf10, vf06       |  addy.x vf15, vf15, vf15        V3-3 | V0-5
  nop                        |  sub.xyzw vf25, vf25, vf28         N | V2-11
  nop                        |  addy.x vf12, vf12, vf12           N | V1-4
  nop                        |  mul.xyzw vf29, vf29, Q            N | V3-10
  nop                        |  addy.x vf16, vf16, vf16           N | V1-5
  nop                        |  addz.x vf11, vf11, vf11           N | V0-6
  nop                        |  addz.x vf15, vf15, vf15           N | V0-7
  nop                        |  sub.xyzw vf26, vf26, vf29         N | V3-11
  nop                        |  addz.x vf12, vf12, vf12           N | V1-6
  nop                        |  addz.x vf16, vf16, vf16           N | V1-7
  nop                        |  addw.x vf11, vf11, vf11           N | V9-8
  nop                        |  mul.xyzw vf13, vf09, vf02         N | V2-1
  nop                        |  addw.x vf12, vf12, vf12           N | V1-8
  nop                        |  mul.xyz vf17, vf21, vf02          N | V2-2
  nop                        |  mul.xyzw vf14, vf10, vf02         N | V3-1
  div Q, vf11.x, vf15.x      |  mul.xyz vf18, vf22, vf02       V0-9 | V3-2
  nop                        |  addy.x vf13, vf13, vf13           N | V2-4
  nop                        |  addy.x vf17, vf17, vf17           N | V2-5
  nop                        |  addy.x vf14, vf14, vf14           N | V3-4
  nop                        |  addy.x vf18, vf18, vf18           N | V3-5
  nop                        |  addz.x vf13, vf13, vf13           N | V2-6
  nop                        |  addz.x vf17, vf17, vf17           N | V2-7
  div Q, vf12.x, vf16.x      |  addz.x vf14, vf14, vf14        V1-9 | V3-6
  nop                        |  mul.xyzw vf19, vf19, Q            N | V0-10
  move.xyzw vf28, vf21       |  addz.x vf18, vf18, vf18           ~ | V3-7
  move.xyzw vf29, vf22       |  addw.x vf13, vf13, vf13           ~ | V2-8
  nop                        |  addw.x vf14, vf14, vf14 :e        N | V3-8
  nop                        |  sub.xyzw vf07, vf07, vf19         N | V0-11
```

`vf03`'s path:
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

This is projecting the vertex onto the plane!

```
L93:
    lw v1, 16(a1)      ;; v1 = dcache-top
    lw a2, 0(a1)       ;; a2 = vtx-table
    daddiu v1, v1, 15  ;; v1 = dcache-top + 15
    lqc2 vf1, 64(a1)   ;; vf1 = center
    dsra v1, v1, 4     ;; aligning dcache ptr
    lqc2 vf2, 80(a1)   ;; vf2 = plane
    dsll a3, v1, 4     ;; aligning dcache ptr
    lh a0, 8(a0)       ;; a0 = num-verts
    or v1, a3, r0      ;; v1 = dest-ptr
    sw a3, 44(a1)      ;; storing ptr-dual-verts
    or a2, a2, r0      ;; no effect
    beq a0, r0, L97
    sll r0, r0, 0

B1:
    lq a3, 0(a2)      ;; a3 = vtx0
    lq t0, 16(a2)     ;; t0 = vtx1
    lq t1, 32(a2)     ;; t1 = vtx2
    lq t2, 48(a2)     ;; t2 = vtx3
    daddiu a2, a2, 64 ;; inc vtx ptr
    qmtc2.i vf3, a3   ;; set vertex to vf3, vf4, vf5, vf6
    qmtc2.ni vf4, t0
    qmtc2.ni vf5, t1
    qmtc2.ni vf6, t2
    vcallms 28        ;; run program 28
    sll r0, r0, 0
    daddiu a0, a0, -4 ;; decrement vertex by 4.
    lq a3, 0(a2)      ;; start loading next
    blez a0, L95      ;; leftovers loop
    lq t0, 16(a2)

B2:
    lq t1, 32(a2)
    lq t2, 48(a2)
    daddiu a2, a2, 64
    qmtc2.i vf3, a3
    qmtc2.ni vf4, t0
    qmtc2.ni vf5, t1
    qmtc2.ni vf6, t2
B3:
L94:
    vcallms 28
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
    qmfc2.i a3, vf23
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sq a3, 0(v1)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    qmfc2.ni a3, vf24
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sq a3, 16(v1)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    qmfc2.ni a3, vf25
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sq a3, 32(v1)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    qmfc2.ni a3, vf26
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sq a3, 48(v1)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    lq a3, 0(a2)
    sll r0, r0, 0
    lq t0, 16(a2)
    sll r0, r0, 0
    lq t1, 32(a2)
    daddiu a0, a0, -4
    lq t2, 48(a2)
    daddiu a2, a2, 64
    daddiu v1, v1, 64
    sll r0, r0, 0
    qmtc2.ni vf3, a3
    sll r0, r0, 0
    qmtc2.ni vf4, t0
    sll r0, r0, 0
    qmtc2.ni vf5, t1
    bgtz a0, L94
    qmtc2.ni vf6, t2

B4:
L95:
    vcallms 68
    sll r0, r0, 0
    vnop
    sll r0, r0, 0
    daddiu a2, a0, 3
    qmfc2.i a3, vf23
    daddiu t0, a0, 2
    qmfc2.i t1, vf24
    daddiu t2, a0, 1
    qmfc2.i t3, vf25
    daddiu a0, a0, 4
    qmfc2.i t4, vf26
    beq a2, r0, L96
    sq a3, 0(v1)

B5:
    beq t0, r0, L96
    sq t1, 16(v1)

B6:
    beq t2, r0, L96
    sq t3, 32(v1)

B7:
    sll r0, r0, 0
    sq t4, 48(v1)
B8:
L96:
    dsll a0, a0, 4
    sll r0, r0, 0
    daddu v1, v1, a0
    sll r0, r0, 0
B9:
L97:
    sw v1, 16(a1)      ;; dcache top store
    or v0, r0, r0
    jr ra
    daddu sp, sp, r0

    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
```

## Scissor Top
If a vertex is past the top, it's projected to the top plane. It does so by moving along the direction of the bottom plane projection.

```
B0:
L83:
    lw a2, 44(a1)     ;; a2 = dual verts
    lw v1, 0(a1)      ;; a1 = normal verts
    lqc2 vf3, 96(a1)  ;; vf3 = top plane
    lh a0, 8(a0)      ;; a0 = num-verts
    or a1, a2, r0     ;; a1 = dual-verts
    or v1, v1, r0
    beq a0, r0, L86
    sll r0, r0, 0

B1:
L84:
    lqc2 vf1, 0(v1)            ;; vf1 = vert
    lqc2 vf2, 0(a1)            ;; vf2 = dual vert
    vsub.xyzw vf4, vf2, vf1    ;; vf4 = dual - orig
    vmul.xyzw vf5, vf1, vf3    ;; dot4(vert, top_plane)
    vmul.xyz vf6, vf4, vf3     ;; dot3(dual-orig, top_plane)
    vaddx.y vf5, vf5, vf5      ;; adds for dots
    vaddy.x vf6, vf6, vf6
    vaddz.y vf5, vf5, vf5
    vaddz.x vf6, vf6, vf6
    vaddw.y vf5, vf5, vf5
    qmfc2.i a2, vf5           ;; checking the dot4 to see which side of top plane we're on
    bltz a2, L85
    sll r0, r0, 0

B2:
    vdiv Q, vf5.y, vf6.x      ;; we're past the top plane, need to project original vertex.
    vwaitq
    vmulq.xyzw vf4, vf4, Q
    vsub.xyzw vf1, vf1, vf4
    sqc2 vf1, 0(v1)
B3:
L85:
    daddiu v1, v1, 16
    daddiu a1, a1, 16
    daddiu a0, a0, -1
    bne a0, r0, L84
    sll r0, r0, 0

B4:
L86:
    or v0, r0, r0
    jr ra
    daddu sp, sp, r0
```

## Scissor Edges

This function is to prevent the shadow edges from going through the camera near plane.
```
L87:
    lw a3, 44(a1)       ;; a3 = dual-verts
    lw a2, 0(a1)        ;; a2 = verts
    lqc2 vf3, 112(a1)   ;; vf3 = near plane
    lh v1, 8(a0)        ;; v1 = num-verts
    or a0, a3, r0       ;; a0 = duals
    or a1, a2, r0       ;; a1 = verts
    beq v1, r0, L92     
    sll r0, r0, 0

B1:
L88:
    lqc2 vf1, 0(a1)           ;; vf1 = vert
    lqc2 vf2, 0(a0)           ;; vf2 = dual vert
    vaddw.z vf7, vf1, vf3     ;; vf7.z = vert.z + near_plane.w
    vaddw.z vf8, vf2, vf3     ;; vf8.z = dual.z + near_plane.w
    vsubz.z vf6, vf1, vf2     ;; vf6.z = vert.z - dual.z
    vaddw.z vf5, vf1, vf3     ;; vf5.z = vert.z + near_plane.w (?? again)

    vaddz.y vf7, vf0, vf7     ;; vf7.y = vert.z + near_plane.w
    vaddz.y vf8, vf0, vf8     ;; vf8.y = dual.z + near_plane.w
    vsub.xyz vf4, vf2, vf1    ;; vf4 = dual - vert
    qmfc2.i a2, vf7           ;; a2 = compare of vert
    qmfc2.i a3, vf8           ;; a3 = compare of dual
    bltz a2, L89
    sll r0, r0, 0

B2:
    bgtz a3, L91
    sll r0, r0, 0

B3:
    beq r0, r0, L90
    sll r0, r0, 0

B4:
L89:
    bltz a3, L91
    sll r0, r0, 0

B5:
    vdiv Q, vf5.z, vf6.z
    vwaitq
    vmulq.xyzw vf4, vf4, Q
    vnop
    vnop
    vnop
    vadd.xyzw vf1, vf1, vf4
    beq r0, r0, L91
    sqc2 vf1, 0(a1)

B6:
L90:
    vdiv Q, vf5.z, vf6.z
    vwaitq
    vmulq.xyzw vf4, vf4, Q
    vnop
    vnop
    vnop
    vadd.xyzw vf1, vf1, vf4
    beq r0, r0, L91
    sqc2 vf1, 0(a0)

B7:
L91:
    daddiu a1, a1, 16
    daddiu a0, a0, 16
    daddiu v1, v1, -1
    bne v1, r0, L88
    sll r0, r0, 0

B8:
L92:
    or v0, r0, r0
    jr ra
    daddu sp, sp, r0
```

## Find Facing Single Tris

```
    daddiu sp, sp, -64
    sd ra, 0(sp)
    sq s4, 16(sp)
    sq s5, 32(sp)
    sq gp, 48(sp)

    lw v1, 16(a1)     ;; dcache top (so we're writing something out!)
    lh t0, 12(a0)     ;; t0 = num-single-tris
    or a2, v1, r0
    lw a3, 28(a0)
    daddu a0, a3, a0
    or a3, a0, r0      ;; a3 = single tris
    lqc2 vf2, 64(a1)   ;; vf2 = center
    lqc2 vf1, 128(a1)  ;; vf1 = light-dir
    lqc2 vf11, 80(a1)  ;; vf11 = plane
    lw a0, 0(a1)       ;; a0 = vtx-ptr
    pextlw a0, a0, a0  ;; a0 = [vtx-ptr, vtx-ptr, vtx-ptr, vtx-ptr]
    pextlw a0, a0, a0
    daddiu t0, t0, -4  ;; 4 tris at a time I guess.
    addiu t1, r0, 1    ;; t1 = 1
    bltz t0, L78
    daddiu t0, t0, 4

B1:
    lq t3, 0(a3)
    pextub t2, r0, t3
    mfc1 r0, f31
    pextlb t3, r0, t3
    mfc1 r0, f31
    psllh t2, t2, 4
    mfc1 r0, f31
    psllh t4, t3, 4
    mfc1 r0, f31
    pextuh t3, r0, t4
    mfc1 r0, f31
    pextlh t4, r0, t4
    mfc1 r0, f31
    pextuh t7, r0, t2
    mfc1 r0, f31
    pextlh t5, r0, t2
    mfc1 r0, f31
    paddw t6, t4, a0
    mfc1 r0, f31
    pcpyud t4, t6, r0
    lq t2, 0(t6)
    paddw t8, t3, a0
    lq t3, 0(t4)
    pcpyud t9, t8, r0
    lq t4, 0(t8)
    dsra32 t6, t6, 0
    dsra32 t8, t8, 0
    paddw s5, t5, a0
    lq t5, 0(t9)
    pcpyud t9, s5, r0
    lq t6, 0(t6)
    paddw gp, t7, a0
    lq t7, 0(t8)
    pcpyud ra, gp, r0
    lq t8, 0(s5)
    dsra32 s5, s5, 0
    dsra32 s4, gp, 0
    lq s5, 0(s5)
    lq t9, 0(t9)
    lq gp, 0(gp)
    lq s4, 0(s4)
    lq ra, 0(ra)
    qmtc2.ni vf2, t2
    qmtc2.ni vf3, t6
    qmtc2.ni vf4, t3
    qmtc2.ni vf7, t4
    qmtc2.ni vf8, t7
    qmtc2.ni vf9, t5
    qmtc2.ni vf12, t8
    qmtc2.ni vf13, s5
    qmtc2.ni vf14, t9
    qmtc2.ni vf17, gp
    qmtc2.ni vf18, s4
    qmtc2.ni vf19, ra
B2:
L73:
    lq t3, 16(a3)
    daddiu t0, t0, -4
    vcallms 0
    pextub t2, r0, t3
    mfc1 r0, f31
    pextlb t3, r0, t3
    mfc1 r0, f31
    psllh t2, t2, 4
    mfc1 r0, f31
    psllh t4, t3, 4
    mfc1 r0, f31
    pextuh t3, r0, t4
    mfc1 r0, f31
    pextlh t4, r0, t4
    mfc1 r0, f31
    pextuh t7, r0, t2
    mfc1 r0, f31
    pextlh t5, r0, t2
    mfc1 r0, f31
    paddw t6, t4, a0
    mfc1 r0, f31
    pcpyud t4, t6, r0
    lq t2, 0(t6)
    paddw t8, t3, a0
    lq t3, 0(t4)
    pcpyud t9, t8, r0
    lq t4, 0(t8)
    dsra32 t6, t6, 0
    dsra32 t8, t8, 0
    paddw s5, t5, a0
    lq t5, 0(t9)
    pcpyud t9, s5, r0
    lq t6, 0(t6)
    paddw gp, t7, a0
    lq t7, 0(t8)
    pcpyud ra, gp, r0
    lq t8, 0(s5)
    dsra32 s5, s5, 0
    dsra32 s4, gp, 0
    lq s5, 0(s5)
    lq t9, 0(t9)
    lq gp, 0(gp)
    lq s4, 0(s4)
    lq ra, 0(ra)
    qmtc2.ni vf2, t2
    qmtc2.ni vf3, t6
    qmtc2.ni vf4, t3
    qmtc2.ni vf7, t4
    qmtc2.ni vf8, t7
    qmtc2.ni vf9, t5
    qmtc2.ni vf12, t8
    qmtc2.ni vf13, s5
    qmtc2.ni vf14, t9
    qmtc2.ni vf17, gp
    qmtc2.ni vf18, s4
    qmtc2.ni vf19, ra
    qmfc2.ni t3, vf22
    qmfc2.ni t4, vf23
    qmfc2.ni t2, vf24
    bgez t3, L74
    qmfc2.ni t3, vf25

B3:
    sb t1, 3(a3)
    sw a3, 0(a2)
    daddiu a2, a2, 4
B4:
L74:
    bgez t4, L75
    daddiu a3, a3, 4

B5:
    sb t1, 3(a3)
    sw a3, 0(a2)
    daddiu a2, a2, 4
B6:
L75:
    bgez t2, L76
    daddiu a3, a3, 4

B7:
    sb t1, 3(a3)
    sw a3, 0(a2)
    daddiu a2, a2, 4
B8:
L76:
    bgez t3, L77
    daddiu a3, a3, 4

B9:
    sb t1, 3(a3)
    sw a3, 0(a2)
    daddiu a2, a2, 4
B10:
L77:
    bgtz t0, L73
    daddiu a3, a3, 4

B11:
L78:
    blez t0, L81
    sll r0, r0, 0

B12:
L79:
    lbu t2, 0(a3)   ;; t2 = ind-0
    lbu t3, 1(a3)   ;; t3 = ind-1
    lbu t1, 2(a3)   ;; t1 = ind-2
    dsll t2, t2, 4  ;; multiply by 16
    dsll t3, t3, 4
    dsll t1, t1, 4
    daddu t2, t2, a0  ;; offset, get original vertex
    daddu t3, t3, a0
    daddu t1, t1, a0
    lqc2 vf2, 0(t2)
    lqc2 vf3, 0(t3)
    lqc2 vf4, 0(t1)
    vsub.xyzw vf5, vf3, vf2
    vsub.xyzw vf6, vf4, vf2
    vopmula.xyz acc, vf5, vf6
    vopmsub.xyz vf5, vf6, vf5 ;; vf5 is the normal
    vmul.xyz vf5, vf5, vf1 ;; dot with the light-dir
    vaddx.y vf5, vf5, vf5
    vaddz.y vf5, vf5, vf5
    qmfc2.i t1, vf5
    sll r0, r0, 0
    bgez t1, L80
    addiu t1, r0, 1

B13:
    sw a3, 0(a2)      ;; output this triangle (as a pointer to the shadow-tri)
    daddiu a2, a2, 4
    sb t1, 3(a3)      ;; store a faces = 1 in the tri itself.
B14:
L80:
    daddiu t0, t0, -1
    bne t0, r0, L79
    daddiu a3, a3, 4

B15:
L81:
    dsubu a0, a2, v1
    dsra a0, a0, 2
    sw a0, 20(a1) ;; num facing-single-tris
    sw v1, 32(a1) ;; single tri list
    sw a2, 16(a1) ;; dcache top
    or v0, r0, r0
    ld ra, 0(sp)
    lq gp, 48(sp)
    lq s5, 32(sp)
    lq s4, 16(sp)
    jr ra
    daddiu sp, sp, 64
```

## Find Single Edges

```
L66:
    lw a2, 16(a1)    ;; top
    lh a3, 14(a0)    ;; a3 = num-single-edges
    or v1, a2, r0    ;; v1 = dcache top
    lw t0, 32(a0)    ;; t0 = ofs-single-edges
    beq a3, r0, L71  ;; exit if none
    lw t1, 28(a0)    ;; t1 = ofs-single-tris

B1:
    daddu t0, t0, a0  ;; t0 = single edge table
    sw a2, 36(a1)     ;; set single-edge-list
    daddu a0, t1, a0  ;; a0 = orig vertices
    sw t0, 4(a1)      ;; set single-edge-table
    or t1, t0, r0     ;; t1 = single edges
    addiu t2, r0, 255 ;; t2 = 255
    sll r0, r0, 0
B2:
L67:
    daddiu a3, a3, -1 ;; dec counter
    lbu t4, 3(t1)     ;; t4 = edge.tri-1
    sll r0, r0, 0
    lbu t5, 2(t1)     ;; t5 = edge.tri-0
    beq t4, t2, L68   ;; goto L68 if tri-1 is 255.
    or t3, r0, r0     ;; t3 = 0

B3:  ;; case where both tris are set.
    dsll t3, t5, 2    ;; t3 = tri-0
    dsll t4, t4, 2
    daddu t3, t3, a0
    daddu t5, t4, a0  ;; t5 = tri-1
    sll r0, r0, 0
    lbu t4, 3(t3)     ;; t4 = tri-0.faces
    sll r0, r0, 0
    lbu t5, 3(t5)     ;; t5 = tri-1.faces
    sltiu t3, t4, 1   ;; t3 = tri-0.faces < 1
    sll r0, r0, 0
    beq t4, t5, L70   ;; if facing is equal skip this.
    sll r0, r0, 0

B4:
    beq r0, r0, L69
    sll r0, r0, 0

B5:
L68:  ;; case where tri 1 is 255
    dsll t4, t5, 2 ;; t4 = tri-0
    sll r0, r0, 0
    daddu t4, t4, a0 ;; t4 = tri0
    sll r0, r0, 0
    sll r0, r0, 0
    lbu t4, 3(t4)    ;; t4 = tri-0.faces:
    beq t4, r0, L70  ;; if facing isn't set, skip this.
    sll r0, r0, 0

B6:
L69:
    dsubu t4, t1, t0 ;; t4 = edge idx
    sh t3, 2(v1)     ;; store (0, or, tri0.faces < 1)
    sh t4, 0(v1)     ;; store the edge idx.
    daddiu v1, v1, 4
B7:
L70:
    bne a3, r0, L67
    daddiu t1, t1, 4

B8:
L71:
    dsubu a0, v1, a2
    dsra a0, a0, 2
    sw a0, 24(a1)
    sw v1, 16(a1)
    or v0, r0, r0
    jr ra
    daddu sp, sp, r0
```

## Find Facing Double Tris

Same as single, but we don't build a list.

## Find Double Edges

```
    lw a2, 16(a1)
    lh a3, 18(a0) ;; num-double-edges
    or v1, a2, r0
    lw t1, 40(a0)
    beq a3, r0, L55
    lw t0, 12(a1)

B1:
    daddu a0, t1, a0
    sw a2, 40(a1)
    sw a0, 8(a1)
    or t1, a0, r0
    addiu t2, r0, 255
B2:
L52:
    daddiu a3, a3, -1
    lbu t3, 3(t1)
    sll r0, r0, 0
    lbu t4, 2(t1)
    beq t3, t2, L53
    or t5, r0, r0

B3:
    dsll t4, t4, 2
    dsll t3, t3, 2
    daddu t4, t4, t0
    daddu t3, t3, t0
    sll r0, r0, 0
    lbu t4, 3(t4)
    sll r0, r0, 0
    lbu t3, 3(t3)
    beq t4, t3, L54
    sll r0, r0, 0

B4:
    sltiu t4, t4, 1
    sll r0, r0, 0
    sltu t3, r0, t3
    sll r0, r0, 0
    sll r0, r0, 0
    sh t4, 2(v1)
    dsubu t4, t1, a0
    sh t3, 6(v1)
    sll r0, r0, 0
    sh t4, 0(v1)
    sll r0, r0, 0
    sh t4, 4(v1)
    beq r0, r0, L54
    daddiu v1, v1, 8

B5:
L53:
    dsll t3, t4, 2
    sll r0, r0, 0
    daddu t3, t3, t0
    sll r0, r0, 0
    sll r0, r0, 0
    lbu t3, 3(t3)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sltiu t3, t3, 1
    dsubu t4, t1, a0
    sh t3, 2(v1)
    sh t4, 0(v1)
    daddiu v1, v1, 4
B6:
L54:
    bne a3, r0, L52
    daddiu t1, t1, 4

B7:
L55:
    dsubu a0, v1, a2
    dsra a0, a0, 2
    sw a0, 28(a1)
    sw v1, 16(a1)
    or v0, r0, r0
    jr ra
    daddu sp, sp, r0

    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
```