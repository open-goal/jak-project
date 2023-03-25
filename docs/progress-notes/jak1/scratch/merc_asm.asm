;; Register use (after init and effect setup)
;; to be extra annoying, they often stash stuff in
;; w (and sometimes z) and use xyz for other stuff.

;;  vf01 = [lt0_dir.xyz, pfog0]
;;  vf02 = [lt1_dir.xyz, fog-min]
;;  vf03 = [lt2_dir.xyz, fog-max]
;;  vf04 = [lt0_color]
;;  vf05 = [lt1_color]
;;  vf06 = [lt2_color]
;;  vf07 = [lt_ambient]
;;  vf17 = [2048, 255, -65537, xyz-add.x]
;;  vf18 = [st-out-X, st-out-X, -65537, xyz-add.y] (X = a if xtop = 0, X = b otherwise)
;;  vf19 = [st-magic, st-magic, -65537, xyz-add.z]
;;  vf22 = hvdf-offset (does get set to others, but is always restored)
;;  vf29 = [??, ??, ??, 0.003921569]


;; Low Memory use (after init and effect setup)
;;  1 - [adgif_giftag, use_mercprime]
;;  2 - hvdf-offset
;;  3 - [p0x, p1y, p2z, p2w] (note p2w, not p3w here!)
;;  4 - [p3x, p3y, p3z, p3w]
;;  5 - [p0x*sc, p1y*sc, p2z*sc, p2w*sc] (where sc is the xyz-scale of the header)

;; 132 - not double buffered, lights + merc-ctrl-header (per ctrl)
;;  132
;;  133 light 1 dir, w has flags (0x44 for ignore alpha, )
;;  134 light 2 dir, w has fade out parameter (how much to fade out)
;; 139 - [xyz_scale, st-magic, st-out-a, st-out-b]
;; 140 - not double buffered, header (per effect)

;; xtop + 140 + st-out
;;  this contains the uploaded fp data.
;;  the first


;; initialization - this runs after the merc program is uploaded and the vu1 low memory
;; block is uploaded. It sets constant registers and stashes some stuff in memory.

;; - set vf01.w to pfog0
  lq.xyzw vf01, 7(vi00)      |  nop  ;; vf01 = fog
  lq.xyzw vf25, 3(vi00)      |  nop  ;; vf25 = perspective.row0
  lq.xyzw vf26, 4(vi00)      |  nop  ;; vf26 = perspective.row1
  lq.xyzw vf27, 5(vi00)      |  nop  ;; vf27 = perspective.row2
  lq.xyzw vf28, 6(vi00)      |  nop  ;; vf28 = perspective.row3
  mr32.xyzw vf01, vf01       |  nop  ;; rotate pfog0 into vf01.w
  move.y vf25, vf26          |  nop  ;; setting up vf25
  move.zw vf25, vf27         |  nop  ;; same
  sq.xyzw vf25, 3(vi00)      |  nop  ;; setting up low 3.
  2048.0                     |  nop :i ;; setup I
  255.0                      |  maxi.x vf17, vf00, I :i ;; setup vf17
  -65537.0                   |  maxi.y vf17, vf00, I :i ;; setup vf17
  mr32.xyzw vf02, vf01       |  minii.z vf17, vf00, I   ;; vf17, setup vf02.w
  lq.xyzw vf22, 2(vi00)      |  minii.z vf18, vf00, I   ;; vf18.z, vf22 as hvdf-offset
  0.003921569                |  minii.z vf19, vf00, I :i ;; vf19.z
  sq.xyzw vf28, 4(vi00)      |  minii.w vf29, vf00, I :e ;; setup low 4
  mr32.xyzw vf03, vf02       |  nop ;; set up vf03

;; the 17/20 entry points are the same as 32/35, but should be used
;; on the first frag of an effect. they set up lights.

;; enter 17 (setup lights, with merc "prime")
  iaddi vi07, vi00, 0x1      |  nop    ;; set vi07 mercprime
  b L2                       |  nop
  isw.w vi07, 1(vi00)        |  nop    ;; leave behind mercprime flag.
ENTER_20: ;; (setup lights, no merc prime)
  iaddi vi07, vi00, 0x0      |  nop    ;; no mercprime for us today.
L2:       ;; (setup lights common)
  lq.xyzw vf25, 139(vi00)    |  nop    ;; vf25 = merc-ctrl-header
  lq.xyzw vf26, 3(vi00)      |  nop    ;; vf26 = persp-diag
  lq.xyz vf01, 132(vi00)     |  nop    ;; setup vf01 light 0 dir
  lq.xyz vf02, 133(vi00)     |  nop    ;; setup vf02 light 1 dir
  lq.xyz vf03, 134(vi00)     |  addy.xy vf19, vf00, vf25 ;; vf03 light setup, st-magic
  lq.xyzw vf04, 135(vi00)    |  mulx.xyzw vf26, vf26, vf25 ;; scaled persp diag
  lq.xyzw vf05, 136(vi00)    |  nop
  lq.xyzw vf06, 137(vi00)    |  nop
  lq.xyzw vf07, 138(vi00)    |  nop
  b L5                       |  nop
  sq.xyzw vf26, 5(vi00)      |  nop ;; stash persp diag scaled

;; 32/35 are prime/normal entry points for normal drawing.
ENTER_32: ;; (run, use prev lights, with merc "prime")
  iaddi vi07, vi00, 0x1      |  nop ;; vi07 for mercprime
  b L5                       |  nop
  isw.w vi07, 1(vi00)        |  nop ;; stash mercprime flag
ENTER_35:
  iaddi vi07, vi00, 0x0      |  nop

L5: ;; common effect drawing entry point.
;; all draw paths go to here.
  lq.xyzw vf28, 139(vi00)    |  minix.xyzw vf15, vf00, vf00 ;; vf28 = mch, vf15 = [0, 0, 0, 0]
  xtop vi15                  |  nop                         ;; vi15 = 0 (output buffer)
  iaddiu vi12, vi15, 0x8c    |  nop                         ;; vi12 = xtop + 140 (merc-byte-header, u4)
  ibeq vi00, vi15, L6        |  nop                         ;; branch if output buffer a (0)
  ilwr.w vi03, vi12          |  maxz.xy vf18, vf00, vf28    ;; set vf18.xy = [st-out-a, st-out-a] (for a buffer)
  nop                        |  maxw.xy vf18, vf00, vf28    ;; set vf18.xy = [st-out-b, st-out-b] (for b buffer)
L6:
  ilw.w vi10, 133(vi00)      |  nop   ;; vi10 = fade out flags
  iaddiu vi15, vi15, 0x173   |  nop   ;; vi15 = xtop + 371
  ilw.y vi02, 2(vi12)        |  nop   ;; vi02 = strip-len
  lq.xyzw vf14, 0(vi00)      |  nop   ;; vf14 = tri-strip-gif-tag
  ibeq vi00, vi10, L7        |  nop   ;; branch to L7 if no fade out.
  iadd vi03, vi03, vi12      |  nop   ;; st-output location = {st-out-a, st-out-b} + xtop + 140

  ;; modify vf14 to have :abe, setup constants for fadeout.
  mr32.xyzw vf27, vf14       |  nop   ;; vf27 = [yzwx] of the tri-strip-gif-tag
  ilw.w vi11, 134(vi00)      |  nop   ;; vi11 = fade-out parameter
  iaddiu vi13, vi00, 0x42    |  nop   ;; vi13 = 0x42
  mr32.y vf14, vf27          |  nop   ;; replace y of tag with w. (enables abe in the gs-prim)

;; next, we set up the memory layout of the output buffer.
;; the output buffer has two (poorly name) parts: "strip" and "shader".
;; the "strip" part will continue using the shader of the previous fragment.
;; it has a single tag. This "strip" part be left out, if desired. It is always first.
;; the "strip-len" is how large this data is.

;; the "shader" part contains a series of [shader, triangles] groups.
;; it sets the shader settings, then draws stuff.
;; the shader-cnt is how many [shader, triangle] groups there are.
;; the sizes are stored in the adgif data itself

;; the blocks below place the shaders and gif tags in memory, setting nloops and eops.

L7:
  ilwr.w vi09, vi03          |  nop   ;; vi09 = fp-header u8's [shader-cnt, kick-off, kick-step, hword-cnt]
  lqi.xyzw vf27, vi03        |  nop   ;; vf27 = xyz-add
  ilw.x vi04, 1(vi12)        |  nop   ;; vi04 = mat1-cnt
  iaddiu vi05, vi00, 0x7f    |  addw.xyz vf15, vf15, vf00 ;; vf15 = [1, 1, 1, 0], vi05 = 0x7f
  iand vi09, vi09, vi05      |  nop   ;; mask to get vi09 = shader-cnt
  ilw.y vi06, 1(vi12)        |  miniz.w vf19, vf00, vf27 ;; set add, vi06 = mat2-cnt
  ibeq vi00, vi02, L9        |  miniy.w vf18, vf00, vf27 ;; skip to L9 if strip-len is 0
  ilwr.z vi01, vi12          |  minix.w vf17, vf00, vf27 ;; vi01 = lump-off

;; we have strip
  ibne vi00, vi09, L8        |  nop  ;; goto L8 if we have nonzero shader-cnt
  sq.yzw vf14, 0(vi15)       |  nop  ;; store giftag (only upper 96 bits)

;; we have strip, but no shader. set eop.
  iaddiu vi02, vi02, 0x4000  |  nop  ;; build the first 32-bits, strip-len + eop flag
  iaddiu vi02, vi02, 0x4000  |  nop  ;; imm 0x8000 doesn't fit, have to add 2x.
  iswr.x vi02, vi15          |  nop  ;; store nloop = strip-len, eop.
  b L12                      |  nop  ;; skip shader setup!
  nop                        |  nop
L8:
  iswr.x vi02, vi15          |  nop ;; nonzero shader and nonzero strip. set nloop on the strip tag, but not eop.
L9:
  lq.xyzw vf13, 1(vi00)      |  nop ;; vf13 = adgif gif tag.
L10: ;; shader loop
  ilwr.w vi02, vi03          |  nop ;; vi02 = shader control word 0 (dest offset)
  lqi.xyzw vf08, vi03        |  nop ;; load shader data
  lqi.xyzw vf09, vi03        |  nop
  lqi.xyzw vf10, vi03        |  nop
  lqi.xyzw vf11, vi03        |  nop
  lqi.xyzw vf12, vi03        |  nop
  iadd vi02, vi02, vi15      |  nop ;; compute destination
  mtir vi08, vf09.w          |  nop ;; vi08 = shader control word 1 ()
  sqi.xyzw vf13, vi02        |  nop ;; store giftag
  sqi.xyzw vf08, vi02        |  nop ;; store shader0/5
  sqi.xyzw vf09, vi02        |  nop ;; store shader1/5
  mfir.x vf14, vi08          |  nop ;; vf14 = post-shader giftag lower-32
  sqi.xyzw vf10, vi02        |  nop ;; store shader2/5
  sqi.xyzw vf11, vi02        |  nop ;; store shader3/5
  ibeq vi00, vi10, L11       |  nop ;; goto L11 if no fadeout
  sqi.xyzw vf12, vi02        |  nop ;; store shader4/5

;; fade out setup (todo). it just modifies shaders
  mtir vi14, vf12.z          |  nop
  isw.x vi10, -1(vi02)       |  nop
  ibeq vi14, vi13, L11       |  nop
  isw.y vi11, -1(vi02)       |  nop
  ilw.x vi13, -4(vi02)       |  nop
  isubiu vi14, vi00, 0x1d    |  nop
  iand vi13, vi13, vi14      |  nop
  iaddi vi13, vi13, 0xc      |  nop
  isw.x vi13, -4(vi02)       |  nop
  iaddiu vi13, vi00, 0x42    |  nop
  isw.z vi13, -1(vi02)       |  nop

;; loop!
L11:
  ibgtz vi08, L10            |  nop ;; cool trick, eop is the sign bit here.
  sq.xyzw vf14, 0(vi02)      |  nop

;; end of shader and strip setup.

;; matrix crap 1
L12:
  lq.xyzw vf28, 3(vi00)      |  nop  ;; vf28 = persp-diag
  ilw.y vi08, 3(vi12)        |  nop  ;; vi08 = mat-slot.0
  lq.xyzw vf16, 5(vi00)      |  nop  ;; vf16 = scaled-persp-diag
  lq.xyzw vf20, 4(vi00)      |  nop  ;; vf20 = persp-off
  ilw.z vi09, 3(vi12)        |  mul.xyzw vf27, vf28, vf15 ;; vf27 = [pdx, pdy, pdz, 0], vi09 = mat-slot.1
  ior vi11, vi08, vi00       |  mul.xyzw vf28, vf28, vf00 ;; vf28 = [0, 0, 0, pdw], vi11 = vi08 = mat-slot.0
  ibeq vi00, vi08, L14       |  mul.xyzw vf15, vf16, vf15 ;; vf15 = [spdx, spdy, spdz, 0], skip if slot = 0
  iaddi vi13, vi12, 0x3      |  mul.xyzw vf16, vf16, vf00 ;; vi13 = mat-slot-ptr, vf16 = [0, 0, 0, spdw]

 ;; pipelined matrix thing
 ;; multiply matrices and store them back
 ;; run until we get a slot of 0.
 ;; mula.xyzw ACC, vf15, vf08
 ;; maddz.xyzw vf09, vf16, vf08
 ;; mula.xyzw ACC, vf15, vf10
 ;; maddz.xyzw vf11, vf16, vf10
 ;; mula.xyzw ACC, vf15, vf12
 ;; maddz.xyzw vf13, vf16, vf12
 ;; addax.xyzw vf20, vf00
 ;; madda.xyzw ACC, vf27, vf25
 ;; maddz.xyzw vf26, vf28, vf25

L13:
  lq.xyzw vf08, 0(vi08)      |  addax.xyzw vf20, vf00
  lq.xyzw vf10, 1(vi08)      |  madda.xyzw ACC, vf27, vf25
  lq.xyzw vf12, 2(vi08)      |  maddz.xyzw vf26, vf28, vf25
  lq.xyzw vf25, 3(vi08)      |  nop
  sq.xyzw vf09, 0(vi11)      |  mula.xyzw ACC, vf15, vf08
  sq.xyzw vf11, 1(vi11)      |  maddz.xyzw vf09, vf16, vf08
  sq.xyzw vf13, 2(vi11)      |  mula.xyzw ACC, vf15, vf10
  sq.xyzw vf26, 3(vi11)      |  maddz.xyzw vf11, vf16, vf10
  ibeq vi00, vi08, L14       |  mula.xyzw ACC, vf15, vf12
  ilwr.w vi10, vi13          |  maddz.xyzw vf13, vf16, vf12
  lq.xyzw vf08, 0(vi09)      |  addax.xyzw vf20, vf00
  lq.xyzw vf10, 1(vi09)      |  madda.xyzw ACC, vf27, vf25
  lq.xyzw vf12, 2(vi09)      |  maddz.xyzw vf26, vf28, vf25
  lq.xyzw vf25, 3(vi09)      |  nop
  sq.xyzw vf09, 0(vi08)      |  mula.xyzw ACC, vf15, vf08
  sq.xyzw vf11, 1(vi08)      |  maddz.xyzw vf09, vf16, vf08
  sq.xyzw vf13, 2(vi08)      |  mula.xyzw ACC, vf15, vf10
  sq.xyzw vf26, 3(vi08)      |  maddz.xyzw vf11, vf16, vf10
  ibeq vi00, vi09, L14       |  mula.xyzw ACC, vf15, vf12
  ilw.x vi11, 1(vi13)        |  maddz.xyzw vf13, vf16, vf12
  lq.xyzw vf08, 0(vi10)      |  addax.xyzw vf20, vf00
  lq.xyzw vf10, 1(vi10)      |  madda.xyzw ACC, vf27, vf25
  lq.xyzw vf12, 2(vi10)      |  maddz.xyzw vf26, vf28, vf25
  lq.xyzw vf25, 3(vi10)      |  nop
  sq.xyzw vf09, 0(vi09)      |  mula.xyzw ACC, vf15, vf08
  sq.xyzw vf11, 1(vi09)      |  maddz.xyzw vf09, vf16, vf08
  sq.xyzw vf13, 2(vi09)      |  mula.xyzw ACC, vf15, vf10
  sq.xyzw vf26, 3(vi09)      |  maddz.xyzw vf11, vf16, vf10
  ibeq vi00, vi10, L14       |  mula.xyzw ACC, vf15, vf12
  ilw.y vi08, 1(vi13)        |  maddz.xyzw vf13, vf16, vf12
  lq.xyzw vf08, 0(vi11)      |  addax.xyzw vf20, vf00
  lq.xyzw vf10, 1(vi11)      |  madda.xyzw ACC, vf27, vf25
  lq.xyzw vf12, 2(vi11)      |  maddz.xyzw vf26, vf28, vf25
  lq.xyzw vf25, 3(vi11)      |  nop
  sq.xyzw vf09, 0(vi10)      |  mula.xyzw ACC, vf15, vf08
  sq.xyzw vf11, 1(vi10)      |  maddz.xyzw vf09, vf16, vf08
  sq.xyzw vf13, 2(vi10)      |  mula.xyzw ACC, vf15, vf10
  sq.xyzw vf26, 3(vi10)      |  maddz.xyzw vf11, vf16, vf10
  iaddi vi13, vi13, 0x1      |  nop
  ibne vi00, vi11, L13       |  mula.xyzw ACC, vf15, vf12
  ilwr.z vi09, vi13          |  maddz.xyzw vf13, vf16, vf12
;; end matrix setup

;; the L14 -> L26 area handles mat1s.
;; it appears to set vi03 as the rgba reading pointer
;; and vi04 as the end of mat1's rgba (poniter to last, not one past the end)
;; and vi06 as the end of mat2's rgba
;; and vi07 as the end of mat3's rgba

L14:
  ilw.x vi02, 3(vi12)        |  nop   ;; vi02 = perc-off
  ibeq vi00, vi04, L26       |  nop   ;; goto L26 if mat1 count is 0
  iadd vi01, vi01, vi12      |  nop   ;; vi01 = lump.
  ilwr.x vi08, vi01          |  nop   ;; vi08 = lump[0].x = mat-0?
  lqi.xyzw vf08, vi01        |  nop   ;; vf08 = lump[0] = [mat0, mat1, nrmx, posx]?
  lqi.xyzw vf11, vi01        |  nop   ;; vf11 = lump[1] = [dst0, dst1, nrmy, posy]?
  lqi.xyzw vf14, vi01        |  nop   ;; vf14 = lump[2] = [texs, text, nrmz, posz]?
  lq.xyz vf29, 4(vi08)       |  nop   ;; vf29 = [nmat0x, nmat0y, nmat0z, 0.003921569]
  lq.xyz vf30, 5(vi08)       |  add.zw vf08, vf08, vf17   ;; add to nrm and pos, keep loading nmat (w is ? here)
  lq.xyzw vf31, 6(vi08)      |  add.xyzw vf11, vf11, vf18 ;; lump unpack and add
  iaddi vi04, vi04, -0x1     |  add.xyzw vf14, vf14, vf19 ;; lump unpack and add, subtract 1 from mat1 count
  iadd vi02, vi02, vi12      |  nop                       ;; vi02 = perc

  ;; right does matrix mul with nmat and the vertex.
  ;; it rotates the normal
  ;; left lqi's from perc, unpacks dst0, dst1 to vi10 and vi13
  lqi.xyzw vf24, vi02        |  mulaz.xyzw ACC, vf29, vf08
  mtir vi10, vf11.x          |  maddaz.xyzw ACC, vf30, vf11
  mtir vi13, vf11.y          |  maddz.xyz vf11, vf31, vf14

  ;; vf25, vf26, vf27, vf28 is the tmat
  lq.xyzw vf25, 0(vi08)      |  nop
  lq.xyzw vf26, 1(vi08)      |  itof0.xyzw vf24, vf24 ;; perc to floats: no idea what this is yet.
  lq.xyzw vf27, 2(vi08)      |  nop
  erleng.xyz P, vf11         |  nop ;; p = normal length.
  lq.xyzw vf28, 3(vi08)      |  mulaw.xyzw ACC, vf25, vf08 ;; transform point, keep loading tmat
  ior vi15, vi07, vi00       |  maddaw.xyzw ACC, vf26, vf11 ;; vi15 = using mercprime
  mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14 ;; vf14.z = 1. (clears nrmz)
  lqi.xyzw vf09, vi01        |  nop ;; vf09 = [mat0, mat1, nrmx, posx]? (P1)
  ilwr.y vi03, vi12          |  nop ;; vi03 = rgba-off
  ilw.z vi07, 1(vi12)        |  nop ;; vi07 = mat3-cnt (why.....)
  lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28 ;; P1 vert load | finish xf pt
  lqi.xyzw vf15, vi01        |  nop ;; P1 vert load
  mtir vi08, vf09.x          |  nop ;; P1 mat0
  ibeq vi00, vi15, L15       |  nop
  iadd vi03, vi03, vi12      |  nop ;; vi03 = rgba

  ;; only if merc-prime
  nop                        |  miniw.w vf08, vf08, vf01

L15:
;; rhs is lump adding
  div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17    ;; perspective divide!
  iadd vi04, vi04, vi03      |  add.xyzw vf12, vf12, vf18  ;; vi04 = rgba + mat1-cnt - 1 (??)
    ;; note: the -1 is because the last vertex is handled
  lq.xyz vf29, 4(vi08)       |  add.xyzw vf15, vf15, vf19  ;; P1 load nmat | P1 lump add v2

  lq.xyz vf30, 5(vi08)       |  nop ;; P1 load nmat
  iadd vi06, vi06, vi04      |  nop ;; vi06 = rgba + mat1-cnt + mat2-cnt - 1
  lq.xyzw vf31, 6(vi08)      |  nop ;; p1 load mat
  lq.xyzw vf25, 0(vi08)      |  nop ;; p1 load mat
  lq.xyzw vf26, 1(vi08)      |  mul.xyz vf08, vf08, Q  ;; p1 load mat | perspective multiply
  mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q ;; p1 get dst | vf14 = [s*Q, t*Q, Q, posz*Q]
  mtir vi14, vf12.y          |  nop ;; p1 get dst
  lq.xyzw vf27, 2(vi08)      |  nop ;; p1 load mat
  lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22 ;; load rgba, hvdf offset
  ;; rhs is p1's normal xform
  iadd vi07, vi07, vi06      |  mulaz.xyzw ACC, vf29, vf09  ;; vi07 = rgba + mat1-cnt + mat2-cnt + mat3-cnt - 1
  lq.xyzw vf28, 3(vi08)      |  maddaz.xyzw ACC, vf30, vf12 ;;
  mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15   ;; vf20.w = normal length
  nop                        |  nop
  1024.0                     |  miniw.w vf08, vf08, vf03 :i  ;; fog max
  ibne vi00, vi15, L82       |  mulaw.xyzw ACC, vf25, vf09 ;; if mercprime, goto L82 | rhs is p1
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20 ;; vi09 = p0 vtx's mat1 | normalization of normal
  erleng.xyz P, vf12         |  nop                        ;; p1's nrm length
  nop                        |  maddaw.xyzw ACC, vf26, vf12 ;; p1 nrm xf
  mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15 ;; p1
  lqi.xyzw vf10, vi01        |  mulax.xyzw ACC, vf01, vf11  ;; dot with light dir
  ibne vi04, vi03, L17       |  madday.xyzw ACC, vf02, vf11 ;; branch to L17 if not done
  nop                        |  maddz.xyzw vf11, vf03, vf11
  ibne vi06, vi03, L31       |  nop ;; branch to L31 if mat2's can be done
  nop                        |  nop
  b L67                      |  nop ;; go to mat3
  nop                        |  nop

;; pipelined mat1 loop here
L16:
  lqi.xyzw vf10, vi01        |  mulax.xyzw ACC, vf01, vf11
  sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
L17: ;; entry from above.
  lqi.xyzw vf13, vi01        |  add.xyzw vf09, vf09, vf28 ;; all p1
  lqi.xyzw vf16, vi01        |  maxw.w vf08, vf08, vf02 ;; p1 | fog min
  mtir vi08, vf10.x          |  itof0.xyzw vf23, vf23   ;; rgba to float
  ilw.y vi09, -9(vi01)       |  maxx.xyzw vf11, vf11, vf00 ;; vi09 = mat1 | light clamp
  div Q, vf01.w, vf09.w      |  add.zw vf10, vf10, vf17     ;; pipe 1
  move.xyzw vf21, vf08       |  add.xyzw vf13, vf13, vf18   ;; move | pipe1
  lq.xyz vf29, 4(vi08)       |  add.xyzw vf16, vf16, vf19   ;; pipe | pipe
  lq.xyz vf30, 5(vi08)       |  mulax.xyzw ACC, vf04, vf11  ;; more lights
  ibgtz vi09, L18            |  madday.xyzw ACC, vf05, vf11
  lq.xyzw vf31, 6(vi08)      |  maddaz.xyzw ACC, vf06, vf11
  nop                        |  addx.w vf21, vf21, vf17
L18:
  lq.xyzw vf25, 0(vi08)      |  maddw.xyzw vf11, vf07, vf00 ;; p1 | add ambient
  lq.xyzw vf26, 1(vi08)      |  mul.xyz vf09, vf09, Q ;; p | p
  mtir vi12, vf13.x          |  mul.xyzw vf15, vf15, Q ;; p | p
  mtir vi15, vf13.y          |  ftoi4.xyzw vf21, vf21  ;; p | position ftoi
  lq.xyzw vf27, 2(vi08)      |  mul.xyzw vf11, vf11, vf23 ;; p | vertex coloring
  lqi.xyzw vf23, vi03        |  add.xyzw vf09, vf09, vf22
  ibne vi00, vi09, L19       |  mulaz.xyzw ACC, vf29, vf10
  sq.xyzw vf21, 2(vi10)      |  maddaz.xyzw ACC, vf30, vf13
  nop                        |  ftoi4.xyzw vf21, vf08
L19:
  mfp.w vf20, P              |  maddz.xyz vf13, vf31, vf16
  sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  sq.xyzw vf14, 0(vi13)      |  miniw.w vf09, vf09, vf03
  sq.xyzw vf21, 2(vi13)      |  mulaw.xyzw ACC, vf25, vf10
  lq.xyzw vf28, 3(vi08)      |  mulw.xyzw vf12, vf12, vf20
  erleng.xyz P, vf13         |  ftoi0.xyzw vf11, vf11
  ;; branch to end (first version in this loop)
  ibne vi04, vi03, L20       |  maddaw.xyzw ACC, vf26, vf13
  mr32.z vf16, vf00          |  maddw.xyzw vf10, vf27, vf16
  ibne vi06, vi03, L36       |  nop ;; branch if there's mat2's to do
  ilw.y vi09, -6(vi01)       |  nop ;; pipeline "exiting"
  ibne vi07, vi03, L72       |  nop
  nop                        |  nop
  b L143                     |  nop
  nop                        |  nop
L20:
  lqi.xyzw vf08, vi01        |  mulax.xyzw ACC, vf01, vf12   ;;
  sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12  ;;
  sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12  ;;
  lqi.xyzw vf11, vi01        |  add.xyzw vf10, vf10, vf28    ;;
  lqi.xyzw vf14, vi01        |  maxw.w vf09, vf09, vf02      ;;
  mtir vi08, vf08.x          |  itof0.xyzw vf23, vf23        ;;
  ilw.y vi09, -9(vi01)       |  maxx.xyzw vf12, vf12, vf00   ;;
  div Q, vf01.w, vf10.w      |  add.zw vf08, vf08, vf17      ;;
  move.xyzw vf21, vf09       |  add.xyzw vf11, vf11, vf18    ;;
  lq.xyz vf29, 4(vi08)       |  add.xyzw vf14, vf14, vf19
  lq.xyz vf30, 5(vi08)       |  mulax.xyzw ACC, vf04, vf12  ;;
  ibgtz vi09, L21            |  madday.xyzw ACC, vf05, vf12 ;;
  lq.xyzw vf31, 6(vi08)      |  maddaz.xyzw ACC, vf06, vf12 ;;
  nop                        |  addx.w vf21, vf21, vf17     ;;
L21:
  lq.xyzw vf25, 0(vi08)      |  maddw.xyzw vf12, vf07, vf00 ;;
  lq.xyzw vf26, 1(vi08)      |  mul.xyz vf10, vf10, Q       ;;
  mtir vi10, vf11.x          |  mul.xyzw vf16, vf16, Q      ;;
  mtir vi13, vf11.y          |  ftoi4.xyzw vf21, vf21       ;;
  lq.xyzw vf27, 2(vi08)      |  mul.xyzw vf12, vf12, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf10, vf10, vf22
  ibne vi00, vi09, L22       |  mulaz.xyzw ACC, vf29, vf08
  sq.xyzw vf21, 2(vi11)      |  maddaz.xyzw ACC, vf30, vf11 ;;
  nop                        |  ftoi4.xyzw vf21, vf09
L22:
  mfp.w vf20, P              |  maddz.xyz vf11, vf31, vf14  ;;
  sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17 ;;
  sq.xyzw vf15, 0(vi14)      |  miniw.w vf10, vf10, vf03    ;;
  sq.xyzw vf21, 2(vi14)      |  mulaw.xyzw ACC, vf25, vf08  ;;
  lq.xyzw vf28, 3(vi08)      |  mulw.xyzw vf13, vf13, vf20  ;;
  erleng.xyz P, vf11         |  ftoi0.xyzw vf12, vf12       ;;
  ibne vi04, vi03, L23       |  maddaw.xyzw ACC, vf26, vf11
  mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  ibne vi06, vi03, L41       |  nop
  ilw.y vi09, -6(vi01)       |  nop
  ibne vi07, vi03, L77       |  nop
  nop                        |  nop
  b L153                     |  nop
  nop                        |  nop
L23:
  lqi.xyzw vf09, vi01        |  mulax.xyzw ACC, vf01, vf13   ;;
  sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13  ;;
  sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13  ;;
  lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  lqi.xyzw vf15, vi01        |  maxw.w vf10, vf10, vf02      ;;
  mtir vi08, vf09.x          |  itof0.xyzw vf23, vf23        ;;
  ilw.y vi09, -9(vi01)       |  maxx.xyzw vf13, vf13, vf00
  div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  move.xyzw vf21, vf10       |  add.xyzw vf12, vf12, vf18    ;;
  lq.xyz vf29, 4(vi08)       |  add.xyzw vf15, vf15, vf19
  lq.xyz vf30, 5(vi08)       |  mulax.xyzw ACC, vf04, vf13   ;;
  ibgtz vi09, L24            |  madday.xyzw ACC, vf05, vf13  ;;
  lq.xyzw vf31, 6(vi08)      |  maddaz.xyzw ACC, vf06, vf13  ;;
  nop                        |  addx.w vf21, vf21, vf17      ;;
L24:
  lq.xyzw vf25, 0(vi08)      |  maddw.xyzw vf13, vf07, vf00
  lq.xyzw vf26, 1(vi08)      |  mul.xyz vf08, vf08, Q
  mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  mtir vi14, vf12.y          |  ftoi4.xyzw vf21, vf21        ;;
  lq.xyzw vf27, 2(vi08)      |  mul.xyzw vf13, vf13, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
  ibne vi00, vi09, L25       |  mulaz.xyzw ACC, vf29, vf09
  sq.xyzw vf21, 2(vi12)      |  maddaz.xyzw ACC, vf30, vf12
  nop                        |  ftoi4.xyzw vf21, vf10
L25:
  mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  sq.xyzw vf16, 0(vi15)      |  miniw.w vf08, vf08, vf03
  sq.xyzw vf21, 2(vi15)      |  mulaw.xyzw ACC, vf25, vf09
  lq.xyzw vf28, 3(vi08)      |  mulw.xyzw vf11, vf11, vf20
  erleng.xyz P, vf12         |  ftoi0.xyzw vf13, vf13
  ibne vi04, vi03, L16       |  maddaw.xyzw ACC, vf26, vf12
  mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  ibne vi06, vi03, L30       |  nop
  ilw.y vi09, -6(vi01)       |  nop
  ibne vi07, vi03, L66       |  nop
  nop                        |  nop
  b L163                     |  nop
  nop                        |  nop

L26: ;; pipeline startup for mat2's (assuming you had no mat1's to get things started)
  ibeq vi00, vi06, L61       |  nop ;; goto L61 if no mat2's
  iadd vi02, vi02, vi12      |  nop ;; compute perc offset
  lqi.xyzw vf08, vi01        |  nop ;; vf08 = vtx0 = [mat0, mat1, nrmx, posx]
  lqi.xyzw vf24, vi02        |  nop ;; vf24 = perc
  lqi.xyzw vf11, vi01        |  nop ;; vf11 = vtx1 = [dst0, dst1, nrmy, posy]
  lqi.xyzw vf14, vi01        |  nop ;; vf14 = vtx2 = [texs, text, nrmz, posz]
  mtir vi10, vf08.x          |  nop                           ;; vi10 = mat0
  mtir vi13, vf08.y          |  itof0.xyzw vf24, vf24         ;; vi13 = mat1 | vf24 = itof0(perc)
  iaddi vi06, vi06, -0x1     |  add.zw vf08, vf08, vf17       ;; dec count | lump offset
  nop                        |  add.xyzw vf11, vf11, vf18     ;; lump offset
  iand vi10, vi10, vi05      |  add.xyzw vf14, vf14, vf19     ;; vi10 = mat0 & 0x7f | lump offset
  nop                        |  mulw.xyzw vf24, vf24, vf29    ;; scale perc
  iand vi13, vi13, vi05      |  nop                           ;; vi13 = mat1 & 0x7f

  ;; left col is loading matrices, right col is multiplication by perc.
  lq.xyzw vf20, 0(vi10)      |  nop
  lq.xyzw vf25, 0(vi13)      |  nop
  lq.xyzw vf23, 1(vi10)      |  nop
  lq.xyzw vf26, 1(vi13)      |  nop
  lq.xyzw vf20, 2(vi10)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf27, 2(vi13)      |  maddy.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 3(vi10)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf28, 3(vi13)      |  maddy.xyzw vf26, vf26, vf24
  lq.xyzw vf20, 4(vi10)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi13)       |  maddy.xyzw vf27, vf27, vf24
  lq.xyzw vf23, 5(vi10)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi13)       |  maddy.xyzw vf28, vf28, vf24
  lq.xyzw vf20, 6(vi10)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf31, 6(vi13)      |  maddy.xyz vf29, vf29, vf24
  mtir vi10, vf11.x          |  mulax.xyzw ACC, vf23, vf24
  mtir vi13, vf11.y          |  maddy.xyz vf30, vf30, vf24
  nop                        |  mulax.xyzw ACC, vf20, vf24
  nop                        |  maddy.xyzw vf31, vf31, vf24  ;; last perc multiply
  nop                        |  mulaz.xyzw ACC, vf29, vf08   ;; rotate normal
  nop                        |  maddaz.xyzw ACC, vf30, vf11
  nop                        |  maddz.xyz vf11, vf31, vf14
  nop                        |  nop
  nop                        |  nop
  nop                        |  mulaw.xyzw ACC, vf25, vf08   ;; xf point
  iaddiu vi08, vi00, 0x243   |  nop                          ;; vi08 = 0x243 = 579
  erleng.xyz P, vf11         |  nop                          ;; length of normal
  ior vi15, vi07, vi00       |  maddaw.xyzw ACC, vf26, vf11  ;; vi15 = mercprimt | xf point
  mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14  ;; set vf14.z = 1.0 | xf point
  lqi.xyzw vf09, vi01        |  nop                          ;; pipe |
  ilwr.y vi03, vi12          |  nop                          ;; vi03 = rgba-off
  ilw.z vi07, 1(vi12)        |  nop                          ;; vi07 = mat3-cnt
  lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28    ;; pipe | xf point
  lqi.xyzw vf15, vi01        |  nop                          ;; pipe
  mtir vi11, vf09.x          |  nop                          ;; pipe
  ibeq vi00, vi15, L27       |  nop                          ;; goto L27 if no merc-prim
  mtir vi14, vf09.y          |  nop                          ;; pipe
  iaddiu vi08, vi00, 0x539   |  miniw.w vf08, vf08, vf01     ;; ONLY RUN IF MERCPRIME
L27:
  div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17      ;; perspective divide | pipe
  iadd vi03, vi03, vi12      |  add.xyzw vf12, vf12, vf18    ;; rgba off | pipe
  iand vi11, vi11, vi05      |  add.xyzw vf15, vf15, vf19    ;; pipe | pipe
  iadd vi06, vi06, vi03      |  nop                          ;; end addr calc1
  iadd vi07, vi07, vi06      |  nop                          ;; end addr calc2
  iand vi14, vi14, vi05      |  nop                          ;; pipe
  ibne vi05, vi11, L28       |  nop                          ;; branch on next(?) vertex mat0 sign bit
  nop                        |  mul.xyz vf08, vf08, Q        ;; perspective
  mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  mtir vi14, vf12.y          |  nop
  b L29                      |  nop                         ;; branch to skip matrix load, if possible
  lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22   ;; load rgba | hvdf offset
L28:
  lq.xyzw vf20, 0(vi11)      |  mul.xyzw vf14, vf14, Q
  lq.xyzw vf25, 0(vi14)      |  nop
  lq.xyzw vf23, 1(vi11)      |  nop
  lq.xyzw vf26, 1(vi14)      |  add.xyzw vf08, vf08, vf22
  lq.xyzw vf20, 2(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyzw vf27, 2(vi14)      |  maddw.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 3(vi11)      |  mulaz.xyzw ACC, vf23, vf24
  lq.xyzw vf28, 3(vi14)      |  maddw.xyzw vf26, vf26, vf24
  lq.xyzw vf20, 4(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi14)       |  maddw.xyzw vf27, vf27, vf24
  lq.xyzw vf23, 5(vi11)      |  mulaz.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi14)       |  maddw.xyzw vf28, vf28, vf24
  lq.xyzw vf20, 6(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyzw vf31, 6(vi14)      |  maddw.xyz vf29, vf29, vf24
  lqi.xyzw vf23, vi02        |  mulaz.xyzw ACC, vf23, vf24
  mtir vi11, vf12.x          |  maddw.xyz vf30, vf30, vf24
  mtir vi14, vf12.y          |  mulaz.xyzw ACC, vf20, vf24
  iaddiu vi08, vi00, 0x1a1   |  maddw.xyzw vf31, vf31, vf24
  ibeq vi00, vi15, L29       |  nop
  lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
  iaddiu vi08, vi00, 0x48e   |  nop
L29:
  nop                        |  mulaz.xyzw ACC, vf29, vf09   ;; pipe
  nop                        |  maddaz.xyzw ACC, vf30, vf12  ;; pipe
  mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15   ;; normal length | pipe
  nop                        |  nop
  1024.0                     |  miniw.w vf08, vf08, vf03 :i  ;; fog max
  ibne vi00, vi15, L93       |  mulaw.xyzw ACC, vf25, vf09   ;; mercprime | pipe
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20   ;; vi09 = mat1 | normalize
  erleng.xyz P, vf12         |  nop                          ;; pipe
  ibeq vi06, vi03, L65       |  maddaw.xyzw ACC, vf26, vf12  ;; check done | pipe
  mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15  ;; pipe | pipe
  lqi.xyzw vf10, vi01        |  mulax.xyzw ACC, vf01, vf11   ;; pipe | dot product with light
  jr vi08                    |  madday.xyzw ACC, vf02, vf11
  nop                        |  maddz.xyzw vf11, vf03, vf11
L30: ;; mat2 cross entry 3
  lqi.xyzw vf10, vi01        |  mulax.xyzw ACC, vf01, vf11
  sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
L31:
  lqi.xyzw vf13, vi01        |  add.xyzw vf09, vf09, vf28
  lqi.xyzw vf16, vi01        |  maxw.w vf08, vf08, vf02
  mtir vi12, vf10.x          |  itof0.xyzw vf23, vf23
  mtir vi15, vf10.y          |  maxx.xyzw vf11, vf11, vf00
  div Q, vf01.w, vf09.w      |  add.zw vf10, vf10, vf17
  move.xyzw vf21, vf08       |  add.xyzw vf13, vf13, vf18
  iand vi12, vi12, vi05      |  add.xyzw vf16, vf16, vf19
  nop                        |  mulax.xyzw ACC, vf04, vf11
  ibgtz vi09, L32            |  madday.xyzw ACC, vf05, vf11
  iand vi15, vi15, vi05      |  maddaz.xyzw ACC, vf06, vf11
  nop                        |  addx.w vf21, vf21, vf17
L32:
  ibne vi05, vi12, L33       |  maddw.xyzw vf11, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf09, vf09, Q
  mtir vi12, vf13.x          |  mul.xyzw vf15, vf15, Q
  mtir vi15, vf13.y          |  ftoi4.xyzw vf21, vf21
  b L34                      |  mul.xyzw vf11, vf11, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf09, vf09, vf22
L33:
  lq.xyzw vf20, 0(vi12)      |  mul.xyzw vf15, vf15, Q
  nop                        |  mulw.xyzw vf24, vf24, vf29
  lq.xyzw vf25, 0(vi15)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf23, 1(vi12)      |  mul.xyzw vf11, vf11, vf23
  lq.xyzw vf26, 1(vi15)      |  add.xyzw vf09, vf09, vf22
  lq.xyzw vf20, 2(vi12)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf27, 2(vi15)      |  maddy.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 3(vi12)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf28, 3(vi15)      |  maddy.xyzw vf26, vf26, vf24
  lq.xyzw vf20, 4(vi12)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi15)       |  maddy.xyzw vf27, vf27, vf24
  lq.xyzw vf23, 5(vi12)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi15)       |  maddy.xyzw vf28, vf28, vf24
  lq.xyzw vf20, 6(vi12)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf31, 6(vi15)      |  maddy.xyz vf29, vf29, vf24
  mtir vi12, vf13.x          |  mulax.xyzw ACC, vf23, vf24
  mtir vi15, vf13.y          |  maddy.xyz vf30, vf30, vf24
  b L49                      |  mulax.xyzw ACC, vf20, vf24
  lqi.xyzw vf23, vi03        |  maddy.xyzw vf31, vf31, vf24
L34:
  ibgez vi09, L35            |  mulaz.xyzw ACC, vf29, vf10
  sq.xyzw vf21, 2(vi10)      |  maddaz.xyzw ACC, vf30, vf13
  nop                        |  ftoi4.xyzw vf21, vf08
L35:
  mfp.w vf20, P              |  maddz.xyz vf13, vf31, vf16
  sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  sq.xyzw vf14, 0(vi13)      |  miniw.w vf09, vf09, vf03
  sq.xyzw vf21, 2(vi13)      |  mulaw.xyzw ACC, vf25, vf10
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf12, vf12, vf20
  erleng.xyz P, vf13         |  ftoi0.xyzw vf11, vf11
  ibne vi06, vi03, L36       |  maddaw.xyzw ACC, vf26, vf13
  mr32.z vf16, vf00          |  maddw.xyzw vf10, vf27, vf16
  ibne vi07, vi03, L72       |  nop
  nop                        |  nop
  b L143                     |  nop
  nop                        |  nop
L36: ;; mat2 cross entry 1
  lqi.xyzw vf08, vi01        |  mulax.xyzw ACC, vf01, vf12
  sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  ;; END of first vertex....
  lqi.xyzw vf11, vi01        |  add.xyzw vf10, vf10, vf28
  lqi.xyzw vf14, vi01        |  maxw.w vf09, vf09, vf02
  mtir vi10, vf08.x          |  itof0.xyzw vf23, vf23
  mtir vi13, vf08.y          |  maxx.xyzw vf12, vf12, vf00
  div Q, vf01.w, vf10.w      |  add.zw vf08, vf08, vf17
  move.xyzw vf21, vf09       |  add.xyzw vf11, vf11, vf18
  iand vi10, vi10, vi05      |  add.xyzw vf14, vf14, vf19
  nop                        |  mulax.xyzw ACC, vf04, vf12
  ibgtz vi09, L37            |  madday.xyzw ACC, vf05, vf12
  iand vi13, vi13, vi05      |  maddaz.xyzw ACC, vf06, vf12
  nop                        |  addx.w vf21, vf21, vf17
L37:
  ibne vi05, vi10, L38       |  maddw.xyzw vf12, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf10, vf10, Q
  mtir vi10, vf11.x          |  mul.xyzw vf16, vf16, Q
  mtir vi13, vf11.y          |  ftoi4.xyzw vf21, vf21
  b L39                      |  mul.xyzw vf12, vf12, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf10, vf10, vf22
L38:
  lq.xyzw vf20, 0(vi10)      |  mul.xyzw vf16, vf16, Q
  nop                        |  mulw.xyzw vf24, vf24, vf29
  lq.xyzw vf25, 0(vi13)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf23, 1(vi10)      |  mul.xyzw vf12, vf12, vf23
  lq.xyzw vf26, 1(vi13)      |  add.xyzw vf10, vf10, vf22
  lq.xyzw vf20, 2(vi10)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf27, 2(vi13)      |  maddy.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 3(vi10)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf28, 3(vi13)      |  maddy.xyzw vf26, vf26, vf24
  lq.xyzw vf20, 4(vi10)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi13)       |  maddy.xyzw vf27, vf27, vf24
  lq.xyzw vf23, 5(vi10)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi13)       |  maddy.xyzw vf28, vf28, vf24
  lq.xyzw vf20, 6(vi10)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf31, 6(vi13)      |  maddy.xyz vf29, vf29, vf24
  mtir vi10, vf11.x          |  mulax.xyzw ACC, vf23, vf24
  mtir vi13, vf11.y          |  maddy.xyz vf30, vf30, vf24
  b L54                      |  mulax.xyzw ACC, vf20, vf24
  lqi.xyzw vf23, vi03        |  maddy.xyzw vf31, vf31, vf24
L39:
  ibgez vi09, L40            |  mulaz.xyzw ACC, vf29, vf08
  sq.xyzw vf21, 2(vi11)      |  maddaz.xyzw ACC, vf30, vf11
  nop                        |  ftoi4.xyzw vf21, vf09
L40:
  mfp.w vf20, P              |  maddz.xyz vf11, vf31, vf14
  sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  sq.xyzw vf15, 0(vi14)      |  miniw.w vf10, vf10, vf03
  sq.xyzw vf21, 2(vi14)      |  mulaw.xyzw ACC, vf25, vf08
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf13, vf13, vf20
  erleng.xyz P, vf11         |  ftoi0.xyzw vf12, vf12
  ibne vi06, vi03, L41       |  maddaw.xyzw ACC, vf26, vf11
  mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  ibne vi07, vi03, L77       |  nop
  nop                        |  nop
  b L153                     |  nop
  nop                        |  nop
L41: ;; mat2 cross entry 2
  lqi.xyzw vf09, vi01        |  mulax.xyzw ACC, vf01, vf13
  sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  lqi.xyzw vf15, vi01        |  maxw.w vf10, vf10, vf02
  mtir vi11, vf09.x          |  itof0.xyzw vf23, vf23
  mtir vi14, vf09.y          |  maxx.xyzw vf13, vf13, vf00
  div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  move.xyzw vf21, vf10       |  add.xyzw vf12, vf12, vf18
  iand vi11, vi11, vi05      |  add.xyzw vf15, vf15, vf19
  nop                        |  mulax.xyzw ACC, vf04, vf13
  ibgtz vi09, L42            |  madday.xyzw ACC, vf05, vf13
  iand vi14, vi14, vi05      |  maddaz.xyzw ACC, vf06, vf13
  nop                        |  addx.w vf21, vf21, vf17
L42:
  ibne vi05, vi11, L43       |  maddw.xyzw vf13, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf08, vf08, Q
  mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  mtir vi14, vf12.y          |  ftoi4.xyzw vf21, vf21
  b L44                      |  mul.xyzw vf13, vf13, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
L43:
  lq.xyzw vf20, 0(vi11)      |  mul.xyzw vf14, vf14, Q
  nop                        |  mulw.xyzw vf24, vf24, vf29
  lq.xyzw vf25, 0(vi14)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf23, 1(vi11)      |  mul.xyzw vf13, vf13, vf23
  lq.xyzw vf26, 1(vi14)      |  add.xyzw vf08, vf08, vf22
  lq.xyzw vf20, 2(vi11)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf27, 2(vi14)      |  maddy.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 3(vi11)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf28, 3(vi14)      |  maddy.xyzw vf26, vf26, vf24
  lq.xyzw vf20, 4(vi11)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi14)       |  maddy.xyzw vf27, vf27, vf24
  lq.xyzw vf23, 5(vi11)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi14)       |  maddy.xyzw vf28, vf28, vf24
  lq.xyzw vf20, 6(vi11)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf31, 6(vi14)      |  maddy.xyz vf29, vf29, vf24
  mtir vi11, vf12.x          |  mulax.xyzw ACC, vf23, vf24
  mtir vi14, vf12.y          |  maddy.xyz vf30, vf30, vf24
  b L59                      |  mulax.xyzw ACC, vf20, vf24
  lqi.xyzw vf23, vi03        |  maddy.xyzw vf31, vf31, vf24
L44:
  ibgez vi09, L45            |  mulaz.xyzw ACC, vf29, vf09
  sq.xyzw vf21, 2(vi12)      |  maddaz.xyzw ACC, vf30, vf12
  nop                        |  ftoi4.xyzw vf21, vf10
L45:
  mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  sq.xyzw vf16, 0(vi15)      |  miniw.w vf08, vf08, vf03
  sq.xyzw vf21, 2(vi15)      |  mulaw.xyzw ACC, vf25, vf09
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20
  erleng.xyz P, vf12         |  ftoi0.xyzw vf13, vf13
  ibne vi06, vi03, L30       |  maddaw.xyzw ACC, vf26, vf12
  mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  ibne vi07, vi03, L66       |  nop
  nop                        |  nop
  b L163                     |  nop
  nop                        |  nop
L46:
  lqi.xyzw vf10, vi01        |  mulax.xyzw ACC, vf01, vf11
  sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
  ;; ugh, I think we jump here.
  lqi.xyzw vf13, vi01        |  add.xyzw vf09, vf09, vf28 ;; pipe | pipe
  lqi.xyzw vf16, vi01        |  maxw.w vf08, vf08, vf02   ;; pipe | fogmin
  mtir vi12, vf10.x          |  itof0.xyzw vf23, vf23     ;; pipe | vtx color convert
  mtir vi15, vf10.y          |  maxx.xyzw vf11, vf11, vf00 ;; pipe | light clamp
  div Q, vf01.w, vf09.w      |  add.zw vf10, vf10, vf17
  move.xyzw vf21, vf08       |  add.xyzw vf13, vf13, vf18
  iand vi12, vi12, vi05      |  add.xyzw vf16, vf16, vf19
  nop                        |  mulax.xyzw ACC, vf04, vf11
  ibgtz vi09, L47            |  madday.xyzw ACC, vf05, vf11
  iand vi15, vi15, vi05      |  maddaz.xyzw ACC, vf06, vf11
  nop                        |  addx.w vf21, vf21, vf17
L47:
  ibne vi05, vi12, L48       |  maddw.xyzw vf11, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf09, vf09, Q
  mtir vi12, vf13.x          |  mul.xyzw vf15, vf15, Q
  mtir vi15, vf13.y          |  ftoi4.xyzw vf21, vf21
  b L49                      |  mul.xyzw vf11, vf11, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf09, vf09, vf22
L48:
  lq.xyzw vf20, 0(vi12)      |  mul.xyzw vf15, vf15, Q
  lq.xyzw vf25, 0(vi15)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf23, 1(vi12)      |  mul.xyzw vf11, vf11, vf23
  lq.xyzw vf26, 1(vi15)      |  add.xyzw vf09, vf09, vf22
  lq.xyzw vf20, 2(vi12)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyzw vf27, 2(vi15)      |  maddw.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 3(vi12)      |  mulaz.xyzw ACC, vf23, vf24
  lq.xyzw vf28, 3(vi15)      |  maddw.xyzw vf26, vf26, vf24
  lq.xyzw vf20, 4(vi12)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi15)       |  maddw.xyzw vf27, vf27, vf24
  lq.xyzw vf23, 5(vi12)      |  mulaz.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi15)       |  maddw.xyzw vf28, vf28, vf24
  lq.xyzw vf20, 6(vi12)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyzw vf31, 6(vi15)      |  maddw.xyz vf29, vf29, vf24
  lqi.xyzw vf23, vi02        |  mulaz.xyzw ACC, vf23, vf24
  mtir vi12, vf13.x          |  maddw.xyz vf30, vf30, vf24
  mtir vi15, vf13.y          |  mulaz.xyzw ACC, vf20, vf24
  b L34                      |  maddw.xyzw vf31, vf31, vf24
  lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
L49:
  ibgez vi09, L50            |  mulaz.xyzw ACC, vf29, vf10
  sq.xyzw vf21, 2(vi10)      |  maddaz.xyzw ACC, vf30, vf13  ;; first store
  nop                        |  ftoi4.xyzw vf21, vf08
L50:
  mfp.w vf20, P              |  maddz.xyz vf13, vf31, vf16
  sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  sq.xyzw vf14, 0(vi13)      |  miniw.w vf09, vf09, vf03
  sq.xyzw vf21, 2(vi13)      |  mulaw.xyzw ACC, vf25, vf10   ;; second store
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf12, vf12, vf20
  erleng.xyz P, vf13         |  ftoi0.xyzw vf11, vf11
  ibne vi06, vi03, L51       |  maddaw.xyzw ACC, vf26, vf13
  mr32.z vf16, vf00          |  maddw.xyzw vf10, vf27, vf16
  ibne vi07, vi03, L72       |  nop
  nop                        |  nop
  b L143                     |  nop
  nop                        |  nop
L51:
  lqi.xyzw vf08, vi01        |  mulax.xyzw ACC, vf01, vf12
  sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  ;; end, for real this time.
  lqi.xyzw vf11, vi01        |  add.xyzw vf10, vf10, vf28
  lqi.xyzw vf14, vi01        |  maxw.w vf09, vf09, vf02
  mtir vi10, vf08.x          |  itof0.xyzw vf23, vf23
  mtir vi13, vf08.y          |  maxx.xyzw vf12, vf12, vf00
  div Q, vf01.w, vf10.w      |  add.zw vf08, vf08, vf17
  move.xyzw vf21, vf09       |  add.xyzw vf11, vf11, vf18
  iand vi10, vi10, vi05      |  add.xyzw vf14, vf14, vf19
  nop                        |  mulax.xyzw ACC, vf04, vf12
  ibgtz vi09, L52            |  madday.xyzw ACC, vf05, vf12
  iand vi13, vi13, vi05      |  maddaz.xyzw ACC, vf06, vf12
  nop                        |  addx.w vf21, vf21, vf17
L52:
  ibne vi05, vi10, L53       |  maddw.xyzw vf12, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf10, vf10, Q
  mtir vi10, vf11.x          |  mul.xyzw vf16, vf16, Q
  mtir vi13, vf11.y          |  ftoi4.xyzw vf21, vf21
  b L54                      |  mul.xyzw vf12, vf12, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf10, vf10, vf22
L53:
  lq.xyzw vf20, 0(vi10)      |  mul.xyzw vf16, vf16, Q
  lq.xyzw vf25, 0(vi13)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf23, 1(vi10)      |  mul.xyzw vf12, vf12, vf23
  lq.xyzw vf26, 1(vi13)      |  add.xyzw vf10, vf10, vf22
  lq.xyzw vf20, 2(vi10)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyzw vf27, 2(vi13)      |  maddw.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 3(vi10)      |  mulaz.xyzw ACC, vf23, vf24
  lq.xyzw vf28, 3(vi13)      |  maddw.xyzw vf26, vf26, vf24
  lq.xyzw vf20, 4(vi10)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi13)       |  maddw.xyzw vf27, vf27, vf24
  lq.xyzw vf23, 5(vi10)      |  mulaz.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi13)       |  maddw.xyzw vf28, vf28, vf24
  lq.xyzw vf20, 6(vi10)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyzw vf31, 6(vi13)      |  maddw.xyz vf29, vf29, vf24
  lqi.xyzw vf23, vi02        |  mulaz.xyzw ACC, vf23, vf24
  mtir vi10, vf11.x          |  maddw.xyz vf30, vf30, vf24
  mtir vi13, vf11.y          |  mulaz.xyzw ACC, vf20, vf24
  b L39                      |  maddw.xyzw vf31, vf31, vf24
  lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
L54:
  ibgez vi09, L55            |  mulaz.xyzw ACC, vf29, vf08
  sq.xyzw vf21, 2(vi11)      |  maddaz.xyzw ACC, vf30, vf11
  nop                        |  ftoi4.xyzw vf21, vf09
L55:
  mfp.w vf20, P              |  maddz.xyz vf11, vf31, vf14
  sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  sq.xyzw vf15, 0(vi14)      |  miniw.w vf10, vf10, vf03
  sq.xyzw vf21, 2(vi14)      |  mulaw.xyzw ACC, vf25, vf08
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf13, vf13, vf20
  erleng.xyz P, vf11         |  ftoi0.xyzw vf12, vf12
  ibne vi06, vi03, L56       |  maddaw.xyzw ACC, vf26, vf11
  mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  ibne vi07, vi03, L77       |  nop
  nop                        |  nop
  b L153                     |  nop
  nop                        |  nop
L56:
  lqi.xyzw vf09, vi01        |  mulax.xyzw ACC, vf01, vf13
  sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  lqi.xyzw vf15, vi01        |  maxw.w vf10, vf10, vf02
  mtir vi11, vf09.x          |  itof0.xyzw vf23, vf23
  mtir vi14, vf09.y          |  maxx.xyzw vf13, vf13, vf00
  div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  move.xyzw vf21, vf10       |  add.xyzw vf12, vf12, vf18
  iand vi11, vi11, vi05      |  add.xyzw vf15, vf15, vf19
  nop                        |  mulax.xyzw ACC, vf04, vf13
  ibgtz vi09, L57            |  madday.xyzw ACC, vf05, vf13
  iand vi14, vi14, vi05      |  maddaz.xyzw ACC, vf06, vf13
  nop                        |  addx.w vf21, vf21, vf17
L57:
  ibne vi05, vi11, L58       |  maddw.xyzw vf13, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf08, vf08, Q
  mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  mtir vi14, vf12.y          |  ftoi4.xyzw vf21, vf21
  b L59                      |  mul.xyzw vf13, vf13, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
L58:
  lq.xyzw vf20, 0(vi11)      |  mul.xyzw vf14, vf14, Q
  lq.xyzw vf25, 0(vi14)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf23, 1(vi11)      |  mul.xyzw vf13, vf13, vf23
  lq.xyzw vf26, 1(vi14)      |  add.xyzw vf08, vf08, vf22
  lq.xyzw vf20, 2(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyzw vf27, 2(vi14)      |  maddw.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 3(vi11)      |  mulaz.xyzw ACC, vf23, vf24
  lq.xyzw vf28, 3(vi14)      |  maddw.xyzw vf26, vf26, vf24
  lq.xyzw vf20, 4(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi14)       |  maddw.xyzw vf27, vf27, vf24
  lq.xyzw vf23, 5(vi11)      |  mulaz.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi14)       |  maddw.xyzw vf28, vf28, vf24
  lq.xyzw vf20, 6(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyzw vf31, 6(vi14)      |  maddw.xyz vf29, vf29, vf24
  lqi.xyzw vf23, vi02        |  mulaz.xyzw ACC, vf23, vf24
  mtir vi11, vf12.x          |  maddw.xyz vf30, vf30, vf24
  mtir vi14, vf12.y          |  mulaz.xyzw ACC, vf20, vf24
  b L44                      |  maddw.xyzw vf31, vf31, vf24
  lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
L59:
  ibgez vi09, L60            |  mulaz.xyzw ACC, vf29, vf09
  sq.xyzw vf21, 2(vi12)      |  maddaz.xyzw ACC, vf30, vf12
  nop                        |  ftoi4.xyzw vf21, vf10
L60:
  mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  sq.xyzw vf16, 0(vi15)      |  miniw.w vf08, vf08, vf03
  sq.xyzw vf21, 2(vi15)      |  mulaw.xyzw ACC, vf25, vf09
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20
  erleng.xyz P, vf12         |  ftoi0.xyzw vf13, vf13
  ibne vi06, vi03, L46       |  maddaw.xyzw ACC, vf26, vf12
  mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  ibne vi07, vi03, L72       |  nop
  nop                        |  nop
  b L163                     |  nop
  nop                        |  nop

;; maybe mat3s
L61:
  lqi.xyzw vf08, vi01        |  nop
  lqi.xyzw vf24, vi02        |  nop
  lqi.xyzw vf11, vi01        |  nop
  lqi.xyzw vf14, vi01        |  nop
  mtir vi10, vf08.x          |  nop
  mtir vi13, vf08.y          |  itof0.xyzw vf24, vf24
  nop                        |  add.zw vf08, vf08, vf17
  nop                        |  add.xyzw vf11, vf11, vf18
  iand vi10, vi10, vi05      |  add.xyzw vf14, vf14, vf19
  ilw.w vi08, -1(vi02)       |  mulw.xyzw vf24, vf24, vf29
  iand vi13, vi13, vi05      |  nop
  lq.xyzw vf20, 0(vi10)      |  nop
  lq.xyzw vf31, 0(vi13)      |  nop
  lq.xyzw vf25, 0(vi08)      |  nop
  lq.xyzw vf23, 1(vi10)      |  nop
  lq.xyzw vf20, 1(vi13)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf26, 1(vi08)      |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 2(vi10)      |  maddz.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 2(vi13)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf27, 2(vi08)      |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 3(vi10)      |  maddz.xyzw vf26, vf26, vf24
  lq.xyzw vf31, 3(vi13)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf28, 3(vi08)      |  madday.xyzw ACC, vf23, vf24
  lq.xyzw vf23, 4(vi10)      |  maddz.xyzw vf27, vf27, vf24
  lq.xyzw vf20, 4(vi13)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi08)       |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 5(vi10)      |  maddz.xyzw vf28, vf28, vf24
  lq.xyzw vf23, 5(vi13)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi08)       |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 6(vi10)      |  maddz.xyz vf29, vf29, vf24
  lq.xyzw vf22, 6(vi13)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 6(vi08)      |  madday.xyzw ACC, vf23, vf24
  lqi.xyzw vf23, vi02        |  maddz.xyz vf30, vf30, vf24
  mtir vi10, vf11.x          |  mulax.xyzw ACC, vf20, vf24
  mtir vi13, vf11.y          |  madday.xyzw ACC, vf22, vf24
  lq.xyzw vf22, 2(vi00)      |  maddz.xyzw vf31, vf31, vf24
  nop                        |  itof0.xyzw vf24, vf23
  nop                        |  mulaz.xyzw ACC, vf29, vf08
  nop                        |  maddaz.xyzw ACC, vf30, vf11
  nop                        |  maddz.xyz vf11, vf31, vf14
  nop                        |  nop
  nop                        |  nop
  nop                        |  mulaw.xyzw ACC, vf25, vf08
  nop                        |  nop
  erleng.xyz P, vf11         |  nop
  ior vi15, vi07, vi00       |  maddaw.xyzw ACC, vf26, vf11
  mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  lqi.xyzw vf09, vi01        |  nop
  ilwr.y vi03, vi12          |  nop
  ilw.z vi07, 1(vi12)        |  nop
  lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  lqi.xyzw vf15, vi01        |  nop
  mtir vi11, vf09.x          |  nop
  ibeq vi00, vi15, L62       |  nop
  mtir vi14, vf09.y          |  nop
  nop                        |  miniw.w vf08, vf08, vf01
L62:
  div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  iadd vi03, vi03, vi12      |  add.xyzw vf12, vf12, vf18
  iand vi11, vi11, vi05      |  add.xyzw vf15, vf15, vf19
  ilw.w vi08, -1(vi02)       |  nop
  iadd vi07, vi07, vi03      |  nop
  iand vi14, vi14, vi05      |  nop
  ibne vi05, vi11, L63       |  nop
  iaddi vi07, vi07, -0x1     |  mul.xyz vf08, vf08, Q
  mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  mtir vi14, vf12.y          |  nop
  b L64                      |  nop
  lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
L63:
  lq.xyzw vf20, 0(vi11)      |  mul.xyzw vf14, vf14, Q
  nop                        |  mulw.xyzw vf24, vf24, vf29
  lq.xyzw vf31, 0(vi14)      |  nop
  lq.xyzw vf25, 0(vi08)      |  nop
  lq.xyzw vf23, 1(vi11)      |  add.xyzw vf08, vf08, vf22
  lq.xyzw vf20, 1(vi14)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf26, 1(vi08)      |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 2(vi11)      |  maddz.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 2(vi14)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf27, 2(vi08)      |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 3(vi11)      |  maddz.xyzw vf26, vf26, vf24
  lq.xyzw vf31, 3(vi14)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf28, 3(vi08)      |  madday.xyzw ACC, vf23, vf24
  lq.xyzw vf23, 4(vi11)      |  maddz.xyzw vf27, vf27, vf24
  lq.xyzw vf20, 4(vi14)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi08)       |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 5(vi11)      |  maddz.xyzw vf28, vf28, vf24
  lq.xyzw vf23, 5(vi14)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi08)       |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 6(vi11)      |  maddz.xyz vf29, vf29, vf24
  lq.xyzw vf22, 6(vi14)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 6(vi08)      |  madday.xyzw ACC, vf23, vf24
  lqi.xyzw vf23, vi02        |  maddz.xyz vf30, vf30, vf24
  mtir vi11, vf12.x          |  mulax.xyzw ACC, vf20, vf24
  mtir vi14, vf12.y          |  madday.xyzw ACC, vf22, vf24
  lq.xyzw vf22, 2(vi00)      |  maddz.xyzw vf31, vf31, vf24
  lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
L64:
  nop                        |  mulaz.xyzw ACC, vf29, vf09
  nop                        |  maddaz.xyzw ACC, vf30, vf12
  mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  nop                        |  nop
  1024.0                     |  miniw.w vf08, vf08, vf03 :i
  ibne vi00, vi15, L125      |  mulaw.xyzw ACC, vf25, vf09
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20
  erleng.xyz P, vf12         |  nop
  nop                        |  maddaw.xyzw ACC, vf26, vf12
  mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
L65:
  lqi.xyzw vf10, vi01        |  mulax.xyzw ACC, vf01, vf11
  b L67                      |  madday.xyzw ACC, vf02, vf11
  nop                        |  maddz.xyzw vf11, vf03, vf11
L66: ;; mat3 cross entry
  lqi.xyzw vf10, vi01        |  mulax.xyzw ACC, vf01, vf11
  sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
L67:
  lqi.xyzw vf13, vi01        |  add.xyzw vf09, vf09, vf28
  lqi.xyzw vf16, vi01        |  maxw.w vf08, vf08, vf02
  mtir vi12, vf10.x          |  itof0.xyzw vf23, vf23
  mtir vi15, vf10.y          |  maxx.xyzw vf11, vf11, vf00
  div Q, vf01.w, vf09.w      |  add.zw vf10, vf10, vf17
  move.xyzw vf21, vf08       |  add.xyzw vf13, vf13, vf18
  iand vi12, vi12, vi05      |  add.xyzw vf16, vf16, vf19
  ilw.w vi08, -1(vi02)       |  mulax.xyzw ACC, vf04, vf11
  ibgtz vi09, L68            |  madday.xyzw ACC, vf05, vf11
  iand vi15, vi15, vi05      |  maddaz.xyzw ACC, vf06, vf11
  nop                        |  addx.w vf21, vf21, vf17
L68:
  ibne vi05, vi12, L69       |  maddw.xyzw vf11, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf09, vf09, Q
  mtir vi12, vf13.x          |  mul.xyzw vf15, vf15, Q
  mtir vi15, vf13.y          |  ftoi4.xyzw vf21, vf21
  b L70                      |  mul.xyzw vf11, vf11, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf09, vf09, vf22
L69:
  lq.xyzw vf20, 0(vi12)      |  mul.xyzw vf15, vf15, Q
  nop                        |  mulw.xyzw vf24, vf24, vf29
  lq.xyzw vf31, 0(vi15)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf25, 0(vi08)      |  mul.xyzw vf11, vf11, vf23
  lq.xyzw vf23, 1(vi12)      |  add.xyzw vf09, vf09, vf22
  lq.xyzw vf20, 1(vi15)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf26, 1(vi08)      |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 2(vi12)      |  maddz.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 2(vi15)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf27, 2(vi08)      |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 3(vi12)      |  maddz.xyzw vf26, vf26, vf24
  lq.xyzw vf31, 3(vi15)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf28, 3(vi08)      |  madday.xyzw ACC, vf23, vf24
  lq.xyzw vf23, 4(vi12)      |  maddz.xyzw vf27, vf27, vf24
  lq.xyzw vf20, 4(vi15)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi08)       |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 5(vi12)      |  maddz.xyzw vf28, vf28, vf24
  lq.xyzw vf23, 5(vi15)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi08)       |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 6(vi12)      |  maddz.xyz vf29, vf29, vf24
  lq.xyzw vf22, 6(vi15)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 6(vi08)      |  madday.xyzw ACC, vf23, vf24
  lqi.xyzw vf23, vi02        |  maddz.xyz vf30, vf30, vf24
  mtir vi12, vf13.x          |  mulax.xyzw ACC, vf20, vf24
  mtir vi15, vf13.y          |  madday.xyzw ACC, vf22, vf24
  lq.xyzw vf22, 2(vi00)      |  maddz.xyzw vf31, vf31, vf24
  lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
L70:
  ibgez vi09, L71            |  mulaz.xyzw ACC, vf29, vf10
  sq.xyzw vf21, 2(vi10)      |  maddaz.xyzw ACC, vf30, vf13
  nop                        |  ftoi4.xyzw vf21, vf08
L71:
  mfp.w vf20, P              |  maddz.xyz vf13, vf31, vf16
  sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  sq.xyzw vf14, 0(vi13)      |  miniw.w vf09, vf09, vf03
  sq.xyzw vf21, 2(vi13)      |  mulaw.xyzw ACC, vf25, vf10
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf12, vf12, vf20
  erleng.xyz P, vf13         |  ftoi0.xyzw vf11, vf11
  ibeq vi07, vi03, L143      |  maddaw.xyzw ACC, vf26, vf13
  mr32.z vf16, vf00          |  maddw.xyzw vf10, vf27, vf16
L72: ;; mat3 cross entry
  lqi.xyzw vf08, vi01        |  mulax.xyzw ACC, vf01, vf12
  sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  lqi.xyzw vf11, vi01        |  add.xyzw vf10, vf10, vf28
  lqi.xyzw vf14, vi01        |  maxw.w vf09, vf09, vf02
  mtir vi10, vf08.x          |  itof0.xyzw vf23, vf23
  mtir vi13, vf08.y          |  maxx.xyzw vf12, vf12, vf00
  div Q, vf01.w, vf10.w      |  add.zw vf08, vf08, vf17
  move.xyzw vf21, vf09       |  add.xyzw vf11, vf11, vf18
  iand vi10, vi10, vi05      |  add.xyzw vf14, vf14, vf19
  ilw.w vi08, -1(vi02)       |  mulax.xyzw ACC, vf04, vf12
  ibgtz vi09, L73            |  madday.xyzw ACC, vf05, vf12
  iand vi13, vi13, vi05      |  maddaz.xyzw ACC, vf06, vf12
  nop                        |  addx.w vf21, vf21, vf17
L73:
  ibne vi05, vi10, L74       |  maddw.xyzw vf12, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf10, vf10, Q
  mtir vi10, vf11.x          |  mul.xyzw vf16, vf16, Q
  mtir vi13, vf11.y          |  ftoi4.xyzw vf21, vf21
  b L75                      |  mul.xyzw vf12, vf12, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf10, vf10, vf22
L74:
  lq.xyzw vf20, 0(vi10)      |  mul.xyzw vf16, vf16, Q
  nop                        |  mulw.xyzw vf24, vf24, vf29
  lq.xyzw vf31, 0(vi13)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf25, 0(vi08)      |  mul.xyzw vf12, vf12, vf23
  lq.xyzw vf23, 1(vi10)      |  add.xyzw vf10, vf10, vf22
  lq.xyzw vf20, 1(vi13)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf26, 1(vi08)      |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 2(vi10)      |  maddz.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 2(vi13)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf27, 2(vi08)      |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 3(vi10)      |  maddz.xyzw vf26, vf26, vf24
  lq.xyzw vf31, 3(vi13)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf28, 3(vi08)      |  madday.xyzw ACC, vf23, vf24
  lq.xyzw vf23, 4(vi10)      |  maddz.xyzw vf27, vf27, vf24
  lq.xyzw vf20, 4(vi13)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi08)       |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 5(vi10)      |  maddz.xyzw vf28, vf28, vf24
  lq.xyzw vf23, 5(vi13)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi08)       |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 6(vi10)      |  maddz.xyz vf29, vf29, vf24
  lq.xyzw vf22, 6(vi13)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 6(vi08)      |  madday.xyzw ACC, vf23, vf24
  lqi.xyzw vf23, vi02        |  maddz.xyz vf30, vf30, vf24
  mtir vi10, vf11.x          |  mulax.xyzw ACC, vf20, vf24
  mtir vi13, vf11.y          |  madday.xyzw ACC, vf22, vf24
  lq.xyzw vf22, 2(vi00)      |  maddz.xyzw vf31, vf31, vf24
  lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
L75:
  ibgez vi09, L76            |  mulaz.xyzw ACC, vf29, vf08
  sq.xyzw vf21, 2(vi11)      |  maddaz.xyzw ACC, vf30, vf11
  nop                        |  ftoi4.xyzw vf21, vf09
L76:
  mfp.w vf20, P              |  maddz.xyz vf11, vf31, vf14
  sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  sq.xyzw vf15, 0(vi14)      |  miniw.w vf10, vf10, vf03
  sq.xyzw vf21, 2(vi14)      |  mulaw.xyzw ACC, vf25, vf08
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf13, vf13, vf20
  erleng.xyz P, vf11         |  ftoi0.xyzw vf12, vf12
  ibeq vi07, vi03, L153      |  maddaw.xyzw ACC, vf26, vf11
  mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
L77: ;; mat3 cross entry
  lqi.xyzw vf09, vi01        |  mulax.xyzw ACC, vf01, vf13
  sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  lqi.xyzw vf15, vi01        |  maxw.w vf10, vf10, vf02
  mtir vi11, vf09.x          |  itof0.xyzw vf23, vf23
  mtir vi14, vf09.y          |  maxx.xyzw vf13, vf13, vf00
  div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  move.xyzw vf21, vf10       |  add.xyzw vf12, vf12, vf18
  iand vi11, vi11, vi05      |  add.xyzw vf15, vf15, vf19
  ilw.w vi08, -1(vi02)       |  mulax.xyzw ACC, vf04, vf13
  ibgtz vi09, L78            |  madday.xyzw ACC, vf05, vf13
  iand vi14, vi14, vi05      |  maddaz.xyzw ACC, vf06, vf13
  nop                        |  addx.w vf21, vf21, vf17
L78:
  ibne vi05, vi11, L79       |  maddw.xyzw vf13, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf08, vf08, Q
  mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  mtir vi14, vf12.y          |  ftoi4.xyzw vf21, vf21
  b L80                      |  mul.xyzw vf13, vf13, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
L79:
  lq.xyzw vf20, 0(vi11)      |  mul.xyzw vf14, vf14, Q
  nop                        |  mulw.xyzw vf24, vf24, vf29
  lq.xyzw vf31, 0(vi14)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf25, 0(vi08)      |  mul.xyzw vf13, vf13, vf23
  lq.xyzw vf23, 1(vi11)      |  add.xyzw vf08, vf08, vf22
  lq.xyzw vf20, 1(vi14)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf26, 1(vi08)      |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 2(vi11)      |  maddz.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 2(vi14)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf27, 2(vi08)      |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 3(vi11)      |  maddz.xyzw vf26, vf26, vf24
  lq.xyzw vf31, 3(vi14)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf28, 3(vi08)      |  madday.xyzw ACC, vf23, vf24
  lq.xyzw vf23, 4(vi11)      |  maddz.xyzw vf27, vf27, vf24
  lq.xyzw vf20, 4(vi14)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi08)       |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 5(vi11)      |  maddz.xyzw vf28, vf28, vf24
  lq.xyzw vf23, 5(vi14)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi08)       |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 6(vi11)      |  maddz.xyz vf29, vf29, vf24
  lq.xyzw vf22, 6(vi14)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 6(vi08)      |  madday.xyzw ACC, vf23, vf24
  lqi.xyzw vf23, vi02        |  maddz.xyz vf30, vf30, vf24
  mtir vi11, vf12.x          |  mulax.xyzw ACC, vf20, vf24
  mtir vi14, vf12.y          |  madday.xyzw ACC, vf22, vf24
  lq.xyzw vf22, 2(vi00)      |  maddz.xyzw vf31, vf31, vf24
  lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
L80:
  ibgez vi09, L81            |  mulaz.xyzw ACC, vf29, vf09
  sq.xyzw vf21, 2(vi12)      |  maddaz.xyzw ACC, vf30, vf12
  nop                        |  ftoi4.xyzw vf21, vf10
L81:
  mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  sq.xyzw vf16, 0(vi15)      |  miniw.w vf08, vf08, vf03
  sq.xyzw vf21, 2(vi15)      |  mulaw.xyzw ACC, vf25, vf09
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20
  erleng.xyz P, vf12         |  ftoi0.xyzw vf13, vf13
  ibne vi07, vi03, L66       |  maddaw.xyzw ACC, vf26, vf12
  mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  b L163                     |  nop
  nop                        |  nop

;; merc prime, or other weird things.
L82:
  erleng.xyz P, vf12         |  maxi.xy vf08, vf08, I
  3072.0                     |  nop :i
  nop                        |  minii.xy vf08, vf08, I
  nop                        |  maddaw.xyzw ACC, vf26, vf12
  mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  lqi.xyzw vf10, vi01        |  mulax.xyzw ACC, vf01, vf11
  ibne vi04, vi03, L84       |  madday.xyzw ACC, vf02, vf11
  nop                        |  maddz.xyzw vf11, vf03, vf11
  ibne vi06, vi03, L95       |  nop
  nop                        |  nop
  b L128                     |  nop
  nop                        |  nop
L83:
  3072.0                     |  mulax.xyzw ACC, vf01, vf11 :i
  lqi.xyzw vf10, vi01        |  minii.xy vf08, vf08, I
  sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
L84:
  lqi.xyzw vf13, vi01        |  add.xyzw vf09, vf09, vf28
  lqi.xyzw vf16, vi01        |  maxw.w vf08, vf08, vf02
  mtir vi08, vf10.x          |  itof0.xyzw vf23, vf23
  ilw.y vi09, -9(vi01)       |  maxx.xyzw vf11, vf11, vf00
  nop                        |  miniw.w vf09, vf09, vf01
  div Q, vf01.w, vf09.w      |  add.zw vf10, vf10, vf17
  move.xyzw vf21, vf08       |  add.xyzw vf13, vf13, vf18
  lq.xyz vf29, 4(vi08)       |  add.xyzw vf16, vf16, vf19
  lq.xyz vf30, 5(vi08)       |  mulax.xyzw ACC, vf04, vf11
  ibgtz vi09, L85            |  madday.xyzw ACC, vf05, vf11
  lq.xyzw vf31, 6(vi08)      |  maddaz.xyzw ACC, vf06, vf11
  nop                        |  addx.w vf21, vf21, vf17
L85:
  lq.xyzw vf25, 0(vi08)      |  maddw.xyzw vf11, vf07, vf00
  lq.xyzw vf26, 1(vi08)      |  mul.xyz vf09, vf09, Q
  mtir vi12, vf13.x          |  mul.xyzw vf15, vf15, Q
  mtir vi15, vf13.y          |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf27, 2(vi08)      |  mul.xyzw vf11, vf11, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf09, vf09, vf22
  ibne vi00, vi09, L86       |  mulaz.xyzw ACC, vf29, vf10
  sq.xyzw vf21, 2(vi10)      |  maddaz.xyzw ACC, vf30, vf13
  nop                        |  ftoi4.xyzw vf21, vf08
L86:
  mfp.w vf20, P              |  maddz.xyz vf13, vf31, vf16
  sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  sq.xyzw vf14, 0(vi13)      |  miniw.w vf09, vf09, vf03
  sq.xyzw vf21, 2(vi13)      |  mulaw.xyzw ACC, vf25, vf10
  lq.xyzw vf28, 3(vi08)      |  mulw.xyzw vf12, vf12, vf20
  1024.0                     |  ftoi0.xyzw vf11, vf11 :i
  erleng.xyz P, vf13         |  maxi.xy vf09, vf09, I
  ibne vi04, vi03, L87       |  maddaw.xyzw ACC, vf26, vf13
  mr32.z vf16, vf00          |  maddw.xyzw vf10, vf27, vf16
  ibne vi06, vi03, L100      |  nop
  ilw.y vi09, -6(vi01)       |  nop
  ibne vi07, vi03, L133      |  nop
  nop                        |  nop
  b L143                     |  nop
  nop                        |  nop
L87:
  3072.0                     |  mulax.xyzw ACC, vf01, vf12 :i
  lqi.xyzw vf08, vi01        |  minii.xy vf09, vf09, I
  sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  lqi.xyzw vf11, vi01        |  add.xyzw vf10, vf10, vf28
  lqi.xyzw vf14, vi01        |  maxw.w vf09, vf09, vf02
  mtir vi08, vf08.x          |  itof0.xyzw vf23, vf23
  ilw.y vi09, -9(vi01)       |  maxx.xyzw vf12, vf12, vf00
  nop                        |  miniw.w vf10, vf10, vf01
  div Q, vf01.w, vf10.w      |  add.zw vf08, vf08, vf17
  move.xyzw vf21, vf09       |  add.xyzw vf11, vf11, vf18
  lq.xyz vf29, 4(vi08)       |  add.xyzw vf14, vf14, vf19
  lq.xyz vf30, 5(vi08)       |  mulax.xyzw ACC, vf04, vf12
  ibgtz vi09, L88            |  madday.xyzw ACC, vf05, vf12
  lq.xyzw vf31, 6(vi08)      |  maddaz.xyzw ACC, vf06, vf12
  nop                        |  addx.w vf21, vf21, vf17
L88:
  lq.xyzw vf25, 0(vi08)      |  maddw.xyzw vf12, vf07, vf00
  lq.xyzw vf26, 1(vi08)      |  mul.xyz vf10, vf10, Q
  mtir vi10, vf11.x          |  mul.xyzw vf16, vf16, Q
  mtir vi13, vf11.y          |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf27, 2(vi08)      |  mul.xyzw vf12, vf12, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf10, vf10, vf22
  ibne vi00, vi09, L89       |  mulaz.xyzw ACC, vf29, vf08
  sq.xyzw vf21, 2(vi11)      |  maddaz.xyzw ACC, vf30, vf11
  nop                        |  ftoi4.xyzw vf21, vf09
L89:
  mfp.w vf20, P              |  maddz.xyz vf11, vf31, vf14
  sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  sq.xyzw vf15, 0(vi14)      |  miniw.w vf10, vf10, vf03
  sq.xyzw vf21, 2(vi14)      |  mulaw.xyzw ACC, vf25, vf08
  lq.xyzw vf28, 3(vi08)      |  mulw.xyzw vf13, vf13, vf20
  1024.0                     |  ftoi0.xyzw vf12, vf12 :i
  erleng.xyz P, vf11         |  maxi.xy vf10, vf10, I
  ibne vi04, vi03, L90       |  maddaw.xyzw ACC, vf26, vf11
  mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  ibne vi06, vi03, L105      |  nop
  ilw.y vi09, -6(vi01)       |  nop
  ibne vi07, vi03, L138      |  nop
  nop                        |  nop
  b L153                     |  nop
  nop                        |  nop
L90:
  3072.0                     |  mulax.xyzw ACC, vf01, vf13 :i
  lqi.xyzw vf09, vi01        |  minii.xy vf10, vf10, I
  sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  lqi.xyzw vf15, vi01        |  maxw.w vf10, vf10, vf02
  mtir vi08, vf09.x          |  itof0.xyzw vf23, vf23
  ilw.y vi09, -9(vi01)       |  maxx.xyzw vf13, vf13, vf00
  nop                        |  miniw.w vf08, vf08, vf01
  div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  move.xyzw vf21, vf10       |  add.xyzw vf12, vf12, vf18
  lq.xyz vf29, 4(vi08)       |  add.xyzw vf15, vf15, vf19
  lq.xyz vf30, 5(vi08)       |  mulax.xyzw ACC, vf04, vf13
  ibgtz vi09, L91            |  madday.xyzw ACC, vf05, vf13
  lq.xyzw vf31, 6(vi08)      |  maddaz.xyzw ACC, vf06, vf13
  nop                        |  addx.w vf21, vf21, vf17
L91:
  lq.xyzw vf25, 0(vi08)      |  maddw.xyzw vf13, vf07, vf00
  lq.xyzw vf26, 1(vi08)      |  mul.xyz vf08, vf08, Q
  mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  mtir vi14, vf12.y          |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf27, 2(vi08)      |  mul.xyzw vf13, vf13, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
  ibne vi00, vi09, L92       |  mulaz.xyzw ACC, vf29, vf09
  sq.xyzw vf21, 2(vi12)      |  maddaz.xyzw ACC, vf30, vf12
  nop                        |  ftoi4.xyzw vf21, vf10
L92:
  mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  sq.xyzw vf16, 0(vi15)      |  miniw.w vf08, vf08, vf03
  sq.xyzw vf21, 2(vi15)      |  mulaw.xyzw ACC, vf25, vf09
  lq.xyzw vf28, 3(vi08)      |  mulw.xyzw vf11, vf11, vf20
  1024.0                     |  ftoi0.xyzw vf13, vf13 :i
  erleng.xyz P, vf12         |  maxi.xy vf08, vf08, I
  ibne vi04, vi03, L83       |  maddaw.xyzw ACC, vf26, vf12
  mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  ibne vi06, vi03, L94       |  nop
  ilw.y vi09, -6(vi01)       |  nop
  ibne vi07, vi03, L127      |  nop
  nop                        |  nop
  b L163                     |  nop
  nop                        |  nop
L93:
  erleng.xyz P, vf12         |  maxi.xy vf08, vf08, I
  3072.0                     |  nop :i
  nop                        |  minii.xy vf08, vf08, I
  ibeq vi06, vi03, L126      |  maddaw.xyzw ACC, vf26, vf12
  mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  lqi.xyzw vf10, vi01        |  mulax.xyzw ACC, vf01, vf11
  jr vi08                    |  madday.xyzw ACC, vf02, vf11
  nop                        |  maddz.xyzw vf11, vf03, vf11
L94:
  3072.0                     |  mulax.xyzw ACC, vf01, vf11 :i
  lqi.xyzw vf10, vi01        |  minii.xy vf08, vf08, I
  sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
L95:
  lqi.xyzw vf13, vi01        |  add.xyzw vf09, vf09, vf28
  lqi.xyzw vf16, vi01        |  maxw.w vf08, vf08, vf02
  mtir vi12, vf10.x          |  itof0.xyzw vf23, vf23
  mtir vi15, vf10.y          |  maxx.xyzw vf11, vf11, vf00
  nop                        |  miniw.w vf09, vf09, vf01
  div Q, vf01.w, vf09.w      |  add.zw vf10, vf10, vf17
  move.xyzw vf21, vf08       |  add.xyzw vf13, vf13, vf18
  iand vi12, vi12, vi05      |  add.xyzw vf16, vf16, vf19
  nop                        |  mulax.xyzw ACC, vf04, vf11
  ibgtz vi09, L96            |  madday.xyzw ACC, vf05, vf11
  iand vi15, vi15, vi05      |  maddaz.xyzw ACC, vf06, vf11
  nop                        |  addx.w vf21, vf21, vf17
L96:
  ibne vi05, vi12, L97       |  maddw.xyzw vf11, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf09, vf09, Q
  mtir vi12, vf13.x          |  mul.xyzw vf15, vf15, Q
  mtir vi15, vf13.y          |  ftoi4.xyzw vf21, vf21
  b L98                      |  mul.xyzw vf11, vf11, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf09, vf09, vf22
L97:
  lq.xyzw vf20, 0(vi12)      |  mul.xyzw vf15, vf15, Q
  nop                        |  mulw.xyzw vf24, vf24, vf29
  lq.xyzw vf25, 0(vi15)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf23, 1(vi12)      |  mul.xyzw vf11, vf11, vf23
  lq.xyzw vf26, 1(vi15)      |  add.xyzw vf09, vf09, vf22
  lq.xyzw vf20, 2(vi12)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf27, 2(vi15)      |  maddy.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 3(vi12)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf28, 3(vi15)      |  maddy.xyzw vf26, vf26, vf24
  lq.xyzw vf20, 4(vi12)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi15)       |  maddy.xyzw vf27, vf27, vf24
  lq.xyzw vf23, 5(vi12)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi15)       |  maddy.xyzw vf28, vf28, vf24
  lq.xyzw vf20, 6(vi12)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf31, 6(vi15)      |  maddy.xyz vf29, vf29, vf24
  mtir vi12, vf13.x          |  mulax.xyzw ACC, vf23, vf24
  mtir vi15, vf13.y          |  maddy.xyz vf30, vf30, vf24
  b L113                     |  mulax.xyzw ACC, vf20, vf24
  lqi.xyzw vf23, vi03        |  maddy.xyzw vf31, vf31, vf24
L98:
  ibgez vi09, L99            |  mulaz.xyzw ACC, vf29, vf10
  sq.xyzw vf21, 2(vi10)      |  maddaz.xyzw ACC, vf30, vf13
  nop                        |  ftoi4.xyzw vf21, vf08
L99:
  mfp.w vf20, P              |  maddz.xyz vf13, vf31, vf16
  sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  sq.xyzw vf14, 0(vi13)      |  miniw.w vf09, vf09, vf03
  sq.xyzw vf21, 2(vi13)      |  mulaw.xyzw ACC, vf25, vf10
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf12, vf12, vf20
  1024.0                     |  ftoi0.xyzw vf11, vf11 :i
  erleng.xyz P, vf13         |  maxi.xy vf09, vf09, I
  ibne vi06, vi03, L100      |  maddaw.xyzw ACC, vf26, vf13
  mr32.z vf16, vf00          |  maddw.xyzw vf10, vf27, vf16
  ibne vi07, vi03, L133      |  nop
  nop                        |  nop
  b L143                     |  nop
  nop                        |  nop
L100:
  3072.0                     |  mulax.xyzw ACC, vf01, vf12 :i
  lqi.xyzw vf08, vi01        |  minii.xy vf09, vf09, I
  sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  lqi.xyzw vf11, vi01        |  add.xyzw vf10, vf10, vf28
  lqi.xyzw vf14, vi01        |  maxw.w vf09, vf09, vf02
  mtir vi10, vf08.x          |  itof0.xyzw vf23, vf23
  mtir vi13, vf08.y          |  maxx.xyzw vf12, vf12, vf00
  nop                        |  miniw.w vf10, vf10, vf01
  div Q, vf01.w, vf10.w      |  add.zw vf08, vf08, vf17
  move.xyzw vf21, vf09       |  add.xyzw vf11, vf11, vf18
  iand vi10, vi10, vi05      |  add.xyzw vf14, vf14, vf19
  nop                        |  mulax.xyzw ACC, vf04, vf12
  ibgtz vi09, L101           |  madday.xyzw ACC, vf05, vf12
  iand vi13, vi13, vi05      |  maddaz.xyzw ACC, vf06, vf12
  nop                        |  addx.w vf21, vf21, vf17
L101:
  ibne vi05, vi10, L102      |  maddw.xyzw vf12, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf10, vf10, Q
  mtir vi10, vf11.x          |  mul.xyzw vf16, vf16, Q
  mtir vi13, vf11.y          |  ftoi4.xyzw vf21, vf21
  b L103                     |  mul.xyzw vf12, vf12, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf10, vf10, vf22
L102:
  lq.xyzw vf20, 0(vi10)      |  mul.xyzw vf16, vf16, Q
  nop                        |  mulw.xyzw vf24, vf24, vf29
  lq.xyzw vf25, 0(vi13)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf23, 1(vi10)      |  mul.xyzw vf12, vf12, vf23
  lq.xyzw vf26, 1(vi13)      |  add.xyzw vf10, vf10, vf22
  lq.xyzw vf20, 2(vi10)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf27, 2(vi13)      |  maddy.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 3(vi10)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf28, 3(vi13)      |  maddy.xyzw vf26, vf26, vf24
  lq.xyzw vf20, 4(vi10)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi13)       |  maddy.xyzw vf27, vf27, vf24
  lq.xyzw vf23, 5(vi10)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi13)       |  maddy.xyzw vf28, vf28, vf24
  lq.xyzw vf20, 6(vi10)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf31, 6(vi13)      |  maddy.xyz vf29, vf29, vf24
  mtir vi10, vf11.x          |  mulax.xyzw ACC, vf23, vf24
  mtir vi13, vf11.y          |  maddy.xyz vf30, vf30, vf24
  b L118                     |  mulax.xyzw ACC, vf20, vf24
  lqi.xyzw vf23, vi03        |  maddy.xyzw vf31, vf31, vf24
L103:
  ibgez vi09, L104           |  mulaz.xyzw ACC, vf29, vf08
  sq.xyzw vf21, 2(vi11)      |  maddaz.xyzw ACC, vf30, vf11
  nop                        |  ftoi4.xyzw vf21, vf09
L104:
  mfp.w vf20, P              |  maddz.xyz vf11, vf31, vf14
  sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  sq.xyzw vf15, 0(vi14)      |  miniw.w vf10, vf10, vf03
  sq.xyzw vf21, 2(vi14)      |  mulaw.xyzw ACC, vf25, vf08
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf13, vf13, vf20
  1024.0                     |  ftoi0.xyzw vf12, vf12 :i
  erleng.xyz P, vf11         |  maxi.xy vf10, vf10, I
  ibne vi06, vi03, L105      |  maddaw.xyzw ACC, vf26, vf11
  mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  ibne vi07, vi03, L138      |  nop
  nop                        |  nop
  b L153                     |  nop
  nop                        |  nop
L105:
  3072.0                     |  mulax.xyzw ACC, vf01, vf13 :i
  lqi.xyzw vf09, vi01        |  minii.xy vf10, vf10, I
  sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  lqi.xyzw vf15, vi01        |  maxw.w vf10, vf10, vf02
  mtir vi11, vf09.x          |  itof0.xyzw vf23, vf23
  mtir vi14, vf09.y          |  maxx.xyzw vf13, vf13, vf00
  nop                        |  miniw.w vf08, vf08, vf01
  div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  move.xyzw vf21, vf10       |  add.xyzw vf12, vf12, vf18
  iand vi11, vi11, vi05      |  add.xyzw vf15, vf15, vf19
  nop                        |  mulax.xyzw ACC, vf04, vf13
  ibgtz vi09, L106           |  madday.xyzw ACC, vf05, vf13
  iand vi14, vi14, vi05      |  maddaz.xyzw ACC, vf06, vf13
  nop                        |  addx.w vf21, vf21, vf17
L106:
  ibne vi05, vi11, L107      |  maddw.xyzw vf13, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf08, vf08, Q
  mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  mtir vi14, vf12.y          |  ftoi4.xyzw vf21, vf21
  b L108                     |  mul.xyzw vf13, vf13, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
L107:
  lq.xyzw vf20, 0(vi11)      |  mul.xyzw vf14, vf14, Q
  nop                        |  mulw.xyzw vf24, vf24, vf29
  lq.xyzw vf25, 0(vi14)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf23, 1(vi11)      |  mul.xyzw vf13, vf13, vf23
  lq.xyzw vf26, 1(vi14)      |  add.xyzw vf08, vf08, vf22
  lq.xyzw vf20, 2(vi11)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf27, 2(vi14)      |  maddy.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 3(vi11)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf28, 3(vi14)      |  maddy.xyzw vf26, vf26, vf24
  lq.xyzw vf20, 4(vi11)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi14)       |  maddy.xyzw vf27, vf27, vf24
  lq.xyzw vf23, 5(vi11)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi14)       |  maddy.xyzw vf28, vf28, vf24
  lq.xyzw vf20, 6(vi11)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf31, 6(vi14)      |  maddy.xyz vf29, vf29, vf24
  mtir vi11, vf12.x          |  mulax.xyzw ACC, vf23, vf24
  mtir vi14, vf12.y          |  maddy.xyz vf30, vf30, vf24
  b L123                     |  mulax.xyzw ACC, vf20, vf24
  lqi.xyzw vf23, vi03        |  maddy.xyzw vf31, vf31, vf24
L108:
  ibgez vi09, L109           |  mulaz.xyzw ACC, vf29, vf09
  sq.xyzw vf21, 2(vi12)      |  maddaz.xyzw ACC, vf30, vf12
  nop                        |  ftoi4.xyzw vf21, vf10
L109:
  mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  sq.xyzw vf16, 0(vi15)      |  miniw.w vf08, vf08, vf03
  sq.xyzw vf21, 2(vi15)      |  mulaw.xyzw ACC, vf25, vf09
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20
  1024.0                     |  ftoi0.xyzw vf13, vf13 :i
  erleng.xyz P, vf12         |  maxi.xy vf08, vf08, I
  ibne vi06, vi03, L94       |  maddaw.xyzw ACC, vf26, vf12
  mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  ibne vi07, vi03, L127      |  nop
  nop                        |  nop
  b L163                     |  nop
  nop                        |  nop
L110:
  3072.0                     |  mulax.xyzw ACC, vf01, vf11 :i
  lqi.xyzw vf10, vi01        |  minii.xy vf08, vf08, I
  sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
  lqi.xyzw vf13, vi01        |  add.xyzw vf09, vf09, vf28
  lqi.xyzw vf16, vi01        |  maxw.w vf08, vf08, vf02
  mtir vi12, vf10.x          |  itof0.xyzw vf23, vf23
  mtir vi15, vf10.y          |  maxx.xyzw vf11, vf11, vf00
  nop                        |  miniw.w vf09, vf09, vf01
  div Q, vf01.w, vf09.w      |  add.zw vf10, vf10, vf17
  move.xyzw vf21, vf08       |  add.xyzw vf13, vf13, vf18
  iand vi12, vi12, vi05      |  add.xyzw vf16, vf16, vf19
  nop                        |  mulax.xyzw ACC, vf04, vf11
  ibgtz vi09, L111           |  madday.xyzw ACC, vf05, vf11
  iand vi15, vi15, vi05      |  maddaz.xyzw ACC, vf06, vf11
  nop                        |  addx.w vf21, vf21, vf17
L111:
  ibne vi05, vi12, L112      |  maddw.xyzw vf11, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf09, vf09, Q
  mtir vi12, vf13.x          |  mul.xyzw vf15, vf15, Q
  mtir vi15, vf13.y          |  ftoi4.xyzw vf21, vf21
  b L113                     |  mul.xyzw vf11, vf11, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf09, vf09, vf22
L112:
  lq.xyzw vf20, 0(vi12)      |  mul.xyzw vf15, vf15, Q
  lq.xyzw vf25, 0(vi15)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf23, 1(vi12)      |  mul.xyzw vf11, vf11, vf23
  lq.xyzw vf26, 1(vi15)      |  add.xyzw vf09, vf09, vf22
  lq.xyzw vf20, 2(vi12)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyzw vf27, 2(vi15)      |  maddw.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 3(vi12)      |  mulaz.xyzw ACC, vf23, vf24
  lq.xyzw vf28, 3(vi15)      |  maddw.xyzw vf26, vf26, vf24
  lq.xyzw vf20, 4(vi12)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi15)       |  maddw.xyzw vf27, vf27, vf24
  lq.xyzw vf23, 5(vi12)      |  mulaz.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi15)       |  maddw.xyzw vf28, vf28, vf24
  lq.xyzw vf20, 6(vi12)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyzw vf31, 6(vi15)      |  maddw.xyz vf29, vf29, vf24
  lqi.xyzw vf23, vi02        |  mulaz.xyzw ACC, vf23, vf24
  mtir vi12, vf13.x          |  maddw.xyz vf30, vf30, vf24
  mtir vi15, vf13.y          |  mulaz.xyzw ACC, vf20, vf24
  b L98                      |  maddw.xyzw vf31, vf31, vf24
  lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
L113:
  ibgez vi09, L114           |  mulaz.xyzw ACC, vf29, vf10
  sq.xyzw vf21, 2(vi10)      |  maddaz.xyzw ACC, vf30, vf13
  nop                        |  ftoi4.xyzw vf21, vf08
L114:
  mfp.w vf20, P              |  maddz.xyz vf13, vf31, vf16
  sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  sq.xyzw vf14, 0(vi13)      |  miniw.w vf09, vf09, vf03
  sq.xyzw vf21, 2(vi13)      |  mulaw.xyzw ACC, vf25, vf10
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf12, vf12, vf20
  1024.0                     |  ftoi0.xyzw vf11, vf11 :i
  erleng.xyz P, vf13         |  maxi.xy vf09, vf09, I
  ibne vi06, vi03, L115      |  maddaw.xyzw ACC, vf26, vf13
  mr32.z vf16, vf00          |  maddw.xyzw vf10, vf27, vf16
  ibne vi07, vi03, L133      |  nop
  nop                        |  nop
  b L143                     |  nop
  nop                        |  nop
L115:
  3072.0                     |  mulax.xyzw ACC, vf01, vf12 :i
  lqi.xyzw vf08, vi01        |  minii.xy vf09, vf09, I
  sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  lqi.xyzw vf11, vi01        |  add.xyzw vf10, vf10, vf28
  lqi.xyzw vf14, vi01        |  maxw.w vf09, vf09, vf02
  mtir vi10, vf08.x          |  itof0.xyzw vf23, vf23
  mtir vi13, vf08.y          |  maxx.xyzw vf12, vf12, vf00
  nop                        |  miniw.w vf10, vf10, vf01
  div Q, vf01.w, vf10.w      |  add.zw vf08, vf08, vf17
  move.xyzw vf21, vf09       |  add.xyzw vf11, vf11, vf18
  iand vi10, vi10, vi05      |  add.xyzw vf14, vf14, vf19
  nop                        |  mulax.xyzw ACC, vf04, vf12
  ibgtz vi09, L116           |  madday.xyzw ACC, vf05, vf12
  iand vi13, vi13, vi05      |  maddaz.xyzw ACC, vf06, vf12
  nop                        |  addx.w vf21, vf21, vf17
L116:
  ibne vi05, vi10, L117      |  maddw.xyzw vf12, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf10, vf10, Q
  mtir vi10, vf11.x          |  mul.xyzw vf16, vf16, Q
  mtir vi13, vf11.y          |  ftoi4.xyzw vf21, vf21
  b L118                     |  mul.xyzw vf12, vf12, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf10, vf10, vf22
L117:
  lq.xyzw vf20, 0(vi10)      |  mul.xyzw vf16, vf16, Q
  lq.xyzw vf25, 0(vi13)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf23, 1(vi10)      |  mul.xyzw vf12, vf12, vf23
  lq.xyzw vf26, 1(vi13)      |  add.xyzw vf10, vf10, vf22
  lq.xyzw vf20, 2(vi10)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyzw vf27, 2(vi13)      |  maddw.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 3(vi10)      |  mulaz.xyzw ACC, vf23, vf24
  lq.xyzw vf28, 3(vi13)      |  maddw.xyzw vf26, vf26, vf24
  lq.xyzw vf20, 4(vi10)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi13)       |  maddw.xyzw vf27, vf27, vf24
  lq.xyzw vf23, 5(vi10)      |  mulaz.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi13)       |  maddw.xyzw vf28, vf28, vf24
  lq.xyzw vf20, 6(vi10)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyzw vf31, 6(vi13)      |  maddw.xyz vf29, vf29, vf24
  lqi.xyzw vf23, vi02        |  mulaz.xyzw ACC, vf23, vf24
  mtir vi10, vf11.x          |  maddw.xyz vf30, vf30, vf24
  mtir vi13, vf11.y          |  mulaz.xyzw ACC, vf20, vf24
  b L103                     |  maddw.xyzw vf31, vf31, vf24
  lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
L118:
  ibgez vi09, L119           |  mulaz.xyzw ACC, vf29, vf08
  sq.xyzw vf21, 2(vi11)      |  maddaz.xyzw ACC, vf30, vf11
  nop                        |  ftoi4.xyzw vf21, vf09
L119:
  mfp.w vf20, P              |  maddz.xyz vf11, vf31, vf14
  sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  sq.xyzw vf15, 0(vi14)      |  miniw.w vf10, vf10, vf03
  sq.xyzw vf21, 2(vi14)      |  mulaw.xyzw ACC, vf25, vf08
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf13, vf13, vf20
  1024.0                     |  ftoi0.xyzw vf12, vf12 :i
  erleng.xyz P, vf11         |  maxi.xy vf10, vf10, I
  ibne vi06, vi03, L120      |  maddaw.xyzw ACC, vf26, vf11
  mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  ibne vi07, vi03, L138      |  nop
  nop                        |  nop
  b L153                     |  nop
  nop                        |  nop
L120:
  3072.0                     |  mulax.xyzw ACC, vf01, vf13 :i
  lqi.xyzw vf09, vi01        |  minii.xy vf10, vf10, I
  sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  lqi.xyzw vf15, vi01        |  maxw.w vf10, vf10, vf02
  mtir vi11, vf09.x          |  itof0.xyzw vf23, vf23
  mtir vi14, vf09.y          |  maxx.xyzw vf13, vf13, vf00
  nop                        |  miniw.w vf08, vf08, vf01
  div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  move.xyzw vf21, vf10       |  add.xyzw vf12, vf12, vf18
  iand vi11, vi11, vi05      |  add.xyzw vf15, vf15, vf19
  nop                        |  mulax.xyzw ACC, vf04, vf13
  ibgtz vi09, L121           |  madday.xyzw ACC, vf05, vf13
  iand vi14, vi14, vi05      |  maddaz.xyzw ACC, vf06, vf13
  nop                        |  addx.w vf21, vf21, vf17
L121:
  ibne vi05, vi11, L122      |  maddw.xyzw vf13, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf08, vf08, Q
  mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  mtir vi14, vf12.y          |  ftoi4.xyzw vf21, vf21
  b L123                     |  mul.xyzw vf13, vf13, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
L122:
  lq.xyzw vf20, 0(vi11)      |  mul.xyzw vf14, vf14, Q
  lq.xyzw vf25, 0(vi14)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf23, 1(vi11)      |  mul.xyzw vf13, vf13, vf23
  lq.xyzw vf26, 1(vi14)      |  add.xyzw vf08, vf08, vf22
  lq.xyzw vf20, 2(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyzw vf27, 2(vi14)      |  maddw.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 3(vi11)      |  mulaz.xyzw ACC, vf23, vf24
  lq.xyzw vf28, 3(vi14)      |  maddw.xyzw vf26, vf26, vf24
  lq.xyzw vf20, 4(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi14)       |  maddw.xyzw vf27, vf27, vf24
  lq.xyzw vf23, 5(vi11)      |  mulaz.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi14)       |  maddw.xyzw vf28, vf28, vf24
  lq.xyzw vf20, 6(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyzw vf31, 6(vi14)      |  maddw.xyz vf29, vf29, vf24
  lqi.xyzw vf23, vi02        |  mulaz.xyzw ACC, vf23, vf24
  mtir vi11, vf12.x          |  maddw.xyz vf30, vf30, vf24
  mtir vi14, vf12.y          |  mulaz.xyzw ACC, vf20, vf24
  b L108                     |  maddw.xyzw vf31, vf31, vf24
  lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
L123:
  ibgez vi09, L124           |  mulaz.xyzw ACC, vf29, vf09
  sq.xyzw vf21, 2(vi12)      |  maddaz.xyzw ACC, vf30, vf12
  nop                        |  ftoi4.xyzw vf21, vf10
L124:
  mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  sq.xyzw vf16, 0(vi15)      |  miniw.w vf08, vf08, vf03
  sq.xyzw vf21, 2(vi15)      |  mulaw.xyzw ACC, vf25, vf09
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20
  1024.0                     |  ftoi0.xyzw vf13, vf13 :i
  erleng.xyz P, vf12         |  maxi.xy vf08, vf08, I
  ibne vi06, vi03, L110      |  maddaw.xyzw ACC, vf26, vf12
  mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  ibne vi07, vi03, L133      |  nop
  nop                        |  nop
  b L163                     |  nop
  nop                        |  nop
L125:
  erleng.xyz P, vf12         |  maxi.xy vf08, vf08, I
  3072.0                     |  nop :i
  nop                        |  minii.xy vf08, vf08, I
  nop                        |  maddaw.xyzw ACC, vf26, vf12
  mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
L126:
  lqi.xyzw vf10, vi01        |  mulax.xyzw ACC, vf01, vf11
  b L128                     |  madday.xyzw ACC, vf02, vf11
  nop                        |  maddz.xyzw vf11, vf03, vf11
L127:
  3072.0                     |  mulax.xyzw ACC, vf01, vf11 :i
  lqi.xyzw vf10, vi01        |  minii.xy vf08, vf08, I
  sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
L128:
  lqi.xyzw vf13, vi01        |  add.xyzw vf09, vf09, vf28
  lqi.xyzw vf16, vi01        |  maxw.w vf08, vf08, vf02
  mtir vi12, vf10.x          |  itof0.xyzw vf23, vf23
  mtir vi15, vf10.y          |  maxx.xyzw vf11, vf11, vf00
  nop                        |  miniw.w vf09, vf09, vf01
  div Q, vf01.w, vf09.w      |  add.zw vf10, vf10, vf17
  move.xyzw vf21, vf08       |  add.xyzw vf13, vf13, vf18
  iand vi12, vi12, vi05      |  add.xyzw vf16, vf16, vf19
  ilw.w vi08, -1(vi02)       |  mulax.xyzw ACC, vf04, vf11
  ibgtz vi09, L129           |  madday.xyzw ACC, vf05, vf11
  iand vi15, vi15, vi05      |  maddaz.xyzw ACC, vf06, vf11
  nop                        |  addx.w vf21, vf21, vf17
L129:
  ibne vi05, vi12, L130      |  maddw.xyzw vf11, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf09, vf09, Q
  mtir vi12, vf13.x          |  mul.xyzw vf15, vf15, Q
  mtir vi15, vf13.y          |  ftoi4.xyzw vf21, vf21
  b L131                     |  mul.xyzw vf11, vf11, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf09, vf09, vf22
L130:
  lq.xyzw vf20, 0(vi12)      |  mul.xyzw vf15, vf15, Q
  nop                        |  mulw.xyzw vf24, vf24, vf29
  lq.xyzw vf31, 0(vi15)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf25, 0(vi08)      |  mul.xyzw vf11, vf11, vf23
  lq.xyzw vf23, 1(vi12)      |  add.xyzw vf09, vf09, vf22
  lq.xyzw vf20, 1(vi15)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf26, 1(vi08)      |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 2(vi12)      |  maddz.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 2(vi15)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf27, 2(vi08)      |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 3(vi12)      |  maddz.xyzw vf26, vf26, vf24
  lq.xyzw vf31, 3(vi15)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf28, 3(vi08)      |  madday.xyzw ACC, vf23, vf24
  lq.xyzw vf23, 4(vi12)      |  maddz.xyzw vf27, vf27, vf24
  lq.xyzw vf20, 4(vi15)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi08)       |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 5(vi12)      |  maddz.xyzw vf28, vf28, vf24
  lq.xyzw vf23, 5(vi15)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi08)       |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 6(vi12)      |  maddz.xyz vf29, vf29, vf24
  lq.xyzw vf22, 6(vi15)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 6(vi08)      |  madday.xyzw ACC, vf23, vf24
  lqi.xyzw vf23, vi02        |  maddz.xyz vf30, vf30, vf24
  mtir vi12, vf13.x          |  mulax.xyzw ACC, vf20, vf24
  mtir vi15, vf13.y          |  madday.xyzw ACC, vf22, vf24
  lq.xyzw vf22, 2(vi00)      |  maddz.xyzw vf31, vf31, vf24
  lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
L131:
  ibgez vi09, L132           |  mulaz.xyzw ACC, vf29, vf10
  sq.xyzw vf21, 2(vi10)      |  maddaz.xyzw ACC, vf30, vf13
  nop                        |  ftoi4.xyzw vf21, vf08
L132:
  mfp.w vf20, P              |  maddz.xyz vf13, vf31, vf16
  sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  sq.xyzw vf14, 0(vi13)      |  miniw.w vf09, vf09, vf03
  sq.xyzw vf21, 2(vi13)      |  mulaw.xyzw ACC, vf25, vf10
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf12, vf12, vf20
  1024.0                     |  ftoi0.xyzw vf11, vf11 :i
  erleng.xyz P, vf13         |  maxi.xy vf09, vf09, I
  ibeq vi07, vi03, L143      |  maddaw.xyzw ACC, vf26, vf13
  mr32.z vf16, vf00          |  maddw.xyzw vf10, vf27, vf16
L133:
  3072.0                     |  mulax.xyzw ACC, vf01, vf12 :i
  lqi.xyzw vf08, vi01        |  minii.xy vf09, vf09, I
  sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  lqi.xyzw vf11, vi01        |  add.xyzw vf10, vf10, vf28
  lqi.xyzw vf14, vi01        |  maxw.w vf09, vf09, vf02
  mtir vi10, vf08.x          |  itof0.xyzw vf23, vf23
  mtir vi13, vf08.y          |  maxx.xyzw vf12, vf12, vf00
  nop                        |  miniw.w vf10, vf10, vf01
  div Q, vf01.w, vf10.w      |  add.zw vf08, vf08, vf17
  move.xyzw vf21, vf09       |  add.xyzw vf11, vf11, vf18
  iand vi10, vi10, vi05      |  add.xyzw vf14, vf14, vf19
  ilw.w vi08, -1(vi02)       |  mulax.xyzw ACC, vf04, vf12
  ibgtz vi09, L134           |  madday.xyzw ACC, vf05, vf12
  iand vi13, vi13, vi05      |  maddaz.xyzw ACC, vf06, vf12
  nop                        |  addx.w vf21, vf21, vf17
L134:
  ibne vi05, vi10, L135      |  maddw.xyzw vf12, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf10, vf10, Q
  mtir vi10, vf11.x          |  mul.xyzw vf16, vf16, Q
  mtir vi13, vf11.y          |  ftoi4.xyzw vf21, vf21
  b L136                     |  mul.xyzw vf12, vf12, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf10, vf10, vf22
L135:
  lq.xyzw vf20, 0(vi10)      |  mul.xyzw vf16, vf16, Q
  nop                        |  mulw.xyzw vf24, vf24, vf29
  lq.xyzw vf31, 0(vi13)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf25, 0(vi08)      |  mul.xyzw vf12, vf12, vf23
  lq.xyzw vf23, 1(vi10)      |  add.xyzw vf10, vf10, vf22
  lq.xyzw vf20, 1(vi13)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf26, 1(vi08)      |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 2(vi10)      |  maddz.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 2(vi13)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf27, 2(vi08)      |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 3(vi10)      |  maddz.xyzw vf26, vf26, vf24
  lq.xyzw vf31, 3(vi13)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf28, 3(vi08)      |  madday.xyzw ACC, vf23, vf24
  lq.xyzw vf23, 4(vi10)      |  maddz.xyzw vf27, vf27, vf24
  lq.xyzw vf20, 4(vi13)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi08)       |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 5(vi10)      |  maddz.xyzw vf28, vf28, vf24
  lq.xyzw vf23, 5(vi13)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi08)       |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 6(vi10)      |  maddz.xyz vf29, vf29, vf24
  lq.xyzw vf22, 6(vi13)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 6(vi08)      |  madday.xyzw ACC, vf23, vf24
  lqi.xyzw vf23, vi02        |  maddz.xyz vf30, vf30, vf24
  mtir vi10, vf11.x          |  mulax.xyzw ACC, vf20, vf24
  mtir vi13, vf11.y          |  madday.xyzw ACC, vf22, vf24
  lq.xyzw vf22, 2(vi00)      |  maddz.xyzw vf31, vf31, vf24
  lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
L136:
  ibgez vi09, L137           |  mulaz.xyzw ACC, vf29, vf08
  sq.xyzw vf21, 2(vi11)      |  maddaz.xyzw ACC, vf30, vf11
  nop                        |  ftoi4.xyzw vf21, vf09
L137:
  mfp.w vf20, P              |  maddz.xyz vf11, vf31, vf14
  sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  sq.xyzw vf15, 0(vi14)      |  miniw.w vf10, vf10, vf03
  sq.xyzw vf21, 2(vi14)      |  mulaw.xyzw ACC, vf25, vf08
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf13, vf13, vf20
  1024.0                     |  ftoi0.xyzw vf12, vf12 :i
  erleng.xyz P, vf11         |  maxi.xy vf10, vf10, I
  ibeq vi07, vi03, L153      |  maddaw.xyzw ACC, vf26, vf11
  mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
L138:
  3072.0                     |  mulax.xyzw ACC, vf01, vf13 :i
  lqi.xyzw vf09, vi01        |  minii.xy vf10, vf10, I
  sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  lqi.xyzw vf15, vi01        |  maxw.w vf10, vf10, vf02
  mtir vi11, vf09.x          |  itof0.xyzw vf23, vf23
  mtir vi14, vf09.y          |  maxx.xyzw vf13, vf13, vf00
  nop                        |  miniw.w vf08, vf08, vf01
  div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  move.xyzw vf21, vf10       |  add.xyzw vf12, vf12, vf18
  iand vi11, vi11, vi05      |  add.xyzw vf15, vf15, vf19
  ilw.w vi08, -1(vi02)       |  mulax.xyzw ACC, vf04, vf13
  ibgtz vi09, L139           |  madday.xyzw ACC, vf05, vf13
  iand vi14, vi14, vi05      |  maddaz.xyzw ACC, vf06, vf13
  nop                        |  addx.w vf21, vf21, vf17
L139:
  ibne vi05, vi11, L140      |  maddw.xyzw vf13, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf08, vf08, Q
  mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  mtir vi14, vf12.y          |  ftoi4.xyzw vf21, vf21
  b L141                     |  mul.xyzw vf13, vf13, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
L140:
  lq.xyzw vf20, 0(vi11)      |  mul.xyzw vf14, vf14, Q
  nop                        |  mulw.xyzw vf24, vf24, vf29
  lq.xyzw vf31, 0(vi14)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf25, 0(vi08)      |  mul.xyzw vf13, vf13, vf23
  lq.xyzw vf23, 1(vi11)      |  add.xyzw vf08, vf08, vf22
  lq.xyzw vf20, 1(vi14)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf26, 1(vi08)      |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 2(vi11)      |  maddz.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 2(vi14)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf27, 2(vi08)      |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 3(vi11)      |  maddz.xyzw vf26, vf26, vf24
  lq.xyzw vf31, 3(vi14)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf28, 3(vi08)      |  madday.xyzw ACC, vf23, vf24
  lq.xyzw vf23, 4(vi11)      |  maddz.xyzw vf27, vf27, vf24
  lq.xyzw vf20, 4(vi14)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi08)       |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 5(vi11)      |  maddz.xyzw vf28, vf28, vf24
  lq.xyzw vf23, 5(vi14)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi08)       |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 6(vi11)      |  maddz.xyz vf29, vf29, vf24
  lq.xyzw vf22, 6(vi14)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 6(vi08)      |  madday.xyzw ACC, vf23, vf24
  lqi.xyzw vf23, vi02        |  maddz.xyz vf30, vf30, vf24
  mtir vi11, vf12.x          |  mulax.xyzw ACC, vf20, vf24
  mtir vi14, vf12.y          |  madday.xyzw ACC, vf22, vf24
  lq.xyzw vf22, 2(vi00)      |  maddz.xyzw vf31, vf31, vf24
  lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
L141:
  ibgez vi09, L142           |  mulaz.xyzw ACC, vf29, vf09
  sq.xyzw vf21, 2(vi12)      |  maddaz.xyzw ACC, vf30, vf12
  nop                        |  ftoi4.xyzw vf21, vf10
L142:
  mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  sq.xyzw vf16, 0(vi15)      |  miniw.w vf08, vf08, vf03
  sq.xyzw vf21, 2(vi15)      |  mulaw.xyzw ACC, vf25, vf09
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20
  1024.0                     |  ftoi0.xyzw vf13, vf13 :i
  erleng.xyz P, vf12         |  maxi.xy vf08, vf08, I
  ibne vi07, vi03, L127      |  maddaw.xyzw ACC, vf26, vf12
  mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  b L163                     |  nop
  nop                        |  nop
L143: ;; exit cross 1
  ilw.w vi08, 1(vi00)        |  nop                            ;; vi08 = mercprime flag
  xtop vi02                  |  mulax.xyzw ACC, vf01, vf12     ;; vi02 = out (0) | pipe
  sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12    ;; pipe | pipe
  sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12    ;; pipe | pipe
  iaddiu vi04, vi02, 0x8c    |  add.xyzw vf10, vf10, vf28      ;; vi04 = 140 (byte header) | pipe
  ilwr.x vi05, vi04          |  maxw.w vf09, vf09, vf02        ;; vi05 = srcdest-off       | pipe
  ilw.w vi06, 1(vi04)        |  itof0.xyzw vf23, vf23          ;; vi06 = samecopy-cnt      | pipe
  ibne vi00, vi08, L151      |  nop                            ;; goto L151 if merc prime (just a detour to set constants).
  ilw.x vi07, 2(vi04)        |  maxx.xyzw vf12, vf12, vf00     ;; vi07 = crosscopy-cnt | pipe
L144:
  div Q, vf01.w, vf10.w      |  minix.xyzw vf25, vf00, vf00    ;; pipe  | vf25 = 0
  move.xyzw vf21, vf09       |  minix.xyzw vf26, vf00, vf00    ;; pipe  | vf26 = 0
  iadd vi05, vi05, vi04      |  nop                            ;; vi05 = srcdest table addr
  iaddiu vi04, vi02, 0x173   |  mulax.xyzw ACC, vf04, vf12     ;; vi04 = "output zone" | pipe
  ibgtz vi09, L145           |  madday.xyzw ACC, vf05, vf12    ;; pipe | pipe
  iadd vi06, vi06, vi05      |  maddaz.xyzw ACC, vf06, vf12    ;; vi06 = cross copy table | pipe
  nop                        |  addx.w vf21, vf21, vf17        ;; pipe | pipe
L145:
  iadd vi07, vi07, vi06      |  maddw.xyzw vf12, vf07, vf00    ;; vi07 = end cross copy table | pipe
  ilw.x vi09, -6(vi01)       |  mul.xyz vf10, vf10, Q          ;; ?????? | pipe
  iaddiu vi08, vi00, 0x1ba   |  mul.xyzw vf16, vf16, Q         ;; vi08 = 442 | pipe
  isub vi08, vi08, vi02      |  ftoi4.xyzw vf21, vf21          ;; vi08 is the other buffer | pipe
  iaddiu vi08, vi08, 0x173   |  mul.xyzw vf12, vf12, vf23      ;; vi08 is the other buffer | pipe
  lqi.xyzw vf23, vi03        |  add.xyzw vf10, vf10, vf22      ;; pipe | pipe
  ibgez vi09, L146           |  nop                            ;; pipe (but weird, not applicable for mat1, but still works)
  sq.xyzw vf21, 2(vi11)      |  nop                            ;; pipe
  nop                        |  ftoi4.xyzw vf21, vf09          ;; pipe (slightly wrong for mat1, but might not matter?)
L146:
  mfp.w vf20, P              |  nop                            ;; pipe
  sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17    ;; pipe
  sq.xyzw vf15, 0(vi14)      |  miniw.w vf10, vf10, vf03       ;; pipe
  sq.xyzw vf21, 2(vi14)      |  nop                            ;; pipe
  ilw.y vi09, -3(vi01)       |  mulw.xyzw vf13, vf13, vf20     ;; werid pipe | pipe
  mfir.x vf25, vi04          |  ftoi0.xyzw vf12, vf12          ;; vf25.x = output zone | pipe
  mfir.y vf25, vi04          |  nop                            ;; vf25.y = output zone
  mfir.x vf26, vi08          |  nop                            ;; vf26.x = old output zone
  ilw.w vi02, 1(vi00)        |  nop                            ;; vi02 = mercprime flag
  mfir.y vf26, vi04          |  mulax.xyzw ACC, vf01, vf13     ;; vf26.y = output zone | pipe
  sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13    ;; pipe | pipe
  sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13    ;; pipe | pipe
  nop                        |  nop                            ;; ? why the nop ?
  ibne vi00, vi02, L152      |  maxw.w vf10, vf10, vf02        ;; mercprime detour | pipe
  nop                        |  itof0.xyzw vf23, vf23          ;; pipe
L147:
  8388608.0                  |  maxx.xyzw vf13, vf13, vf00 :i  ;; vf13 = 0
  256.0                      |  maxi.xy vf27, vf00, I :i       ;; vf27.xy = 8388608.0
  move.xyzw vf21, vf10       |  maxi.w vf27, vf00, I           ;; pipe | vf27.w = 256.0
  nop                        |  nop                            ;; ??
  nop                        |  mulax.xyzw ACC, vf04, vf13     ;; pipe
  ibgtz vi09, L148           |  madday.xyzw ACC, vf05, vf13    ;; pipe | pipe
  nop                        |  maddaz.xyzw ACC, vf06, vf13    ;; pipe
  nop                        |  addx.w vf21, vf21, vf17        ;; pipe
L148:
  nop                        |  maddw.xyzw vf13, vf07, vf00    ;; pipe
  ilw.x vi09, -3(vi01)       |  itof0.xyzw vf25, vf25          ;; pipe | zone addrs to float
  nop                        |  itof0.xyzw vf26, vf26          ;; zone addrs to float
  nop                        |  ftoi4.xyzw vf21, vf21          ;; pipe
  nop                        |  mul.xyzw vf13, vf13, vf23      ;; pipe
  ior vi02, vi05, vi00       |  add.xyzw vf25, vf25, vf27      ;; vi02 = srcdst table | float trick
  ibgez vi09, L149           |  add.xyzw vf26, vf26, vf27      ;; pipe | float trick
  sq.xyzw vf21, 2(vi12)      |  nop                            ;; pipe
  nop                        |  ftoi4.xyzw vf21, vf10          ;; pipe
L149:
  ibne vi06, vi05, L150      |  nop
  sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  ior vi06, vi07, vi00       |  max.xyzw vf25, vf26, vf26
L150:
  sq.xyzw vf16, 0(vi15)      |  nop   ;; pipe
  sq.xyzw vf21, 2(vi15)      |  nop   ;; pipe
  lqi.xyzw vf27, vi05        |  nop
  nop                        |  ftoi0.xyzw vf13, vf13 ;; pipe
  nop                        |  nop
  nop                        |  nop
  nop                        |  itof0.xyzw vf27, vf27
  sq.xyzw vf13, 1(vi12)      |  nop ;; pipe
  b L173                     |  nop
  sq.xyzw vf13, 1(vi15)      |  nop ;; pipe
L151:
  3072.0                     |  miniw.w vf10, vf10, vf01 :i
  b L144                     |  minii.xy vf09, vf09, I
  nop                        |  nop
L152:
  1024.0                     |  nop :i
  3072.0                     |  maxi.xy vf10, vf10, I :i
  b L147                     |  minii.xy vf10, vf10, I
  isw.w vi00, 1(vi00)        |  nop

L153: ;; exit cross 2
  ilw.w vi08, 1(vi00)        |  nop
  xtop vi02                  |  mulax.xyzw ACC, vf01, vf13
  sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  iaddiu vi04, vi02, 0x8c    |  add.xyzw vf08, vf08, vf28
  ilwr.x vi05, vi04          |  maxw.w vf10, vf10, vf02
  ilw.w vi06, 1(vi04)        |  itof0.xyzw vf23, vf23
  ibne vi00, vi08, L161      |  nop
  ilw.x vi07, 2(vi04)        |  maxx.xyzw vf13, vf13, vf00
L154:
  div Q, vf01.w, vf08.w      |  minix.xyzw vf25, vf00, vf00
  move.xyzw vf21, vf10       |  minix.xyzw vf26, vf00, vf00
  iadd vi05, vi05, vi04      |  nop
  iaddiu vi04, vi02, 0x173   |  mulax.xyzw ACC, vf04, vf13
  ibgtz vi09, L155           |  madday.xyzw ACC, vf05, vf13
  iadd vi06, vi06, vi05      |  maddaz.xyzw ACC, vf06, vf13
  nop                        |  addx.w vf21, vf21, vf17
L155:
  iadd vi07, vi07, vi06      |  maddw.xyzw vf13, vf07, vf00
  ilw.x vi09, -6(vi01)       |  mul.xyz vf08, vf08, Q
  iaddiu vi08, vi00, 0x1ba   |  mul.xyzw vf14, vf14, Q
  isub vi08, vi08, vi02      |  ftoi4.xyzw vf21, vf21
  iaddiu vi08, vi08, 0x173   |  mul.xyzw vf13, vf13, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
  ibgez vi09, L156           |  nop
  sq.xyzw vf21, 2(vi12)      |  nop
  nop                        |  ftoi4.xyzw vf21, vf10
L156:
  mfp.w vf20, P              |  nop
  sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  sq.xyzw vf16, 0(vi15)      |  miniw.w vf08, vf08, vf03
  sq.xyzw vf21, 2(vi15)      |  nop
  ilw.y vi09, -3(vi01)       |  mulw.xyzw vf11, vf11, vf20
  mfir.x vf25, vi04          |  ftoi0.xyzw vf13, vf13
  mfir.y vf25, vi04          |  nop
  mfir.x vf26, vi08          |  nop
  ilw.w vi02, 1(vi00)        |  nop
  mfir.y vf26, vi04          |  mulax.xyzw ACC, vf01, vf11
  sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
  nop                        |  nop
  ibne vi00, vi02, L162      |  maxw.w vf08, vf08, vf02
  nop                        |  itof0.xyzw vf23, vf23
L157:
  8388608.0                  |  maxx.xyzw vf11, vf11, vf00 :i
  256.0                      |  maxi.xy vf27, vf00, I :i
  move.xyzw vf21, vf08       |  maxi.w vf27, vf00, I
  nop                        |  nop
  nop                        |  mulax.xyzw ACC, vf04, vf11
  ibgtz vi09, L158           |  madday.xyzw ACC, vf05, vf11
  nop                        |  maddaz.xyzw ACC, vf06, vf11
  nop                        |  addx.w vf21, vf21, vf17
L158:
  nop                        |  maddw.xyzw vf11, vf07, vf00
  ilw.x vi09, -3(vi01)       |  itof0.xyzw vf25, vf25
  nop                        |  itof0.xyzw vf26, vf26
  nop                        |  ftoi4.xyzw vf21, vf21
  nop                        |  mul.xyzw vf11, vf11, vf23
  ior vi02, vi05, vi00       |  add.xyzw vf25, vf25, vf27
  ibgez vi09, L159           |  add.xyzw vf26, vf26, vf27
  sq.xyzw vf21, 2(vi10)      |  nop
  nop                        |  ftoi4.xyzw vf21, vf08
L159:
  ibne vi06, vi05, L160      |  nop
  sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  ior vi06, vi07, vi00       |  max.xyzw vf25, vf26, vf26
L160:
  sq.xyzw vf14, 0(vi13)      |  nop
  sq.xyzw vf21, 2(vi13)      |  nop
  lqi.xyzw vf27, vi05        |  nop
  nop                        |  ftoi0.xyzw vf11, vf11
  nop                        |  nop
  nop                        |  nop
  nop                        |  itof0.xyzw vf27, vf27
  sq.xyzw vf11, 1(vi10)      |  nop
  b L173                     |  nop
  sq.xyzw vf11, 1(vi13)      |  nop
L161:
  3072.0                     |  miniw.w vf08, vf08, vf01 :i
  b L154                     |  minii.xy vf10, vf10, I
  nop                        |  nop
L162:
  1024.0                     |  nop :i
  3072.0                     |  maxi.xy vf08, vf08, I :i
  b L157                     |  minii.xy vf08, vf08, I
  isw.w vi00, 1(vi00)        |  nop
L163: ;; exit cross 3
  ilw.w vi08, 1(vi00)        |  nop
  xtop vi02                  |  mulax.xyzw ACC, vf01, vf11
  sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
  iaddiu vi04, vi02, 0x8c    |  add.xyzw vf09, vf09, vf28
  ilwr.x vi05, vi04          |  maxw.w vf08, vf08, vf02
  ilw.w vi06, 1(vi04)        |  itof0.xyzw vf23, vf23
  ibne vi00, vi08, L171      |  nop
  ilw.x vi07, 2(vi04)        |  maxx.xyzw vf11, vf11, vf00
L164:
  div Q, vf01.w, vf09.w      |  minix.xyzw vf25, vf00, vf00
  move.xyzw vf21, vf08       |  minix.xyzw vf26, vf00, vf00
  iadd vi05, vi05, vi04      |  nop
  iaddiu vi04, vi02, 0x173   |  mulax.xyzw ACC, vf04, vf11
  ibgtz vi09, L165           |  madday.xyzw ACC, vf05, vf11
  iadd vi06, vi06, vi05      |  maddaz.xyzw ACC, vf06, vf11
  nop                        |  addx.w vf21, vf21, vf17
L165:
  iadd vi07, vi07, vi06      |  maddw.xyzw vf11, vf07, vf00
  ilw.x vi09, -6(vi01)       |  mul.xyz vf09, vf09, Q
  iaddiu vi08, vi00, 0x1ba   |  mul.xyzw vf15, vf15, Q
  isub vi08, vi08, vi02      |  ftoi4.xyzw vf21, vf21
  iaddiu vi08, vi08, 0x173   |  mul.xyzw vf11, vf11, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf09, vf09, vf22
  ibgez vi09, L166           |  nop
  sq.xyzw vf21, 2(vi10)      |  nop
  nop                        |  ftoi4.xyzw vf21, vf08
L166:
  mfp.w vf20, P              |  nop
  sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  sq.xyzw vf14, 0(vi13)      |  miniw.w vf09, vf09, vf03
  sq.xyzw vf21, 2(vi13)      |  nop
  ilw.y vi09, -3(vi01)       |  mulw.xyzw vf12, vf12, vf20
  mfir.x vf25, vi04          |  ftoi0.xyzw vf11, vf11
  mfir.y vf25, vi04          |  nop
  mfir.x vf26, vi08          |  nop
  ilw.w vi02, 1(vi00)        |  nop
  mfir.y vf26, vi04          |  mulax.xyzw ACC, vf01, vf12
  sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  nop                        |  nop
  ibne vi00, vi02, L172      |  maxw.w vf09, vf09, vf02
  nop                        |  itof0.xyzw vf23, vf23
L167:
  8388608.0                  |  maxx.xyzw vf12, vf12, vf00 :i
  256.0                      |  maxi.xy vf27, vf00, I :i
  move.xyzw vf21, vf09       |  maxi.w vf27, vf00, I
  nop                        |  nop
  nop                        |  mulax.xyzw ACC, vf04, vf12
  ibgtz vi09, L168           |  madday.xyzw ACC, vf05, vf12
  nop                        |  maddaz.xyzw ACC, vf06, vf12
  nop                        |  addx.w vf21, vf21, vf17
L168:
  nop                        |  maddw.xyzw vf12, vf07, vf00
  ilw.x vi09, -3(vi01)       |  itof0.xyzw vf25, vf25
  nop                        |  itof0.xyzw vf26, vf26
  nop                        |  ftoi4.xyzw vf21, vf21
  nop                        |  mul.xyzw vf12, vf12, vf23
  ior vi02, vi05, vi00       |  add.xyzw vf25, vf25, vf27
  ibgez vi09, L169           |  add.xyzw vf26, vf26, vf27
  sq.xyzw vf21, 2(vi11)      |  nop
  nop                        |  ftoi4.xyzw vf21, vf09
L169:
  ibne vi06, vi05, L170      |  nop
  sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  ior vi06, vi07, vi00       |  max.xyzw vf25, vf26, vf26
L170:
  sq.xyzw vf15, 0(vi14)      |  nop
  sq.xyzw vf21, 2(vi14)      |  nop
  lqi.xyzw vf27, vi05        |  nop
  nop                        |  ftoi0.xyzw vf12, vf12
  nop                        |  nop
  nop                        |  nop
  nop                        |  itof0.xyzw vf27, vf27
  sq.xyzw vf12, 1(vi11)      |  nop
  b L173                     |  nop
  sq.xyzw vf12, 1(vi14)      |  nop
L171:
  3072.0                     |  miniw.w vf09, vf09, vf01 :i
  b L164                     |  minii.xy vf08, vf08, I
  nop                        |  nop
L172:
  1024.0                     |  nop :i
  3072.0                     |  maxi.xy vf09, vf09, I :i
  b L167                     |  minii.xy vf09, vf09, I
  isw.w vi00, 1(vi00)        |  nop

;; end routine.
L173:
  ibeq vi07, vi02, L179      |  nop
  ilw.w vi15, 132(vi00)      |  nop
  ibne vi06, vi05, L174      |  add.xyzw vf11, vf27, vf25
  nop                        |  nop
  ibne vi07, vi06, L174      |  nop
  ior vi06, vi07, vi00       |  max.xyzw vf25, vf26, vf26
  nop                        |  nop
  nop                        |  nop
  mtir vi08, vf11.x          |  nop
  mtir vi10, vf11.y          |  nop
  nop                        |  nop
  nop                        |  nop
  lq.xyzw vf16, 2(vi08)      |  maxx.xyzw vf15, vf11, vf00
  lq.xyzw vf13, 0(vi08)      |  nop
  b L178                     |  nop
  nop                        |  nop
L174:
  lqi.xyzw vf27, vi05        |  nop
  nop                        |  nop
  mtir vi08, vf11.x          |  nop
  mtir vi09, vf11.y          |  nop
  nop                        |  itof0.xyzw vf27, vf27
  nop                        |  nop
  lq.xyzw vf12, 2(vi08)      |  maxx.xyzw vf15, vf11, vf00
  lq.xyzw vf13, 0(vi08)      |  nop
  ibne vi06, vi05, L175      |  add.xyzw vf11, vf27, vf25
  nop                        |  nop
  ibeq vi07, vi06, L177      |  nop
  ior vi06, vi07, vi00       |  max.xyzw vf25, vf26, vf26
L175:
  lqi.xyzw vf27, vi05        |  itof15.w vf12, vf12
  lq.xyzw vf14, 1(vi08)      |  nop
  mtir vi08, vf11.x          |  nop
  mtir vi10, vf11.y          |  nop
  sq.xyzw vf13, 0(vi09)      |  itof0.xyzw vf27, vf27
  sq.xyzw vf14, 1(vi09)      |  add.w vf12, vf12, vf15
  lq.xyzw vf16, 2(vi08)      |  maxx.xyzw vf15, vf11, vf00
  lq.xyzw vf13, 0(vi08)      |  nop
  ibne vi06, vi05, L176      |  add.xyzw vf11, vf27, vf25
  sq.xyzw vf12, 2(vi09)      |  nop
  ibne vi07, vi06, L176      |  nop
  ior vi06, vi07, vi00       |  max.xyzw vf25, vf26, vf26
  move.xyzw vf12, vf16       |  nop
  b L177                     |  nop
  ior vi09, vi10, vi00       |  nop
L176:
  lqi.xyzw vf27, vi05        |  itof15.w vf16, vf16
  lq.xyzw vf14, 1(vi08)      |  nop
  mtir vi08, vf11.x          |  nop
  mtir vi09, vf11.y          |  nop
  sq.xyzw vf13, 0(vi10)      |  itof0.xyzw vf27, vf27
  sq.xyzw vf14, 1(vi10)      |  add.w vf16, vf16, vf15
  lq.xyzw vf12, 2(vi08)      |  maxx.xyzw vf15, vf11, vf00
  lq.xyzw vf13, 0(vi08)      |  nop
  ibne vi06, vi05, L175      |  add.xyzw vf11, vf27, vf25
  sq.xyzw vf16, 2(vi10)      |  nop
  ibne vi07, vi06, L175      |  nop
  ior vi06, vi07, vi00       |  max.xyzw vf25, vf26, vf26
L177:
  nop                        |  itof15.w vf12, vf12
  lq.xyzw vf14, 1(vi08)      |  nop
  mtir vi08, vf11.x          |  nop
  mtir vi10, vf11.y          |  nop
  sq.xyzw vf13, 0(vi09)      |  nop
  sq.xyzw vf14, 1(vi09)      |  add.w vf12, vf12, vf15
  lq.xyzw vf16, 2(vi08)      |  maxx.xyzw vf15, vf11, vf00
  lq.xyzw vf13, 0(vi08)      |  nop
  nop                        |  nop
  sq.xyzw vf12, 2(vi09)      |  nop
L178:
  nop                        |  itof15.w vf16, vf16
  lq.xyzw vf14, 1(vi08)      |  nop
  nop                        |  nop
  nop                        |  nop
  sq.xyzw vf13, 0(vi10)      |  nop
  sq.xyzw vf14, 1(vi10)      |  add.w vf16, vf16, vf15
  nop                        |  nop
  nop                        |  nop
  nop                        |  nop
  sq.xyzw vf16, 2(vi10)      |  nop
L179:
  ibne vi00, vi15, L180      |  nop
  nop                        |  nop
  xgkick vi04                |  nop
  nop                        |  nop :e
  nop                        |  nop
L180:
  lq.xyzw vf20, 132(vi00)    |  nop
  lq.xyzw vf21, 1(vi00)      |  nop
  iaddi vi01, vi00, 0x1      |  nop
  isw.x vi01, -2(vi04)       |  nop
  iaddiu vi02, vi00, 0x47    |  maxw.x vf20, vf00, vf20
  isw.z vi02, -1(vi04)       |  nop
  sq.yzw vf21, -2(vi04)      |  nop
  isw.w vi00, 132(vi00)      |  nop
  sq.x vf20, -1(vi04)        |  nop
  iaddi vi04, vi04, -0x2     |  nop
  xgkick vi04                |  nop
  nop                        |  nop :e
  nop                        |  nop

