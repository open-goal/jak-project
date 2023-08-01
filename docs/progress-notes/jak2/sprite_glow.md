# Sprite Glow: EE side
It's pretty simple, no asm.

# VU Memory Map
Offset is 400 (0-400 -> 400-800 as input buffers)
```

400 - 800 (input second buffer)
800 - template copy 1
884 - template copy 2
980 - constants
```
# DMA init
```
@ 0x500930 tag: TAG: 0x00000000 cnt  qwc 0x0018
 vif0: STCYCL cl: 4 wl: 4
 vif1: UNPACK-V4-32: 24 addr: 980 us: false tops: false

@ 0x500ac0 tag: TAG: 0x00777650 ref  qwc 0x0054
 vif0: STCYCL cl: 4 wl: 4
 vif1: UNPACK-V4-32: 84 addr: 800 us: false tops: false

@ 0x500ad0 tag: TAG: 0x00777650 ref  qwc 0x0054
 vif0: MSCAL 0x0
 vif1: UNPACK-V4-32: 84 addr: 884 us: false tops: false

@ 0x500ae0 tag: TAG: 0x00000000 cnt  qwc 0x0000
 vif0: BASE 0x0
 vif1: OFFSET 0x190

@ 0x500af0 tag: TAG: 0x00000000 cnt  qwc 0x0000
 vif0: NOP
 vif1: FLUSHE
```

First tag is uploading the constants.

Next two are uploading two copies of the templates and run init. Output is double buffered, so this makes sense.

Next tag sets up input double buffer.

Final tag is just sync before starting the draws

# Template system
The VU program is (as usual) double buffered.
There are two input buffers, containing data uploaded through VIF.
There are two output buffers, each containing the `sprite-glow-template`.
While the program runs, it transform vertices, putting the results in the template.
Once done, it `xgkick`s, which waits for the previous run's draw to finish if needed,
then begins the drawing process.

# What is drawn?

Annoyingly, there are 6 draws this time:
- Probe Clear alpha
- Probe Draw alpha
- Offscreen sample
- Offscreen repeat
- Draw Alpha
- Draw Final

In summary:
- Probe Clear alpha: draw a `alpha = 0` square. Always drawn.
- Probe Draw alpha: draw an `alpha = 1` square. 1 px smaller than first draw. Uses normal z test.

## Draw 1 and 2: "Probe Clear alpha" and "Probe Draw alpha"
First is the `clear`. The first tag is 5 regs of adgif:
- `texflush`
- `(new 'static 'gs-alpha :a 2 :b 2 :c 2 :d 1)`
- `(new 'static 'gs-test :ate 1 :afail 1 :zte 1 :ztst 2)`
- `(new 'static 'gs-zbuf :zbp 304 :psm 1 :zmsk 1)`
- `(new 'static 'gs-frame :fbp 408 :fbw 8 :fbmsk #xffffff)`

The alpha is:
```
Cv = (0 - 0) * 0 + Cd
```
which means to not write any color.

The test is:
- alpha test always fails, means only rgba is written always
- ztest is usual GEQUAL

When combined with the alpha settings, this only writes alpha, no rgb or depth.

The zbuf looks at the normal zbuf, but z writing is masked again. Just in case.

The gs-frame yet again masks away rgb writes. Just in case.

There are two draws. Both are sprites. The first draw sets alpha to 0 and uses z = #xffffff, so it always passes.

The second draw is similar but:
- z comes from the transformed vertex. So it has normal depth test behavior
- alpha is 1
- it's 1 pixel smaller than the first draw.

So the end result is:
- a `alpha = 0` square always
- an `alpha = 1` square, offset in by 1 pixel on all sides, but only where depth passes.

## Draw 3: Offscreen Sample
Switches to GS context 2. Samples from framebuffer and writes to a temporary texture (64 px width, uses only 32).
- only writes alpha
- mmag/mmin on
- tcc = 1, tfx = 1 (rgba, decal) - RGBA all come from texture, nothing fancy
- rgb writes masked (only write alpha)
- clamp clamps
- alpha test thing disables z buffer writing.

This basically just copies the inner square from draw 2 to a separate texture.

## Draw 4: Repeat draw
This appears to draw over itself again and again. I think it effectively blends using texture filtering. So the 0, 0 px will have the average value of alpha.

## Draw 5: flare alpha
This is set up in the `repeat-draw-adcmds`. Drawing to the framebuffer again.




# VU1 program
```
out memory map:

68: adgif0
69: adgif1
70: adgif2
71: adgif3
72: adgif4
```
```
;; math:

first, input position is multiplied by camera matrix (including adding part). Only xyz is computed here. (p0, vf01)

color: rgb *= a

fade = clamp(0, 1, p0.z * fade_a + fade_b)




INIT program
  iaddiu vi05, vi00, 0x320   |  nop      ;; vi05 = 800 (template)
  lq.xyzw vf25, 988(vi00)    |  nop
  lq.xyzw vf26, 989(vi00)    |  nop
  lq.xyzw vf27, 990(vi00)    |  nop
  lq.xyzw vf30, 996(vi00)    |  nop
  lq.xyzw vf31, 997(vi00)    |  nop
  lq.xyzw vf28, 1002(vi00)   |  nop
  lq.xyzw vf29, 1003(vi00)   |  nop
  nop                        |  nop :e
  nop                        |  nop

regs:

vi02 = input ptr
vi03 = num_sprites
vi04 = adgif ptr
vi05 = output buffer (double buffered, so toggles)

vf25 = hvdf
vf26 = hmge
vf27 = consts
vf30 = basis_x
vf31 = basis_y
vf28 = clamp_min
vf29 = clamp_max

DRAW program
  xtop vi02                  |  nop          ;; vi02 = input buffer's control
  nop                        |  nop
  ilwr.x vi03, vi02          |  nop          ;; vi03 = num_sprites (1 always?)
  iaddi vi02, vi02, 0x1      |  nop          ;; vi02 = sprite data
  iaddiu vi04, vi02, 0x90    |  nop          ;; vi04 = adgif data
L1:
  lq.xyzw vf03, 2(vi02)      |  nop          ;; vf03 = color
  lq.xyzw vf02, 1(vi02)      |  nop          ;; vf02 = [size_probe, z_offset, rot-angle, size-y]
  lq.xyzw vf01, 0(vi02)      |  nop          ;; vf01 = [position.xyz, size-x]
  lq.xyzw vf24, 983(vi00)    |  nop          ;; vf24 = [camera_mat[3]]
  lq.xyzw vf21, 980(vi00)    |  nop          ;; vf21 = [camera_mat[0]]
  lq.xyzw vf22, 981(vi00)    |  nop          ;; vf22 = [camera_mat[1]]
  lq.xyzw vf23, 982(vi00)    |  nop          ;; vf23 = [camera_mat[2]]
  lq.xyzw vf04, 3(vi02)      |  mulaw.xyz ACC, vf24, vf00   ;; vf04 = [fade_a, fade_b, X, X] | multiply
  lq.xyzw vf24, 987(vi00)    |  maddax.xyz ACC, vf21, vf01  ;; vf24 = [perspective[3]] | multiply
  lq.xyzw vf21, 984(vi00)    |  madday.xyz ACC, vf22, vf01  ;; vf21 = [perspective[0]] | multiply
  lq.xyzw vf22, 985(vi00)    |  maddz.xyz vf01, vf23, vf01  ;; vf22 = [perspective[1]] | multiply
  lq.xyzw vf23, 986(vi00)    |  nop                         ;; vf23 = [perspective[2]]
  lq.xyzw vf09, 0(vi04)      |  nop                         ;; vf09 = adgif[0]
  lq.xyzw vf10, 1(vi04)      |  mulw.xyz vf03, vf03, vf03   ;; vf10 = adgif[1] | color multiply by alpha
  div Q, vf02.y, vf01.z      |  mulz.x vf04, vf04, vf01     ;; Q = (z_offset / p0.z) | fade_a *= p0.z
  lq.xyzw vf11, 2(vi04)      |  nop                         ;; vf11 = adgif[2]
  0.0078125                  |  nop :i                      ;; I = 0.0078125 (= 1/128)
  lq.xyzw vf12, 3(vi04)      |  nop                         ;; vf12 = adgif[3]
  lq.xyzw vf13, 4(vi04)      |  addy.x vf04, vf04, vf04     ;; vf13 = adgif[4] | fade_a += fade_b
  sq.xyzw vf09, 68(vi05)     |  muly.z vf05, vf02, vf27     ;; adgif0 store    | vf05.z = rot-angle * deg_to_rad
  move.w vf05, vf00          |  addw.z vf02, vf00, vf01     ;; vf05.w = 1      | vf02.z = size-x
  sq.xyzw vf10, 69(vi05)     |  mul.w vf09, vf00, Q         ;; agdif1 store    | vf09.w = (z_offset / p0.z)
  sq.xyzw vf11, 70(vi05)     |  nop                         ;; adgif2 store
  sq.xyzw vf12, 71(vi05)     |  miniw.x vf04, vf04, vf00    ;; adgif3 store    | clamp fade 1
  sq.xyzw vf13, 72(vi05)     |  nop                         ;; adgif4 store
  nop                        |  subw.w vf09, vf00, vf09     ;; vf09.w = 1 - (z_offset / p0.z);
  nop                        |  maxx.x vf04, vf04, vf00     ;; clamp fade 2
  nop                        |  mulw.xyz vf01, vf01, vf09   ;; multiply by pscale
  nop                        |  mulx.xyz vf03, vf03, vf04   ;; multiply color by fade
  nop                        |  mulaw.xyzw ACC, vf24, vf00  ;; multiply by perspective matrix
  nop                        |  maddax.xyzw ACC, vf21, vf01
  nop                        |  madday.xyzw ACC, vf22, vf01
  nop                        |  muli.xyz vf03, vf03, I      ;; color scaling.
  nop                        |  maddz.xyzw vf01, vf23, vf01 ;; perspective matrix.
  nop                        |  nop
  iaddi vi03, vi03, -0x1     |  mulz.z vf06, vf05, vf05     ;; dec sprite count | vf06 = rot^2
  lq.xyzw vf15, 991(vi00)    |  nop                         ;; vf15 = sincos01
  iaddi vi02, vi02, 0x3      |  nop                         ;; inc input pointer... by the wrong amount lol
  fcset 0x0                  |  mul.xyzw vf07, vf01, vf26   ;; hmge mult
  nop                        |  mulz.zw vf09, vf05, vf06    ;; vf09 = rot^3
  lq.xyzw vf15, 992(vi00)    |  mula.zw ACC, vf05, vf15     ;; vf15 = sincos23 | acc working on sincos.
  nop                        |  nop
  div Q, vf00.w, vf07.w      |  clipw.xyz vf07, vf07        ;; Q = 1 / p_hmged.w | clip!!!
  nop                        |  mulz.zw vf10, vf09, vf06    ;; vf10 is rot thing
  lq.xyzw vf15, 993(vi00)    |  madda.zw ACC, vf09, vf15    ;; vf15 is rot coeff, working on sincos
  nop                        |  nop
  fcand vi01, 0x3f           |  nop                         ;; check clipping result
  ibne vi00, vi01, L2        |  mulz.zw vf09, vf10, vf06    ;; skip if clipped | working on rot
  lq.xyzw vf15, 994(vi00)    |  madda.zw ACC, vf10, vf15    ;; rot | rot
  nop                        |  mul.xyz vf01, vf01, Q       ;; perspective multiply
  nop                        |  mul.xyzw vf02, vf02, Q      ;; vf02 *= q
  nop                        |  mulz.zw vf10, vf09, vf06    ;; rot
  lq.xyzw vf15, 995(vi00)    |  madda.zw ACC, vf09, vf15    ;; rot89 | rot
  nop                        |  add.xyzw vf01, vf01, vf25   ;; hvdf offset
  nop                        |  maxw.x vf02, vf02, vf00     ;; clip size_probe to 1
  nop                        |  miniw.x vf02, vf02, vf29    ;; min size_probe against clamp max.w
  nop                        |  miniz.zw vf02, vf02, vf29   ;; min zw against clamp max.z
  nop                        |  madd.zw vf05, vf10, vf15    ;; vf05 = sincos output
  nop                        |  ftoi0.xyzw vf03, vf03       ;; colors to ints or whatever
  nop                        |  addx.xy vf09, vf28, vf02    ;; vf09 = [cmin.x + probe_size, cmin.y + probe_size]
  nop                        |  subx.xy vf11, vf01, vf02    ;; vf11 = probe lower corner = [p0.xy - probe_size]
  nop                        |  addx.xy vf12, vf01, vf02    ;; vf12 = probe upper corner = [po.xy + probe_size]
  nop                        |  subx.xy vf10, vf29, vf02    ;; vf10 = [cmax.x - probe_size, cmax.y - probe_size]
  nop                        |  mulaz.xyzw ACC, vf30, vf05
  nop                        |  msubw.xyzw vf15, vf31, vf05 ;; rotate the basis

  nop                        |  max.xy vf20, vf01, vf09     ;; vf20 is some clamped position.
  nop                        |  addx.zw vf11, vf01, vf00    ;; vf11.zw = vf01.zw
  nop                        |  addx.zw vf12, vf01, vf00    ;; vf12.zw = vf01.zw
  nop                        |  subw.xy vf17, vf28, vf00    ;; vf17 = [clamp_min.x - 1, clamp_min.y - 1]
  nop                        |  mulz.xyzw vf15, vf15, vf02  ;; rot_basis *= vf02.z (x-scale)
  nop                        |  addw.xy vf18, vf28, vf00    ;; vf18 = [calm_min.x + 1, clamp_min.y + 1]
  nop                        |  ftoi4.xyzw vf11, vf11       ;; usual ftoi4 of clear positions
  nop                        |  ftoi4.xyzw vf12, vf12
  nop                        |  mini.xy vf20, vf20, vf10    ;; vf20 clamp again.
  nop                        |  mulaw.xyzw ACC, vf30, vf05
  sq.xyzw vf03, 75(vi05)     |  maddz.xyzw vf16, vf31, vf05 ;; store color of flare | second row of rotation matrix
  sq.xyz vf11, 11(vi05)      |  sub.xy vf17, vf20, vf17     ;; store clear pos      | offset vf17
  sq.xyz vf12, 12(vi05)      |  sub.xy vf18, vf20, vf18     ;; store clear pos      | offset vf18
  lq.xyzw vf11, 998(vi00)    |  subx.xy vf19, vf20, vf02    ;; compute first clear pos,
  lq.xyzw vf12, 999(vi00)    |  mulw.xyzw vf16, vf16, vf02  ;; y scale
  lq.xyzw vf13, 1000(vi00)   |  addx.xy vf20, vf20, vf02    ;; first clear pos, upper corner.
  lq.xyzw vf14, 1001(vi00)   |  mulaw.xyzw ACC, vf01, vf00
  nop                        |  maddax.xyzw ACC, vf15, vf11
  nop                        |  maddy.xyzw vf11, vf16, vf11
  nop                        |  mulaw.xyzw ACC, vf01, vf00
  nop                        |  maddax.xyzw ACC, vf15, vf12
  nop                        |  maddy.xyzw vf12, vf16, vf12
  nop                        |  mulaw.xyzw ACC, vf01, vf00
  nop                        |  maddax.xyzw ACC, vf15, vf13
  nop                        |  maddy.xyzw vf13, vf16, vf13
  nop                        |  mulaw.xyzw ACC, vf01, vf00
  nop                        |  maddax.xyzw ACC, vf15, vf14
  nop                        |  maddy.xyzw vf14, vf16, vf14
  nop                        |  subx.xy vf17, vf17, vf02
  nop                        |  addx.xy vf18, vf18, vf02
  iaddiu vi04, vi04, 0x50    |  subw.xy vf19, vf19, vf00 ;; offset first clear pos lower
  nop                        |  addw.xy vf20, vf20, vf00 ;; offset first clear pos upper
  nop                        |  ftoi4.xyzw vf11, vf11
  nop                        |  ftoi4.xyzw vf12, vf12
  nop                        |  ftoi4.xyzw vf13, vf13
  nop                        |  ftoi4.xyzw vf14, vf14
  sq.xy vf11, 61(vi05)       |  ftoi4.xyzw vf17, vf17
  sq.xy vf12, 62(vi05)       |  ftoi4.xyzw vf18, vf18
  sq.xy vf13, 63(vi05)       |  ftoi4.xyzw vf19, vf19
  sq.xy vf14, 64(vi05)       |  ftoi4.xyzw vf20, vf20
  sq.xy vf17, 24(vi05)       |  nop
  sq.xy vf18, 26(vi05)       |  nop
  sq.xy vf19, 8(vi05)        |  nop
  sq.xy vf20, 9(vi05)        |  nop
  sq.xy vf11, 77(vi05)       |  nop
  sq.xy vf12, 79(vi05)       |  nop
  sq.xy vf13, 81(vi05)       |  nop
  sq.xy vf14, 83(vi05)       |  nop
  xgkick vi05                |  nop
L2:
  iaddiu vi01, vi00, 0x694   |  nop
  ibne vi00, vi03, L1        |  nop
  isub vi05, vi01, vi05      |  nop
  nop                        |  nop :e
  nop                        |  nop
```