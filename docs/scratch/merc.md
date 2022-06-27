Loop over effect (skip those flagged with use-mercneric)

Loop over fragments

First thing is the "row" data.
row.x/y is st-vif-add from the merc-ctrl-header.
row.z = 0x47800000, row.w = 0x4b010000


Next is the unsigned-four.
This is unpacked with unpack8.
It always goes to 140 in the VU memory.
The source data is just the merc-fragment (I believe it includes merc-byte-header)

After is the lump-four. This has a STMOD enabled and is unpack8.
It goes after the unsigned-four (variable size) in VU memory.
The source data is after. Rounding in static data is ((u4c + 3) >> 2) << 4.

After is the Floating Point data. This is copied as unpack32.

_only_ on the first fragment of an effect, there's an upload to 132 of 8 qw:
the first 7 are lights.
the final is the first quadword of the merc-ctrl-header (xyz-scale, st-magic, st-out-a, st-out-b)

there are secrets hidden in the lights:
- light 0's w is some flag with ignore alpha in it.


Next is (optional) matrix uploads.
There is a loop of transfers. These are all size 7 qw.

Next is the MSCAL!
It has a different number of the first fragment of an effect.
This tells merc to load the light stuff.

End Loop over fragments

Increment effect
Increment effect info
Decrement effect count

update some next-merc thing in the scratchpad


# Merc Renderer

## Memory layout
Unpacks adgif shaders/giftags to the output memory.
Can reuse the last shader from the last frag in the effect.

## Matrix Setup
The matrix contains a "tmat" and "nmat". The "tmat" transforms the point and the "nmat" rotates the normals.

Matrices that are freshly uploaded with this fragment are preprocessed to include the effect of the perspective matrix. The inputs are in registers 8, 10, 12, 25, and the outputs are 9, 11, 13, 26. The output is written over the input.

```
mula.xyzw ACC, vf15, vf08
maddz.xyzw vf09, vf16, vf08
mula.xyzw ACC, vf15, vf10
maddz.xyzw vf11, vf16, vf10
mula.xyzw ACC, vf15, vf12
maddz.xyzw vf13, vf16, vf12
addax.xyzw vf20, vf00
madda.xyzw ACC, vf27, vf25
maddz.xyzw vf26, vf28, vf25
```

with
```
vf15 = [spdx, spdy, spdz, 0]
vf16 = [0, 0, 0, spdw]
vf27 = [pdx, pdy, pdz, 0]
vf28 = [0, 0, 0, pdw]
```
(see `merc_asm.asm` for how to compute these in more detail)

## Vertex layout
The "lump" data contains the vertices. The layout is:
```
mat0, mat1, nrmx, posx
dst0, dst1, nrmy, posy
texs, text, nrmz, posz
```
It begins with `mat1-cnt` vertices that are deformed by only a single matrix.

The `rgba-offset` points to somewhere in the u4 data. Each vertex has a single rgba, stored as unpacked u8's.

## Mat1 Loop
`vi01` is the lump pointer that reads vertices, `vf17`, `vf18`, `vf19` are the "lump offsets" to be applied to the unpacked vertices.

The operations are:
```asm
ilwr.x vi08, vi01    ;; load mat0 from vertex
lqi.xyzw vf08, vi01  ;; load vertex qw 0
lqi.xyzw vf11, vi01  ;; load vertex qw 1
lqi.xyzw vf14, vi01  ;; load vertex qw 2

lq.xyz vf29, 4(vi08)  ;; load nmat0
lq.xyz vf30, 5(vi08)  ;; load nmat1
lq.xyzw vf31, 6(vi08) ;; load nmat2

add.zw vf08, vf08, vf17    ;; lump offset
add.xyzw vf11, vf11, vf18  ;; lump offset
add.xyzw vf14, vf14, vf19  ;; lump offset

mtir vi10, vf11.x ;; get dest0
mtir vi13, vf11.y ;; get dest1

;; rotate normal
mulaz.xyzw ACC, vf29, vf08
maddaz.xyzw ACC, vf30, vf11
maddz.xyz vf11, vf31, vf14

;; load tmat
lq.xyzw vf25, 0(vi08)
lq.xyzw vf26, 1(vi08)
lq.xyzw vf27, 2(vi08)
lq.xyzw vf28, 3(vi08)

;; get normal 1/length
erleng.xyz P, vf11

;; transform point
mulaw.xyzw ACC, vf25, vf08
maddaw.xyzw ACC, vf26, vf11
maddw.xyzw vf08, vf27, vf14
add.xyzw vf08, vf08, vf28

;; clear nrmz
mr32.z vf14, vf00

;; ONLY if merc prime miniw.w vf08, vf08, vf01

;; perspective divide
div Q, vf01.w, vf08.w
mul.xyz vf08, vf08, Q
mul.xyzw vf14, vf14, Q

;; load rgba
lqi.xyzw vf23, vi03

;; hvdf offset
add.xyzw vf08, vf08, vf22

;; normalize normal
mfp.w vf20, P
mulw.xyzw vf11, vf11, vf20

;; fog max
miniw.w vf08, vf08, vf03

;; fetch mat1 (note that vi01 is incremented 3x in pipeline)
ilw.y vi09, -6(vi01)

;; dot product with light
mulax.xyzw ACC, vf01, vf11
madday.xyzw ACC, vf02, vf11
maddz.xyzw vf11, vf03, vf11

;; fog min
maxw.w vf08, vf08, vf02

;; rgba itof
itof0.xyzw vf23, vf23

;; light clamp
maxx.xyzw vf11, vf11, vf00

move.xyzw vf21, vf08

;; color the lights
mulax.xyzw ACC, vf04, vf11
madday.xyzw ACC, vf05, vf11
maddaz.xyzw ACC, vf06, vf11

;; IF vi09 <= 0
addx.w vf21, vf21, vf17

;; add ambient lighting
maddw.xyzw vf11, vf07, vf00

;; ftoi the vertex position
ftoi4.xyzw vf21, vf21

;; apply vertex color
mul.xyzw vf11, vf11, vf23

;; store dest
sq.xyzw vf21, 2(vi10)

;; IF vi09 == 0
ftoi4.xyzw vf21, vf08

;; store st
sq.xyzw vf14, 0(vi10)
sq.xyzw vf14, 0(vi13)

;; store second position
sq.xyzw vf21, 2(vi13)

;; final light
miniy.xyzw vf11, vf11, vf17
ftoi0.xyzw vf11, vf11
sq.xyzw vf11, 1(vi10)
sq.xyzw vf11, 1(vi13)

```
Note: it might be that the last vertex can't change its adc flag for dst2. Maybe only in some cases. Worth checking more if there are stripping issues.

## Mat2 Loop

NOTE: we might need to advance perc by 1 at the beginning if there are any mat0's.
```asm
;; compute perc ptr
ilw.x vi02, 3(vi12)
iadd vi02, vi02, vi12

;; load vertex
lqi.xyzw vf08, vi01
lqi.xyzw vf11, vi01
lqi.xyzw vf14, vi01

;; and perc
lqi.xyzw vf24, vi02

;; extract mat idx
mtir vi10, vf08.x
mtir vi13, vf08.y

;; convert perc
itof0.xyzw vf24, vf24

;; lump offset
add.zw vf08, vf08, vf17
add.xyzw vf11, vf11, vf18
add.xyzw vf14, vf14, vf19

;; mask off sign bit of mat0 and mat1
iand vi10, vi10, vi05 (vi05 = 0x7f)  <- looks like a 0 here means "reuse prev mat"
iand vi13, vi13, vi05

;; scale perc
mulw.xyzw vf24, vf24, vf29  ;; vf29.w = 0.003921569

;; load matrices
lq.xyzw vf20, 0(vi10) ;; tmat0.0
lq.xyzw vf25, 0(vi13) ;; tmat1.0
lq.xyzw vf23, 1(vi10) ;; tmat0.1
lq.xyzw vf26, 1(vi13) ;; tmat1.1
lq.xyzw vf20, 2(vi10) ;; tmat0.2
lq.xyzw vf27, 2(vi13) ;; tmat1.2
lq.xyzw vf23, 3(vi10) ;; tmat0.3
lq.xyzw vf28, 3(vi13) ;; tmat1.3
lq.xyzw vf20, 4(vi10) ;; nmat0.0
lq.xyz vf29,  4(vi13) ;; nmat1.0
lq.xyzw vf23, 5(vi10) ;; nmat0.1
lq.xyz vf30, 5(vi13)  ;; nmat1.1
lq.xyzw vf20, 6(vi10) ;; nmat0.2
lq.xyzw vf31, 6(vi13) ;; nmat1.2

;; multiply rows by perc.
;; mat0 uses perc.x, mat0 uses perc.y. Matrices are added.
;; ex: tmat2 = tmat0.2 * perc.x + tmat1.2 * perc.y
Results are

vf25 = tmat0
vf26 = tmat1
vf27 = tmat2
vf28 = tmat3

vf29 = nmat0
vf30 = nmat1
vf31 = nmat2

;; rotate normal
mulaz.xyzw ACC, vf29, vf08
maddaz.xyzw ACC, vf30, vf11
maddz.xyz vf11, vf31, vf14

;; transform point
mulaw.xyzw ACC, vf25, vf08
maddaw.xyzw ACC, vf26, vf11
maddw.xyzw vf08, vf27, vf14
add.xyzw vf08, vf08, vf28

;; length of normal
erleng.xyz P, vf11

;; rgba offset
ilwr.y vi03, vi12
iadd vi03, vi03, vi12

;; persepctive divide
div Q, vf01.w, vf08.w
mul.xyz vf08, vf08, Q
mul.xyzw vf14, vf14, Q

;; load rgba
lqi.xyzw vf23, vi03

;; hvdf offset
add.xyzw vf08, vf08, vf22

;; normalize normal
mfp.w vf20, P
mulw.xyzw vf11, vf11, vf20

;; fog
miniw.w vf08, vf08, vf03
maxw.w vf08, vf08, vf02

;; go get mat1 again
ilw.y vi09, -6(vi01)

;; light dot product
mulax.xyzw ACC, vf01, vf11
madday.xyzw ACC, vf02, vf11
maddz.xyzw vf11, vf03, vf11

;; vertex color convert
itof0.xyzw vf23, vf23

;; light clamp
maxx.xyzw vf11, vf11, vf00


;; adc logic
ilw.y vi09, -6(vi01)
move.xyzw vf21, vf08
ibgtz vi09, L47

addx.w vf21, vf21, vf17

L47:
ilw.x vi09, -9(vi01)
ftoi4.xyzw vf21, vf21
ilw.x vi09, -9(vi01)

sq.xyzw vf21, 2(vi10)
ibgez vi09, L50

ftoi4.xyzw vf21, vf08

L50:
sq.xyzw vf21, 2(vi13)
```


## Final Copies
assuming no mercprime, leaving out pipe flush (which is a tangled mess to flush the pipes of whatever of the 3 different loops were)
```
;; vi08 = mercprime flag
ilw.w vi08, 1(vi00)

;; find the byte header again
xtop vi02
iaddiu vi04, vi02, 0x8c

;; srcdest-off
ilwr.x vi05, vi04

;; samecopy-cnt
ilw.w vi06, 1(vi04)

;; crosscopy-cnt
ilw.x vi07, 2(vi04)

;; zero out vf25, vf26
minix.xyzw vf25, vf00, vf00
minix.xyzw vf26, vf00, vf00

;; compute srcdest table address
iadd vi05, vi05, vi04

;; compute output zone
iaddiu vi04, vi02, 0x173

;; compute srcdest cross copy table address
iadd vi06, vi06, vi05

;; compute end of cross copy
iadd vi07, vi07, vi06

;; compute address of the _old_ output buffer
iaddiu vi08, vi00, 0x1ba
isub vi08, vi08, vi02
iaddiu vi08, vi08, 0x173

;; move addrs to vf
mfir.x vf25, vi04 ;; vf25.x = output zone
mfir.y vf25, vi04 ;; vf25.y = output zone
mfir.x vf26, vi08 ;; vf26.x = old output zone
mfir.y vf26, vi04 ;; vf26.y = output zone

;; set up
maxx.xyzw vf13, vf13, vf00 ;; vf13 = 0
maxi.xy vf27, vf00, I, I = 8388608.0 ;; vf27.xy = [8388608.0 8388608.0]
maxi.w vf27, vf00, I, I = 256        ;; vf27.w = 256

itof0.xyzw vf25, vf25
itof0.xyzw vf26, vf26

ior vi02, vi05, vi00 ;; vi02 = srcdst table

add.xyzw vf25, vf25, vf27    ;; hack float add trick
add.xyzw vf26, vf26, vf27

miniy.xyzw vf13, vf13, vf17 ;; float tricks
ibne vi06, vi05, L150 ;; branch if there are samecopys

ior vi06, vi07, vi00       |  max.xyzw vf25, vf26, vf26

L150:

;; load from copy table
lqi.xyzw vf27, vi05
;; table to float
itof0.xyzw vf27, vf27

```
