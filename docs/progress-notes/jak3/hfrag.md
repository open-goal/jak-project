# C++ Plan

- Write the extractor:
  - giant mesh vertex data (position, color, texture coordinates per vertex)
  - giant mesh index data (a chunk of indices per corner)
  - list of buckets, each contains a list of corners in the bucket, and montage table
  - Each corner has a vis ID for precomputed vis, bsphere for view frustum culling, and index group
  - time-of-day colors
  - Draw mode for textures (HACK: get the un-animated textures in here too, just for debug)

- Write the loader
  - load mesh and colors to GPU

- Write the C++ renderer
  - skip texture for now.

- Write the GOAL renderer code (send camera data/visibility data to C++)

- Debug!

- Add textures (un-animated)

# Vertex

- hfragment
- buckets: collection of corners, likely using shared texture?
- "corners": smallest thing that can be processed by vertex generation asm
- "polyN": square grid of vertices, culled together

# Far Vertices

Vertex pos spacing is 32
index0 = index1 = index2.

5 chains

always poly25's.
Each poly25 is a 5x5 grid of vertex indices.

160x160 position units.

Can optionally scissor.

Taken if entire "corner" sphere is past far threshold.

# Mid Vertices

Vertex pos spacing is 16
index0,1,2 different.

9 chains

(poly-mid25, far, if the sphere is in mid and far. No scissoring support.)
2x2 grid of poly25's. Each poly25 is a 5x5 grid of vertex indices.
160x160 position unit

(poly-mid, far-mid, if the sphere is entirely mid. no scissoring.)
4x4 grid of poly9's. Each poly9 is a 3x3 grid of vertex indices.
192x192 position units


# Near Vertices

Vertex pos spacing is 8.
index0,1,2 different.

17 chains

(poly-near, near-mid, if sphere is in mid and near, has scissoring support.)
8x8 grid of poly9's, each poly9 is a grid of 3x3 indices.
192x129 position units

poly4 as well??
"near" if sphere is in near. No scissoring option (always on?)

# Poly4
2x2
12 quadword upload

# Poly9
3x3
36 quadword upload

# poly25
5x5
120 quadword upload

# DMA generation asms

- `near`, 31, 4326 lines
- `near-mid`, 30, 3274
- `mid`, 29
- `far-mid`, 28, 3244 line
- `far`, 27, 753 lines
- `far-scissor`, 18
- `near-mid-scissor`, 21, 3628 lines

# Montage Texture Coords

16x8 grid texture.

# VU1 program addresses

- 0x06: scissor??
- 0x0c: another init??
- 0x20: abort
- 0x2c: init
- 0x2e: poly4-near
- 0x7d: poly25-far
- 0x7f: poly25-mid
- 0x14b: poly9-mid
- 0x14f: poly9-near

# hfrag-vert-index
Metadata about a vertex?

# Corners
32x32 grid
Each corner is 524288 apart (128 meters)

# Draw table (in work)
Stores linked list (by index, not pointer) of chains per mode.

Example:
`work.next-far-scissor` points to entry in draw-table for the first corner to be drawn with far scissor. Value in the draw table is the index of the next corner... eventually it's -1, and that's the end of the chain.

# Drawing process
- `init-work-from-current-hfrag`
  - maybe set lowres
  - build corner array
  - zero out work's next/count (next is set to -1)
  - zero out hfragment static data bucket next/count (next is 0)
- `pick-level-of-detail`
  - loop over all corners, check vis bit.
  - do sphere cull to reject
  - do guard-band-cull to set scissor
  - subdiv-index bits: 8: back mid, 4: front mid, 2: back near, 1: front near
  - build draw-table chains per rendering type
- `trim-draw-lists`
  - remove stuff from draw table if too big
- `time-of-day-interp-colors-scratch`

- `generate-dma`
  - call asm stuff
- method 23
  - no idea
- `finalize-dma`
  - shader asm stuff, finshing up buffers.

# `asm-near` summary

## Vertex 0
`vt0` only

## Vertex 1
`weight * vt0 + (1 - weight) * 0.5 * (vt1 + vt2)`

## Vertex 2
`vt0` only

## Vertex 3
`weight * vt0 + (1 - weight) * 0.5 * (vt1 + vt2)`

## Vertex 4
`weight * vt0 + (1 - weight) * 0.5 * (vt1 + vt2)`

## Vertex 5
`weight * vt0 + (1 - weight) * 0.5 * (vt1 + vt2)`

## Vertex 6
`vt0` only

## Vertex 7
`weight * vt0 + (1 - weight) * 0.5 * (vt1 + vt2)`

## Vertex 8
`vt0` only

# DMA from spr order (within a poly9)
0: 16 byte header
16: c0's pos (xyzw)
32: c0, c3, c1, c4 (pos'ys as floats)
48: 16 byte gap
64: c0, c3, c1, c4 (colors as u8's packed)


# poly9 orders

;; 0  1  2  | 0  1  2
;; 3  4  5  | 3  4  5
;; 6  7  8  | 6  7  8

(0, 3, 1, 4)
(1, 4, 2, 5)
(3, 6, 4, 7)
(4, 7, 5, 8)


First poly9 is special (no shared verts w/ prev)
Then 6 shared poly9's

0
1
2
3
4
5
6

# Texture Stuff

```lisp
(defmethod login ((this hfragment))
  "Initialize the object after it is loaded."
  (dotimes (s5-0 3)
    (adgif-shader-login (-> this shaders s5-0))
    (if (> s5-0 0)
        (set! (-> this shaders s5-0 tex0 tcc) 0)
        )
    (set! (-> this shaders 1 tex0 cbp) 3904)
    (set! (-> this shaders 2 tex0 cbp) 3904)
    )
  this
  )
```

# Method 23 near texture data summary:

- `work.frame-tmpl`: 4 a+d data setup
- `work.frames[0]`:  a+d for frame (hard-coded fbp), scissor 0x7f, xyoffset 0, test (no zbuf, always pass)
- `work.adgif-tmpl`: usual adgif direct template
- `hfrag.shaders[2]`: the whole thing.

Loop over montage (16x?)
- `hfrag.sprite-tmpl`: 2 qw (template rgba, st, xyz, st, xyz)
- `colors[0] = 0x80, 0x80, 0x80, 0x80`
- `montage-tex-coords[montage[m]].st0`
- `near-data[2 * m]`
- `montage-tex-coords[montage[m]].st1`
- `near-data[2 * m + 1]`

- `work.frame-tmpl`
- `work.frames[1]`: half size frame, seems like the MIP data


# C++ version of texture stuff:

```c++
struct Bucket {
    std::vector<int> corners;
    u16 montage_table[16];
};
```

# `asm-near`

```
L15:
    daddiu sp, sp, -64
    sd ra, 0(sp)
    sq s4, 16(sp)
    sq s5, 32(sp)
    sq gp, 48(sp)

a0 hfrag-work
a1 dma-buffer
a2 draw-table chain

B0:
    sll r0, r0, 0
    lw v1, 4(a1)     ;; v1 = dma-buffer.base
    sll r0, r0, 0
    sw a1, 8148(a0)  ;; stash dma-buffer in work
    lui t0, 4096     ;; 0x1000????
    lui t1, 4096
    sync.l              ;; cache stuff
    cache dxwbin v1, 0
    sync.l
    cache dxwbin v1, 1
    sync.l
    lui a3, 28672      ;; a3 = scratchpad memory 0x7000'0000
    daddiu v1, a0, 512 ;; v1 = work.near-chaina
    daddiu t3, a0, 800 ;; t3 = work.near-chainb
    ori t0, t0, 54272  ;; t0 = 0x1000'd400 (SPR-TO DMA)
    ori t2, t1, 53248  ;; t2 = 0x1000'd000 (SPR-FROM DMA)
    or t1, a3, r0      ;; t1 = scratchpad memory (input bank buffer)
    sw v1, 8180(a0)    ;; work.chain-ptr-next = work.near-chaina
    vmaxw.xyzw vf11, vf0, vf0  ;; vf11 = [1, 1, 1, 1]
    sw t3, 8176(a0)    ;; work.chain-ptr = work.near-chainb
    lw t3, 4(a1)       ;; t3 = dma-buffer.base
    ori v1, a3, 2720   ;; v1 = spad.outa
    addiu a1, r0, 0    ;; a1 = 0
    sw t0, 8192(a0)    ;; stash to-spr (ctrl reg)
    or t0, v1, r0      ;; t0 = spad.outa
    sw t2, 8196(a0)    ;; stash from-spr (ctrl reg)
    sll r0, r0, 0
    sw t3, 8152(a0)    ;; stash base
    sll r0, r0, 0
    lqc2 vf20, 7840(a0) ;; stq.vectors[0] = [1, 0, 0, 0]
    sll r0, r0, 0
    lqc2 vf21, 7872(a0) ;; stq.vectors[2] = [0x1001, 0, 0, 0]
    sll r0, r0, 0
    lqc2 vf22, 7936(a0) ;; stq.vectors[6] = [0x1000, 0, 0, 0]
    sll r0, r0, 0
    lqc2 vf23, 7968(a0) ;; stq.vectors[8] = [0x1000, 0x1000, 0, 0]
    lui t2, 8192        ;; t2 = 0x2000'????
    lw t3, 8176(a0)     ;; t3 = work.chain-ptr
    andi t6, a2, 31     ;; t6 = corner_idx[0] (maybe x?)  idx = 32 * z + x
    lw t4, 8288(a0)     ;; t4 = work.frag.verts
    sra t5, a2, 5       ;; t5 = corner_idx[1] (maybe z?)
    sll t6, t6, 6       ;; t6 = cx * 64
    sll t5, t5, 15      ;; t5 = cz * 32768 ???
    daddu t4, t6, t4
    or t2, t3, t2       ;; uncached chain ptr.
    daddu t3, t4, t5

    ;; loop over gi's, upload 5qw

    ;; vertex data

    ;; verts is a 512x512 grid of 4-byte vertices.
    ;; vx,vz -> vx + vz * 512 -> (vx * 4 + vz * 2048)

    ;; a "corner" is a 16x16 tile of vertices (there are 32x32 corners)
    ;; cx,cz -> v(cx * 16, cz * 16) -> (cx * 64 + cz * 32768)

    ;; DMA chain to scratchpad to load vertices
    ;; vx_start = cx * 16
    ;; vz_start = cz * 16
    ;; for vz in range(vz_start, vz_start + 17):
    ;;    for vx in range(vx_start, vx_start + 20):
    ;;       vi = vx + 512 * vz
    ;;       vaddr = vi * 4
    ;;       get_vertex(vaddr)

    ;; to draw a corner, you need 1 vertex past the end in z and x

    ;; patching near-chain addresses for each line of vertices
    sw t3, 4(t2)
    daddiu t3, t3, 2048
    sw t3, 20(t2)
    daddiu t3, t3, 2048
    sw t3, 36(t2)
    daddiu t3, t3, 2048
    sw t3, 52(t2)
    daddiu t3, t3, 2048
    sw t3, 68(t2)
    daddiu t3, t3, 2048
    sw t3, 84(t2)
    daddiu t3, t3, 2048
    sw t3, 100(t2)
    daddiu t3, t3, 2048
    sw t3, 116(t2)
    daddiu t3, t3, 2048
    sw t3, 132(t2)
    daddiu t3, t3, 2048
    sw t3, 148(t2)
    daddiu t3, t3, 2048
    sw t3, 164(t2)
    daddiu t3, t3, 2048
    sw t3, 180(t2)
    daddiu t3, t3, 2048
    sw t3, 196(t2)
    daddiu t3, t3, 2048
    sw t3, 212(t2)
    daddiu t3, t3, 2048
    sw t3, 228(t2)
    daddiu t3, t3, 2048
    sw t3, 244(t2)
    daddiu t3, t3, 2048
    sw t3, 260(t2)

    sll r0, r0, 0
    lw t2, 8192(a0) ;; to-spr
    sll r0, r0, 0

;; wait for to-spr transfer to finish
B1:
L16:
    lw t3, 0(t2)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t3, t3, 256
    sll r0, r0, 0
    beq t3, r0, L17
    sll r0, r0, 0

B2:
    sll r0, r0, 0
    lw t3, 8156(a0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu t3, t3, 1
    sll r0, r0, 0
    sw t3, 8156(a0)
    beq r0, r0, L16
    sll r0, r0, 0

B3:
L17:
    sll r0, r0, 0
    lw t3, 8176(a0) ;; t3 = chain-ptr (to-spr vertex gather)
    sll r0, r0, 0
    lw t4, 8180(a0) ;; t4 = chain-ptr-next
    sll r0, r0, 0
    sw t3, 8180(a0) ;; swap chain-ptr to chan-ptr-next
    sll r0, r0, 0
    sw t4, 8176(a0) ;; swap chain-ptr-next to chain-ptr
    sw t1, 128(t2)  ;; set spad addr
    sll r0, r0, 0
    sw t3, 48(t2)   ;; set chain ptr
    sll r0, r0, 0
    sw r0, 32(t2)   ;; other control stuf
    addiu t3, r0, 260
    sw t3, 0(t2)    ;; run DMA to spr!
    xori t1, t1, 1360 ;; toggling bank ptr buffer (next spad upload addr)
    sll r0, r0, 0
    or a2, a2, r0   ;; a2 still draw-table index

;; loop over corners
B4:
L18:
    dsll t2, a2, 1    ;; int16's
    sll r0, r0, 0
    daddu t2, t2, a0  ;; idx * 2 + hfrag_work
    dsll t3, a2, 4    ;; corner_index * 16
    bltz a2, L21      ;; check if draw index is negative, if so don't start next one.
    lh a2, 8456(t2)   ;; load next draw

B5:
    lui t2, 8192
    lw t4, 8176(a0) ;; t4 = chain-ptr
    andi t7, a2, 31
    lw t5, 8288(a0)  ;; t5 - verts
    sra t6, a2, 5
    sll t7, t7, 6
    sll t6, t6, 15
    daddu t5, t7, t5
    or t2, t4, t2
    daddu t4, t5, t6

    ;; patch the chain
    sw t4, 4(t2)
    daddiu t4, t4, 2048
    sw t4, 20(t2)
    daddiu t4, t4, 2048
    sw t4, 36(t2)
    daddiu t4, t4, 2048
    sw t4, 52(t2)
    daddiu t4, t4, 2048
    sw t4, 68(t2)
    daddiu t4, t4, 2048
    sw t4, 84(t2)
    daddiu t4, t4, 2048
    sw t4, 100(t2)
    daddiu t4, t4, 2048
    sw t4, 116(t2)
    daddiu t4, t4, 2048
    sw t4, 132(t2)
    daddiu t4, t4, 2048
    sw t4, 148(t2)
    daddiu t4, t4, 2048
    sw t4, 164(t2)
    daddiu t4, t4, 2048
    sw t4, 180(t2)
    daddiu t4, t4, 2048
    sw t4, 196(t2)
    daddiu t4, t4, 2048
    sw t4, 212(t2)
    daddiu t4, t4, 2048
    sw t4, 228(t2)
    daddiu t4, t4, 2048
    sw t4, 244(t2)
    daddiu t4, t4, 2048
    sw t4, 260(t2)
    sll r0, r0, 0

   ;; wait for spr to to be free
    lw t2, 8192(a0)
    sll r0, r0, 0
B6:
L19:
    lw t4, 0(t2)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t4, t4, 256
    sll r0, r0, 0
    beq t4, r0, L20
    sll r0, r0, 0

B7:
    sll r0, r0, 0
    lw t4, 8156(a0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu t4, t4, 1
    sll r0, r0, 0
    sw t4, 8156(a0)
    beq r0, r0, L19
    sll r0, r0, 0

B8:
L20:
    sll r0, r0, 0    ;; swap chain-ptrs
    lw t4, 8176(a0)
    sll r0, r0, 0
    lw t5, 8180(a0)
    sll r0, r0, 0
    sw t4, 8180(a0)
    sll r0, r0, 0
    sw t5, 8176(a0)
    sw t1, 128(t2)   ;; start next vetex gather dma
    sll r0, r0, 0
    sw t4, 48(t2)
    sll r0, r0, 0
    sw r0, 32(t2)
    addiu t4, r0, 260
    sw t4, 0(t2)
    xori t1, t1, 1360 ;; swap spad input bank ptr
    beq r0, r0, L23
    sll r0, r0, 0
    ;; end next vertex gather dma setup.

;; wait for spad to to complete if we're doing the last one.
B9:
L21:
    lw t2, 8192(a0)
    sll r0, r0, 0
B10:
L22:
    lw t4, 0(t2)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t4, t4, 256
    sll r0, r0, 0
    beq t4, r0, L23
    sll r0, r0, 0

B11:
    sll r0, r0, 0
    lw t4, 8156(a0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu t4, t4, 1
    sll r0, r0, 0
    sw t4, 8156(a0)
    beq r0, r0, L22
    sll r0, r0, 0

;; assume we have input data in spad
B12:
L23:
    addiu t2, r0, 6     ;; t2 = 6
    lhu t4, 2(t1)       ;; t4 = vertex[0,0].packed-index
    daddu t5, t3, a0    ;; preparing to index into work.corners
    lw t3, 8384(a0)     ;; t3 = work.poly-near
    dsra t4, t4, 11     ;; t4 = bit11 (unsigned)
    lqc2 vf15, 10512(t5) ;; vf15 = work.corners[this_corner]
    beq t4, r0, L52     ;; skip entirely if bit11.16 is 0
    sll r0, r0, 0

B13:
    daddiu t4, a1, -171  ;; t4 = a1 (0) - 171
    sll r0, r0, 0
    blez t4, L26
    sll r0, r0, 0

B14:
    lw t4, 8196(a0)
    sll r0, r0, 0
B15:
L24: ;; wait for from-spr to finish
    lw t0, 0(t4)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t0, t0, 256
    sll r0, r0, 0
    beq t0, r0, L25
    sll r0, r0, 0

B16:
    sll r0, r0, 0
    lw t0, 8160(a0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu t0, t0, 1
    sll r0, r0, 0
    sw t0, 8160(a0)
    beq r0, r0, L24
    sll r0, r0, 0

B17:
L25: ;; swapping from-spr buffer
    lw t0, 8152(a0)
    sll r0, r0, 0
    sw v1, 128(t4)
    xori v1, v1, 4720
    sw t0, 16(t4)
    sll t5, a1, 4
    addu t5, t0, t5
    or t0, v1, r0
    sw a1, 32(t4)
    addiu a1, r0, 256
    sw a1, 0(t4)
    addiu a1, r0, 0
    sw t5, 8152(a0)
    sll r0, r0, 0
B18:

;; start of actual vertex procssing.
L26:
    daddiu t4, a3, 9984 ;; t4 = hfrag-cache-line
    addiu t5, r0, 6     ;; t5 = 6
    sll r0, r0, 0
;; Vertex 0
    lhu t6, 2(t3)       ;; t6 = p9[0].index0
    sll r0, r0, 0
    lhu t7, 0(t3)       ;; t7 = p9[0].pos (packed u8's)
    daddu t6, t6, t1    ;; t6 = gathered_verts[p9[0].index0] (index0 has the shift applied)
    sll r0, r0, 0
    pextlb t8, r0, t7   ;; expanding the pos (u8's to u16's)
    lhu t7, 2(t6)       ;; t7 = t6(the vertex).packed-index
    pextlh t8, r0, t8   ;; expanding the pos (u32's) -> [pos.x, pos.y, 0, 0]
    lhu t6, 0(t6)       ;; t6 = t6(the vertex).height
    dsll t8, t8, 12     ;; t8 -> pos to meters (multiply by 4096)
    andi t7, t7, 2047   ;; extract first 11 bits of packed-index
    pexcw t8, t8        ;; t8 = [pos.x * 2048, 0, pos.y * 2048, 0]
    mfc1 r0, f31
    dsll t7, t7, 2      ;; t7 = vert.packed-index.first11 * 4
    qmtc2.i vf1, t8     ;; vf1 = pos
    daddu t7, t7, a3    ;; t7 = spad + 4 * packed-index.first11
    sll t6, t6, 3       ;; t6 = height is multiplied by 8
    vitof0.xyzw vf1, vf1  ;; vf1 pos to float
    mtc1 f0, t6         ;; f0 = t6 height to fpr
    sll r0, r0, 0
    lw t6, 12288(t7)    ;; load colors!
    cvt.s.w f0, f0      ;; height to float
    sll r0, r0, 0
    vadd.xyzw vf1, vf1, vf15 ;; vf1 += works.corners[this_corner]
    sll r0, r0, 0
    pextlb t6, r0, t6  ;; colors expand to u16
    mfc1 r0, f31
    pextlh t6, r0, t6  ;; colors expand to u32
    mfc1 r0, f31
    sll r0, r0, 0
    sq t6, 0(t4)       ;; stash colors in hfrag-cache-vertex[0]
    sll r0, r0, 0
    sqc2 vf1, 16(t4)   ;; stash pos in hfrag-cache-vertex[0]
    sll r0, r0, 0
    swc1 f0, 20(t4)    ;; stash y component (height) in hfrag-cache-vertex[0]
    sll r0, r0, 0
    lqc2 vf1, 16(t4)   ;; load pos, now containing the height

    ;; camera projection of the vertex
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31 ;; multiply for clipping check
    sll r0, r0, 0
    vclip.xyz vf1, vf1 ;; check if in view...
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping ;; check clipping result
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47 ;; mask clip flag for this clip
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 28(t4)   ;; stash clip result for later
    sll r0, r0, 0

;; Vertex 1
    lhu t6, 10(t3)   ;; t6 = p9[1].index0
    sll r0, r0, 0
    lhu t8, 12(t3)    ;; t8 = p9[1].index1
    daddu t7, t6, t1  ;; t7 = verts[p9[1].index0] vt0
    lhu t6, 14(t3)    ;; t6 = p9[1].index2
    daddu t8, t8, t1  ;; t8 = verts[p9[1].index1] vt1
    lhu t9, 8(t3)     ;; t9 = pos
    daddu t6, t6, t1  ;; t6 = verts[p9[1].index2] vt2
    lhu ra, 2(t7)     ;; ra = vt0.packed-index
    pextlb t9, r0, t9 ;; pos to u16's
    lhu t7, 0(t7)     ;; t7 = vt0.height
    pextlh t9, r0, t9 ;; pos to u32's
    lhu gp, 2(t8)     ;; gp = vt1.packed-index
    dsll t9, t9, 12   ;; pos *= 4096
    lhu t8, 0(t8)     ;; t8 = vt1.height
    pexcw t9, t9      ;; move pos.y to pos.z
    lhu s5, 2(t6)     ;; s5 = vt2.packed-index
    andi ra, ra, 2047 ;; mask packed index to get color indices
    andi gp, gp, 2047
    andi s5, s5, 2047
    dsll s4, ra, 2    ;; shift to load 32-bit rgba's from colors
    dsll gp, gp, 2
    dsll ra, s5, 2
    daddu s4, s4, a3  ;; color addr
    lhu t6, 0(t6)     ;; t6 = vt2.height
    sll s5, t7, 3     ;; mult height by 8
    lw t7, 12288(s4)  ;; t7 = vt0's color
    daddu gp, gp, a3  ;; color addr
    sq t9, 8080(a0)   ;; stash pos in work
    sll t8, t8, 3     ;; mult height by 8
    lw t9, 12288(gp)  ;; t9 = vt1's color
    daddu ra, ra, a3  ;; color addr
    sw s5, 8084(a0)   ;; store vt0.height * 8 in pos-temp.y
    sll t6, t6, 3     ;; mult height by 8
    lw ra, 12288(ra)  ;; ra = vt2's color
    pextlb t7, r0, t7 ;; color to u16's
    lqc2 vf1, 8080(a0) ;; vf1 = load pos-temp with [px_i, vt0.h * 8, pz_i, 0]
    pextlh t7, r0, t7 ;; color to u32's
    sw t8, 8084(a0)   ;; store vt1's height in pos-temp
    pextlb t8, r0, t9 ;; color u16's
    qmtc2.i vf4, t7   ;; vf4 = vt0's color
    pextlh t7, r0, t8 ;; t7 = vt1's color
    lqc2 vf2, 8080(a0) ;; vf2 = pos-temp with [px_i, vt1.h * 8, pz_i, 0]
    pextlb t8, r0, ra ;; color to u16's
    qmtc2.i vf5, t7   ;; vf5 = vt1's color
    pextlh t7, r0, t8  ;; vt2's color to u32's
    sw t6, 8084(a0)    ;; store vt2's height in pos-temp
    vitof0.xyzw vf4, vf4 ;; vf4 = vt0's color as float
    qmtc2.i vf6, t7    ;; vf6 = vt2's color
    vitof0.xyzw vf5, vf5 ;; vf5 = vt1's color as float
    lqc2 vf3, 8080(a0) ;; vf3 = vt2 pos temp

    ;; vf1,vf2,vf3 - positions; vf4,vf5,vf6 - colors;
    ;; convert all to floats
    vitof0.xyzw vf6, vf6
    sll r0, r0, 0
    vitof0.xyzw vf1, vf1
    sll r0, r0, 0
    vitof0.xyzw vf2, vf2
    sll r0, r0, 0
    vitof0.xyzw vf3, vf3

    sll r0, r0, 0
    vadd.xyzw vf5, vf5, vf6 ;; vf5 = vt1.col + vt2.col
    sll r0, r0, 0
    vadd.xz vf1, vf1, vf15  ;; vf1 = vt0.pos + [cx, 0, cz] < world position of vt0
    sll r0, r0, 0
    vmulaw.xyzw acc, vf27, vf0 ;; acc = cam-pos
    sll r0, r0, 0
    vadd.xyzw vf2, vf2, vf3    ;; vf2 = vt1.pos + vt2.pos
    sll r0, r0, 0
    vmulx.xyzw vf5, vf5, vf28  ;; vf5 = 0.5*(vt1.col + vt2.col)
    sll r0, r0, 0
    vmaddax.xyzw acc, vf24, vf1 ;; camera-rot multiplication...
    sll r0, r0, 0
    vmadday.xyzw acc, vf25, vf1 ;; camera-rot multiplication...
    sll r0, r0, 0
    vmaddz.xyzw vf7, vf26, vf1  ;; vf7 = pos_vt0_rt_camera
    sll r0, r0, 0
    vmulx.xyzw vf2, vf2, vf28   ;; vf2 = 0.5*(vt1.pos + vt2.pos)
    sll r0, r0, 0
    vsub.xyzw vf6, vf4, vf5     ;; vf6 = vt0.col - 0.5*(vt1.col + vt2.col)
    sll r0, r0, 0
    vaddz.xyzw vf7, vf13, vf7   ;; vf7 = depth-rt-cam + dists
    sll r0, r0, 0
    vsub.xyz vf3, vf1, vf2      ;; vf3 = corner + vt0.pos - 0.5*(vt1.pos + vt2.pos)
    sll r0, r0, 0
    vmulw.w vf3, vf0, vf0       ;; vf3.w = 1
    sll r0, r0, 0
    vmul.xyzw vf7, vf7, vf14    ;; vf7 = (depth-rt-cam + dists)*rdists
    sll r0, r0, 0
    vmini.xyzw vf7, vf7, vf11   ;; clamp dists within 0 to 1
    sll r0, r0, 0
    vmaxx.xyzw vf7, vf7, vf0    ;; clamp dists within 0 to 1
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf5   ;; acc = 0.5*(vt1.col + vt2.col)
    sll r0, r0, 0
    vmaddy.xyzw vf4, vf6, vf7   ;; vf4 = 0.5*(vt1.col + vt2.col) + weight * (vt0.col - 0.5*(vt1.col + vt2.col))
                                ;; vf4 = weight * vt0 + (1 - weight) * 0.5 * (vt1.col + vt2.col)
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf2
    sll r0, r0, 0
    vmaddy.yw vf1, vf3, vf7
    sll r0, r0, 0
    vftoi0.xyzw vf4, vf4 ;; colors back to int
    sll r0, r0, 0
    sll r0, r0, 0
    sqc2 vf4, 32(t4) ;; store in cache vertex 1
    sll r0, r0, 0
    sqc2 vf1, 48(t4) ;; pos
    sll r0, r0, 0
    lqc2 vf1, 48(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 60(t4) ;; store clip
    sll r0, r0, 0

;; Vertex 2
    lhu t6, 18(t3) ;; p9[2].index0
    sll r0, r0, 0
    lhu t7, 16(t3)
    daddu t6, t6, t1
    sll r0, r0, 0
    pextlb t8, r0, t7
    lhu t7, 2(t6)
    pextlh t8, r0, t8
    lhu t6, 0(t6)
    dsll t8, t8, 12
    andi t7, t7, 2047
    pexcw t8, t8
    mfc1 r0, f31
    dsll t7, t7, 2
    qmtc2.i vf1, t8
    daddu t7, t7, a3
    sll t6, t6, 3
    vitof0.xyzw vf1, vf1
    mtc1 f0, t6
    sll r0, r0, 0
    lw t6, 12288(t7)
    cvt.s.w f0, f0
    sll r0, r0, 0
    vadd.xyzw vf1, vf1, vf15
    sll r0, r0, 0
    pextlb t6, r0, t6
    mfc1 r0, f31
    pextlh t6, r0, t6
    mfc1 r0, f31
    sll r0, r0, 0
    sq t6, 64(t4) ;; store in vertex 2 of cache
    sll r0, r0, 0
    sqc2 vf1, 80(t4)
    sll r0, r0, 0
    swc1 f0, 84(t4)
    sll r0, r0, 0
    lqc2 vf1, 80(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 92(t4)
    sll r0, r0, 0

# Vertex 3
    lhu t6, 26(t3)
    sll r0, r0, 0
    lhu t8, 28(t3)
    daddu t7, t6, t1
    lhu t6, 30(t3)
    daddu t8, t8, t1
    lhu t9, 24(t3)
    daddu t6, t6, t1
    lhu ra, 2(t7)
    pextlb t9, r0, t9
    lhu t7, 0(t7)
    pextlh t9, r0, t9
    lhu gp, 2(t8)
    dsll t9, t9, 12
    lhu t8, 0(t8)
    pexcw t9, t9
    lhu s5, 2(t6)
    andi ra, ra, 2047
    andi gp, gp, 2047
    andi s5, s5, 2047
    dsll s4, ra, 2
    dsll gp, gp, 2
    dsll ra, s5, 2
    daddu s4, s4, a3
    lhu t6, 0(t6)
    sll s5, t7, 3
    lw t7, 12288(s4)
    daddu gp, gp, a3
    sq t9, 8080(a0)
    sll t8, t8, 3
    lw t9, 12288(gp)
    daddu ra, ra, a3
    sw s5, 8084(a0)
    sll t6, t6, 3
    lw ra, 12288(ra)
    pextlb t7, r0, t7
    lqc2 vf1, 8080(a0)
    pextlh t7, r0, t7
    sw t8, 8084(a0)
    pextlb t8, r0, t9
    qmtc2.i vf4, t7
    pextlh t7, r0, t8
    lqc2 vf2, 8080(a0)
    pextlb t8, r0, ra
    qmtc2.i vf5, t7
    pextlh t7, r0, t8
    sw t6, 8084(a0)
    vitof0.xyzw vf4, vf4
    qmtc2.i vf6, t7
    vitof0.xyzw vf5, vf5
    lqc2 vf3, 8080(a0)
    vitof0.xyzw vf6, vf6
    sll r0, r0, 0
    vitof0.xyzw vf1, vf1
    sll r0, r0, 0
    vitof0.xyzw vf2, vf2
    sll r0, r0, 0
    vitof0.xyzw vf3, vf3
    sll r0, r0, 0
    vadd.xyzw vf5, vf5, vf6
    sll r0, r0, 0
    vadd.xz vf1, vf1, vf15
    sll r0, r0, 0
    vmulaw.xyzw acc, vf27, vf0
    sll r0, r0, 0
    vadd.xyzw vf2, vf2, vf3
    sll r0, r0, 0
    vmulx.xyzw vf5, vf5, vf28
    sll r0, r0, 0
    vmaddax.xyzw acc, vf24, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf25, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf7, vf26, vf1
    sll r0, r0, 0
    vmulx.xyzw vf2, vf2, vf28
    sll r0, r0, 0
    vsub.xyzw vf6, vf4, vf5
    sll r0, r0, 0
    vaddz.xyzw vf7, vf13, vf7
    sll r0, r0, 0
    vsub.xyz vf3, vf1, vf2
    sll r0, r0, 0
    vmulw.w vf3, vf0, vf0
    sll r0, r0, 0
    vmul.xyzw vf7, vf7, vf14
    sll r0, r0, 0
    vmini.xyzw vf7, vf7, vf11
    sll r0, r0, 0
    vmaxx.xyzw vf7, vf7, vf0
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf5
    sll r0, r0, 0
    vmaddy.xyzw vf4, vf6, vf7
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf2
    sll r0, r0, 0
    vmaddy.yw vf1, vf3, vf7
    sll r0, r0, 0
    vftoi0.xyzw vf4, vf4
    sll r0, r0, 0
    sll r0, r0, 0
    sqc2 vf4, 96(t4)
    sll r0, r0, 0
    sqc2 vf1, 112(t4)
    sll r0, r0, 0
    lqc2 vf1, 112(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 124(t4)
    sll r0, r0, 0

# Vertex 4
    lhu t6, 34(t3)
    sll r0, r0, 0
    lhu t8, 36(t3)
    daddu t7, t6, t1
    lhu t6, 38(t3)
    daddu t8, t8, t1
    lhu t9, 32(t3)
    daddu t6, t6, t1
    lhu ra, 2(t7)
    pextlb t9, r0, t9
    lhu t7, 0(t7)
    pextlh t9, r0, t9
    lhu gp, 2(t8)
    dsll t9, t9, 12
    lhu t8, 0(t8)
    pexcw t9, t9
    lhu s5, 2(t6)
    andi ra, ra, 2047
    andi gp, gp, 2047
    andi s5, s5, 2047
    dsll s4, ra, 2
    dsll gp, gp, 2
    dsll ra, s5, 2
    daddu s4, s4, a3
    lhu t6, 0(t6)
    sll s5, t7, 3
    lw t7, 12288(s4)
    daddu gp, gp, a3
    sq t9, 8080(a0)
    sll t8, t8, 3
    lw t9, 12288(gp)
    daddu ra, ra, a3
    sw s5, 8084(a0)
    sll t6, t6, 3
    lw ra, 12288(ra)
    pextlb t7, r0, t7
    lqc2 vf1, 8080(a0)
    pextlh t7, r0, t7
    sw t8, 8084(a0)
    pextlb t8, r0, t9
    qmtc2.i vf4, t7
    pextlh t7, r0, t8
    lqc2 vf2, 8080(a0)
    pextlb t8, r0, ra
    qmtc2.i vf5, t7
    pextlh t7, r0, t8
    sw t6, 8084(a0)
    vitof0.xyzw vf4, vf4
    qmtc2.i vf6, t7
    vitof0.xyzw vf5, vf5
    lqc2 vf3, 8080(a0)
    vitof0.xyzw vf6, vf6
    sll r0, r0, 0
    vitof0.xyzw vf1, vf1
    sll r0, r0, 0
    vitof0.xyzw vf2, vf2
    sll r0, r0, 0
    vitof0.xyzw vf3, vf3
    sll r0, r0, 0
    vadd.xyzw vf5, vf5, vf6
    sll r0, r0, 0
    vadd.xz vf1, vf1, vf15
    sll r0, r0, 0
    vmulaw.xyzw acc, vf27, vf0
    sll r0, r0, 0
    vadd.xyzw vf2, vf2, vf3
    sll r0, r0, 0
    vmulx.xyzw vf5, vf5, vf28
    sll r0, r0, 0
    vmaddax.xyzw acc, vf24, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf25, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf7, vf26, vf1
    sll r0, r0, 0
    vmulx.xyzw vf2, vf2, vf28
    sll r0, r0, 0
    vsub.xyzw vf6, vf4, vf5
    sll r0, r0, 0
    vaddz.xyzw vf7, vf13, vf7
    sll r0, r0, 0
    vsub.xyz vf3, vf1, vf2
    sll r0, r0, 0
    vmulw.w vf3, vf0, vf0
    sll r0, r0, 0
    vmul.xyzw vf7, vf7, vf14
    sll r0, r0, 0
    vmini.xyzw vf7, vf7, vf11
    sll r0, r0, 0
    vmaxx.xyzw vf7, vf7, vf0
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf5
    sll r0, r0, 0
    vmaddy.xyzw vf4, vf6, vf7
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf2
    sll r0, r0, 0
    vmaddy.yw vf1, vf3, vf7
    sll r0, r0, 0
    vftoi0.xyzw vf4, vf4
    sll r0, r0, 0
    sll r0, r0, 0
    sqc2 vf4, 128(t4)
    sll r0, r0, 0
    sqc2 vf1, 144(t4)
    sll r0, r0, 0
    lqc2 vf1, 144(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 156(t4)
    sll r0, r0, 0

# Vertex 5
    lhu t6, 42(t3)
    sll r0, r0, 0
    lhu t8, 44(t3)
    daddu t7, t6, t1
    lhu t6, 46(t3)
    daddu t8, t8, t1
    lhu t9, 40(t3)
    daddu t6, t6, t1
    lhu ra, 2(t7)
    pextlb t9, r0, t9
    lhu t7, 0(t7)
    pextlh t9, r0, t9
    lhu gp, 2(t8)
    dsll t9, t9, 12
    lhu t8, 0(t8)
    pexcw t9, t9
    lhu s5, 2(t6)
    andi ra, ra, 2047
    andi gp, gp, 2047
    andi s5, s5, 2047
    dsll s4, ra, 2
    dsll gp, gp, 2
    dsll ra, s5, 2
    daddu s4, s4, a3
    lhu t6, 0(t6)
    sll s5, t7, 3
    lw t7, 12288(s4)
    daddu gp, gp, a3
    sq t9, 8080(a0)
    sll t8, t8, 3
    lw t9, 12288(gp)
    daddu ra, ra, a3
    sw s5, 8084(a0)
    sll t6, t6, 3
    lw ra, 12288(ra)
    pextlb t7, r0, t7
    lqc2 vf1, 8080(a0)
    pextlh t7, r0, t7
    sw t8, 8084(a0)
    pextlb t8, r0, t9
    qmtc2.i vf4, t7
    pextlh t7, r0, t8
    lqc2 vf2, 8080(a0)
    pextlb t8, r0, ra
    qmtc2.i vf5, t7
    pextlh t7, r0, t8
    sw t6, 8084(a0)
    vitof0.xyzw vf4, vf4
    qmtc2.i vf6, t7
    vitof0.xyzw vf5, vf5
    lqc2 vf3, 8080(a0)
    vitof0.xyzw vf6, vf6
    sll r0, r0, 0
    vitof0.xyzw vf1, vf1
    sll r0, r0, 0
    vitof0.xyzw vf2, vf2
    sll r0, r0, 0
    vitof0.xyzw vf3, vf3
    sll r0, r0, 0
    vadd.xyzw vf5, vf5, vf6
    sll r0, r0, 0
    vadd.xz vf1, vf1, vf15
    sll r0, r0, 0
    vmulaw.xyzw acc, vf27, vf0
    sll r0, r0, 0
    vadd.xyzw vf2, vf2, vf3
    sll r0, r0, 0
    vmulx.xyzw vf5, vf5, vf28
    sll r0, r0, 0
    vmaddax.xyzw acc, vf24, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf25, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf7, vf26, vf1
    sll r0, r0, 0
    vmulx.xyzw vf2, vf2, vf28
    sll r0, r0, 0
    vsub.xyzw vf6, vf4, vf5
    sll r0, r0, 0
    vaddz.xyzw vf7, vf13, vf7
    sll r0, r0, 0
    vsub.xyz vf3, vf1, vf2
    sll r0, r0, 0
    vmulw.w vf3, vf0, vf0
    sll r0, r0, 0
    vmul.xyzw vf7, vf7, vf14
    sll r0, r0, 0
    vmini.xyzw vf7, vf7, vf11
    sll r0, r0, 0
    vmaxx.xyzw vf7, vf7, vf0
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf5
    sll r0, r0, 0
    vmaddy.xyzw vf4, vf6, vf7
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf2
    sll r0, r0, 0
    vmaddy.yw vf1, vf3, vf7
    sll r0, r0, 0
    vftoi0.xyzw vf4, vf4
    sll r0, r0, 0
    sll r0, r0, 0
    sqc2 vf4, 160(t4)
    sll r0, r0, 0
    sqc2 vf1, 176(t4)
    sll r0, r0, 0
    lqc2 vf1, 176(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 188(t4)
    sll r0, r0, 0

 # Vertex 6
    lhu t6, 50(t3)
    sll r0, r0, 0
    lhu t7, 48(t3)
    daddu t6, t6, t1
    sll r0, r0, 0
    pextlb t8, r0, t7
    lhu t7, 2(t6)
    pextlh t8, r0, t8
    lhu t6, 0(t6)
    dsll t8, t8, 12
    andi t7, t7, 2047
    pexcw t8, t8
    mfc1 r0, f31
    dsll t7, t7, 2
    qmtc2.i vf1, t8
    daddu t7, t7, a3
    sll t6, t6, 3
    vitof0.xyzw vf1, vf1
    mtc1 f0, t6
    sll r0, r0, 0
    lw t6, 12288(t7)
    cvt.s.w f0, f0
    sll r0, r0, 0
    vadd.xyzw vf1, vf1, vf15
    sll r0, r0, 0
    pextlb t6, r0, t6
    mfc1 r0, f31
    pextlh t6, r0, t6
    mfc1 r0, f31
    sll r0, r0, 0
    sq t6, 192(t4)
    sll r0, r0, 0
    sqc2 vf1, 208(t4)
    sll r0, r0, 0
    swc1 f0, 212(t4)
    sll r0, r0, 0
    lqc2 vf1, 208(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 220(t4)
    sll r0, r0, 0

 # vertex 7
    lhu t6, 58(t3)
    sll r0, r0, 0
    lhu t8, 60(t3)
    daddu t7, t6, t1
    lhu t6, 62(t3)
    daddu t8, t8, t1
    lhu t9, 56(t3)
    daddu t6, t6, t1
    lhu ra, 2(t7)
    pextlb t9, r0, t9
    lhu t7, 0(t7)
    pextlh t9, r0, t9
    lhu gp, 2(t8)
    dsll t9, t9, 12
    lhu t8, 0(t8)
    pexcw t9, t9
    lhu s5, 2(t6)
    andi ra, ra, 2047
    andi gp, gp, 2047
    andi s5, s5, 2047
    dsll s4, ra, 2
    dsll gp, gp, 2
    dsll ra, s5, 2
    daddu s4, s4, a3
    lhu t6, 0(t6)
    sll s5, t7, 3
    lw t7, 12288(s4)
    daddu gp, gp, a3
    sq t9, 8080(a0)
    sll t8, t8, 3
    lw t9, 12288(gp)
    daddu ra, ra, a3
    sw s5, 8084(a0)
    sll t6, t6, 3
    lw ra, 12288(ra)
    pextlb t7, r0, t7
    lqc2 vf1, 8080(a0)
    pextlh t7, r0, t7
    sw t8, 8084(a0)
    pextlb t8, r0, t9
    qmtc2.i vf4, t7
    pextlh t7, r0, t8
    lqc2 vf2, 8080(a0)
    pextlb t8, r0, ra
    qmtc2.i vf5, t7
    pextlh t7, r0, t8
    sw t6, 8084(a0)
    vitof0.xyzw vf4, vf4
    qmtc2.i vf6, t7
    vitof0.xyzw vf5, vf5
    lqc2 vf3, 8080(a0)
    vitof0.xyzw vf6, vf6
    sll r0, r0, 0
    vitof0.xyzw vf1, vf1
    sll r0, r0, 0
    vitof0.xyzw vf2, vf2
    sll r0, r0, 0
    vitof0.xyzw vf3, vf3
    sll r0, r0, 0
    vadd.xyzw vf5, vf5, vf6
    sll r0, r0, 0
    vadd.xz vf1, vf1, vf15
    sll r0, r0, 0
    vmulaw.xyzw acc, vf27, vf0
    sll r0, r0, 0
    vadd.xyzw vf2, vf2, vf3
    sll r0, r0, 0
    vmulx.xyzw vf5, vf5, vf28
    sll r0, r0, 0
    vmaddax.xyzw acc, vf24, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf25, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf7, vf26, vf1
    sll r0, r0, 0
    vmulx.xyzw vf2, vf2, vf28
    sll r0, r0, 0
    vsub.xyzw vf6, vf4, vf5
    sll r0, r0, 0
    vaddz.xyzw vf7, vf13, vf7
    sll r0, r0, 0
    vsub.xyz vf3, vf1, vf2
    sll r0, r0, 0
    vmulw.w vf3, vf0, vf0
    sll r0, r0, 0
    vmul.xyzw vf7, vf7, vf14
    sll r0, r0, 0
    vmini.xyzw vf7, vf7, vf11
    sll r0, r0, 0
    vmaxx.xyzw vf7, vf7, vf0
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf5
    sll r0, r0, 0
    vmaddy.xyzw vf4, vf6, vf7
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf2
    sll r0, r0, 0
    vmaddy.yw vf1, vf3, vf7
    sll r0, r0, 0
    vftoi0.xyzw vf4, vf4
    sll r0, r0, 0
    sll r0, r0, 0
    sqc2 vf4, 224(t4)
    sll r0, r0, 0
    sqc2 vf1, 240(t4)
    sll r0, r0, 0
    lqc2 vf1, 240(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 252(t4)
    sll r0, r0, 0

 # Vertex 8
    lhu t6, 66(t3)
    sll r0, r0, 0
    lhu t7, 64(t3)
    daddu t6, t6, t1
    sll r0, r0, 0
    pextlb t8, r0, t7
    lhu t7, 2(t6)
    pextlh t8, r0, t8
    lhu t6, 0(t6)
    dsll t8, t8, 12
    andi t7, t7, 2047
    pexcw t8, t8
    mfc1 r0, f31
    dsll t7, t7, 2
    qmtc2.i vf1, t8
    daddu t7, t7, a3
    sll t6, t6, 3
    vitof0.xyzw vf1, vf1
    mtc1 f0, t6
    sll r0, r0, 0
    lw t6, 12288(t7)
    cvt.s.w f0, f0
    sll r0, r0, 0
    vadd.xyzw vf1, vf1, vf15
    sll r0, r0, 0
    pextlb t6, r0, t6
    mfc1 r0, f31
    pextlh t6, r0, t6
    mfc1 r0, f31
    sll r0, r0, 0
    sq t6, 256(t4)
    sll r0, r0, 0
    sqc2 vf1, 272(t4)
    sll r0, r0, 0
    swc1 f0, 276(t4)
    sll r0, r0, 0
    lqc2 vf1, 272(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 284(t4)

    bgezal r0, L39
    sll r0, r0, 0

B19:
    daddiu t3, t3, 72  ;; onto the next poly9
    daddiu t4, t4, 288 ;; onto next cache line
B20:
L27:
    daddiu t6, a1, -171
    sll r0, r0, 0
    blez t6, L30   ;; maybe dma sync
    sll r0, r0, 0

B21:
    lw t6, 8196(a0)
    sll r0, r0, 0
B22:
L28:
    lw t0, 0(t6)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t0, t0, 256
    sll r0, r0, 0
    beq t0, r0, L29
    sll r0, r0, 0

B23:
    sll r0, r0, 0
    lw t0, 8160(a0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu t0, t0, 1
    sll r0, r0, 0
    sw t0, 8160(a0)
    beq r0, r0, L28
    sll r0, r0, 0

B24:
L29:
    lw t0, 8152(a0)
    sll r0, r0, 0
    sw v1, 128(t6)
    xori v1, v1, 4720
    sw t0, 16(t6)
    sll t7, a1, 4
    addu t7, t0, t7
    or t0, v1, r0
    sw a1, 32(t6)
    addiu a1, r0, 256
    sw a1, 0(t6)
    addiu a1, r0, 0
    sw t7, 8152(a0)
    sll r0, r0, 0
B25:
L30:
;; copying shared vertices in cache
    sll r0, r0, 0
    lq t6, -96(t4)
    sll r0, r0, 0
    lqc2 vf2, -80(t4)
    sll r0, r0, 0
    sq t6, 0(t4)
    sll r0, r0, 0
    sqc2 vf2, 16(t4)
    sll r0, r0, 0
    lq t6, -64(t4)
    sll r0, r0, 0
    lqc2 vf2, -48(t4)
    sll r0, r0, 0
    sq t6, 32(t4)
    sll r0, r0, 0
    sqc2 vf2, 48(t4)
    sll r0, r0, 0
    lq t6, -32(t4)
    sll r0, r0, 0
    lqc2 vf2, -16(t4)
    sll r0, r0, 0
    sq t6, 64(t4)
    sll r0, r0, 0
    sqc2 vf2, 80(t4)
    sll r0, r0, 0

 ;; another vertex processing
 ;; vertex 0
    lhu t6, 26(t3)
    sll r0, r0, 0
    lhu t8, 28(t3)
    daddu t7, t6, t1
    lhu t6, 30(t3)
    daddu t8, t8, t1
    lhu t9, 24(t3)
    daddu t6, t6, t1
    lhu ra, 2(t7)
    pextlb t9, r0, t9
    lhu t7, 0(t7)
    pextlh t9, r0, t9
    lhu gp, 2(t8)
    dsll t9, t9, 12
    lhu t8, 0(t8)
    pexcw t9, t9
    lhu s5, 2(t6)
    andi ra, ra, 2047
    andi gp, gp, 2047
    andi s5, s5, 2047
    dsll s4, ra, 2
    dsll gp, gp, 2
    dsll ra, s5, 2
    daddu s4, s4, a3
    lhu t6, 0(t6)
    sll s5, t7, 3
    lw t7, 12288(s4)
    daddu gp, gp, a3
    sq t9, 8080(a0)
    sll t8, t8, 3
    lw t9, 12288(gp)
    daddu ra, ra, a3
    sw s5, 8084(a0)
    sll t6, t6, 3
    lw ra, 12288(ra)
    pextlb t7, r0, t7
    lqc2 vf1, 8080(a0)
    pextlh t7, r0, t7
    sw t8, 8084(a0)
    pextlb t8, r0, t9
    qmtc2.i vf4, t7
    pextlh t7, r0, t8
    lqc2 vf2, 8080(a0)
    pextlb t8, r0, ra
    qmtc2.i vf5, t7
    pextlh t7, r0, t8
    sw t6, 8084(a0)
    vitof0.xyzw vf4, vf4
    qmtc2.i vf6, t7
    vitof0.xyzw vf5, vf5
    lqc2 vf3, 8080(a0)
    vitof0.xyzw vf6, vf6
    sll r0, r0, 0
    vitof0.xyzw vf1, vf1
    sll r0, r0, 0
    vitof0.xyzw vf2, vf2
    sll r0, r0, 0
    vitof0.xyzw vf3, vf3
    sll r0, r0, 0
    vadd.xyzw vf5, vf5, vf6
    sll r0, r0, 0
    vadd.xz vf1, vf1, vf15
    sll r0, r0, 0
    vmulaw.xyzw acc, vf27, vf0
    sll r0, r0, 0
    vadd.xyzw vf2, vf2, vf3
    sll r0, r0, 0
    vmulx.xyzw vf5, vf5, vf28
    sll r0, r0, 0
    vmaddax.xyzw acc, vf24, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf25, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf7, vf26, vf1
    sll r0, r0, 0
    vmulx.xyzw vf2, vf2, vf28
    sll r0, r0, 0
    vsub.xyzw vf6, vf4, vf5
    sll r0, r0, 0
    vaddz.xyzw vf7, vf13, vf7
    sll r0, r0, 0
    vsub.xyz vf3, vf1, vf2
    sll r0, r0, 0
    vmulw.w vf3, vf0, vf0
    sll r0, r0, 0
    vmul.xyzw vf7, vf7, vf14
    sll r0, r0, 0
    vmini.xyzw vf7, vf7, vf11
    sll r0, r0, 0
    vmaxx.xyzw vf7, vf7, vf0
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf5
    sll r0, r0, 0
    vmaddy.xyzw vf4, vf6, vf7
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf2
    sll r0, r0, 0
    vmaddy.yw vf1, vf3, vf7
    sll r0, r0, 0
    vftoi0.xyzw vf4, vf4
    sll r0, r0, 0
    sll r0, r0, 0
    sqc2 vf4, 96(t4)
    sll r0, r0, 0
    sqc2 vf1, 112(t4)
    sll r0, r0, 0
    lqc2 vf1, 112(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 124(t4)
    sll r0, r0, 0

 # Vertex 1
    lhu t6, 34(t3)
    sll r0, r0, 0
    lhu t8, 36(t3)
    daddu t7, t6, t1
    lhu t6, 38(t3)
    daddu t8, t8, t1
    lhu t9, 32(t3)
    daddu t6, t6, t1
    lhu ra, 2(t7)
    pextlb t9, r0, t9
    lhu t7, 0(t7)
    pextlh t9, r0, t9
    lhu gp, 2(t8)
    dsll t9, t9, 12
    lhu t8, 0(t8)
    pexcw t9, t9
    lhu s5, 2(t6)
    andi ra, ra, 2047
    andi gp, gp, 2047
    andi s5, s5, 2047
    dsll s4, ra, 2
    dsll gp, gp, 2
    dsll ra, s5, 2
    daddu s4, s4, a3
    lhu t6, 0(t6)
    sll s5, t7, 3
    lw t7, 12288(s4)
    daddu gp, gp, a3
    sq t9, 8080(a0)
    sll t8, t8, 3
    lw t9, 12288(gp)
    daddu ra, ra, a3
    sw s5, 8084(a0)
    sll t6, t6, 3
    lw ra, 12288(ra)
    pextlb t7, r0, t7
    lqc2 vf1, 8080(a0)
    pextlh t7, r0, t7
    sw t8, 8084(a0)
    pextlb t8, r0, t9
    qmtc2.i vf4, t7
    pextlh t7, r0, t8
    lqc2 vf2, 8080(a0)
    pextlb t8, r0, ra
    qmtc2.i vf5, t7
    pextlh t7, r0, t8
    sw t6, 8084(a0)
    vitof0.xyzw vf4, vf4
    qmtc2.i vf6, t7
    vitof0.xyzw vf5, vf5
    lqc2 vf3, 8080(a0)
    vitof0.xyzw vf6, vf6
    sll r0, r0, 0
    vitof0.xyzw vf1, vf1
    sll r0, r0, 0
    vitof0.xyzw vf2, vf2
    sll r0, r0, 0
    vitof0.xyzw vf3, vf3
    sll r0, r0, 0
    vadd.xyzw vf5, vf5, vf6
    sll r0, r0, 0
    vadd.xz vf1, vf1, vf15
    sll r0, r0, 0
    vmulaw.xyzw acc, vf27, vf0
    sll r0, r0, 0
    vadd.xyzw vf2, vf2, vf3
    sll r0, r0, 0
    vmulx.xyzw vf5, vf5, vf28
    sll r0, r0, 0
    vmaddax.xyzw acc, vf24, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf25, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf7, vf26, vf1
    sll r0, r0, 0
    vmulx.xyzw vf2, vf2, vf28
    sll r0, r0, 0
    vsub.xyzw vf6, vf4, vf5
    sll r0, r0, 0
    vaddz.xyzw vf7, vf13, vf7
    sll r0, r0, 0
    vsub.xyz vf3, vf1, vf2
    sll r0, r0, 0
    vmulw.w vf3, vf0, vf0
    sll r0, r0, 0
    vmul.xyzw vf7, vf7, vf14
    sll r0, r0, 0
    vmini.xyzw vf7, vf7, vf11
    sll r0, r0, 0
    vmaxx.xyzw vf7, vf7, vf0
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf5
    sll r0, r0, 0
    vmaddy.xyzw vf4, vf6, vf7
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf2
    sll r0, r0, 0
    vmaddy.yw vf1, vf3, vf7
    sll r0, r0, 0
    vftoi0.xyzw vf4, vf4
    sll r0, r0, 0
    sll r0, r0, 0
    sqc2 vf4, 128(t4)
    sll r0, r0, 0
    sqc2 vf1, 144(t4)
    sll r0, r0, 0
    lqc2 vf1, 144(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 156(t4)
    sll r0, r0, 0

;; vertex 2
    lhu t6, 42(t3)
    sll r0, r0, 0
    lhu t8, 44(t3)
    daddu t7, t6, t1
    lhu t6, 46(t3)
    daddu t8, t8, t1
    lhu t9, 40(t3)
    daddu t6, t6, t1
    lhu ra, 2(t7)
    pextlb t9, r0, t9
    lhu t7, 0(t7)
    pextlh t9, r0, t9
    lhu gp, 2(t8)
    dsll t9, t9, 12
    lhu t8, 0(t8)
    pexcw t9, t9
    lhu s5, 2(t6)
    andi ra, ra, 2047
    andi gp, gp, 2047
    andi s5, s5, 2047
    dsll s4, ra, 2
    dsll gp, gp, 2
    dsll ra, s5, 2
    daddu s4, s4, a3
    lhu t6, 0(t6)
    sll s5, t7, 3
    lw t7, 12288(s4)
    daddu gp, gp, a3
    sq t9, 8080(a0)
    sll t8, t8, 3
    lw t9, 12288(gp)
    daddu ra, ra, a3
    sw s5, 8084(a0)
    sll t6, t6, 3
    lw ra, 12288(ra)
    pextlb t7, r0, t7
    lqc2 vf1, 8080(a0)
    pextlh t7, r0, t7
    sw t8, 8084(a0)
    pextlb t8, r0, t9
    qmtc2.i vf4, t7
    pextlh t7, r0, t8
    lqc2 vf2, 8080(a0)
    pextlb t8, r0, ra
    qmtc2.i vf5, t7
    pextlh t7, r0, t8
    sw t6, 8084(a0)
    vitof0.xyzw vf4, vf4
    qmtc2.i vf6, t7
    vitof0.xyzw vf5, vf5
    lqc2 vf3, 8080(a0)
    vitof0.xyzw vf6, vf6
    sll r0, r0, 0
    vitof0.xyzw vf1, vf1
    sll r0, r0, 0
    vitof0.xyzw vf2, vf2
    sll r0, r0, 0
    vitof0.xyzw vf3, vf3
    sll r0, r0, 0
    vadd.xyzw vf5, vf5, vf6
    sll r0, r0, 0
    vadd.xz vf1, vf1, vf15
    sll r0, r0, 0
    vmulaw.xyzw acc, vf27, vf0
    sll r0, r0, 0
    vadd.xyzw vf2, vf2, vf3
    sll r0, r0, 0
    vmulx.xyzw vf5, vf5, vf28
    sll r0, r0, 0
    vmaddax.xyzw acc, vf24, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf25, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf7, vf26, vf1
    sll r0, r0, 0
    vmulx.xyzw vf2, vf2, vf28
    sll r0, r0, 0
    vsub.xyzw vf6, vf4, vf5
    sll r0, r0, 0
    vaddz.xyzw vf7, vf13, vf7
    sll r0, r0, 0
    vsub.xyz vf3, vf1, vf2
    sll r0, r0, 0
    vmulw.w vf3, vf0, vf0
    sll r0, r0, 0
    vmul.xyzw vf7, vf7, vf14
    sll r0, r0, 0
    vmini.xyzw vf7, vf7, vf11
    sll r0, r0, 0
    vmaxx.xyzw vf7, vf7, vf0
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf5
    sll r0, r0, 0
    vmaddy.xyzw vf4, vf6, vf7
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf2
    sll r0, r0, 0
    vmaddy.yw vf1, vf3, vf7
    sll r0, r0, 0
    vftoi0.xyzw vf4, vf4
    sll r0, r0, 0
    sll r0, r0, 0
    sqc2 vf4, 160(t4)
    sll r0, r0, 0
    sqc2 vf1, 176(t4)
    sll r0, r0, 0
    lqc2 vf1, 176(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 188(t4)
    sll r0, r0, 0

 ;; vertex 3
    lhu t6, 50(t3)
    sll r0, r0, 0
    lhu t7, 48(t3)
    daddu t6, t6, t1
    sll r0, r0, 0
    pextlb t8, r0, t7
    lhu t7, 2(t6)
    pextlh t8, r0, t8
    lhu t6, 0(t6)
    dsll t8, t8, 12
    andi t7, t7, 2047
    pexcw t8, t8
    mfc1 r0, f31
    dsll t7, t7, 2
    qmtc2.i vf1, t8
    daddu t7, t7, a3
    sll t6, t6, 3
    vitof0.xyzw vf1, vf1
    mtc1 f0, t6
    sll r0, r0, 0
    lw t6, 12288(t7)
    cvt.s.w f0, f0
    sll r0, r0, 0
    vadd.xyzw vf1, vf1, vf15
    sll r0, r0, 0
    pextlb t6, r0, t6
    mfc1 r0, f31
    pextlh t6, r0, t6
    mfc1 r0, f31
    sll r0, r0, 0
    sq t6, 192(t4)
    sll r0, r0, 0
    sqc2 vf1, 208(t4)
    sll r0, r0, 0
    swc1 f0, 212(t4)
    sll r0, r0, 0
    lqc2 vf1, 208(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 220(t4)
    sll r0, r0, 0

 ;; vertex 4
    lhu t6, 58(t3)
    sll r0, r0, 0
    lhu t8, 60(t3)
    daddu t7, t6, t1
    lhu t6, 62(t3)
    daddu t8, t8, t1
    lhu t9, 56(t3)
    daddu t6, t6, t1
    lhu ra, 2(t7)
    pextlb t9, r0, t9
    lhu t7, 0(t7)
    pextlh t9, r0, t9
    lhu gp, 2(t8)
    dsll t9, t9, 12
    lhu t8, 0(t8)
    pexcw t9, t9
    lhu s5, 2(t6)
    andi ra, ra, 2047
    andi gp, gp, 2047
    andi s5, s5, 2047
    dsll s4, ra, 2
    dsll gp, gp, 2
    dsll ra, s5, 2
    daddu s4, s4, a3
    lhu t6, 0(t6)
    sll s5, t7, 3
    lw t7, 12288(s4)
    daddu gp, gp, a3
    sq t9, 8080(a0)
    sll t8, t8, 3
    lw t9, 12288(gp)
    daddu ra, ra, a3
    sw s5, 8084(a0)
    sll t6, t6, 3
    lw ra, 12288(ra)
    pextlb t7, r0, t7
    lqc2 vf1, 8080(a0)
    pextlh t7, r0, t7
    sw t8, 8084(a0)
    pextlb t8, r0, t9
    qmtc2.i vf4, t7
    pextlh t7, r0, t8
    lqc2 vf2, 8080(a0)
    pextlb t8, r0, ra
    qmtc2.i vf5, t7
    pextlh t7, r0, t8
    sw t6, 8084(a0)
    vitof0.xyzw vf4, vf4
    qmtc2.i vf6, t7
    vitof0.xyzw vf5, vf5
    lqc2 vf3, 8080(a0)
    vitof0.xyzw vf6, vf6
    sll r0, r0, 0
    vitof0.xyzw vf1, vf1
    sll r0, r0, 0
    vitof0.xyzw vf2, vf2
    sll r0, r0, 0
    vitof0.xyzw vf3, vf3
    sll r0, r0, 0
    vadd.xyzw vf5, vf5, vf6
    sll r0, r0, 0
    vadd.xz vf1, vf1, vf15
    sll r0, r0, 0
    vmulaw.xyzw acc, vf27, vf0
    sll r0, r0, 0
    vadd.xyzw vf2, vf2, vf3
    sll r0, r0, 0
    vmulx.xyzw vf5, vf5, vf28
    sll r0, r0, 0
    vmaddax.xyzw acc, vf24, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf25, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf7, vf26, vf1
    sll r0, r0, 0
    vmulx.xyzw vf2, vf2, vf28
    sll r0, r0, 0
    vsub.xyzw vf6, vf4, vf5
    sll r0, r0, 0
    vaddz.xyzw vf7, vf13, vf7
    sll r0, r0, 0
    vsub.xyz vf3, vf1, vf2
    sll r0, r0, 0
    vmulw.w vf3, vf0, vf0
    sll r0, r0, 0
    vmul.xyzw vf7, vf7, vf14
    sll r0, r0, 0
    vmini.xyzw vf7, vf7, vf11
    sll r0, r0, 0
    vmaxx.xyzw vf7, vf7, vf0
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf5
    sll r0, r0, 0
    vmaddy.xyzw vf4, vf6, vf7
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf2
    sll r0, r0, 0
    vmaddy.yw vf1, vf3, vf7
    sll r0, r0, 0
    vftoi0.xyzw vf4, vf4
    sll r0, r0, 0
    sll r0, r0, 0
    sqc2 vf4, 224(t4)
    sll r0, r0, 0
    sqc2 vf1, 240(t4)
    sll r0, r0, 0
    lqc2 vf1, 240(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 252(t4)
    sll r0, r0, 0

 ;; vertex 5
    lhu t6, 66(t3)
    sll r0, r0, 0
    lhu t7, 64(t3)
    daddu t6, t6, t1
    sll r0, r0, 0
    pextlb t8, r0, t7
    lhu t7, 2(t6)
    pextlh t8, r0, t8
    lhu t6, 0(t6)
    dsll t8, t8, 12
    andi t7, t7, 2047
    pexcw t8, t8
    mfc1 r0, f31
    dsll t7, t7, 2
    qmtc2.i vf1, t8
    daddu t7, t7, a3
    sll t6, t6, 3
    vitof0.xyzw vf1, vf1
    mtc1 f0, t6
    sll r0, r0, 0
    lw t6, 12288(t7)
    cvt.s.w f0, f0
    sll r0, r0, 0
    vadd.xyzw vf1, vf1, vf15
    sll r0, r0, 0
    pextlb t6, r0, t6
    mfc1 r0, f31
    pextlh t6, r0, t6
    mfc1 r0, f31
    sll r0, r0, 0
    sq t6, 256(t4)
    sll r0, r0, 0
    sqc2 vf1, 272(t4)
    sll r0, r0, 0
    swc1 f0, 276(t4)
    sll r0, r0, 0
    lqc2 vf1, 272(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 284(t4)
    bgezal r0, L39
    sll r0, r0, 0

B26:
    daddiu t3, t3, 72
    daddiu t4, t4, 288
    bgtz t5, L27 ;; loop over 6 'inner' poly9's
    daddiu t5, t5, -1

B27:
L31:
    daddiu t4, a1, -171
    sll r0, r0, 0
    blez t4, L34 ;; optional dma sync
    sll r0, r0, 0

B28:
    lw t4, 8196(a0)
    sll r0, r0, 0
B29:
L32:
    lw t0, 0(t4)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t0, t0, 256
    sll r0, r0, 0
    beq t0, r0, L33
    sll r0, r0, 0

B30:
    sll r0, r0, 0
    lw t0, 8160(a0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu t0, t0, 1
    sll r0, r0, 0
    sw t0, 8160(a0)
    beq r0, r0, L32
    sll r0, r0, 0

B31:
L33:
    lw t0, 8152(a0)
    sll r0, r0, 0
    sw v1, 128(t4)
    xori v1, v1, 4720
    sw t0, 16(t4)
    sll t5, a1, 4
    addu t5, t0, t5
    or t0, v1, r0
    sw a1, 32(t4)
    addiu a1, r0, 256
    sw a1, 0(t4)
    addiu a1, r0, 0
    sw t5, 8152(a0)
    sll r0, r0, 0
B32:
L34: ;; last poly9 in the row??
    daddiu t4, a3, 9984 ;; reset cache to the beginning
    addiu t5, r0, 6
    sll r0, r0, 0
    lq t6, 64(t4) ;; swap 2 -> 0
    sll r0, r0, 0
    lqc2 vf2, 80(t4)
    sll r0, r0, 0
    sq t6, 0(t4)
    sll r0, r0, 0
    sqc2 vf2, 16(t4)
    sll r0, r0, 0

;; vertex 1
    lhu t6, 10(t3)
    sll r0, r0, 0
    lhu t8, 12(t3)
    daddu t7, t6, t1
    lhu t6, 14(t3)
    daddu t8, t8, t1
    lhu t9, 8(t3)
    daddu t6, t6, t1
    lhu ra, 2(t7)
    pextlb t9, r0, t9
    lhu t7, 0(t7)
    pextlh t9, r0, t9
    lhu gp, 2(t8)
    dsll t9, t9, 12
    lhu t8, 0(t8)
    pexcw t9, t9
    lhu s5, 2(t6)
    andi ra, ra, 2047
    andi gp, gp, 2047
    andi s5, s5, 2047
    dsll s4, ra, 2
    dsll gp, gp, 2
    dsll ra, s5, 2
    daddu s4, s4, a3
    lhu t6, 0(t6)
    sll s5, t7, 3
    lw t7, 12288(s4)
    daddu gp, gp, a3
    sq t9, 8080(a0)
    sll t8, t8, 3
    lw t9, 12288(gp)
    daddu ra, ra, a3
    sw s5, 8084(a0)
    sll t6, t6, 3
    lw ra, 12288(ra)
    pextlb t7, r0, t7
    lqc2 vf1, 8080(a0)
    pextlh t7, r0, t7
    sw t8, 8084(a0)
    pextlb t8, r0, t9
    qmtc2.i vf4, t7
    pextlh t7, r0, t8
    lqc2 vf2, 8080(a0)
    pextlb t8, r0, ra
    qmtc2.i vf5, t7
    pextlh t7, r0, t8
    sw t6, 8084(a0)
    vitof0.xyzw vf4, vf4
    qmtc2.i vf6, t7
    vitof0.xyzw vf5, vf5
    lqc2 vf3, 8080(a0)
    vitof0.xyzw vf6, vf6
    sll r0, r0, 0
    vitof0.xyzw vf1, vf1
    sll r0, r0, 0
    vitof0.xyzw vf2, vf2
    sll r0, r0, 0
    vitof0.xyzw vf3, vf3
    sll r0, r0, 0
    vadd.xyzw vf5, vf5, vf6
    sll r0, r0, 0
    vadd.xz vf1, vf1, vf15
    sll r0, r0, 0
    vmulaw.xyzw acc, vf27, vf0
    sll r0, r0, 0
    vadd.xyzw vf2, vf2, vf3
    sll r0, r0, 0
    vmulx.xyzw vf5, vf5, vf28
    sll r0, r0, 0
    vmaddax.xyzw acc, vf24, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf25, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf7, vf26, vf1
    sll r0, r0, 0
    vmulx.xyzw vf2, vf2, vf28
    sll r0, r0, 0
    vsub.xyzw vf6, vf4, vf5
    sll r0, r0, 0
    vaddz.xyzw vf7, vf13, vf7
    sll r0, r0, 0
    vsub.xyz vf3, vf1, vf2
    sll r0, r0, 0
    vmulw.w vf3, vf0, vf0
    sll r0, r0, 0
    vmul.xyzw vf7, vf7, vf14
    sll r0, r0, 0
    vmini.xyzw vf7, vf7, vf11
    sll r0, r0, 0
    vmaxx.xyzw vf7, vf7, vf0
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf5
    sll r0, r0, 0
    vmaddy.xyzw vf4, vf6, vf7
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf2
    sll r0, r0, 0
    vmaddy.yw vf1, vf3, vf7
    sll r0, r0, 0
    vftoi0.xyzw vf4, vf4
    sll r0, r0, 0
    sll r0, r0, 0
    sqc2 vf4, 32(t4)
    sll r0, r0, 0
    sqc2 vf1, 48(t4)
    sll r0, r0, 0
    lqc2 vf1, 48(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 60(t4)
    sll r0, r0, 0

;; vertex 2
    lhu t6, 18(t3)
    sll r0, r0, 0
    lhu t7, 16(t3)
    daddu t6, t6, t1
    sll r0, r0, 0
    pextlb t8, r0, t7
    lhu t7, 2(t6)
    pextlh t8, r0, t8
    lhu t6, 0(t6)
    dsll t8, t8, 12
    andi t7, t7, 2047
    pexcw t8, t8
    mfc1 r0, f31
    dsll t7, t7, 2
    qmtc2.i vf1, t8
    daddu t7, t7, a3
    sll t6, t6, 3
    vitof0.xyzw vf1, vf1
    mtc1 f0, t6
    sll r0, r0, 0
    lw t6, 12288(t7)
    cvt.s.w f0, f0
    sll r0, r0, 0
    vadd.xyzw vf1, vf1, vf15
    sll r0, r0, 0
    pextlb t6, r0, t6
    mfc1 r0, f31
    pextlh t6, r0, t6
    mfc1 r0, f31
    sll r0, r0, 0
    sq t6, 64(t4)
    sll r0, r0, 0
    sqc2 vf1, 80(t4)
    sll r0, r0, 0
    swc1 f0, 84(t4)
    sll r0, r0, 0
    lqc2 vf1, 80(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 92(t4)

;; copy cached vertex 5 -> 3
    sll r0, r0, 0
    lq t6, 160(t4)
    sll r0, r0, 0
    lqc2 vf2, 176(t4)
    sll r0, r0, 0
    sq t6, 96(t4)
    sll r0, r0, 0
    sqc2 vf2, 112(t4)
    sll r0, r0, 0

;; vertex 4
    lhu t6, 34(t3)
    sll r0, r0, 0
    lhu t8, 36(t3)
    daddu t7, t6, t1
    lhu t6, 38(t3)
    daddu t8, t8, t1
    lhu t9, 32(t3)
    daddu t6, t6, t1
    lhu ra, 2(t7)
    pextlb t9, r0, t9
    lhu t7, 0(t7)
    pextlh t9, r0, t9
    lhu gp, 2(t8)
    dsll t9, t9, 12
    lhu t8, 0(t8)
    pexcw t9, t9
    lhu s5, 2(t6)
    andi ra, ra, 2047
    andi gp, gp, 2047
    andi s5, s5, 2047
    dsll s4, ra, 2
    dsll gp, gp, 2
    dsll ra, s5, 2
    daddu s4, s4, a3
    lhu t6, 0(t6)
    sll s5, t7, 3
    lw t7, 12288(s4)
    daddu gp, gp, a3
    sq t9, 8080(a0)
    sll t8, t8, 3
    lw t9, 12288(gp)
    daddu ra, ra, a3
    sw s5, 8084(a0)
    sll t6, t6, 3
    lw ra, 12288(ra)
    pextlb t7, r0, t7
    lqc2 vf1, 8080(a0)
    pextlh t7, r0, t7
    sw t8, 8084(a0)
    pextlb t8, r0, t9
    qmtc2.i vf4, t7
    pextlh t7, r0, t8
    lqc2 vf2, 8080(a0)
    pextlb t8, r0, ra
    qmtc2.i vf5, t7
    pextlh t7, r0, t8
    sw t6, 8084(a0)
    vitof0.xyzw vf4, vf4
    qmtc2.i vf6, t7
    vitof0.xyzw vf5, vf5
    lqc2 vf3, 8080(a0)
    vitof0.xyzw vf6, vf6
    sll r0, r0, 0
    vitof0.xyzw vf1, vf1
    sll r0, r0, 0
    vitof0.xyzw vf2, vf2
    sll r0, r0, 0
    vitof0.xyzw vf3, vf3
    sll r0, r0, 0
    vadd.xyzw vf5, vf5, vf6
    sll r0, r0, 0
    vadd.xz vf1, vf1, vf15
    sll r0, r0, 0
    vmulaw.xyzw acc, vf27, vf0
    sll r0, r0, 0
    vadd.xyzw vf2, vf2, vf3
    sll r0, r0, 0
    vmulx.xyzw vf5, vf5, vf28
    sll r0, r0, 0
    vmaddax.xyzw acc, vf24, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf25, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf7, vf26, vf1
    sll r0, r0, 0
    vmulx.xyzw vf2, vf2, vf28
    sll r0, r0, 0
    vsub.xyzw vf6, vf4, vf5
    sll r0, r0, 0
    vaddz.xyzw vf7, vf13, vf7
    sll r0, r0, 0
    vsub.xyz vf3, vf1, vf2
    sll r0, r0, 0
    vmulw.w vf3, vf0, vf0
    sll r0, r0, 0
    vmul.xyzw vf7, vf7, vf14
    sll r0, r0, 0
    vmini.xyzw vf7, vf7, vf11
    sll r0, r0, 0
    vmaxx.xyzw vf7, vf7, vf0
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf5
    sll r0, r0, 0
    vmaddy.xyzw vf4, vf6, vf7
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf2
    sll r0, r0, 0
    vmaddy.yw vf1, vf3, vf7
    sll r0, r0, 0
    vftoi0.xyzw vf4, vf4
    sll r0, r0, 0
    sll r0, r0, 0
    sqc2 vf4, 128(t4)
    sll r0, r0, 0
    sqc2 vf1, 144(t4)
    sll r0, r0, 0
    lqc2 vf1, 144(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 156(t4)
    sll r0, r0, 0

 ;; vertex 5
    lhu t6, 42(t3)
    sll r0, r0, 0
    lhu t8, 44(t3)
    daddu t7, t6, t1
    lhu t6, 46(t3)
    daddu t8, t8, t1
    lhu t9, 40(t3)
    daddu t6, t6, t1
    lhu ra, 2(t7)
    pextlb t9, r0, t9
    lhu t7, 0(t7)
    pextlh t9, r0, t9
    lhu gp, 2(t8)
    dsll t9, t9, 12
    lhu t8, 0(t8)
    pexcw t9, t9
    lhu s5, 2(t6)
    andi ra, ra, 2047
    andi gp, gp, 2047
    andi s5, s5, 2047
    dsll s4, ra, 2
    dsll gp, gp, 2
    dsll ra, s5, 2
    daddu s4, s4, a3
    lhu t6, 0(t6)
    sll s5, t7, 3
    lw t7, 12288(s4)
    daddu gp, gp, a3
    sq t9, 8080(a0)
    sll t8, t8, 3
    lw t9, 12288(gp)
    daddu ra, ra, a3
    sw s5, 8084(a0)
    sll t6, t6, 3
    lw ra, 12288(ra)
    pextlb t7, r0, t7
    lqc2 vf1, 8080(a0)
    pextlh t7, r0, t7
    sw t8, 8084(a0)
    pextlb t8, r0, t9
    qmtc2.i vf4, t7
    pextlh t7, r0, t8
    lqc2 vf2, 8080(a0)
    pextlb t8, r0, ra
    qmtc2.i vf5, t7
    pextlh t7, r0, t8
    sw t6, 8084(a0)
    vitof0.xyzw vf4, vf4
    qmtc2.i vf6, t7
    vitof0.xyzw vf5, vf5
    lqc2 vf3, 8080(a0)
    vitof0.xyzw vf6, vf6
    sll r0, r0, 0
    vitof0.xyzw vf1, vf1
    sll r0, r0, 0
    vitof0.xyzw vf2, vf2
    sll r0, r0, 0
    vitof0.xyzw vf3, vf3
    sll r0, r0, 0
    vadd.xyzw vf5, vf5, vf6
    sll r0, r0, 0
    vadd.xz vf1, vf1, vf15
    sll r0, r0, 0
    vmulaw.xyzw acc, vf27, vf0
    sll r0, r0, 0
    vadd.xyzw vf2, vf2, vf3
    sll r0, r0, 0
    vmulx.xyzw vf5, vf5, vf28
    sll r0, r0, 0
    vmaddax.xyzw acc, vf24, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf25, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf7, vf26, vf1
    sll r0, r0, 0
    vmulx.xyzw vf2, vf2, vf28
    sll r0, r0, 0
    vsub.xyzw vf6, vf4, vf5
    sll r0, r0, 0
    vaddz.xyzw vf7, vf13, vf7
    sll r0, r0, 0
    vsub.xyz vf3, vf1, vf2
    sll r0, r0, 0
    vmulw.w vf3, vf0, vf0
    sll r0, r0, 0
    vmul.xyzw vf7, vf7, vf14
    sll r0, r0, 0
    vmini.xyzw vf7, vf7, vf11
    sll r0, r0, 0
    vmaxx.xyzw vf7, vf7, vf0
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf5
    sll r0, r0, 0
    vmaddy.xyzw vf4, vf6, vf7
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf2
    sll r0, r0, 0
    vmaddy.yw vf1, vf3, vf7
    sll r0, r0, 0
    vftoi0.xyzw vf4, vf4
    sll r0, r0, 0
    sll r0, r0, 0
    sqc2 vf4, 160(t4)
    sll r0, r0, 0
    sqc2 vf1, 176(t4)
    sll r0, r0, 0
    lqc2 vf1, 176(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 188(t4)
    sll r0, r0, 0

  ;; copy 8 -> 6
    lq t6, 256(t4)
    sll r0, r0, 0
    lqc2 vf2, 272(t4)
    sll r0, r0, 0
    sq t6, 192(t4)
    sll r0, r0, 0
    sqc2 vf2, 208(t4)
    sll r0, r0, 0

;; vertex 7
    lhu t6, 58(t3)
    sll r0, r0, 0
    lhu t8, 60(t3)
    daddu t7, t6, t1
    lhu t6, 62(t3)
    daddu t8, t8, t1
    lhu t9, 56(t3)
    daddu t6, t6, t1
    lhu ra, 2(t7)
    pextlb t9, r0, t9
    lhu t7, 0(t7)
    pextlh t9, r0, t9
    lhu gp, 2(t8)
    dsll t9, t9, 12
    lhu t8, 0(t8)
    pexcw t9, t9
    lhu s5, 2(t6)
    andi ra, ra, 2047
    andi gp, gp, 2047
    andi s5, s5, 2047
    dsll s4, ra, 2
    dsll gp, gp, 2
    dsll ra, s5, 2
    daddu s4, s4, a3
    lhu t6, 0(t6)
    sll s5, t7, 3
    lw t7, 12288(s4)
    daddu gp, gp, a3
    sq t9, 8080(a0)
    sll t8, t8, 3
    lw t9, 12288(gp)
    daddu ra, ra, a3
    sw s5, 8084(a0)
    sll t6, t6, 3
    lw ra, 12288(ra)
    pextlb t7, r0, t7
    lqc2 vf1, 8080(a0)
    pextlh t7, r0, t7
    sw t8, 8084(a0)
    pextlb t8, r0, t9
    qmtc2.i vf4, t7
    pextlh t7, r0, t8
    lqc2 vf2, 8080(a0)
    pextlb t8, r0, ra
    qmtc2.i vf5, t7
    pextlh t7, r0, t8
    sw t6, 8084(a0)
    vitof0.xyzw vf4, vf4
    qmtc2.i vf6, t7
    vitof0.xyzw vf5, vf5
    lqc2 vf3, 8080(a0)
    vitof0.xyzw vf6, vf6
    sll r0, r0, 0
    vitof0.xyzw vf1, vf1
    sll r0, r0, 0
    vitof0.xyzw vf2, vf2
    sll r0, r0, 0
    vitof0.xyzw vf3, vf3
    sll r0, r0, 0
    vadd.xyzw vf5, vf5, vf6
    sll r0, r0, 0
    vadd.xz vf1, vf1, vf15
    sll r0, r0, 0
    vmulaw.xyzw acc, vf27, vf0
    sll r0, r0, 0
    vadd.xyzw vf2, vf2, vf3
    sll r0, r0, 0
    vmulx.xyzw vf5, vf5, vf28
    sll r0, r0, 0
    vmaddax.xyzw acc, vf24, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf25, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf7, vf26, vf1
    sll r0, r0, 0
    vmulx.xyzw vf2, vf2, vf28
    sll r0, r0, 0
    vsub.xyzw vf6, vf4, vf5
    sll r0, r0, 0
    vaddz.xyzw vf7, vf13, vf7
    sll r0, r0, 0
    vsub.xyz vf3, vf1, vf2
    sll r0, r0, 0
    vmulw.w vf3, vf0, vf0
    sll r0, r0, 0
    vmul.xyzw vf7, vf7, vf14
    sll r0, r0, 0
    vmini.xyzw vf7, vf7, vf11
    sll r0, r0, 0
    vmaxx.xyzw vf7, vf7, vf0
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf5
    sll r0, r0, 0
    vmaddy.xyzw vf4, vf6, vf7
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf2
    sll r0, r0, 0
    vmaddy.yw vf1, vf3, vf7
    sll r0, r0, 0
    vftoi0.xyzw vf4, vf4
    sll r0, r0, 0
    sll r0, r0, 0
    sqc2 vf4, 224(t4)
    sll r0, r0, 0
    sqc2 vf1, 240(t4)
    sll r0, r0, 0
    lqc2 vf1, 240(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 252(t4)
    sll r0, r0, 0

 ;; vertex 8
    lhu t6, 66(t3)
    sll r0, r0, 0
    lhu t7, 64(t3)
    daddu t6, t6, t1
    sll r0, r0, 0
    pextlb t8, r0, t7
    lhu t7, 2(t6)
    pextlh t8, r0, t8
    lhu t6, 0(t6)
    dsll t8, t8, 12
    andi t7, t7, 2047
    pexcw t8, t8
    mfc1 r0, f31
    dsll t7, t7, 2
    qmtc2.i vf1, t8
    daddu t7, t7, a3
    sll t6, t6, 3
    vitof0.xyzw vf1, vf1
    mtc1 f0, t6
    sll r0, r0, 0
    lw t6, 12288(t7)
    cvt.s.w f0, f0
    sll r0, r0, 0
    vadd.xyzw vf1, vf1, vf15
    sll r0, r0, 0
    pextlb t6, r0, t6
    mfc1 r0, f31
    pextlh t6, r0, t6
    mfc1 r0, f31
    sll r0, r0, 0
    sq t6, 256(t4)
    sll r0, r0, 0
    sqc2 vf1, 272(t4)
    sll r0, r0, 0
    swc1 f0, 276(t4)
    sll r0, r0, 0
    lqc2 vf1, 272(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 284(t4)
    bgezal r0, L39
    sll r0, r0, 0

B33:
    daddiu t3, t3, 72   ;; advance cache and poly table
    daddiu t4, t4, 288
B34:
L35:
    daddiu t6, a1, -171
    sll r0, r0, 0
    blez t6, L38       ;; maybe dma sync
    sll r0, r0, 0

B35:
    lw t6, 8196(a0)
    sll r0, r0, 0
B36:
L36:
    lw t0, 0(t6)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t0, t0, 256
    sll r0, r0, 0
    beq t0, r0, L37
    sll r0, r0, 0

B37:
    sll r0, r0, 0
    lw t0, 8160(a0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu t0, t0, 1
    sll r0, r0, 0
    sw t0, 8160(a0)
    beq r0, r0, L36
    sll r0, r0, 0

B38:
L37:
    lw t0, 8152(a0)
    sll r0, r0, 0
    sw v1, 128(t6)
    xori v1, v1, 4720
    sw t0, 16(t6)
    sll t7, a1, 4
    addu t7, t0, t7
    or t0, v1, r0
    sw a1, 32(t6)
    addiu a1, r0, 256
    sw a1, 0(t6)
    addiu a1, r0, 0
    sw t7, 8152(a0)
    sll r0, r0, 0
B39:
L38: ;; cache copy "down"
    sll r0, r0, 0
    lq t6, -96(t4)
    sll r0, r0, 0
    lqc2 vf2, -80(t4)
    sll r0, r0, 0
    sq t6, 0(t4)
    sll r0, r0, 0
    sqc2 vf2, 16(t4)
    sll r0, r0, 0
    lq t6, -64(t4)
    sll r0, r0, 0
    lqc2 vf2, -48(t4)
    sll r0, r0, 0
    sq t6, 32(t4)
    sll r0, r0, 0
    sqc2 vf2, 48(t4)
    sll r0, r0, 0
    lq t6, -32(t4)
    sll r0, r0, 0
    lqc2 vf2, -16(t4)
    sll r0, r0, 0
    sq t6, 64(t4)
    sll r0, r0, 0
    sqc2 vf2, 80(t4)
    sll r0, r0, 0
    lq t6, 160(t4)
    sll r0, r0, 0
    lqc2 vf2, 176(t4)
    sll r0, r0, 0
    sq t6, 96(t4)
    sll r0, r0, 0
    sqc2 vf2, 112(t4)
    sll r0, r0, 0
    lhu t6, 34(t3)
    sll r0, r0, 0

 ;; inner 0
    lhu t8, 36(t3)
    daddu t7, t6, t1
    lhu t6, 38(t3)
    daddu t8, t8, t1
    lhu t9, 32(t3)
    daddu t6, t6, t1
    lhu ra, 2(t7)
    pextlb t9, r0, t9
    lhu t7, 0(t7)
    pextlh t9, r0, t9
    lhu gp, 2(t8)
    dsll t9, t9, 12
    lhu t8, 0(t8)
    pexcw t9, t9
    lhu s5, 2(t6)
    andi ra, ra, 2047
    andi gp, gp, 2047
    andi s5, s5, 2047
    dsll s4, ra, 2
    dsll gp, gp, 2
    dsll ra, s5, 2
    daddu s4, s4, a3
    lhu t6, 0(t6)
    sll s5, t7, 3
    lw t7, 12288(s4)
    daddu gp, gp, a3
    sq t9, 8080(a0)
    sll t8, t8, 3
    lw t9, 12288(gp)
    daddu ra, ra, a3
    sw s5, 8084(a0)
    sll t6, t6, 3
    lw ra, 12288(ra)
    pextlb t7, r0, t7
    lqc2 vf1, 8080(a0)
    pextlh t7, r0, t7
    sw t8, 8084(a0)
    pextlb t8, r0, t9
    qmtc2.i vf4, t7
    pextlh t7, r0, t8
    lqc2 vf2, 8080(a0)
    pextlb t8, r0, ra
    qmtc2.i vf5, t7
    pextlh t7, r0, t8
    sw t6, 8084(a0)
    vitof0.xyzw vf4, vf4
    qmtc2.i vf6, t7
    vitof0.xyzw vf5, vf5
    lqc2 vf3, 8080(a0)
    vitof0.xyzw vf6, vf6
    sll r0, r0, 0
    vitof0.xyzw vf1, vf1
    sll r0, r0, 0
    vitof0.xyzw vf2, vf2
    sll r0, r0, 0
    vitof0.xyzw vf3, vf3
    sll r0, r0, 0
    vadd.xyzw vf5, vf5, vf6
    sll r0, r0, 0
    vadd.xz vf1, vf1, vf15
    sll r0, r0, 0
    vmulaw.xyzw acc, vf27, vf0
    sll r0, r0, 0
    vadd.xyzw vf2, vf2, vf3
    sll r0, r0, 0
    vmulx.xyzw vf5, vf5, vf28
    sll r0, r0, 0
    vmaddax.xyzw acc, vf24, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf25, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf7, vf26, vf1
    sll r0, r0, 0
    vmulx.xyzw vf2, vf2, vf28
    sll r0, r0, 0
    vsub.xyzw vf6, vf4, vf5
    sll r0, r0, 0
    vaddz.xyzw vf7, vf13, vf7
    sll r0, r0, 0
    vsub.xyz vf3, vf1, vf2
    sll r0, r0, 0
    vmulw.w vf3, vf0, vf0
    sll r0, r0, 0
    vmul.xyzw vf7, vf7, vf14
    sll r0, r0, 0
    vmini.xyzw vf7, vf7, vf11
    sll r0, r0, 0
    vmaxx.xyzw vf7, vf7, vf0
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf5
    sll r0, r0, 0
    vmaddy.xyzw vf4, vf6, vf7
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf2
    sll r0, r0, 0
    vmaddy.yw vf1, vf3, vf7
    sll r0, r0, 0
    vftoi0.xyzw vf4, vf4
    sll r0, r0, 0
    sll r0, r0, 0
    sqc2 vf4, 128(t4)
    sll r0, r0, 0
    sqc2 vf1, 144(t4)
    sll r0, r0, 0
    lqc2 vf1, 144(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 156(t4)
    sll r0, r0, 0

;; inner 1
    lhu t6, 42(t3)
    sll r0, r0, 0
    lhu t8, 44(t3)
    daddu t7, t6, t1
    lhu t6, 46(t3)
    daddu t8, t8, t1
    lhu t9, 40(t3)
    daddu t6, t6, t1
    lhu ra, 2(t7)
    pextlb t9, r0, t9
    lhu t7, 0(t7)
    pextlh t9, r0, t9
    lhu gp, 2(t8)
    dsll t9, t9, 12
    lhu t8, 0(t8)
    pexcw t9, t9
    lhu s5, 2(t6)
    andi ra, ra, 2047
    andi gp, gp, 2047
    andi s5, s5, 2047
    dsll s4, ra, 2
    dsll gp, gp, 2
    dsll ra, s5, 2
    daddu s4, s4, a3
    lhu t6, 0(t6)
    sll s5, t7, 3
    lw t7, 12288(s4)
    daddu gp, gp, a3
    sq t9, 8080(a0)
    sll t8, t8, 3
    lw t9, 12288(gp)
    daddu ra, ra, a3
    sw s5, 8084(a0)
    sll t6, t6, 3
    lw ra, 12288(ra)
    pextlb t7, r0, t7
    lqc2 vf1, 8080(a0)
    pextlh t7, r0, t7
    sw t8, 8084(a0)
    pextlb t8, r0, t9
    qmtc2.i vf4, t7
    pextlh t7, r0, t8
    lqc2 vf2, 8080(a0)
    pextlb t8, r0, ra
    qmtc2.i vf5, t7
    pextlh t7, r0, t8
    sw t6, 8084(a0)
    vitof0.xyzw vf4, vf4
    qmtc2.i vf6, t7
    vitof0.xyzw vf5, vf5
    lqc2 vf3, 8080(a0)
    vitof0.xyzw vf6, vf6
    sll r0, r0, 0
    vitof0.xyzw vf1, vf1
    sll r0, r0, 0
    vitof0.xyzw vf2, vf2
    sll r0, r0, 0
    vitof0.xyzw vf3, vf3
    sll r0, r0, 0
    vadd.xyzw vf5, vf5, vf6
    sll r0, r0, 0
    vadd.xz vf1, vf1, vf15
    sll r0, r0, 0
    vmulaw.xyzw acc, vf27, vf0
    sll r0, r0, 0
    vadd.xyzw vf2, vf2, vf3
    sll r0, r0, 0
    vmulx.xyzw vf5, vf5, vf28
    sll r0, r0, 0
    vmaddax.xyzw acc, vf24, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf25, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf7, vf26, vf1
    sll r0, r0, 0
    vmulx.xyzw vf2, vf2, vf28
    sll r0, r0, 0
    vsub.xyzw vf6, vf4, vf5
    sll r0, r0, 0
    vaddz.xyzw vf7, vf13, vf7
    sll r0, r0, 0
    vsub.xyz vf3, vf1, vf2
    sll r0, r0, 0
    vmulw.w vf3, vf0, vf0
    sll r0, r0, 0
    vmul.xyzw vf7, vf7, vf14
    sll r0, r0, 0
    vmini.xyzw vf7, vf7, vf11
    sll r0, r0, 0
    vmaxx.xyzw vf7, vf7, vf0
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf5
    sll r0, r0, 0
    vmaddy.xyzw vf4, vf6, vf7
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf2
    sll r0, r0, 0
    vmaddy.yw vf1, vf3, vf7
    sll r0, r0, 0
    vftoi0.xyzw vf4, vf4
    sll r0, r0, 0
    sll r0, r0, 0
    sqc2 vf4, 160(t4)
    sll r0, r0, 0
    sqc2 vf1, 176(t4)
    sll r0, r0, 0
    lqc2 vf1, 176(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 188(t4)
    sll r0, r0, 0

    lq t6, 256(t4)
    sll r0, r0, 0
    lqc2 vf2, 272(t4)
    sll r0, r0, 0
    sq t6, 192(t4)
    sll r0, r0, 0
    sqc2 vf2, 208(t4)
    sll r0, r0, 0

 ;; inner 2
    lhu t6, 58(t3)
    sll r0, r0, 0
    lhu t8, 60(t3)
    daddu t7, t6, t1
    lhu t6, 62(t3)
    daddu t8, t8, t1
    lhu t9, 56(t3)
    daddu t6, t6, t1
    lhu ra, 2(t7)
    pextlb t9, r0, t9
    lhu t7, 0(t7)
    pextlh t9, r0, t9
    lhu gp, 2(t8)
    dsll t9, t9, 12
    lhu t8, 0(t8)
    pexcw t9, t9
    lhu s5, 2(t6)
    andi ra, ra, 2047
    andi gp, gp, 2047
    andi s5, s5, 2047
    dsll s4, ra, 2
    dsll gp, gp, 2
    dsll ra, s5, 2
    daddu s4, s4, a3
    lhu t6, 0(t6)
    sll s5, t7, 3
    lw t7, 12288(s4)
    daddu gp, gp, a3
    sq t9, 8080(a0)
    sll t8, t8, 3
    lw t9, 12288(gp)
    daddu ra, ra, a3
    sw s5, 8084(a0)
    sll t6, t6, 3
    lw ra, 12288(ra)
    pextlb t7, r0, t7
    lqc2 vf1, 8080(a0)
    pextlh t7, r0, t7
    sw t8, 8084(a0)
    pextlb t8, r0, t9
    qmtc2.i vf4, t7
    pextlh t7, r0, t8
    lqc2 vf2, 8080(a0)
    pextlb t8, r0, ra
    qmtc2.i vf5, t7
    pextlh t7, r0, t8
    sw t6, 8084(a0)
    vitof0.xyzw vf4, vf4
    qmtc2.i vf6, t7
    vitof0.xyzw vf5, vf5
    lqc2 vf3, 8080(a0)
    vitof0.xyzw vf6, vf6
    sll r0, r0, 0
    vitof0.xyzw vf1, vf1
    sll r0, r0, 0
    vitof0.xyzw vf2, vf2
    sll r0, r0, 0
    vitof0.xyzw vf3, vf3
    sll r0, r0, 0
    vadd.xyzw vf5, vf5, vf6
    sll r0, r0, 0
    vadd.xz vf1, vf1, vf15
    sll r0, r0, 0
    vmulaw.xyzw acc, vf27, vf0
    sll r0, r0, 0
    vadd.xyzw vf2, vf2, vf3
    sll r0, r0, 0
    vmulx.xyzw vf5, vf5, vf28
    sll r0, r0, 0
    vmaddax.xyzw acc, vf24, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf25, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf7, vf26, vf1
    sll r0, r0, 0
    vmulx.xyzw vf2, vf2, vf28
    sll r0, r0, 0
    vsub.xyzw vf6, vf4, vf5
    sll r0, r0, 0
    vaddz.xyzw vf7, vf13, vf7
    sll r0, r0, 0
    vsub.xyz vf3, vf1, vf2
    sll r0, r0, 0
    vmulw.w vf3, vf0, vf0
    sll r0, r0, 0
    vmul.xyzw vf7, vf7, vf14
    sll r0, r0, 0
    vmini.xyzw vf7, vf7, vf11
    sll r0, r0, 0
    vmaxx.xyzw vf7, vf7, vf0
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf5
    sll r0, r0, 0
    vmaddy.xyzw vf4, vf6, vf7
    sll r0, r0, 0
    vmula.xyzw acc, vf11, vf2
    sll r0, r0, 0
    vmaddy.yw vf1, vf3, vf7
    sll r0, r0, 0
    vftoi0.xyzw vf4, vf4
    sll r0, r0, 0
    sll r0, r0, 0
    sqc2 vf4, 224(t4)
    sll r0, r0, 0
    sqc2 vf1, 240(t4)
    sll r0, r0, 0
    lqc2 vf1, 240(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 252(t4)
    sll r0, r0, 0


    lhu t6, 66(t3)
    sll r0, r0, 0
    lhu t7, 64(t3)
    daddu t6, t6, t1
    sll r0, r0, 0
    pextlb t8, r0, t7
    lhu t7, 2(t6)
    pextlh t8, r0, t8
    lhu t6, 0(t6)
    dsll t8, t8, 12
    andi t7, t7, 2047
    pexcw t8, t8
    mfc1 r0, f31
    dsll t7, t7, 2
    qmtc2.i vf1, t8
    daddu t7, t7, a3
    sll t6, t6, 3
    vitof0.xyzw vf1, vf1
    mtc1 f0, t6
    sll r0, r0, 0
    lw t6, 12288(t7)
    cvt.s.w f0, f0
    sll r0, r0, 0
    vadd.xyzw vf1, vf1, vf15
    sll r0, r0, 0
    pextlb t6, r0, t6
    mfc1 r0, f31
    pextlh t6, r0, t6
    mfc1 r0, f31
    sll r0, r0, 0
    sq t6, 256(t4)
    sll r0, r0, 0
    sqc2 vf1, 272(t4)
    sll r0, r0, 0
    swc1 f0, 276(t4)
    sll r0, r0, 0
    lqc2 vf1, 272(t4)
    vmulaw.xyzw acc, vf19, vf0
    sll r0, r0, 0
    vmaddax.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmadday.xyzw acc, vf17, vf1
    sll r0, r0, 0
    vmaddz.xyzw vf1, vf18, vf1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf31
    sll r0, r0, 0
    vclip.xyz vf1, vf1
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
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    cfc2.i t6, Clipping
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 47
    sll r0, r0, 0
    sll r0, r0, 0
    sw t6, 284(t4)
    bgezal r0, L39
    sll r0, r0, 0

B40:
    daddiu t3, t3, 72
    daddiu t4, t4, 288
    bgtz t5, L35
    daddiu t5, t5, -1

B41:
    bgtz t2, L31
    daddiu t2, t2, -1

B42:
    beq r0, r0, L52
    sll r0, r0, 0

B43:
L39:
  ;;;;;;; subroutine
  ;; t4 = hfrag-cache
  ;; t3 = p9
  ;; t1 = gathered verts (spad input buffer)
  ;; t0 = output buffer on spad
  ;; a0 = work
    sll r0, r0, 0
    lw t6, 28(t4)  ;; cv0.clip
    sll r0, r0, 0
    lw t7, 124(t4) ;; cv6.clip
    sll r0, r0, 0
    lw t8, 60(t4) ;; cv6.clip
    sll r0, r0, 0
    lw t9, 156(t4) ;; cv8.clip
    and gp, t6, t7
    and s5, t8, t9
    and gp, gp, s5
    or t8, t8, t9
    bne gp, r0, L41
    or t6, t6, t7

B44:
    or t7, t6, t8
    lq t6, 0(t4)    ;; t6 = cv0.col
    bne t7, r0, L40
    lq t7, 96(t4)   ;; t7 = cv3.col

B45:
    sll r0, r0, 0
    lq t9, 32(t4)    ;; t9 = cv1.col
    sll r0, r0, 0
    lq t8, 128(t4)   ;; t8 = cv4.col
    ppach t6, r0, t6 ;; cv0.col u16's
    lwc1 f0, 20(t4)  ;; f0 = cv0.posy
    ppach t7, r0, t7 ;; cv3.col u16's
    lwc1 f1, 116(t4) ;; f1 = cv3.posy
    ppach t9, r0, t9 ;; t9 = cv1.col u16's
    lwc1 f2, 52(t4)  ;; f2 = cv1.height
    ppach t8, r0, t8 ;; col to u8's
    lwc1 f3, 148(t4) ;; f3 = cv4's height
    ppacb t6, r0, t6 ;; cv0.col u8's
    swc1 f0, 32(t0)
    ppacb t7, r0, t7 ;; cv3.col u8's
    swc1 f1, 36(t0)
    ppacb t9, r0, t9
    swc1 f2, 40(t0)
    ppacb t8, r0, t8
    swc1 f3, 44(t0)
    sll r0, r0, 0
    sw t6, 64(t0) ;; writing colors 0
    sll r0, r0, 0
    sw t7, 68(t0) ;; writing colors 3
    sll r0, r0, 0
    sw t9, 72(t0) ;; colors 1
    sll r0, r0, 0
    sw t8, 76(t0) ;; colors 4
    sll r0, r0, 0

    lhu t7, 2(t3) ;; p9[0].index0
    sll r0, r0, 0
    lw t6, 8284(a0) ;; buckets-near
    daddu t7, t7, t1 ;; t7 = gathered_verts[p9[0].index0]
    lw t8, 8152(a0)  ;; t8 = dma-buffer base.
    sll r0, r0, 0
    lhu t7, 2(t7)    ;; t7 = t7.packed-index
    dsra t7, t7, 11  ;; t7 = upperbit11
    lqc2 vf1, 16(t4) ;; vf1 is the whole position
    sll r0, r0, 0
    dsll t7, t7, 4   ;; multiply by 16 to get the right bucket
    daddu t7, t6, t7 ;; hfrag-bucket pointer
    sqc2 vf1, 16(t0) ;; store output corner
    sll r0, r0, 0
    lh t9, 6(t7)     ;; vtx count
    sll r0, r0, 0
    lw t6, 0(t7)     ;; next pointer
    dsll s5, a1, 4
    lh gp, 4(t7)     ;; count
    daddu s5, s5, t8 ;; dma dest stuff...
    sw t6, 1700(a0)  ;; dma next stuff
    daddu t8, t9, a0
    sw s5, 0(t7)     ;; update bucket
    daddiu gp, gp, 1 ;; inc count
    daddiu s5, t9, -16
    addiu t9, r0, 32
    sh gp, 4(t7)
    movz s5, t9, s5 ;; dma crap
    lq t9, 1696(a0)
    sll r0, r0, 0
    sh s5, 6(t7)
    sll r0, r0, 0
    lq gp, 1728(a0)
    sll r0, r0, 0
    lq t7, 1392(t8)
    movz t9, gp, t6
    lq t6, 1424(t8)
    sll r0, r0, 0
    sq t7, 0(t0)
    sll r0, r0, 0
    sq t6, 48(t0)
    daddiu a1, a1, 6
    sq t9, 80(t0)
    beq r0, r0, L42
    daddiu t0, t0, 96

B46:
L40:
    sll r0, r0, 0
    lq t8, 32(t4)
    sll r0, r0, 0
    lq t9, 128(t4)
    sq t6, 32(t0)
    sll r0, r0, 0
    sq t7, 80(t0)
    sll r0, r0, 0
    sq t8, 128(t0)
    sll r0, r0, 0
    sq t9, 176(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    lhu t7, 2(t3)
    sll r0, r0, 0
    lw t6, 8284(a0)
    daddu t7, t7, t1
    lqc2 vf2, 16(t4)
    sll r0, r0, 0
    lhu t7, 2(t7)
    sll r0, r0, 0
    lqc2 vf3, 112(t4)
    dsra t7, t7, 11
    lw t9, 8152(a0)
    dsll t7, t7, 4
    sll r0, r0, 0
    daddu t6, t6, t7
    lqc2 vf4, 48(t4)
    sll r0, r0, 0
    lh t8, 14(t6)
    sll r0, r0, 0
    lw t7, 8(t6)
    dsll s5, a1, 4
    lh gp, 12(t6)
    daddu s5, s5, t9
    sw t7, 1748(a0)
    daddu t9, t8, a0
    sw s5, 8(t6)
    daddiu gp, gp, 1
    lq t9, 1072(t9)
    daddiu t8, t8, -16
    sh gp, 12(t6)
    addiu gp, r0, 48
    lq s5, 1744(a0)
    sll r0, r0, 0
    lq s4, 1760(a0)
    movz t8, gp, t8
    sq t9, 0(t0)
    movz s5, s4, t7
    sh t8, 14(t6)
    sll r0, r0, 0
    sq s5, 208(t0)
    sll r0, r0, 0
    lqc2 vf5, 144(t4)
    sqc2 vf2, 48(t0)
    sll r0, r0, 0
    sqc2 vf20, 16(t0)
    sll r0, r0, 0
    sqc2 vf3, 96(t0)
    sll r0, r0, 0
    sqc2 vf21, 64(t0)
    sll r0, r0, 0
    sqc2 vf4, 144(t0)
    sll r0, r0, 0
    sqc2 vf22, 112(t0)
    sll r0, r0, 0
    sqc2 vf5, 192(t0)
    daddiu a1, a1, 14
    sqc2 vf23, 160(t0)
    daddiu t0, t0, 224
    beq r0, r0, L42
    sll r0, r0, 0

B47:
L41:
    sll r0, r0, 0
    sll r0, r0, 0
B48:
L42:
    sll r0, r0, 0
    lw t6, 60(t4) ;; 1
    sll r0, r0, 0
    lw t7, 156(t4) ;; 4
    sll r0, r0, 0
    lw t8, 92(t4) ;; 2
    sll r0, r0, 0
    lw t9, 188(t4) ;; 5
    and gp, t6, t7
    and s5, t8, t9
    and gp, gp, s5
    or t8, t8, t9
    bne gp, r0, L44
    or t6, t6, t7

B49:
    or t7, t6, t8
    lq t6, 32(t4)
    bne t7, r0, L43
    lq t7, 128(t4)

B50:
    sll r0, r0, 0
    lq t9, 64(t4)
    sll r0, r0, 0
    lq t8, 160(t4)
    ppach t6, r0, t6
    lwc1 f0, 52(t4)
    ppach t7, r0, t7
    lwc1 f1, 148(t4)
    ppach t9, r0, t9
    lwc1 f2, 84(t4)
    ppach t8, r0, t8
    lwc1 f3, 180(t4)
    ppacb t6, r0, t6
    swc1 f0, 32(t0)
    ppacb t7, r0, t7
    swc1 f1, 36(t0)
    ppacb t9, r0, t9
    swc1 f2, 40(t0)
    ppacb t8, r0, t8
    swc1 f3, 44(t0)
    sll r0, r0, 0
    sw t6, 64(t0)
    sll r0, r0, 0
    sw t7, 68(t0)
    sll r0, r0, 0
    sw t9, 72(t0)
    sll r0, r0, 0
    sw t8, 76(t0)
    sll r0, r0, 0
    lhu t7, 10(t3)
    sll r0, r0, 0
    lw t6, 8284(a0)
    daddu t7, t7, t1
    lw t8, 8152(a0)
    sll r0, r0, 0
    lhu t7, 2(t7)
    dsra t7, t7, 11
    lqc2 vf1, 48(t4)
    sll r0, r0, 0
    dsll t7, t7, 4
    daddu t7, t6, t7
    sqc2 vf1, 16(t0)
    sll r0, r0, 0
    lh t9, 6(t7)
    sll r0, r0, 0
    lw t6, 0(t7)
    dsll s5, a1, 4
    lh gp, 4(t7)
    daddu s5, s5, t8
    sw t6, 1700(a0)
    daddu t8, t9, a0
    sw s5, 0(t7)
    daddiu gp, gp, 1
    daddiu s5, t9, -16
    addiu t9, r0, 32
    sh gp, 4(t7)
    movz s5, t9, s5
    lq t9, 1696(a0)
    sll r0, r0, 0
    sh s5, 6(t7)
    sll r0, r0, 0
    lq gp, 1728(a0)
    sll r0, r0, 0
    lq t7, 1392(t8)
    movz t9, gp, t6
    lq t6, 1424(t8)
    sll r0, r0, 0
    sq t7, 0(t0)
    sll r0, r0, 0
    sq t6, 48(t0)
    daddiu a1, a1, 6
    sq t9, 80(t0)
    beq r0, r0, L45
    daddiu t0, t0, 96

B51:
L43:
    sll r0, r0, 0
    lq t8, 64(t4)
    sll r0, r0, 0
    lq t9, 160(t4)
    sq t6, 32(t0)
    sll r0, r0, 0
    sq t7, 80(t0)
    sll r0, r0, 0
    sq t8, 128(t0)
    sll r0, r0, 0
    sq t9, 176(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    lhu t7, 10(t3)
    sll r0, r0, 0
    lw t6, 8284(a0)
    daddu t7, t7, t1
    lqc2 vf2, 48(t4)
    sll r0, r0, 0
    lhu t7, 2(t7)
    sll r0, r0, 0
    lqc2 vf3, 144(t4)
    dsra t7, t7, 11
    lw t9, 8152(a0)
    dsll t7, t7, 4
    sll r0, r0, 0
    daddu t6, t6, t7
    lqc2 vf4, 80(t4)
    sll r0, r0, 0
    lh t8, 14(t6)
    sll r0, r0, 0
    lw t7, 8(t6)
    dsll s5, a1, 4
    lh gp, 12(t6)
    daddu s5, s5, t9
    sw t7, 1748(a0)
    daddu t9, t8, a0
    sw s5, 8(t6)
    daddiu gp, gp, 1
    lq t9, 1072(t9)
    daddiu t8, t8, -16
    sh gp, 12(t6)
    addiu gp, r0, 48
    lq s5, 1744(a0)
    sll r0, r0, 0
    lq s4, 1760(a0)
    movz t8, gp, t8
    sq t9, 0(t0)
    movz s5, s4, t7
    sh t8, 14(t6)
    sll r0, r0, 0
    sq s5, 208(t0)
    sll r0, r0, 0
    lqc2 vf5, 176(t4)
    sqc2 vf2, 48(t0)
    sll r0, r0, 0
    sqc2 vf20, 16(t0)
    sll r0, r0, 0
    sqc2 vf3, 96(t0)
    sll r0, r0, 0
    sqc2 vf21, 64(t0)
    sll r0, r0, 0
    sqc2 vf4, 144(t0)
    sll r0, r0, 0
    sqc2 vf22, 112(t0)
    sll r0, r0, 0
    sqc2 vf5, 192(t0)
    daddiu a1, a1, 14
    sqc2 vf23, 160(t0)
    daddiu t0, t0, 224
    beq r0, r0, L45
    sll r0, r0, 0

B52:
L44:
    sll r0, r0, 0
    sll r0, r0, 0
B53:
L45:
    sll r0, r0, 0
    lw t6, 124(t4)
    sll r0, r0, 0
    lw t7, 220(t4)
    sll r0, r0, 0
    lw t8, 156(t4)
    sll r0, r0, 0
    lw t9, 252(t4)
    and gp, t6, t7
    and s5, t8, t9
    and gp, gp, s5
    or t8, t8, t9
    bne gp, r0, L47
    or t6, t6, t7

B54:
    or t7, t6, t8
    lq t6, 96(t4)
    bne t7, r0, L46
    lq t7, 192(t4)

B55:
    sll r0, r0, 0
    lq t9, 128(t4)
    sll r0, r0, 0
    lq t8, 224(t4)
    ppach t6, r0, t6
    lwc1 f0, 116(t4)
    ppach t7, r0, t7
    lwc1 f1, 212(t4)
    ppach t9, r0, t9
    lwc1 f2, 148(t4)
    ppach t8, r0, t8
    lwc1 f3, 244(t4)
    ppacb t6, r0, t6
    swc1 f0, 32(t0)
    ppacb t7, r0, t7
    swc1 f1, 36(t0)
    ppacb t9, r0, t9
    swc1 f2, 40(t0)
    ppacb t8, r0, t8
    swc1 f3, 44(t0)
    sll r0, r0, 0
    sw t6, 64(t0)
    sll r0, r0, 0
    sw t7, 68(t0)
    sll r0, r0, 0
    sw t9, 72(t0)
    sll r0, r0, 0
    sw t8, 76(t0)
    sll r0, r0, 0
    lhu t7, 26(t3)
    sll r0, r0, 0
    lw t6, 8284(a0)
    daddu t7, t7, t1
    lw t8, 8152(a0)
    sll r0, r0, 0
    lhu t7, 2(t7)
    dsra t7, t7, 11
    lqc2 vf1, 112(t4)
    sll r0, r0, 0
    dsll t7, t7, 4
    daddu t7, t6, t7
    sqc2 vf1, 16(t0)
    sll r0, r0, 0
    lh t9, 6(t7)
    sll r0, r0, 0
    lw t6, 0(t7)
    dsll s5, a1, 4
    lh gp, 4(t7)
    daddu s5, s5, t8
    sw t6, 1700(a0)
    daddu t8, t9, a0
    sw s5, 0(t7)
    daddiu gp, gp, 1
    daddiu s5, t9, -16
    addiu t9, r0, 32
    sh gp, 4(t7)
    movz s5, t9, s5
    lq t9, 1696(a0)
    sll r0, r0, 0
    sh s5, 6(t7)
    sll r0, r0, 0
    lq gp, 1728(a0)
    sll r0, r0, 0
    lq t7, 1392(t8)
    movz t9, gp, t6
    lq t6, 1424(t8)
    sll r0, r0, 0
    sq t7, 0(t0)
    sll r0, r0, 0
    sq t6, 48(t0)
    daddiu a1, a1, 6
    sq t9, 80(t0)
    beq r0, r0, L48
    daddiu t0, t0, 96

B56:
L46:
    sll r0, r0, 0
    lq t8, 128(t4)
    sll r0, r0, 0
    lq t9, 224(t4)
    sq t6, 32(t0)
    sll r0, r0, 0
    sq t7, 80(t0)
    sll r0, r0, 0
    sq t8, 128(t0)
    sll r0, r0, 0
    sq t9, 176(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    lhu t7, 26(t3)
    sll r0, r0, 0
    lw t6, 8284(a0)
    daddu t7, t7, t1
    lqc2 vf2, 112(t4)
    sll r0, r0, 0
    lhu t7, 2(t7)
    sll r0, r0, 0
    lqc2 vf3, 208(t4)
    dsra t7, t7, 11
    lw t9, 8152(a0)
    dsll t7, t7, 4
    sll r0, r0, 0
    daddu t6, t6, t7
    lqc2 vf4, 144(t4)
    sll r0, r0, 0
    lh t8, 14(t6)
    sll r0, r0, 0
    lw t7, 8(t6)
    dsll s5, a1, 4
    lh gp, 12(t6)
    daddu s5, s5, t9
    sw t7, 1748(a0)
    daddu t9, t8, a0
    sw s5, 8(t6)
    daddiu gp, gp, 1
    lq t9, 1072(t9)
    daddiu t8, t8, -16
    sh gp, 12(t6)
    addiu gp, r0, 48
    lq s5, 1744(a0)
    sll r0, r0, 0
    lq s4, 1760(a0)
    movz t8, gp, t8
    sq t9, 0(t0)
    movz s5, s4, t7
    sh t8, 14(t6)
    sll r0, r0, 0
    sq s5, 208(t0)
    sll r0, r0, 0
    lqc2 vf5, 240(t4)
    sqc2 vf2, 48(t0)
    sll r0, r0, 0
    sqc2 vf20, 16(t0)
    sll r0, r0, 0
    sqc2 vf3, 96(t0)
    sll r0, r0, 0
    sqc2 vf21, 64(t0)
    sll r0, r0, 0
    sqc2 vf4, 144(t0)
    sll r0, r0, 0
    sqc2 vf22, 112(t0)
    sll r0, r0, 0
    sqc2 vf5, 192(t0)
    daddiu a1, a1, 14
    sqc2 vf23, 160(t0)
    daddiu t0, t0, 224
    beq r0, r0, L48
    sll r0, r0, 0

B57:
L47:
    sll r0, r0, 0
    sll r0, r0, 0
B58:
L48:
    sll r0, r0, 0
    lw t6, 156(t4)
    sll r0, r0, 0
    lw t7, 252(t4)
    sll r0, r0, 0
    lw t8, 188(t4)
    sll r0, r0, 0
    lw t9, 284(t4)
    and gp, t6, t7
    and s5, t8, t9
    and gp, gp, s5
    or t8, t8, t9
    bne gp, r0, L50
    or t6, t6, t7

B59:
    or t7, t6, t8
    lq t6, 128(t4)
    bne t7, r0, L49
    lq t7, 224(t4)

B60:
    sll r0, r0, 0
    lq t9, 160(t4)
    sll r0, r0, 0
    lq t8, 256(t4)
    ppach t6, r0, t6
    lwc1 f0, 148(t4)
    ppach t7, r0, t7
    lwc1 f1, 244(t4)
    ppach t9, r0, t9
    lwc1 f2, 180(t4)
    ppach t8, r0, t8
    lwc1 f3, 276(t4)
    ppacb t6, r0, t6
    swc1 f0, 32(t0)
    ppacb t7, r0, t7
    swc1 f1, 36(t0)
    ppacb t9, r0, t9
    swc1 f2, 40(t0)
    ppacb t8, r0, t8
    swc1 f3, 44(t0)
    sll r0, r0, 0
    sw t6, 64(t0)
    sll r0, r0, 0
    sw t7, 68(t0)
    sll r0, r0, 0
    sw t9, 72(t0)
    sll r0, r0, 0
    sw t8, 76(t0)
    sll r0, r0, 0
    lhu t7, 34(t3)
    sll r0, r0, 0
    lw t6, 8284(a0)
    daddu t7, t7, t1
    lw t8, 8152(a0)
    sll r0, r0, 0
    lhu t7, 2(t7)
    dsra t7, t7, 11
    lqc2 vf1, 144(t4)
    sll r0, r0, 0
    dsll t7, t7, 4
    daddu t7, t6, t7
    sqc2 vf1, 16(t0)
    sll r0, r0, 0
    lh t9, 6(t7)
    sll r0, r0, 0
    lw t6, 0(t7)
    dsll s5, a1, 4
    lh gp, 4(t7)
    daddu s5, s5, t8
    sw t6, 1700(a0)
    daddu t8, t9, a0
    sw s5, 0(t7)
    daddiu gp, gp, 1
    daddiu s5, t9, -16
    addiu t9, r0, 32
    sh gp, 4(t7)
    movz s5, t9, s5
    lq t9, 1696(a0)
    sll r0, r0, 0
    sh s5, 6(t7)
    sll r0, r0, 0
    lq gp, 1728(a0)
    sll r0, r0, 0
    lq t7, 1392(t8)
    movz t9, gp, t6
    lq t6, 1424(t8)
    sll r0, r0, 0
    sq t7, 0(t0)
    sll r0, r0, 0
    sq t6, 48(t0)
    daddiu a1, a1, 6
    sq t9, 80(t0)
    beq r0, r0, L51
    daddiu t0, t0, 96

B61:
L49:
    sll r0, r0, 0
    lq t8, 160(t4)
    sll r0, r0, 0
    lq t9, 256(t4)
    sq t6, 32(t0)
    sll r0, r0, 0
    sq t7, 80(t0)
    sll r0, r0, 0
    sq t8, 128(t0)
    sll r0, r0, 0
    sq t9, 176(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    lhu t7, 34(t3)
    sll r0, r0, 0
    lw t6, 8284(a0)
    daddu t7, t7, t1
    lqc2 vf2, 144(t4)
    sll r0, r0, 0
    lhu t7, 2(t7)
    sll r0, r0, 0
    lqc2 vf3, 240(t4)
    dsra t7, t7, 11
    lw t9, 8152(a0)
    dsll t7, t7, 4
    sll r0, r0, 0
    daddu t6, t6, t7
    lqc2 vf4, 176(t4)
    sll r0, r0, 0
    lh t8, 14(t6)
    sll r0, r0, 0
    lw t7, 8(t6)
    dsll s5, a1, 4
    lh gp, 12(t6)
    daddu s5, s5, t9
    sw t7, 1748(a0)
    daddu t9, t8, a0
    sw s5, 8(t6)
    daddiu gp, gp, 1
    lq t9, 1072(t9)
    daddiu t8, t8, -16
    sh gp, 12(t6)
    addiu gp, r0, 48
    lq s5, 1744(a0)
    sll r0, r0, 0
    lq s4, 1760(a0)
    movz t8, gp, t8
    sq t9, 0(t0)
    movz s5, s4, t7
    sh t8, 14(t6)
    sll r0, r0, 0
    sq s5, 208(t0)
    sll r0, r0, 0
    lqc2 vf5, 272(t4)
    sqc2 vf2, 48(t0)
    sll r0, r0, 0
    sqc2 vf20, 16(t0)
    sll r0, r0, 0
    sqc2 vf3, 96(t0)
    sll r0, r0, 0
    sqc2 vf21, 64(t0)
    sll r0, r0, 0
    sqc2 vf4, 144(t0)
    sll r0, r0, 0
    sqc2 vf22, 112(t0)
    sll r0, r0, 0
    sqc2 vf5, 192(t0)
    daddiu a1, a1, 14
    sqc2 vf23, 160(t0)
    daddiu t0, t0, 224
    beq r0, r0, L51
    sll r0, r0, 0

B62:
L50:
    sll r0, r0, 0
    sll r0, r0, 0
B63:
L51:
    jr ra
    sll r0, r0, 0

B64:
L52:
    bgez a2, L18
    or a2, a2, r0

B65:
    beq a1, r0, L55
    sll r0, r0, 0

B66:
    lw a2, 8196(a0)
    sll r0, r0, 0
B67:
L53:
    lw a3, 0(a2)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi a3, a3, 256
    sll r0, r0, 0
    beq a3, r0, L54
    sll r0, r0, 0

B68:
    sll r0, r0, 0
    lw a3, 8160(a0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu a3, a3, 1
    sll r0, r0, 0
    sw a3, 8160(a0)
    beq r0, r0, L53
    sll r0, r0, 0

B69:
L54:
    lw a3, 8152(a0)
    sll r0, r0, 0
    sw v1, 128(a2)
    xori t0, v1, 4720
    sw a3, 16(a2)
    sll v1, a1, 4
    addu v1, a3, v1
    or a3, t0, r0
    sw a1, 32(a2)
    addiu a1, r0, 256
    sw a1, 0(a2)
    addiu a1, r0, 0
    sw v1, 8152(a0)
    sll r0, r0, 0
B70:
L55:
    lw v1, 8196(a0)
    sll r0, r0, 0
B71:
L56:
    lw a1, 0(v1)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi a1, a1, 256
    sll r0, r0, 0
    beq a1, r0, L57
    sll r0, r0, 0

B72:
    sll r0, r0, 0
    lw a1, 8160(a0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu a1, a1, 1
    sll r0, r0, 0
    sw a1, 8160(a0)
    beq r0, r0, L56
    sll r0, r0, 0

B73:
L57:
    lw v1, 8152(a0)
    sll r0, r0, 0
    lw a0, 8148(a0)
    sll r0, r0, 0
    sw v1, 4(a0)
    sll r0, r0, 0
    or v0, r0, r0
    ld ra, 0(sp)
    lq gp, 48(sp)
    lq s5, 32(sp)
    lq s4, 16(sp)
    jr ra
    daddiu sp, sp, 64
```

# Unknown Method 23 (texture DMA generation?)
```
L219:
    daddiu sp, sp, -64
    sd ra, 0(sp)
    sq s4, 16(sp)
    sq s5, 32(sp)
    sq gp, 48(sp)
B0:
    lw v1, *display*(s7)
    lw v1, 0(v1)
    dsll v1, v1, 2
    lw a1, *display*(s7)
    daddu v1, v1, a1
    lwu v1, 8(v1)        ;; load the on-screen display frame
    lwu a2, 36(v1)       ;; global-buf for DMA
    sll r0, r0, 0
    sll r0, r0, 0
    lw a1, 4(a2)         ;; dma-buffer base
    lui v1, 4096
    sw a2, 8148(a0)      ;; stash buffer in work.
    sync.l
    cache dxwbin a1, 0
    sync.l
    cache dxwbin a1, 1
    sync.l
    lui a2, 28672       ;; a2 = scratchpad
    ori a3, v1, 53248   ;; a3 = spr-from dma control register
    vmaxw.xyzw vf11, vf0, vf0 ;; vf11 = [1, 1, 1, 1]
    ori v1, a2, 2720    ;; v1 = spad output buffer
    sw r0, 1736(a0)     ;; modify dma return template
    addiu a2, r0, 0     ;; a2 = 0
    sw a3, 8196(a0)     ;; stash spr-from reg
    or a3, v1, r0       ;; a3 = spad output
    sw a1, 8152(a0)     ;; stash global dma (EE) ptr
    sll r0, r0, 0
    lqc2 vf29, 1728(a0) ;; vf29 = ret-tmpl
    sll r0, r0, 0
    lw t0, 8152(a0)     ;; t0 = global dma ptr
    sll r0, r0, 0
    lw a1, 8264(a0)     ;; a1 = work.hfragment.shaders
    sll r0, r0, 0
    sw t0, 8388(a0)     ;; far-texture = global dma ptr
    daddiu t0, a2, -200
    sll r0, r0, 0
    blez t0, L222
    sll r0, r0, 0

B1: ;; if full, dma from transfer.
    lw t0, 8196(a0)
    sll r0, r0, 0
B2:
L220:
    lw a3, 0(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi a3, a3, 256
    sll r0, r0, 0
    beq a3, r0, L221
    sll r0, r0, 0

B3:
    sll r0, r0, 0
    lw a3, 8160(a0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu a3, a3, 1
    sll r0, r0, 0
    sw a3, 8160(a0)
    beq r0, r0, L220
    sll r0, r0, 0

B4:
L221:
    lw a3, 8152(a0) ;; dma buffer swap
    sll r0, r0, 0
    sw v1, 128(t0)
    xori v1, v1, 4720
    sw a3, 16(t0)
    sll t1, a2, 4
    addu t1, a3, t1
    or a3, v1, r0
    sw a2, 32(t0)
    addiu a2, r0, 256
    sw a2, 0(t0)
    addiu a2, r0, 0
    sw t1, 8152(a0)
    sll r0, r0, 0
B5:
L222:
    sll r0, r0, 0 ;; set up the 1 far-texture bucket.
    lq t1, 1776(a0)
    sll r0, r0, 0
    lq t2, 1792(a0)
    sll r0, r0, 0
    lq t3, 1936(a0)
    sll r0, r0, 0
    lq t4, 1952(a0)
    sll r0, r0, 0
    lq t5, 1968(a0)
    sll r0, r0, 0
    lq t0, 1984(a0)
    sq t1, 0(a3)
    sll r0, r0, 0
    sq t2, 16(a3)
    sll r0, r0, 0
    sq t3, 32(a3)
    sll r0, r0, 0
    sq t4, 48(a3)
    sll r0, r0, 0
    sq t5, 64(a3)
    daddiu a2, a2, 6
    sq t0, 80(a3)
    daddiu a3, a3, 96
    sll r0, r0, 0
    lq t0, 2128(a0)
    sll r0, r0, 0
    lq t1, 2144(a0)
    sll r0, r0, 0
    lq t2, 80(a1)
    sll r0, r0, 0
    lq t3, 96(a1)
    sll r0, r0, 0
    lq t4, 112(a1)
    sll r0, r0, 0
    lq t5, 128(a1)
    sll r0, r0, 0
    lq t6, 144(a1)
    sq t0, 0(a3)
    sll r0, r0, 0
    sq t1, 16(a3)
    sll r0, r0, 0
    sq t2, 32(a3)
    sll r0, r0, 0
    sq t3, 48(a3)
    sll r0, r0, 0
    sq t4, 64(a3)
    sll r0, r0, 0
    sq t5, 80(a3)
    daddiu a1, a2, 7
    sq t6, 96(a3)
    daddiu a2, a3, 112
    sll r0, r0, 0
    lq t0, 2192(a0)
    sll r0, r0, 0
    lq t1, 2208(a0)
    sll r0, r0, 0
    lq t2, 2256(a0)
    sll r0, r0, 0
    lq t3, 3360(a0)
    sll r0, r0, 0
    lq t4, 2352(a0)
    sll r0, r0, 0
    lq t5, 3360(a0)
    sll r0, r0, 0
    lq a3, 2368(a0)
    sq t0, 0(a2)
    sll r0, r0, 0
    sq t1, 16(a2)
    sll r0, r0, 0
    sq t2, 32(a2)
    sll r0, r0, 0
    sq t3, 48(a2)
    sll r0, r0, 0
    sq t4, 64(a2)
    sll r0, r0, 0
    sq t5, 80(a2)
    daddiu a1, a1, 7
    sq a3, 96(a2)
    daddiu a2, a2, 112
    sll r0, r0, 0
    lq t0, 1776(a0)
    sll r0, r0, 0
    lq t1, 1792(a0)
    sll r0, r0, 0
    lq t2, 2064(a0)
    sll r0, r0, 0
    lq t3, 2080(a0)
    sll r0, r0, 0
    lq t4, 2096(a0)
    sll r0, r0, 0
    lq a3, 2112(a0)
    sq t0, 0(a2)
    sll r0, r0, 0
    sq t1, 16(a2)
    sll r0, r0, 0
    sq t2, 32(a2)
    sll r0, r0, 0
    sq t3, 48(a2)
    sll r0, r0, 0
    sq t4, 64(a2)
    daddiu a1, a1, 6
    sq a3, 80(a2)
    daddiu a2, a2, 96
    sll r0, r0, 0
    daddiu t2, a1, 1
    sqc2 vf29, 0(a2)
    daddiu t3, a2, 16
    or a1, a0, r0
    lw a2, 8284(a0) ;; a2 = hfragment.buckets-near
    sll r0, r0, 0
    lw a3, 8272(a0) ;; a3 = hfragment.montage
    sll r0, r0, 0
    lhu t0, 8302(a0) ;; t0 = hfragment.num-buckets-near
    beq r0, r0, L230
    lw t1, 8264(a0)  ;; t1 = hfragment.shaders

B6:
L223:
    daddu t5, t4, t5 ;; t5 = count + count-scissor
    lw t4, 8152(a0)  ;; t4 = dma-buffer.base
    beq t5, r0, L230 ;; skip ahead to L230 if no near buckets
    dsll t5, t2, 4   ;; t5 = qw offset in EE memory dma buffer??

B7:
    daddu t4, t5, t4  ;; t4 = output for this texture DMA
    sw t4, 8392(a1)   ;; store dma ptr in work.near-textures[bucket-idx]
    daddiu t4, t2, -102
    sll r0, r0, 0
    blez t4, L226
    sll r0, r0, 0

B8:                  ;; copy back from scratchpad to main memory
    lw t4, 8196(a0)
    sll r0, r0, 0
B9:
L224:
    lw t3, 0(t4)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t3, t3, 256
    sll r0, r0, 0
    beq t3, r0, L225
    sll r0, r0, 0

B10:
    sll r0, r0, 0
    lw t3, 8160(a0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu t3, t3, 1
    sll r0, r0, 0
    sw t3, 8160(a0)
    beq r0, r0, L224
    sll r0, r0, 0

B11:
L225:
    lw t3, 8152(a0) ;; swap dma buffers
    sll r0, r0, 0
    sw v1, 128(t4)
    xori v1, v1, 4720
    sw t3, 16(t4)
    sll t5, t2, 4
    addu t5, t3, t5
    or t3, v1, r0
    sw t2, 32(t4)
    addiu t2, r0, 256
    sw t2, 0(t4)
    addiu t2, r0, 0
    sw t5, 8152(a0)
    sll r0, r0, 0
B12:
L226:
    sll r0, r0, 0
    lq t5, 1776(a0) ;; t5 = work.frame-tmpl[0]
    sll r0, r0, 0
    lq t6, 1792(a0) ;; t6 = work.frame-tmpl[1]
    sll r0, r0, 0
    lq t7, 1808(a0) ;; t7 = work.frames[0].ad[0] (frame)
    sll r0, r0, 0
    lq t8, 1824(a0) ;; t8 = work.frames[0].ad[1] (scissor, 0x7f)
    sll r0, r0, 0
    lq t9, 1840(a0) ;; t9 = work.frames[0].ad[2] (xyoffset)
    sll r0, r0, 0
    lq t4, 1856(a0) ;; t4 = work.frames[0].ad[3] (test)

    ;; store the frame template for frames[0]
    sq t5, 0(t3)
    sll r0, r0, 0
    sq t6, 16(t3)
    sll r0, r0, 0
    sq t7, 32(t3)
    sll r0, r0, 0
    sq t8, 48(t3)
    sll r0, r0, 0
    sq t9, 64(t3)
    daddiu t2, t2, 6   ;; inc offset
    sq t4, 80(t3)
    daddiu t3, t3, 96  ;; inc offset
    sll r0, r0, 0

    lq t5, 2128(a0)    ;; t5 = adgif-tmpl[0]
    sll r0, r0, 0
    lq t6, 2144(a0)    ;; t6 = adgif-tmpl[1]
    sll r0, r0, 0
    lq t7, 160(t1)     ;; t7 = shaders[2][0]
    sll r0, r0, 0
    lq t8, 176(t1)     ;; t8 = shaders[2][1]
    sll r0, r0, 0
    lq t9, 192(t1)     ;; t9 = shaders[2][2]
    sll r0, r0, 0
    lq ra, 208(t1)     ;; ra = shaders[2][3]
    sll r0, r0, 0
    lq t4, 224(t1)     ;; t4 = shaders[2][4]
    sq t5, 0(t3)
    sll r0, r0, 0
    sq t6, 16(t3)
    sll r0, r0, 0
    sq t7, 32(t3)
    sll r0, r0, 0
    sq t8, 48(t3)
    sll r0, r0, 0
    sq t9, 64(t3)
    sll r0, r0, 0
    sq ra, 80(t3)
    daddiu t2, t2, 7   ;; inc offset
    sq t4, 96(t3)
    daddiu t3, t3, 112 ;; inc offset
    sll r0, r0, 0

;; montage 0
    lhu t5, 0(a3)     ;; t5 = montage[0]
    sll r0, r0, 0
    lq t4, 2192(a0)   ;; t4 = sprite-tmpl[0]
    daddu t9, t5, a0  ;; t9 = work + montage value
    lq t5, 2208(a0)   ;; t5 = sprite-tmpl[1]
    sll r0, r0, 0
    lq t6, 2256(a0)   ;; t6 = color[0] = [0x80, 0x80, 0x80, 0x80]
    sll r0, r0, 0
    lq t7, 3456(t9)   ;; load montage-tex-coords + montage[0]
    sll r0, r0, 0
    lq t8, 2384(a0)   ;; t8 = near-data[0]
    sll r0, r0, 0
    lq ra, 3472(t9)   ;; load second montage tex coords + montage[0]
    sll r0, r0, 0
    lq t9, 2400(a0)   ;; t9 = near-data[1]
    sq t4, 0(t3)      ;; store sprite[0]
    sll r0, r0, 0
    sq t5, 16(t3)     ;; store sprite[1]
    sll r0, r0, 0
    sq t6, 32(t3)     ;; store color 0x80, 0x80, 0x80, 0x80
    sll r0, r0, 0
    sq t7, 48(t3)     ;; store first tex coords
    sll r0, r0, 0
    sq t8, 64(t3)     ;; store first position
    sll r0, r0, 0
    sq ra, 80(t3)     ;; store second text coords
    daddiu t2, t2, 7
    sq t9, 96(t3)     ;; store second position
    daddiu t3, t3, 112
    sll r0, r0, 0

;; montage 1
    lhu t5, 2(a3)
    sll r0, r0, 0
    lq t4, 2192(a0)
    daddu t9, t5, a0
    lq t5, 2208(a0)
    sll r0, r0, 0
    lq t6, 2256(a0)
    sll r0, r0, 0
    lq t7, 3456(t9)
    sll r0, r0, 0
    lq t8, 2416(a0)
    sll r0, r0, 0
    lq ra, 3472(t9)
    sll r0, r0, 0
    lq t9, 2432(a0)
    sq t4, 0(t3)
    sll r0, r0, 0
    sq t5, 16(t3)
    sll r0, r0, 0
    sq t6, 32(t3)
    sll r0, r0, 0
    sq t7, 48(t3)
    sll r0, r0, 0
    sq t8, 64(t3)
    sll r0, r0, 0
    sq ra, 80(t3)
    daddiu t2, t2, 7
    sq t9, 96(t3)
    daddiu t3, t3, 112
    sll r0, r0, 0

;; montage 2
    lhu t5, 4(a3)
    sll r0, r0, 0
    lq t4, 2192(a0)
    daddu t9, t5, a0
    lq t5, 2208(a0)
    sll r0, r0, 0
    lq t6, 2256(a0)
    sll r0, r0, 0
    lq t7, 3456(t9)
    sll r0, r0, 0
    lq t8, 2448(a0)
    sll r0, r0, 0
    lq ra, 3472(t9)
    sll r0, r0, 0
    lq t9, 2464(a0)
    sq t4, 0(t3)
    sll r0, r0, 0
    sq t5, 16(t3)
    sll r0, r0, 0
    sq t6, 32(t3)
    sll r0, r0, 0
    sq t7, 48(t3)
    sll r0, r0, 0
    sq t8, 64(t3)
    sll r0, r0, 0
    sq ra, 80(t3)
    daddiu t2, t2, 7
    sq t9, 96(t3)
    daddiu t3, t3, 112
    sll r0, r0, 0

;; montage 3
    lhu t5, 6(a3)
    sll r0, r0, 0
    lq t4, 2192(a0)
    daddu t9, t5, a0
    lq t5, 2208(a0)
    sll r0, r0, 0
    lq t6, 2256(a0)
    sll r0, r0, 0
    lq t7, 3456(t9)
    sll r0, r0, 0
    lq t8, 2480(a0)
    sll r0, r0, 0
    lq ra, 3472(t9)
    sll r0, r0, 0
    lq t9, 2496(a0)
    sq t4, 0(t3)
    sll r0, r0, 0
    sq t5, 16(t3)
    sll r0, r0, 0
    sq t6, 32(t3)
    sll r0, r0, 0
    sq t7, 48(t3)
    sll r0, r0, 0
    sq t8, 64(t3)
    sll r0, r0, 0
    sq ra, 80(t3)
    daddiu t2, t2, 7
    sq t9, 96(t3)
    daddiu t3, t3, 112
    sll r0, r0, 0

;; montage 4
    lhu t5, 8(a3)
    sll r0, r0, 0
    lq t4, 2192(a0)
    daddu t9, t5, a0
    lq t5, 2208(a0)
    sll r0, r0, 0
    lq t6, 2256(a0)
    sll r0, r0, 0
    lq t7, 3456(t9)
    sll r0, r0, 0
    lq t8, 2512(a0)
    sll r0, r0, 0
    lq ra, 3472(t9)
    sll r0, r0, 0
    lq t9, 2528(a0)
    sq t4, 0(t3)
    sll r0, r0, 0
    sq t5, 16(t3)
    sll r0, r0, 0
    sq t6, 32(t3)
    sll r0, r0, 0
    sq t7, 48(t3)
    sll r0, r0, 0
    sq t8, 64(t3)
    sll r0, r0, 0
    sq ra, 80(t3)
    daddiu t2, t2, 7
    sq t9, 96(t3)
    daddiu t3, t3, 112
    sll r0, r0, 0

;; montage 5
    lhu t5, 10(a3)
    sll r0, r0, 0
    lq t4, 2192(a0)
    daddu t9, t5, a0
    lq t5, 2208(a0)
    sll r0, r0, 0
    lq t6, 2256(a0)
    sll r0, r0, 0
    lq t7, 3456(t9)
    sll r0, r0, 0
    lq t8, 2544(a0)
    sll r0, r0, 0
    lq ra, 3472(t9)
    sll r0, r0, 0
    lq t9, 2560(a0)
    sq t4, 0(t3)
    sll r0, r0, 0
    sq t5, 16(t3)
    sll r0, r0, 0
    sq t6, 32(t3)
    sll r0, r0, 0
    sq t7, 48(t3)
    sll r0, r0, 0
    sq t8, 64(t3)
    sll r0, r0, 0
    sq ra, 80(t3)
    daddiu t2, t2, 7
    sq t9, 96(t3)
    daddiu t3, t3, 112
    sll r0, r0, 0

;; montage 6
    lhu t5, 12(a3)
    sll r0, r0, 0
    lq t4, 2192(a0)
    daddu t9, t5, a0
    lq t5, 2208(a0)
    sll r0, r0, 0
    lq t6, 2256(a0)
    sll r0, r0, 0
    lq t7, 3456(t9)
    sll r0, r0, 0
    lq t8, 2576(a0)
    sll r0, r0, 0
    lq ra, 3472(t9)
    sll r0, r0, 0
    lq t9, 2592(a0)
    sq t4, 0(t3)
    sll r0, r0, 0
    sq t5, 16(t3)
    sll r0, r0, 0
    sq t6, 32(t3)
    sll r0, r0, 0
    sq t7, 48(t3)
    sll r0, r0, 0
    sq t8, 64(t3)
    sll r0, r0, 0
    sq ra, 80(t3)
    daddiu t2, t2, 7
    sq t9, 96(t3)
    daddiu t3, t3, 112
    sll r0, r0, 0

;; montage 7
    lhu t5, 14(a3)
    sll r0, r0, 0
    lq t4, 2192(a0)
    daddu t9, t5, a0
    lq t5, 2208(a0)
    sll r0, r0, 0
    lq t6, 2256(a0)
    sll r0, r0, 0
    lq t7, 3456(t9)
    sll r0, r0, 0
    lq t8, 2608(a0)
    sll r0, r0, 0
    lq ra, 3472(t9)
    sll r0, r0, 0
    lq t9, 2624(a0)
    sq t4, 0(t3)
    sll r0, r0, 0
    sq t5, 16(t3)
    sll r0, r0, 0
    sq t6, 32(t3)
    sll r0, r0, 0
    sq t7, 48(t3)
    sll r0, r0, 0
    sq t8, 64(t3)
    sll r0, r0, 0
    sq ra, 80(t3)
    daddiu t2, t2, 7
    sq t9, 96(t3)
    daddiu t3, t3, 112
    sll r0, r0, 0

;; montage 8
    lhu t5, 16(a3)
    sll r0, r0, 0
    lq t4, 2192(a0)
    daddu t9, t5, a0
    lq t5, 2208(a0)
    sll r0, r0, 0
    lq t6, 2256(a0)
    sll r0, r0, 0
    lq t7, 3456(t9)
    sll r0, r0, 0
    lq t8, 2640(a0)
    sll r0, r0, 0
    lq ra, 3472(t9)
    sll r0, r0, 0
    lq t9, 2656(a0)
    sq t4, 0(t3)
    sll r0, r0, 0
    sq t5, 16(t3)
    sll r0, r0, 0
    sq t6, 32(t3)
    sll r0, r0, 0
    sq t7, 48(t3)
    sll r0, r0, 0
    sq t8, 64(t3)
    sll r0, r0, 0
    sq ra, 80(t3)
    daddiu t2, t2, 7
    sq t9, 96(t3)
    daddiu t3, t3, 112
    sll r0, r0, 0

;; montage 9
    lhu t5, 18(a3)
    sll r0, r0, 0
    lq t4, 2192(a0)
    daddu t9, t5, a0
    lq t5, 2208(a0)
    sll r0, r0, 0
    lq t6, 2256(a0)
    sll r0, r0, 0
    lq t7, 3456(t9)
    sll r0, r0, 0
    lq t8, 2672(a0)
    sll r0, r0, 0
    lq ra, 3472(t9)
    sll r0, r0, 0
    lq t9, 2688(a0)
    sq t4, 0(t3)
    sll r0, r0, 0
    sq t5, 16(t3)
    sll r0, r0, 0
    sq t6, 32(t3)
    sll r0, r0, 0
    sq t7, 48(t3)
    sll r0, r0, 0
    sq t8, 64(t3)
    sll r0, r0, 0
    sq ra, 80(t3)
    daddiu t2, t2, 7
    sq t9, 96(t3)
    daddiu t3, t3, 112
    sll r0, r0, 0

;; montage 10
    lhu t5, 20(a3)
    sll r0, r0, 0
    lq t4, 2192(a0)
    daddu t9, t5, a0
    lq t5, 2208(a0)
    sll r0, r0, 0
    lq t6, 2256(a0)
    sll r0, r0, 0
    lq t7, 3456(t9)
    sll r0, r0, 0
    lq t8, 2704(a0)
    sll r0, r0, 0
    lq ra, 3472(t9)
    sll r0, r0, 0
    lq t9, 2720(a0)
    sq t4, 0(t3)
    sll r0, r0, 0
    sq t5, 16(t3)
    sll r0, r0, 0
    sq t6, 32(t3)
    sll r0, r0, 0
    sq t7, 48(t3)
    sll r0, r0, 0
    sq t8, 64(t3)
    sll r0, r0, 0
    sq ra, 80(t3)
    daddiu t2, t2, 7
    sq t9, 96(t3)
    daddiu t3, t3, 112
    sll r0, r0, 0

;; montage 11
    lhu t5, 22(a3)
    sll r0, r0, 0
    lq t4, 2192(a0)
    daddu t9, t5, a0
    lq t5, 2208(a0)
    sll r0, r0, 0
    lq t6, 2256(a0)
    sll r0, r0, 0
    lq t7, 3456(t9)
    sll r0, r0, 0
    lq t8, 2736(a0)
    sll r0, r0, 0
    lq ra, 3472(t9)
    sll r0, r0, 0
    lq t9, 2752(a0)
    sq t4, 0(t3)
    sll r0, r0, 0
    sq t5, 16(t3)
    sll r0, r0, 0
    sq t6, 32(t3)
    sll r0, r0, 0
    sq t7, 48(t3)
    sll r0, r0, 0
    sq t8, 64(t3)
    sll r0, r0, 0
    sq ra, 80(t3)
    daddiu t2, t2, 7
    sq t9, 96(t3)
    daddiu t3, t3, 112
    sll r0, r0, 0

;; montage 12
    lhu t5, 24(a3)
    sll r0, r0, 0
    lq t4, 2192(a0)
    daddu t9, t5, a0
    lq t5, 2208(a0)
    sll r0, r0, 0
    lq t6, 2256(a0)
    sll r0, r0, 0
    lq t7, 3456(t9)
    sll r0, r0, 0
    lq t8, 2768(a0)
    sll r0, r0, 0
    lq ra, 3472(t9)
    sll r0, r0, 0
    lq t9, 2784(a0)
    sq t4, 0(t3)
    sll r0, r0, 0
    sq t5, 16(t3)
    sll r0, r0, 0
    sq t6, 32(t3)
    sll r0, r0, 0
    sq t7, 48(t3)
    sll r0, r0, 0
    sq t8, 64(t3)
    sll r0, r0, 0
    sq ra, 80(t3)
    daddiu t2, t2, 7
    sq t9, 96(t3)
    daddiu t3, t3, 112
    sll r0, r0, 0

;; montage 13
    lhu t5, 26(a3)
    sll r0, r0, 0
    lq t4, 2192(a0)
    daddu t9, t5, a0
    lq t5, 2208(a0)
    sll r0, r0, 0
    lq t6, 2256(a0)
    sll r0, r0, 0
    lq t7, 3456(t9)
    sll r0, r0, 0
    lq t8, 2800(a0)
    sll r0, r0, 0
    lq ra, 3472(t9)
    sll r0, r0, 0
    lq t9, 2816(a0)
    sq t4, 0(t3)
    sll r0, r0, 0
    sq t5, 16(t3)
    sll r0, r0, 0
    sq t6, 32(t3)
    sll r0, r0, 0
    sq t7, 48(t3)
    sll r0, r0, 0
    sq t8, 64(t3)
    sll r0, r0, 0
    sq ra, 80(t3)
    daddiu t2, t2, 7
    sq t9, 96(t3)
    daddiu t3, t3, 112
    sll r0, r0, 0

;; montage 14
    lhu t5, 28(a3)
    sll r0, r0, 0
    lq t4, 2192(a0)
    daddu t9, t5, a0
    lq t5, 2208(a0)
    sll r0, r0, 0
    lq t6, 2256(a0)
    sll r0, r0, 0
    lq t7, 3456(t9)
    sll r0, r0, 0
    lq t8, 2832(a0)
    sll r0, r0, 0
    lq ra, 3472(t9)
    sll r0, r0, 0
    lq t9, 2848(a0)
    sq t4, 0(t3)
    sll r0, r0, 0
    sq t5, 16(t3)
    sll r0, r0, 0
    sq t6, 32(t3)
    sll r0, r0, 0
    sq t7, 48(t3)
    sll r0, r0, 0
    sq t8, 64(t3)
    sll r0, r0, 0
    sq ra, 80(t3)
    daddiu t2, t2, 7
    sq t9, 96(t3)
    daddiu t3, t3, 112
    sll r0, r0, 0

;; montage 15
    lhu t5, 30(a3)
    sll r0, r0, 0
    lq t4, 2192(a0)
    daddu t9, t5, a0
    lq t5, 2208(a0)
    sll r0, r0, 0
    lq t6, 2256(a0)
    sll r0, r0, 0
    lq t7, 3456(t9)
    sll r0, r0, 0
    lq t8, 2864(a0)
    sll r0, r0, 0
    lq ra, 3472(t9)
    sll r0, r0, 0
    lq t9, 2880(a0)
    sq t4, 0(t3)
    sll r0, r0, 0
    sq t5, 16(t3)
    sll r0, r0, 0
    sq t6, 32(t3)
    sll r0, r0, 0
    sq t7, 48(t3)
    sll r0, r0, 0
    sq t8, 64(t3)
    sll r0, r0, 0
    sq ra, 80(t3)
    daddiu t2, t2, 7
    sq t9, 96(t3)
    daddiu t3, t3, 112

    daddiu t4, t2, -138
    sll r0, r0, 0
    blez t4, L229 ;; see if we need dma transfer
    sll r0, r0, 0

B13:
    lw t4, 8196(a0)
    sll r0, r0, 0
B14:
L227:
    lw t3, 0(t4)   ;; dma wait
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t3, t3, 256
    sll r0, r0, 0
    beq t3, r0, L228
    sll r0, r0, 0

B15:
    sll r0, r0, 0
    lw t3, 8160(a0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu t3, t3, 1
    sll r0, r0, 0
    sw t3, 8160(a0)
    beq r0, r0, L227
    sll r0, r0, 0

B16:
L228:
    lw t3, 8152(a0)  ;; dma buffer swap
    sll r0, r0, 0
    sw v1, 128(t4)
    xori v1, v1, 4720
    sw t3, 16(t4)
    sll t5, t2, 4
    addu t5, t3, t5
    or t3, v1, r0
    sw t2, 32(t4)
    addiu t2, r0, 256
    sw t2, 0(t4)
    addiu t2, r0, 0
    sw t5, 8152(a0)
    sll r0, r0, 0
B17:
L229:
    sll r0, r0, 0
    lq t5, 1776(a0) ;; t5 = work.frame-tmpl[0]
    sll r0, r0, 0
    lq t6, 1792(a0) ;; t6 = work.frame-tmpl[1]
    sll r0, r0, 0
    lq t7, 1872(a0)  ;; t7 = work.frames[1].ad[0] (frame)
    sll r0, r0, 0
    lq t8, 1888(a0) ;; t8 = work.frames[1].ad[1] (scissor, 0x3f)
    sll r0, r0, 0
    lq t9, 1904(a0) ;; t9 = work.frames[0].ad[2] (xyoffset)
    sll r0, r0, 0
    lq t4, 1920(a0) ;; t4 = work.frames[0].ad[3] (test)

    sq t5, 0(t3)
    sll r0, r0, 0
    sq t6, 16(t3)
    sll r0, r0, 0
    sq t7, 32(t3)
    sll r0, r0, 0
    sq t8, 48(t3)
    sll r0, r0, 0
    sq t9, 64(t3)
    daddiu t2, t2, 6
    sq t4, 80(t3)
    daddiu t3, t3, 96
    sll r0, r0, 0

    lq t5, 2224(a0) ;; t5 = mip-tmpl (different!)
    sll r0, r0, 0
    lq t6, 2240(a0) ;; t6 = mip-tmpl[2]
    sll r0, r0, 0
    lq t7, 3120(a0) ;; t7 = tex-data[0][0]
    sll r0, r0, 0
    lq t8, 3136(a0) ;; t8 = tex-data[0][1]
    sll r0, r0, 0
    lq t9, 3152(a0) ;; t9 = tex-data[0][2]
    sll r0, r0, 0
    lq ra, 2256(a0) ;; ra = color[0] = [0x80, 0x80, 0x80, 0x80]
    sll r0, r0, 0
    lq gp, 3360(a0) ;; gp = tex[0]
    sll r0, r0, 0
    lq s5, 2896(a0) ;; s5 = mip-data[0]
    sll r0, r0, 0
    lq s4, 3376(a0) ;; s4 = tex[1]
    sll r0, r0, 0

    lq t4, 2912(a0)
    sq t5, 0(t3)
    sll r0, r0, 0
    sq t6, 16(t3)
    sll r0, r0, 0
    sq t7, 32(t3)
    sll r0, r0, 0
    sq t8, 48(t3)
    sll r0, r0, 0
    sq t9, 64(t3)
    sll r0, r0, 0
    sq ra, 80(t3)
    sll r0, r0, 0
    sq gp, 96(t3)
    sll r0, r0, 0
    sq s5, 112(t3)
    sll r0, r0, 0
    sq s4, 128(t3)
    daddiu t2, t2, 10
    sq t4, 144(t3)
    daddiu t3, t3, 160
    sll r0, r0, 0
    lq t5, 1776(a0)
    sll r0, r0, 0
    lq t6, 1792(a0)
    sll r0, r0, 0
    lq t7, 1936(a0)
    sll r0, r0, 0
    lq t8, 1952(a0)
    sll r0, r0, 0
    lq t9, 1968(a0)
    sll r0, r0, 0
    lq t4, 1984(a0)
    sq t5, 0(t3)
    sll r0, r0, 0
    sq t6, 16(t3)
    sll r0, r0, 0
    sq t7, 32(t3)
    sll r0, r0, 0
    sq t8, 48(t3)
    sll r0, r0, 0
    sq t9, 64(t3)
    daddiu t2, t2, 6
    sq t4, 80(t3)
    daddiu t3, t3, 96
    sll r0, r0, 0
    lq t5, 2128(a0)
    sll r0, r0, 0
    lq t6, 2144(a0)
    sll r0, r0, 0
    lq t7, 80(t1)
    sll r0, r0, 0
    lq t8, 96(t1)
    sll r0, r0, 0
    lq t9, 112(t1)
    sll r0, r0, 0
    lq ra, 128(t1)
    sll r0, r0, 0
    lq t4, 144(t1)
    sq t5, 0(t3)
    sll r0, r0, 0
    sq t6, 16(t3)
    sll r0, r0, 0
    sq t7, 32(t3)
    sll r0, r0, 0
    sq t8, 48(t3)
    sll r0, r0, 0
    sq t9, 64(t3)
    sll r0, r0, 0
    sq ra, 80(t3)
    daddiu t2, t2, 7
    sq t4, 96(t3)
    daddiu t3, t3, 112
    sll r0, r0, 0
    lq t5, 2192(a0)
    sll r0, r0, 0
    lq t6, 2208(a0)
    sll r0, r0, 0
    lq t7, 2256(a0)
    sll r0, r0, 0
    lq t8, 3360(a0)
    sll r0, r0, 0
    lq t9, 3088(a0)
    sll r0, r0, 0
    lq ra, 3360(a0)
    sll r0, r0, 0
    lq t4, 3104(a0)
    sq t5, 0(t3)
    sll r0, r0, 0
    sq t6, 16(t3)
    sll r0, r0, 0
    sq t7, 32(t3)
    sll r0, r0, 0
    sq t8, 48(t3)
    sll r0, r0, 0
    sq t9, 64(t3)
    sll r0, r0, 0
    sq ra, 80(t3)
    daddiu t2, t2, 7
    sq t4, 96(t3)
    daddiu t3, t3, 112
    sll r0, r0, 0
    lq t5, 1776(a0)
    sll r0, r0, 0
    lq t6, 1792(a0)
    sll r0, r0, 0
    lq t7, 2000(a0)
    sll r0, r0, 0
    lq t8, 2016(a0)
    sll r0, r0, 0
    lq t9, 2032(a0)
    sll r0, r0, 0
    lq t4, 2048(a0)
    sq t5, 0(t3)
    sll r0, r0, 0
    sq t6, 16(t3)
    sll r0, r0, 0
    sq t7, 32(t3)
    sll r0, r0, 0
    sq t8, 48(t3)
    sll r0, r0, 0
    sq t9, 64(t3)
    daddiu t2, t2, 6
    sq t4, 80(t3)
    daddiu t3, t3, 96
    sll r0, r0, 0
    lq t5, 2224(a0)
    sll r0, r0, 0
    lq t6, 2240(a0)
    sll r0, r0, 0
    lq t7, 3168(a0)
    sll r0, r0, 0
    lq t8, 3184(a0)
    sll r0, r0, 0
    lq t9, 3200(a0)
    sll r0, r0, 0
    lq ra, 2272(a0)
    sll r0, r0, 0
    lq gp, 3360(a0)
    sll r0, r0, 0
    lq s5, 2928(a0)
    sll r0, r0, 0
    lq s4, 3392(a0)
    sll r0, r0, 0
    lq t4, 2944(a0)
    sq t5, 0(t3)
    sll r0, r0, 0
    sq t6, 16(t3)
    sll r0, r0, 0
    sq t7, 32(t3)
    sll r0, r0, 0
    sq t8, 48(t3)
    sll r0, r0, 0
    sq t9, 64(t3)
    sll r0, r0, 0
    sq ra, 80(t3)
    sll r0, r0, 0
    sq gp, 96(t3)
    sll r0, r0, 0
    sq s5, 112(t3)
    sll r0, r0, 0
    sq s4, 128(t3)
    daddiu t2, t2, 10
    sq t4, 144(t3)
    daddiu t3, t3, 160
    sll r0, r0, 0
    lq t5, 2224(a0)
    sll r0, r0, 0
    lq t6, 2240(a0)
    sll r0, r0, 0
    lq t7, 3216(a0)
    sll r0, r0, 0
    lq t8, 3232(a0)
    sll r0, r0, 0
    lq t9, 3248(a0)
    sll r0, r0, 0
    lq ra, 2288(a0)
    sll r0, r0, 0
    lq gp, 3360(a0)
    sll r0, r0, 0
    lq s5, 2960(a0)
    sll r0, r0, 0
    lq s4, 3408(a0)
    sll r0, r0, 0
    lq t4, 2976(a0)
    sq t5, 0(t3)
    sll r0, r0, 0
    sq t6, 16(t3)
    sll r0, r0, 0
    sq t7, 32(t3)
    sll r0, r0, 0
    sq t8, 48(t3)
    sll r0, r0, 0
    sq t9, 64(t3)
    sll r0, r0, 0
    sq ra, 80(t3)
    sll r0, r0, 0
    sq gp, 96(t3)
    sll r0, r0, 0
    sq s5, 112(t3)
    sll r0, r0, 0
    sq s4, 128(t3)
    daddiu t2, t2, 10
    sq t4, 144(t3)
    daddiu t3, t3, 160
    sll r0, r0, 0
    lq t5, 2224(a0)
    sll r0, r0, 0
    lq t6, 2240(a0)
    sll r0, r0, 0
    lq t7, 3264(a0)
    sll r0, r0, 0
    lq t8, 3280(a0)
    sll r0, r0, 0
    lq t9, 3296(a0)
    sll r0, r0, 0
    lq ra, 2304(a0)
    sll r0, r0, 0
    lq gp, 3360(a0)
    sll r0, r0, 0
    lq s5, 2992(a0)
    sll r0, r0, 0
    lq s4, 3424(a0)
    sll r0, r0, 0
    lq t4, 3008(a0)
    sq t5, 0(t3)
    sll r0, r0, 0
    sq t6, 16(t3)
    sll r0, r0, 0
    sq t7, 32(t3)
    sll r0, r0, 0
    sq t8, 48(t3)
    sll r0, r0, 0
    sq t9, 64(t3)
    sll r0, r0, 0
    sq ra, 80(t3)
    sll r0, r0, 0
    sq gp, 96(t3)
    sll r0, r0, 0
    sq s5, 112(t3)
    sll r0, r0, 0
    sq s4, 128(t3)
    daddiu t2, t2, 10
    sq t4, 144(t3)
    daddiu t3, t3, 160
    sll r0, r0, 0
    lq t5, 2224(a0)
    sll r0, r0, 0
    lq t6, 2240(a0)
    sll r0, r0, 0
    lq t7, 3312(a0)
    sll r0, r0, 0
    lq t8, 3328(a0)
    sll r0, r0, 0
    lq t9, 3344(a0)
    sll r0, r0, 0
    lq ra, 2320(a0)
    sll r0, r0, 0
    lq gp, 3360(a0)
    sll r0, r0, 0
    lq s5, 3024(a0)
    sll r0, r0, 0
    lq s4, 3440(a0)
    sll r0, r0, 0
    lq t4, 3040(a0)
    sq t5, 0(t3)
    sll r0, r0, 0
    sq t6, 16(t3)
    sll r0, r0, 0
    sq t7, 32(t3)
    sll r0, r0, 0
    sq t8, 48(t3)
    sll r0, r0, 0
    sq t9, 64(t3)
    sll r0, r0, 0
    sq ra, 80(t3)
    sll r0, r0, 0
    sq gp, 96(t3)
    sll r0, r0, 0
    sq s5, 112(t3)
    sll r0, r0, 0
    sq s4, 128(t3)
    daddiu t2, t2, 10
    sq t4, 144(t3)
    daddiu t3, t3, 160
    sll r0, r0, 0
    lq t5, 1776(a0)
    sll r0, r0, 0
    lq t6, 1792(a0)
    sll r0, r0, 0
    lq t7, 2064(a0)
    sll r0, r0, 0
    lq t8, 2080(a0)
    sll r0, r0, 0
    lq t9, 2096(a0)
    sll r0, r0, 0
    lq t4, 2112(a0)
    sq t5, 0(t3)
    sll r0, r0, 0
    sq t6, 16(t3)
    sll r0, r0, 0
    sq t7, 32(t3)
    sll r0, r0, 0
    sq t8, 48(t3)
    sll r0, r0, 0
    sq t9, 64(t3)
    daddiu t2, t2, 6
    sq t4, 80(t3)
    daddiu t3, t3, 96
    sll r0, r0, 0
    daddiu t2, t2, 1
    sqc2 vf29, 0(t3)
    daddiu t3, t3, 16
B18:
L230:
    daddiu a2, a2, 16 ;; advance bucket
    daddiu a3, a3, 32 ;; advange montage
    daddiu t0, t0, -1 ;; decrement remaining bucket count.
    lhu t4, 4(a2)     ;; t4 = this-bucket.count (num tags?)
    daddiu a1, a1, 4  ;; work-offset += 4
    lhu t5, 12(a2)    ;; t5 = this-bucket.count-scissor (ignore)
    bgtz t0, L223     ;; go back
    sll r0, r0, 0


;;;;;;;;;;;;;;;;;;;;;;;; End near texture stuff

B19:
    beq t2, r0, L233
    sll r0, r0, 0

B20:
    lw a1, 8196(a0)
    sll r0, r0, 0
B21:
L231:
    lw a2, 0(a1)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi a2, a2, 256
    sll r0, r0, 0
    beq a2, r0, L232
    sll r0, r0, 0

B22:
    sll r0, r0, 0
    lw a2, 8160(a0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu a2, a2, 1
    sll r0, r0, 0
    sw a2, 8160(a0)
    beq r0, r0, L231
    sll r0, r0, 0

B23:
L232:
    lw a2, 8152(a0)
    sll r0, r0, 0
    sw v1, 128(a1)
    xori a3, v1, 4720
    sw a2, 16(a1)
    sll v1, t2, 4
    addu v1, a2, v1
    or a2, a3, r0
    sw t2, 32(a1)
    addiu a2, r0, 256
    sw a2, 0(a1)
    addiu a1, r0, 0
    sw v1, 8152(a0)
    sll r0, r0, 0
B24:
L233:
    lw v1, 8196(a0)
    sll r0, r0, 0
B25:
L234:
    lw a1, 0(v1)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi a1, a1, 256
    sll r0, r0, 0
    beq a1, r0, L235
    sll r0, r0, 0

B26:
    sll r0, r0, 0
    lw a1, 8160(a0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu a1, a1, 1
    sll r0, r0, 0
    sw a1, 8160(a0)
    beq r0, r0, L234
    sll r0, r0, 0

B27:
L235:
    lw v1, 8152(a0)
    sll r0, r0, 0
    lw a0, 8148(a0)
    sll r0, r0, 0
    sw v1, 4(a0)
    sll r0, r0, 0
    or v1, r0, r0
    or v0, r0, r0
    ld ra, 0(sp)
    lq gp, 48(sp)
    lq s5, 32(sp)
    lq s4, 16(sp)
    jr ra
    daddiu sp, sp, 64

    sll r0, r0, 0
```

# Method 34
```
vf6, vf7, vf8, vf9, vf10 have shader-near.

    sll r0, r0, 0
    sll r0, r0, 0
    lw a2, 4(a1)    ;; a2 = dma ptr
    lui v1, 4096
    sw a1, 8148(a0) ;; stash dma buffer
    sync.l
    cache dxwbin a2, 0
    sync.l
    cache dxwbin a2, 1
    sync.l
    lui a3, 28672
    ori a1, v1, 53248
    vmaxw.xyzw vf11, vf0, vf0
    ori v1, a3, 2720
    sw r0, 1736(a0)
    addiu t0, r0, 0
    sw a1, 8196(a0)
    or t1, v1, r0
    sw a2, 8152(a0)
    sll r0, r0, 0
    lqc2 vf29, 1728(a0) ;; vf29 = ret-tmpl
    or a1, a0, r0       ;; a1 is offset work
    lw a2, 8284(a0)     ;; a2 = work.hfragment.buckets-near
    sll r0, r0, 0
    lhu a3, 8302(a0)    ;; a3 = work.hfragment.num-buckets-near
    beq r0, r0, L195
    lhu t2, 4(a2)       ;; t2 = this-bucket.count

B1:
L188:
    sll r0, r0, 0
    lw t2, 8392(a1)   ;; t2 = near-textures[bucket-idx]
    beq t3, r0, L195  ;; skip if nothing in bucket.
    sll r0, r0, 0

B2:
    sll r0, r0, 0
    sw t2, 1716(a0)  ;; put near-textures in call
    daddiu t2, t0, -226
    sll r0, r0, 0
    blez t2, L191 ;; maybe dma out
    sll r0, r0, 0

B3: ;; dma wait
    lw t2, 8196(a0)
    sll r0, r0, 0
B4:
L189:
    lw t1, 0(t2)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t1, t1, 256
    sll r0, r0, 0
    beq t1, r0, L190
    sll r0, r0, 0

B5:
    sll r0, r0, 0
    lw t1, 8160(a0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu t1, t1, 1
    sll r0, r0, 0
    sw t1, 8160(a0)
    beq r0, r0, L189
    sll r0, r0, 0

B6:
L190: ;; dma swap
    lw t1, 8152(a0)
    sll r0, r0, 0
    sw v1, 128(t2)
    xori v1, v1, 4720
    sw t1, 16(t2)
    sll t3, t0, 4
    addu t3, t1, t3
    or t1, v1, r0
    sw t0, 32(t2)
    addiu t0, r0, 256
    sw t0, 0(t2)
    addiu t0, r0, 0
    sw t3, 8152(a0)
    sll r0, r0, 0
B7:
L191:
    lq t2, 7584(a0)   ;; t2 = call-abort-vu1
    daddiu t0, t0, 1
    sq t2, 0(t1)
    daddiu t2, t1, 16
    daddiu t1, t0, -215
    sll r0, r0, 0
    blez t1, L194
    sll r0, r0, 0

B8:
    lw t1, 8196(a0) ;; dma out of scratchpad if needed
    sll r0, r0, 0
B9:
L192:
    lw t2, 0(t1)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t2, t2, 256
    sll r0, r0, 0
    beq t2, r0, L193
    sll r0, r0, 0

B10:
    sll r0, r0, 0
    lw t2, 8160(a0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu t2, t2, 1
    sll r0, r0, 0
    sw t2, 8160(a0)
    beq r0, r0, L192
    sll r0, r0, 0

B11:
L193: ;; swap dma buffer
    lw t2, 8152(a0)
    sll r0, r0, 0
    sw v1, 128(t1)
    xori v1, v1, 4720
    sw t2, 16(t1)
    sll t3, t0, 4
    addu t3, t2, t3
    or t2, v1, r0
    sw t0, 32(t1)
    addiu t0, r0, 256
    sw t0, 0(t1)
    addiu t0, r0, 0
    sw t3, 8152(a0)
    sll r0, r0, 0
B12:
L194:
    sll r0, r0, 0
    lqc2 vf30, 1712(a0) ;; vf30 = call
    sll r0, r0, 0
    daddiu t0, t0, 1
    sqc2 vf30, 0(t2)
    daddiu t1, t2, 16
    sll r0, r0, 0
    lq t2, 2160(a0) ;; t2 = adgif2[0]
    sll r0, r0, 0
    lq t3, 2176(a0) ;; t3 = adgif2[1]
    sq t2, 0(t1)
    sll r0, r0, 0
    sq t3, 16(t1)
    sll r0, r0, 0

    sqc2 vf6, 32(t1)
    sll r0, r0, 0
    sqc2 vf7, 48(t1)
    sll r0, r0, 0
    sqc2 vf8, 64(t1)
    sll r0, r0, 0
    sqc2 vf9, 80(t1)
    sll r0, r0, 0
    sqc2 vf10, 96(t1)
    daddiu t0, t0, 8
    sqc2 vf5, 112(t1) ;; plus the texflush!
    daddiu t1, t1, 128
    sll r0, r0, 0
    lh t3, 6(a2) ;; t3 = vertex count
    sll r0, r0, 0
    lw t2, 0(a2) ;; t2 = bucket.next (vertex uploads for bucket)
    dsll t4, t3, 1 ;; t4 = vertex-count * 2
    sll r0, r0, 0
    daddu t3, t4, t3 ;; t3 = vertex-count * 3 = 48 or 96
    sll r0, r0, 0
    daddu t5, t3, a0
    sll r0, r0, 0
    sll r0, r0, 0
    lq t3, 1552(t5) ;; 1648 or 1600
    sll r0, r0, 0
    lq t4, 1568(t5)
    sll r0, r0, 0
    lq t5, 1584(t5)
    sq t3, 0(t1)
    sll r0, r0, 0
    sq t4, 16(t1)
    sll r0, r0, 0
    sq t5, 32(t1)
    daddiu t0, t0, 3
    sw t2, 4(t1)
    daddiu t1, t1, 48
B13:
L195:
    daddiu a2, a2, 16 ;; inc bucket
    daddiu a1, a1, 4  ;; inc offset work
    daddiu a3, a3, -1  ;; dec bucket count
    lhu t3, 4(a2)     ;; load count in bucket
    bgtz a3, L188     ;; go back
    sll r0, r0, 0

B14:
    beq t0, r0, L198
    sll r0, r0, 0

B15:
    lw a1, 8196(a0)
    sll r0, r0, 0
B16:
L196:
    lw a2, 0(a1)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi a2, a2, 256
    sll r0, r0, 0
    beq a2, r0, L197
    sll r0, r0, 0

B17:
    sll r0, r0, 0
    lw a2, 8160(a0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu a2, a2, 1
    sll r0, r0, 0
    sw a2, 8160(a0)
    beq r0, r0, L196
    sll r0, r0, 0

B18:
L197:
    lw a2, 8152(a0)
    sll r0, r0, 0
    sw v1, 128(a1)
    xori a3, v1, 4720
    sw a2, 16(a1)
    sll v1, t0, 4
    addu v1, a2, v1
    or a2, a3, r0
    sw t0, 32(a1)
    addiu a2, r0, 256
    sw a2, 0(a1)
    addiu a1, r0, 0
    sw v1, 8152(a0)
    sll r0, r0, 0
B19:
L198:
    lw v1, 8196(a0)
    sll r0, r0, 0
B20:
L199:
    lw a1, 0(v1)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi a1, a1, 256
    sll r0, r0, 0
    beq a1, r0, L200
    sll r0, r0, 0

B21:
    sll r0, r0, 0
    lw a1, 8160(a0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu a1, a1, 1
    sll r0, r0, 0
    sw a1, 8160(a0)
    beq r0, r0, L199
    sll r0, r0, 0

B22:
L200:
    lw v1, 8152(a0)
    sll r0, r0, 0
    lw a0, 8148(a0)
    sll r0, r0, 0
    sw v1, 4(a0)
    sll r0, r0, 0
    or v0, r0, r0
    jr ra
    daddu sp, sp, r0

```