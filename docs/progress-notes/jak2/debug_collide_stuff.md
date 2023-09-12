    .type collide-hash
L22131:
    .word 0x5480548    ;; 4
    .word 0xae9        ;; 8
    .word 0xb          ;; 12
    .word 0x47be7bfd   ;; 16
    .word 0x47bcc8a3   ;; 20
    .word 0x47bd026c   ;; 24
    .word 0x3f800000   ;; 28
    .word 0xc95925e7   ;; 32
    .word 0xc82e2b0c   ;; 36
    .word 0xc9492279   ;; 40
    .word L22132       ;; 44  bucket-array
    .word 0x372c064f   ;; 48
    .word 0x372d9303   ;; 52
    .word 0x372d5df3   ;; 56
    .word L22133       ;; 60 item-array
    .word 0xfff26da2
    .word 0xfffd4754
    .word 0xfff36dd9
    .word 0x130715
    .word 0x11adf9
    .word 0x79a4c
    .word 0xf7c34
    .word 0x330c



    ;; inline array of collide-hash-item in item-array

L22133:
    .word 0x4d3
    .word L21546
    .word 0x4d3
    .word L21546
    .word 0x4d3
    .word L21546
    .word 0x4ca
    .word L21501
    .word 0x4ca
    .word L21501
    .word 0x4ca
    .word L21501
    .word 0x4d3
    .word L21546


    .type collide-hash-fragment
L21546:
    .word 0x30400fc   ;; 4
    .word L22409      ;; 8 (pat-array)
    .word L21547      ;; 12 (bucket-array)
    .word 0x48a200a6  ;; 16
    .word 0x464c1252  ;; 20
    .word 0xc91c8848  ;; 24
    .word 0x47d7e29f  ;; 28
    .word 0x46ca40ac  ;; 32
    .word 0x4635bb09  ;; 36
    .word 0x470044c3  ;; 40
    .word 0x60706     ;; 44
    .word 0x486ff875  ;; 48
    .word 0xc4491e71  ;; 52
    .word 0xc932f9ec  ;; 56
    .word 0xf8f800dc  ;; 60  <- f8 in here (off 2) is poly array size.
    .word 0x382203e4  ;; 64
    .word 0x38b44f94  ;; 68
    .word 0x37ff76c4  ;; 72
    .word L21548      ;; 76 - poly array (248 words)
    .word 0x3bfe1     ;; 80
    .word 0xfffffcdc  ;; 84
    .word 0xfff4d062  ;; 88
    .word L21549      ;; 92 - vert array (330 words)
    .word 0x61ea3     ;;
    .word 0x13ae2
    .word 0xfff7d1fe
    .word L21550      ;; 108 - index-array

    lw v1, 40(a1)      ;; v1 = frag.dimension_array[0]
    lqc2 vf1, 44(a1)   ;; vf1 = frag.bbox.min
    lui a3, 1          ;; a3 = 0x10000
    lqc2 vf2, 128(a2)  ;; vf2 = query.bbox.min
    ori a3, a3, 257    ;; a3 = 0x010101
    lqc2 vf3, 144(a2)  ;; vf3 = query.bbox.max
    dsubu v1, v1, a3   ;; subtract 1 from each value in dimension array.
    lqc2 vf6, 60(a1)   ;; vf6 = frag.axis_scale
    vsub.xyzw vf2, vf2, vf1  ;; vf2 = query.bbox.min - frag.bbox.min
    lq a3, 160(a2)           ;; a3 = query.bbox.min.i
    vsub.xyzw vf3, vf3, vf1  ;; vf3 = query.bbox.max - frag.bbox.min
    lq t0, 176(a2)           ;; t0 = query.bbox.max.i
    pextlb v1, r0, v1        ;; byte -> halfword for (dim - 1)
    lq t1, 76(a1)            ;; t1 = frag.bbox.min.i
    pextlh v1, r0, v1        ;; halfword -> word for (dim - 1)
    lq t2, 92(a1)            ;; t2 = frag.bbox.max.i

   Determine if this fragment is entirely outside the query on any axis.
   we'll set bits in t0 if so.

    pcgtw t0, t1, t0         ;; t0 = (frag.bbox.min.i > query.bbox.max.i)
    vftoi0.xyzw vf4, vf2     ;; vf4 = int(query.bbox.min - frag.bbox.min)
    pcgtw a3, a3, t2         ;; a3 = (query.bbox.min.i > frag.bbox.max.i)
    vftoi0.xyzw vf5, vf3     ;; vf5 = int(query.bbox.max - frag.bbox.min)
    por a3, t0, a3           ;; combine min/max tests into a3.
    vmul.xyzw vf2, vf2, vf6  ;; vf2 = frag.axis_scale * (query.bbox.min - frag.bbox.min)
    ppach a3, r0, a3
    vmul.xyzw vf3, vf3, vf6  ;; vf3 = frag.axis_scale * (query.bbox.max - frag.bbox.min)
    dsll t0, a3, 16
    qmfc2.i a3, vf4          ;; a3 = int(query.bbox.min - frag.bbox.min)
    bne t0, r0, L75
    qmfc2.i t0, vf5          ;; t0 = int(query.bbox.max - frag.bbox.min)

B1:
    vftoi0.xyzw vf2, vf2     ;; vf2 = int(frag.axis_scale * (query.bbox.min - frag.bbox.min))
    psraw a3, a3, 4          ;; a3 = int(query.bbox.min - frag.bbox.min) >> 4
    vftoi0.xyzw vf3, vf3     ;; vf3 = int(frag.axis_scale * (query.bbox.max - frag.bbox.min))
    psraw t0, t0, 4          ;; t0 = int(query.bbox.max - frag.bbox.min) >> 4

    sq a3, 368(a2)       ;; query.local_box4w.min = a3 = int(query.bbox.min - frag.bbox.min) >> 4
    sq t0, 384(a2)       ;; query.local_box4w.max = t0 = int(query.bbox.max - frag.bbox.min) >> 4

    qmfc2.i t0, vf2    ;; t0 = int(frag.axis_scale * (query.bbox.min - frag.bbox.min))

    qmfc2.i a3, vf3    ;; a3 = int(frag.axis_scale * (query.bbox.max - frag.bbox.min))
    pmaxw t0, t0, r0   ;; t0 = max(0, int(frag.axis_scale * (query.bbox.min - frag.bbox.min)))
    lqc2 vf10, 448(a2) ;; vf10 = mat_0
    pmaxw a3, a3, r0   ;; a3 = max(0, int(frag.axis_scale * (query.bbox.max - frag.bbox.min)))
    lqc2 vf11, 464(a2) ;; vf11 = mat_1
    pminw t0, t0, v1   ;; t0 = min(dim-1, max(0, int(frag.axis_scale * (query.bbox.min - frag.bbox.min))))
    lqc2 vf12, 480(a2) ;; vf12 = mat_2
    pminw v1, a3, v1   ;; v1 = min(dim-1, max(0, int(frag.axis_scale * (query.bbox.max - frag.bbox.min))))
    lqc2 vf13, 496(a2) ;; vf13 = mat_3
    sq t0, 400(a2)     ;; search_box.min = t0
    sq v1, 416(a2)     ;; search_box.max = v1

    addiu v1, r0, 4    ;; v1 = 4
    lbu a3, 40(a1)     ;; a3 = dim_array.x
    multu3 t8, a3, v1  ;; t8 = dim_array.x * 4
    lbu a3, 42(a1)     ;; a3 = dim_array.z
    multu3 t6, a3, t8  ;; t6 = dim_array.z * dim_array.x * 4
    addiu a3, r0, 1    ;; a3 = 1
    lw t0, 400(a2)     ;; t0 = search_box.min.x
    dsubu a3, a3, t0   ;; a3 = 1 - search_box.min.x
    lw t0, 416(a2)     ;; t0 = search_box.max.x
    daddu a3, a3, t0   ;; a3 = 1 + search_box_max.x - search_box.min.x
    addiu t0, r0, 1    ;; t0 = 1
    lw t1, 404(a2)     ;; t1 = search_box.min.y
    dsubu t0, t0, t1   ;; t0 = 1 - search_box.min.y
    lw t1, 420(a2)     ;; t1 = search_box.max.y
    daddu t1, t0, t1   ;; t1 = 1 + search_box.max.y - search_box.min.y
    addiu t0, r0, 1    ;; t0 = 1
    lw t2, 408(a2)     ;; t2 = search_box.min.z
    dsubu t0, t0, t2   ;; t0 = 1 - search_box.min.z
    lw t2, 424(a2)     ;; t2 = search_box.max.z
    daddu t0, t0, t2   ;; t0 = 1 + search_box.max.z - search_box.min.z
    lwu t2, 8(a1)      ;; t2 = frag.bucket_array
    lw t3, 400(a2)     ;; t3 = search_box.min.x
    mult3 t3, t3, v1   ;; t3 = search_box.min.x * 4
    lw t4, 404(a2)     ;; t4 = search_box.min.y
    mult3 t4, t4, t6   ;; t4 = search_box.min.y * dim_array.z * dim_array.x * 4
    daddu t3, t3, t4   ;; t3 = search_box.min.y * dim_array.z * dim_array.x * 4 + search_box.min.x * 4
    lw t4, 408(a2)     ;; t4 = search_box.min.z
    mult3 t4, t4, t8   ;; t4 = dim_array.x * 4 * search_box.min.z
    daddu t3, t3, t4   ;; t3 = 4 * search_box.min.x
                       ;;    + 4 * dim_array.x * search_box.min.z
                       ;;    + 4 * dim_array.z * dim_array.x * search_box.min.y
    daddu t7, t2, t3   ;; t7 = bucket ptr.
    or t1, t1, r0      ;; t1 = y remaining...
    sll r0, r0, 0
  ////////// CELL ITERATION : OUTER LOOP OVER Y
B2:
L68:
    or t5, t0, r0   ;; t5 = reset z remaining count
    or t9, t7, r0   ;; t9 = bucket ptr for starting this y loop.
    sw t7, 524(a2)  ;; y_addr = bucket ptr (stash)
    sw t6, 528(a2)  ;; y_step stash
  ////// MIDDLE LOOP OVER Z
B3:
L69:
    or t6, a3, r0  ;; t6 = reset x remaining count
    or t7, t9, r0  ;; t7 = bucket ptr for starting this x loop
    sw t9, 532(a2) ;; stash z addr
    sw t8, 536(a2) ;; stash z step
  ////////// INNER LOOP OVER X
B4:
L70:
    lhu ra, 0(t7)     ;; ra = bucket-index
    lw gp, 104(a1)    ;; gp = frag.index_array
    lhu t8, 2(t7)     ;; t8 = bucket-count
    lw t9, 72(a1)     ;; t9 = poly-array
    beq t8, r0, L74
    daddu ra, ra, gp  ;; ptr to index list for this bucket

  ////////// INNER LOOP OVER THINGS IN THE BUCKET.
B5:
L71:
    lui gp, 28672        ;; gp = scratchpad
    lbu s4, 0(ra)        ;; s4 = read the index list!
    addiu s3, r0, 1      ;; s3 = 1
    andi s2, s4, 7       ;; s2 = (index & 7)
    sra s5, s4, 3        ;; s5 = index / 8
    sll s4, s4, 2        ;; s4 = index * 4
    daddu s5, s5, gp     ;; s5 = (scratchpad + index / 8)
    daddu s4, s4, t9     ;; s4 = poly-ptr (each poly 4 bytes)
    sllv s3, s3, s2      ;; s3 = shifted bit for this poly in bit array
    lb s2, 0(s5)         ;; s2 = old byte in the bit array
    lbu s0, 3(s4)        ;; s0 = poly pat index
    and s1, s2, s3       ;; check bit in byte for seen this poly
    daddiu t8, t8, -1    ;; decrement poly remaining in bucket count
    bne s1, r0, L73      ;; check if we've seen it before
    lw s1, 4(a1)         ;; pat array

B6:
    dsll s0, s0, 2       ;; pat idx * 4
    or s3, s2, s3        ;; set bit for seen this poly
    daddu s2, s0, s1     ;; pat ptr
    sb s3, 0(s5)         ;; store byte for seen this poly
    lwu s5, 0(s2)        ;; s5 = pat
    lwu s2, 96(a2)       ;; ignore pat
    lw s3, 88(a1)        ;; s3 = vert array
    and s2, s2, s5       ;; and pat
    lbu s1, 0(s4)        ;; load vert 0 index
    bne s2, r0, L73      ;; skip if pat miss
    lbu s2, 1(s4)        ;; load vert1 index

B7:
    sll s1, s1, 1        ;; s1 = vi * 2
    lbu s4, 2(s4)
    sll s0, s1, 1        ;; s0 = vi * 2 * 2 = vi * 4
    sll s2, s2, 1
    daddu s1, s1, s0     ;; s1 = vi * 6
    sll s0, s2, 1
    sll s4, s4, 1
    daddu s0, s2, s0
    sll v0, s4, 1
    daddu s2, s1, s3     ;; add to vert table
    daddu s4, s4, v0
    daddu s1, s0, s3
    daddu s4, s4, s3
    ldr t2, 0(s2)        ;; load vert
    sll r0, r0, 0
    ldl t2, 7(s2)
    sll r0, r0, 0
    ldr t3, 0(s1)
    sll r0, r0, 0
    ldl t3, 7(s1)
    sll r0, r0, 0
    ldr t4, 0(s4)
    pextlh t2, r0, t2   ;; extend to 32bits
    ldl t4, 7(s4)
    pextlh t3, r0, t3
    lq s4, 368(a2)
    pextlh t4, r0, t4
    lq s3, 384(a2)
    pminw s1, t2, t3
    mfc1 r0, f31
    pmaxw s2, t2, t3
    mfc1 r0, f31
    pminw s1, s1, t4
    mfc1 r0, f31
    pmaxw s2, s2, t4
    mfc1 r0, f31
    pcgtw s3, s1, s3
    mfc1 r0, f31
    pcgtw s4, s4, s2
    mfc1 r0, f31
    por s4, s3, s4
    mfc1 r0, f31
    ppach s4, r0, s4
    mfc1 r0, f31
    dsll s4, s4, 16
    sll r0, r0, 0
    bne s4, r0, L73
    sll r0, r0, 0

B8:
    psllw t2, t2, 4 ;; multiply by 16
    lw s4, 2048(gp)
    psllw t3, t3, 4
    qmtc2.i vf7, t2  ;; vert
    psllw t4, t4, 4
    qmtc2.i vf8, t3
    sll r0, r0, 0
    qmtc2.i vf9, t4
    vitof0.xyzw vf7, vf7 ;; to float
    daddiu s4, s4, 1
    vitof0.xyzw vf8, vf8
    sw s4, 2048(gp)
    vitof0.xyzw vf9, vf9
    sll r0, r0, 0
    vadd.xyzw vf7, vf7, vf1 ;; add frag.bbox.min.
    sll r0, r0, 0
    vadd.xyzw vf8, vf8, vf1
    lwu gp, 512(a2)
    vadd.xyzw vf9, vf9, vf1
    sll r0, r0, 0
    beq gp, s7, L72
    sll r0, r0, 0

B9:
    vmulax.xyzw acc, vf10, vf7
    sll r0, r0, 0
    vmadday.xyzw acc, vf11, vf7
    sll r0, r0, 0
    vmaddaz.xyzw acc, vf12, vf7
    sll r0, r0, 0
    vmaddw.xyzw vf7, vf13, vf0
    sll r0, r0, 0
    vmulax.xyzw acc, vf10, vf8
    sll r0, r0, 0
    vmadday.xyzw acc, vf11, vf8
    sll r0, r0, 0
    vmaddaz.xyzw acc, vf12, vf8
    sll r0, r0, 0
    vmaddw.xyzw vf8, vf13, vf0
    sll r0, r0, 0
    vmulax.xyzw acc, vf10, vf9
    sll r0, r0, 0
    vmadday.xyzw acc, vf11, vf9
    sll r0, r0, 0
    vmaddaz.xyzw acc, vf12, vf9
    sll r0, r0, 0
    vmaddw.xyzw vf9, vf13, vf0
    sll r0, r0, 0
B10:
L72:
    pextlw gp, gp, s5
    lw s4, 0(a0)
    sll r0, r0, 0
    daddiu s5, a0, 4908
    daddiu s3, s4, -460
    sll r0, r0, 0
    bgez s3, L76
    dsll s3, s4, 6

B11:
    daddiu s4, s4, 1
    daddu s5, s5, s3
    sll r0, r0, 0
    sw s4, 0(a0)
    sll r0, r0, 0
    sq gp, 48(s5)
    sll r0, r0, 0
    sqc2 vf7, 0(s5)
    sll r0, r0, 0
    sqc2 vf8, 16(s5)
    sll r0, r0, 0
    sqc2 vf9, 32(s5)
B12:
L73:
    bgtz t8, L71
    daddiu ra, ra, 1

B13:
L74:
    daddiu t6, t6, -1
    sll r0, r0, 0
    bne t6, r0, L70
    daddu t7, t7, v1

B14:
    sll r0, r0, 0
    lw t6, 532(a2)
    sll r0, r0, 0
    lw t8, 536(a2)
    daddiu t5, t5, -1
    sll r0, r0, 0
    bne t5, r0, L69
    daddu t9, t6, t8

B15:
    sll r0, r0, 0
    lw t5, 524(a2)
    sll r0, r0, 0
    lw t6, 528(a2)
    daddiu t1, t1, -1
    sll r0, r0, 0
    bne t1, r0, L68
    daddu t7, t5, t6

B16:
    lw v1, *collide-stats*(s7)
    lwu v1, 12(v1)
    daddiu v1, v1, 1
    lw a0, *collide-stats*(s7)
    sw v1, 12(a0)
B17:
L75:
    or v0, s7, r0
    beq r0, r0, L78
    sll r0, r0, 0

B18:
L76:
    daddiu v1, s7, debug
    lw a0, *cheat-mode*(s7)
    bne a0, v1, L77
    or v1, s7, r0

B19:
    lw t9, print-exceeded-max-cache-tris(s7)
    jalr ra, t9
    sll v0, ra, 0

    or v1, v0, r0
B20:
L77:
    or v0, r0, r0