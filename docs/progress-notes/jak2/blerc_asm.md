```
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; .function setup-blerc-chains-for-one-fragment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setup-blerc-chains-for-one-fragment
    a0: num-targets
    a1: blend-shape-coeffs (s16 array)
    a2: dma-mem-ptr
    a3: blend-data
    t0: blend-ctrl
    t1: geo-data)

B0:
L9:
    daddiu sp, sp, -128
    sd ra, 0(sp)
    sq s0, 16(sp)
    sq s1, 32(sp)
    sq s2, 48(sp)
    sq s3, 64(sp)
    sq s4, 80(sp)
    sq s5, 96(sp)
    sq gp, 112(sp)
    lb v1, 0(t0)                ;; v1 = blend-vtx-count
    addiu t2, r0, 0             ;; t2 = 0
    por t7, r0, r0              ;; t7 = 0
    lw t3, *blerc-globals*(s7)  ;; t3 = *blerc-globals*
    lw t3, 4(t3)                ;; t3 = blec-globals.next
    or t4, v1, r0               ;; t4 = blend-vtx-count.
    ;; link
B1:
L10:
    lui t7, 4096
    sll r0, r0, 0
    daddiu t7, t7, 1
    sll r0, r0, 0
    beq t3, r0, L11
    sq t7, 0(a2)

B2:
    sw a2, 12(t3)
    sll r0, r0, 0
B3:
L11:
    or t3, a2, r0
    daddu t6, t4, t4
    daddu t5, v1, v1
    daddu t6, t6, t4
    daddu t7, t5, v1
    daddu t5, t6, t6
    daddu t6, t7, t7
    daddu t7, t5, t5
    daddiu t6, t6, 15
    daddiu t5, t5, 15
    andi t6, t6, 65520
    dsrl t5, t5, 4
    daddu t8, t2, t2
    daddiu t7, t7, 15
    daddu t9, t8, t2
    dsrl t8, t7, 4
    daddu ra, t9, t9
    addiu t9, r0, 0
    daddu t7, ra, ra
    daddiu s3, a2, 32
    daddu s2, ra, a3
    daddu ra, t7, t1
    lui t7, 12288
    daddiu gp, a0, -1
    daddu t7, t7, t5
    or s5, a1, r0
    sq t7, 0(s3)
    daddiu s4, t0, 2
    sw s2, 4(s3)
    daddu s2, s2, t6
    daddiu s3, s3, 16
    sll r0, r0, 0
B4:
L12:
    lb s1, 0(s4)
    daddiu s4, s4, 1
    lh s0, 0(s5)
    daddiu s5, s5, 2
    beq s1, r0, L13
    sq t7, 0(s3)

B5:
    sw s2, 4(s3)
    daddu s2, s2, t6
    beq s0, r0, L13
    sw s0, 12(s3)

B6:
    daddiu s3, s3, 16
    daddiu t9, t9, 1
B7:
L13:
    bne gp, r0, L12
    daddiu gp, gp, -1

B8:
    sq t7, 0(s3)
    por t6, r0, r0
    sw ra, 4(s3)
    lui t6, 28672
    sb t8, 0(s3)
    sll r0, r0, 0
    sq t6, 16(s3)
    daddiu t6, s3, 32
    sw t9, 20(a2)
    sll r0, r0, 0
    sw t4, 16(a2)
    sll r0, r0, 0
    sw ra, 24(a2)
    sll r0, r0, 0
    sw t8, 28(a2)
    sll r0, r0, 0
    bne t4, v1, L14
    daddiu t5, t5, 1

B9:
    daddiu t7, t9, 3
    multu3 t5, t5, t7
    daddiu t5, t5, -457
    sll r0, r0, 0
    blez t5, L14
    sll r0, r0, 0

B10:
    beq r0, r0, L10
    addiu t4, r0, 24

B11:
L14:
    or a2, t6, r0
    daddu t2, t2, t4
    beq t2, v1, L15
    daddu t5, t2, t4

B12:
    dsubu t5, t5, v1
    sll r0, r0, 0
    blez t5, L10
    sll r0, r0, 0

B13:
    beq r0, r0, L10
    dsubu t4, v1, t2

B14:
L15:
    lw v1, *blerc-globals*(s7)
    sw t3, 4(v1)
    or v0, a2, r0
    ld ra, 0(sp)
    lq gp, 112(sp)
    lq s5, 96(sp)
    lq s4, 80(sp)
    lq s3, 64(sp)
    lq s2, 48(sp)
    lq s1, 32(sp)
    lq s0, 16(sp)
    jr ra
    daddiu sp, sp, 128



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; .function blerc-execute
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;BAD PROLOGUE
;; Warnings:
;; INFO: Flagged as mips2c by config
;; INFO: Assembly Function

B0:
L37:
    daddiu sp, sp, -96
    sd ra, 0(sp)
    sq s2, 16(sp)
    sq s3, 32(sp)
    sq s4, 48(sp)
    sq s5, 64(sp)
    sq gp, 80(sp)
    lw v1, *blerc-globals*(s7)
    lwu s5, 0(v1)
    beq s5, r0, L56
    or v1, s7, r0

B1:
    addiu v1, r0, 0
    addiu gp, r0, 0
    addiu v1, r0, 0
    lw v1, *gsf-buffer*(s7)
    lw t9, flush-cache(s7)
    addiu a0, r0, 0
    jalr ra, t9
    sll v0, ra, 0

    addiu v1, r0, 848
    lui a0, 28672
    daddu a1, v1, a0
    lw v1, *blerc-globals*(s7)
    daddu v1, r0, v1
    sll r0, r0, 0
    lui a0, 4096
    sll r0, r0, 0
    ori a0, a0, 54272
    andi a1, a1, 16383
B2:
L38:
    lw a2, 0(a0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi a2, a2, 256
    sll r0, r0, 0
    bne a2, r0, L38
    lw a2, 0(v1)

B3:
    beq a2, r0, L39
    sw a1, 128(a0)

B4:
    addiu v1, r0, 324
    sw a2, 48(a0)
    sw r0, 32(a0)
    sync.l
    sw v1, 0(a0)
    sync.l
B5:
L39:
    or v1, r0, r0
    beq r0, r0, L55
    sll r0, r0, 0

B6:
L40:
    bne gp, r0, L41
    sll r0, r0, 0

B7:
    lui v1, 28672
    daddu a0, r0, v1
    beq r0, r0, L42
    sll r0, r0, 0

B8:
L41:
    addiu v1, r0, 8192
    lui a0, 28672
    daddu a0, v1, a0
B9:
L42:
    bne gp, r0, L43
    sll r0, r0, 0

B10:
    addiu v1, r0, 8192
    lui a1, 28672
    daddu a1, v1, a1
    beq r0, r0, L44
    sll r0, r0, 0

B11:
L43:
    lui v1, 28672
    daddu a1, r0, v1
B12:
L44:
    daddiu v1, a0, 848
    daddu a2, r0, a0
    daddiu a3, a1, 848
    daddiu a1, v1, 12
    sll r0, r0, 0
    lui a2, 4096
    sll r0, r0, 0
    ori a2, a2, 54272
    andi a3, a3, 16383
B13:
L45:
    lw t0, 0(a2)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t0, t0, 256
    sll r0, r0, 0
    bne t0, r0, L45
    lw t0, 0(a1)

B14:
    beq t0, r0, L46
    sw a3, 128(a2)

B15:
    addiu a1, r0, 324
    sw t0, 48(a2)
    sw r0, 32(a2)
    sync.l
    sw a1, 0(a2)
    sync.l
B16:
L46:
 ;; blend ctrl
 ;; blend data
 ;; coeffs.
 ;;
    or a1, r0, r0
    or a2, a0, r0              ;; likely the blerc-block.
    lw a3, *gsf-buffer*(s7)
    daddiu t2, a2, 880         ;; t2 is whatever's after blerc-block
    lb t1, 0(t2)               ;; size of whatever's after
    sll r0, r0, 0
    or t0, a2, r0              ;; t0 is the blerc block
    lw a1, 868(a2)             ;; a1 = block.header.overlap
    daddiu t3, t1, 1           ;; some counts??
    or t1, a3, r0              ;; t1 = ee buffer.
    sll t8, t3, 4              ;; t8 = some count (stride)
    sll t3, a1, 4              ;; t3 = overlap * 16
    daddu t9, t3, a3           ;; t9 = &buffer[overlap] (qws)
    daddu t3, t2, t8           ;; t3 = &dummy[unk_count + 1] (qws)
    beq a1, r0, L49
    daddiu ra, t2, 16          ;; ra = after block ptr (+1 qw to skip a header I guess)

   ;; this block expands "overlap"s to
B17:
    lh t5, 12(t3)
    daddu t2, t3, t8
    beq r0, r0, L48
    sll r0, r0, 0

B18:
L47:
    lh t5, 12(t2)
    daddu t2, t2, t8
    sq t6, 0(t1)
    daddiu t1, t1, 16
B19:
L48:
    pcpyh t5, t5
    mfc1 r0, f31
    bne t1, t9, L47
    pcpyld t6, t5, t5

B20:
    dsubu t3, t2, t8
    sll r0, r0, 0
B21:
L49:
    addiu t1, r0, 255 ;; cs
    addiu t2, r0, 8192 ;; cs
    lb s5, 0(t3)
    daddiu s4, t3, 16
    pcpyh t1, t1
    mfc1 r0, f31
    pcpyld t1, t1, t1
    mfc1 r0, f31
    pcpyh t2, t2
    mfc1 r0, f31
    pcpyld t2, t2, t2 ;; t2 = 8192's
    mfc1 r0, f31
    por t3, t1, r0    ;; t3 = 255's
    mfc1 r0, f31
    por t4, r0, r0
    mfc1 r0, f31
B22:
L50:
    ld t6, 0(ra)
    daddu s2, ra, t8
    daddiu ra, ra, 8
    or s3, a3, r0
    pextlb t6, r0, t6
    mfc1 r0, f31
    pmulth t7, t6, t2
    ld t5, 0(s2)
    daddiu s5, s5, -1
    sll r0, r0, 0
    beq r0, r0, L52
    daddu s2, s2, t8

B23:
L51:
    pmaddh t7, t5, t6
    ld t5, 0(s2)
    daddu s2, s2, t8
    daddiu s3, s3, 16
B24:
L52:
    lq t6, 0(s3)
    pextlb t5, t5, r0
    bne s3, t9, L51
    psrah t5, t5, 8

B25:
    pmfhl.uw t5
    mfc1 r0, f31
    psraw t7, t7, 13
    mfc1 r0, f31
    psraw t5, t5, 13
    mfc1 r0, f31
    pinteh t5, t5, t7
    mfc1 r0, f31
    pminh t3, t3, t5
    mfc1 r0, f31
    pmaxh t4, t4, t5
    mfc1 r0, f31
    pminh t5, t5, t1
    mfc1 r0, f31
    pmaxh t5, t5, r0
    lq t7, 0(s4)
    ppacb t5, r0, t5
    mfc1 r0, f31
    ppach t7, r0, t7
    mfc1 r0, f31
    pextlh t5, t5, t7
    mfc1 r0, f31
    sq t5, 0(t0)
    daddiu t0, t0, 16
    bne s5, r0, L50
    daddiu s4, s4, 16

B26:
    lw a3, *stats-blerc*(s7)
    beq a3, s7, L53
    lw a3, *blerc-globals*(s7)

B27:
    lw t2, 12(a3)
    lw t1, 16(a3)
    lw t0, 20(a3)
    lw a2, 864(a2)
    multu3 a1, a1, a2
    daddiu t2, t2, 1
    daddu a2, t1, a2
    daddu a1, t0, a1
    sw t2, 12(a3)
    sw a2, 16(a3)
    sw a1, 20(a3)
    pcpyud a1, t3, r0
    pminh t3, t3, a1
    dsrl32 a1, t3, 0
    pminh t3, t3, a1
    dsrl t3, t3, 16
    lh a1, 8(a3)
    pminh t3, t3, a1
    sh t3, 8(a3)
    pcpyud a1, t4, r0
    pmaxh t4, t4, a1
    dsrl32 a1, t4, 0
    pmaxh t4, t4, a1
    dsrl t4, t4, 16
    lh a1, 10(a3)
    pmaxh t4, t4, a1
    sh t4, 10(a3)
B28:
L53:
    or a1, r0, r0
    lwu a1, 872(a0)
    or a3, a0, r0
    lwu a0, 876(a0)
    lui a2, 4096
    sll r0, r0, 0
    ori a2, a2, 53248
    andi a3, a3, 16383
B29:
L54:
    lw t0, 0(a2)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t0, t0, 256
    sll r0, r0, 0
    bne t0, r0, L54
    sll r0, r0, 0

B30:
    sw a3, 128(a2)
    addiu a3, r0, 256
    sw a1, 16(a2)
    sll r0, r0, 0
    sw a0, 32(a2)
    sync.l
    sw a3, 0(a2)
    sync.l
    or a0, r0, r0
    addiu a0, r0, 1
    dsubu gp, a0, gp
    lwu s5, 12(v1)
    or v1, s5, r0
B31:
L55:
    bne s5, r0, L40
    sll r0, r0, 0

B32:
    or v1, s7, r0
B33:
L56:
    or v0, r0, r0
    ld ra, 0(sp)
    lq gp, 80(sp)
    lq s5, 64(sp)
    lq s4, 48(sp)
    lq s3, 32(sp)
    lq s2, 16(sp)
    jr ra
    daddiu sp, sp, 96

    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
```