;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; .function draw-inline-array-instance-tie
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;BAD PROLOGUE
;; Warnings:
;; INFO: Flagged as asm by config
;; INFO: Assembly Function

# this is yet-another insane double-buffered DMA function.
# it tries to keep a SPR to or SPR from DMA going always, while processing other stuff
# on the Scratch Pad.

# args are:
#  a0 : vis bits
#  a1 : instances
#  a2 : num instances
#  a3 : output dma buf.


# common vars:
#  t0 work
#  t1 SPR to DMA bank
#  a3 SPR from DMA bank

# t5 *wind-work*
# t2 bank ptr
# a1 out ptr

B0:
L137:
    daddiu sp, sp, -128
    sd ra, 0(sp)
    sq s0, 16(sp)
    sq s1, 32(sp)
    sq s2, 48(sp)
    sq s3, 64(sp)
    sq s4, 80(sp)
    sq s5, 96(sp)
    sq gp, 112(sp)

# Initialization block.
# On exit, we set t6 to the first 32-bits of vis data.

    lui t4, 28672    # = 0x7000
    lw v1, 4(a3)     # = dma buf base
    lui t1, 4096     # = 0x1000
    lui t2, 4096     # = 0x1000

    sync.l
    cache dxwbin v1, 0
    sync.l
    cache dxwbin v1, 1
    sync.l

    lw t0, *instance-tie-work-copy*(s7)  # use the work copy.
    ori t1, t1, 54272                    # SPR TO
    sw a3, 396(t0)                       # store dma-buffer
    ori a3, t2, 53248                    # SPR FROM
    lw t5, *wind-work*(s7)
    lw t6, 0(a0)                 # load first vis bits (32 bits)
    ori t2, t4, 16               # spr + 16 (bank ptr?)
    vmaxw.xyzw vf1, vf0, vf0     # vf1 = (1, 1, 1, 1)
    addiu t3, a1, -4             # remove basic ptr.
    ori a1, t4, 4112             # set out ptr
    sw t1, 400(t0)               # stash to-spr
    addiu t9, r0, 0              # t9 = 0
    sw a3, 404(t0)               # stash from-spr
    or t8, a1, r0                # another copy of out ptr
    sw t5, 408(t0)               # stash wind work
    sll r0, r0, 0
    lqc2 vf3, 64(t0)             # constant (4096., 128., 0., 0.)
    sll r0, r0, 0
    sw r0, 432(t0)               # set the flags to 0.

# Find the first group of 32 with a visible.
# Note: this only runs on the entrance, and this runs before any
# SPR DMA is started.
B1:
L138:
    bne t6, r0, L139 # if we found something advance to 139
    sll r0, r0, 0

B2:
    addiu a0, a0, 4     # advance vis pointer by 32 bits
    addiu t3, t3, 2048  # advance instance ptr by 32 * 64 = 2048
    daddiu a2, a2, -32  # decrement remaining instances by 32.
    lw t6, 0(a0)        # load the next vis 32-bits.
    blez a2, L177       # Return, if we got to the end.
    sll r0, r0, 0

B3:
    beq r0, r0, L138
    sll r0, r0, 0

# Here once we point to a visible thing.
# Now we should begin DMA to SPR.
# But first, we must wait for any in-progress transfer to end:
B4:
L139:
    lw t4, 0(t1)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t4, t4, 256
    sll r0, r0, 0
    bne t4, r0, L139
    sll r0, r0, 0

# No more in progress tranfer.
# Initialize the first transfer (special case)
# Initialize the double buffering.
B5:
    sw t3, 16(t1)      # source is the input instance array
    xori t4, t2, 2048  # dest is "the other" bank array (in spr)
    sw t4, 128(t1)
    addiu t4, r0, 128  # copy 128 qw = 2048 bytes = 32 instances
    sw t4, 32(t1)
    addiu t4, r0, 256  # start!!!
    sw t4, 0(t1)
    sll r0, r0, 0

# MAIN LOOP TOP
# In here, we have an in progress DMA SPR to transfer.
# we're going to do as much stuff as possible before waiting on the transfer.
# we'll start with finding the address of the next to transfer source.
# (we'll skip groups of 32 that have no visible)
B6:
L140:
    or ra, a0, r0       # ra = vis ptr for the SPR to transfer's instances
    xori t2, t2, 2048   # toggle spr bank to point to the uploading data
    daddiu a0, a0, 4    # increment vis ptr to the next upload's vis data
    or t7, a0, r0       # remember this as the end of this group's vis data
    or t4, t2, r0       # t4 = the uploading bank
    daddiu t6, a2, -32  # t6 = the number remaining after this group.
    bgtz t6, L142       # if there's nothing left, fall through (will skip "find next")
    lw t6, 0(a0)        # t6 is the next 32-bits of vis

B7:
    beq r0, r0, L145   # nothing left after the current upload, don't look for next
    sll r0, r0, 0

B8:
    sll r0, r0, 0
    lw v1, 400(r0)    # ?? probably will crash if we hit this, but it's unreachable.
B9:
L141:
    daddiu a2, a2, -32 # skip 32 instances that are invisible
    addiu a0, a0, 4    # advance vis ptr
    blez a2, L145      # did we reach the end? if so, give up looking for another.
    lw t6, 0(a0)       # load next vis

B10:
    sll r0, r0, 0
    sll r0, r0, 0
B11:
L142:
    beq t6, r0, L141     # did we find visible?
    addiu t3, t3, 2048   # advance input to bank upload ptr.

# If we reach here, we've found the next SPR to transfer's source
B12:
L143:
    # check if in progress upload is done?
    lw t6, 0(t1)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 256
    sll r0, r0, 0
    beq t6, r0, L144
    sll r0, r0, 0

B13:
    # nope, increment stall counter.
    sll r0, r0, 0
    lw t6, 444(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu t6, t6, 1
    sll r0, r0, 0
    sw t6, 444(t0)
    beq r0, r0, L143
    sll r0, r0, 0

# if we reach here, we've found the next SPR to source, and
# the previous upload is done.
B14:
L144:
    sw t3, 16(t1)      # source addr
    xori t6, t2, 2048  # dest is the other
    sw t6, 128(t1)
    addiu t6, r0, 128  # 128 qw (see prev)
    sw t6, 32(t1)
    addiu t6, r0, 256
    beq r0, r0, L146   # skip waiting for completion!
    sw t6, 0(t1)

# if we reach here, we didn't find the next SPR to source
# because there's nothing to do next. So just wait for
# the current upload to finish
B15:
L145:
    lw t6, 0(t1)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 256
    sll r0, r0, 0
    beq t6, r0, L146
    sll r0, r0, 0

B16:
    # inc wait counter if we had to wait.
    sll r0, r0, 0
    lw t6, 444(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu t6, t6, 1
    sll r0, r0, 0
    sw t6, 444(t0)
    beq r0, r0, L145
    sll r0, r0, 0

# if we reach here, we've got some bank data.
# This is per-8 instances
B17:
L146:
    lb t6, 0(ra)      # load 8 bits of vis data for this bank.
    addiu ra, ra, 1   # seek to next vis byte
    sll r0, r0, 0
    sw ra, 412(t0)    # stash our vis bits
    bne t6, r0, L147  # branch if we have at least one visible in this group of 8.
    sw t7, 416(t0)    # stash end vis ptr.

B18:
    # here if the group of 8 was invisible
    daddiu a2, a2, -8 # skip 8
    addiu t4, t4, 512 # advance input array pointer by 8
    beq r0, r0, L173  # skip to end of 8-block loop.
    sll r0, r0, 0

# we have some visible!
B19:
L147:
    addiu t7, r0, 128  # some constant
    lqc2 vf2, 16(t4)   # load the bsphere of the instance.
B20:
L148:
    daddiu ra, t9, -246 # do we have room left in the output for up to 8 visible?
    sll r0, r0, 0       # note, really 10, not sure why they leave 2 empty.
    blez ra, L151   # branch if we have room, vcallms 42 no matter what.
    vcallms 42      # these people are insane.
                    # vi01 = clipping result
                    # vf05 = M(vf28) * bsphere (camera)
                    # vf06 = M(vf24) * bsphere (camera rot with special z)

B21:
L149:
    lw t8, 0(a3)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t8, t8, 256
    sll r0, r0, 0
    beq t8, r0, L150
    sll r0, r0, 0

B22:
    sll r0, r0, 0
    lw t8, 440(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu t8, t8, 1
    sll r0, r0, 0
    sw t8, 440(t0)
    beq r0, r0, L149
    sll r0, r0, 0

B23:
L150:
    sw a1, 128(a3)
    xori a1, a1, 12288
    sw v1, 16(a3)
    sll t8, t9, 4
    addu v1, v1, t8
    or t8, a1, r0
    sw t9, 32(a3)
    addiu t9, r0, 256
    sw t9, 0(a3)
    addiu t9, r0, 0
B24:
L151:
    sll r0, r0, 0
    lw ra, 12(t4)
    and gp, t6, t7
    ld s5, 56(t4)
    beq gp, r0, L172
    ld s2, 32(t4)

B25:
    sll gp, t9, 4
    ld s4, 40(t4)
    pextlh s3, s5, r0
    ld s5, 48(t4)
    psraw s3, s3, 10
    lq s1, 28(ra)
    pextlh s2, s2, r0
    lq s0, 44(ra)
    psraw s2, s2, 16
    qmtc2.ni vf14, s1
    pextlh s4, s4, r0
    qmtc2.ni vf15, s0
    psraw s4, s4, 16
    qmtc2.ni vf13, s3
    pextlh s5, s5, r0
    qmtc2.ni vf10, s2
    psraw s3, s5, 16
    lhu s2, 62(t4)
    addu gp, gp, v1
    qmtc2.ni vf11, s4
    dsll s5, s2, 4
    qmtc2.ni vf12, s3
    daddu s4, s2, t5
    lw s2, 408(t0)
    andi s4, s4, 63
    lw s3, 384(t0)
    sll s1, s4, 4
    lw s4, 4(ra)
    daddu s5, s3, s5
    addu s3, s1, s2
    andi s1, s4, 1
    andi s4, s4, 2
    bne s1, r0, L172
    cfc2.ni s1, vi1

B26:
    vitof0.xyzw vf13, vf13
    lw t5, 1324(s2)
    bne s1, r0, L172
    lqc2 vf25, 112(t0)

B27:
    sll r0, r0, 0
    lqc2 vf16, 16(t0)
    sll r0, r0, 0
    lqc2 vf17, 32(t0)
    vmulaz.xyzw acc, vf1, vf6
    sw gp, 196(t0)
    vmsubw.xyzw vf8, vf1, vf2
    sw gp, 276(t0)
    vadd.xyz vf5, vf0, vf0
    sll r0, r0, 0
    vadd.xyz vf13, vf13, vf2
    sll r0, r0, 0
    vmula.xyzw acc, vf1, vf1
    sll r0, r0, 0
    vsub.xyzw vf14, vf8, vf14
    sll r0, r0, 0
    vaddw.w vf5, vf5, vf17
    sll r0, r0, 0
    sll r0, r0, 0
    lqc2 vf30, 80(t0)
    vmini.xyzw vf25, vf8, vf25
    sll r0, r0, 0
    vmsub.xyz vf15, vf14, vf15
    sll r0, r0, 0
    vminiy.w vf5, vf5, vf16
    sll r0, r0, 0
    sll r0, r0, 0
    lqc2 vf24, 128(t0)
    sll r0, r0, 0
    sqc2 vf25, 112(t0)
    vmini.xyz vf15, vf15, vf1
    sll r0, r0, 0
    vmaxx.w vf5, vf5, vf16
    sll r0, r0, 0
    vsubz.xyzw vf16, vf8, vf16
    sll r0, r0, 0
    sll r0, r0, 0
    lqc2 vf25, 144(t0)
    sll r0, r0, 0
    lqc2 vf26, 160(t0)
    sll r0, r0, 0
    lqc2 vf27, 176(t0)
    vmulax.xyzw acc, vf24, vf2
    sll r0, r0, 0
    vmadday.xyzw acc, vf25, vf2
    sll r0, r0, 0
    vmaddaz.xyzw acc, vf26, vf2
    sll r0, r0, 0
    vmsubaw.xyzw acc, vf27, vf0
    sll r0, r0, 0
    vmsubw.xyzw vf24, vf1, vf2
    sll r0, r0, 0
    sll r0, r0, 0
    qmfc2.i s2, vf16
    vmulw.xyzw vf28, vf15, vf30
    sll r0, r0, 0
    vmulw.xyzw vf29, vf15, vf30
    sll r0, r0, 0
    sll r0, r0, 0
    lqc2 vf19, 0(t0)
    vitof12.xyzw vf10, vf10
    sll r0, r0, 0
    pcgtw s1, r0, s2
    qmfc2.i s0, vf24
    vmulx.xyzw vf28, vf1, vf28
    sll r0, r0, 0
    vmulz.xyzw vf29, vf1, vf29
    lw s2, 56(ra)
    pcgtw s0, r0, s0
    sqc2 vf5, 80(t8)
    ppach s0, r0, s0
    sw s4, 80(t8)
    or s1, s0, s1
    sqc2 vf14, 96(t0)
    ppacb s1, r0, s1
    mfc1 r0, f31
    beq s2, r0, L153
    sw s1, 84(t8)

B28:
    vftoi0.zw vf28, vf28
    ld s1, 8(s5)
    vftoi0.zw vf29, vf29
    ld s2, 0(s5)
    pextlw s1, r0, s1
    lqc2 vf16, 12(s3)
    pextlw s3, r0, s2
    qmtc2.i vf18, s1
    sll r0, r0, 0
    qmtc2.i vf17, s3
    vmula.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmsubax.xyzw acc, vf18, vf19
    sll r0, r0, 0
    vmsuby.xyzw vf16, vf17, vf19
    sll r0, r0, 0
    vsubx.x vf28, vf30, vf15
    sll r0, r0, 0
    vsubz.x vf29, vf1, vf15
    sll r0, r0, 0
    vitof0.zw vf28, vf28
    sll r0, r0, 0
    vmulaz.xyzw acc, vf16, vf19
    sll r0, r0, 0
    vmadd.xyzw vf18, vf1, vf18
    sll r0, r0, 0
    vitof0.zw vf29, vf29
    sll r0, r0, 0
    vaddy.y vf28, vf0, vf0
    sll r0, r0, 0
    vaddy.y vf29, vf0, vf0
    sll r0, r0, 0
    vmulaz.xyzw acc, vf18, vf19
    sll r0, r0, 0
    vmadd.xyzw vf17, vf17, vf1
    sll r0, r0, 0
    vitof12.xyzw vf11, vf11
    sll r0, r0, 0
    vitof12.xyzw vf12, vf12
    sll r0, r0, 0
    vsubw.w vf28, vf30, vf28
    sll r0, r0, 0
    vminiw.xyzw vf17, vf17, vf0
    sll r0, r0, 0
    vsubw.w vf29, vf30, vf29
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    qmfc2.i s3, vf18
    vmaxw.xyzw vf27, vf17, vf19
    sll r0, r0, 0
    ppacw s3, r0, s3
    mfc1 r0, f31
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    vmulw.xyzw vf27, vf27, vf15
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    vmulax.yw acc, vf0, vf0
    sll r0, r0, 0
    vmulay.xz acc, vf27, vf10
    sll r0, r0, 0
    vmadd.xyzw vf10, vf1, vf10
    sll r0, r0, 0
    sll r0, r0, 0
    qmfc2.i s2, vf27
    vmulax.yw acc, vf0, vf0
    lw s1, 436(t0)
    vmulay.xz acc, vf27, vf11
    sll r0, r0, 0
    vmadd.xyzw vf11, vf1, vf11
    sll r0, r0, 0
    bne s1, s7, L152
    ppacw s2, r0, s2

B29:
    vmulax.yw acc, vf0, vf0
    sd s3, 8(s5)
    vmulay.xz acc, vf27, vf12
    sd s2, 0(s5)
    bne s4, r0, L164
    vmadd.xyzw vf12, vf1, vf12

B30:
    beq r0, r0, L154
    sll r0, r0, 0

B31:
L152:
    vmulax.yw acc, vf0, vf0
    sll r0, r0, 0
    vmulay.xz acc, vf27, vf12
    sll r0, r0, 0
    bne s4, r0, L164
    vmadd.xyzw vf12, vf1, vf12

B32:
    beq r0, r0, L154
    sll r0, r0, 0

B33:
L153:
    vftoi0.zw vf28, vf28
    sll r0, r0, 0
    vftoi0.zw vf29, vf29
    sll r0, r0, 0
    vsubx.x vf28, vf30, vf15
    sll r0, r0, 0
    vsubz.x vf29, vf1, vf15
    sll r0, r0, 0
    vitof0.zw vf28, vf28
    sll r0, r0, 0
    vitof0.zw vf29, vf29
    sll r0, r0, 0
    vaddy.y vf28, vf0, vf0
    sll r0, r0, 0
    vaddy.y vf29, vf0, vf0
    sll r0, r0, 0
    vsubw.w vf28, vf30, vf28
    sll r0, r0, 0
    vsubw.w vf29, vf30, vf29
    sll r0, r0, 0
    vitof12.xyzw vf11, vf11
    sll r0, r0, 0
    bne s4, r0, L164
    vitof12.xyzw vf12, vf12

B34:
L154:
    sll r0, r0, 0
    lw s5, 84(t8)
    sll r0, r0, 0
    lw s4, 108(t0)
    addiu t9, t9, 6
    lw s3, 104(t0)
    bne s5, r0, L158
    vsubw.w vf10, vf10, vf10

B35:
    bgtz s4, L156
    sll r0, r0, 0

B36:
    bgtz s3, L155
    sll r0, r0, 0

B37:
    sll r0, r0, 0
    lh s4, 78(ra)
    sll r0, r0, 0
    lw s5, 64(ra)
    daddiu s4, s4, 1
    sqc2 vf28, 64(t8)
    vmulax.xyzw acc, vf20, vf10
    addiu gp, gp, 96
    vmadday.xyzw acc, vf21, vf10
    sw gp, 64(ra)
    vmaddz.xyzw vf10, vf22, vf10
    sh s4, 78(ra)
    vmulax.xyzw acc, vf20, vf11
    lbu s4, 109(ra)
    vmadday.xyzw acc, vf21, vf11
    lhu gp, 118(ra)
    vmaddz.xyzw vf11, vf22, vf11
    lbu s3, 113(ra)
    beq r0, r0, L157
    sll r0, r0, 0

B38:
L155:
    sll r0, r0, 0
    lh s4, 80(ra)
    sll r0, r0, 0
    lw s5, 68(ra)
    daddiu s4, s4, 1
    sqc2 vf29, 64(t8)
    vmulax.xyzw acc, vf20, vf10
    addiu gp, gp, 96
    vmadday.xyzw acc, vf21, vf10
    sw gp, 68(ra)
    vmaddz.xyzw vf10, vf22, vf10
    sh s4, 80(ra)
    vmulax.xyzw acc, vf20, vf11
    lbu s4, 110(ra)
    vmadday.xyzw acc, vf21, vf11
    lhu gp, 120(ra)
    vmaddz.xyzw vf11, vf22, vf11
    lbu s3, 114(ra)
    beq r0, r0, L157
    sll r0, r0, 0

B39:
L156:
    sll r0, r0, 0
    lh s4, 82(ra)
    sll r0, r0, 0
    lw s5, 72(ra)
    daddiu s4, s4, 1
    sqc2 vf30, 64(t8)
    vmulax.xyzw acc, vf20, vf10
    addiu gp, gp, 96
    vmadday.xyzw acc, vf21, vf10
    sw gp, 72(ra)
    vmaddz.xyzw vf10, vf22, vf10
    sh s4, 82(ra)
    vmulax.xyzw acc, vf20, vf11
    lbu s4, 111(ra)
    vmadday.xyzw acc, vf21, vf11
    lhu gp, 122(ra)
    vmaddz.xyzw vf11, vf22, vf11
    lbu s3, 115(ra)
B40:
L157:
    vmulax.xyzw acc, vf20, vf12
    lq s2, 224(t0)
    vmadday.xyzw acc, vf21, vf12
    lq s1, 240(t0)
    vmaddz.xyzw vf12, vf22, vf12
    dsll gp, gp, 4
    vmulax.xyzw acc, vf20, vf13
    daddu s3, s3, ra
    vmadday.xyzw acc, vf21, vf13
    sll r0, r0, 0
    vmaddaz.xyzw acc, vf22, vf13
    sll r0, r0, 0
    vmaddw.xyzw vf13, vf23, vf0
    sll r0, r0, 0
    sqc2 vf10, 0(t8)
    sll r0, r0, 0
    sqc2 vf11, 16(t8)
    movz s2, s1, s5
    sqc2 vf12, 32(t8)
    daddiu t8, t8, 96
    beq r0, r0, L159
    sqc2 vf13, -48(t8)

B41:
L158:
    sll r0, r0, 0
    lqc2 vf24, 320(t0)
    sll r0, r0, 0
    lqc2 vf25, 336(t0)
    sll r0, r0, 0
    lqc2 vf26, 352(t0)
    sll r0, r0, 0
    lqc2 vf27, 368(t0)
    sll r0, r0, 0
    lh s4, 76(ra)
    sll r0, r0, 0
    lw s5, 60(ra)
    daddiu s4, s4, 1
    sqc2 vf28, 64(t8)
    vmulax.xyzw acc, vf24, vf10
    addiu gp, gp, 96
    vmadday.xyzw acc, vf25, vf10
    sw gp, 60(ra)
    vmaddz.xyzw vf10, vf26, vf10
    sh s4, 76(ra)
    vmulax.xyzw acc, vf24, vf11
    lbu s4, 108(ra)
    vmadday.xyzw acc, vf25, vf11
    lhu gp, 116(ra)
    vmaddz.xyzw vf11, vf26, vf11
    lbu s3, 112(ra)
    vmulax.xyzw acc, vf24, vf12
    lq s2, 224(t0)
    vmadday.xyzw acc, vf25, vf12
    lq s1, 240(t0)
    vmaddz.xyzw vf12, vf26, vf12
    dsll gp, gp, 4
    vmulax.xyzw acc, vf24, vf13
    daddu s3, s3, ra
    vmadday.xyzw acc, vf25, vf13
    sll r0, r0, 0
    vmaddaz.xyzw acc, vf26, vf13
    sll r0, r0, 0
    vmaddw.xyzw vf13, vf27, vf0
    sll r0, r0, 0
    sqc2 vf10, 0(t8)
    sll r0, r0, 0
    sqc2 vf11, 16(t8)
    sll r0, r0, 0
    sqc2 vf12, 32(t8)
    movz s2, s1, s5
    sqc2 vf13, 48(t8)
    daddiu t8, t8, 96
B42:
L159:
    sll r0, r0, 0
    lw ra, 8(t4)
    sll r0, r0, 0
    sq s2, 256(t0)
    sll r0, r0, 0
    lbu s2, 144(s3)
    addu s1, gp, ra
    sw s5, 260(t0)
    daddiu t9, t9, 3
    sw s1, 212(t0)
    sll s1, s2, 2
    sh s2, 208(t0)
    sll s2, s2, 4
    sb s1, 222(t0)
    daddu gp, gp, s2
    lq s2, 192(t0)
    daddiu s5, s5, 48
    lq s1, 208(t0)
    daddiu t8, t8, 48
    lq s0, 256(t0)
    daddiu s3, s3, 1
    sq s2, -48(t8)
    daddiu s4, s4, -1
    sq s1, -32(t8)
    blez s4, L172
    sq s0, -16(t8)

B43:
L160:
    daddiu s2, t9, -252
    sll r0, r0, 0
    blez s2, L163
    sll r0, r0, 0

B44:
L161:
    lw t8, 0(a3)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t8, t8, 256
    sll r0, r0, 0
    beq t8, r0, L162
    sll r0, r0, 0

B45:
    sll r0, r0, 0
    lw t8, 440(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu t8, t8, 1
    sll r0, r0, 0
    sw t8, 440(t0)
    beq r0, r0, L161
    sll r0, r0, 0

B46:
L162:
    sw a1, 128(a3)
    xori a1, a1, 12288
    sw v1, 16(a3)
    sll t8, t9, 4
    addu v1, v1, t8
    or t8, a1, r0
    sw t9, 32(a3)
    addiu t9, r0, 256
    sw t9, 0(a3)
    addiu t9, r0, 0
B47:
L163:
    sll r0, r0, 0
    lbu s2, 144(s3)
    addu s1, gp, ra
    sw s5, 260(t0)
    daddiu t9, t9, 3
    sw s1, 212(t0)
    sll s1, s2, 2
    sh s2, 208(t0)
    sll s2, s2, 4
    sb s1, 222(t0)
    daddu gp, gp, s2
    lq s2, 192(t0)
    daddiu s5, s5, 48
    lq s1, 208(t0)
    daddiu t8, t8, 48
    lq s0, 256(t0)
    daddiu s3, s3, 1
    sq s2, -48(t8)
    daddiu s4, s4, -1
    sq s1, -32(t8)
    bgtz s4, L160
    sq s0, -16(t8)

B48:
    beq r0, r0, L172
    sll r0, r0, 0

B49:
L164:
    vmul.xyz vf16, vf6, vf6
    sll r0, r0, 0
    sll r0, r0, 0
    lqc2 vf9, 124(ra)
    vsubw.w vf10, vf10, vf10
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    vadday.x acc, vf16, vf16
    sll r0, r0, 0
    vmaddz.x vf16, vf1, vf16
    sll r0, r0, 0
    vsqrt Q, vf16.x
    sll r0, r0, 0
    vmulay.xyzw acc, vf1, vf9
    sll r0, r0, 0
    vmaddaw.xyzw acc, vf1, vf2
    sll r0, r0, 0
    sll r0, r0, 0
    vwaitq
    vmsubq.xyzw vf16, vf1, Q
    sll r0, r0, 0
    vmulx.xyzw vf16, vf16, vf9
    sll r0, r0, 0
    vmaxx.x vf16, vf16, vf0
    sll r0, r0, 0
    vminiy.x vf16, vf16, vf3
    sll r0, r0, 0
    vftoi0.xyzw vf16, vf16
    sll r0, r0, 0
    sll r0, r0, 0
    qmfc2.i s5, vf16
    sll r0, r0, 0
    sll r0, r0, 0
    andi s5, s5, 255
    sll r0, r0, 0
    beq s5, r0, L154
    sll r0, r0, 0

B50:
    vcallms 29
    sw s4, 432(t0)
    sll r0, r0, 0
    lw s4, 108(t0)
    addiu t9, t9, 6
    lw s3, 104(t0)
    sll r0, r0, 0
    sw s5, 80(t8)
    bgtz s4, L166
    sll r0, r0, 0

B51:
    bgtz s3, L165
    sll r0, r0, 0

B52:
    sll r0, r0, 0
    lh s4, 86(ra)
    sll r0, r0, 0
    lw s5, 96(ra)
    daddiu s4, s4, 1
    sqc2 vf28, 64(t8)
    vmulax.xyzw acc, vf24, vf10
    addiu gp, gp, 96
    vmadday.xyzw acc, vf25, vf10
    sw gp, 96(ra)
    vmaddz.xyzw vf10, vf26, vf10
    sh s4, 86(ra)
    vmulax.xyzw acc, vf24, vf11
    lbu s3, 109(ra)
    vmadday.xyzw acc, vf25, vf11
    lhu gp, 118(ra)
    vmaddz.xyzw vf11, vf26, vf11
    lbu s4, 113(ra)
    beq r0, r0, L167
    sll r0, r0, 0

B53:
L165:
    sll r0, r0, 0
    lh s4, 88(ra)
    sll r0, r0, 0
    lw s5, 100(ra)
    daddiu s4, s4, 1
    sqc2 vf29, 64(t8)
    vmulax.xyzw acc, vf24, vf10
    addiu gp, gp, 96
    vmadday.xyzw acc, vf25, vf10
    sw gp, 100(ra)
    vmaddz.xyzw vf10, vf26, vf10
    sh s4, 88(ra)
    vmulax.xyzw acc, vf24, vf11
    lbu s3, 110(ra)
    vmadday.xyzw acc, vf25, vf11
    lhu gp, 120(ra)
    vmaddz.xyzw vf11, vf26, vf11
    lbu s4, 114(ra)
    beq r0, r0, L167
    sll r0, r0, 0

B54:
L166:
    sll r0, r0, 0
    lh s4, 90(ra)
    sll r0, r0, 0
    lw s5, 104(ra)
    daddiu s4, s4, 1
    sqc2 vf30, 64(t8)
    vmulax.xyzw acc, vf24, vf10
    addiu gp, gp, 96
    vmadday.xyzw acc, vf25, vf10
    sw gp, 104(ra)
    vmaddz.xyzw vf10, vf26, vf10
    sh s4, 90(ra)
    vmulax.xyzw acc, vf24, vf11
    lbu s3, 111(ra)
    vmadday.xyzw acc, vf25, vf11
    lhu gp, 122(ra)
    vmaddz.xyzw vf11, vf26, vf11
    lbu s4, 115(ra)
B55:
L167:
    vmulax.xyzw acc, vf24, vf12
    dsll gp, gp, 4
    vmadday.xyzw acc, vf25, vf12
    daddu s4, s4, ra
    vmaddz.xyzw vf12, vf26, vf12
    sll r0, r0, 0
    vmulax.xyzw acc, vf24, vf13
    sll r0, r0, 0
    vmadday.xyzw acc, vf25, vf13
    sll r0, r0, 0
    vmaddaz.xyzw acc, vf26, vf13
    sll r0, r0, 0
    vmaddw.xyzw vf13, vf27, vf0
    sll r0, r0, 0
    sqc2 vf10, 0(t8)
    sll r0, r0, 0
    sqc2 vf11, 16(t8)
    sll r0, r0, 0
    sqc2 vf12, 32(t8)
    sll r0, r0, 0
    sqc2 vf13, 48(t8)
    daddiu t8, t8, 96
    sll r0, r0, 0
    lw ra, 8(t4)
    sll r0, r0, 0
    lbu s2, 144(s4)
    addu s1, gp, ra
    sw s5, 284(t0)
    daddiu t9, t9, 3
    sw s1, 292(t0)
    sll s1, s2, 4
    sh s2, 288(t0)
    daddu gp, gp, s1
    lq s2, 272(t0)
    daddiu s5, s5, 48
    lq s1, 288(t0)
    daddiu t8, t8, 48
    lq s0, 304(t0)
    daddiu s4, s4, 1
    sq s2, -48(t8)
    daddiu s3, s3, -1
    sq s1, -32(t8)
    blez s3, L172
    sq s0, -16(t8)

B56:
L168:
    daddiu s2, t9, -252
    sll r0, r0, 0
    blez s2, L171
    sll r0, r0, 0

B57:
L169:
    lw t8, 0(a3)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t8, t8, 256
    sll r0, r0, 0
    beq t8, r0, L170
    sll r0, r0, 0

B58:
    sll r0, r0, 0
    lw t8, 440(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu t8, t8, 1
    sll r0, r0, 0
    sw t8, 440(t0)
    beq r0, r0, L169
    sll r0, r0, 0

B59:
L170:
    sw a1, 128(a3)
    xori a1, a1, 12288
    sw v1, 16(a3)
    sll t8, t9, 4
    addu v1, v1, t8
    or t8, a1, r0
    sw t9, 32(a3)
    addiu t9, r0, 256
    sw t9, 0(a3)
    addiu t9, r0, 0
B60:
L171:
    sll r0, r0, 0
    lbu s2, 144(s4)
    addu s1, gp, ra
    sw s5, 284(t0)
    daddiu t9, t9, 3
    sw s1, 292(t0)
    sll s1, s2, 4
    sh s2, 288(t0)
    daddu gp, gp, s1
    lq s2, 272(t0)
    daddiu s5, s5, 48
    lq s1, 288(t0)
    daddiu t8, t8, 48
    lq s0, 304(t0)
    daddiu s4, s4, 1
    sq s2, -48(t8)
    daddiu s3, s3, -1
    sq s1, -32(t8)
    bgtz s3, L168
    sq s0, -16(t8)

B61:
L172:
    addiu a2, a2, -1
    srl t7, t7, 1
    daddiu t4, t4, 64
    sll r0, r0, 0
    bne t7, r0, L148
    lqc2 vf2, 16(t4)

# early exit for per-8
B62:
L173:
    sll r0, r0, 0
    lw ra, 412(t0)
    sll r0, r0, 0
    lw t7, 416(t0)
    bne ra, t7, L146
    sll r0, r0, 0

B63:
    bgtz a2, L140
    sll r0, r0, 0

B64:
    beq t9, r0, L176
    sll r0, r0, 0

B65:
L174:
    lw a0, 0(a3)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi a0, a0, 256
    sll r0, r0, 0
    beq a0, r0, L175
    sll r0, r0, 0

B66:
    sll r0, r0, 0
    lw a0, 440(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu a0, a0, 1
    sll r0, r0, 0
    sw a0, 440(t0)
    beq r0, r0, L174
    sll r0, r0, 0

B67:
L175:
    sw a1, 128(a3)
    xori a0, a1, 12288
    sw v1, 16(a3)
    sll a1, t9, 4
    addu v1, v1, a1
    or a0, a0, r0
    sw t9, 32(a3)
    addiu a0, r0, 256
    sw a0, 0(a3)
    addiu a0, r0, 0
B68:
L176:
    lw a0, 0(a3)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi a0, a0, 256
    sll r0, r0, 0
    beq a0, r0, L177
    sll r0, r0, 0

B69:
    sll r0, r0, 0
    lw a0, 440(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu a0, a0, 1
    sll r0, r0, 0
    sw a0, 440(t0)
    beq r0, r0, L176
    sll r0, r0, 0

# Final exit
B70:
L177:
    lw a0, 396(t0)
    sll r0, r0, 0
    sw v1, 4(a0)
    sll r0, r0, 0
    or v0, r0, r0
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

    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0