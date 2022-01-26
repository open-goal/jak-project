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
# The upload chunks are 2kB and complete in 128 cycles ideally.

# it's documented, not including:
# -tie near (but this is very simple)
# -wind (probably not too hard)
# -generic (for another day)

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
# TOP for per-8 instances
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
    addiu t7, r0, 128  # vis mask constant (start at highest bit)
    lqc2 vf2, 16(t4)   # load the bsphere of the instance.
B20:
L148:
    daddiu ra, t9, -246 # do we have room left in the output for up to 8 visible?
    sll r0, r0, 0       # note, really 10, not sure why they leave 2 empty.
    blez ra, L151   # branch if we have room, vcallms 42 no matter what.
    vcallms 42      # these people are insane.
                    # vi01 = in view frustum result
                    # vf05 = M(vf28) * bsphere (camera)
                    # vf06 = M(vf24) * bsphere (camera rot (I think includes trans, but no perspective))

# if we got here, we filled the output buffer.
# we should then copy the output FROM SPR to the dma buffer.
B21:
L149:
    lw t8, 0(a3)    # SPR FROM sync.
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
    sw a1, 128(a3)     # copy from out ptr
    xori a1, a1, 12288 # toggle out
    sw v1, 16(a3)      # copy to dma buf ptr
    sll t8, t9, 4
    addu v1, v1, t8    # seek dma buf ptr to the next
    or t8, a1, r0      # t8 = the next out ptr
    sw t9, 32(a3)      # store qwc
    addiu t9, r0, 256
    sw t9, 0(a3)       # start the FROM.
    addiu t9, r0, 0    # t9 = 0 (why?)

# here, we have room in the scratchpad output for up to 10.
B24:
L151:
    sll r0, r0, 0
    lw ra, 12(t4)       # ra = our prototype bucket
    and gp, t6, t7      # check if our vis bit is set.
    ld s5, 56(t4)       # s5 = origin_3
    beq gp, r0, L172    # are we even visible?
    ld s2, 32(t4)       # s2 = origin_0

B25:
    sll gp, t9, 4       # gp = output offset
    ld s4, 40(t4)       # s4 = origin_1
    pextlh s3, s5, r0   # s3 = s5 << 16 (now 128-bits, origin3)
    ld s5, 48(t4)       # s5 = origin_2
    psraw s3, s3, 10    # s3 = origin3, now signed
    lq s1, 28(ra)       # s1 = our prototype bucket's dists
    pextlh s2, s2, r0   # conversion of origin matrix
    lq s0, 44(ra)       # s0 = [rlength-near, rlength-stiff, rlength-mid, stiffness]
    psraw s2, s2, 16    # conversion of origin matrix (see tie format doc)
    qmtc2.ni vf14, s1   # vf14 = dists
    pextlh s4, s4, r0   # conversion
    qmtc2.ni vf15, s0   # vf15 = rlengths
    psraw s4, s4, 16    # conversion
    qmtc2.ni vf13, s3   # vf13 = origin3
    pextlh s5, s5, r0   # conversion
    qmtc2.ni vf10, s2   # vf10 = origin0
    psraw s3, s5, 16    # conversion
    lhu s2, 62(t4)      # s2 = wind index
    addu gp, gp, v1     # gp = output address (spr buffer)
    qmtc2.ni vf11, s4   # vf11 = origin1
    dsll s5, s2, 4      # s5 = wind_idx * 16
    qmtc2.ni vf12, s3   # vf12 = origin2
    daddu s4, s2, t5    # s4 = wind_idx + time?  (on the first time through, seems like it's not time...)
    lw s2, 408(t0)      # s2 = wind work
    andi s4, s4, 63     # truncate upper bits so we fit in the 64 wind array.
    lw s3, 384(t0)      # s3 = wind vectors
    sll s1, s4, 4       # index qw's with our [0, 64) index
    lw s4, 4(ra)        # flags
    daddu s5, s3, s5    # s5 = wind vector pointer (not from wind work, in the static data.)
    addu s3, s1, s2     # wind work vector array pointer
    andi s1, s4, 1      # s1 = flag1
    andi s4, s4, 2      # s4 = flag2
    bne s1, r0, L172    # skip if flag1 is set.
    cfc2.ni s1, vi1     # get the in-view-frustum result from VU0.

B26:
    vitof0.xyzw vf13, vf13  # convert origin matrix row to float.
    lw t5, 1324(s2)         # load wind time
    bne s1, r0, L172        # skip if not in view frustum.
    lqc2 vf25, 112(t0)      # vf25 = current TIE min distance

B27:
    sll r0, r0, 0
    lqc2 vf16, 16(t0)          # vf16 = hmge-d
    sll r0, r0, 0
    lqc2 vf17, 32(t0)          # vf17 = hvdf-offset
    vmulaz.xyzw acc, vf1, vf6  # [z, z, z, z] (how in front of camera)
    sw gp, 196(t0)             # work.upload-color-0.addr = output_ptr
    vmsubw.xyzw vf8, vf1, vf2  # vf8 = dist in front of cam (origin - r_bs)
    sw gp, 276(t0)             # work.generic-color-0.addr = output_ptr
    vadd.xyz vf5, vf0, vf0     # vf5 = [0, 0, 0, transformed_w]
    sll r0, r0, 0
    vadd.xyz vf13, vf13, vf2   # vf13 = (+, +, +) corner of bounding box (outside of bsphere)
    sll r0, r0, 0
    vmula.xyzw acc, vf1, vf1   # acc = [1, 1, 1, 1]
    sll r0, r0, 0
    vsub.xyzw vf14, vf8, vf14  # (origin - r_bs) - dists
    sll r0, r0, 0
    vaddw.w vf5, vf5, vf17     # vf5 = [0, 0, 0, transformed_w + hvdf_offset.w]
    sll r0, r0, 0
    sll r0, r0, 0
    lqc2 vf30, 80(t0)          # vf30 = far_morph
    vmini.xyzw vf25, vf8, vf25 # update TIE min distance
    sll r0, r0, 0
    vmsub.xyz vf15, vf14, vf15 # dist weights
    sll r0, r0, 0
    vminiy.w vf5, vf5, vf16    # clip w min
    sll r0, r0, 0
    sll r0, r0, 0
    lqc2 vf24, 128(t0)         # vf24 = guard plane 0 (i think w plane)
    sll r0, r0, 0
    sqc2 vf25, 112(t0)         # vf25 = min-dist
    vmini.xyz vf15, vf15, vf1  # saturate dist weights
    sll r0, r0, 0
    vmaxx.w vf5, vf5, vf16     # clip w max
    sll r0, r0, 0
    vsubz.xyzw vf16, vf8, vf16 # vf16 = dist in front of the camera - some hmge thing.
    sll r0, r0, 0
    sll r0, r0, 0
    lqc2 vf25, 144(t0)         # vf25 = some plane
    sll r0, r0, 0
    lqc2 vf26, 160(t0)         # vf26 = another plane
    sll r0, r0, 0
    lqc2 vf27, 176(t0)         # vf27 = yet another plane
    vmulax.xyzw acc, vf24, vf2 # perform clipping with this plane.
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
    qmfc2.i s2, vf16             # s2 = dists - hmge thing
    vmulw.xyzw vf28, vf15, vf30  # vf28 = scaled dist weights
    sll r0, r0, 0
    vmulw.xyzw vf29, vf15, vf30  # vf29 = scaled dist weights, again?
    sll r0, r0, 0
    sll r0, r0, 0
    lqc2 vf19, 0(t0)             # vf19 = wind const (WIND)
    vitof12.xyzw vf10, vf10      # convert origin (back to this, I guess)
    sll r0, r0, 0
    pcgtw s1, r0, s2             # dists check
    qmfc2.i s0, vf24             # clip check
    vmulx.xyzw vf28, vf1, vf28   # distweights x
    sll r0, r0, 0
    vmulz.xyzw vf29, vf1, vf29   # distweights z
    lw s2, 56(ra)                # s2 = stiffness
    pcgtw s0, r0, s0             # check clip again
    sqc2 vf5, 80(t8)             # store magic w.
    ppach s0, r0, s0             # s0 = more clip
    sw s4, 80(t8)                # store some flags
    or s1, s0, s1                # more clipping/distance crap
    sqc2 vf14, 96(t0)            # called "dist-test"
    ppacb s1, r0, s1             # distance stuff.
    mfc1 r0, f31
    beq s2, r0, L153             # if stiffness == 0, skip ahead
    sw s1, 84(t8)                # output some clip/dist info.

# apply wind.
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
    vmulax.yw acc, vf0, vf0    # acc = [? 0 ? 0]
    sll r0, r0, 0
    vmulay.xz acc, vf27, vf12  # acc = [o2.y*gp3.x, 0, o2.y*gp3.z, 0] (wtf is this)
    sll r0, r0, 0
    bne s4, r0, L164
    vmadd.xyzw vf12, vf1, vf12

B32:
    beq r0, r0, L154
    sll r0, r0, 0

# Don't Apply Wind.
B33:
L153:
    vftoi0.zw vf28, vf28    # dist weights
    sll r0, r0, 0
    vftoi0.zw vf29, vf29    # more dist weights
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
    vitof12.xyzw vf11, vf11 # convert origin1
    sll r0, r0, 0
    bne s4, r0, L164
    vitof12.xyzw vf12, vf12 # convert origin2


# End of stiffness calcultion.
# S4 = 0 version. Goto L164 for S4 != 0 version.
# Maybe this is the version for non-generic??
B34:
L154:
    sll r0, r0, 0
    lw s5, 84(t8)   # s5 = clipping/dist flags
    sll r0, r0, 0
    lw s4, 108(t0) # s4 = dist_test_w (from work)
    addiu t9, t9, 6 # advance output qwc by 6 (96 bytes)
    lw s3, 104(t0) # s3 = dist_test_z (from work)
    bne s5, r0, L158          # if we clip, go to TIE NEAR.
    vsubw.w vf10, vf10, vf10  # clear the w component of origin0

B35:
    bgtz s4, L156
    sll r0, r0, 0

B36:
    bgtz s3, L155
    sll r0, r0, 0

B37: # GEOM 1 (this is the non-near version) (believed to be the high-LOD one)
    sll r0, r0, 0
    lh s4, 78(ra)     # load count 1
    sll r0, r0, 0
    lw s5, 64(ra)     # load next
    daddiu s4, s4, 1  # inc count
    sqc2 vf28, 64(t8) # store morph constants
    vmulax.xyzw acc, vf20, vf10 # matmul 0/16
    addiu gp, gp, 96   # output pointer, I think this is a pointer to the RAM dma buffer, not spr.
    vmadday.xyzw acc, vf21, vf10 # matmul 1/16
    sw gp, 64(ra)      # update next.
    vmaddz.xyzw vf10, vf22, vf10 # matmul 2/16 (out vf10)
    sh s4, 78(ra)      # store updated count
    vmulax.xyzw acc, vf20, vf11 # matmul 3/16
    lbu s4, 109(ra)    # s4 = frag-count 1
    vmadday.xyzw acc, vf21, vf11 # matmul 4/16
    lhu gp, 118(ra)    # gp = base-qw
    vmaddz.xyzw vf11, vf22, vf11 # matmul 5/16 (out vf11)
    lbu s3, 113(ra)    # s3 = index-start
    beq r0, r0, L157
    sll r0, r0, 0

B38:
L155: # geom 2 version (same as above)
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
L156: # geom 3 version (same as above)
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

# common for 1,2,3 geoms
B40:
L157:
    vmulax.xyzw acc, vf20, vf12  # matmul 6/16
    lq s2, 224(t0)               # s2 = upload color 2
    vmadday.xyzw acc, vf21, vf12 # matmul 7/16
    lq s1, 240(t0)               # s1 = upload color ret
    vmaddz.xyzw vf12, vf22, vf12 # matmul 8/16 (out vf12)
    dsll gp, gp, 4               # base-offset (from base-qw in the prototype)
    vmulax.xyzw acc, vf20, vf13  # matmul
    daddu s3, s3, ra             # s3 = prototype bucket + index zone (noe sure what's here yet.)
    vmadday.xyzw acc, vf21, vf13 # matmul
    sll r0, r0, 0
    vmaddaz.xyzw acc, vf22, vf13 # matmul
    sll r0, r0, 0
    vmaddw.xyzw vf13, vf23, vf0  # matmul (out vf13, we're done)
    sll r0, r0, 0
    sqc2 vf10, 0(t8)  # store matrix
    sll r0, r0, 0
    sqc2 vf11, 16(t8) # store matrix
    movz s2, s1, s5   # if we're the first thing added, we'll be the last in the chain, and should put a upload-color-ret!
    sqc2 vf12, 32(t8) # store matrix
    daddiu t8, t8, 96 # inc SPR ptr.
    beq r0, r0, L159
    sqc2 vf13, -48(t8) # store matrix

# TIE NEAR DMA generation
# (don't care)
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

# And Back to common non-generic TIE.
# it is time for colors. This is a loop over fragments
# s2 = tag2
# s4 = frag-count 1
# gp = base-qw * 16
# s3 = prototype + index-start
B42:
L159:
    sll r0, r0, 0
    lw ra, 8(t4)  # ra = color-indices
    sll r0, r0, 0
    sq s2, 256(t0) # upload color temp (either ret or 2, we'll refer to it as 2)
    sll r0, r0, 0
    lbu s2, 144(s3) # load the first index (actually counts)
    addu s1, gp, ra # s1 = color-indices + base-offset
    sw s5, 260(t0)  # store the address of next in the colors2 tag (doesn't do anything if ret)
    daddiu t9, t9, 3 # another 3 qw for the colors.
    sw s1, 212(t0)  # color + base goes in colors 1
    sll s1, s2, 2   # s1 = index * 4
    sh s2, 208(t0)  # color1's qwc = *index
    sll s2, s2, 4   # s2 = index * 16
    sb s1, 222(t0)  # set something in the vif tag of upload color 1
    daddu gp, gp, s2 # advance our colors ptrs
    lq s2, 192(t0)     # s2 = upload color 0
    daddiu s5, s5, 48  # inc next
    lq s1, 208(t0)     # s1 = upload color 1
    daddiu t8, t8, 48  # inc output
    lq s0, 256(t0)     # s0 = upload temp (2 or ret)
    daddiu s3, s3, 1   # inc the instance pointer
    sq s2, -48(t8)     # upload color 0 (seems to be, a constant?)
    daddiu s4, s4, -1  # decrement frag count.
    sq s1, -32(t8)     # upload color 1
    blez s4, L172      # did we run out of fragments?
    sq s0, -16(t8)     # upload color 2

# top of fragment loop
B43:
L160:
    daddiu s2, t9, -252 # did we run out of room in the scratchpad?
    sll r0, r0, 0
    blez s2, L163
    sll r0, r0, 0

# swap output buffer
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

# here we have an output buffer that has room for another fragment.
# and add another fragment.
B47:
L163:
    sll r0, r0, 0
    lbu s2, 144(s3) # load next index
    addu s1, gp, ra # s1 = color-indices + base-offset
    sw s5, 260(t0)  # store the address of next in the colors2 tag (doesn't do anything if ret)
    daddiu t9, t9, 3 # another 3 qw for the colors.
    sw s1, 212(t0)  # color + base goes in colors 1
    sll s1, s2, 2   # s1 = index * 4
    sh s2, 208(t0)  # color1's qwc = *index
    sll s2, s2, 4   # s2 = index * 16
    sb s1, 222(t0)  # set something in the vif tag of upload color 1
    daddu gp, gp, s2  # advance our colors ptrs
    # same as B42
    lq s2, 192(t0)
    daddiu s5, s5, 48
    lq s1, 208(t0)
    daddiu t8, t8, 48
    lq s0, 256(t0)
    daddiu s3, s3, 1
    sq s2, -48(t8)
    daddiu s4, s4, -1
    sq s1, -32(t8)
    bgtz s4, L160 # except we keep looping if there are fragments left.
    sq s0, -16(t8)

B48:
    beq r0, r0, L172
    sll r0, r0, 0

# s4 != 0 version
# I think, for the GENERIC renderer.
# wtf there's a square root...
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
# early exit for 1 in a per-8
L172:
    addiu a2, a2, -1  # decrement instance count
    srl t7, t7, 1     # update vis mask
    daddiu t4, t4, 64 # update instance ptr
    sll r0, r0, 0
    bne t7, r0, L148  # reloop, if we've got any left in the group of 8.
    lqc2 vf2, 16(t4)  # load the bsphere.

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; .function draw-inline-array-prototype-tie-asm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;BAD PROLOGUE
;; Warnings:
;; INFO: Flagged as asm by config
;; INFO: Assembly Function

# runs after the instance drawing.
# a0 - dma buffer
# a1 - num prototypes
# a2 - prototype array (a prototype-array-tie)

# t0 = work
# t1 = SPR from DMA bank
# t3 = SPR ptr to input buffer
# 


B0:
L84:
    daddiu sp, sp, -112
    sd ra, 0(sp)
    sq s1, 16(sp)
    sq s2, 32(sp)
    sq s3, 48(sp)
    sq s4, 64(sp)
    sq s5, 80(sp)
    sq gp, 96(sp)
    sll r0, r0, 0

    # SETUP
    lui a3, 28672      # = 0x7000
    lw v1, 4(a0)       # = dma buf base
    lui t1, 4096       # = 0x1000
    lui t2, 4096       # = 0x1000

    sync.l
    cache dxwbin v1, 0
    sync.l
    cache dxwbin v1, 1
    sync.l

    lw t0, *prototype-tie-work*(s7)
    ori t1, t1, 53248       # spr FROM
    ori t4, t2, 54272       # spr TO
    ori t3, a3, 16          # setup spr input buffer pointer
    ori t2, a3, 2064        # spr output buffer pointer
    sw a0, 10260(a3)        # stash dma buffer in spad (dma-buffer of prototype-tie-dma)
    daddiu t7, a1, -1       # not sure, is the input data length off by one or something?
    sll r0, r0, 0
    lw t5, 12(a2)           # t5 = prototype-bucket-tie
    addiu a0, r0, 0         # a0 = 0 (offset into the scratchpad's output offset)
    or a1, t2, r0           # a1 = spr output


B1:
L85:
    sll r0, r0, 0
    lq t6, 60(t5)          # load next's for the bucket (start of dma chains per geom, in 0, 1, 2, 3 order)
    daddiu t8, a2, 4       # t8 = array pointer (array prottype-bucket-tie), load at +12.
    sw t7, 10256(a3)       # stash length in spad
    dsrl32 a2, t6, 0       # a2 = 1's next
    sw t8, 280(t0)         # stash prototype-array
    pcpyud t8, t6, t6      # [2,3]
    lw t7, 140(t5)         # tie colors
    or t8, a2, t8          # t8 = any next's nonzero?
    lw a2, 108(t5)         # a2 = frag counts array
    beq t8, r0, L99        # skip ahead if 0 prototypes drawn.
    lw t8, 4(a3)           # load mood off of the terrain-context (spr)

B2:
    sll r0, r0, 0
    lq t5, 12(t5)          # load geom's from bucket
    sll r0, r0, 0
    sq t6, 10272(a3)       # stash next's on spad
    sll r0, r0, 0
    sw a2, 10304(a3)       # stash frag counts on spad
    sll r0, r0, 0
    sq t5, 10288(a3)       # stash geom's on spad
    sll r0, r0, 0
    ld a2, 272(t0)         # clamp constant: #x0080'00ff'00ff'00ff (for time of day color interp)
    sll r0, r0, 0
    lw t6, 4(t7)           # t6 = time of day palette's height
    daddiu ra, t7, 12      # ra = time of day palette's data
    lq t5, 1852(t8)        # t5 = itimes0 from mood
    sra t7, t6, 2          # t7 = width / 4
    sll r0, r0, 0
    addu t7, t7, a0        # some spad out buffer ptr, or something.
    addiu t9, r0, 221      # t9 = 221 (I think the max number of qws we can safely have used after colors)
    dsubu t7, t9, t7
    sll r0, r0, 0
    bgez t7, L88           # see if we have too much crap in the DMA output buffer
    sll r0, r0, 0

# dma output full, copy from scratchpad.
B3:
L86:
    lw a1, 0(t1)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi a1, a1, 256
    sll r0, r0, 0
    beq a1, r0, L87
    sll r0, r0, 0

B4:
    sll r0, r0, 0
    lw a1, 292(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu a1, a1, 1
    sll r0, r0, 0
    sw a1, 292(t0)
    beq r0, r0, L86
    sll r0, r0, 0

B5:
L87:
    sw t2, 128(t1)
    xori t2, t2, 4096
    sw v1, 16(t1)
    sll a1, a0, 4
    addu v1, v1, a1
    or a1, t2, r0
    sw a0, 32(t1)
    addiu a0, r0, 256
    sw a0, 0(t1)
    addiu a0, r0, 0

# here it is safe to output colors to the current scratchpad
# output buffer.
B6:
L88:
    addiu t7, t6, 31  # width + 31
    lq t6, 0(t0)      # upload palette 0
    sra t7, t7, 5     # (width + 31) / 5
    addiu a0, a0, 2   # 2 qw's, I guess for upload palette 0 and upload palette 1
    sll t9, t7, 5     # ((width + 31) / 5) * 5
    sq t6, 0(a1)      # store upload palette 0
    sra t6, t9, 2     # aligned width / 4
    sb t9, 30(t0)     # probably the viftag's unpack count, or something.
    addu a0, a0, t6   # going to use up (aligned width / 4) qw's in our dma buffer.
    sh t6, 16(t0)     # also put this in the giftag for upload1
    sll r0, r0, 0
    lq t6, 1868(t8)   # t6 = itimes1
    sll r0, r0, 0
    lq gp, 16(t0)     # gp = upload1's tag
    sll r0, r0, 0
    lq t7, 1884(t8)   # t7 = itimes2
    sll r0, r0, 0
    sq gp, 16(a1)     # store upload palette 1
    addiu a1, a1, 32  # advance output pointer (spr)
    lq t8, 1900(t8)   # t8 = itimes3

# begin color stuff
B7:
L89:
    lw gp, 0(t4)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi gp, gp, 256
    sll r0, r0, 0
    beq gp, r0, L90
    sll r0, r0, 0

B8:
    sll r0, r0, 0
    lw gp, 296(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu gp, gp, 1
    sll r0, r0, 0
    sw gp, 296(t0)
    beq r0, r0, L89
    sll r0, r0, 0

B9:
L90:
    sw ra, 16(t4)
    daddiu t9, t9, -32
    sw t3, 128(t4)
    addiu gp, r0, 64
    sw gp, 32(t4)
    addiu gp, r0, 256
    sw gp, 0(t4)
    daddiu ra, ra, 1024
B10:
L91:
    or s5, t3, r0
    xori t3, t3, 1024
    blez t9, L94
    daddiu t9, t9, -32

B11:
L92:
    lw gp, 0(t4)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi gp, gp, 256
    sll r0, r0, 0
    beq gp, r0, L93
    sll r0, r0, 0

B12:
    sll r0, r0, 0
    lw gp, 296(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu gp, gp, 1
    sll r0, r0, 0
    sw gp, 296(t0)
    beq r0, r0, L92
    sll r0, r0, 0

B13:
L93:
    sw ra, 16(t4)
    sll r0, r0, 0
    sw t3, 128(t4)
    addiu gp, r0, 64
    sw gp, 32(t4)
    addiu gp, r0, 256
    sw gp, 0(t4)
    daddiu ra, ra, 1024
    beq r0, r0, L95
    sll r0, r0, 0

B14:
L94:
    lw gp, 0(t4)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi gp, gp, 256
    sll r0, r0, 0
    beq gp, r0, L95
    sll r0, r0, 0

B15:
    sll r0, r0, 0
    lw gp, 296(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu gp, gp, 1
    sll r0, r0, 0
    sw gp, 296(t0)
    beq r0, r0, L94
    sll r0, r0, 0

B16:
L95:
    addiu gp, a1, 128
    lq s2, 12(s5)
    sll r0, r0, 0
    lq s4, 28(s5)
    pextlb s3, r0, s2
    mfc1 r0, f31
    pextub s2, r0, s2
    mfc1 r0, f31
    pmulth r0, s3, t5
    mfc1 r0, f31
    pextlb s3, r0, s4
    mfc1 r0, f31
    pmaddh r0, s2, t6
    mfc1 r0, f31
    pextub s4, r0, s4
    mfc1 r0, f31
    pmaddh r0, s3, t7
    lq s3, 44(s5)
    addiu s5, s5, 32
    sll r0, r0, 0
    pmaddh r0, s4, t8
    lq s4, 28(s5)
    pextlb s2, r0, s3
    mfc1 r0, f31
B17:
L96:
    pextub s3, r0, s3
    mfc1 r0, f31
    pmfhl.lh s1
    mfc1 r0, f31
    pmulth r0, s2, t5
    mfc1 r0, f31
    psrlh s2, s1, 6
    mfc1 r0, f31
    pcpyud s1, s2, s2
    mfc1 r0, f31
    paddh s2, s1, s2
    mfc1 r0, f31
    pminh s2, s2, a2
    mfc1 r0, f31
    ppacb s1, r0, s2
    mfc1 r0, f31
    pextlb s2, r0, s4
    mfc1 r0, f31
    pmaddh r0, s3, t6
    sw s1, 0(a1)
    pextub s4, r0, s4
    mfc1 r0, f31
    pmaddh r0, s2, t7
    lq s3, 44(s5)
    addiu s5, s5, 32
    addiu a1, a1, 4
    pmaddh r0, s4, t8
    lq s4, 28(s5)
    bne a1, gp, L96
    pextlb s2, r0, s3

B18:
    bgez t9, L91
    sll r0, r0, 0

# done with colors
B19:
    sll r0, r0, 0
    lw a2, 10276(a3)  # next1
    sll r0, r0, 0
    lw t6, 10292(a3)  # geom1
    beq a2, r0, L97
    lbu t5, 10305(a3) # frag1

B20:
    bgezal r0, L100   # call sub at L100 for adding the geom.
    sll r0, r0, 0

B21:
L97:
    sll r0, r0, 0
    lw a2, 10280(a3)
    sll r0, r0, 0
    lw t6, 10296(a3)
    beq a2, r0, L98
    lbu t5, 10306(a3)

B22:
    bgezal r0, L100
    sll r0, r0, 0

B23:
L98:
    sll r0, r0, 0
    lw a2, 10284(a3)
    sll r0, r0, 0
    lw t6, 10300(a3)
    beq a2, r0, L99
    lbu t5, 10307(a3)

B24:
    bgezal r0, L100
    sll r0, r0, 0

# early exit if we didn't draw any of this protytpe
B25:
L99:
    sll r0, r0, 0
    lw a2, 280(t0)
    sll r0, r0, 0
    lw t6, 10256(a3)
    sll r0, r0, 0
    lw t5, 12(a2)
    bne t6, r0, L85
    daddiu t7, t6, -1

B26:
    beq r0, r0, L105
    sll r0, r0, 0

# geom upload thing.
B27:
L100:
    addiu t6, t6, 32 # offset of first frag within a prototype-tie
    sll r0, r0, 0
B28:
L101:
    addiu t7, a0, 4  # looks like we're going to use 4 qw's of our buffer
    addiu t8, r0, 255 # and we can go all the way to the end this time (last time we did 251)
    dsubu t7, t8, t7
    lw t8, 0(t6)      # t8 = gif ref
    bgez t7, L104
    lhu t7, 30(t6)

B29:
L102:
    lw a1, 0(t1)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi a1, a1, 256
    sll r0, r0, 0
    beq a1, r0, L103
    sll r0, r0, 0

B30:
    sll r0, r0, 0
    lw a1, 292(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu a1, a1, 1
    sll r0, r0, 0
    sw a1, 292(t0)
    beq r0, r0, L102
    sll r0, r0, 0

B31:
L103:
    sw t2, 128(t1)
    xori t2, t2, 4096
    sw v1, 16(t1)
    sll a1, a0, 4
    addu v1, v1, a1
    or a1, t2, r0
    sw a0, 32(t1)
    addiu a0, r0, 256
    sw a0, 0(t1)
    addiu a0, r0, 0
# got enough room, make the output for this frag:
# a2 = next, t6 = tie-fragment, t5 = frag count, 
B32:
L104:
    sll r0, r0, 0
    lw s5, 4(t6)   # s5 = point ref
    sll r0, r0, 0
    lhu s4, 28(t6) # s4 = tex-count
    sll r0, r0, 0
    lhu gp, 32(t6) # gp = vertex count
    sll r0, r0, 0
    sw t8, 36(t0)  # store gif ref ptr in upload model 0
    sll r0, r0, 0
    sh s4, 32(t0)  # store tex count in upload 0
    daddiu t9, s4, 16384 # what's this doing... something with the unpack. maybe an offset.
    sb s4, 46(t0)  # store tex count in upload 0's vif tag
    dsll s4, s4, 4 # s4 now bytes
    sw s5, 68(t0)  # point ref in upload 2
    daddu t8, t8, s4 # advance gif ref
    sh gp, 64(t0)   # vertex count in upload 2 dma
    dsll gp, gp, 1  # multiply by 2 (I guess upacks to 2x as big?)
    sw a2, 84(t0)   # store next in upload 3
    sll r0, r0, 0
    sb gp, 78(t0)  # vertex count * 2 in upload 2 viftag unpack
    sll r0, r0, 0
    sw t8, 52(t0)  # ?? in upload 1 (more "gif" stuff)
    sll r0, r0, 0
    sh t7, 48(t0)  # upload 1 gets gif-count
    dsll t7, t7, 2 # also unpacks
    sh t9, 60(t0)  # really not sure what this is here...
    sll r0, r0, 0
    sb t7, 62(t0)  # unpack count for the extra gif stuff.
    sll r0, r0, 0
    lq t7, 32(t0)  # t7 = upload0
    sll r0, r0, 0
    lq t8, 48(t0)  # t8 = upload1
    sll r0, r0, 0 
    lq t9, 64(t0)  # t9 = upload2
    sll r0, r0, 0
    lq gp, 80(t0)  # gp = upload3
    daddiu a0, a0, 4
    sq t7, 0(a1)
    daddiu t5, t5, -1
    sq t8, 16(a1)
    daddiu a2, a2, 48
    sq t9, 32(a1)
    sq gp, 48(a1)
    daddiu a1, a1, 64
    bgtz t5, L101
    daddiu t6, t6, 64

B33:
    jr ra
    sll r0, r0, 0
# end of geom upload subroutine

B34:
L105:
    beq a0, r0, L108
    sll r0, r0, 0

B35:
L106:
    lw a1, 0(t1)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi a1, a1, 256
    sll r0, r0, 0
    beq a1, r0, L107
    sll r0, r0, 0

B36:
    sll r0, r0, 0
    lw a1, 292(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu a1, a1, 1
    sll r0, r0, 0
    sw a1, 292(t0)
    beq r0, r0, L106
    sll r0, r0, 0

B37:
L107:
    sw t2, 128(t1)
    sll r0, r0, 0
    sw v1, 16(t1)
    sll a1, a0, 4
    addu v1, v1, a1
    sll r0, r0, 0
    sw a0, 32(t1)
    addiu a0, r0, 256
    sw a0, 0(t1)
    sll r0, r0, 0
B38:
L108:
    lw a0, 0(t1)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi a0, a0, 256
    sll r0, r0, 0
    beq a0, r0, L109
    sll r0, r0, 0

B39:
    sll r0, r0, 0
    lw a0, 292(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu a0, a0, 1
    sll r0, r0, 0
    sw a0, 292(t0)
    beq r0, r0, L108
    sll r0, r0, 0

B40:
L109:
    lw a0, 10260(a3)
    sll r0, r0, 0
    sw v1, 4(a0)
    sll r0, r0, 0
    or v0, r0, r0
    ld ra, 0(sp)
    lq gp, 96(sp)
    lq s5, 80(sp)
    lq s4, 64(sp)
    lq s3, 48(sp)
    lq s2, 32(sp)
    lq s1, 16(sp)
    jr ra
    daddiu sp, sp, 112

    sll r0, r0, 0
    sll r0, r0, 0