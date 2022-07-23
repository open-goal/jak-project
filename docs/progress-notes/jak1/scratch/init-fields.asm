;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; .function sp-init-fields!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;BAD PROLOGUE
;; Warnings:
;; INFO: Flagged as asm by config
;; INFO: Assembly Function

# inputs: a0 output-data (multiple types)
#         a1 init-spec-array
#         a2 start-field-idx
#         a3 end-field-idx (not inclusive, not that it matters?)
#         t0 usually #f.


## Initialize
B0:
L155:
    or v1, a0, r0
    or v1, a2, r0
    or v1, a3, r0
    or v1, t0, r0
    sll r0, r0, 0
    daddiu a2, a2, 1    # actual first field to care about.
    or v0, a1, r0       # v0 = current field?

## Loop to advance our init spec until we hit a field >= our minimum
B1:
L156:
    lh a1, 0(v0)        # a1 = field-id
    sll r0, r0, 0
    dsubu a1, a1, a2    # 
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    bltzl a1, L156
B2:
    daddiu v0, v0, 16

## If our range is size 0 (or negative), exit early, we have no fields to initialize
B3:
    dsubu a1, a2, a3
    sll r0, r0, 0
    bgez a1, L169
    sll r0, r0, 0


# LOOP TOP:
# v0 = init-spec, a2 = field to init, 
B4:
L157:
    lh a1, 0(v0)
    sll r0, r0, 0
    bne a1, a2, L167 # jump to end if field doesn't match spec.
    vrget.xyzw vf1

B5:
    vsqrt Q, vf1.x
    lh a1, 2(v0)        # a1 = flags
    vaddq.x vf2, vf0, Q # more random?
    lw t2, 8(v0)        # t2 = random-range (float or int possible here)
    addiu v1, r0, 7
    beq a2, v1, L159    # spt-sound
    addiu t1, r0, 1

B6:
    beq a1, t1, L160    # flags = 1.
    addiu t1, r0, 2

B7:
    beq a1, t1, L162    # flags = 2.
    addiu t1, r0, 3

B8:
    beq a1, t1, L163    # flags = 3
    addiu t1, r0, 5

B9:
    beq a1, t1, L164    # flags = 5
    addiu t1, r0, 6

B10:
    beq a1, t1, L165   # flags = 6
    addiu t1, r0, 4

B11:
    beq a1, t1, L166   # flags = 4
    sll r0, r0, 0

B12:
    beq t2, r0, L158  # flags = 0
    sll r0, r0, 0

B13:
    vrxorw vf2   # other flags
    lw t1, 12(v0)
    vrnext.xyzw vf1
    lw t3, 4(v0)
    vsubw.xyzw vf1, vf1, vf0
    sll r0, r0, 0
    qmtc2.i vf2, t2
    sll r0, r0, 0
    vitof0.xyzw vf2, vf2
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf2
    sll r0, r0, 0
    vftoi0.xyzw vf1, vf1
    sll r0, r0, 0
    qmfc2.i t2, vf1
    sll r0, r0, 0
    mult3 t2, t2, t1
    sll r0, r0, 0
    daddu t2, t2, t3
    daddiu a2, a2, 1
    sw t2, 0(a0)
    daddiu a0, a0, 4
    bne a2, a3, L157
    daddiu v0, v0, 16

B14:
    jr ra
    sll r0, r0, 0

B15:
L158:  # flags = 0
    lw t3, 4(v0)      ## int32
    daddiu a2, a2, 1
    sw t3, 0(a0)
    daddiu a0, a0, 4
    bne a2, a3, L157
    daddiu v0, v0, 16

B16:
    jr ra
    sll r0, r0, 0

B17:
L159: ## special case for sound.
    lw t3, 4(v0)
    daddiu a2, a2, 1
    sw t3, 0(a0)
    daddiu a0, a0, 4
    bne a2, a3, L157
    daddiu v0, v0, 16

B18:
    jr ra
    sll r0, r0, 0

B19:
L160:                  ## flags = 1
    beq t2, r0, L161   # skip if range = 0
    vrxorw vf2

B20:
    vrnext.xyzw vf1
    lw t1, 12(v0)                  ## t1 = multiplier
    vsubw.xyzw vf1, vf1, vf0
    lw t3, 4(v0)                   ## t3 = initial-value
    qmtc2.i vf2, t2
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf2
    sll r0, r0, 0
    qmtc2.i vf2, t1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf2
    sll r0, r0, 0
    qmtc2.i vf2, t3
    sll r0, r0, 0
    vadd.xyzw vf1, vf1, vf2
    sll r0, r0, 0
    qmfc2.i t2, vf1
    daddiu a2, a2, 1
    sw t2, 0(a0)
    daddiu a0, a0, 4
    bne a2, a3, L157
    daddiu v0, v0, 16

B21:
    jr ra
    sll r0, r0, 0

B22:
L161:
    lw t3, 4(v0)
    daddiu a2, a2, 1
    sw t3, 0(a0)
    daddiu a0, a0, 4
    bne a2, a3, L157
    daddiu v0, v0, 16

B23:
    jr ra
    sll r0, r0, 0

B24:
L162:
    beq t2, r0, L161 ## flags = 2
    vrxorw vf2

B25:
    vrnext.xyzw vf1
    lw t1, 12(v0)
    vsubw.xyzw vf1, vf1, vf0
    daddiu t2, t2, 1
    qmtc2.i vf2, t2
    lw t3, 4(v0)
    vitof0.xyzw vf2, vf2
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf2
    sll r0, r0, 0
    vftoi0.xyzw vf1, vf1
    sll r0, r0, 0
    vitof0.xyzw vf1, vf1
    sll r0, r0, 0
    qmtc2.i vf2, t1
    sll r0, r0, 0
    vmul.xyzw vf1, vf1, vf2
    sll r0, r0, 0
    qmtc2.i vf2, t3
    sll r0, r0, 0
    vadd.xyzw vf1, vf1, vf2
    sll r0, r0, 0
    qmfc2.i t2, vf1
    daddiu a2, a2, 1
    sw t2, 0(a0)
    daddiu a0, a0, 4
    bne a2, a3, L157
    daddiu v0, v0, 16

B26:
    jr ra
    sll r0, r0, 0

B27:
L163:                  ## flags = 3
    lw t1, 4(v0)        # t1 = init-val
    sll r0, r0, 0
    dsll t1, t1, 2   # val * 4
    sll r0, r0, 0
    daddu t1, t1, a0   # t1 = dest + val*4
    sll r0, r0, 0
    lw t3, 0(t1)
    daddiu a2, a2, 1
    sw t3, 0(a0)
    daddiu a0, a0, 4
    bne a2, a3, L157
    daddiu v0, v0, 16

B28:
    jr ra
    sll r0, r0, 0

B29:
L164:                ## flags = 5
    lw t1, 4(v0)
    sll r0, r0, 0
    lw t3, 0(t1)
    daddiu a2, a2, 1
    sw t3, 0(a0)
    daddiu a0, a0, 4
    bne a2, a3, L157
    daddiu v0, v0, 16

B30:
    jr ra
    sll r0, r0, 0

B31:
L165:
    lw t1, *part-id-table*(s7)  ## flags = 6
    sll r0, r0, 0
    lw t3, 4(v0)
    sll r0, r0, 0
    dsll t3, t3, 2
    daddiu t1, t1, 12
    daddu t3, t3, t1
    sll r0, r0, 0
    lw t2, 0(t3)
    daddiu a2, a2, 1
    sw t2, 0(a0)
    daddiu a0, a0, 4
    bne a2, a3, L157
    daddiu v0, v0, 16

B32:
    jr ra
    sll r0, r0, 0

B33:
L166:
    lw t3, 4(v0) ## flags = 4
    daddiu a2, a2, 1
    sw t3, 0(a0)
    daddiu a0, a0, 4
    bne a2, a3, L157
    daddiu v0, v0, 16

B34:
    jr ra
    sll r0, r0, 0

B35:
L167:
    bnel t0, s7, L168
B36:
    sw r0, 0(a0)

B37:
L168:
    daddiu a2, a2, 1
    daddiu a0, a0, 4
    bne a2, a3, L157
    sll r0, r0, 0

B38:
L169:
    jr ra
    sll r0, r0, 0

    jr ra
    daddu sp, sp, r0

    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
