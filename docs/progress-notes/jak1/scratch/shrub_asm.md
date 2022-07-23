# Shrub Renderer

The shrub renderer is part of the background system. Each level probably has 1 or 0 `drawable-tree-instance-shrub`s, containing all of the shrubs in that level (if it has any shrubs).

Because the shrub renderer is part of the background system, actual DMA generation happens in `finish-background`.

## Original Design
In `shrub`, there are prototypes and instances.  Each "prototype" defines a model (like a bush, tree, etc).  Each "instance" is a particular placement of a prototype in the world.

Each "prototype" has 4 different geometries. Some of the geometries can be missing:
- prototype-generic-shrub
- prototype-shrubbery
- prototype-trans-shrubbery
- billboard

The first two are believed to have the same data, but if the shrub is very close to the player and partially off-screen, it must be scissored, and only the `generic` renderer supports scissoring. 

The `prototype-trans-shrubbery` allows shrubs to fade away. It's likely that the format is extremely similar, or even the exact same. 

The `billboard` is a single quad.

Effects:
- Time of Day lighting. It looks like each "drawable-tree-instance-shrub" has a time-of-day color palette that is adjusted based on the time of day
- Per-instance time of day lighting.  Each instance may use different colors.
- Wind effect. This applies an additional transformation matrix per instance.


## Our Design
We will ignore the prototype-generic-shrub - OpenGL will take care of scissoring for us.

Like with tfrag/tie, we will do the time of day interpolation in C++.

The shrubs without wind effect will be converted into a single giant mesh. Doing it as a single mesh reduces the number of draw calls, and the entire mesh can be left in GPU memory the whole time.

The shrubs with wind effect will be drawn as individual instances, as different shrubs need different wind matrices. It's likely going to be similar to `render_tree_wind`.

The time-of-day effect will be done like in tfrag/tie. We will create a new time of day texture on each frame, based on the current time, and each vertex will index into a single large texture. This approach is nice because the interpolation/upload can be done in a single large batch.


## Setup Before (in `background.gc`)
The shrub system doesn't use the precomputed visibility strings, so we can ignore this.

- The `background-upload-vu0` function loads `vf16-vf31` with various math camera values.
- The `background-upload-vu0` function loads hte `background-vu0-block` program to VU0 and runs the subroutine at 0.
- The current level index (0 or 1) is stored in the scratchpad (as a `terrain-context`)
- The time of day colors are calculated with `time-of-day-interp-colors`. The colors are stored in `*instance-tie-work*`.  We can move this to C++ and do it faster.

After setup, the main function to generate DMA is `draw-drawable-tree-instance-shrub`. This function will be removed in the PC port. Instead, we will send the C++ code some data:
- camera matrix
- name of the level


## `draw-drawable-tree-instance-shrub`
Basic outline
- Reset the `instance-shrub-work`
- Check if renderer is enabled
- Call `draw-inline-array-instance-shrub`. Each prototype has a "bucket" containing a linked list of instances. This function adds the instances to the buckets.
- Call `draw-prototype-inline-array-shrub`.  This builds the final DMA list from the buckets.
- Various performance counter things that we can ignore.

## `draw-inline-array-instance-shrub`
Args:
- `a0` dma buffer
- `a1` inline array of `draw-node` (a usual draw-node BVH with child type `instance-shrubbery`)
- `a2` length of this array
- `a3` inline array of `prototype-bucket-shrub`

```lisp
B0: ;; block 0: one-time setup
L57:

;; Function prologue
    daddiu sp, sp, -32
    sd ra, 0(sp)
    sq gp, 16(sp)

    lui t3, 28672                      ;; t3 = 0x70000000, the scratchpad
    lw v1, 4(a0)                       ;; v1 = (-> dma-buf base). we'll be writing DMA data here.
    lui t2, 4096                       ;; t2 = 0x10000000 (used later)
    lui t1, 4096                       ;; t1 = 0x10000000 (used later)

;; this does some data cache stuff. we don't have to worry about it.
    sync.l
    cache dxwbin v1, 0
    sync.l
    cache dxwbin v1, 1
    sync.l


    lw t0, *instance-shrub-work*(s7)  ;; t0 = instance-shrub-work. This stores many temporary variables.
    ori t5, t2, 54272                 ;; t5 = 0x1000D400 (DMA SPR_TO register)
    sw a0, 6524(t0)                   ;; stash dma-buf argument in instance-shrub-work.dma-buffer
    ori a0, t1, 53248                 ;; a0 = 0x1000D000 (DMA SPR_FROM register)
    lw t2, *wind-work*(s7)            ;; t2 = *wind-work*

;; note on crazy scratchpad stuff.
;; to get faster speed, it is useful to have both the input (instances) and output (DMA data) stored
;; in the scratchpad.  However, the scratchpad is not big enough to store everything.

;; they divide the scrachpad in 4:
;; 0-5200 is one "instance" buffer
;; 5200-10400 is the other "instance" buffer
;; 10400-12448 is on "out" buffer
;; 12448-end is the other "out" buffer.
;; This code reads instance data from one instance buffer and writes DMA data to one out buffer.
;; while this is happening, the SPR_TO/SPR_FROM channels will be copying the next instances to
;; the other instance buffer, and copying the output dma back into the dma-buf.
;; Once they are done, the buffers will swapped. So there is continuous copying and processing.

;; I will use notation like spad.instance-buf and spad.out-buf to indicate the scratchpad buffers.
;; There are two instance buffers, and we don't have to really care which one they are using -
;; we can assume that they implemented double buffering properly.

    ori t1, t3, 10416                 ;; t1 = spad.out-buf (high buffer)
    sw r0, 6544(t0)                   ;; instance-work.chains = 0
    ;; Note on "stack"
    ;; this draw-node tree is... a tree.
    ;; this drawing function traverses the tree.
    ;; in order to traverse a tree, you need something like a stack.
    ;; the tree has a fixed max depth of 6
    ;; The node/length fields of the instance-shrub-work are this stack.
    ;; t4 is the "stack pointer". It points to instance-shrub-work + 4*depth.
    ;; Then you can access at the normal offsets of node/length to access the correct
    ;; slot for your stack frame.

    or t4, t0, r0                     ;; t4 = instance-work (todo, why?)
    lqc2 vf3, 6064(t0)                ;; vf3 = instance-work.constants (128, 1.0, 0.0, fog0)
    sw t5, 6412(t0)                   ;; instance-work.to-spr = 0x1000D400 (just stashing this here for later)
    ori t6, t3, 16                    ;; t6 = spad.instance-buf (low buffer)
    addiu t7, r0, 720                 ;; t7 = 720
    sw a3, 6476(t0)                   ;; instance-work.prototypes = the input inline array of prototypes
    addiu t3, r0, 0                   ;; t3 = 0
    sw a3, 6404(t0)                   ;; instance-work.bucket-ptr = the input inline array of prototypes
    addiu a3, r0, 0                   ;; a3 = 0
    sw a1, 6428(t4)                   ;; instance-work.node = the input draw node. (note, we're using t4 here)
    or t3, t1, r0                     ;; t3 = spad.out-buf
    sw a2, 6452(t4)                   ;; instance-work.length = the input length (num draw nodes at this level)
    addiu a1, r0, -1                  ;; a1 = -1
    sw t7, 6516(t0)                   ;; instance-work.current-shrub-near-packet = 720 (?) 
    daddiu t7, t0, 48                 ;; t7 = instance-work.chaina
    sw t6, 6408(t0)                   ;; instance-work.src-ptr = spad.instance-buf
    daddiu a2, t0, 176                ;; a2 = instance-work.chainb
    sw t6, 6388(t0)                   ;; instance-work.instance-ptr = spad.instance-buf
    daddiu t6, r0, -64                ;; t6 = -64
    sw t5, 6412(t0)                   ;; instance-work.to-spr = 0x1000D4000 (oops, did it twice)
    ;; note on alignment.
    ;; the instance-shrub-work object is only 16-byte aligned.
    ;; but, for some reason, they want these chaina/chainb things to be 64 byte aligned.
    ;; they put a 48 byte "dummy" field before them, and and with -64 to get aligned versions.
    ;; I'll call these aligned versions chaina-aligned/chainb-aligned
    and t5, t7, t6                    ;; t5 = chaina-aligned
    sw a0, 6416(t0)                   ;; instance-work.from-spr = 0x1000D000
    and a2, a2, t6                    ;; a2 = chainb-aligned
    sw t5, 6392(t0)                   ;; instance-work.chain-ptr = chaina-aligned
    addiu t5, r0, -1                  ;; t5 = -1
    sw a2, 6396(t0)                   ;; instance-work.chain-ptr-next = chainb-aligned
    sll r0, r0, 0                     ;; nop
    sw t4, 6400(t0)                   ;; instance-work.stack-ptr = t4 (right now, at base)
    sll r0, r0, 0                     ;; nop
    sw t5, 6540(t0)                   ;; instance-work.last-shrubs = -1
    sll r0, r0, 0                     ;; nop
    sw r0, 6548(t0)                   ;; instance-work.flags = 0
    sll r0, r0, 0                     ;; nop
    sw r0, 6560(t0)                   ;; instance-work.inst-count = 0
    sll r0, r0, 0                     ;; nop
    sw r0, 6556(t0)                   ;; instance-work.node-count = 0

;; Note on vcallms 17. this is a tiny program that loads vf's
;; plane is the culling planes (in normal world coordinates)
;; vf24-vf27 use the camera-rot matrix. This confusingly also includes the
;; translation, but does not include the projection matrix.
;; each vector is just the z component of that camera vector repeated 4 times
;; (it's computed in the vcallms 0 of background-upload-vu0)
;;  lq.xyzw vf16, 0(vi00)      |  nop      ;; plane0                
;;  lq.xyzw vf17, 1(vi00)      |  nop      ;; plane1                
;;  lq.xyzw vf18, 2(vi00)      |  nop      ;; plane2                
;;  lq.xyzw vf19, 3(vi00)      |  nop      ;; plane3                
;;  lq.xyzw vf24, 12(vi00)     |  nop      ;; [cam-rot0.z cam0-rot.z cam0-rot.z cam0-rot.z]                
;;  lq.xyzw vf25, 13(vi00)     |  nop      ;; same but cam-rot1                  
;;  lq.xyzw vf26, 14(vi00)     |  nop :e                   
;;  lq.xyzw vf27, 15(vi00)     |  nop    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
B1:
L58: ;; LOOP TOP. We reach here when we want to explore a new draw node.
    vcallms 17        ;; set up vf registers
    lw t4, 6400(t0)   ;; t4 = instance-work.stack-ptr
    addiu t5, r0, 7   ;; t5 = 7 (remaining instances in group. we find up to 7 visible instances)
    lw a2, 6392(t0)   ;; a2 = instance-work.chain-ptr
    sll r0, r0, 0     ;; nops, I guess to wait for the vu program?
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0

;; starting here, we're looking for a node that we can draw.
;; this is doing "sphere in view frustum" culling through the BVH tree
;; it will exit once it's found the next visible thing to draw.
;; the details here are:
;; - normal "can we see the sphere?" check
;; - also a distance from the camera check. If we fail that, skip.
;; - this builds DMA, but not drawing DMA. It builds DMA to upload the thing to the scratchpad.
;; once we find it, go to L63.
B2:
L59:
    dsubu t7, t4, t0 ;; t7 = 0 if at root of tree, negative otherwise
    lw t6, 6452(t4)  ;; t6 = length at current stack frame
    bltz t7, L63     ;; if we're not at one of the roots, draw it. we wouldn't have added it otherwise.
    lw t8, 6428(t4)  ;; t8 = node

;; we'll only get here if we're at the root. We have no idea if the roots are visible or not
    beq t6, r0, L62  ;; if no nodes, skip!
    lqc2 vf2, 12(t8) ;; vf2 = bsphere of the node

;; note that this code assumes we're deep enough to find instance-shrubs.
;; and sets up DMA to DMA them to the scratchpad for later processing.
;; but, we might have only found draw-nodes.
;; this is okay. The DMA we set up here will only be used if we actually find instance-shrubs.
;; we also set up the stack for more draw nodes. Again, it's okay because we'll only actually increment
;; the stack pointer if we find out that there are more levels.
;; the bsphere culling code for draw nodes/instances are identical, so that part
;; can be used in either case.
B4:
    sll r0, r0, 0                     ;; nop
    lqc2 vf6, -4(t8)                  ;; vf6.w = distance of the node. (other stuff is junk I think)
    vmulax.xyzw acc, vf16, vf2        ;; sphere in view frustum (will eventually put result in vf4)
    lbu t6, 3(t8)                     ;; t6 = node flags
    vmadday.xyzw acc, vf17, vf2       ;; sphere in view frustum
    lw t7, 4(t8)                      ;; t7 = node child
    vmaddaz.xyzw acc, vf18, vf2       ;; sphere in view frustum
    lbu t8, 2(t8)                     ;; t8 = node child count
    vmsubaw.xyzw acc, vf19, vf0       ;; sphere in view frustum
    lq t9, 6016(t0)                   ;; t9 = instance-work.dma-ref
    vmaddw.xyzw vf4, vf1, vf2         ;; sphere in view frustum (done!, vf4 now has signed distance from planes)
    sw t7, 6432(t4)                   ;; place child on stack
    vmulaw.xyzw acc, vf1, vf6         ;; acc = [dist, dist, dist, dist]
    sw t8, 6456(t4)                   ;; place child's length on stack
    vmsubax.xyzw acc, vf24, vf2       ;; dist calc (note, just for computing z)
    sq t9, 0(a2)                      ;; store dma-ref in chain-ptr
    vmsubay.xyzw acc, vf25, vf2       ;; more dist calc
    daddiu t9, t7, -4                 ;; t9 = node minus type tag
    vmsubaz.xyzw acc, vf26, vf2       ;; more dist calc
    sll t7, t8, 2                     ;; t7 = num children * 4
    qmfc2.i ra, vf4                   ;; ra = sphere/plane signed distances
    addu t7, t7, t8                   ;; t7 = num children * 5
    vmsubaw.xyzw acc, vf27, vf0       ;; more dist calc
    sw t9, 4(a2)                      ;; store address of draw nodes in the dma tag
    vmaddw.xyzw vf7, vf1, vf2         ;; finish dist calc
    sw t8, 8(a2)                      ;; stash the child count after the dma tag (space unused)
    pcgtw t8, r0, ra                  ;; check signed distance to planes
    lw t9, 6452(t4)                   ;; t9 = current stack length
    ppach ra, r0, t8                  ;; pack so signed distance compares are in lower 64
    lw t8, 6428(t4)                   ;; t8 = node
    bne ra, r0, L61                   ;; branch on reject
    sb t7, 0(a2)                      ;; store qwc in chain

;; if we reach here, we passed the sphere in view check
B5:
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu t7, t9, -1    ;; t7 = stack length - 1
    qmfc2.i t9, vf7      ;; t9 = dist check result
    daddiu t8, t8, 32    ;; advance to next node (assuming draw nodes)
    sll r0, r0, 0
    bltz t9, L61         ;; branch if failed dist check
    sll r0, r0, 0

B6:
    beq t6, r0, L60      ;; check if we actually reached the instances (0 = instances).
    sll r0, r0, 0        ;; 
B7:
    beq r0, r0, L59      ;; didn't reach instances. need to go deeper in tree!
    daddiu t4, t4, 4     ;; inrease stack depth. branch will find visible things.

;; if we reach here:
;; - we've reached leaves (instances)
;; - the instance is visible
;; - we have a chain set up to DMA it to the scratchpad.
B8:
L60:
    daddiu a2, a2, 16  ;; advance dma building pointer (looks like we have room for up to 8)
    sw t7, 6452(t4)    ;; decrement stack length (we're done with this one)
    daddiu t5, t5, -1  ;; decrement instance count (counts down from 7, we can only do 7 in a group)
    sw t8, 6428(t4)    ;; increment node in stack
    blez t5, L63       ;; goto L63 if we're full for this group
    dsubu t6, t4, t0   ;; check if we're at the root still

B9:
    bgtz t7, L59       ;; not full, more at this level.
    sll r0, r0, 0

B10:
    blez t6, L63       ;; if we're at the root of the tree and the lenth is zero, we're done, draw what we have.
    daddiu t4, t4, -4  ;; "return" and decrement sp (go up a level, we finished exploring this one)

;; common "advance to next based on stack"
;; we might have to return multiple levels, and this loop here does this.
B11:
L61:
    sll r0, r0, 0
    lw t7, 6452(t4) ;; t7 = length
    sll r0, r0, 0
    lw t6, 6428(t4) ;; t6 = node
    daddiu t7, t7, -1 ;; dec
    dsubu t8, t4, t0 ;; depth check
    daddiu t6, t6, 32 ;; inc node
    sw t7, 6452(t4)   ;; store len
    bgtz t7, L59      ;; keep going if not done (break out of returning loop)
    sw t6, 6428(t4)   ;; store node

B12:
    blez t8, L63     ;; draw if we're at the end.
    sll r0, r0, 0

B13:
L62:
    beq r0, r0, L61 ;; reloop in the return loop
    daddiu t4, t4, -4 ;; ascend one level

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DMA TO SPR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; if we reach here, we've got a chain set up that will send visible instances to the SPR.
B14:
L63:
    sll r0, r0, 0     ;; nop
    sw t4, 6400(t0)   ;; store draw node stack pointer in instance-shurb-work
    sll r0, r0, 0     ;; nop
    lw t5, 6392(t0)   ;; t5 = instance-work.chain-ptr (the start of the visible instance chain we just made)
    sll r0, r0, 0     ;; nop
    lw t4, 6412(t0)   ;; t4 = instance-work.to-spr (EE DMA control register address)
    beq t5, a2, L66   ;; will be equal if we didn't have any DMA
    lq t5, 6032(t0)   ;; dma-end (an 'end packet)

;; if we get here, we actually have data to send

;; these two blocks just wait until any in-progress to-sprs finish.
;; every iteration of the loop increments the "wait-to-spr" counter
;; (they likely tuned this code to reduce waits by moving stuff around)
B15:
L64:
    lw t6, 0(t4)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t6, t6, 256
    sll r0, r0, 0
    beq t6, r0, L65
    sll r0, r0, 0

B16:
    sll r0, r0, 0
    lw t6, 6568(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu t6, t6, 1
    sll r0, r0, 0
    sw t6, 6568(t0)
    beq r0, r0, L64
    sll r0, r0, 0

;; when we get here, there is no in-progress spr-to transfer

B17:
L65:
    sll r0, r0, 0        ;; nop
    lw t6, 6544(t0)      ;; t6 = instance-work.chains (just a counter of how many spad uploads we do)
    sll r0, r0, 0        ;; nop
    sq t5, 0(a2)         ;; store the end DMA tag (must go at the end of the DMA transfer)
    lw t5, 6392(t0)      ;; t5 = instance-work.chain-ptr (start of the DMA chain) 
    addiu a2, r0, 324    ;; a2 = 324 (constant to start DMA)
    lw t7, 6396(t0)      ;; t7 = instance-work.chain-ptr-next (to-spr chain dma mem is double buffered)
    ori t8, r0, 65535    ;; t8 = 65535
    sw t5, 6396(t0)      ;; instance-work.chain-ptr-next = chain-ptr (swap!)
    daddiu t6, t6, 1     ;; increment chain count
    sw t7, 6392(t0)      ;; instance-work.chain-ptr = chain-ptr-next (swap!)
    or t7, t5, r0        ;; t7 = chain for next time
    sll r0, r0, 0        ;; nop
    sw t6, 6544(t0)      ;; write back incremented chain count
    sll r0, r0, 0        ;; nop
    lw t6, 6388(t0)      ;; t6 = instance-work.instance-ptr (the scratchpad destination for the instance)
    sync.l
    cache dxwbin t7, 0   ;; write back the data (required before DMAing, EE DMA bypasses CPU caches)
    sync.l
    cache dxwbin t7, 1
    sync.l
    daddiu t7, t7, 64
    sync.l
    cache dxwbin t7, 0
    sync.l
    cache dxwbin t7, 1
    sync.l
    sw t6, 128(t4)      ;; set up destination addr in DMA register
    sw t5, 48(t4)       ;; set up source addr
    xori t5, t6, 5232   ;; toggle destination pointer (scratchpad destinations are double buffered)
    sw r0, 32(t4)       ;; set qwc = 0 (I think it's ignored in chain mode)
    sync.l
    sw a2, 0(t4)        ;; start transfer!
    sync.l
    sll r0, r0, 0
    sw t5, 6408(t0) ;; store instance-work.src-ptr
    beq r0, r0, L68 ;; always go to L68!
    sw t5, 6388(t0) ;; store instance-work.instance-ptr (starting a new block, so equal to src-ptr)

;; if we reach here, it's because we didn't have any more visible instances.
;; we have two cases:
;; 1). we have stuff in scratchpad (the other buffer) waiting to be drawn.
;; 2). nothing was visible, so we have nothing in scratchpad.
;; we can tell these two cases from the sign of the a1 flag.
B18:
L66:
    bltz a1, L98    ;; goto end (L98) if the flag is negative
    lw a2, 6388(t0) ;; a2 = instance-work.instance-ptr.

B19:
    sll r0, r0, 0
    sw r0, 6540(t0)  ;; instance-work.last-shrubs = 0
    sll r0, r0, 0
    xori a2, a2, 5232  ;; flip spad buffer (the last group isn't double buffered)
    sll r0, r0, 0
    sw a2, 6408(t0)    ;; store src-ptr
    sll r0, r0, 0
    sw a2, 6388(t0)    ;; store instance-ptr

;; dma sync - make sure the last to-spr is done.
B20:
L67:
    lw a2, 0(t4)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi a2, a2, 256
    sll r0, r0, 0
    beq a2, r0, L68
    sll r0, r0, 0

B21:
    sll r0, r0, 0
    lw a2, 6568(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu a2, a2, 1
    sll r0, r0, 0
    sw a2, 6568(t0)
    beq r0, r0, L67
    sll r0, r0, 0

;; the details of the from-spr is unknown, but it seems like setting a1 flag > 0 is used to indicate
;; that we have some pending stuff in spad that we have to copy back.
B22:
L68:
    bgez a1, L93    ;; if we have stuff, go to some later spad dma code
    lw a2, 6408(t0) ;; a2 = instance-work.src-ptr

B23:
    beq r0, r0, L58       ;; nope, we're done, go to loop top
    addiu a1, r0, 10000   ;; but, remember we just did a dma sync for to. So we do have more work to do.
                          ;; ideally we'll find more visible stuff and add to what we have now.
                          ;; but if we don't, we set this flag to >0 to indicate that we have
                          ;; stuff that we still need to process.

;; we reach here once we have visible instances in the scratchpad.
;; but, before we can process them, we have to make sure the output buffer
;; in the scratchpad has enough room.
;; If not, we do a DMA transfer back to RAM (to the dma-buf passed in)
;; this is copying completed VU1 DMA data.
B24:
L69:
    daddiu t4, a3, -106  ;; 106 instances max in out buf, I guess
    lqc2 vf2, 16(a2)     ;; vf2 = bsphere of the first instance (they start prepping for the instance loop here...)
    blez t4, L72         ;; goto L72 if we have enough room in spr
    lbu t4, 6(a2)        ;; t4 = instance.bucket-index (loaded as a u8, maybe only up to 255 buckets/tree?)

;; next three blocks wait for from-spr to finish. Need to do this before
;; starting the next from-spr transfer
B25:
    sll r0, r0, 0
    lw a0, 6416(t0)
    sll r0, r0, 0
    sll r0, r0, 0
B26:
L70:
    lw t3, 0(a0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t3, t3, 256
    sll r0, r0, 0
    beq t3, r0, L71
    sll r0, r0, 0

B27:
    sll r0, r0, 0
    lw t3, 6564(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu t3, t3, 1
    sll r0, r0, 0
    sw t3, 6564(t0)
    beq r0, r0, L70
    sll r0, r0, 0

;; start from-spr and swap output data buffers
B28:
L71:
    sw t1, 128(a0)
    xori t1, t1, 6144 ;; swap buffer
    sw v1, 16(a0)
    sll t3, a3, 4     ;; compute size (16 qw's per instance?)
    addu v1, v1, t3   ;; v1 is the next dma-buf output address (maybe needed for refs in upcoming DMA build)
    or t3, t1, r0
    sw a3, 32(a0)
    addiu a3, r0, 256
    sw a3, 0(a0)      ;; start!
    addiu a3, r0, 0   ;; reset count
   

;; if we reach here, we're finally ready to process the instance.
;; one cool trick they do here is to build 
B29:
L72:
    vcallms 33        ;; see backround-vu0-result.txt. This program does the sphere in view and distance checks.
                      ;; the result is stored in vf04/vf06 and vi02
    lw t5, 6548(t0)   ;; t5 = instance-work.flags (was initialized to 0)
    beq a1, t4, L74   ;; if we're using the same prototype as last time, skip ahead a bit.
    daddiu t6, a1, -10000

B30:
    beq t6, r0, L73
    lw a1, 6404(t0)

B31:                ;; I think this only runs on the very first run.
    sll r0, r0, 0   ;; it copies the last/next/counts of instance-work to the first thing in the proto bucket array
    lq t5, 6336(t0)
    sll r0, r0, 0
    lq t6, 6352(t0)
    sll r0, r0, 0
    lq t7, 6368(t0)
    sll r0, r0, 0
    sq t5, 92(a1)
    sll r0, r0, 0
    sq t6, 60(a1)
    sll r0, r0, 0
    sq t7, 76(a1)
B32:
L73:
    or a1, t4, r0      ;; a1 = current prototype idx (remember it for next time)
    lw t5, 6476(t0)    ;; t5 = prototypes array
    addiu t6, r0, 112  ;; t6 = 112 
    sq r0, 6336(t0)    ;; work.lasts = 0
    multu3 t4, t4, t6  ;; multiply for array access
    sq r0, 6352(t0)    ;; work.nexts = 0
    daddu t4, t5, t4   ;; t4 = ptr to bucket
    sq r0, 6368(t0)    ;; work.counts = 0
    sll r0, r0, 0      ;; nop
    sw t4, 6404(t0)    ;; store bucket in work.bucket-ptr
    sll r0, r0, 0      ;; nop
    lw t5, 4(t4)       ;; t5 = bucket flags
    sll r0, r0, 0      ;; nop
    lqc2 vf15, 44(t4)  ;; vf15 = lengths
    andi t5, t5, 1     ;; t5 = flag & 1
    lqc2 vf14, 28(t4)  ;; vf14 = near/mid/far plane
    vmul.xyz vf15, vf15, vf3 ;; vf15 = lengths * some constants?
    sw t5, 6548(t0)    ;; store flags in instance-work.flags

;; from here on, it looks like we jump to L92 if we reject the instance
;; NOTE: starting here is the matrix stuff.
;; we'll need to understand this to "de-instance" the non-wind instances
;; and to implement wind in C++
B33:
L74:
    bne t5, r0, L92  ;; check flags & 1. This flag is only set from the debug menu (see dm-enable-instance-func)
                     ;; and it's just used to disable a specific prototype for debugging.
    ld t5, 56(a2)    ;; loading the origin matrix (4x 16-bit integers/row) (this the last row)

B34:
    sll r0, r0, 0
    ld t4, 32(a2)      ;; t4 = row 0
    pextlh t5, t5, r0  ;; unpack row 3 to u32's (effectively shifts left 16)
    ld t6, 40(a2)      ;; t6 = row 1
    psraw t7, t5, 10   ;; t7 = shift row 3 right by 10 (two shifts equivalent to shift left by 6 and sign extend)
    ld t5, 48(a2)      ;; t5 = row 2
    pextlh t8, t4, r0  ;; t8 = row 0 to u32's
    lhu t4, 8(a2)      ;; t4 = instance.color-indices (I think an offset in the tree's palette, different from TIE)
    psraw t8, t8, 16   ;; t8 = shift row 0 right by 16 (two shifts equivalent to just sign extending)
    lq t9, 64(a2)      ;; t9 = instance.flat-normal
    pextlh t6, t6, r0  ;; t6 = row 1 unpacked
    qmtc2.ni vf13, t7  ;; vf13 = row 3
    psraw t6, t6, 16   ;; t6 = row 1 shifted
    qmtc2.ni vf18, t9  ;; vf18 = instance.flat-normal
    pextlh t5, t5, r0  ;; t5 = row 2 unpacked
    qmtc2.ni vf10, t8  ;; vf10 = row 0
    psraw t5, t5, 16   ;; t5 = row 2 shifted
    qmtc2.ni vf11, t6  ;; vf11 = row 1
    daddu t4, t4, t0   ;; t4 = color data - 304
    qmtc2.ni vf12, t5  ;; vf12  = row 2
    sll r0, r0, 0
    cfc2.i t5, vi1         ;; t5 = vis result.
    vitof0.xyzw vf13, vf13 ;; vf13 = row 3, as floats
    lw t6, 304(t4)     ;; t6 = rgba for this instance (8888 format)
    bne t5, r0, L92    ;; possibly reject this instance.
    lq t4, 6080(t0)    ;; t4 = color constants (some hacky int to float stuff here)

B35:
    pextlb t5, r0, t6        ;; t5 = unpacked rgba to u16's
    lqc2 vf4, 6096(t0)       ;; vf4 = hmge-d
    pextlh t5, r0, t5        ;; t5 = unpacked rgba to u32's
    lqc2 vf25, 6176(t0)      ;; vf25 = min-dist (interesting...)
    vsub.xyzw vf9, vf6, vf14 ;; vf6 is the "dist" of the draw node?
    sll r0, r0, 0
    psllw t6, t5, 8          ;; t6 = multiply colors by 256
    mfc1 r0, f31
    paddw t4, t6, t4         ;; t4 = colors + color constants
    mfc1 r0, f31
    vmula.xyzw acc, vf1, vf3 ;; 
    sll r0, r0, 0
    vmsub.xyzw vf9, vf9, vf15
    sq t5, 6160(t0)          ;; stash bb color
    vadd.xyz vf13, vf13, vf2 ;; same bsphere origin trick as tie
    sq t4, 6144(t0)          ;; store floating point color
    vsubw.xyzw vf8, vf6, vf2 ;; distance compensate for bsphere radius
    sll r0, r0, 0
    vitof12.xyzw vf10, vf10  ;; row 0 as floats
    sll r0, r0, 0
    vmini.xyzw vf9, vf9, vf3    ;; dist crap
    lw t4, 6404(t0)             ;; t4 = bucket-ptr
    vadd.xyz vf18, vf18, vf13   ;; flat-normal + real-origin
    sll r0, r0, 0
    vmulax.xyzw acc, vf28, vf13 ;; 
    lw t4, 24(t4)               ;; geom3
    vmadday.xyzw acc, vf29, vf13
    sll r0, r0, 0
    vmaxx.xyzw vf9, vf9, vf0
    sll r0, r0, 0
    vmaddaz.xyzw acc, vf30, vf13
    sll r0, r0, 0
    vmaddw.xyzw vf5, vf31, vf0 ;; vf.w is inverse distance from camera, I think
    sll r0, r0, 0
    vitof12.xyzw vf11, vf11    ;; vf11 = row 1 floats
    sll r0, r0, 0
    vftoi0.xyzw vf19, vf9      ;; distance stuff
    sll r0, r0, 0
    vmini.xyzw vf25, vf8, vf25 ;; apply min dist
    sll r0, r0, 0
    vsubz.xyzw vf4, vf8, vf4   ;; apply hmge
    addiu t5, r0, 128          ;; ?? t5 = 128
    vitof12.xyzw vf12, vf12    ;; vf12 = row 2 float
    addiu t6, r0, 255          ;; ?? t6 = 255
    vmulw.y vf9, vf9, vf15     ;; multiply by lengths
    sll r0, r0, 0
    sll r0, r0, 0
    qmfc2.i t7, vf19           ;; integer dist compare
    vdiv Q, vf3.w, vf5.w       ;; compute Q here, I guess
    sll r0, r0, 0
    and t6, t7, t6
    sll r0, r0, 0
    dsubu t7, t5, t6
    sw t6, 6156(t0)           ;; adjusted color for fade out.
    beq t5, t6, L80           ;; branch if don't try billboard, I think?
    sqc2 vf25, 6176(t0)

B36:
    beq t4, r0, L75           ;; don't do billboard if we don't have it
    sw t7, 6172(t0)

B37:
;;;;;;;;;;;;;;;
;; BILLBOARD
;;;;;;;;;;;;;;;
    vmulax.xyzw acc, vf28, vf18
    lq t4, 5104(t0)
    vmadday.xyzw acc, vf29, vf18
    lq t5, 5120(t0)
    vmaddaz.xyzw acc, vf30, vf18
    lw t6, 6348(t0)
    vmaddw.xyzw vf18, vf31, vf0
    lw t7, 6364(t0)
    sll t8, a3, 4
    lqc2 vf8, 6112(t0)
    addu t8, t8, v1
    lqc2 vf7, 64(a2)
    vmulaq.xyz acc, vf5, Q
    lq a2, 6160(t0)
    vmulaw.w acc, vf5, vf0
    movz t6, t8, t6
    vmadd.xyzw vf5, vf1, vf8
    lhu t9, 6374(t0)
    vmulq.w vf19, vf7, Q
    sll r0, r0, 0
    daddiu t9, t9, 1
    lqc2 vf6, 5136(t0)
    vmulq.xyzw vf26, vf1, Q
    sw t6, 6348(t0)
    vmulq.xyzw vf27, vf1, Q
    sw t8, 6364(t0)
    vnop
    sll r0, r0, 0
    vmaxz.w vf5, vf5, vf6
    sh t9, 6374(t0)
    vdiv Q, vf3.w, vf18.w
    sll r0, r0, 0
    vmulax.xyzw acc, vf20, vf10
    sq t4, 0(t3)
    vaddx.x vf26, vf0, vf0
    sq t5, 16(t3)
    vminiw.w vf5, vf5, vf6
    sq a2, 48(t3)
    vmadday.xyzw acc, vf21, vf10
    sq a2, 96(t3)
    vmaddz.xyzw vf10, vf22, vf10
    sq a2, 144(t3)
    vmulaw.w acc, vf18, vf0
    sq a2, 192(t3)
    vmulaq.xyz acc, vf18, Q
    sw t7, 4(t3)
    vmadd.xyzw vf18, vf1, vf8
    sll r0, r0, 0
    vmulq.w vf8, vf7, Q
    sll r0, r0, 0
    vmulq.xyzw vf24, vf1, Q
    sll r0, r0, 0
    vmulq.xyzw vf25, vf1, Q
    sll r0, r0, 0
    vmaxz.w vf18, vf18, vf6
    sll r0, r0, 0
    vadd.xy vf24, vf0, vf0
    sll r0, r0, 0
    vaddy.y vf25, vf0, vf0
    sll r0, r0, 0
    vmulax.xyzw acc, vf20, vf11
    sll r0, r0, 0
    vminiw.w vf18, vf18, vf6
    sll r0, r0, 0
    vmadday.xyzw acc, vf21, vf11
    sll r0, r0, 0
    vmaddz.xyzw vf11, vf22, vf11
    sll r0, r0, 0
    vmulax.xyzw acc, vf20, vf12
    sll r0, r0, 0
    vsub.xyzw vf16, vf18, vf5
    sll r0, r0, 0
    vmadday.xyzw acc, vf21, vf12
    sll r0, r0, 0
    vmaddz.xyzw vf12, vf22, vf12
    sll r0, r0, 0
    vmulax.xyzw acc, vf20, vf13
    sll r0, r0, 0
    vaddy.y vf16, vf16, vf16
    sll r0, r0, 0
    vmadday.xyzw acc, vf21, vf13
    sll r0, r0, 0
    vmaddaz.xyzw acc, vf22, vf13
    sll r0, r0, 0
    vmaddw.xyzw vf13, vf23, vf0
    sll r0, r0, 0
    vmul.xy vf17, vf16, vf16
    sll r0, r0, 0
    sll r0, r0, 0
    sqc2 vf24, 32(t3)
    sll r0, r0, 0
    sqc2 vf25, 80(t3)
    sll r0, r0, 0
    sqc2 vf26, 128(t3)
    vaddy.x vf17, vf17, vf17
    sll r0, r0, 0
    sll r0, r0, 0
    sqc2 vf27, 176(t3)
    vmulw.xyzw vf2, vf18, vf0
    sll r0, r0, 0
    vmulw.xyzw vf4, vf18, vf0
    sll r0, r0, 0
    vrsqrt Q, vf0.w, vf17.x
    sll r0, r0, 0
    sll r0, r0, 0
    vwaitq
    vmulq.xy vf17, vf16, Q
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    vsuby.x vf16, vf0, vf17
    sll r0, r0, 0
    vaddx.y vf16, vf0, vf17
    sll r0, r0, 0
    sll r0, r0, 0
    sqc2 vf10, 240(t3)
    sll r0, r0, 0
    sqc2 vf11, 256(t3)
    vmulw.xy vf8, vf16, vf8
    sll r0, r0, 0
    vmulw.xy vf19, vf16, vf19
    sll r0, r0, 0
    sll r0, r0, 0
    lq a2, 6144(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    vmul.xy vf8, vf8, vf6
    sll r0, r0, 0
    vmul.xy vf19, vf19, vf6
    sll r0, r0, 0
    vmulw.xyzw vf6, vf5, vf0
    sll r0, r0, 0
    vmulw.xyzw vf7, vf5, vf0
    sq a2, 304(t3)
    vadd.xy vf2, vf18, vf8
    sll r0, r0, 0
    vsub.xy vf4, vf18, vf8
    sll r0, r0, 0
    vadd.xy vf6, vf5, vf19
    sll r0, r0, 0
    vsub.xy vf7, vf5, vf19
    sll r0, r0, 0
    vftoi4.xyzw vf2, vf2
    sll r0, r0, 0
    vftoi4.xyzw vf4, vf4
    daddiu t3, t3, 224
    vftoi4.xyzw vf6, vf6
    daddiu a3, a3, 14
    vftoi4.xyzw vf7, vf7
    lw a2, 6156(t0)
    sll r0, r0, 0
    sqc2 vf2, -160(t3)
    sll r0, r0, 0
    sqc2 vf4, -112(t3)
    sll r0, r0, 0
    sqc2 vf6, -64(t3)
    beq a2, r0, L92
    sqc2 vf7, -16(t3)

B38:
    beq r0, r0, L76
    sll r0, r0, 0

B39:
L75:
    beq t6, r0, L92
    vmulax.xyzw acc, vf20, vf10

B40:
    vmadday.xyzw acc, vf21, vf10
    lq a2, 6144(t0)
    vmaddz.xyzw vf10, vf22, vf10
    sll r0, r0, 0
    vmulax.xyzw acc, vf20, vf11
    sll r0, r0, 0
    vmadday.xyzw acc, vf21, vf11
    sll r0, r0, 0
    vmaddz.xyzw vf11, vf22, vf11
    sll r0, r0, 0
    vmulax.xyzw acc, vf20, vf12
    sll r0, r0, 0
    vmadday.xyzw acc, vf21, vf12
    sll r0, r0, 0
    vmaddz.xyzw vf12, vf22, vf12
    sll r0, r0, 0
    vmulax.xyzw acc, vf20, vf13
    sll r0, r0, 0
    vmadday.xyzw acc, vf21, vf13
    sll r0, r0, 0
    vmaddaz.xyzw acc, vf22, vf13
    sll r0, r0, 0
    vmaddw.xyzw vf13, vf23, vf0
    sq a2, 80(t3)
    sll r0, r0, 0
    sqc2 vf10, 16(t3)
    sll r0, r0, 0
    sqc2 vf11, 32(t3)
B41:
L76:
    sll a2, a3, 4
    lhu t4, 6380(t0)
    addu t5, a2, v1
    lhu t7, 6372(t0)
    sll t6, t4, 4
    lw a2, 6360(t0)
    daddu t8, t6, t0
    lw t6, 6344(t0)
    daddiu t7, t7, 1
    lq t8, 4400(t8)
    daddiu a3, a3, 6
    sh t7, 6372(t0)
    daddiu t7, t4, 1
    sq t8, 0(t3)
    daddiu t8, t7, -20
    sqc2 vf12, 48(t3)
    movz t7, r0, t8
    sqc2 vf13, 64(t3)
    daddiu t8, t4, -10
    sh t7, 6380(t0)
    daddiu t3, t3, 96
    sw a2, -92(t3)
    beq t4, r0, L77
    sw t5, 6360(t0)

B42:
    bne t8, r0, L78
    sll r0, r0, 0

B43:
L77:
    sll r0, r0, 0
    lq t4, 5040(t0)
    sll r0, r0, 0
    lq t7, 5056(t0)
    sll r0, r0, 0
    sw t5, 6344(t0)
    sll r0, r0, 0
    movz t4, t7, t6
    daddiu a3, a3, 1
    sq t4, 0(t3)
    sll r0, r0, 0
    sw a2, 4(t3)
    beq r0, r0, L92
    daddiu t3, t3, 16

B44:
L78:
    daddiu t5, t4, -9
    sll r0, r0, 0
    beq t5, r0, L79
    daddiu t4, t4, -19

B45:
    bne t4, r0, L92
    sll r0, r0, 0

B46:
L79:
    sll r0, r0, 0
    sll t4, t7, 4
    sll r0, r0, 0
    daddu t4, t4, t0
    daddiu a3, a3, 1
    lq t4, 4720(t4)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sq t4, 0(t3)
    sll r0, r0, 0
    sw a2, 4(t3)
    beq r0, r0, L92
    daddiu t3, t3, 16


;; I think the end of billboard.
B47:
L80:
    sll r0, r0, 0
    lw t4, 1324(t2) ;; t4 = wind time (from global wind work)
    sll r0, r0, 0
    lhu t5, 62(a2)  ;; t5 = wind-index of the instance
    sll r0, r0, 0
    lw a2, 6384(t0)     ;; a2 = wind-vectors
    dsll t6, t5, 4      ;; t6 = t5 * 16
    lqc2 vf19, 6048(t0) ;; vf19 = wind-const
    daddu a2, a2, t6    ;; a2 = wind-vector + (wind-index * 16)
    daddu t4, t5, t4    ;; t4 = wind-time + wind-index
    andi t5, t4, 63     ;; t5 = (wind-time + wind-index) & 63
    ld t4, 8(a2)        ;; t4 = winds
    sll t6, t5, 4       ;; t6 = ((wind-time + wind-index) & 63) * 16
    ld t5, 0(a2)        ;; t5 = winds
    addu t7, t6, t2
    qmfc2.i t6, vf4
    pextlw t4, r0, t4
    lqc2 vf16, 12(t7)
    pextlw t5, r0, t5
    qmtc2.i vf18, t4
    sll r0, r0, 0
    qmtc2.i vf17, t5
    vmula.xyzw acc, vf16, vf1
    sll r0, r0, 0
    vmsubax.xyzw acc, vf18, vf19
    sll r0, r0, 0
    vmsuby.xyzw vf16, vf17, vf19
    sll r0, r0, 0
    pcgtw t5, r0, t6
    mfc1 r0, f31
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    lqc2 vf24, 6208(t0)
    vmulaz.xyzw acc, vf16, vf19
    sll r0, r0, 0
    vmadd.xyzw vf18, vf1, vf18
    sll r0, r0, 0
    sll r0, r0, 0
    lqc2 vf25, 6224(t0)
    sll r0, r0, 0
    lqc2 vf26, 6240(t0)
    sll r0, r0, 0
    lqc2 vf27, 6256(t0)
    vmulaz.xyzw acc, vf18, vf19
    sll r0, r0, 0
    vmadd.xyzw vf17, vf17, vf1
    sll r0, r0, 0
    vmulax.xyzw acc, vf24, vf2
    sll r0, r0, 0
    vmadday.xyzw acc, vf25, vf2
    sll r0, r0, 0
    vmaddaz.xyzw acc, vf26, vf2
    sll r0, r0, 0
    vminiw.xyzw vf17, vf17, vf0
    sll r0, r0, 0
    vmsubaw.xyzw acc, vf27, vf0
    sll r0, r0, 0
    vmsubw.xyzw vf24, vf1, vf2
    sll r0, r0, 0
    sll r0, r0, 0
    qmfc2.i t4, vf18
    vmaxw.xyzw vf27, vf17, vf19
    sll r0, r0, 0
    ppacw t4, r0, t4
    mfc1 r0, f31
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    qmfc2.i t6, vf24
    vmuly.xyzw vf27, vf27, vf9
    sll r0, r0, 0
    pcgtw t6, r0, t6
    mfc1 r0, f31
    ppach t6, r0, t6
    mfc1 r0, f31
    vmulax.yw acc, vf0, vf0
    sll r0, r0, 0
    vmulay.xz acc, vf27, vf10
    sll r0, r0, 0
    vmadd.xyzw vf10, vf1, vf10
    sll r0, r0, 0
    or t5, t6, t5
    qmfc2.i t6, vf27
    vmulax.yw acc, vf0, vf0
    lw t7, 6552(t0)
    vmulay.xz acc, vf27, vf11
    sll r0, r0, 0
    vmadd.xyzw vf11, vf1, vf11
    sll r0, r0, 0
    bne t7, s7, L81
    ppacw t6, r0, t6

B48:
    vmulax.yw acc, vf0, vf0
    sd t4, 8(a2)
    vmulay.xz acc, vf27, vf12
    sd t6, 0(a2)
    bne t5, r0, L86
    vmadd.xyzw vf12, vf1, vf12

B49:
    beq r0, r0, L82
    sll r0, r0, 0

B50:
L81:
    vmulax.yw acc, vf0, vf0
    sll r0, r0, 0
    vmulay.xz acc, vf27, vf12
    sll r0, r0, 0
    bne t5, r0, L86
    vmadd.xyzw vf12, vf1, vf12

B51:
L82:
    vmulax.xyzw acc, vf20, vf10
    lq a2, 6144(t0)
    vmadday.xyzw acc, vf21, vf10
    sll r0, r0, 0
    vmaddz.xyzw vf10, vf22, vf10
    sll r0, r0, 0
    vmulax.xyzw acc, vf20, vf11
    sll r0, r0, 0
    vmadday.xyzw acc, vf21, vf11
    sll r0, r0, 0
    vmaddz.xyzw vf11, vf22, vf11
    sll r0, r0, 0
    vmulax.xyzw acc, vf20, vf12
    sll r0, r0, 0
    vmadday.xyzw acc, vf21, vf12
    sll r0, r0, 0
    vmaddz.xyzw vf12, vf22, vf12
    sll r0, r0, 0
    vmulax.xyzw acc, vf20, vf13
    sll r0, r0, 0
    vmadday.xyzw acc, vf21, vf13
    sll r0, r0, 0
    vmaddaz.xyzw acc, vf22, vf13
    sll r0, r0, 0
    vmaddw.xyzw vf13, vf23, vf0
    sq a2, 80(t3)
    sll r0, r0, 0
    sqc2 vf10, 16(t3)
    sll r0, r0, 0
    sqc2 vf11, 32(t3)
    sll a2, a3, 4
    lhu t4, 6378(t0)
    addu t5, a2, v1
    lhu t7, 6370(t0)
    sll t6, t4, 4
    lw a2, 6356(t0)
    daddu t8, t6, t0
    lw t6, 6340(t0)
    daddiu t7, t7, 1
    lq t8, 4400(t8)
    daddiu a3, a3, 6
    sh t7, 6370(t0)
    daddiu t7, t4, 1
    sq t8, 0(t3)
    daddiu t8, t7, -20
    sqc2 vf12, 48(t3)
    movz t7, r0, t8
    sqc2 vf13, 64(t3)
    daddiu t8, t4, -10
    sh t7, 6378(t0)
    daddiu t3, t3, 96
    sw a2, -92(t3)
    beq t4, r0, L83
    sw t5, 6356(t0)

B52:
    bne t8, r0, L84
    sll r0, r0, 0

B53:
L83:
    sll r0, r0, 0
    lq t4, 5040(t0)
    sll r0, r0, 0
    lq t7, 5056(t0)
    sll r0, r0, 0
    sw t5, 6340(t0)
    sll r0, r0, 0
    movz t4, t7, t6
    daddiu a3, a3, 1
    sq t4, 0(t3)
    sll r0, r0, 0
    sw a2, 4(t3)
    beq r0, r0, L92
    daddiu t3, t3, 16

B54:
L84:
    daddiu t5, t4, -9
    sll r0, r0, 0
    beq t5, r0, L85
    daddiu t4, t4, -19

B55:
    bne t4, r0, L92
    sll r0, r0, 0

B56:
L85:
    sll r0, r0, 0
    sll t4, t7, 4
    sll r0, r0, 0
    daddu t4, t4, t0
    daddiu a3, a3, 1
    lq t4, 4720(t4)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sq t4, 0(t3)
    sll r0, r0, 0
    sw a2, 4(t3)
    beq r0, r0, L92
    daddiu t3, t3, 16

B57:
L86:
    vmulax.xyzw acc, vf28, vf10
    lqc2 vf24, 6160(t0)
    vmadday.xyzw acc, vf29, vf10
    sll r0, r0, 0
    vmaddz.xyzw vf10, vf30, vf10
    sll r0, r0, 0
    vmulax.xyzw acc, vf28, vf11
    sll r0, r0, 0
    vmadday.xyzw acc, vf29, vf11
    lhu t4, 6536(t0)
    vmaddz.xyzw vf11, vf30, vf11
    lw a2, 6404(t0)
    vmulax.xyzw acc, vf28, vf12
    daddiu t8, t4, 1
    vmadday.xyzw acc, vf29, vf12
    sh t8, 6536(t0)
    vmaddz.xyzw vf12, vf30, vf12
    lw t4, 12(a2) ;; load the generic geometry?
    vmulax.xyzw acc, vf28, vf13
    lw t5, 6532(t0)
    vmadday.xyzw acc, vf29, vf13
    lh t6, 2(t4)                    ;; generic frag count.
    vmaddaz.xyzw acc, vf30, vf13
    lw a2, 6528(t0)
    vmaddw.xyzw vf13, vf31, vf0
    lw t7, 6516(t0)
    vitof0.xyz vf24, vf24
    sh t8, 6368(t0)
B58:                           ;; generic loop
L87:
    daddiu t8, a3, -115
    sll r0, r0, 0
    blez t8, L90
    lw t8, 28(t4)             ;; load the frag

B59:                          ;; dma
L88:
    lw t3, 0(a0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t3, t3, 256
    sll r0, r0, 0
    beq t3, r0, L89
    sll r0, r0, 0

B60:
    sll r0, r0, 0
    lw t3, 6564(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu t3, t3, 1
    sll r0, r0, 0
    sw t3, 6564(t0)
    beq r0, r0, L88
    sll r0, r0, 0

B61:
L89:
    sw t1, 128(a0)
    xori t1, t1, 6144
    sw v1, 16(a0)
    sll t3, a3, 4
    addu v1, v1, t3
    or t3, t1, r0
    sw a3, 32(a0)
    addiu a3, r0, 256
    sw a3, 0(a0)
    addiu a3, r0, 0
B62:
L90:
    daddu t9, t7, t0
    addiu t7, t7, -144
    daddiu t4, t4, 4
    daddiu t9, t9, 5152
    bgez t7, L91
    lq ra, 0(t9)

B63:
    sll r0, r0, 0
    addiu t7, r0, 720
B64:
L91:
    sll r0, r0, 0
    sw t5, 84(t9)
    sll t5, a3, 4
    sq ra, 0(t3)
    addu t5, t5, v1
    sqc2 vf10, 16(t3)
    movz a2, t5, a2
    sqc2 vf11, 32(t3)
    daddiu a3, a3, 12
    sqc2 vf12, 48(t3)
    sll r0, r0, 0
    lw ra, 4(t8)            ;; ra = vtx-cnt
    sll r0, r0, 0
    sqc2 vf13, 64(t3)
    sll r0, r0, 0
    sqc2 vf24, 80(t3)
    sll r0, r0, 0
    sw ra, 96(t3)
    sll r0, r0, 0
    lw ra, 12(t8)           ;; ra = cnt
    sll r0, r0, 0
    lbu gp, 8(t8)           ;; gp = cnt-qwc
    sll r0, r0, 0
    sw ra, 20(t9)
    sll r0, r0, 0
    sb gp, 16(t9)
    sll r0, r0, 0
    sb gp, 30(t9)
    sll r0, r0, 0
    lw ra, 24(t8)          ;; ra = stq
    sll r0, r0, 0
    lbu gp, 11(t8)         ;; gp = stq-qwc
    sll r0, r0, 0
    sw ra, 36(t9)
    sll r0, r0, 0
    sb gp, 32(t9)
    sll r0, r0, 0
    lw ra, 20(t8)          ;; ra = col
    sll r0, r0, 0
    lbu gp, 10(t8)         ;; gp = col-qwc
    sll r0, r0, 0
    sw ra, 52(t9)
    sll r0, r0, 0
    sb gp, 48(t9)
    sll r0, r0, 0
    lw ra, 16(t8)          ;; ra = vtx
    sll r0, r0, 0
    lbu gp, 9(t8)          ;; gp = vtx-qwc
    sll r0, r0, 0
    sw ra, 68(t9)
    sll r0, r0, 0
    sb gp, 64(t9)
    sll r0, r0, 0
    lw t8, 4(t8)
    sll r0, r0, 0
    lq ra, 16(t9)
    sll r0, r0, 0
    sb t8, 46(t9)
    sll r0, r0, 0
    sb t8, 62(t9)
    sll r0, r0, 0
    sb t8, 78(t9)
    sll r0, r0, 0
    sq ra, 112(t3)
    sll r0, r0, 0
    lq t8, 32(t9)
    sll r0, r0, 0
    lq ra, 48(t9)
    sll r0, r0, 0
    sq t8, 128(t3)
    sll r0, r0, 0
    sq ra, 144(t3)
    sll r0, r0, 0
    lq t8, 64(t9)
    sll r0, r0, 0
    lq t9, 80(t9)
    sll r0, r0, 0
    sq t8, 160(t3)
    daddiu t3, t3, 192
    sq t9, -16(t3)
    daddiu t6, t6, -1
    sll r0, r0, 0
    bgtz t6, L87
    sll r0, r0, 0

B65:
    sll r0, r0, 0
    sw t7, 6516(t0)
    lui t4, 4096
    sw t5, 6532(t0)
    ori t4, t4, 54272
    sw a2, 6528(t0)
    sll r0, r0, 0
    sll r0, r0, 0
B66:
L92:
    vcallms 25
    lw a2, 6408(t0)
    sll r0, r0, 0
    lw t4, 6420(t0)
    daddiu a2, a2, 80
    sll r0, r0, 0
    daddiu t4, t4, -1
    sw a2, 6408(t0)
    bgtz t4, L69
    sw t4, 6420(t0)

B67:
L93:
    sll r0, r0, 0
    lw t4, 8(a2)
    daddiu a2, a2, 16
    lw t5, 6540(t0)
    sll r0, r0, 0
    sw a2, 6408(t0)
    bne t4, r0, L69
    sw t4, 6420(t0)

B68:
    bne t5, r0, L58
    sll r0, r0, 0

B69:
    sll r0, r0, 0
    lw a1, 6404(t0)
    sll r0, r0, 0
    lq a2, 6336(t0)
    sll r0, r0, 0
    lq t2, 6352(t0)
    sll r0, r0, 0
    lq t3, 6368(t0)
    sll r0, r0, 0
    sq a2, 92(a1)
    sll r0, r0, 0
    sq t2, 60(a1)
    sll r0, r0, 0
    sq t3, 76(a1)
    beq a3, r0, L96
    sll r0, r0, 0

B70:
    sll r0, r0, 0
    lw a0, 6416(t0)
    sll r0, r0, 0
    sll r0, r0, 0
B71:
L94:
    lw a1, 0(a0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi a1, a1, 256
    sll r0, r0, 0
    beq a1, r0, L95
    sll r0, r0, 0

B72:
    sll r0, r0, 0
    lw a1, 6564(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu a1, a1, 1
    sll r0, r0, 0
    sw a1, 6564(t0)
    beq r0, r0, L94
    sll r0, r0, 0

B73:
L95:
    sw v1, 16(a0)
    sll a1, a3, 4
    sw t1, 128(a0)
    xori a2, t1, 6144
    addu v1, v1, a1
    or a1, a2, r0
    sw a3, 32(a0)
    addiu a1, r0, 256
    sw a1, 0(a0)
    addiu a1, r0, 0
B74:
L96:
    lw a1, 0(a0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi a1, a1, 256
    sll r0, r0, 0
    beq a1, r0, L97
    sll r0, r0, 0

B75:
    sll r0, r0, 0
    lw a1, 6564(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu a1, a1, 1
    sll r0, r0, 0
    sw a1, 6564(t0)
    beq r0, r0, L96
    sll r0, r0, 0

B76:
L97:
    lw a0, 6524(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    sw v1, 4(a0)
    sll r0, r0, 0
B77:
L98:
    or v0, r0, r0
    ld ra, 0(sp)
    lq gp, 16(sp)
    jr ra
    daddiu sp, sp, 32

    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
```
