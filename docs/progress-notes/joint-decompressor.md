This is an explanation of reverse engineering and porting the Jak joint decompressor from PS2 assembly to higher-level OpenGOAL code. It's the same from all three games. I made this document as an example of how to approach this type of reverse engineering.

# What is `joint.gc`?

The "joint" system is used to play back animations.

At a very high level, the process to animate a character:
- the gameplay code uses the `ja` macros to set up joint animations.
- the process-drawable system updates the animation metadata in joint-control. This is responsible for
  producing an array of joint-control-channel. The channels are arranged into a blend tree, and each
  channel has a frame number (possibly in between frames) and interpolation weight for the blend tree.
- This file, joint.gc looks at those animations, frame numbers, blend weights, and tree structure, then
 produces the relative transform between bones, called "joint transforms"
- The process-drawable system uses the joint transforms to compute bone transforms.
- The gameplay code and collision code use the bone transforms to determine world-space positions/rotations.
- The "bones.gc" system builds rendering matrices for the foreground renderer
- The merc, mercneric, and shadow renderers consume these matrices for their skinning calculations.

The focus of this documentation is the function that computes joint transforms.

There are three:

1. `create-interpolated-joint-animation-frame`, which is the most standard one. It takes a `joint-control`, a number of joints (since low lod versions might not need them all), and produces a `joint-anim-frame` - a list of joint transforms.
2. `create-interpolated2-joint-animation-frame`, which is the same, but uses the "interp2" system described later.
3. `cspace<-matrix-no-push-joint!` which computes a single joint transform. The joint must be one of the first two, and it uses a special `no-push` mode. More on this later.

Reading the top of `joint.gc` has a lot more details, if you are curious about the details of the blending, etc. That was written before touching the actually decompressor assembly. Figuring all this out first can be really helpful.

# Background information

The end result of this routine is to compute a `joint-anim-frame`:
```lisp
(deftype joint-anim-frame (structure)
  ((matrices  matrix      2 :inline)
   (data      transformq  :inline :dynamic)
   )
  (:methods
    (new (symbol type int) _type_)
    )
  )
```
which is all the joint transforms for a interpolated frame. Note that there is interpolation in both time (in between frames) and between different animations (blending).

This is the code that is used to generate a `joint-anim-frame`:
```lisp
(defun create-interpolated-joint-animation-frame ((dst joint-anim-frame) (num-joints int) (jc joint-control))
  (flatten-joint-control-to-spr jc)
  (make-joint-jump-tables)
  (calc-animation-from-spr dst num-joints)
  0
  )
```

# Flattening the Joint Control

The `flatten-joint-control-to-spr` function is written in plain GOAL, so I've decompiled/annotated it here.
The first part of the function is iterating through `joint-control-channel`s, and computing blend weights from a blend tree. This computation acts like a stack machine. There is a `push` operation to push a new stack frame, with all channels zero except for the specified animation. There is a `stack` operation which takes the two most top things on the stack, then adds them together with the given weights (effectively `pop`ing back 1 stack frame in the end). Finally, there is a `blend` operation, which modifies the top of the animation stack to be a blend of multiple animations.  I imagine this can stay almost the same in the new decompressor, other than scratchpad use.

The second part builds a list of "uploads". This is a list of data that should be uploaded to the scratchpad. Each animation has some "fixed" data, which is a chunk of common data needed to produce any frame.

There is also "frame" data, one for each frame in the animation. If we need to interpolate between two frames, we upload 2 frames worth of data. Otherwise, if we want an exact frame, we just upload the one.

```lisp

(defun flatten-joint-control-to-spr ((jc joint-control))
  "Walk the blend tree and compute interpolation weights, prepare animation upload info."
  (rlet ((vf1 :class vf)
         (vf10 :class vf)
         (vf11 :class vf)
         (vf12 :class vf)
         (vf13 :class vf)
         (vf14 :class vf)
         (vf2 :class vf)
         (vf3 :class vf)
         (vf4 :class vf)
         (vf5 :class vf)
         (vf6 :class vf)
         (vf7 :class vf)
         (vf8 :class vf)
         (vf9 :class vf)
         )
    ;; We assume a maximum of 4 * 6 = 24 channels.
    (let ((chan-count (+ (-> jc active-channels) (-> jc float-channels))))
      (let ((one 1.0)             ;; constant
            (chan-float-offset 0) ;; which channel's weight to adjust.
            (chan-vector-ptr (the-as (inline-array vector) #x70000960)) ;; stack pointer
            (interp2-selected-idx (-> jc active-frame-interp)) ;; interp2 0 or 1 selector.
            )
        ;; loop over channels
        (dotimes (chan-idx (the-as int chan-count))
          (let ((chan (-> jc channel chan-idx)))
            (case (-> chan command)
              (((joint-control-command push))
               ;; push a new stack frame with this anim set to 1.
               (let ((flt1 (the-as (pointer float) (+ (the-as int chan-vector-ptr) chan-float-offset))))
                 ;; initialize all channels in this frame to 0
                 (set! (-> chan-vector-ptr 0 quad) (the-as uint128 0))
                 (set! (-> chan-vector-ptr 1 quad) (the-as uint128 0))
                 (set! (-> chan-vector-ptr 2 quad) (the-as uint128 0))
                 (set! (-> chan-vector-ptr 3 quad) (the-as uint128 0))
                 (set! (-> chan-vector-ptr 4 quad) (the-as uint128 0))
                 (set! (-> chan-vector-ptr 5 quad) (the-as uint128 0))
                 ;; then, set the weight of this animation to 1.
                 (set! (-> flt1 0) one)
                 )
               ;; advance stack pointer.
               (set! chan-vector-ptr (the-as (inline-array vector) (-> chan-vector-ptr 6)))
               )
              (((joint-control-command blend) (joint-control-command push1) (joint-control-command float))
               ;; determine the blend factor for this animation.
               (let ((interp2-selected-weight1 (-> chan frame-interp interp2-selected-idx)))
                 ;; one - blend_factor gives us the blend weight for the previous thing in the frame
                 (let ((a3-5 (- one interp2-selected-weight1)))
                   (.mov vf1 a3-5)
                   )
                 ;; modify the previous thing in the stack to reduce weight:
                 (let ((prev-chan-ptr (the-as (inline-array vector) (-> chan-vector-ptr -6))))
                   (.lvf vf2 (&-> prev-chan-ptr 0 quad))
                   (let ((a3-6 (&+ (the-as pointer prev-chan-ptr) chan-float-offset)))
                     (.lvf vf3 (&-> prev-chan-ptr 1 quad))
                     (.lvf vf4 (&-> prev-chan-ptr 2 quad))
                     (.lvf vf5 (&-> prev-chan-ptr 3 quad))
                     (.lvf vf6 (&-> prev-chan-ptr 4 quad))
                     (.lvf vf7 (&-> prev-chan-ptr 5 quad))
                     (.mul.x.vf vf2 vf2 vf1) ;; multiply all weights by (1 - push_blend)
                     (.mul.x.vf vf3 vf3 vf1)
                     (.mul.x.vf vf4 vf4 vf1)
                     (.mul.x.vf vf5 vf5 vf1)
                     (.mul.x.vf vf6 vf6 vf1)
                     (.mul.x.vf vf7 vf7 vf1)
                     (.svf (&-> prev-chan-ptr 0 quad) vf2)
                     (.svf (&-> prev-chan-ptr 1 quad) vf3)
                     (.svf (&-> prev-chan-ptr 2 quad) vf4)
                     (.svf (&-> prev-chan-ptr 3 quad) vf5)
                     (.svf (&-> prev-chan-ptr 4 quad) vf6)
                     (.svf (&-> prev-chan-ptr 5 quad) vf7)
                     ;; but, modify our channel to add in the push_blend
                     (+! (-> (the-as (pointer float) a3-6) 0) interp2-selected-weight1)
                     )
                   (set! chan-vector-ptr (the-as (inline-array vector) (-> prev-chan-ptr 6)))
                   )
                 )
               )
              (((joint-control-command stack))
               ;; add together the last two stack frames, using the given weight.
               (let* ((interp2-selected-weight2 (-> chan frame-interp interp2-selected-idx))
                      (one-minus-interp2 (- one interp2-selected-weight2))
                      ;; back up 2 stack frames, to add them.
                      (chans-to-stack (the-as (inline-array vector) (-> chan-vector-ptr -12)))
                      )
                 (let ((a3-8 interp2-selected-weight2))
                   (.mov vf1 a3-8)
                   )
                 (let ((a3-9 one-minus-interp2))
                   (.mov vf2 a3-9)
                   )
                 ;; load first stack frame
                 (.lvf vf3 (&-> chans-to-stack 0 quad))
                 (.lvf vf4 (&-> chans-to-stack 1 quad))
                 (.lvf vf5 (&-> chans-to-stack 2 quad))
                 (.lvf vf6 (&-> chans-to-stack 3 quad))
                 (.lvf vf7 (&-> chans-to-stack 4 quad))
                 (.lvf vf8 (&-> chans-to-stack 5 quad))
                 ;; multiply by blend weight
                 (.mul.x.vf vf3 vf3 vf2)
                 (.mul.x.vf vf4 vf4 vf2)
                 (.mul.x.vf vf5 vf5 vf2)
                 (.mul.x.vf vf6 vf6 vf2)
                 (.mul.x.vf vf7 vf7 vf2)
                 (.mul.x.vf vf8 vf8 vf2)
                 ;; load second stack frame
                 (.lvf vf9 (&-> chans-to-stack 6 quad))
                 (.lvf vf10 (&-> chans-to-stack 7 quad))
                 (.lvf vf11 (&-> chans-to-stack 8 quad))
                 (.lvf vf12 (&-> chans-to-stack 9 quad))
                 (.lvf vf13 (&-> chans-to-stack 10 quad))
                 (.lvf vf14 (&-> chans-to-stack 11 quad))
                 ;; multiply by blend weight
                 (.mul.x.vf vf9 vf9 vf1)
                 (.mul.x.vf vf10 vf10 vf1)
                 (.mul.x.vf vf11 vf11 vf1)
                 (.mul.x.vf vf12 vf12 vf1)
                 (.mul.x.vf vf13 vf13 vf1)
                 (.mul.x.vf vf14 vf14 vf1)
                 ;; the add!
                 (.add.vf vf3 vf3 vf9)
                 (.add.vf vf4 vf4 vf10)
                 (.add.vf vf5 vf5 vf11)
                 (.add.vf vf6 vf6 vf12)
                 (.add.vf vf7 vf7 vf13)
                 (.add.vf vf8 vf8 vf14)
                 ;; overwrite the first
                 (.svf (&-> chans-to-stack 0 quad) vf3)
                 (.svf (&-> chans-to-stack 1 quad) vf4)
                 (.svf (&-> chans-to-stack 2 quad) vf5)
                 (.svf (&-> chans-to-stack 3 quad) vf6)
                 (.svf (&-> chans-to-stack 4 quad) vf7)
                 (.svf (&-> chans-to-stack 5 quad) vf8)
                 ;; this ends up moving the stack pointer back 1 stack frame (went back 2, then fwd 1)
                 (set! chan-vector-ptr (the-as (inline-array vector) (&+ (the-as pointer chans-to-stack) 96)))
                 )
               )
              )
            )
          ;; advance channel
          (+! chan-float-offset 4)
          )
        )

      ;; now we have figured out all the weights for each channel - we need to figure out which animations need decompressing.

      (let ((upload-count 0))
        (dotimes (upload-chan-idx (the-as int chan-count))
          ;; only upload if the weight is nonzero.
          (when (< 0.001 (-> (the-as terrain-context #x70000000) work foreground joint-work flatten-array upload-chan-idx))

            ;; determine integer frame we need
            (let* ((upload-chan (-> jc channel upload-chan-idx))
                   (anim (-> upload-chan frame-group frames))
                   (frame-num (-> upload-chan frame-num))
                   (int-frame-num (the int frame-num))
                   (frame-frac (- frame-num (the float int-frame-num)))
                   )
              (let ((last-frame (+ (-> anim num-frames) -1)))
                (if (not (-> upload-chan frame-group))
                    (format 0 "Channel ~D skel ~A frame-group is #f!!!~%" upload-chan-idx jc)
                    )
                ;; if we're past the end, clamp
                (when (>= int-frame-num (the-as int last-frame))
                  (set! frame-frac 0.0)
                  (set! int-frame-num (the-as int last-frame))
                  )
                )
              ;; set up the upload:
              (let ((upload (-> (the-as terrain-context #x70000000) work foreground joint-work uploads upload-count)))
                (set! (-> upload fixed) (-> anim fixed))
                (set! (-> upload fixed-qwc) (the-as int (-> anim fixed-qwc)))
                (set! (-> upload frame) (-> anim data int-frame-num))
                ;; if we are a fractional frame, upload 2 frames so we can interpolate from the next.
                (set! (-> upload frame-qwc) (the-as int (if (= frame-frac 0.0)
                                                            (-> anim frame-qwc)
                                                            (* (-> anim frame-qwc) 2)
                                                            )
                                                    )
                      )
                (set! (-> upload amount)
                      (-> (the-as terrain-context #x70000000) work foreground joint-work flatten-array upload-chan-idx)
                      )
                (set! (-> upload interp) frame-frac)
                )
              )
            (+! upload-count 1)
            )
          )
        (set! (-> (the-as terrain-context #x70000000) work foreground joint-work num-uploads) upload-count)
        )
      ;; record amounts in the channel so we can print it for debug.
      (dotimes (v1-26 (the-as int chan-count))
        (set! (-> jc channel v1-26 inspector-amount)
              (the-as
                uint
                (the int (* 255.0 (-> (the-as terrain-context #x70000000) work foreground joint-work flatten-array v1-26)))
                )
              )
        )
      )
    0
    )
  )

```


# Make Joint Jump Tables

This function is building a number of jump tables like this:
```
    lw v1, decompress-fixed-data-to-accumulator(s7) ;; get address of function
    addiu a0, r0, 301         ;; offset into instruction (in instructions)
    dsll a0, a0, 2            ;; convert offset to bytes
    daddu v1, v1, a0          ;; get pointer to inside of function
    lui a0, 28672             ;; store into the jump table (in the scratchpad)
    sw v1, 1632(a0)
```
This is likely some strange macro, as I think the GOAL compiler would normally have constant propagated the `addiu`/`dsll` pair.

We can see the layout of the scratchpad here:
```lisp
(deftype joint-work (structure)
  ;; used in cspace<-matrix-no-push-joint!, which is not asm
  ((temp-mtx       matrix                      :inline :offset-assert 0)
   (joint-stack    matrix-stack                :inline :offset-assert 64)

   ;; the jump tables. seems like there's 3 different modes: fix (fixed), frm (frame), pair (2x frame)
   (fix-jmp-table  (function none)             16      :offset-assert 1616) ;; guessed by decompiler
   (frm-jmp-table  (function none)             16      :offset-assert 1680) ;; guessed by decompiler
   (pair-jmp-table (function none)             16      :offset-assert 1744) ;; guessed by decompiler

   ;; upload records generated in the `flatten-joint-control-to-spr` function above
   (uploads        channel-upload-info         24 :inline     :offset-assert 1808) ;; guessed by decompiler
   (num-uploads    int32                               :offset-assert 2384)

   ;; "accumulators", which I believe will eventually contain the full transforms
   (mtx-acc        matrix                      2  :inline     :offset-assert 2400) ;; guessed by decompiler
   (tq-acc         transformq                  100 :inline    :offset-assert 2528) ;; guessed by decompiler

   ;; ?? likely destination for uploads
   (jacp-hdr       joint-anim-compressed-hdr   :inline :offset-assert 7328)
   (fixed-data     joint-anim-compressed-fixed :inline :offset-assert 7392)
   (frame-data     joint-anim-compressed-frame 2  :inline     :offset-assert 9600) ;; guessed by decompiler

   ;; used during the flatten-joint-control-to-spr function
   (flatten-array  float                       576     :offset 2400) ;; guessed by decompiler
   (flattened      vector                      24 :inline     :offset 2400) ;; guessed by decompiler
   )
```

and the upload type:
```
(deftype channel-upload-info (structure)
  "Information about an upload of animation data to a single joint channel."
  ((fixed     joint-anim-compressed-fixed  :offset-assert 0)
   (fixed-qwc int32                        :offset-assert 4)
   (frame     joint-anim-compressed-frame  :offset-assert 8)
   (frame-qwc int32                        :offset-assert 12)
   (amount    float                        :offset-assert 16)
   (interp    float                        :offset-assert 20)
   )
```

I did not learn much from staring at this yet, so I am going to move on to `calc-animation-from-spr`, and revisit once I find the actual jumps.

# `calc-animation-from-spr`


This function is entirely assembly. I approach these by adding a comment to each line:
```python
# a0 = dst-joint-anim-frame
# a1 = num-joints
    or v1, a1, r0          # v1 = num-joint

    # backup registers
    daddiu sp, sp, -192
    sq s0, 0(sp)
    sq s1, 16(sp)
    sq s2, 32(sp)
    sq s3, 48(sp)
    sq s4, 64(sp)
    sq s5, 80(sp)
    sq s6, 96(sp)
    sq t8, 112(sp)
    sq t9, 128(sp)
    sq gp, 144(sp)
    sq fp, 160(sp)
    sq ra, 176(sp)

    daddiu sp, sp, -16  # allocate another 16 bytes on the stack

    qmtc2.i vf15, r0    # vf15 = 0
    sw a1, 0(sp)        # 0(sp) = num-joints
    lui v1, 28672
    lw s1, 2384(v1)     # s1 = num-uploads
    daddiu t7, v1, 1808 # t7 = uploads
    lui s0, 4096        # generating some constant here, likely DMA register
    daddiu t1, v1, 7328 # t1 = jacp-hdr
    beq s1, r0, L53     # early return if we don't have any uploads
    ori s0, s0, 54272   # more DMA register constant stuff

B1:
    lw t2, 0(t7)       # t2 = fixed
    addiu t3, r0, 7392 # t3 = fixed-data
    lw t4, 4(t7)       # t4 = fixed-qwc
    addiu v1, r0, 256  # DMA constant?
    sw t2, 16(s0)      # DMA setup
    vaddw.xyzw vf14, vf15, vf0 # vf14 = 1, 1, 1, 1
    sw t3, 128(s0)     # DMA setup
    sll r0, r0, 0      #
    sw t4, 32(s0)      # DMA setup QWC
    sync.l
    sw v1, 0(s0)       # DMA GO!!
    sync.l
    lw t9, clear-frame-accumulator(s7) # t9 = this function
    vadd.yz vf14, vf14, vf14           # vf14 = [1, 2, 2, 1]
    lw s2, 0(sp)                       # s2 = num-joint
    sll r0, r0, 0
    jalr ra, t9                        # call!
    vadd.yz vf14, vf14, vf14           # vf14 = [1, 4, 4, 1]

B2:
L49:
    lw v1, 0(s0)                       # wait on DMA
    sll r0, r0, 0
    andi v1, v1, 256
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    bne v1, r0, L49
    sll r0, r0, 0

B3:
    lw t2, 8(t7)                      # t2 = frame
    addiu t3, r0, 9600                # t3 = frame-data
    lw t4, 12(t7)                     # t4 = frame-qwc
    addiu v1, r0, 256                 # dma constant
    sw t2, 16(s0)                     # dma (src addr)
    sll r0, r0, 0
    sw t3, 128(s0)                    # dma (dst addr)
    sll r0, r0, 0
    sw t4, 32(s0)                     # dma (qwc)
    sync.l
    sw v1, 0(s0)                      # dma (start)
    sync.l
    lw a2, 16(t7)
    lui a1, 28672
    lw t9, decompress-fixed-data-to-accumulator(s7)
    daddiu a1, a1, 7392              # a1 = fixed-data
    jalr ra, t9
    daddiu s1, s1, -1                # decrement upload counter

B4:
L50:
    lw v1, 0(s0)                     # wait on frame data dma
    sll r0, r0, 0
    andi v1, v1, 256
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    bne v1, r0, L50
    sll r0, r0, 0

B5:
    beq s1, r0, L51                 # jump over next dma start if there's nothing next
    sll r0, r0, 0

B6:
    lw t2, 24(t7)                   # dma the next upload
    addiu t3, r0, 7392
    lw t4, 28(t7)
    addiu v1, r0, 256
    sw t2, 16(s0)
    sll r0, r0, 0
    sw t3, 128(s0)
    sll r0, r0, 0
    sw t4, 32(s0)
    sync.l
    sw v1, 0(s0)
    sync.l
B7:
L51:
    lw t0, 20(t7)                 # t0 = interp (between consecutive frames)
    lui a1, 28672
    lw a2, 16(t7)                 # a2 = amount (weight)
    daddiu a1, a1, 9600           # a1 = frame-data
    beq t0, r0, L52               # if interp is 0, take the non-pair case, just one frame
    sll r0, r0, 0

B8:
    lw a3, 12(t7)                # a3 = frame-qwc
    sll r0, r0, 0
    lw t9, decompress-frame-data-pair-to-accumulator(s7)
    sll r0, r0, 0
    jalr ra, t9
    sll a3, a3, 3

    bne s1, r0, L49
    daddiu t7, t7, 24

B9:
    lw t9, normalize-frame-quaternions(s7)
    sll r0, r0, 0
    lw s2, 0(sp)
    sll r0, r0, 0
    jalr ra, t9
    sll r0, r0, 0

    daddiu sp, sp, 16
    sll r0, r0, 0
    lq s0, 0(sp)
    lq s1, 16(sp)
    lq s2, 32(sp)
    lq s3, 48(sp)
    lq s4, 64(sp)
    lq s5, 80(sp)
    lq s6, 96(sp)
    lq t8, 112(sp)
    lq t9, 128(sp)
    lq gp, 144(sp)
    lq ra, 176(sp)
    lq fp, 160(sp)
    jr ra
    daddiu sp, sp, 192

B10:
L52:
    lw t9, decompress-frame-data-to-accumulator(s7)
    sll r0, r0, 0
    jalr ra, t9
    sll r0, r0, 0

    bne s1, r0, L49
    daddiu t7, t7, 24

B11:
    lw t9, normalize-frame-quaternions(s7)
    sll r0, r0, 0
    lw s2, 0(sp)
    sll r0, r0, 0
    jalr ra, t9
    sll r0, r0, 0

B12:
L53:
    daddiu sp, sp, 16
    sll r0, r0, 0
    lq s0, 0(sp)
    lq s1, 16(sp)
    lq s2, 32(sp)
    lq s3, 48(sp)
    lq s4, 64(sp)
    lq s5, 80(sp)
    lq s6, 96(sp)
    lq t8, 112(sp)
    lq t9, 128(sp)
    lq gp, 144(sp)
    lq ra, 176(sp)
    lq fp, 160(sp)
    jr ra
    daddiu sp, sp, 192

    jr ra
    daddu sp, sp, r0
```

The basic operation is:
- call `clear-frame-accumulator`, to zero out the tq and matrix accumulators
- for each upload
  - DMA the fixed data, then `decompress-fixed-data-to-accumulator`
  - DMA the fixed data
  - Call `decompress-frame-data-pair-to-accumulator` or `decompress-frame-data-to-accumulator`. Use the `pair` one if interp is nonzero, meaning we need to combine two consecutive frames
  - Call `normalize-frame-quaternions`


# `clear-frame-accumulator`
Just sets the accumulator to 0. The first two joints are matrices and zeroed entirely. The remaining ones are `transformq` (position, quaternion, scale). The pos/quat are zeroed, but not the scale.

# `normalize-frame-quaternion`
This function simply sets `trans.w = 1`, `scale.w = 1`, and normalizes the quaternions.
The quaternions may be slightly off due to the blending or decompression.

```python
# s2 = num-joints
L91:
    daddiu sp, sp, -16
B0:
    daddiu s2, s2, -2    # subtract off first two matrix joints
    sw a0, 0(sp)
    daddiu a0, a0, 128   # seek past matrices in the accumulator
B1:
L92:                     # per transformq loop
    lqc2 vf4, 16(a0)     # vf4 = quat
    lqc2 vf1, 0(a0)      # vf1 = trans
    lqc2 vf7, 32(a0)     # vf7 = scale
    vmul.xyzw vf10, vf4, vf4  # vf10 = x^2, y^2, z^2, w^2
    vmove.w vf1, vf0          # vf1.w = 1
    vmove.w vf7, vf0          # vf7.w = 1
    vmulaw.xyzw acc, vf0, vf10    # sum up x^2, y^2, z^2, w^2
    vmaddaz.xyzw acc, vf0, vf10
    vmadday.xyzw acc, vf0, vf10
    vmaddx.xyzw vf10, vf0, vf10
    sqc2 vf1, 0(a0)              # store fixed trans
    sqc2 vf7, 32(a0)             # store fixed scale
    daddiu a0, a0, 48            # advance tq ptr
    vrsqrt Q, vf0.w, vf10.w      # get length
    vwaitq
    vmulq.xyzw vf4, vf4, Q       # multiply by inverse length to normalize
    daddiu s2, s2, -1            # dec count
    bne s2, r0, L92
    sqc2 vf4, -32(a0)            # store fixed quat.

B2:
    lw a0, 0(sp)
    sll r0, r0, 0
    jr ra
    daddiu sp, sp, 16

    jr ra
    daddu sp, sp, r0
```

# `decompress-fixed-data-to-accumulator`

Useful types
```lisp
(deftype joint-anim-compressed-hdr (structure)
  "Header for the compressed joint animation format."
  ((control-bits uint32 14 :offset-assert 0) ;; guessed by decompiler
   (num-joints   uint32    :offset-assert 56)
   (matrix-bits  uint32    :offset-assert 60)
   )
  :method-count-assert 9
  :size-assert         #x40
  :flag-assert         #x900000040
  )

(deftype joint-anim-compressed-fixed (structure)
  ((hdr       joint-anim-compressed-hdr :inline :offset-assert 0)
   (offset-64 uint32                            :offset-assert 64)
   (offset-32 uint32                            :offset-assert 68)
   (offset-16 uint32                            :offset-assert 72)
   (reserved  uint32                            :offset-assert 76)
   (data      vector                    133 :inline    :offset-assert 80) ;; guessed by decompiler
   )
```

```python
# a0 = accumulator
# a1 = fixed data (joint-anim-compressed-fixed)
# a2 = amount
# t1 = jacp-header (unset when we get here)
# s2 = num-joints
# vf14 = [1, 4, 4, 1]

L79:
    # copy the header to the jacp-header (i think, so we can continue using in frame functions
    # even when dma'ing the next upload's fixed to here)
    lq t4, 0(a1)         # t4 = fixed.hdr.quad[0]
    daddiu sp, sp, -16
    lq t5, 16(a1)        # t4 = fixed.hdr.quad[1]
    sll r0, r0, 0
    sq t4, 0(t1)         # move to jacp header
    sll r0, r0, 0
    sq t5, 16(t1)        # move to jacp header
    sll r0, r0, 0
    lq t4, 32(a1)        # quad2
    sll r0, r0, 0
    lq t5, 48(a1)        # quad3
    sll r0, r0, 0
    sq t4, 32(t1)
    sll r0, r0, 0
    sq t5, 48(t1)

    sll r0, r0, 0
    sq a0, 0(sp)        # backup acc pointer
    sll r0, r0, 0
    qmtc2.i vf13, a2    # vf13.x = amount
    lui t2, 28672       # scratchpad addr
    lw t4, 64(a1)       # t4 = fixed.offset-64
    daddiu v1, a1, 80   # v1 = fixed.data
    lw t5, 68(a1)       # t4 = fixed.offset-32
    daddu s5, t1, r0    # s5 = control-bits-ptr
    lw t6, 72(a1)       # t6 = fixed.offset-16
    daddu t4, t4, v1    # t4 = fixed.data + fixed.offset-64
    vmulx.xyzw vf13, vf14, vf13 # vf13 = [amt, 4*amt, 4*amt, amt]
    daddiu t2, t2, 1616   # t2 = fix-jmp-table
    lw s2, 56(t1)        # hdr.num_joints = num-joints
    daddu t5, t5, v1   # t5 = fixed.data + fixed.offset-32
    lw s4, 60(t1)       # s4 = matrix-bits
    daddu t6, t6, v1    # t6 = fixed.data + fixed.offset-16
    addiu s3, r0, 8     # s3 = 8
    daddiu s5, s5, 4    # s5 (control-bits-ptr) += 4 bytes
    andi t3, s4, 1      # t3 = matrix-bits & 1
    sll r0, r0, 0
    bne t3, r0, L80
    sll r0, r0, 0

B1: # if (matrix-bits & 1) == 0
    lqc2 vf1, 0(t4)      # load matrix from offset-64 data (1, 2, 4, 4)
    lqc2 vf2, 16(t4)
    lqc2 vf3, 32(t4)
    lqc2 vf4, 48(t4)
    lqc2 vf9, 0(a0)      # load matrix from accumulator
    daddiu t4, t4, 64    # increment data-64 ptr
    lqc2 vf10, 16(a0)
    sll r0, r0, 0
    lqc2 vf11, 32(a0)
    lqc2 vf12, 48(a0)
    vmulaw.xyzw acc, vf9, vf0     # acc = existing
    vmaddx.xyzw vf9, vf1, vf13    # existing += amount * new
    vmulaw.xyzw acc, vf10, vf0
    vmaddx.xyzw vf10, vf2, vf13
    vmulaw.xyzw acc, vf11, vf0
    vmaddx.xyzw vf11, vf3, vf13
    vmulaw.xyzw acc, vf12, vf0
    vmaddx.xyzw vf12, vf4, vf13
    sqc2 vf9, 0(a0)              # store back modified matrix.
    sqc2 vf10, 16(a0)
    sqc2 vf11, 32(a0)
    sqc2 vf12, 48(a0)
B2:
L80: # endif
    andi t3, s4, 2    # check matrix bit
    daddiu a0, a0, 64 # increment acc ptr to second matrix
    bne t3, r0, L81
    sll r0, r0, 0

B3: # if (matrix-bits & 2) == 0
    # same stuff as for the first matrix.
    lqc2 vf1, 0(t4)
    lqc2 vf2, 16(t4)
    lqc2 vf3, 32(t4)
    lqc2 vf4, 48(t4)
    lqc2 vf9, 0(a0)
    daddiu t4, t4, 64
    lqc2 vf10, 16(a0)
    sll r0, r0, 0
    lqc2 vf11, 32(a0)
    lqc2 vf12, 48(a0)
    vmulaw.xyzw acc, vf9, vf0
    vmaddx.xyzw vf9, vf1, vf13
    vmulaw.xyzw acc, vf10, vf0
    vmaddx.xyzw vf10, vf2, vf13
    vmulaw.xyzw acc, vf11, vf0
    vmaddx.xyzw vf11, vf3, vf13
    vmulaw.xyzw acc, vf12, vf0
    vmaddx.xyzw vf12, vf4, vf13
    sqc2 vf9, 0(a0)
    sqc2 vf10, 16(a0)
    sqc2 vf11, 32(a0)
    sqc2 vf12, 48(a0)
B4:
L81:
    lw s4, -4(s5)     # load control-bits-ptr[-1]
    daddiu a0, a0, 64 # increment to first TQ
B5:
L82:
    andi t3, s4, 15   # grab lower 4 bits of the control bits
    sra s4, s4, 4     # shift in next 4
    sll t3, t3, 2     # multiply by 4 (ah - this tells us which case!)
    daddiu s3, s3, -1 # count down if we need to load the next u32 control bits or not.
    daddu t3, t3, t2  # look up in the jump table
    daddiu s2, s2, -1 # count down joints
    lw t3, 0(t3)      # load from jump table!
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    jr t3
    sll r0, r0, 0

B6: # JUMP TABLE 7 and 15, also used to advance
L83:
    beq s2, r0, L90     # if out of joints, end.
    daddiu a0, a0, 48   # increment acc ptr.

B7:
    bne s3, r0, L82     # see if we need to load a new control word or not.
    sll r0, r0, 0

B8:
    lw s4, 0(s5)        # load up new control word
    daddiu s5, s5, 4
    beq r0, r0, L82
    addiu s3, r0, 8     # back to the beginning!

B9: # JUMP TABLE 0
    ld t9, 0(t4)        # load one uint64 from data64
    daddiu t4, t4, 8
    lqc2 vf6, 16(a0)    # vf6 = acc.quat
    pextlh t9, t9, r0   # expand to uint32's (upper)
    psraw t9, t9, 16    # treat as signed s16
    qmtc2.i vf4, t9     # vf4 = new_data (sext)
    lw s6, 0(t5)        # load one uint32 from data32
    daddiu t5, t5, 4
    lh gp, 0(t6)        # load one uint16 from data16
    daddiu t6, t6, 2
    lqc2 vf3, 0(a0)     # vf3 = acc.trans
    vitof15.xyzw vf4, vf4 # quat int -> float
    pextlw s6, gp, s6   # s6 = [XX, XX, u16, u32]
    pextlh s6, s6, r0   # s6 = [XX, u16, u32[1], u32[0]]
    psraw s6, s6, 16    # sext
    vmul.xyzw vf10, vf4, vf6 # elementwise quat product
    qmtc2.i vf1, s6     # vf1 = the first triple
    lw t8, 0(t5)        # load another uint32, uint16
    daddiu t5, t5, 4
    lh fp, 0(t6)
    daddiu t6, t6, 2
    vmulaw.xyzw acc, vf10, vf0    # add up quaternions (dot product)
    vmaddaz.xyzw acc, vf0, vf10
    vmadday.xyzw acc, vf0, vf10
    vmaddx.xyzw vf10, vf0, vf10

    vitof0.xyzw vf1, vf1         # first triple to float (trans)
    lqc2 vf9, 32(a0)             # acc scale
    pextlw t8, fp, t8            # triple stuff
    qmfc2.i t9, vf10             # quat dot to grp
    pextlh t8, t8, r0            # triple stuff
    psraw t8, t8, 16             # triple sext
    qmtc2.i vf7, t8              # second triple
    pcpyud t9, t9, r0            # check sign of quaternion
    bltzl t9, L84
B10:
    vsub.xyzw vf4, vf15, vf4    # flip quaternion if dot product is negative.

B11:
L84:
    vitof12.xyzw vf7, vf7       # second triple (scale)
    vmulaw.xyzw acc, vf6, vf0
    vmaddx.xyzw vf6, vf4, vf13  # vf6 = acc.quat + amount * decompressed_quat
    vmulaw.xyzw acc, vf3, vf0
    vmaddy.xyzw vf3, vf1, vf13  # vf3 = acc.trans + amount * 4 * decompressed_trans
    vmulaw.xyzw acc, vf9, vf0   # vf9 = acc.scale + amount * decompressed_scale
    vmaddx.xyzw vf9, vf7, vf13
    sqc2 vf3, 0(a0)
    sqc2 vf6, 16(a0)
    sqc2 vf9, 32(a0)
    beq r0, r0, L83
    sll r0, r0, 0

B12: # JUMP TABLE 8
    ld t9, 8(t4)      # one ahead data64 load??
    daddiu t4, t4, 8
    lqc2 vf6, 16(a0)  # vf6 = acc.quat
    pextlh t9, t9, r0 #
    psraw t9, t9, 16
    qmtc2.i vf4, t9   # quat expand
    ld s6, -8(t4)     # now load the previous data64
    daddiu t4, t4, 8
    lw gp, 0(t5)      # load one uint32 from data32
    daddiu t5, t5, 4
    lqc2 vf3, 0(a0)   # vf3 = acc.trans
    vitof15.xyzw vf4, vf4 # quat int->float
    pcpyld s6, gp, s6  # trans combingin ints
    vmul.xyzw vf10, vf4, vf6 # quat dot
    qmtc2.i vf1, s6   # trans int
    lw t8, 0(t5)      # scale load
    daddiu t5, t5, 4
    lh fp, 0(t6)
    daddiu t6, t6, 2
    vmulaw.xyzw acc, vf10, vf0   # quat dot add
    vmaddaz.xyzw acc, vf0, vf10
    vmadday.xyzw acc, vf0, vf10
    vmaddx.xyzw vf10, vf0, vf10
    lqc2 vf9, 32(a0)     # existing scale
    pextlw t8, fp, t8
    qmfc2.i t9, vf10
    pextlh t8, t8, r0
    psraw t8, t8, 16
    qmtc2.i vf7, t8
    pcpyud t9, t9, r0
    bltzl t9, L85
B13:
    vsub.xyzw vf4, vf15, vf4

B14:
L85:
    vitof12.xyzw vf7, vf7
    vmulaw.xyzw acc, vf6, vf0
    vmaddx.xyzw vf6, vf4, vf13
    vmulaw.xyzw acc, vf3, vf0
    vmaddx.xyzw vf3, vf1, vf13
    vmulaw.xyzw acc, vf9, vf0
    vmaddx.xyzw vf9, vf7, vf13
    sqc2 vf3, 0(a0)
    sqc2 vf6, 16(a0)
    sqc2 vf9, 32(a0)
    beq r0, r0, L83
    sll r0, r0, 0

B15: # JUMP 1, 9
    ld t9, 0(t4)      # 64 load for quat
    daddiu t4, t4, 8
    lqc2 vf6, 16(a0)  # vf6 = acc.quat
    pextlh t9, t9, r0
    psraw t9, t9, 16
    qmtc2.i vf4, t9   # vf4 = quat expand int
    lw t8, 0(t5)      # load a 32-16
    daddiu t5, t5, 4
    lh fp, 0(t6)
    daddiu t6, t6, 2
    lqc2 vf9, 32(a0) # vf9 = acc.scale
    vitof15.xyzw vf4, vf4
    pextlw t8, fp, t8
    pextlh t8, t8, r0
    psraw t8, t8, 16  # scale expand
    vmul.xyzw vf10, vf4, vf6
    qmtc2.i vf7, t8  # vf7 = scale
    vmulaw.xyzw acc, vf10, vf0
    vitof12.xyzw vf7, vf7
    vmaddaz.xyzw acc, vf0, vf10
    vmadday.xyzw acc, vf0, vf10
    vmaddx.xyzw vf10, vf0, vf10
    vmulaw.xyzw acc, vf9, vf0
    vmaddx.xyzw vf9, vf7, vf13
    qmfc2.i t9, vf10
    pcpyud t9, t9, r0
    bltzl t9, L86
B16:
    vsub.xyzw vf4, vf15, vf4

B17:
L86:
    vmulaw.xyzw acc, vf6, vf0
    vmaddx.xyzw vf6, vf4, vf13
    sqc2 vf6, 16(a0)
    sqc2 vf9, 32(a0)
    beq r0, r0, L83
    sll r0, r0, 0

B18: # JUMP 2
    lw s6, 0(t5) # load 32, 16
    daddiu t5, t5, 4
    lh gp, 0(t6)
    daddiu t6, t6, 2
    lqc2 vf3, 0(a0) # old trans
    pextlw s6, gp, s6
    pextlh s6, s6, r0
    psraw s6, s6, 16
    qmtc2.i vf1, s6
    lw t8, 0(t5)  # load 32, 16
    daddiu t5, t5, 4
    lh fp, 0(t6)
    daddiu t6, t6, 2
    lqc2 vf9, 32(a0)
    vitof0.xyzw vf1, vf1
    pextlw t8, fp, t8
    pextlh t8, t8, r0
    psraw t8, t8, 16
    qmtc2.i vf7, t8
    vmulaw.xyzw acc, vf3, vf0
    vmaddy.xyzw vf3, vf1, vf13
    vitof12.xyzw vf7, vf7
    vmulaw.xyzw acc, vf9, vf0
    vmaddx.xyzw vf9, vf7, vf13
    sqc2 vf3, 0(a0)
    sqc2 vf9, 32(a0)
    beq r0, r0, L83
    sll r0, r0, 0

B19: # JUMP 10
    ld s6, 0(t4)
    daddiu t4, t4, 8
    lw gp, 0(t5)
    daddiu t5, t5, 4
    lqc2 vf3, 0(a0)
    pcpyld s6, gp, s6
    qmtc2.i vf1, s6
    lw t8, 0(t5)
    daddiu t5, t5, 4
    lh fp, 0(t6)
    daddiu t6, t6, 2
    lqc2 vf9, 32(a0)
    pextlw t8, fp, t8
    pextlh t8, t8, r0
    psraw t8, t8, 16
    qmtc2.i vf7, t8
    vmulaw.xyzw acc, vf3, vf0
    vmaddx.xyzw vf3, vf1, vf13
    vitof12.xyzw vf7, vf7
    vmulaw.xyzw acc, vf9, vf0
    vmaddx.xyzw vf9, vf7, vf13
    sqc2 vf3, 0(a0)
    sqc2 vf9, 32(a0)
    beq r0, r0, L83
    sll r0, r0, 0

B20: # JUMP 3, 11
    lw t8, 0(t5)
    daddiu t5, t5, 4
    lh fp, 0(t6)
    daddiu t6, t6, 2
    lqc2 vf9, 32(a0)
    pextlw t8, fp, t8
    pextlh t8, t8, r0
    psraw t8, t8, 16
    qmtc2.i vf7, t8
    vitof12.xyzw vf7, vf7
    vmulaw.xyzw acc, vf9, vf0
    vmaddx.xyzw vf9, vf7, vf13
    sqc2 vf9, 32(a0)
    beq r0, r0, L83
    sll r0, r0, 0

B21: # JUMP 4
    ld t9, 0(t4)
    daddiu t4, t4, 8
    lqc2 vf6, 16(a0)
    pextlh t9, t9, r0
    psraw t9, t9, 16
    qmtc2.i vf4, t9
    lw s6, 0(t5)
    daddiu t5, t5, 4
    lh gp, 0(t6)
    daddiu t6, t6, 2
    lqc2 vf3, 0(a0)
    vitof15.xyzw vf4, vf4
    pextlw s6, gp, s6
    pextlh s6, s6, r0
    psraw s6, s6, 16
    vmul.xyzw vf10, vf4, vf6
    qmtc2.i vf1, s6
    vmulaw.xyzw acc, vf10, vf0
    vitof0.xyzw vf1, vf1
    vmaddaz.xyzw acc, vf0, vf10
    vmadday.xyzw acc, vf0, vf10
    vmaddx.xyzw vf10, vf0, vf10
    vmulaw.xyzw acc, vf3, vf0
    vmaddy.xyzw vf3, vf1, vf13
    qmfc2.i t9, vf10
    pcpyud t9, t9, r0
    bltzl t9, L87
B22:
    vsub.xyzw vf4, vf15, vf4

B23:
L87:
    vmulaw.xyzw acc, vf6, vf0
    vmaddx.xyzw vf6, vf4, vf13
    sqc2 vf3, 0(a0)
    sqc2 vf6, 16(a0)
    beq r0, r0, L83
    sll r0, r0, 0

B24: # JUMP 12
    ld t9, 8(t4)
    daddiu t4, t4, 8
    lqc2 vf6, 16(a0)
    pextlh t9, t9, r0
    psraw t9, t9, 16
    qmtc2.i vf4, t9
    ld s6, -8(t4)
    daddiu t4, t4, 8
    lw gp, 0(t5)
    daddiu t5, t5, 4
    lqc2 vf3, 0(a0)
    vitof15.xyzw vf4, vf4
    pcpyld s6, gp, s6
    vmul.xyzw vf10, vf4, vf6
    qmtc2.i vf1, s6
    vmulaw.xyzw acc, vf10, vf0
    vmaddaz.xyzw acc, vf0, vf10
    vmadday.xyzw acc, vf0, vf10
    vmaddx.xyzw vf10, vf0, vf10
    vmulaw.xyzw acc, vf3, vf0
    vmaddx.xyzw vf3, vf1, vf13
    qmfc2.i t9, vf10
    pcpyud t9, t9, r0
    bltzl t9, L88
B25:
    vsub.xyzw vf4, vf15, vf4

B26:
L88:
    vmulaw.xyzw acc, vf6, vf0
    vmaddx.xyzw vf6, vf4, vf13
    sqc2 vf3, 0(a0)
    sqc2 vf6, 16(a0)
    beq r0, r0, L83
    sll r0, r0, 0

B27: # JUMP 5, 13
    ld t9, 0(t4)
    daddiu t4, t4, 8
    lqc2 vf6, 16(a0)
    pextlh t9, t9, r0
    psraw t9, t9, 16
    qmtc2.i vf4, t9
    vitof15.xyzw vf4, vf4
    vmul.xyzw vf10, vf4, vf6
    vmulaw.xyzw acc, vf10, vf0
    vmaddaz.xyzw acc, vf0, vf10
    vmadday.xyzw acc, vf0, vf10
    vmaddx.xyzw vf10, vf0, vf10
    qmfc2.i t9, vf10
    pcpyud t9, t9, r0
    bltzl t9, L89
B28:
    vsub.xyzw vf4, vf15, vf4

B29:
L89:
    vmulaw.xyzw acc, vf6, vf0
    vmaddx.xyzw vf6, vf4, vf13
    sqc2 vf6, 16(a0)
    beq r0, r0, L83
    sll r0, r0, 0

B30: # JUMP 6
    lw s6, 0(t5)
    daddiu t5, t5, 4
    lh gp, 0(t6)
    daddiu t6, t6, 2
    lqc2 vf3, 0(a0)
    pextlw s6, gp, s6
    pextlh s6, s6, r0
    psraw s6, s6, 16
    qmtc2.i vf1, s6
    vitof0.xyzw vf1, vf1
    vmulaw.xyzw acc, vf3, vf0
    vmaddy.xyzw vf3, vf1, vf13
    sqc2 vf3, 0(a0)
    beq r0, r0, L83
    sll r0, r0, 0

B31:
    ld s6, 0(t4)
    daddiu t4, t4, 8
    lw gp, 0(t5)
    daddiu t5, t5, 4
    lqc2 vf3, 0(a0)
    pcpyld s6, gp, s6
    qmtc2.i vf1, s6
    vmulaw.xyzw acc, vf3, vf0
    vmaddx.xyzw vf3, vf1, vf13
    sqc2 vf3, 0(a0)
    beq r0, r0, L83
    sll r0, r0, 0

B32:
L90:
    lq a0, 0(sp)
    sll r0, r0, 0
    jr ra
    daddiu sp, sp, 16

    jr ra
    daddu sp, sp, r0
```

Explanation:
```python
# set up pointers
data64 = data + offset_64
data32 = data + offset_32
data16 = data + offset_16

# do matrices
if (matrix_bits & 1) == 0:
  acc.matrix[0] += amount * Matrix(data64)
  data64 += sizeof(Matrix)

if (matrix_bits & 2) == 0:
  acc.matrix[1] += amount * Matrix(data64)
  data64 += sizeof(Matrix)

# decomp helpers
def unpack_quat_s16s_from_d64():
  data = S16x4(data64)
  data64++
  quat_ints = [data[0], data[1], data[2], data[3]]

# the control words have 4-bits per joint

for joint_idx in range(num_joints):
  control = control_words.extract_4bits(joint_idx)
  if control == 0:
    # quat is packed as 4x s16's in data64, scale is 32768
    # dot product is checked before lerping.
    # trans is packed as 2x s16's in data32, 1x s16 in data16, scale is 4
    # scale is packed as 2x s16's in data32, 1x s16 in data16, scale is 4096

  if control == 7:
    pass # nothing to do!
  if control == 8:
    # trans is packed as 2x s32's in data64, 1x s32 in data32, scale is 1
    # quat is packed as 4x s16's in data64, scale is 32768
    # dot product is checked before lerping.
    # scale is packed as 2x s16's in data32, 1x s16 in data16, scale is 4096
  if control == 15:
    pass # nothing to do!

```

Control Modes:

0. FIXED has `trans-32-16-4`, `quat-64-32768`, `scale-32-16-4096`, FRAME has nothing
1. FIXED has `trans-0`, `quat-64-32768`, `scale-32-16-4096`, FRAME has trans (small)
2. FIXED has `trans-32-16-4`, `quat-0`, `scale-32-16-4096`, FRAME has quat
3. FIXED has `trans-0`, `quat-0`, `scale-32-16-4096`
4. FIXED has `trans-32-16-4`, `quat-64-32768`, `scale-0`
5. FIXED has `trans-0`, `quat-64-32768`, `scale-0`
6. FIXED has `trans-32-16-4`, `quat-0`, `scale-0`
7. FIXED 0
8. FIXED as `trans-64-32-1`, `quat-64-32768`, `scale-32-16-4096`, FRAME has nothing
9. FIXED has `trans-0`, `quat-64-32768`, `scale-32-16-4096`, FRAME has trans (large)
10. FIXED has `trans-64-32-1`, `quat-0`, `scale-32-16-4096`
11. FIXED has `trans-0`, `quat-0`, `scale-32-16-4096`, FRAME HAS QUAT
12. FIXED has `trans-64-32-1`, `quat-64-32768`, `scale-0`
13. FIXED has `trans-0`, `quat-64-32768`, `scale-0`
14. FIXED has `trans-64-32-1`, `quat-0`, `scale-0`
15. FIXED 0

So bits are:
control:

0. frame-trans
1. frame-quat
2. frame-scale
4. big-trans

matrix:

0. matrix 0 from frame
1. matrix 1 from frame

# `decompress-frame-data-to-accumulator`

```lisp
(deftype joint-anim-compressed-frame (structure)
  ((offset-64 uint32     :offset-assert 0)
   (offset-32 uint32     :offset-assert 4)
   (offset-16 uint32     :offset-assert 8)
   (reserved  uint32     :offset-assert 12)
   (data      vector 133 :inline :offset-assert 16) ;; guessed by decompiler
   )
  :method-count-assert 9
  :size-assert         #x860
  :flag-assert         #x900000860
  )
```

```python
# a2 = amount (weight)
# a1 = frame-data

    qmtc2.i vf13, a2 # vf13 = amount
    sq a0, 0(sp)     # back up accumulator
    lui t2, 28672    # t2 = spad
    lw t4, 0(a1)     # t4 = offset64
    daddiu v1, a1, 16 # v1 = data
    lw t5, 4(a1)     # offset32
    daddu s5, t1, r0 # control bits ptr
    lw t6, 8(a1)    # offset16
    daddu t4, t4, v1 # t4 = data64
    vmulx.xyzw vf13, vf14, vf13 # usual [amt, 4*amt, 4*amt, amt] stuff
    daddiu t2, t2, 1680 # t2 = jump table
    lw s2, 56(t1)     # s2 = num-joints
    daddu t5, t5, v1  # t5 = data32
    lw s4, 60(t1)     # s4 = matrix-bits
    daddu t6, t6, v1  # t6 = data16
    addiu s3, r0, 8   # load next u32 word counter
    daddiu s5, s5, 4  # control word ptr
    andi t3, s4, 1    # check matrix bit
    sll r0, r0, 0
    beq t3, r0, L68
    sll r0, r0, 0

B1: # if matrix bit is set:
    lqc2 vf1, 0(t4)
    lqc2 vf2, 16(t4)
    lqc2 vf3, 32(t4)
    lqc2 vf4, 48(t4)
    lqc2 vf9, 0(a0)
    daddiu t4, t4, 64
    lqc2 vf10, 16(a0)
    sll r0, r0, 0
    lqc2 vf11, 32(a0)
    lqc2 vf12, 48(a0)
    vmulaw.xyzw acc, vf9, vf0
    vmaddx.xyzw vf9, vf1, vf13
    vmulaw.xyzw acc, vf10, vf0
    vmaddx.xyzw vf10, vf2, vf13
    vmulaw.xyzw acc, vf11, vf0
    vmaddx.xyzw vf11, vf3, vf13
    vmulaw.xyzw acc, vf12, vf0
    vmaddx.xyzw vf12, vf4, vf13
    sqc2 vf9, 0(a0)
    sqc2 vf10, 16(a0)
    sqc2 vf11, 32(a0)
    sqc2 vf12, 48(a0)
B2:
L68:
    andi t3, s4, 2
    daddiu a0, a0, 64
    beq t3, r0, L69
    sll r0, r0, 0

B3:
    lqc2 vf1, 0(t4)
    lqc2 vf2, 16(t4)
    lqc2 vf3, 32(t4)
    lqc2 vf4, 48(t4)
    lqc2 vf9, 0(a0)
    daddiu t4, t4, 64
    lqc2 vf10, 16(a0)
    sll r0, r0, 0
    lqc2 vf11, 32(a0)
    lqc2 vf12, 48(a0)
    vmulaw.xyzw acc, vf9, vf0
    vmaddx.xyzw vf9, vf1, vf13
    vmulaw.xyzw acc, vf10, vf0
    vmaddx.xyzw vf10, vf2, vf13
    vmulaw.xyzw acc, vf11, vf0
    vmaddx.xyzw vf11, vf3, vf13
    vmulaw.xyzw acc, vf12, vf0
    vmaddx.xyzw vf12, vf4, vf13
    sqc2 vf9, 0(a0)
    sqc2 vf10, 16(a0)
    sqc2 vf11, 32(a0)
    sqc2 vf12, 48(a0)
B4:
L69:
    lw s4, -4(s5)
    daddiu a0, a0, 64
B5:
L70:
    andi t3, s4, 15
    sra s4, s4, 4
    sll t3, t3, 2
    daddiu s3, s3, -1
    daddu t3, t3, t2
    daddiu s2, s2, -1
    lw t3, 0(t3)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    jr t3
    sll r0, r0, 0

B6: # JUMP 0, 8
L71:
    beq s2, r0, L78
    daddiu a0, a0, 48

B7:
    bne s3, r0, L70
    sll r0, r0, 0

B8:
    lw s4, 0(s5)
    daddiu s5, s5, 4
    beq r0, r0, L70
    addiu s3, r0, 8

B9: # JUMP 1
    lw s6, 0(t5)
    daddiu t5, t5, 4
    lh gp, 0(t6)
    daddiu t6, t6, 2
    lqc2 vf3, 0(a0)
    pextlw s6, gp, s6
    pextlh s6, s6, r0
    psraw s6, s6, 16
    qmtc2.i vf1, s6
    vitof0.xyzw vf1, vf1
    vmulaw.xyzw acc, vf3, vf0
    vmaddy.xyzw vf3, vf1, vf13
    sqc2 vf3, 0(a0)
    beq r0, r0, L71
    sll r0, r0, 0

B10: # JUMP 9
    ld s6, 0(t4)
    daddiu t4, t4, 8
    lw gp, 0(t5)
    daddiu t5, t5, 4
    lqc2 vf3, 0(a0)
    pcpyld s6, gp, s6
    qmtc2.i vf1, s6
    vmulaw.xyzw acc, vf3, vf0
    vmaddx.xyzw vf3, vf1, vf13
    sqc2 vf3, 0(a0)
    beq r0, r0, L71
    sll r0, r0, 0

B11:
    ld t9, 0(t4)
    daddiu t4, t4, 8
    lqc2 vf6, 16(a0)
    pextlh t9, t9, r0
    psraw t9, t9, 16
    qmtc2.i vf4, t9
    vitof15.xyzw vf4, vf4
    vmul.xyzw vf10, vf4, vf6
    vmulaw.xyzw acc, vf10, vf0
    vmaddaz.xyzw acc, vf0, vf10
    vmadday.xyzw acc, vf0, vf10
    vmaddx.xyzw vf10, vf0, vf10
    qmfc2.i t9, vf10
    pcpyud t9, t9, r0
    bltzl t9, L72
B12:
    vsub.xyzw vf4, vf15, vf4

B13:
L72:
    vmulaw.xyzw acc, vf6, vf0
    vmaddx.xyzw vf6, vf4, vf13
    sqc2 vf6, 16(a0)
    beq r0, r0, L71
    sll r0, r0, 0

B14:
    ld t9, 0(t4)
    daddiu t4, t4, 8
    lqc2 vf6, 16(a0)
    pextlh t9, t9, r0
    psraw t9, t9, 16
    qmtc2.i vf4, t9
    lw s6, 0(t5)
    daddiu t5, t5, 4
    lh gp, 0(t6)
    daddiu t6, t6, 2
    lqc2 vf3, 0(a0)
    vitof15.xyzw vf4, vf4
    pextlw s6, gp, s6
    pextlh s6, s6, r0
    psraw s6, s6, 16
    vmul.xyzw vf10, vf4, vf6
    qmtc2.i vf1, s6
    vmulaw.xyzw acc, vf10, vf0
    vitof0.xyzw vf1, vf1
    vmaddaz.xyzw acc, vf0, vf10
    vmadday.xyzw acc, vf0, vf10
    vmaddx.xyzw vf10, vf0, vf10
    vmulaw.xyzw acc, vf3, vf0
    vmaddy.xyzw vf3, vf1, vf13
    qmfc2.i t9, vf10
    pcpyud t9, t9, r0
    bltzl t9, L73
B15:
    vsub.xyzw vf4, vf15, vf4

B16:
L73:
    vmulaw.xyzw acc, vf6, vf0
    vmaddx.xyzw vf6, vf4, vf13
    sqc2 vf3, 0(a0)
    sqc2 vf6, 16(a0)
    beq r0, r0, L71
    sll r0, r0, 0

B17:
    ld t9, 8(t4)
    daddiu t4, t4, 8
    lqc2 vf6, 16(a0)
    pextlh t9, t9, r0
    psraw t9, t9, 16
    qmtc2.i vf4, t9
    ld s6, -8(t4)
    daddiu t4, t4, 8
    lw gp, 0(t5)
    daddiu t5, t5, 4
    lqc2 vf3, 0(a0)
    vitof15.xyzw vf4, vf4
    pcpyld s6, gp, s6
    vmul.xyzw vf10, vf4, vf6
    qmtc2.i vf1, s6
    vmulaw.xyzw acc, vf10, vf0
    vmaddaz.xyzw acc, vf0, vf10
    vmadday.xyzw acc, vf0, vf10
    vmaddx.xyzw vf10, vf0, vf10
    vmulaw.xyzw acc, vf3, vf0
    vmaddx.xyzw vf3, vf1, vf13
    qmfc2.i t9, vf10
    pcpyud t9, t9, r0
    bltzl t9, L74
B18:
    vsub.xyzw vf4, vf15, vf4

B19:
L74:
    vmulaw.xyzw acc, vf6, vf0
    vmaddx.xyzw vf6, vf4, vf13
    sqc2 vf3, 0(a0)
    sqc2 vf6, 16(a0)
    beq r0, r0, L71
    sll r0, r0, 0

B20:
    lw t8, 0(t5)
    daddiu t5, t5, 4
    lh fp, 0(t6)
    daddiu t6, t6, 2
    lqc2 vf9, 32(a0)
    pextlw t8, fp, t8
    pextlh t8, t8, r0
    psraw t8, t8, 16
    qmtc2.i vf7, t8
    vitof12.xyzw vf7, vf7
    vmulaw.xyzw acc, vf9, vf0
    vmaddx.xyzw vf9, vf7, vf13
    sqc2 vf9, 32(a0)
    beq r0, r0, L71
    sll r0, r0, 0

B21:
    lw s6, 0(t5)
    daddiu t5, t5, 4
    lh gp, 0(t6)
    daddiu t6, t6, 2
    lqc2 vf3, 0(a0)
    pextlw s6, gp, s6
    pextlh s6, s6, r0
    psraw s6, s6, 16
    qmtc2.i vf1, s6
    lw t8, 0(t5)
    daddiu t5, t5, 4
    lh fp, 0(t6)
    daddiu t6, t6, 2
    lqc2 vf9, 32(a0)
    vitof0.xyzw vf1, vf1
    pextlw t8, fp, t8
    pextlh t8, t8, r0
    psraw t8, t8, 16
    qmtc2.i vf7, t8
    vmulaw.xyzw acc, vf3, vf0
    vmaddy.xyzw vf3, vf1, vf13
    vitof12.xyzw vf7, vf7
    vmulaw.xyzw acc, vf9, vf0
    vmaddx.xyzw vf9, vf7, vf13
    sqc2 vf3, 0(a0)
    sqc2 vf9, 32(a0)
    beq r0, r0, L71
    sll r0, r0, 0

B22:
    ld s6, 0(t4)
    daddiu t4, t4, 8
    lw gp, 0(t5)
    daddiu t5, t5, 4
    lqc2 vf3, 0(a0)
    pcpyld s6, gp, s6
    qmtc2.i vf1, s6
    lw t8, 0(t5)
    daddiu t5, t5, 4
    lh fp, 0(t6)
    daddiu t6, t6, 2
    lqc2 vf9, 32(a0)
    pextlw t8, fp, t8
    pextlh t8, t8, r0
    psraw t8, t8, 16
    qmtc2.i vf7, t8
    vmulaw.xyzw acc, vf3, vf0
    vmaddx.xyzw vf3, vf1, vf13
    vitof12.xyzw vf7, vf7
    vmulaw.xyzw acc, vf9, vf0
    vmaddx.xyzw vf9, vf7, vf13
    sqc2 vf3, 0(a0)
    sqc2 vf9, 32(a0)
    beq r0, r0, L71
    sll r0, r0, 0

B23:
    ld t9, 0(t4)
    daddiu t4, t4, 8
    lqc2 vf6, 16(a0)
    pextlh t9, t9, r0
    psraw t9, t9, 16
    qmtc2.i vf4, t9
    lw t8, 0(t5)
    daddiu t5, t5, 4
    lh fp, 0(t6)
    daddiu t6, t6, 2
    lqc2 vf9, 32(a0)
    vitof15.xyzw vf4, vf4
    pextlw t8, fp, t8
    pextlh t8, t8, r0
    psraw t8, t8, 16
    vmul.xyzw vf10, vf4, vf6
    qmtc2.i vf7, t8
    vmulaw.xyzw acc, vf10, vf0
    vitof12.xyzw vf7, vf7
    vmaddaz.xyzw acc, vf0, vf10
    vmadday.xyzw acc, vf0, vf10
    vmaddx.xyzw vf10, vf0, vf10
    vmulaw.xyzw acc, vf9, vf0
    vmaddx.xyzw vf9, vf7, vf13
    qmfc2.i t9, vf10
    pcpyud t9, t9, r0
    bltzl t9, L75
B24:
    vsub.xyzw vf4, vf15, vf4

B25:
L75:
    vmulaw.xyzw acc, vf6, vf0
    vmaddx.xyzw vf6, vf4, vf13
    sqc2 vf6, 16(a0)
    sqc2 vf9, 32(a0)
    beq r0, r0, L71
    sll r0, r0, 0

B26:
    ld t9, 0(t4)
    daddiu t4, t4, 8
    lqc2 vf6, 16(a0)
    pextlh t9, t9, r0
    psraw t9, t9, 16
    qmtc2.i vf4, t9
    lw s6, 0(t5)
    daddiu t5, t5, 4
    lh gp, 0(t6)
    daddiu t6, t6, 2
    lqc2 vf3, 0(a0)
    vitof15.xyzw vf4, vf4
    pextlw s6, gp, s6
    pextlh s6, s6, r0
    psraw s6, s6, 16
    vmul.xyzw vf10, vf4, vf6
    qmtc2.i vf1, s6
    lw t8, 0(t5)
    daddiu t5, t5, 4
    lh fp, 0(t6)
    daddiu t6, t6, 2
    vmulaw.xyzw acc, vf10, vf0
    vmaddaz.xyzw acc, vf0, vf10
    vmadday.xyzw acc, vf0, vf10
    vmaddx.xyzw vf10, vf0, vf10
    vitof0.xyzw vf1, vf1
    lqc2 vf9, 32(a0)
    pextlw t8, fp, t8
    qmfc2.i t9, vf10
    pextlh t8, t8, r0
    psraw t8, t8, 16
    qmtc2.i vf7, t8
    pcpyud t9, t9, r0
    bltzl t9, L76
B27:
    vsub.xyzw vf4, vf15, vf4

B28:
L76:
    vitof12.xyzw vf7, vf7
    vmulaw.xyzw acc, vf6, vf0
    vmaddx.xyzw vf6, vf4, vf13
    vmulaw.xyzw acc, vf3, vf0
    vmaddy.xyzw vf3, vf1, vf13
    vmulaw.xyzw acc, vf9, vf0
    vmaddx.xyzw vf9, vf7, vf13
    sqc2 vf3, 0(a0)
    sqc2 vf6, 16(a0)
    sqc2 vf9, 32(a0)
    beq r0, r0, L71
    sll r0, r0, 0

B29:
    ld t9, 8(t4)
    daddiu t4, t4, 8
    lqc2 vf6, 16(a0)
    pextlh t9, t9, r0
    psraw t9, t9, 16
    qmtc2.i vf4, t9
    ld s6, -8(t4)
    daddiu t4, t4, 8
    lw gp, 0(t5)
    daddiu t5, t5, 4
    lqc2 vf3, 0(a0)
    vitof15.xyzw vf4, vf4
    pcpyld s6, gp, s6
    vmul.xyzw vf10, vf4, vf6
    qmtc2.i vf1, s6
    lw t8, 0(t5)
    daddiu t5, t5, 4
    lh fp, 0(t6)
    daddiu t6, t6, 2
    vmulaw.xyzw acc, vf10, vf0
    vmaddaz.xyzw acc, vf0, vf10
    vmadday.xyzw acc, vf0, vf10
    vmaddx.xyzw vf10, vf0, vf10
    lqc2 vf9, 32(a0)
    pextlw t8, fp, t8
    qmfc2.i t9, vf10
    pextlh t8, t8, r0
    psraw t8, t8, 16
    qmtc2.i vf7, t8
    pcpyud t9, t9, r0
    bltzl t9, L77
B30:
    vsub.xyzw vf4, vf15, vf4

B31:
L77:
    vitof12.xyzw vf7, vf7
    vmulaw.xyzw acc, vf6, vf0
    vmaddx.xyzw vf6, vf4, vf13
    vmulaw.xyzw acc, vf3, vf0
    vmaddx.xyzw vf3, vf1, vf13
    vmulaw.xyzw acc, vf9, vf0
    vmaddx.xyzw vf9, vf7, vf13
    sqc2 vf3, 0(a0)
    sqc2 vf6, 16(a0)
    sqc2 vf9, 32(a0)
    beq r0, r0, L71
    sll r0, r0, 0

B32:
L78:
    lq a0, 0(sp)
    sll r0, r0, 0
    jr ra
    daddiu sp, sp, 16

    jr ra
    daddu sp, sp, r0
```