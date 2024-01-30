# Generic
What do we expect to fix by adding generic?

- "warp" effect (distortion effect that samples the framebuffer)
- "death" effect (get transformed vertices in world for spawning death effect particles)
- "ripple query" feature (get transformed vertices in world, kinda like depth)
- "hud drawing" for the orb/metal head gem

# Part 1: redirecting stuff to generic
Right now, we send everything to PC merc all the time, completely ignoring the original decision. I added code to redirect stuff back to generic when any of the above features are used. I noticed that warp stuff often contained two merc "effects" (really just chunks of frags drawn with the same renderer). One has `warp`, and the other has `force-mercneric`. So I redirected both of these to mercneric for now, though it's possible that the `force-mercneric` one can work with PC merc. (doesn't seem like a big deal either way...).

This seemed to work - lots of broken merc stuff is correclty redirected.

I also checked for the same sort of issue that we had with jak 1's "transparent sculptor visor". They have a trick to draw transparent stuff in a texture bucket that normally doesn't have transparency - they set `force-mercneric` and set bit `0b10` in the effect bits, which changes the blending mode for this effect. This same thing is possible in jak 2, and I added support for it as well. I couldn't find any cases where this actually mattered, but it doesn't hurt.

# Part 2: Generic Buffer Setup
Like with many renderers, the bucket setup is only done if the renderer is actually used.
This path is:
- `swap-display` is called in `main.gc`
- this calls `display-frame-finish`
- this calls `generic-vu1-init-buffers`
- this calls a function to initialize each generic bucket. For warp, it does
```
;; s5-0 is the usual gs-zbuf with depth writes enabled
(generic-vu1-init-buf-special (bucket-id gmerc-warp) s5-0)
```

```
(defun generic-vu1-init-buf-special ((arg0 bucket-id) (arg1 gs-zbuf))
  "At the beginning of this bucket's chain, insert DMA data for fx-copy-buf and initializing generic."
  (let ((s5-0 (-> *display* frames (-> *display* on-screen) bucket-group arg0)))
    (when (!= s5-0 (-> s5-0 last))
      (let* ((s4-0 (-> *display* frames (-> *display* on-screen) global-buf))
             (s3-1 (-> s4-0 base))
             )
        ;; generate DMA data for copying the framebuffer
        (fx-copy-buf s4-0)
        ;; generate DMA data for initializing generic VU1.
        (generic-init-buf s4-0 arg1)

        (let ((v1-12 (the-as dma-packet (-> s4-0 base))))
          (set! (-> v1-12 dma) (new 'static 'dma-tag :id (dma-tag-id next) :addr (-> s5-0 next)))
          (set! (-> v1-12 vif0) (new 'static 'vif-tag))
          (set! (-> v1-12 vif1) (new 'static 'vif-tag))
          (set! (-> s4-0 base) (the-as pointer (&+ v1-12 16)))
          )
        (set! (-> s5-0 next) (the-as uint s3-1))
        )
      )
    )
  0
  (none)
  )
```

The import part is
```
;; generate DMA data for copying the framebuffer
(fx-copy-buf s4-0)
;; generate DMA data for initializing generic VU1.
(generic-init-buf s4-0 arg1)
```
The `fx-copy-buf` function needs to be decompiled. There's nothing too interesting, other than it copies the screen in chunks, rather than one giant sprite. The offsets and stuff are a bit confusing, but we might be able to guess our way through it.

For now, I'm going to leave out this function, and we'll just remember to do the copy on the C++ at the start of this bucket.

# Part 3: Submitting merc models to generic
In part 1, we set things up so the main `foreground-draw` functions sends models to `foreground-generic-merc`, which isn't decompiled yet. If this works like Jak 1, this doesn't actually do the EE side of generic yet - it just adds them to a list.

The next step was to mips2c/decompile `foreground-generic-merc` and the functions it calls. Unfortunately, the main function can't easily be decompiled. I suspect it's very similar to `draw-bones-generic-merc` where it fills out generic parameters based on settings. The death vertex and fragment adding loop are split into separate functions that we can decompile this time:

```
(defun foreground-generic-merc-death ((arg0 draw-control) (arg1 generic-merc-ctrl))
  "Modify a generic-merc-ctrl to set up merc-death effect."

  ;; possibly disable drawing if requested
  (when (and (>= (the-as int (- (-> arg0 death-timer-org) (-> arg0 death-timer)))
                 (the-as int (-> arg0 death-draw-overlap))
                 )
             (!= (-> arg0 death-draw-overlap) 255)
             )
    (set! (-> arg1 header display-triangles) (the-as uint 0))
    0
    )

  ;; update the vertices that we're querying.
  (when (not (paused?))
    (let ((v1-6 (+ (-> arg0 death-vertex-skip) (rand-vu-int-count (the-as int (-> arg0 death-vertex-skip))))))
      (set! (-> arg1 header death-vertex-skip) v1-6)
      (set! (-> arg1 header death-effect) (-> arg0 death-effect))
      (set! (-> arg1 header death-start-vertex)
            (/ (* v1-6 (- (-> arg0 death-timer-org) (-> arg0 death-timer))) (-> arg0 death-timer-org))
            )
      )
    )
  (none)
  )

(defun foreground-generic-merc-add-fragments ((arg0 merc-effect) (arg1 pointer) (arg2 mercneric-chain))
  "Add fragments from a merc-effect to the generic chain."
  (let ((v1-0 (-> arg0 frag-geo))
        (a3-0 (the-as structure (-> arg0 frag-ctrl)))
        (a0-1 (-> arg0 frag-count))
        )

    ;; loop over fragments, adding matrix refs.
    (dotimes (t0-0 (the-as int a0-1))
      (let ((t1-2 (+ (* (-> (the-as merc-fragment-control a3-0) mat-xfer-count) 2) 4))
            (t2-0 (-> v1-0 header mm-quadword-size))
            )
        (set! (-> (the-as dma-packet arg1) dma)
              (new 'static 'dma-tag :id (dma-tag-id ref) :addr (the-as int v1-0) :qwc t2-0)
              )
        (set! (-> (the-as dma-packet arg1) vif0) (new 'static 'vif-tag))
        (set! (-> (the-as dma-packet arg1) vif1) (new 'static 'vif-tag))
        (when (nonzero? t0-0)
          (set! (-> (the-as (pointer int32) (-> arg2 next)) 0) (the-as int arg1))
          (set! (-> arg2 next) (the-as uint (&+ arg1 12)))
          )
        (let ((a1-1 (the-as object (&+ arg1 16))))
          (dotimes (t3-6 (the-as int (-> (the-as merc-fragment-control a3-0) mat-xfer-count)))
            (let ((t5-4
                    (-> (scratchpad-object foreground-work)
                        regs
                        mtxs
                        (-> (the-as merc-fragment-control a3-0) mat-dest-data t3-6 matrix-number)
                        )
                    )
                  )
              (set! (-> (the-as dma-packet a1-1) dma)
                    (new 'static 'dma-tag :qwc #x7 :id (dma-tag-id ref) :addr (the-as int t5-4))
                    )
              )
            (set! (-> (the-as dma-packet a1-1) vif0) (new 'static 'vif-tag))
            (set! (-> (the-as dma-packet a1-1) vif1) (new 'static 'vif-tag))
            (set! a1-1 (&+ (the-as pointer a1-1) 16))
            )
          (set! (-> (the-as dma-packet a1-1) dma) (new 'static 'dma-tag :id (dma-tag-id end)))
          (set! (-> (the-as dma-packet a1-1) vif0) (new 'static 'vif-tag))
          (set! (-> (the-as dma-packet a1-1) vif1) (new 'static 'vif-tag))
          (set! arg1 (&+ (the-as pointer a1-1) 16))
          )
        (set! a3-0 (&+ a3-0 t1-2))
        (set! v1-0 (the-as merc-fragment (+ (the-as uint v1-0) (* t2-0 16))))
        )
      )
    )
  arg1
  )
```
It's overall very similar, but the way of finding the matrices is little different, but it all makes sense. The generic chain will be processed later, and it will dma these matrices to the scratchpad for transforming generic merc data.

# Part 4: Running Generic Merc EE processing
Part 3 just adds fragments to a list. We actually have to process these fragments on the EE before drawing them with VU1. Assuming it's like Jak 1, the EE step transforms vertices to world frame and computes stuff like envmap coordinates. The VU1 part does the perspective tranformation and clipping.

This is run from `generic-merc-execute-all`, called from `drawable.gc`.

```
(defun generic-merc-execute-all ((arg0 dma-buffer))
  "Run EE processing on all generic merc chains."
  (let ((s4-0 (-> *foreground* foreground-grid))
        (gp-0 (-> *display* frames (-> *display* on-screen) global-buf base))
        )

    (with-profiler 'generic-merc *profile-generic-merc-color*
      ;; reset profiling
      (reset! (-> *perf-stats* data (perf-stat-bucket mercneric)))
      (set! (-> (scratchpad-object generic-work) saves to-vu0-waits) (the-as uint 0))
      (set! (-> (scratchpad-object generic-work) saves to-spr-waits) (the-as uint 0))
      (set! (-> (scratchpad-object generic-work) saves from-spr-waits) (the-as uint 0))
      (flush-cache 0)

      ;; init generic
      (generic-initialize-without-sync (-> *math-camera* perspective) *default-lights*)
      (generic-merc-init-asm)
      (set! (-> (scratchpad-object generic-work) in-buf merc shadow write-limit) (the-as int (&+ (-> arg0 end) -262144)))

      ;; loop over grid of chains (levels x textures) and do the chain.
      (dotimes (s3-1 7)
        (dotimes (s2-1 7)
          (generic-merc-do-chain (-> s4-0 level-buckets s3-1 data s2-1 mercneric) arg0)
          )
        )

      ;; do separate warp chain
      (generic-merc-do-chain (-> s4-0 warp-chain) arg0)

      ;; finish profiling
      (read! (-> *perf-stats* data (perf-stat-bucket mercneric)))
      (update-wait-stats
        (-> *perf-stats* data 38)
        (-> (scratchpad-object generic-work) saves to-vu0-waits)
        (-> (scratchpad-object generic-work) saves to-spr-waits)
        (-> (scratchpad-object generic-work) saves from-spr-waits)
        )
      )

    ;; update dma memory usage stats.
    (let ((v1-66 *dma-mem-usage*))
      (when (nonzero? v1-66)
        (set! (-> v1-66 length) (max 90 (-> v1-66 length)))
        (set! (-> v1-66 data 89 name) "pris-generic")
        (+! (-> v1-66 data 89 count) 1)
        (+! (-> v1-66 data 89 used)
            (&- (-> *display* frames (-> *display* on-screen) global-buf base) (the-as uint gp-0))
            )
        (set! (-> v1-66 data 89 total) (-> v1-66 data 89 used))
        )
      )
    )
  (none)
  )
```

This function doesn't look too bad...



# Part 5: Decompiling `generic-merc` stuff

- `generic-merc-init-asm`
Nothing interesting, dma sync patched out.

- `mercneric-convert`
The worst function. Copied all the vu0 stuff from jak 1.

- `high-speed-reject`
Copied vu0 stuff from jak1

- `generic-translucent`
New for jak 2, easy.

- `generic-merc-query`
New for jak 2, easy. (maybe no mips2c?)

- `generic-merc-death`
New for jak 2, easy. (maybe no mips2c?)

- `generic-merc-execute-asm`
Did the same direct-call patch for the `mercneric-convert` call. Not sure why I did this originally, but it can only help.

- `generic-merc-do-chain`
New for jak 2, easy (patched out dma sync).




# Part 6: Decompiling `generic-effect` stuff
- `generic-work-init`
Not mips2c. Just copies stuff to generic-work.

- `generic-light-proc`

- `generic-warp-source`
- `generic-warp-envmap-dest`
- `generic-warp-dest`
- `generic-prepare-dma-single` (`generic-effect`)
- `generic-prepare-dma-double` (`generic-effect`)

- `generic-envmap-proc`

