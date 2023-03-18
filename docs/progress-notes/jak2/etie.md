# ETIE
environment mapped tie.

This document is a set of notes I took while reversing the renderer and porting to C++.

My hope is that it can be used as a bit of a guide for OpenGOAL renderer implementation.

# Part 0: From Jak 1 Tie
Before starting, I looked over Jak 1's TIE stuff. We know ETIE is at least kinda similar because some stuff sorta renders if we pretend it's normal TIE.

## Terminology
- Level: this is an in-game level. Precomputed visibility is computed per level. In code, `level` generally refers to the relatively small metadata describing the current state (which level is loaded, info about the level). The actual _data_ is stored in the "BSP file". This name is a bit imprecise because the BSP is only used for precomupted visibility - stuff like level geometry/collision isn't stored in an actual BSP data structure. But, all this data is accessible through a type called `bsp-header`. This type is at the beginning of every level file, and provides pointers to all the data stored within the level. For TIE, this doesn't really matter. I'll probably use level/BSP somewhat interchangably. The actual TIE data is stored in the BSP.

- Tree: Each level has some small number of TIE "tree"s. Trees contain "Prototypes", which are models (eg: a tree, a rock); "Instances", which are placements of a model (eg: place Rock_A at location X), and a BVH tree.  The BVH tree stores the position of each instance and is used to speed up the "is this instance in the view frustum" check. The details don't really matter, but they use a ball tree with a branching factor of at most 8. Usually the number of trees is small (Sandover has 3). The reason to split into multiple trees is unknown. The limit may due to a maximum tree depth.

- Bucket: The rendering is done partially on the EE and partially on the VU. Like all drawing, this is double buffered -- on frame 1, the engine produces DMA data that is processed by VIF1/VU1 on frame 2. This DMA data is in a linked list, divided into "buckets". The purpose of buckets is to allow them to reorder DMA data. After buckets are complete, they are stitched together in the order specified in the `bucket-it` enum.

Here's an example of how the reordering might be useful. The renderer processes one tree at a time, possibly in this order
- data from Level 1's Tie Tree
- data from Level 2's Tie Tree

But, the correct draw order should be:
- background stuff from Level 1
- background stuff from Level 2
- Transparent stuff from level 1
- Transparent stuff from level 2

The solution is to have a separate background and transparent bucket. When processing each level, the renderer will put stuff in both buckets. Then, once everything is rendered, the buckets are stitched in the appropriate order. This stitching process is fast because it's just a linked list. This bucketing trick is used to insert texture uploads at the right time as well. Jak 2 has a total of 326 buckets.

Here's an example of what might be contained in the DMA data within a bucket (simplified):

- Instructions to upload a program to VU1 instruction memory
- Instruction to upload some per-prototype data to VU1 (vertices)
- Instructions to upload per-instance data to VU1
- Instruction to run a VU1 microprogram
- Instruction to upload another per-instance data to VU1
- Instruction to run a VU1 microprogram
- ....

These VU1 programs will build a list of stuff to draw in the format of the GS, then use the XGKICK instruction to tell the GS to start rendering the list.

From what I remember, the per-prototype data is the vertices (their position, texture coordinates). The per-instance data is the location of the instance, and the instance colors (each instance has a 256?? color palette).

Another detail is that uploading data to the VU1 is processed by VIF1. This can be configured to do relatively simple "unpack" operations.

And of course, because this is ND, _everything_ is double buffered. At any point in time:
- There will be an upload from DMA to VU1 data memory
- VU1 will be reading previously uploaded data, transforming vertices, and writing a list of GS data
- GS will be reading a previously generated list from VU1 memory, drawing pixels to the framebuffer

(and yes, they double buffer both the per-prototype and per-instance data)

## Visibility Info
There are two visibility systems: precomputed occlusion culling data, and frustum culling.

Precomputing visibility is stored based on the camera position. This works differently in jak 2 and jak 1, but the basic idea is that a visibility string can be looked up based on camera position. This contains a bit for each drawable-id (up to 2^14 per level). It tells you "if the camera is in this area, here are all the things you could possible see".

The frustum culling just checks if something is within the field of view. It's computed on each frame.

Note that the actual implementation `draw-node-cull` does some cool tricks to efficient combine the precomputed data with the tree-traversal to produce the final visibility string. The final data is just a string.

## Storage format in tree data

There's a flat array of "instance"s. Each instance stores its position/orientation (as a compressed matrix) some parameters, and has a pointer back to "prototype". (Note that due to some clever arrangement, this flat array is also part of the BVH tree)

Each prototype has a few different versions depending on the LOD. These versions


# Part 1: Reading code

This is always the first step. The system of levels/tree/instances/prototypes and all the different lists that are built are super confusing, but we can't write any code until we understand all the details. We need to answer questions like "are the trees that output to multiple buckets"?


TIE drawing procedure. All TIEs are drawn from the display process

Display process loop:
```
    (while *run*
      (display-loop-main gp-1)
      (with-profiler 'actors *profile-actors-color*
        (suspend)
        )
      )
```
In `display-loop-main`:
```
(with-profiler 'draw-hook *profile-draw-hook-color*
  (*draw-hook*)
  )
```
In `drawable.gc`:
```
(defun main-draw-hook ()
  (real-main-draw-hook)
  (none)
  )

(define *draw-hook* main-draw-hook)
```
in `real-main-draw-hook`:
```
    (with-profiler 'background *profile-background-color*
      ;; Run the background renderers!

      ;; first, reset the background-work
      (init-background)

      ;; next, collect all levels that are registered with the engine
      ;; this will call the drawable system's draw method on the levels which adds all
      ;; trees known to the background system to *background-work*.
      (execute-connections *background-draw-engine* (-> *display* frames (-> *display* on-screen)))

      ;; execute all background drawing
      (reset! (-> *perf-stats* data (perf-stat-bucket background)))
      (finish-background)
      (read! (-> *perf-stats* data (perf-stat-bucket background)))

      ;; update VU stats for background draw.
      (update-wait-stats (-> *perf-stats* data (perf-stat-bucket background))
                         (-> *background-work* wait-to-vu0)
                         (the-as uint 0)
                         (the-as uint 0)
                         )
      )
```
in `finish-background`:
```
    (when (nonzero? (-> *background-work* tie-tree-count))
      (set! (-> *instance-tie-work* tod-env-color quad) (-> *time-of-day-context* current-env-color quad))
      (with-profiler 'instance-tie *profile-instance-tie-color*
        ;; loop over trees from all level
        (dotimes (s5-10 (-> *background-work* tie-tree-count))
          (let ((s4-8 (-> *background-work* tie-levels s5-10)))
            (let ((a2-29 (-> s4-8 bsp)))
              (when (!= s4-8 gp-6)
                (set! (-> *instance-tie-work* min-dist x) 4095996000.0)
                (upload-vis-bits s4-8 gp-6 a2-29)
                (set! gp-6 s4-8)
                )
              )
            (set! *draw-index* (-> s4-8 draw-index))
            (set! (-> *prototype-tie-work* mood) (-> s4-8 mood-context))
            (set-background-regs! s4-8)
            (set-tie-quard-planes! s4-8)
            (tie-scissor-make-perspective-matrix
              (-> *instance-tie-work* tie-scissor-perspective-matrix)
              (if (-> s4-8 info use-camera-other)
                  (-> *math-camera* camera-temp-other)
                  (-> *math-camera* camera-temp)
                  )
              )
            (draw-drawable-tree-instance-tie (-> *background-work* tie-trees s5-10) s4-8)
            )
          (set! (-> *background-work* tie-generic s5-10) (the-as basic (-> *prototype-tie-work* generic-next)))
          (set! (-> *background-work* tie-generic-trans s5-10)
                (the-as basic (-> *prototype-tie-work* generic-trans-next))
                )
          )
        )
      )
```

The `draw-drawable-tree-instance-tie` function does dma generation for an entire "tree". The "tree" refers to the visibility structure. At the top of `draw-drawable-tree-instance-tie`, `draw-node-cull` is used to determine visibility for all instances in the tree, and is stashed in the scratchpad. After this, there are 3 main functions:
- `draw-inline-array-instance-tie`
- `draw-inline-array-prototype-tie-asm`
- `instance-tie-patch-buckets`
and there's a much later call to `tie-vu1-init-buffers` in `display-frame-finish`.

The `draw-inline-array-instance-tie` function builds lists per-prototype of instances. It is responsible for generating per-instance stuff, like computing distances and picking the right lod settings, and setting up DMA for per-instance colors.

The `draw-inline-array-prototype-tie-asm` function then patches the per-proto lists into a single large list, inserting DMA stuff for uploading the proto. (actually, there's a list per renderer mode)

The `instance-tie-patch-buckets` function links the per-renderer lists to bucket lists, optionally skipping modes disabled from the debug menu.

The `tie-vu1-init-buffers` runs much later. It checks to see if TIE buckets are empty, and if not, adds the DMA to upload the VU1 program. There are a lot of tie buckets (13 modes x 6 levels), so it seems like a good idea to skip the program upload for unused ones.

## Buckets/modes/levels/trees...

There is a bucket per level and per mode. All tie bucket start with `tie`. For example `tie-esw-l2-water` is for mode "environment mapped, scissor, water" and level 2. The "water" part means a few things:
- implies something about draw order compared to other buckets (for transparency)
- possibly something about draw settings. (eg: water has alpha blending on, others don't)
- textures that are available (water tpage of level 2)

Each level can have multiple trees. The reason for multiple trees isn't super clear, but it could be limits of the visibility system. (seems to actually be number of protos?) Unlike tfrag, a single tie tree can output to different modes.

The `draw-drawable-tree-instance-tie` function runs per-tree. It can output to any tie bucket for the level (likely multiple). In theory, the `draw-index` stuff could be used to make it to output to another level's buckets, though I suspect this isn't used.

The `tie-vu1-init-buffers` runs once per frame. So it can see if any of the `draw-drawable-tree-instance-tie`'s wrote to each bucket.


## What we need to find out:
- what is the math for envmap?
- how the envmap shader is set?
- how envmap settings are set (fade, tint)
- how normals are computed?
- what are the different TIE modes? (envmap, others?)

### Math:
We know that:
- Jak 2's generic-merc can render emerc data identically (happens in cutscenes when character is near the border)
- Jak 2's emerc works the same as Jak 1's generic-merc
- Jak 1's generic merc uses the same code as Jak 1's generic tie for envmapping (pretty sure...).

Unfortunately, we don't know if Jak 2's etie differs from these renderers. Jak 2 never uses generic-tie. It would make sense for etie to be the same as emerc/generic-merc, so you can place merc/tie stuff next to each other and it looks the same. But who knows - they could have done something like negate the texture coordinates because it was easier to write the VU program this way, and they don't have to worry about compatibility between generic-tie/etie.

So it would be ideal to get the envmap math out of the ETIE vu program. Or just guess it's the same as emerc and check carefully with PCSX2.

### Shader:
There's an "adgif-shader" for the envmap. This is the texture for the reflection. I'd guess there's a way for models to specify their own adgif-shader, or fall back to the default envmap.  This will need to be added to `extract_tie.cpp`.

### Settings:
Envmap has relatively few settings: fade/tint. The fade is used to fade out the effect as you move away. The tint is used to apply time of day lighting to envmap. Unlike the vertex colors, there's a single rgba for the entire envmap draw. Need to find this and pass it to the C++ rendering code. It changes on each frame with the time of day.

### Normals:
Tie normally doesn't need normals. However, computing reflection directions needs normals.

I'm not sure how these normals are generated. They could compute per-triangle face normals and use that, or they could store normals.

In `generic-tie`, there's a `generic-tie-normal`, using 8-bit integers. It's not clear if these are included in the model data, or if they are generated. The use of only 8-bits makes me think that they are stored, though it might also be packed so they can be easily snuck through the generic pipeline.

### Modes:
Somehow, the drawing code builds separate lists for different bucket types. The types are:
- `tie`: buckets like `tie-lX-tfrag`
- `trans`: buckets like `tie-t-lX-alpha`
- `water`: buckets like `tie-w-lX-water`
- `scissor`: buckets like `tie-s-lX-tfrag`
- `scissor-trans`: buckets like `tie-st-lX-alpha`
- `scissor-water`: buckets like `tie-sw-lX-water`
- `envmap`: buckets like `etie-lX-tfrag`
- `envmap-trans`: buckets like `etie-t-lX-alpha`
- `envmap-water`: buckets like `etie-w-lX-water`
- `envmap-scissor`: buckets like `etie-s-lX-tfrag`
- `envmap-scissor-trans`: buckets like `etie-st-lX-alpha`
- `envmap-scissor-water`: buckets like `etie-sw-lX-water`
- `generic`: (unused??)
- `vanish`: buckets like `tie-v-lX-tfrag`

Currently, stuff is set up to do a single TIE per level, using the original game's `tie` bucket. However, this will need to be split up to render the appropriate bucket data at the appropriate time.

Bucket order:
```
  (tex-l0-tfrag      7)   ;; level 0 tex
  (tfrag-l0-tfrag    8)   ;; tfrag
  (tie-l0-tfrag      9)   ;; tie
  (etie-l0-tfrag    10)   ;; tie
  (tfrag-s-l0-tfrag 11)   ;; tfrag
  (tie-s-l0-tfrag   12)   ;; tie
  (etie-s-l0-tfrag  13)   ;; tie
  (merc-l0-tfrag    14)   ;; merc
  (emerc-l0-tfrag   15)   ;; emerc
  (gmerc-l0-tfrag   16)   ;; mercneric
  (tie-v-l0-tfrag   17)   ;; tie
  ;; ...
  (tex-l0-alpha        127)  ;; tex
  (tfrag-t-l0-alpha    128)  ;; tfrag
  (tie-t-l0-alpha      129)  ;; tie
  (etie-t-l0-alpha     130)  ;; tie
  (merc-l0-alpha       131)  ;; merc
  (emerc-l0-alpha      132)  ;; emerc
  (gmerc-l0-alpha      133)  ;; mercneric
  (tfrag-st-l0-alpha   134)  ;; tfrag
  (tie-st-l0-alpha     135)  ;; tie
  (etie-st-l0-alpha    136)  ;; tie
  ;; ...
  (tex-l0-water        252)  ;; tex
  (merc-l0-water       253)  ;; merc
  (gmerc-l0-water      254)  ;; mercneric
  (tfrag-w-l0-water    255)  ;; tfrag
  (tie-w-l0-water      256)
  (etie-w-l0-water     257)
  (tie-sw-l0-water     258)
  (tfrag-ws-l0-water   259)  ;; tfrag
  (etie-sw-l0-water    260)
```
this looks like it's safe to have three groups per level on PC.

- `tfrag` page: `tie`/`etie`. The vanish bucket is going to be ignored.
- `alpha` page: `tie-t`/`etie-t`
- `water` page: `tie-w`, `etie-w`

Note that is is always safe to move things in between the scissor/normal versions of a bucket because this happens all the time at runtime and it's not noticable.

I have no idea why they sometimes put the merc/gmerc in between. But it must be okay to swap these around because stuff jumps between normal and scissor all the time.


# The Envmap Shader
I can't find any envmap shader other than the `*default-envmap-shader*`.

I looked at the textures for "mountain", and I do see an envmap texture, but it's in the `pris` folder, and tie never seems to log it in. (so I think it's for merc)

All adgif shaders need to be "logged in" to be linked to a texture, and I can't find log-in code for envmap shaders, so I think this means that etie always envmaps with the default texture?

(edit from after: this was wrong.)

# ETIE stuff

Setup is pretty reasonable, the "constants" block is:
```
(deftype etie-consts (structure)
  ((gifbufs qword        :inline :offset-assert 0)
   (adgif gs-gif-tag     :inline :offset-assert 16)
   (alpah   gs-adcmd     :inline)
   (strgif  gs-gif-tag        :inline :offset-assert 48)
   (envgif  gs-gif-tag        :inline :offset-assert 64)
   (envmap  adgif-shader :inline :offset-assert 80)
   (pers0   vector       :inline :offset-assert 160)
   (pers1   vector       :inline :offset-assert 176)
   )
  :method-count-assert 9
  :size-assert         #xc0
  :flag-assert         #x9000000c0
  )
```
These "constants" get uploaded to VU1 memory once per frame, and are shared between all rendered objects. So it's typically stuff like the the camera perspective transformation.


The `gifbufs` is the usual "low bits of floats storing a memory address" nonsense:
```
  (let ((v1-3 510))
    (set! (-> arg0 gifbufs vector4w x) (gpr->fpr (+ #x4b000000 v1-3)))
    )
  (let ((v1-5 249))
    (set! (-> arg0 gifbufs vector4w y) (gpr->fpr (+ #x4b000000 v1-5)))
    )
  (let ((v1-7 510))
    (set! (-> arg0 gifbufs vector4w z) (gpr->fpr (+ #x4b000000 v1-7)))
    )
  (let ((v1-9 249))
    (set! (-> arg0 gifbufs vector4w w) (gpr->fpr (+ #x4b000000 v1-9)))
    )
```
The name `gifbuf` makes me think they store the memory address of the VU1 program's output GIF buffer. (GIF = the data sent to the GS). This is double buffered, and they need some way to quickly swap between two different values. The VU1 instruction set doesn't support many integer operations, so doing dumb tricks with the float registers can be worth it. These magic float contstants:
- Can be added together, and their lower bits will add, as if they were integers. (assuming you stay within the range of the VU1 memory address)
- Can be "toggled" with a single `mr32` instruction (`[a b a b] -> [b a b a]`)
- Can be converted to an integer register with a single instruction


The `alpha` mode is set by the call to `etie-init-engine`. We'll have to track this down later, but we know for now that the alpha mode might be different per-bucket. This alpha will be used to set the ALPHA register of the GS by the VU program. (the exact details of when this alpha is used is not known yet)
```
  (set! (-> arg0 alpah data) (the-as uint arg1))
  (set! (-> arg0 alpah cmds) (gs-reg64 alpha-1))
```

the giftags for drawing:
```
     (set! (-> arg0 strgif tag)
           (new 'static 'gif-tag64
             :pre #x1
             :prim (new 'static 'gs-prim :prim (gs-prim-type tri-strip) :iip #x1 :tme #x1 :fge #x1 :abe #x1)
             :nreg #x3
             )
           )
     (set! (-> arg0 envgif tag)
           (new 'static 'gif-tag64
             :pre #x1
             :prim (new 'static 'gs-prim :prim (gs-prim-type tri-strip) :iip #x1 :tme #x1 :abe #x1)
             :nreg #x3
             )
           )
     )
;; snip
  (set! (-> arg0 strgif regs)
        (new 'static 'gif-tag-regs :regs0 (gif-reg-id st) :regs1 (gif-reg-id rgbaq) :regs2 (gif-reg-id xyzf2))
        )
  (set! (-> arg0 envgif regs)
        (new 'static 'gif-tag-regs :regs0 (gif-reg-id st) :regs1 (gif-reg-id rgbaq) :regs2 (gif-reg-id xyzf2))
        )
```
Nothing surprising. The first part tells us it uses tri-strips. The `strgif` is likely the stuff under the envmap, and `envgif` is likely the shiny part.

The `regs` tells us that each vertex will have texture coordinates, colors, and position in that order.


The perspective stuff:
```
  (let* ((mc *math-camera*)
         (pmat (-> mc perspective))
         (inv-fog (/ 1.0 (-> mc pfog0)))
         (hvdf-off (-> mc hvdf-off))
         (xx (-> pmat vector 0 x))
         (yy (-> pmat vector 1 y))
         (zz (-> pmat vector 2 z))
         (zw (-> pmat vector 2 w))
         (wz (-> pmat trans z))
         )
    (let ((f4-1 (* zw inv-fog)))
      (set! (-> arg0 pers0 x) (* f4-1 (-> hvdf-off x)))
      (set! (-> arg0 pers0 y) (* f4-1 (-> hvdf-off y)))
      (set! (-> arg0 pers0 z) (+ (* f4-1 (-> hvdf-off z)) zz))
      (set! (-> arg0 pers0 w) f4-1)
      )
    (set! (-> arg0 pers1 x) xx)
    (set! (-> arg0 pers1 y) yy)
    (set! (-> arg0 pers1 z) wz)
    )
  (set! (-> arg0 pers1 w) 0.0)
```

Interesting notes:
- they don't have anything that includes the position of the camera. This means that the per-instance data contains a `cam_T_inst` transformation, not a `world_T_inst` one. (details don't matter, but the VU1 program can be a bit more optimized if the transform is given as separate affine + perspective)
- they have some weird thing that combines hvdf/perspective. On first glance, it doens't seem the same as EMERC. This is kinda confusing because I'd guess that emerc/etie would work the same way. I am a little suspicious that they end up being the same in the end.

The envmap stuff:
```
  (let* ((v1-44 *default-envmap-shader*)
         (a0-2 (-> arg0 envmap))
         (a1-8 (-> v1-44 quad 0 quad))
         (a2-5 (-> v1-44 quad 1 quad))
         (a3-0 (-> v1-44 quad 2 quad))
         (t0-0 (-> v1-44 quad 3 quad))
         (v1-45 (-> v1-44 quad 4 quad))
         )
    (set! (-> a0-2 quad 0 quad) a1-8)
    (set! (-> a0-2 quad 1 quad) a2-5)
    (set! (-> a0-2 quad 2 quad) a3-0)
    (set! (-> a0-2 quad 3 quad) t0-0)
    (set! (-> a0-2 quad 4 quad) v1-45)
    )
```
they're just copying an "adgif shader". An "adgif shader" is simply a chunk of data that tells the GS texturing/blending modes. The "ad" stands for "address+data", which is a mode for controlling the GS that consists of sending packets with an address and some data. So the exact format of ADGIF shaders can very slightly.

Some more info about adgif shaders:
- they are 5 QW's long. Typically the VU program will insert a 1 QW giftag header that tells the GS that the following 5 QW's are a+d format data.
- The `texture` system updates these so they point to the correct VRAM address for textures.
- Sometimes the VU program will replace parts of an adgif shader to override their behavior. The `gs-adcmd` in the VU constants looks suspiciously like this.
- in static level data, the adgif shaders contain the texture ID for the texture they should be linked to. When the level is loaded, there is a "log in" function that must be called to adjust the adgif to point to the appropriate texture, and to add this to the linked list of adgifs for the texture.
- there are bits in the adgif shader that aren't used by the GS. They hide a pointer in here to build the linked list of adgifs per texture
- It's not super clear why they need the per-texture lists of adgifs (other than for debugging purposes)

Here's where this adgif shader is set up
```
;; grab the texture object from an ID. The texture object has metadata about the texture
(define *generic-envmap-texture* (lookup-texture-by-id (new 'static 'texture-id :index #x2 :page #x1f)))

;; allocate the adgif shader
(define *default-envmap-shader* (new 'global 'adgif-shader))

(let ((gp-0 *default-envmap-shader*))
  (let ((a1-1 *generic-envmap-texture*))
    ;; link this adgif to the texture. The settings will be set up for the texture.
    (adgif-shader<-texture! gp-0 a1-1)
    )
  ;; manually set some settings, overwriting what was done above

  ;; turn on texture filtering
  (set! (-> gp-0 tex1) (new 'static 'gs-tex1 :mmag #x1 :mmin #x1))

  ;; turn on texture clamping
  (set! (-> gp-0 clamp) (new 'static 'gs-clamp :wms (gs-tex-wrap-mode clamp) :wmt (gs-tex-wrap-mode clamp)))

  ;; set a specific alpha blend mode
  (set! (-> gp-0 alpha) (new 'static 'gs-alpha :b #x2 :c #x1 :d #x1))

  ;; set the addresses for the register (some of these were automatically done by the adgif-shader<-texture! call)
  (set! (-> gp-0 prims 1) (gs-reg64 tex0-1))
  (set! (-> gp-0 prims 3) (gs-reg64 tex1-1))
  (set! (-> gp-0 prims 5) (gs-reg64 miptbp1-1))
  (set! (-> gp-0 clamp-reg) (gs-reg64 clamp-1))
  (set! (-> gp-0 prims 9) (gs-reg64 alpha-1))
  )
  ```

# Part 2: Implementation

## Splitting up TIE3: the idea
A tie "tree" is a bunch of ties arranged in a BVH for easier culling. Generally, a level has a small number of TIE trees (somewhere between 1 and 3?).

Previously TIE3 assumed that a single tree would be drawn all at once on PC. But now, the tree must be split up into (normal, water, trans) categories. Each category should be drawn at the appropriate time.

The tricky part about splitting this up is that culling has to be done for the whole tree at once, and we only want to compute this once - not per category.

So in the first TIE bucket for a given level, we'll:
- do the normal logic to see which trees the game is trying to draw
- do culling for the entire tree
- draw the first category (normal TIE)

Then, in the other two buckets:
- Check to see if the first bucket drew on this frame
- If so, reuse culling/selection logic, but draw the other category.

To make this change:
- each PC-format tree has a list of draws: `std::vector<Draw>`. Split this into a list per-category.
- add a `Tie3AnotherCategory` renderer.

Jak 1 doesn't do this multi-bucket-from-a-single-tree stuff, so we can just put everything in the "default" bucket and it will work the same as before.

But before that...

### How are TIEs sorted into categories?
Remember that TIE DMA generation is broken into three steps (executed per _tree_):
- iterate over instances. if visible, add them to a "per prototype" list
- iterate over protypes. if any visible, add them to a list for the appropriate category
- insert per-category lists into the appropriate DMA bucket.

The category decision doesn't happen in the third step: at this point the `*prototype-tie-work*` structure contains per-category lists (eg: `(-> *prototype-tie-work* envmap-next)` will contain all the `envmap` category for this tree).

The other two functions are asm... We need to guess where to look first, or it will take a full day to read through all this code.

I'd guess that the category decision happens in the first function. The category decision has to be different per-instance to handle stuff like "this instance needs scissoring, but this other one doesn't", and similar for envmap fadeout. They could have done this work in either the first or the second, but I think it makes a lot more sense in the first one based on what I remember from Jak 1's code. In jak 1, stuff like picking the LOD was done in the first pass, which means they'd compute the invserse distances here, which you'd also need for envmap.

Perhaps another clue is the `prototype-bucket-tie` type - this contains a per-catgory list of instances:
```
   (tie-next                    uint32             4        :offset 64)
   (tie-scissor-next            uint32                       :offset 64)
   (tie-near-next               uint32                       :offset 68)
   (tie-mid-next                uint32                       :offset 72)
   (tie-far-next                uint32                       :offset 76)
   (trans-next                  uint32             4        :offset 64)
   (trans-scissor-next          uint32             4        :offset 64)
   (trans-near-next             uint32                       :offset 68)
   (trans-mid-next              uint32                       :offset 72)
   (trans-far-next              uint32                       :offset 76)
   (water-next                  uint32             4        :offset 64)
   (water-scissor-next          uint32             4        :offset 64)
   (water-near-next             uint32                       :offset 68)
   (water-mid-next              uint32                       :offset 72)
   (water-far-next              uint32                       :offset 76)
   ;; many more...
```
Note that `near`/`mid`/`far` are LODs here. (confusing because "near" in jak 1 meant "scissor" in jak 2).

The first function is `draw-inline-array-instance-tie` and is annoyingly large (about 1400 lines of asm), so I don't want to annotate the whole thing.

Based on this, I'd guess the function does something like:
- skip over instances that don't have vis-bit set (unlike shrub, there's a precomputed vis string for the sphere culling, so this should be a simple "is it set" check)
- upload instance metadata to scratchpad (they love the scratchpad)
- compute the instance matrix, determine LOD and LOD interpolation coefficients.
- generate DMA on scratchpad
- download DMA from scratchpad to DMA buffer
- patch DMA appropritately

and of course, all of this is double buffered, and very complicated.

Arguments:
- `a0` is visibility data
- `a1` is the array of `instance-tie`s
- `a2` is the number of `instance-tie`s
- `a3` is the DMA buffer to hold the instance drawing DMA

The beginning is the same as Jak 1's, documented in `tie_ee.asm`. It basically sets us up with some visible instances in the scratchpad. (note that these "instances" are just metadata describing the TIE).

There's a tiny bit of difference before the following snippet, but I think it's just related to stopping DMA generation when out of DMA memory (in jak 1, they'd just overflow and crash everything).

This is where the real differences from Jak 1 start, and I'm guessing will contain category-related stuff:

```
t5 = the instance-tie
t4 = our index in this group

B24:
L284:
    sll r0, r0, 0
    lw t9, 12(t5)      ;; t9 = inst.bucket_ptr
    and ra, t6, t7     ;; check vis bit
    ld s5, 56(t5)      ;; s5 = inst.origin_3
    beq ra, r0, L316   ;; skip if invisible
    ld s3, 32(t5)      ;; s3 = origin_0

B25:
    sll ra, t4, 4      ;; DMA output offset
    ld gp, 40(t5)      ;; gp = origin_1
    pextlh s4, s5, r0  ;; s4 = s5 << 16 (128-bits version of origin 3)
    ld s5, 48(t5)      ;; s5 = origin_2
    psraw s4, s4, 10   ;; s4 = origin_3, signed
    lq s2, 28(t9)      ;; s2 = our bucket's dists (lod transitions)
    pextlh s3, s3, r0  ;; origin conversion stuff, see jak 1
    lq s1, 44(t9)      ;; s0 = more dist stuff
    psraw s3, s3, 16   ;; origin stuff
    qmtc2.ni vf14, s2  ;; origin
    pextlh gp, gp, r0  ;; converion
    qmtc2.ni vf15, s1
    psraw gp, gp, 16
    qmtc2.ni vf13, s4
    pextlh s5, s5, r0
    qmtc2.ni vf10, s3
    psraw s4, s5, 16
    lw s3, 4(t9)       ;; DIFFERENT: s3 = flags
    addu ra, ra, v1    ;; output addr calc
    lhu s0, 62(t5)     ;; wind garbage
    andi s2, s3, 33    ;; and with (prototype-flags disable visible)
    lw s1, 468(t0)     ;; wind garbage
    andi s5, s3, 2     ;; S5 = and with (flags flag-bit-one)
    qmtc2.ni vf11, gp
    dsll gp, s0, 5
    qmtc2.ni vf12, s4
    daddu s4, s1, gp
    sw ra, 196(t0)
    andi s1, s3, 8    ;; S1 = and with (flags vanish)
    lwc1 f7, 152(t9)
    andi gp, s3, 132  ;; GP = and with (flags tpage-alpha tpage-water)
    lwc1 f6, 148(t9)
    sll r0, r0, 0
    sw s1, 516(t0)
    sll r0, r0, 0
    sw gp, 520(t0)
    bne s2, r0, L316 ;; skip if invisible/disabled
    cfc2.ni s3, vi1

B26:
    vitof0.xyzw vf13, vf13
    lwc1 f9, 160(t9)
    bne s3, r0, L316
    lwc1 f8, 156(t9)

B27:
    vitof12.xyzw vf10, vf10
    lhu s3, 46(t5)
    sll r0, r0, 0
    lqc2 vf17, 32(t0)
    andi s3, s3, 2      ;; check instance flag and skip (new for jak 2??)
    lqc2 vf19, 16(t0)
    bne s3, r0, L316
    qmfc2.i s2, vf6
```
Not too much learned here:
- Disabled/Invisible are both skipped here.
- `s5` is set if `flag-bit-one` is set (could this be envmap? just a guess).
- vanish sets `vanish-flag` in `instance-tie-work`.
- using either `tpage-alpha` or `tpage-water` will set `translucent-flag`
- there's a per-instance visibility flag now.


After this, there's some math likely for distance/interpolation.

And some more reference to the `gp` (containing the `alpha`/`water` flag bits):
```
    bne gp, r0, L285  ;; check flag
    lhu s2, 6(t9)

B29:
    beq r0, r0, L286
    lw s3, 544(t0)    ;; if flag is 0 , s3 = itw.tfrag-dists

B30:
L285:
    andi gp, gp, 128
    sll r0, r0, 0
    bne gp, r0, L286
    lw s3, 552(t0) ;; itw.water-dists

B31:
    sll r0, r0, 0
    lw s3, 548(t0) ;; itw.alpha-dists
B32:
```
these `dists` are part of the texture system, so it can know which MIP levels need to be in VRAM. Again, nothing surprising, but more evidence that these proto flags will be used to pick trans/water modes.

B32 is a bunch more math, and starts writing the output.
```
~~~~~~
    sqc2 vf5, 80(t8)
    ppach s1, r0, s1
    sw s5, 80(t8) ;; kinda sus, this is flags and with (flags flag-bit-one)
    or s2, s1, s2
    sqc2 vf14, 96(t0)
~~~~~~
```
note that this similar suspicious line appeared in jak 1, and was unused. may just be leftover. We don't seem to have a type for this output strangely.

B33 is wind-only (ugh, different from jak 1).

B34 is back on the main path:
```
bne s5, r0, L299 ;; if bit-one set, go to L299
...

L288
```

This seems very promising to be the environment map bit. I checked `bit-one` in `extract_tie.cpp` and found that it seems to the environment map bit.

So to summarize, at this point:
- seems very likely that `tpage-alpha`/`tpage-water` bits control the category.
- seems like `bit-one` controls envmapping.
- `L299` is the envmap drawing part of `draw-inline-array-instance-tie`
- `L288` is the non-envmap path

At this point, I printed names/flags for all protos, and the bit-one guess seems right.

I will take a break from reverse engineering and start splitting up TIE to draw during multiple buckets. Right now we draw _all_ ties as part of the normal tie bucket. But in the real game, parts of the tree are drawn in different buckets, based on the "category". It's important that we change this, otherwise our draw order will be different from the normal games, causing issues with transparency.

The solution is to split up the C++ Tie renderer by category. It needs to support something like "draw everything from this tree, but only if it matches this category".

### Splitting up TIE 1

Detecting the category is simple:
```cpp
struct TieCategoryInfo {
  tfrag3::TieCategory category;
  bool uses_envmap;
};

TieCategoryInfo get_jak2_tie_category(u32 flags) {
  constexpr int kJak2ProtoDisable = 1;
  constexpr int kJak2ProtoEnvmap = 2;
  constexpr int kJak2ProtoTpageAlpha = 4;
  constexpr int kJak2ProtoVanish = 8;
  constexpr int kJak2ProtoBitFour = 16;
  constexpr int kJak2ProtoVisible = 32;
  constexpr int kJak2ProtoNoCollide = 64;
  constexpr int kJak2ProtoTpageWater = 128;
  TieCategoryInfo result;
  if (flags & kJak2ProtoTpageAlpha) {
    result.category = tfrag3::TieCategory::TRANS;
    ASSERT((flags & kJak2ProtoTpageWater) == 0);
  } else if (flags & kJak2ProtoTpageWater) {
    result.category = tfrag3::TieCategory::WATER;
    ASSERT((flags & kJak2ProtoTpageAlpha) == 0);
  } else {
    result.category = tfrag3::TieCategory::NORMAL;
  }
  result.uses_envmap = flags & kJak2ProtoEnvmap;
  return result;
}

TieCategoryInfo get_jak1_tie_category(u32 flags) {
  TieCategoryInfo result;
  result.category = tfrag3::TieCategory::NORMAL;
  result.uses_envmap = flags & 2;
  return result;
}
```

Updating the data format:
```cpp
enum class TieCategory {
  NORMAL,
  TRANS,  // also called alpha
  WATER,
};
constexpr int kNumTieCategories = 3;

// A tie model
struct TieTree {
  BVH bvh;
  std::vector<StripDraw> static_draws;
  // Category n uses draws: static_draws[cdi[n]] to static_draws[cdi[n + 1]]
  std::array<u32, kNumTieCategories + 1> category_draw_indices;
```
This lets us keep a flat list of draws, which will work with all the existing shared tfrag/tie/shrub code.

Updating `extract_tie.cpp` was straightforward.

Updating `Tie3.cpp` required a pretty big rewrite (a few hours). Hopefully I won't have to do this all over again for envmap!

It looks something like this. First, in the main `Tie3` renderer (associated with the default bucket), we do all setup for all trees, but only draw the default bucket.
```cpp
/*!
 * Render method called from bucket render system.
 * Does common setup for all category, but only renderers default_category.
 */
void Tie3::render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) {
  // read dma, figure out what level to draw, camera matrix, etc.
  set_up_common_data_from_dma(dma, render_state);

  // for each tree, do culling, time of day
  setup_all_trees(lod(), m_common_data.settings, m_common_data.proto_vis_data,
                  m_common_data.proto_vis_data_size, !render_state->no_multidraw);

  // for each tree, render stuff in default category
  draw_matching_draws_for_all_trees(lod(), m_common_data.settings, render_state, prof,
                                    m_default_category);
}
```
then, in the other buckets, we end up calling this method:
```cpp
void Tie3::render_from_another(SharedRenderState* render_state,
                               ScopedProfilerNode& prof,
                               tfrag3::TieCategory category) {
  // only draw if the main bucket drew on this frame.
  if (render_state->frame_idx != m_common_data.frame_idx) {
    return;
  }

  // draw for this category. Reuse setup from before.
  draw_matching_draws_for_all_trees(lod(), m_common_data.settings, render_state, prof, category);
}
```
This is kind of nice because we don't have to actually generate DMA for the non-default buckets - the C++ renderer will know to draw these as long as the default bucket was drawn.

I plan to extend this for the envmap buckets, with something like `draw_matching_envmap_draws`.

## Getting the envmap shader data into the FR3
I am blind and missed that there's an `envmap-shader` reference in `prototype-bucket-tie`. It's just a reference to an adgif shader stored somewhere else.

```
   (envmap-rfade    float            :offset-assert 160)
   (envmap-fade-far float            :offset-assert 164)
   (envmap-shader   adgif-shader     :offset-assert 168)
   (tint-color      uint32           :offset-assert 172)
```
I bet the `tint-color` will be important too...

I'd exepct something to "log in" this `envmap-shader`. If it's part of the normal list of shaders, there's no need to explicitly log it in. But I'd guess it's not part of the normal list (based on what a vaguely remember for how Jak 1 built TIE stuff), so I looked for code that specifically logs it in:
```
;; get the envmap-shader from the prototype
(let ((envmap-shader (-> proto2 envmap-shader)))
  ;; check if it exists (not set on non-envmapped protos)
 (when (nonzero? envmap-shader)
   ;; "log in" the adgif shader to link it to a texture.
   ;; in the static
   (let ((envmap-tex (adgif-shader-login-no-remap envmap-shader)))
     ;; that returns the texture
     (when envmap-tex
       ;; update texture masks
       (dotimes (v1-137 3)
         (dotimes (a0-74 3)
           (set! (-> (the-as (pointer int32) (+ (+ (* v1-137 16) (* a0-74 4)) (the-as int *texture-masks*))))
                 (logior (-> (the-as (pointer int32) (+ (* a0-74 4) (the-as int *texture-masks*) (* v1-137 16))) 0)
                         (-> (the-as (pointer int32) (+ (* a0-74 4) (the-as int envmap-tex) (* v1-137 16))) 15)
                         )
                 )
           )
         (set! (-> *texture-masks* data v1-137 mask w)
               (the-as int (fmax (-> *texture-masks* data v1-137 dist) (-> envmap-tex masks data v1-137 dist)))
               )
         )
       )
     )

   ;; set envmap shader stuff.
   (set! (-> envmap-shader tex1) (new 'static 'gs-tex1 :mmag #x1 :mmin #x1))
   (set! (-> envmap-shader clamp)
         (new 'static 'gs-clamp :wms (gs-tex-wrap-mode clamp) :wmt (gs-tex-wrap-mode clamp))
         )
   (set! (-> envmap-shader alpha) (new 'static 'gs-alpha :b #x2 :c #x1 :d #x1))
   (set! (-> envmap-shader prims 1) (gs-reg64 tex0-1))
   (set! (-> envmap-shader prims 3) (gs-reg64 tex1-1))
   (set! (-> envmap-shader prims 5) (gs-reg64 miptbp1-1))
   (set! (-> envmap-shader clamp-reg) (gs-reg64 clamp-1))
   (set! (-> envmap-shader prims 9) (gs-reg64 alpha-1))
   )
 )
```

As more confirmation that this envmap shader is "special" and outside of the normal tie-proto shader lists, I looked at `mountain`'s `.asm` file, and saw that nothing else reference this adgif data other than the label in the `envmap-shader` slot. And it didn't appear to be part of the other shader list for the proto.


The mystery now is: how does this envmap shader work when there's also a default-envmap uploaded to VU1 for etie? My guess is that this per-proto will override the default shader.  To get an idea of how often this happened, I modified `extract_tie.cpp` to also grab this shader, and added this assert
```cpp
bool using_envmap = info.uses_envmap; // set from proto flags
ASSERT(using_envmap == proto.envmap_adgif.has_value()); // set from envmap-shader being nonzero
```
and it passed! So it seems like all protos using envmap provide their own shader.

Looking quickly at `draw-inline-array-prototype-tie-asm` shows that it always uploads the `envmap-shader`, so I guess the default one isn't used?

Progress up to this point took about 1 weekend day (8 hours).

## Looking for a shortcut: I really don't want to figure out the whole program

Figure out all of etie looks absolutely awful (3k lines of EE assembly, 1.5k lines of VU1 assembly), so let's make some guesses.

First, add some new categories for envmap:
```
enum class TieCategory {
  NORMAL,
  TRANS,  // also called alpha
  WATER,
  NORMAL_ENVMAP,
  TRANS_ENVMAP,
  WATER_ENVMAP
};
```

I refactored things so I could write this:

```cpp
handle_draw_for_strip(tree, static_draws_by_tex,
                      draws_by_category.at((int)info.category), packed_vert_indices,
                      mode, idx_in_lev_data, strip, inst, ifrag, proto_idx, frag_idx,
                      strip_idx, matrix_idx);

// also add the envmap draw: note useing envmap_drawmode and envmap_category this time
if (info.uses_envmap) {
  handle_draw_for_strip(tree, static_draws_by_tex,
                        draws_by_category.at((int)info.envmap_category),
                        packed_vert_indices, envmap_drawmode, idx_in_lev_data, strip,
                        inst, ifrag, proto_idx, frag_idx, strip_idx, matrix_idx);
}
```
But that doesn't make sense...
This code does:
- always add normal draw to normal category
- if we envmap, add a draw to envmap category

What we really want is:
- if we envmap, add normal draw to envmap category
- add a second draw, also in the envmap category, somehow flagged to behave differently.

Environment mapped stuff is drawn in two passes: a base pass that's very similar to normal TIE, and a second pass to draw on the shiny stuff.

We merge and reorder draws (within a category) to reduce the number of texture switches. The number of switches for the first and second pass may be different (eg: two objects may use the same base texture, but different envmap texture), and in these cases we want to merge their second-pass draws but not their first. So it makes sense to have an entirely separate draw list for envmap drawing.

```cpp
if (info.uses_envmap) {
  // first pass: normal draw mode, envmap bucket, normal draw list
  handle_draw_for_strip(tree, static_draws_by_tex,
                        draws_by_category.at((int)info.envmap_category),
                        packed_vert_indices, mode, idx_in_lev_data, strip, inst, ifrag,
                        proto_idx, frag_idx, strip_idx, matrix_idx);
  // second pass envmap draw mode, in envmap bucket, envmap-specific draw list
  handle_draw_for_strip(tree, envmap_draws_by_tex,
                        envmap_draws_by_category.at((int)info.envmap_category),
                        packed_vert_indices, envmap_drawmode, idx_in_lev_data, strip,
                        inst, ifrag, proto_idx, frag_idx, strip_idx, matrix_idx);
} else {
  // totally normal stuff
  handle_draw_for_strip(tree, static_draws_by_tex,
                        draws_by_category.at((int)info.category), packed_vert_indices,
                        mode, idx_in_lev_data, strip, inst, ifrag, proto_idx, frag_idx,
                        strip_idx, matrix_idx);
}
```
At this point, I did a round of actually implementing all the bits and pieces to make this load. I just skipped doing anything with the envmap draws, and only drew the normal ones.

And this worked! I saw the "first pass" draw for stuff like mountain temple:
(TODO picture)
So it appears, but isn't shiny.

I realized two problems:
- this is a bad idea, because we no longer have a single flat list of draws.
- there are still bad settings (end of pipe at pumping station is messed up)

For the first problem, I think I should still have a single draw list for the entire TIE tree, but just add more categories. For example `NORMAL_ENVMAP` should be split into `NORMAL_ENVMAP_FIRST_PASS` and `NORMAL_ENVMAP_SECOND_PASS`. The `Tie3AnotherCategory` renderer assigned to first pass will be responsible for also handling the second pass.

For the second problem, I think I can guess what's happening. Usually the alpha channel is used for "alpha test", which is a discrete "draw/don't draw" decision per pixel. This is useful for some textures that have totally see-through parts (eg: leafy things). For envmapped stuff, I think they turn off alpha test, and this ends up getting used as an "envmap multiplier".

This reminds me of a third problem, which is that the trans/alpha/water TIE buckets may have different renderer settings that are not properly reflected anywhere.

This reminds me of a 4th problem, which is that the tfx/tcc settings don't seem to work right for TIE. There are places where they switch this mode to DECAL for lights, and getting this wrong makes lights look dim. I don't really know why they do this, but it was a common pattern for jak 1 lights (and eyes).

With all these problems, I am out of time. (spent about 4 hours over an evening)

### Looking at problem 4
DECAL doesn't seem to work. From what I remember, decal should be set during adgif-shader login.
Sure enough, I have this in `extract_tie.cpp`
```
  // the value is overwritten by the login function. We don't care about this value, it's
  // specific to the PS2's texture system.
  ASSERT(ra_tex0_val == 0 || ra_tex0_val == 0x800000000);  // note: decal
  // the original value is a flag. this means to use decal texture mode (todo)
  if (uses_magic_tex0_bit) {
    *uses_magic_tex0_bit = ra_tex0_val == 0x800000000;
  }
```
and it gets set on tons of lights:
```
magic bit: sewer-light-base-rectangular.mb
magic bit: skatea-interior-wall-light.mb
magic bit: city-slum-lamp-01.mb
```
but nothing actually uses this info.

Easy fix for the fragment shader:
```
    if (decal == 1) {
        // output color is only from the texture.
        // likely more efficient to have this branch in the vertex shader.
        fragment_color = vec4(1.0, 1.0, 1.0, 1.0);
    } else {
        // time of day lookup
        fragment_color = texelFetch(tex_T1, time_of_day_index, 0);
        // color adjustment
        fragment_color *= 2;
        fragment_color.a *= 2;
    }
```

set up the uniform:
```cpp
void Tie3::init_shaders(ShaderLibrary& shaders) {
  m_uniforms.decal = glGetUniformLocation(shaders[ShaderId::TFRAG3].id(), "decal");
}
```
then in the drawing loop:
```cpp
    glUniform1i(m_uniforms.decal, draw.mode.get_decal() ? 1 : 0);
```
and it fixes the lights!

I checked on jak 1, and found that we missed some blue things in citadel, near the robot.

### Looking at problem 2, then getting distracted and working on problem 3
This problem is that alpha test is removing pixels that shouldn't be removed on the first draw of envmap.

My guess is that alpha-test should be entirely disabled for the envmap base draw. This is how it worked in jak 1.

The alpha test is controlled by the `gs-test` register, typically set by A+D data. There's some asserts in `process_adgif`that prove that it's not being set through `adgif-shaders`. In the original TIE renderer, there was some magic to toggle on and off alpha testing (which we handle). In the EE code, they generate template data:
```
  (set! (-> arg0 atestgif tag) (new 'static 'gif-tag64 :nloop #x2 :eop #x1 :nreg #x1))
  (set! (-> arg0 atestgif regs) (new 'static 'gif-tag-regs :regs0 (gif-reg-id a+d)))
  (set! (-> arg0 alpha data) (the-as uint arg1))
  (set! (-> arg0 alpha cmds) (gs-reg64 alpha-1))
  (set! (-> arg0 atest-tra cmds) (gs-reg64 test-1))
  (set! (-> arg0 atest-tra data) (the-as uint arg2))
  (set! (-> arg0 atest-def cmds) (gs-reg64 test-1))
  (set! (-> arg0 atest-def data) (the-as uint arg3))
```
The "tra" and "def" test modes can be controlled by the VU program.

In Jak 1 we detected this with the `misc_x` flag
```cpp
// determine the draw mode
DrawMode mode = process_draw_mode(strip.adgif, frag.prog_info.misc_x == 0, frag.has_magic_tex0_bit);
```
which gets set from the lower bit of some mysterious VU program register (ugh I hope we don't have to find more crap like this ever again).

```
frag.prog_info.misc_x = vi01;

u16 vi01 = vf04_z;

u16 vf04_z = frag.other_gif_data.at(10);

// each frag also has "other" data. This is some index data that the VU program uses.
// it comes in gif_data, after tex_qwc (determined from EE program)
int tex_qwc = proto.geometry[geo].tie_fragments.at(frag_idx).tex_count;
int other_qwc = proto.geometry[geo].tie_fragments.at(frag_idx).gif_count;
frag_info.other_gif_data.resize(16 * other_qwc);
memcpy(frag_info.other_gif_data.data(),
       proto.geometry[geo].tie_fragments[frag_idx].gif_data.data() + (16 * tex_qwc),
       16 * other_qwc);
```

Depending on the category, the `tra` and `def` are different.

Normal:
```
(tie-init-buf
  (-> s2-0 tie-bucket)
  (the-as gs-alpha gp-0)
  (new 'static 'gs-test
    :ate #x1
    :atst (gs-atest greater-equal)
    :aref #x26
    :zte #x1
    :ztst (gs-ztest greater-equal)
    )
  (new 'static 'gs-test :zte #x1 :ztst (gs-ztest greater-equal))
  )
```

Trans: (same as normal for `gs-test`)
```
(tie-init-buf
  (-> s2-0 tie-trans-bucket)
  (the-as gs-alpha s4-0)
  (new 'static 'gs-test
    :ate #x1
    :atst (gs-atest greater-equal)
    :aref #x26
    :zte #x1
    :ztst (gs-ztest greater-equal)
    )
  (new 'static 'gs-test :zte #x1 :ztst (gs-ztest greater-equal))
  )
```

Water:
```
(tie-init-buf
  (-> s2-0 tie-water-bucket)
  (the-as gs-alpha s4-0)
  (new 'static 'gs-test :ate #x1 :afail #x1 :zte #x1 :ztst (gs-ztest greater-equal))
  (new 'static 'gs-test :ate #x1 :afail #x1 :zte #x1 :ztst (gs-ztest greater-equal))
  )
```
(note that this weird alpha test setting is effectively used to mask z-buffer writes).

This was a good point to notice that they turned on alpha blending for TIE in jak 2.

From here, we can modify `extract_tie` to apply the correct `gs-test` settings based on the category.

## Going back to problem 2

We need to look at the code that sets up ETIE to understand the settings.
Unlike normal TIE, there doesn't seem to be a feature to toggle between `tra` and `def` mode.  Here is `etie-init-engine`, which sets up DMA to set `gs-test` prior to any ETIE stuff running. (this is different from TIE, which puts the `gs-test` stuff in the constants block so the VU program can manipulate gs-test at runtime)
```
(defun etie-init-engine ((arg0 dma-buffer) (arg1 gs-alpha) (arg2 gs-test))
  ;; set up DMA for 1 register
  (let* ((v1-0 arg0)
         (a0-1 (the-as dma-packet (-> v1-0 base)))
         )
    (set! (-> a0-1 dma) (new 'static 'dma-tag :qwc #x2 :id (dma-tag-id cnt)))
    (set! (-> a0-1 vif0) (new 'static 'vif-tag))
    (set! (-> a0-1 vif1) (new 'static 'vif-tag :imm #x2 :cmd (vif-cmd direct) :msk #x1))
    (set! (-> v1-0 base) (the-as pointer (&+ a0-1 16)))
    )
  (let* ((v1-1 arg0)
         (a0-3 (the-as gs-gif-tag (-> v1-1 base)))
         )
    (set! (-> a0-3 tag) (new 'static 'gif-tag64 :nloop #x1 :eop #x1 :nreg #x1))
    (set! (-> a0-3 regs) GIF_REGS_ALL_AD)
    (set! (-> v1-1 base) (the-as pointer (&+ a0-3 16)))
    )
  (let* ((v1-2 arg0)
         (a0-5 (-> v1-2 base))
         )
    (set! (-> (the-as (pointer uint64) a0-5)) arg2) ;; this is where gs-test is set
    (set! (-> (the-as (pointer gs-reg64) a0-5) 1) (gs-reg64 test-1))
    (set! (-> v1-2 base) (&+ a0-5 16))
    )
  ;; set up DMA to load ETIE program
  (dma-buffer-add-vu-function arg0 etie-vu1-block 1)

  ;; set up DMA to load ETIE constants
  (let ((s4-0 12))
    (let* ((v1-3 arg0)
           (a0-8 (the-as dma-packet (-> v1-3 base)))
           )
      (set! (-> a0-8 dma) (new 'static 'dma-tag :id (dma-tag-id cnt) :qwc s4-0))
      (set! (-> a0-8 vif0) (new 'static 'vif-tag :cmd (vif-cmd stmod)))
      (set! (-> a0-8 vif1) (new 'static 'vif-tag :imm #x382 :cmd (vif-cmd unpack-v4-32) :num s4-0))
      (set! (-> v1-3 base) (the-as pointer (&+ a0-8 16)))
      )
    (etie-init-consts (the-as etie-consts (-> arg0 base)) arg1)
    (&+! (-> arg0 base) (* s4-0 16))
    )

  ;; DMA to run program at address 8 (likely some init thing), then flusha.
  (let* ((v1-6 arg0)
         (a0-12 (the-as dma-packet (-> v1-6 base)))
         )
    (set! (-> a0-12 dma) (new 'static 'dma-tag :id (dma-tag-id cnt)))
    (set! (-> a0-12 vif0) (new 'static 'vif-tag :imm #x8 :cmd (vif-cmd mscalf) :msk #x1))
    (set! (-> a0-12 vif1) (new 'static 'vif-tag :cmd (vif-cmd flusha) :msk #x1))
    (set! (-> v1-6 base) (the-as pointer (&+ a0-12 16)))
    )

  ;; dma to set the ROW register
  (let* ((v1-7 arg0)
         (a0-14 (the-as dma-packet (-> v1-7 base)))
         )
    (set! (-> a0-14 dma) (new 'static 'dma-tag :qwc #x2 :id (dma-tag-id cnt)))
    (set! (-> a0-14 vif0) (new 'static 'vif-tag))
    (set! (-> a0-14 vif1) (new 'static 'vif-tag :cmd (vif-cmd strow) :msk #x1))
    (set! (-> v1-7 base) (the-as pointer (&+ a0-14 16)))
    )
  (let* ((v1-8 arg0)
         (a0-16 (-> v1-8 base))
         )
    ;; common values used for their "use ROW add to convert ints to floats" trick
    (set! (-> (the-as (pointer uint32) a0-16) 0) (the-as uint #x4b000000))
    (set! (-> (the-as (pointer uint32) a0-16) 1) (the-as uint #x4b000000))
    (set! (-> (the-as (pointer uint32) a0-16) 2) (the-as uint #x4b000000))
    (set! (-> (the-as (pointer uint32) a0-16) 3) (the-as uint #x4b000000))

    ;; set up base/offset for xtop-based double-buffering
    (set! (-> (the-as (pointer vif-tag) a0-16) 4) (new 'static 'vif-tag :imm #xb8 :cmd (vif-cmd base)))
    (set! (-> (the-as (pointer vif-tag) a0-16) 5) (new 'static 'vif-tag :imm #x20 :cmd (vif-cmd offset)))

    ;; setup default uploading mode for ETIE, I guess.
    (set! (-> (the-as (pointer vif-tag) a0-16) 6) (new 'static 'vif-tag :cmd (vif-cmd stmod)))
    (set! (-> (the-as (pointer vif-tag) a0-16) 7) (new 'static 'vif-tag :imm #x404 :cmd (vif-cmd stcycl)))
    (set! (-> v1-8 base) (&+ a0-16 32))
    )
  (none)
  )
  ```
There's still some mysterious stuff going on with alpha (why does that need to be sent at all? adgif shaders should be setting alpha!), but let's ignore it for now. We ignored it for Jak 1's TIE and it was ok. (I kinda suspect it's for vanish or something like that?).

Just like TIE, this can be traced back to figure out the per-bucket settings:
```cpp

/*!
 * Get the draw mode settings that are pre-set for the entire bucket and not controlled by adgif
 * shaders
 */
DrawMode get_base_draw_mode_jak2(bool use_tra, tfrag3::TieCategory category) {
  DrawMode mode;
  mode.enable_ab();
  switch (category) {
    case tfrag3::TieCategory::NORMAL:
    case tfrag3::TieCategory::TRANS:
      mode.enable_zt();
      mode.set_depth_test(GsTest::ZTest::GEQUAL);
      if (use_tra) {
        mode.enable_at();
        mode.set_aref(0x26);
        mode.set_alpha_fail(GsTest::AlphaFail::KEEP);
        mode.set_alpha_test(DrawMode::AlphaTest::GEQUAL);
      } else {
        mode.disable_at();
      }
      break;
      mode.enable_zt();
      mode.set_depth_test(GsTest::ZTest::GEQUAL);
      if (use_tra) {
        mode.enable_at();
        mode.set_aref(0x26);
        mode.set_alpha_fail(GsTest::AlphaFail::KEEP);
        mode.set_alpha_test(DrawMode::AlphaTest::GEQUAL);
      } else {
        mode.disable_at();
      }
      break;
    case tfrag3::TieCategory::WATER:
    case tfrag3::TieCategory::WATER_ENVMAP:
    case tfrag3::TieCategory::WATER_ENVMAP_SECOND_DRAW:
      // (new 'static 'gs-test :ate #x1 :afail #x1 :zte #x1 :ztst (gs-ztest greater-equal))
      mode.enable_zt();
      mode.set_depth_test(GsTest::ZTest::GEQUAL);
      mode.enable_at();
      mode.set_aref(0);
      mode.set_alpha_fail(GsTest::AlphaFail::FB_ONLY);
      mode.set_alpha_test(DrawMode::AlphaTest::NEVER);
      break;

    case tfrag3::TieCategory::NORMAL_ENVMAP:
    case tfrag3::TieCategory::TRANS_ENVMAP:
    case tfrag3::TieCategory::NORMAL_ENVMAP_SECOND_DRAW:
    case tfrag3::TieCategory::TRANS_ENVMAP_SECOND_DRAW:
      mode.enable_zt();
      mode.set_depth_test(GsTest::ZTest::GEQUAL);
      mode.disable_at();
      break;

    default:
      ASSERT(false);
  }
  return mode;
}
```
So we can take these settings, then apply alpha/tex0/clamp from the adgif shader, then we'll get our final draw mode. Notice that I'm assuming that the second draw uses the same settings. I think this actually makes sense for `gs-test`, but I'm less sure about alpha (not that we're setting it yet...).

Also, refactored the categories again to have separate "second draw" categories.
```cpp
TieCategoryInfo get_jak2_tie_category(u32 flags) {
  constexpr int kJak2ProtoDisable = 1;
  constexpr int kJak2ProtoEnvmap = 2;
  constexpr int kJak2ProtoTpageAlpha = 4;
  constexpr int kJak2ProtoVanish = 8;
  constexpr int kJak2ProtoBitFour = 16;
  constexpr int kJak2ProtoVisible = 32;
  constexpr int kJak2ProtoNoCollide = 64;
  constexpr int kJak2ProtoTpageWater = 128;
  TieCategoryInfo result;
  result.uses_envmap = flags & kJak2ProtoEnvmap;

  if (flags & kJak2ProtoTpageAlpha) {
    if (result.uses_envmap) {
      result.category = tfrag3::TieCategory::TRANS_ENVMAP;
      result.envmap_second_draw_category = tfrag3::TieCategory::TRANS_ENVMAP_SECOND_DRAW;
    } else {
      result.category = tfrag3::TieCategory::TRANS;
    }
    ASSERT((flags & kJak2ProtoTpageWater) == 0);
  } else if (flags & kJak2ProtoTpageWater) {
    if (result.uses_envmap) {
      result.category = tfrag3::TieCategory::WATER_ENVMAP;
      result.envmap_second_draw_category = tfrag3::TieCategory::WATER_ENVMAP_SECOND_DRAW;
    } else {
      result.category = tfrag3::TieCategory::WATER;
    }
    ASSERT((flags & kJak2ProtoTpageAlpha) == 0);
  } else {
    if (result.uses_envmap) {
      result.category = tfrag3::TieCategory::NORMAL_ENVMAP;
      result.envmap_second_draw_category = tfrag3::TieCategory::NORMAL_ENVMAP_SECOND_DRAW;
    } else {
      result.category = tfrag3::TieCategory::NORMAL;
    }
  }
  return result;
}
```
The "category" now means the category for the base draw. If there is a second pass draw, it goes in `envmap_second_draw_category`. This removes the envmap-specific draw list.

Some more refactoring later, it looks awful. Turning on alpha blending has caused the envmap base draw to make stuff partially transparent:

PICTURE

Some ideas:
- the alpha blend mode used is wrong
- the VU program clears the alpha blend enable bit
- the VU program overwrites alpha somehow

Looking into the first idea first. All the adgifs go through this, so I think that all adgifs do set alpha:
```cpp
  // alpha settings. we care about these, but no hidden value
  u8 ra_alpha = gif_data.at(16 * (tex_idx * 5 + 4) + 8);
  ASSERT(ra_alpha == (u8)GsRegisterAddress::ALPHA_1); // make sure it's setting alpha
  u64 alpha;
  memcpy(&alpha, &gif_data.at(16 * (tex_idx * 5 + 4)), 8);
  adgif.alpha_val = alpha;
  return adgif;
```
unsupported alphas are also an error, so I don't think it's ignoring a weird alpha mode. Interestingly, almost all TIE adgifs in jak 2 have SRC_DST_SRC_DST ("normal transparency"). The only exception is some `vil1-outdoor-light` copied from jak 1. This is different from jak 1, which sometimes set other modes (despite having alpha blend turned off, causing them to do nothing).

To investiage the others, we'll have to figure out the address of the alpha cmd, and the `abe` bit
```
   (alpah   gs-adcmd     :inline :offset-assert  32)
   (strgif  gs-gif-tag   :inline :offset-assert  48)
```
in constants.
The unpack is at
```
      (set! (-> a0-8 vif1) (new 'static 'vif-tag :imm #x382 :cmd (vif-cmd unpack-v4-32) :num s4-0))
```
So the address is `0x384` for the alpha and `0x385` for the "strgif" (containing `:abe 1` bit).

The `0x384` is 900. It's used here:

```
  lq.xyzw vf17, 903(vi00)    |  nop  ;; load, from constants, the envmap shader
  lq.xyzw vf18, 904(vi00)    |  nop  ;; (5x A+D format data)
  lq.xyzw vf19, 905(vi00)    |  nop
  lq.xyzw vf20, 906(vi00)    |  nop
  lq.xyzw vf21, 907(vi00)    |  nop
  iaddi vi04, vi00, 0x0      |  nop  ;; vi04 = 0
  lq.xyz vf11, 899(vi00)     |  nop  ;; vf11 = adgif, the template giftag for sending an adgif shader
  ilwr.w vi05, vi04          |  nop  ;; loading... something...
  ilw.w vi07, 1(vi04)        |  nop  ;; loading more things
  ilw.w vi13, 2(vi04)        |  nop  ;; more things
  lq.xyzw vf24, 900(vi00)    |  nop  ;; load the alpha a+d data
  lqi.xyzw vf12, vi04        |  nop  ;; load 5 qw in a row. Very likely to be an adgif shader
  lqi.xyzw vf13, vi04        |  nop
  lqi.xyzw vf14, vi04        |  nop
  lqi.xyzw vf15, vi04        |  nop
  lqi.xyzw vf16, vi04        |  subw.w vf11, vf11, vf11 ;; this clears the upper 32-bits of the giftag
  iadd vi05, vi05, vi12      |  nop                     ;; they like to stash random crap here.
  iadd vi06, vi05, vi13      |  nop
  iaddi vi01, vi00, 0x6      |  nop
  sq.xyzw vf11, -1(vi05)     |  nop  ;; stores the giftag, the data after vi05 should be adgif format.
  isw.x vi01, -1(vi05)       |  nop  ;; _modifies_ giftag to include 6 a+d's instead of usual 5
  sqi.xyzw vf12, vi05        |  nop  ;; store the 5 a+d's like normal
  sqi.xyzw vf13, vi05        |  nop
  sqi.xyzw vf14, vi05        |  nop
  sqi.xyzw vf15, vi05        |  nop
  sqi.xyzw vf16, vi05        |  nop
  b L6                       |  nop
  sqi.xyzw vf24, vi05        |  nop  ;; (branch delay slot), store the 6th thing, the alpha

;; now we have some sort of loop to store more adgifs.
;; strangely, vf16 isn't reloaded, so they always store the same one
;; (which will be alpha... but not the adjusted alpha.)
  L5:
  iadd vi05, vi05, vi12      |  nop
  iadd vi06, vi05, vi13      |  nop
  sqi.xyzw vf11, vi05        |  nop ;; store adgif giftag
  sqi.xyzw vf12, vi05        |  nop ;; store adgif data (5 qw)
  sqi.xyzw vf13, vi05        |  nop
  sqi.xyzw vf14, vi05        |  nop
  sqi.xyzw vf15, vi05        |  nop
  sqi.xyzw vf16, vi05        |  nop ;; they don't reload vf16 ever?
L6:
  sqi.xyzw vf11, vi06        |  nop ;; store adgif giftag
  sqi.xyzw vf17, vi06        |  nop ;; store the envmap shader's a+d
  sqi.xyzw vf18, vi06        |  nop
  sqi.xyzw vf19, vi06        |  nop
  sqi.xyzw vf20, vi06        |  nop
  sqi.xyzw vf21, vi06        |  nop
  iaddi vi07, vi07, -0x1     |  nop
  ilwr.w vi05, vi04          |  nop
  lqi.xyzw vf12, vi04        |  nop
  lqi.xyzw vf13, vi04        |  nop
  lqi.xyzw vf14, vi04        |  nop
  lqi.xyzw vf15, vi04        |  nop
  ibgtz vi07, L5             |  nop
  ;; edit: OOPS the load of vf16 was here...
```
which brings up a few mysteries:
- it looks like they do the second envmap draw with the shader supplied in the constants block (always set to default-envmap-shader on VU program load). Then why did we think they were giving a per-proto envmap shader earlier?
- they "extend" the first adgif to use the alpha provided in the constants block. But then future adgifs don't get to use this.
- Future adgifs inherit the alpha from the first alpha. (or whatever a+d that had. I'm pretty sure it's always alpha, but who knows if other games are being played).

The first mystery is solved by looking at `tie-work.gc`
```
:envmap-shader (new 'static 'dma-packet
 :dma (new 'static 'dma-tag :qwc #x5 :id (dma-tag-id ref))
 :vif0 (new 'static 'vif-tag :cmd (vif-cmd stmod))
 :vif1 (new 'static 'vif-tag :imm #x387 :num #x5 :cmd (vif-cmd unpack-v4-32))
 )
```
this is a DMA template to load an adgif shader to address `0x387` - 5 qw into the constants block, which is where the `envmap` adgif-shader is located. So they basically upload the envmap adgif-shader on top of the default one. Good news. It means that our extraction of the envmap-specific adgif-shader was right.


The other two mysteries are unknown. We'll have to wait to see trans/water in use first.

Back to the alpha-blend-enable mystery. Searching for the use of the "strgif" tag (enables alpha blending, which seems wrong):
```
  iaddi vi04, vi04, -0x2     |  subw.w vf22, vf00, vf00
  ilwr.x vi08, vi04          |  subw.w vf23, vf00, vf00 ;; clear w of vf22.
  ilwr.y vi09, vi04          |  nop
  ilwr.z vi05, vi04          |  nop
  iaddi vi07, vi07, -0x1     |  nop
  iaddi vi04, vi04, 0x1      |  nop
  lq.xyz vf22, 901(vi09)     |  nop  ;; load only xyz part of strgif (w stays 0)
  ibeq vi00, vi07, L8        |  nop
  lq.xyz vf23, 902(vi09)     |  nop
L7:
  iadd vi05, vi05, vi12      |  nop
  iadd vi06, vi05, vi13      |  nop
  iaddi vi07, vi07, -0x1     |  nop
  sq.xyzw vf22, 0(vi05)      |  nop ;; store the strgif somewhere
  iswr.x vi08, vi05          |  nop
  sq.xyzw vf23, 0(vi06)      |  nop
  iswr.x vi08, vi06          |  nop
  ilwr.x vi08, vi04          |  nop
  ilwr.y vi09, vi04          |  nop
  ilwr.z vi05, vi04          |  nop
  iaddi vi04, vi04, 0x1      |  nop
  ibne vi00, vi07, L7        |  nop
  lq.xyz vf22, 901(vi09)     |  nop
```
zeoring out the upper 32-bits doesn't clear `:abe` (it's likely because they hid stuff in those bits, and they need to be 0 for the GS). So no answer there...

Looking through envmapped things _most_ of them have only 1 adgif. A few have more. What if this is actually the right behavior?
```cpp
  if (use_crazy_jak2_etie_alpha_thing) {
    if (tex_idx == 0) {
      first_adgif_alpha = adgif.alpha_val;
      adgif.alpha_val =
          alpha_value_for_etie_alpha_override(get_jak2_tie_category(proto.flags).category);
    } else {
      adgif.alpha_val = first_adgif_alpha;
    }
  }
```
it kinda seems promising. The alpha mode of 0 (the override mode for normal tie) corresponds to `(cs - cs) * as + cs`, which is just like having blending off.

So I tried this... and it still wasn't right. Setting the override is doing the right thing and making things not-transparent, but for the second/third adgifs, it is definitely wrong.

At this point, my guess is that the final a+d _doesn't set alpha at all_. So it looks like this:
```
first shader, special
a+d
a+d
a+d
a+d
a+d
bonus a+d: sets alpha to override

seonc shader:
a+d
a+d
a+d
a+d
a+d, but not actually alpha.

we still have the override alpha.
```

I scrolled through the entire VU1 program and didn't see any obvious place where alpha would be modified.

So I traced back. All ADGIF shaders are "logged in", which updates the `TEX0` data to point to the right VRAM address (and also sets stuff like tcc). This is a bit complicated because there's a bunch of packed flags, and the formats are terrible (in the file, it's some packed mess read by `adgif-shader-login-remap`, which outputs a GS format tag, with a bunch of ND specific stuff packed in the unused bits - some stuff is inserted by the `login` function - like a linked list pointer - and other magic bits are just passed through and have renderer-specific meaning).

This showed that some tie logins would actually change the register address in A+D format data:
```
      (let ((pre (-> s5-0 reg-4))
            (v1-1 (adgif-shader-login-no-remap s5-0)))
        (format 0 "TIE LOGIN: ~D, ~D -> ~D~%" (-> s5-0 alpha) pre (-> s5-0 reg-4))
        ...

;; this printed
TIE LOGIN: 34981577296, 66 -> 54
```
which is an alpha -> `miptbp2-1` register change. That would make perfect sense!

The actual `adgif-shader-login-no-remap` is an asm mess, but I see:
```
  (when (< (the-as uint 4) (-> arg1 num-mips))
    (set! (-> arg0 alpha-as-miptb2) (new 'static 'gs-miptbp
                                      :tbp1 (-> arg1 dest 4)
                                      :tbw1 (-> arg1 width 4)
                                      :tbp2 (-> arg1 dest 5)
                                      :tbw2 (-> arg1 width 5)
                                      :tbp3 (-> arg1 dest 6)
                                      :tbw3 (-> arg1 width 6)
                                      )
          )
    (set! (-> (&-> arg0 reg-4-u32) 0) (gs-reg32 miptbp2-1))
    )
```
which is replacing alpha with `miptbp2-1` if num_mips is more than 4. This is in `adgif-shader<-texture!` which I think is similar to the mip2sc version. The asm version has
```
daddiu t0, a3, -5
bltz t0, L44
```
which is a "branch if more than 5"...

Ignoring the 4 vs. 5 thing:
```cpp
  if (version == GameVersion::Jak2 && tfrag3::is_envmap_first_draw_category(category)) {
    if (num_mips > 5) {
      update_mode_from_alpha1(alpha_value_for_etie_alpha_override(category), mode);
    } else {
      // do ther normal stuff...
    }
  }
```
but I hit two problems:
- there were cases where it seemed like an alpha would "sneak through" where it shouldn't.
- I've also broken normal the doors in the stadium (jetboard). Might be normal TIE that's broken here.

So the plan for next time is:
- do my best guess at what ETIE should do, based on literal reading of the VU program.
- Identify a place where I think an alpha "sneaks through"
- Check it in game and compare to PCSX2.

There's also the fact that enabling alpha blend for normal TIE seems to do the wrong thing sometimes (stadium doors transparent, the TIE ones).

The good news is that some transparent TIEs look right now. So there was some benefit to all this.

And this is all the time I have for today (spent about 4.5 hours)

## Next. Looking into normal tie

the drawing kicks for normal tie. The first kick is (I think) initializing alpha, then the second draws stuff
```
  ilw.x vi01, 971(vi00)      |  nop
  ilw.y vi12, 971(vi00)      |  nop
  ilw.z vi02, 971(vi00)      |  nop
  lq.xyzw vf05, 972(vi00)    |  nop
  lq.xyzw vf06, 973(vi00)    |  nop
  lq.xyzw vf07, 974(vi00)    |  nop
  lq.xyzw vf08, 975(vi00)    |  nop
  sq.xyzw vf05, 976(vi00)    |  nop
  sq.xyzw vf06, 977(vi00)    |  nop
  isw.y vi02, 977(vi00)      |  nop
  ibeq vi00, vi01, L41       |  nop
  sq.xyzw vf07, 978(vi00)    |  nop
  sq.xyzw vf08, 978(vi00)    |  nop
L41:
  iaddiu vi02, vi00, 0x3d0   |  nop ;; vi02 = 976
  isw.y vi01, 971(vi00)      |  nop
  nop                        |  nop
  xgkick vi02                |  nop
```

This places the following giftag at 976:
```
(-> consts atestgif)
alpha a+d
test a+d
```
which sets alpha/test.

However, my current understanding of VU code is that alpha can "sneak through" if:
- less than 4 (or 5?) mips on any adgif in normal tie
- less than 4 (or 5?) mips on any adgif after the first one in etie

Using this "sneak through" logic does obviously wrong things for normal tie (pipe in atoll, the one you cross on the first mission).

Doing the sensible thing, and somehow assuming that the alpha from the category always "wins":

```cpp
  if (version == GameVersion::Jak1) {
    // use alpha from adgif shader, that's what we did in the past (could be wrong?)
    update_mode_from_alpha1(info.alpha_val, mode);
  } else {
    if (tfrag3::is_envmap_second_draw_category(category)) {
      // envmap shader gets to control its own alpha
      update_mode_from_alpha1(info.alpha_val, mode);
    } else {
      // non-envmap always get overriden (both the first draw of etie, and normal tie)
      update_mode_from_alpha1(alpha_value_for_jak2_tie_or_etie_alpha_override(category), mode);
    }
  }
```
this seems to work everywhere I looked (included stuff that should be transparent, or drawn wrong earlier). Though it's very confusing how this actually happens. I've wasted enough time here.


## Vertex normals
One of the challenges of environment mapping is that the VU1 program needs a normal for each vertex. Note that these aren't "face normals" - these would cause obvious discontinuities in the environment map texture coordinates. This means that each vertex needs a normal.

The challenge here is finding the surface normals from inside the TIE mess.

Remembering back to TIE
- BP1, drawing vertices without interpolation that appear once
- BP2, drawing vertices without interpolation that appear twice
- IP1, drawing vertices with interpolation that appear one
- IP2, guess what it does

Very strangely, they didn't have a case for "draw IPs, but don't do interpolation". (TFRAG had this, but it also had a totally different interpolation scheme).

They had an absolutely wild scheme to jump between the piplined loops, but we can look at the main loop bodies for simplicity.

ETIE seems to have the same 4 loops (L16, L24, L34, L37).

We can look at BP1 as it's the simplest loop. Hopefully we can derive most stuff from here.
```
L16:
  mtir vi01, vf11.x          |  addw.xy vf20, vf20, vf03
  mfp.x vf30, P              |  add.xy vf21, vf21, vf14
  mtir vi04, vf16.w          |  mulaw.zw ACC, vf10, vf00
  esum.xyzw P, vf13          |  mulz.xyz vf14, vf16, vf22
  lqi.xyz vf12, vi08         |  mul.xyz vf19, vf18, Q
  sqi.xyzw vf20, vi11        |  mul.xyz vf28, vf24, Q
  rsqrt Q, vf02.w, vf30.x    |  mul.xyz vf20, vf20, Q
  lqi.xyzw vf17, vi09        |  maddz.xyzw vf18, vf09, vf17
  lq.xyz vf30, 770(vi01)     |  mulax.xyzw ACC, vf02, vf12
  lqi.xy vf27, vi09          |  madday.xyzw ACC, vf03, vf12
  iadd vi02, vi02, vi12      |  maddz.xyzw vf23, vf04, vf12
  iadd vi06, vi02, vi13      |  mulaw.xyzw ACC, vf08, vf00
  sq.xyzw vf30, 1(vi02)      |  maddax.xyzw ACC, vf05, vf17
  esadd.xyz P, vf14          |  madday.xyzw ACC, vf06, vf17
  mfp.x vf13, P              |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi02)      |  subw.z vf23, vf23, vf00
  sq.xyzw vf20, 0(vi06)      |  maddz.xyz vf17, vf07, vf17
  sq.xyzw vf01, 1(vi06)      |  addw.z vf21, vf00, vf00
  sq.xyzw vf19, 2(vi02)      |  mulx.xy vf22, vf22, vf13
  div Q, vf00.w, vf18.w      |  mul.xy vf21, vf21, Q
  ibeq vi14, vi02, L18       |  mula.xy ACC, vf10, vf16
  sq.xyzw vf19, 2(vi06)      |  mul.xyz vf13, vf17, vf23
  mtir vi01, vf11.y          |  addw.xy vf21, vf21, vf03
  mfp.x vf30, P              |  add.xy vf22, vf22, vf14
  mtir vi05, vf17.w          |  mulaw.zw ACC, vf10, vf00
  esum.xyzw P, vf13          |  mulz.xyz vf14, vf17, vf23
  lqi.xyz vf12, vi08         |  mul.xyz vf19, vf18, Q
  sqi.xyzw vf21, vi11        |  mul.xyz vf28, vf25, Q
  rsqrt Q, vf02.w, vf30.x    |  mul.xyz vf21, vf21, Q
  lq.xyz vf30, 770(vi01)     |  maddz.xyzw vf18, vf09, vf16
  lqi.xyzw vf16, vi09        |  mulax.xyzw ACC, vf02, vf12
  lqi.xy vf24, vi09          |  madday.xyzw ACC, vf03, vf12
  iadd vi03, vi03, vi12      |  maddz.xyzw vf20, vf04, vf12
  iadd vi06, vi03, vi13      |  mulaw.xyzw ACC, vf08, vf00
  sq.xyzw vf30, 1(vi03)      |  maddax.xyzw ACC, vf05, vf16
  esadd.xyz P, vf14          |  madday.xyzw ACC, vf06, vf16
  mfp.x vf13, P              |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi03)      |  subw.z vf20, vf20, vf00
  sq.xyzw vf21, 0(vi06)      |  maddz.xyz vf16, vf07, vf16
  sq.xyzw vf01, 1(vi06)      |  addw.z vf22, vf00, vf00
  sq.xyzw vf19, 2(vi03)      |  mulx.xy vf23, vf23, vf13
  div Q, vf00.w, vf18.w      |  mul.xy vf22, vf22, Q
  ibeq vi14, vi03, L20       |  mula.xy ACC, vf10, vf17
  sq.xyzw vf19, 2(vi06)      |  mul.xyz vf13, vf16, vf20
  mtir vi01, vf11.z          |  addw.xy vf22, vf22, vf03
  mfp.x vf30, P              |  add.xy vf23, vf23, vf14
  mtir vi02, vf16.w          |  mulaw.zw ACC, vf10, vf00
  esum.xyzw P, vf13          |  mulz.xyz vf14, vf16, vf20
  lqi.xyz vf12, vi08         |  mul.xyz vf19, vf18, Q
  sqi.xyzw vf22, vi11        |  mul.xyz vf28, vf26, Q
  rsqrt Q, vf02.w, vf30.x    |  mul.xyz vf22, vf22, Q
  lq.xyz vf30, 770(vi01)     |  maddz.xyzw vf18, vf09, vf17
  lqi.xyzw vf17, vi09        |  mulax.xyzw ACC, vf02, vf12
  lqi.xy vf25, vi09          |  madday.xyzw ACC, vf03, vf12
  iadd vi04, vi04, vi12      |  maddz.xyzw vf21, vf04, vf12
  iadd vi06, vi04, vi13      |  mulaw.xyzw ACC, vf08, vf00
  sq.xyzw vf30, 1(vi04)      |  maddax.xyzw ACC, vf05, vf17
  esadd.xyz P, vf14          |  madday.xyzw ACC, vf06, vf17
  mfp.x vf13, P              |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi04)      |  subw.z vf21, vf21, vf00
  sq.xyzw vf22, 0(vi06)      |  maddz.xyz vf17, vf07, vf17
  sq.xyzw vf01, 1(vi06)      |  addw.z vf23, vf00, vf00
  sq.xyzw vf19, 2(vi04)      |  mulx.xy vf20, vf20, vf13
  div Q, vf00.w, vf18.w      |  mul.xy vf23, vf23, Q
  ibeq vi14, vi04, L22       |  mula.xy ACC, vf10, vf16
  sq.xyzw vf19, 2(vi06)      |  mul.xyz vf13, vf17, vf21
  mtir vi01, vf11.w          |  addw.xy vf23, vf23, vf03
  mfp.x vf30, P              |  add.xy vf20, vf20, vf14
  mtir vi03, vf17.w          |  mulaw.zw ACC, vf10, vf00
  esum.xyzw P, vf13          |  mulz.xyz vf14, vf17, vf21
  lqi.xyz vf12, vi08         |  mul.xyz vf19, vf18, Q
  sqi.xyzw vf23, vi11        |  mul.xyz vf28, vf27, Q
  rsqrt Q, vf02.w, vf30.x    |  mul.xyz vf23, vf23, Q
  lqi.xyzw vf16, vi09        |  maddz.xyzw vf18, vf09, vf16
  lq.xyz vf30, 770(vi01)     |  mulax.xyzw ACC, vf02, vf12
  lqi.xy vf26, vi09          |  madday.xyzw ACC, vf03, vf12
  iadd vi05, vi05, vi12      |  maddz.xyzw vf22, vf04, vf12
  iadd vi06, vi05, vi13      |  mulaw.xyzw ACC, vf08, vf00
  sq.xyzw vf30, 1(vi05)      |  maddax.xyzw ACC, vf05, vf16
  esadd.xyz P, vf14          |  madday.xyzw ACC, vf06, vf16
  mfp.x vf13, P              |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi05)      |  subw.z vf22, vf22, vf00
  sq.xyzw vf23, 0(vi06)      |  maddz.xyz vf16, vf07, vf16
  sq.xyzw vf01, 1(vi06)      |  addw.z vf20, vf00, vf00
  sq.xyzw vf19, 2(vi05)      |  mulx.xy vf21, vf21, vf13
  lqi.xyzw vf11, vi10        |  nop
  div Q, vf00.w, vf18.w      |  mul.xy vf20, vf20, Q
  ibne vi14, vi05, L16       |  mula.xy ACC, vf10, vf17
  sq.xyzw vf19, 2(vi06)      |  mul.xyz vf13, vf16, vf22
```
Dealing with a chunk of VU1 code like this is hard, especially without much context.

The best strategy I have is to start with the store at the end, and work backward. Here's what I did to figure out the envmap math.
Note that I removed instructions related to memory addressing.
```
;; inputs

;; vi08 normal data
;; vi09 point/tex data

;; - vf02, vf03, vf04        rotation
;; - vf05, vf06, vf07, vf08  affine transform
;; - vf09, vf10              perspective

L16:
  mtir vi01, vf11.x          |  addw.xy vf20, vf20, vf03
  mfp.x vf30, P              |  add.xy vf21, vf21, vf14
  mtir vi04, vf16.w          |  mulaw.zw ACC, vf10, vf00
  esum.xyzw P, vf13          |  mulz.xyz vf14, vf16, vf22

  lqi.xyz vf12, vi08         |  ;; LOAD normal
                             |
                             |
  lqi.xyzw vf17, vi09        |  ;; LOAD point
                             |  mulax.xyzw ACC, vf02, vf12    ;; rotate normal
  lqi.xy vf27, vi09          |  madday.xyzw ACC, vf03, vf12   ;; rotate normal
                             |  maddz.xyzw vf23, vf04, vf12   ;; vf23 = rotated normal
                             |  mulaw.xyzw ACC, vf08, vf00    ;; transform point
                             |  maddax.xyzw ACC, vf05, vf17   ;; transform point
                             |  madday.xyzw ACC, vf06, vf17   ;; transform point
                             |
                             |  subw.z vf23, vf23, vf00       ;; nrm.z -= 1.0
                             |  maddz.xyz vf17, vf07, vf17    ;; vf17 = transformed point
                             |
                             |
                             |
                             |
                             |  mul.xyz vf13, vf17, vf23     ;; vf13 = nrm .* ptx
                             |
                             |
                             |
  esum.xyzw P, vf13          |  mulz.xyz vf14, vf17, vf23
                             |
                             |
                             |
                             |
                             |
                             |
                             |
                             |
                             |
  esadd.xyz P, vf14          |
  mfp.x vf13, P              |
                             |
                             |
                             |  addw.z vf22, vf00, vf00
                             |  mulx.xy vf23, vf23, vf13
                             |
                             |  mula.xy ACC, vf10, vf17  ;; acc build 1
                             |  -- 13 BAD
                             |  addw.xy vf22, vf22, vf03
  mfp.x vf30, P              |  add.xy vf23, vf23, vf14
                             |  mulaw.zw ACC, vf10, vf00 ;; acc build 2
                             |  -- 14 BAD
                             |
                             |
  rsqrt Q, vf02.w, vf30.x    |
                             |  maddz.xyzw vf18, vf09, vf17 ;; acc star
                             |
                             |
                             |
                             |
                             |
                             |
                             |
                             |  subw.z vf21, vf21, vf00
                             |
                             |  addw.z vf23, vf00, vf00
                             |
  div Q, vf00.w, vf18.w      |  mul.xy vf23, vf23, Q
                             |
                             |
                             |  addw.xy vf23, vf23, vf03
                             |  add.xy vf20, vf20, vf14
                             |
                             |
                             |  mul.xyz vf19, vf18, Q
  sqi.xyzw vf23, vi11        |  mul.xyz vf28, vf27, Q
                             |  mul.xyz vf23, vf23, Q
                             |
                             |
                             |
                             |
                             |
  sq.xyzw vf30, 1(vi05)      |
  esadd.xyz P, vf14          |
  mfp.x vf13, P              |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi05)      |
  sq.xyzw vf23, 0(vi06)      |
  sq.xyzw vf01, 1(vi06)      |
  sq.xyzw vf19, 2(vi05)      |
                             |
                             |
                             |
  sq.xyzw vf19, 2(vi06)      |
```

These are the stores. Each vertex has 3 QW (texture coords, rgba, xyz)
```
;; STORES
sq.xyzw vf28, 0(vi05)
sq.xyzw vf23, 0(vi06)

sq.xyzw vf30, 1(vi05)
sq.xyzw vf01, 1(vi06)

;; xyz of first and second draw are the same, nice
sq.xyzw vf19, 2(vi05)
sq.xyzw vf19, 2(vi06)
```

### Trace back `vf19` first (the vertex position):
```
;; load the vertex position in world space
lqi.xyzw vf17, vi09

;; apply affine transform stored in matrix vf05, vf06, vf07, vf08
;; (likely transforms from world to camera space, no projection)
mulaw.xyzw ACC, vf08, vf00
maddax.xyzw ACC, vf05, vf17
madday.xyzw ACC, vf06, vf17
maddz.xyz vf17, vf07, vf17

;; at this point vf17 contains the point relative to the camera.

;; apply perspective (using vf09/vf10 as the perspective coefficients)
mula.xy ACC, vf10, vf17
mulaw.zw ACC, vf10, vf00
maddz.xyzw vf18, vf09, vf17

;; perspective divide
div Q, vf00.w, vf18.w
mul.xyz vf19, vf18, Q

;; convert to int for the GS format
ftoi4.xyz vf19, vf19

;; store
sq.xyzw vf19, 2(vi05)
sq.xyzw vf19, 2(vi06)
```
This is relatively uninteresting - the only difference is that `hvdf-offset` is not applied in the usual way. But, in `etie-vu1`, we see that `hvdf-off` is invovled in computing the perspective coefficients (likely vf08, vf09), so it still enters in. This is different from normal, but it makes some sense - there's an intermediate result of `vf17` being the point relative to the camera, which will be useful to compute the reflection direction for environment mapping.

### Trace back `vf01` (the color, I think of the envmap):
```
;; this comes from outside the loop. It's a constant!
;; note that vi10 comes from the result of xtop, so it's the input from the EE asm function.
;; (which we don't have a type for, darn)
lq.xyzw vf01, 7(vi10)
```
leave this for now, we'll have to dig through the EE asm later

### Trace back `vf30` (the color of the base draw):
```
;; load from input (it's the address of interpolated color)
lqi.xyzw vf11, vi10
;; load interpolated color
mtir vi01, vf11.w
;; store interpolated color
lq.xyz vf30, 770(vi01)
```
This is basically the same as normal TIE (there's some additional logic because the 4 vertices use the x, y, z, w components of the `vf11`, which is only reloaded once per 4). The existing `extract_tie` has the color_index_index nonsense.

The difference is that alpha is set from earlier, and constant:
```
;; outside loop
lq.w vf30, 6(vi10)
```
This is held constant for an entire fragment and computed on the EE. It's likely an envmap intensity scale used to fade it out.



### Trace back vf28, texture coords of first draw

```
lqi.xy vf27, vi09
mul.xyz vf28, vf27, Q ;; Q is the perspective divisor
```
Kind of as expected, this just loads and multiplies (required for GS perspective-correct tex).

### Trace back `vf23`: the second draws texture coordinates
I'd expect this to compute:
- place a plane with normal parallel to the vertex normal, intersecting the vertex
- place a vector from the camera to the vertex
- reflect this vertex
- somehow map this direction onto the envmap texture


```
;; reg names:
;; vf13 dot
;; vf14 rfl
;; vf17 pos
;; vf23 nrm


;; load the normal and point.
lqi.xyz vf12, vi08
lqi.xyzw vf17, vi09

;; rotate the normal
mulax.xyzw ACC, vf02, vf12    ;; rotate normal
madday.xyzw ACC, vf03, vf12   ;; rotate normal
maddz.xyzw vf23, vf04, vf12   ;; vf23 = rotated normal

;; transform the point
mulaw.xyzw ACC, vf08, vf00    ;; transform point
maddax.xyzw ACC, vf05, vf17   ;; transform point
madday.xyzw ACC, vf06, vf17   ;; transform point
maddz.xyz vf17, vf07, vf17    ;; vf17 = transformed point

;; nrm.z -= 1
subw.z vf23, vf23, vf00

;; dot = nrm.xyz * pt.xyz
mul.xyz vf13, vf17, vf23
esum.xyzw P, vf13
mfp.x vf13, P

;; rfl = pt.xzy * nrm.z
mulz.xyz vf14, vf17, vf23
;; Q_envmap = vf02.w / norm(rfl.xyz)
esadd.xyz P, vf14
mfp.x vf30, P
rsqrt Q, vf02.w, vf30.x

;; nrm.xy *= dot.x
mulx.xy vf23, vf23, vf13

;; perspective transform
mula.xy ACC, vf10, vf17  ;; acc build 1
mulaw.zw ACC, vf10, vf00 ;; acc build 2
maddz.xyzw vf18, vf09, vf17 ;; acc star

;; nrm.xy += rfl.xy
add.xy vf23, vf23, vf14

;; nrm.z = 1.0
addw.z vf23, vf00, vf00

;; nrm.xy *= Q_envmap
mul.xy vf23, vf23, Q

;; perspective divide
div Q, vf00.w, vf18.w

;; nrm.xy += vf03.w
addw.xy vf23, vf23, vf03

;; that's the non-persp_Q multiplied envmap value.
```

### Mysterious `sqi.xyzw vf23, vi11`
Highly observant readers will have noticed this instruction, which stashes the texture coordinates (before perspective divide) to some address. These are picked up later on in the IP drawing routines.

### Summary

Main results:
- transformation is like normal, but computes point in camera space (no perspect) first.
- perspective transformation is expressed differently, but same math in the end
- second draw rgba computed on EE (constant per frag)
- first draw alpha computed on EE (constant per frag)
- first draw tex coord is usual.
- we got the math for envmapping! (at least for BP1 drawing)
- the normals are uploaded to VU (at least for BP1 drawing)

And that all for today (about 4.5 hours)

## Where do those stupid normals come from?

The `vi08` is the pointer to the normals, initialized to `0x85 = 133`
```
iaddiu vi08, vi00, 0x85 ;; normals start at 0x85
```
the fixed address here is a super awesome clue.

Looking in `tie-work`
```
;; in *prototype-tie-work*
;; (upload-envmap-3            dma-packet   :inline :offset-assert 256)
:upload-envmap-3 (new 'static 'dma-packet
  :dma (new 'static 'dma-tag :id (dma-tag-id ref))
  :vif1 (new 'static 'vif-tag :imm #x84 :cmd (vif-cmd unpack-v4-8))
  )
```
which is uploading some data to an offset of `0x84`, from int8's. The `int8` is promising because 8-bits seems reasonable for normals on a ps2 game.

There's also:
```
(deftype generic-tie-normal (structure)
  ((x     int8  :offset-assert 0)
   (y     int8  :offset-assert 1)
   (z     int8  :offset-assert 2)
   (dummy int8  :offset-assert 3)
   )
  )
```
which is vaguely promising that TIE envmap has 8-bit normals.

Uploading normals is done per-model, so likely in `draw-inline-array-prototype-tie-asm`.

We're looking for a place where they use the `upload-envmap-3` DMA template. They will modify this template, then store it in a scratchpad buffer (later DMA'd to the DMA buffer, which is later DMA'd to VIF1 for unpack)
```
(deftype dma-tag (uint64)
  ((qwc uint16    :offset 0)           ;; quadword count
   (pce uint8     :offset 26 :size 2)  ;; priority (source mode)
   (id dma-tag-id :offset 28 :size 3)  ;; ID (what the tag means)
   (irq uint8     :offset 31 :size 1)  ;; interrupt at the end?
   (addr uint32   :offset 32 :size 31) ;; address (31 bits)
   (spr uint8     :offset 63 :size 1)  ;; spr or not flag.
   )
  )

(deftype vif-unpack-imm (uint16)
  ((addr  uint16 :offset 0  :size 10)
   (usn   uint8  :offset 14 :size 1)
   (flg   uint8  :offset 15 :size 1)
   )
  )

(deftype vif-tag (uint32)
  ((imm uint16  :offset 0 :size 16)
   (num uint8   :offset 16 :size 8)
   (cmd vif-cmd :offset 24 :size 7)
   (irq uint8   :offset 31 :size 1)
   (msk uint8   :offset 28 :size 1)
   )
  )
```

Here is this part (found that `s0` is the `prototype-work-structure`, searched for 256 offset). Many unrelated instructions removed:
```
    ;; store some qwc value into the dma-tag's qwc (number of qw read from EE)
    sh ra, 256(s0)
    ;; multiply qwc by 4 (to get the number of qw written on VU)
    dsll ra, ra, 2
    ;; store some source pointer in the addr field of the dma tag
    sw a2, 260(s0)
    ;; store the unpack num
    sb ra, 270(s0)
    ;; load the entire template
    lq s5, 256(s0)
    ;; store the entire template
    sq s5, 48(a3)
```

Tracing back:
```
lhu ra, 50(t6)
lw a2, 52(t6)
```
which I'm guessing is:
```
(deftype tie-fragment (drawable)
  ((gif-ref       (inline-array adgif-shader)  :offset 4)
   (point-ref     uint32                       :offset 8)
   ...
   (normal-count  uint16                       :offset 54)
   (normal-ref    uint32                       :offset 56) ;; <- NORMALS!
```
so `normal-count` looks like QW of normals on the EE.

Let's try grabbing this data in the extraction tool:
```cpp
  if (version > GameVersion::Jak1) {
    u16 normals_qwc = read_plain_data_field<u16>(ref, "normal-count", dts);
    if (normals_qwc) {
      normals.resize(16 * normals_qwc);
      auto normals_data_ref = deref_label(get_field_ref(ref, "normal-ref", dts));
      memcpy_plain_data((u8*)normals.data(), normals_data_ref, normals_qwc * 16);
      // print them for debug
```

This looks very reasonable!
```
Normals:
-41 102 62 0
-47 102 58 0
-41 102 62 0
-47 102 58 0
-53 102 53 0
-53 102 53 0
...
```
Some promising patterns:
- `w` component is 0. I don't see it used in the VU code, and generic tie calls it "dummy".
- Normals in a fragment are similar-ish
- The length of a few normals I spot-checked is about 128
- There's some fragments where the Y normals are all 0 (vertical surfaces), which seems like a common thing.

Now the challenge is matching up these normals with the points in the mesh. Some observations:
- The 0x84/0x85 offset mismatch doesn't matter - 0x84 is the right number. There's a `lq.xyz vf12, 132(vi00)` (0x84) for the first point.
- I don't see any magic flags at the end of the normals. I was half expecting some confusing table of indices at the end, but it doesn't look like it.

Passing this through the `extract_tie.cpp` maze:
```cpp
// normals
const auto& normal_data = proto.geometry[geo].tie_fragments[frag_idx].normals;
frag_info.normal_data_packed.resize(normal_data.size() / 4);
for (size_t ni = 0; ni < normal_data.size() / 4; ni++) {
  for (int j = 0; j < 4; j++) {
    frag_info.normal_data_packed[ni][j] = normal_data[ni * 4 + j];
  }
}
```

Doing some debug, we find something amazing: They just stored one normal for each unique vertex! This will be super easy to extract.

I thought this wasn't the case because they were stashing some texture coordinates related to envmap and reusing them later. But I now realize this might be just needed for interpolating from a base point. (you want to the interp before applying perspective correct texturing multiply).

So now I will try to get the normals into the FR3 mesh, and see if we can draw them.

The basic idea is:

when processing vertices (per proto), add a line like this (this goes in draw order)
```cpp
vertex_info.nrm = frag.get_normal_if_present(normal_table_offset++);
```

Expand the on-disk FR3 format to store a normal per tie proto vertex, and populate this from vertex info above when converting formats.
```cpp
struct PackedTieVertices {
  struct Vertex {
    float x, y, z;
    float s, t;
    s8 nx, ny, nz; // added
  };
```

When the TIE mesh is loaded, it is de-instanced, so we need to rotate the normals (and convert to opengl format). The speed of this operation isn't important because it happens at load time (in the loader thread too):
```cpp
math::Vector<s16, 3> unpack_tie_normal(const std::array<math::Vector4f, 4>& mat,
                                       s8 nx,
                                       s8 ny,
                                       s8 nz) {
  // rotate the normal
  math::Vector3f nrm = math::Vector3f::zero();
  nrm += mat[0].xyz() * nx;
  nrm += mat[1].xyz() * ny;
  nrm += mat[2].xyz() * nz;
  // convert to s16 for OpenGL renderer
  nrm.normalize(INT16_MAX - 2);
  return nrm.cast<s16>();
}
```

### Drawing envmap
Next I refactored TIE3 to do a second draw pass when doing an envmap category, with a separate shader.

I passed the normals through, and used them as RGB. I'd expect to see horizontal surface (+y normal) be green, and the usual "smooth rainbow on curves" effect. Which I did! It worked on the first try.

PICTURE

### Envmap Math Start
The envmap math computes both the vertex location and the reflection. We can start with the vertex location because that's super easy to verify.

I needed to add two new matrices to `add-pc-tfrag3-data` so the GOAL code sends the renderer the required information.

These are used to compute `persp0`/`persp1`, which can then be used for perspective projection of the vertex position
```cpp
  math::Vector4f perspective[2];
  float inv_fog = 1.f / render_state->camera_fog[0];
  auto& hvdf_off = render_state->camera_hvdf_off;
  float pxx = render_state->camera_persp[0].x();
  float pyy = render_state->camera_persp[1].y();
  float pzz = render_state->camera_persp[2].z();
  float pzw = render_state->camera_persp[2].w();
  float pwz = render_state->camera_persp[3].z();
  float scale = pzw * inv_fog;
  perspective[0].x() = scale * hvdf_off.x();
  perspective[0].y() = scale * hvdf_off.y();
  perspective[0].z() = scale * hvdf_off.z() + pzz;
  perspective[0].w() = scale;

  perspective[1].x() = pxx;
  perspective[1].y() = pyy;
  perspective[1].z() = pwz;
  perspective[1].w() = 0;

  set_uniform(m_etie_uniforms.persp0, perspective[0]);
  set_uniform(m_etie_uniforms.persp1, perspective[1]);
```

I wrote a shader for just vertex position (no reflection) to test this:
```cpp
vec4 vf17 = cam_no_persp[3];
vf17 += cam_no_persp[0] * position_in.x;
vf17 += cam_no_persp[1] * position_in.y;
vf17 += cam_no_persp[2] * position_in.z;

//;; perspective transform
//mula.xy ACC, vf10, vf17  ;; acc build 1
//mulaw.zw ACC, vf10, vf00 ;; acc build 2
vec4 p_proj = vec4(persp1.x * vf17.x, persp1.y * vf17.y, persp1.z, persp1.w);
//maddz.xyzw vf18, vf09, vf17 ;; acc star
p_proj += persp0 * vf17.z;

//;; perspective divide
//div Q, vf00.w, vf18.w
float pQ = 1.f / p_proj.w;

vec4 transformed = p_proj * pQ;

// usual rest of the stuff
```
and it worked, but brought up a really annoying problem - the math is different by a tiny amount (floating point rounding, an impossible to see difference), so sometimes the depth test fails when drawing over something.

So, we have to refactor TIE to use the similar projection math.

After that, things aren't a flickerly/fighting mess.

In theory, we could see fighting in between ETIE and TIE geometry, but I don't see that yet.

### Envmap Math 2

I tried implementing the envmap math, and it seems slightly wrong.

```cpp
    // nrm.z -= 1
    //subw.z vf23, vf23, vf00
    nrm_vf23.z -= 1.f;

    // dot = nrm.xyz * pt.xyz
    //mul.xyz vf13, vf17, vf23
    //esum.xyzw P, vf13
    //mfp.x vf13, P
    float nrm_dot = dot(vf17.xyz, nrm_vf23);

    // rfl = pt.xzy * nrm.z
    //mulz.xyz vf14, vf17, vf23
    vec3 rfl_vf14 = vf17.xyz * nrm_vf23.z;

    //;; Q_envmap = vf02.w / norm(rfl.xyz)
    //esadd.xyz P, vf14
    //mfp.x vf30, P
    //rsqrt Q, vf02.w, vf30.x
    float Q_envmap = -0.5 / length(rfl_vf14);

    //
    //;; nrm.xy *= dot.x
    //mulx.xy vf23, vf23, vf13
    nrm_vf23.xy *= nrm_dot;
    //;; perspective transform
    //mula.xy ACC, vf10, vf17  ;; acc build 1
    //mulaw.zw ACC, vf10, vf00 ;; acc build 2
    vec4 p_proj = vec4(persp1.x * vf17.x, persp1.y * vf17.y, persp1.z, persp1.w);
    //maddz.xyzw vf18, vf09, vf17 ;; acc star
    p_proj += persp0 * vf17.z;

    //
    //;; nrm.xy += rfl.xy
    //add.xy vf23, vf23, vf14
    nrm_vf23.xy += rfl_vf14.xy;
    //
    //;; nrm.z = 1.0
    //addw.z vf23, vf00, vf00
    nrm_vf23.z = 1.0;
    //
    //;; nrm.xy *= Q_envmap
    //mul.xy vf23, vf23, Q
    nrm_vf23.xy *= Q_envmap;
    //
    //;; perspective divide
    //div Q, vf00.w, vf18.w
    float pQ = 1.f / p_proj.w;
    //
    //;; nrm.xy += vf03.w
    //addw.xy vf23, vf23, vf03
    nrm_vf23.xy += 0.5;
    tex_coord = nrm_vf23;
```

after staring at it for a while, I didn't see any bugs, so I became suspicious of my rotation matrix:
- I assume this matrix is just `cam_R_world`
- In the game, they upload separate "rotate normal" and "transform point" matrices. I know I have transform point right. I assumed that "rotate normal" is just the upper 3x3 of "transform point"... but why would they upload separate matrices if that was the case?

Now we have to track down how this matrix is built.

This is the VU program that uses the matrix to rotate the normal:
```
mulax.xyzw ACC, vf02, vf12
madday.xyzw ACC, vf03, vf12
maddz.xyzw vf23, vf04, vf12
```

This is where the matrix is loaded:
```
lq.xyzw vf02, 4(vi10)
lq.xyzw vf03, 5(vi10)
lq.xyzw vf04, 6(vi10)
```
where `vi10` is the register set from `xtop` (the per-instance data). At offsets 0, 1, 2, 3 are the tranformation matrix.

Blindly guessing didn't make much progress... time to dive into `draw-inline-array-instance-tie`'s envmap path.

At entry:
- `t0` is `instance-tie-work`
- `t8` is matrix
- `t9` is bucket

Let's go block by block:
```
L299:
    beq s4, r0, L288
    lw s3, 532(t0) ;; (-> itw use-etie)
```
Not sure what the branch checks. Jumps back to where we came from.

```
B57:
    vcallms 29
```
Run VU0 microprogram. I think the `background-vu` block is loaded, so:
```
  lq.xyzw vf24, 4(vi00)      |  nop
  lq.xyzw vf25, 5(vi00)      |  nop
  lq.xyzw vf26, 6(vi00)      |  nop :e
  lq.xyzw vf27, 7(vi00)      |  nop
```
these registers are, I think, set from `set-background-regs!`:
```
          (.lvf vf24 (&-> v1-0 camera-rot quad 0))
          (.lvf vf25 (&-> v1-0 camera-rot quad 1))
          (.lvf vf26 (&-> v1-0 camera-rot quad 2))
          (.lvf vf27 (&-> v1-0 camera-rot trans quad))
```


Back to the EE program:
```
    sw s5, 512(t0)    ;; set flags, who care
    beq s3, s7, L308  ;; branch somewhere if use-etie if #f, who cares
    lw gp, 108(t0)    ;; dist-test.w, who knows?

    lw s5, 84(t8)     ;; see note below
    sll r0, r0, 0
    sqc2 vf5, 448(t0) ;; set fog temp, no idea.
    bne s5, r0, L302  ;; if we have envmap-mid's go to L302
    lw s5, 104(t0)    ;; another dist-test.z
```
I think earlier code stashed a `guard-flag` at `84(t8)` (maybe). Either way not super important because whether we take this branch doesn't change much in the end...


(reordered, removed nops)
```
L300:
    ;; increment count in the bucket for envmap-mid-count
    lh gp, 120(t9)
    daddiu gp, gp, 1
    sh gp, 120(t9)

    ;; something to set up the dma chain
    lw s5, 84(t9)                  ;; envmap-mid-next
    addiu ra, ra, 128
    sw ra, 84(t9)

    ;; morph temp
    sqc2 vf29, 432(t0)

    ;; matrix multiplies... looks like camera rot being used to rotate.
    vmulax.xyzw acc, vf24, vf10
    vmadday.xyzw acc, vf25, vf10
    vmaddz.xyzw vf16, vf26, vf10

    vmulax.xyzw acc, vf24, vf11
    vmadday.xyzw acc, vf25, vf11
    vmaddz.xyzw vf17, vf26, vf11

    ;; count stuff
    lbu s3, 134(t9)
    lhu gp, 144(t9)
    lbu s4, 138(t9)

    beq r0, r0, L303
    sll r0, r0, 0
```
so we're mid way through rotating a matrix vf10, vf11, vf12, vf13.

```
    ;; rest of the mult...
    vmulax.xyzw acc, vf24, vf12
    vmadday.xyzw acc, vf25, vf12
    vmaddz.xyzw vf18, vf26, vf12

    ;; this one's different
    vmulax.xyzw acc, vf24, vf13
    vmadday.xyzw acc, vf25, vf13
    vmaddaz.xyzw acc, vf26, vf13
    vmaddw.xyzw vf19, vf27, vf0
```
weird. no idea what the `vmaddw` is adding, but I am 99% sure it won't be part of the rotation matrix, so ignore for now!

```
    lq s2, 224(t0) ;; upload-color-2 tmpl
    lq ra, 240(t0) ;; upload-color-ret tmpl
```
These DMA templates I don't care about.

```
    dsll gp, gp, 4
    daddu s4, s4, t9
    mfc1 s1, f15
    addiu t4, t4, 8
    qmtc2.i vf14, s1
```
no idea, doesn't look important.

I think, at `t8`, we're now building:
```
(deftype etie-matrix (structure)
  ((rmtx  matrix  :inline :offset-assert 0)
   (nmtx  matrix3 :inline :offset-assert 64)
   (morph float           :offset 76)
   (fog   float           :offset 92)
   (fade  uint32          :offset 108)
   (tint  qword   :inline :offset-assert 112)
   )
  :method-count-assert 9
  :size-assert         #x80
  :flag-assert         #x900000080
  )
```
Whwere `rmtx` is the camera matrix not including perspective, and `nmtx` is the thing I need to figure out. The multiplies above seemed to compute `rmtx` in vf16, 20.


```
    sqc2 vf16, 0(t8) ;; first matrix store?
    sqc2 vf17, 16(t8)
    sqc2 vf18, 32(t8)
    sqc2 vf19, 48(t8)
```
and, as expected, they do!

(regs at this point, it's getting confusing):
```
vf16, vf17, vf18, vf19: rmtx (includes translation, but no perspective)
vf10, vf11, vf13, vf14: tie instance matrix
```

Here is the extracted part of this math.
```
    vmulx.xyz vf16, vf10, vf14

    vopmula.xyz acc, vf11, vf16

    vopmsub.xyz vf17, vf16, vf11

    vopmula.xyz acc, vf16, vf17

    vopmsub.xyz vf17, vf17, vf16

    vmul.xyz vf14, vf17, vf17

    vmulax.w acc, vf0, vf14
    vmadday.w acc, vf0, vf14
    vmaddz.w vf14, vf0, vf14
    vrsqrt Q, vf0.w, vf14.w

    vmulax.xyzw acc, vf24, vf16
    vmadday.xyzw acc, vf25, vf16
    vmaddz.xyzw vf10, vf26, vf16

    vwaitq
    vmulq.xyz vf17, vf17, Q

    vopmula.xyz acc, vf16, vf17
    vopmsub.xyz vf18, vf17, vf16

    vmulax.xyzw acc, vf24, vf17
    vmadday.xyzw acc, vf25, vf17
    vmaddz.xyzw vf11, vf26, vf17

    vmulax.xyzw acc, vf24, vf18
    vmadday.xyzw acc, vf25, vf18
    vmaddz.xyzw vf12, vf26, vf18

    sqc2 vf10, -112(t8)
    sqc2 vf11, -96(t8)
    sqc2 vf12, -80(t8)
```

which just ended up computing the inverse transpose. I tried my own implementation, and their implementation, and same problems.

## Frustration
At this point, I didn't take good notes for a few hours. The summary is:
- I thought things were still wrong
- I implemented emerc's math in the etie shader
- Realized we were using the wrong envmap textures.
- With the right textures, things looked better
- still some uncertainty with the length of normals.
- A few possible options:
  - using emerc, which doesn't care about normal length
  - use etie, normalize normals
  - use etie, don't normalize, assume magic number in TIE EE asm was 1.0

For now I'm using `emerc`, but it's easy to switch. I think once I get everything else working properly, it will be easier to compare them.

## Tint
The envmap has some color applied. Right now, I put some constant white, but it's clearly not right. In the VU program, it just copies the color from the input. I found a block of code that looks promising for the color:

```
lw t9, 168(t9)             ;; tint color from bucket (4x u8's)
pextlb t9, t9, r0          ;; unpack tint from bucket to u16's
dsll s2, s2, 5             ;; ?? maybe fade or something
pextlh t9, r0, t9          ;; unpack tint from bucket to u32's
qmtc2.i vf9, s2            ;; ??
psraw t9, t9, 3            ;; shift bucket tint
sll r0, r0, 0
qmtc2.i vf14, t9           ;; bucket tint
sll r0, r0, 0
lqc2 vf15, 416(t0)         ;; envmap-tod from instance-tie-work
vitof12.xyzw vf9, vf9      ;; bucket to float
sll r0, r0, 0
vitof12.xyzw vf14, vf14    ;; ?? to float
sll r0, r0, 0
vmul.xyzw vf15, vf15, vf14  ;; multiply by envmap
sll r0, r0, 0
vmulx.xyzw vf15, vf15, vf9 ;; scale by ??
```
I'm assuming the `??` bit is to fade it out (would also explain why this is done per-instance). But we need `envmap-tod`...

Which is set in normal GOAL code for once:
```
(set! (-> *instance-tie-work* tod-env-color quad) (-> *time-of-day-context* current-env-color quad))
```

I ended up sneaking the tint color into the actual vertices (a bit wasteful, but I had a spare 4 bytes), then setting the time of day color through a uniform.


## Final results on the normal thing

I'm pretty sure the unknown normal matrix scaling factor is designed to give normals the right length, even if the instance matrix isn't a pure rotation.

EMERC (built-in normalization) and ETIE-with-add-normalization are idential.

Cheating the normal length makes things look obviously worse/wrong.