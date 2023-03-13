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

# Getting the envmap shader data into the FR3
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


```
  b L14                      |  nop
  nop                        |  nop
  b L2                       |  nop
  nop                        |  nop
  b L2                       |  nop
  nop                        |  nop
  b L3                       |  nop
  isw.z vi00, 914(vi00)      |  nop
  b L1                       |  nop
  nop                        |  nop
  b L9                       |  nop
  nop                        |  nop
L1:
  isw.w vi00, 914(vi00)      |  nop :e
  isw.z vi00, 915(vi00)      |  nop
L2:
  nop                        |  nop :e
  nop                        |  nop
L3:
  bal vi15, L4               |  nop
  nop                        |  nop
  ilw.w vi01, 914(vi00)      |  nop
  nop                        |  nop
  nop                        |  nop
  nop                        |  nop
  iaddi vi01, vi01, 0x1      |  nop
  isw.w vi01, 914(vi00)      |  nop
  nop                        |  nop :e
  nop                        |  nop
L4:
  ilw.w vi12, 898(vi00)      |  nop
  lq.xyzw vf17, 903(vi00)    |  nop
  lq.xyzw vf18, 904(vi00)    |  nop
  lq.xyzw vf19, 905(vi00)    |  nop
  lq.xyzw vf20, 906(vi00)    |  nop
  lq.xyzw vf21, 907(vi00)    |  nop
  iaddi vi04, vi00, 0x0      |  nop
  lq.xyz vf11, 899(vi00)     |  nop
  ilwr.w vi05, vi04          |  nop
  ilw.w vi07, 1(vi04)        |  nop
  ilw.w vi13, 2(vi04)        |  nop
  lq.xyzw vf24, 900(vi00)    |  nop
  lqi.xyzw vf12, vi04        |  nop
  lqi.xyzw vf13, vi04        |  nop
  lqi.xyzw vf14, vi04        |  nop
  lqi.xyzw vf15, vi04        |  nop
  lqi.xyzw vf16, vi04        |  subw.w vf11, vf11, vf11
  iadd vi05, vi05, vi12      |  nop
  iadd vi06, vi05, vi13      |  nop
  iaddi vi01, vi00, 0x6      |  nop
  sq.xyzw vf11, -1(vi05)     |  nop
  isw.x vi01, -1(vi05)       |  nop
  sqi.xyzw vf12, vi05        |  nop
  sqi.xyzw vf13, vi05        |  nop
  sqi.xyzw vf14, vi05        |  nop
  sqi.xyzw vf15, vi05        |  nop
  sqi.xyzw vf16, vi05        |  nop
  b L6                       |  nop
  sqi.xyzw vf24, vi05        |  nop
L5:
  iadd vi05, vi05, vi12      |  nop
  iadd vi06, vi05, vi13      |  nop
  sqi.xyzw vf11, vi05        |  nop
  sqi.xyzw vf12, vi05        |  nop
  sqi.xyzw vf13, vi05        |  nop
  sqi.xyzw vf14, vi05        |  nop
  sqi.xyzw vf15, vi05        |  nop
  sqi.xyzw vf16, vi05        |  nop
L6:
  sqi.xyzw vf11, vi06        |  nop
  sqi.xyzw vf17, vi06        |  nop
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
  lqi.xyzw vf16, vi04        |  nop
  mtir vi07, vf12.w          |  nop
  sq.xy vf12, 914(vi00)      |  nop
  sq.xyzw vf13, 913(vi00)    |  nop
  sq.xy vf14, 915(vi00)      |  nop
  iaddi vi04, vi04, -0x2     |  subw.w vf22, vf00, vf00
  ilwr.x vi08, vi04          |  subw.w vf23, vf00, vf00
  ilwr.y vi09, vi04          |  nop
  ilwr.z vi05, vi04          |  nop
  iaddi vi07, vi07, -0x1     |  nop
  iaddi vi04, vi04, 0x1      |  nop
  lq.xyz vf22, 901(vi09)     |  nop
  ibeq vi00, vi07, L8        |  nop
  lq.xyz vf23, 902(vi09)     |  nop
L7:
  iadd vi05, vi05, vi12      |  nop
  iadd vi06, vi05, vi13      |  nop
  iaddi vi07, vi07, -0x1     |  nop
  sq.xyzw vf22, 0(vi05)      |  nop
  iswr.x vi08, vi05          |  nop
  sq.xyzw vf23, 0(vi06)      |  nop
  iswr.x vi08, vi06          |  nop
  ilwr.x vi08, vi04          |  nop
  ilwr.y vi09, vi04          |  nop
  ilwr.z vi05, vi04          |  nop
  iaddi vi04, vi04, 0x1      |  nop
  ibne vi00, vi07, L7        |  nop
  lq.xyz vf22, 901(vi09)     |  nop
L8:
  iadd vi05, vi05, vi12      |  nop
  iadd vi06, vi05, vi13      |  nop
  sq.xyzw vf22, 0(vi05)      |  nop
  iswr.x vi08, vi05          |  nop
  iaddiu vi08, vi08, 0x4000  |  nop
  iaddiu vi08, vi08, 0x4000  |  nop
  sq.xyzw vf23, 0(vi06)      |  nop
  jr vi15                    |  nop
  iswr.x vi08, vi06          |  nop
L9:
  iaddiu vi05, vi00, 0x84    |  nop
  0.0078125                  |  nop :i
  lqi.xyzw vf20, vi05        |  addi.x vf23, vf00, I
  ilw.x vi01, 915(vi00)      |  addw.z vf17, vf00, vf00
  lq.xyzw vf14, 32(vi00)     |  addw.z vf18, vf00, vf00
  lq.xyw vf17, 33(vi00)      |  addw.z vf19, vf00, vf00
  iaddiu vi03, vi00, 0x22    |  itof0.xyzw vf20, vf20
  iaddiu vi01, vi01, 0x20    |  itof0.xyz vf14, vf14
  lqi.xyzw vf15, vi03        |  itof12.xy vf17, vf17
  lqi.xyw vf18, vi03         |  nop
  lqi.xyzw vf21, vi05        |  nop
  64.0                       |  mulx.xyzw vf20, vf20, vf23 :i
  ibeq vi01, vi03, L11       |  muli.xyz vf14, vf14, I
  ilw.y vi02, 915(vi00)      |  itof0.xyz vf15, vf15
L10:
  lqi.xyzw vf22, vi05        |  itof0.xyzw vf21, vf21
  lqi.xyzw vf16, vi03        |  itof12.xy vf18, vf18
  lqi.xyw vf19, vi03         |  nop
  sq.xyzw vf17, -5(vi03)     |  nop
  sq.xyzw vf20, -3(vi05)     |  mulx.xyzw vf21, vf21, vf23
  ibeq vi01, vi03, L11       |  muli.xyz vf15, vf15, I
  sq.xyzw vf14, -6(vi03)     |  itof0.xyz vf16, vf16
  lqi.xyzw vf20, vi05        |  itof0.xyzw vf22, vf22
  lqi.xyzw vf14, vi03        |  itof12.xy vf19, vf19
  lqi.xyw vf17, vi03         |  nop
  sq.xyzw vf18, -5(vi03)     |  nop
  sq.xyzw vf21, -3(vi05)     |  mulx.xyzw vf22, vf22, vf23
  ibeq vi01, vi03, L11       |  muli.xyz vf16, vf16, I
  sq.xyzw vf15, -6(vi03)     |  itof0.xyz vf14, vf14
  lqi.xyzw vf21, vi05        |  itof0.xyzw vf20, vf20
  lqi.xyzw vf15, vi03        |  itof12.xy vf17, vf17
  lqi.xyw vf18, vi03         |  nop
  sq.xyzw vf19, -5(vi03)     |  nop
  sq.xyzw vf22, -3(vi05)     |  mulx.xyzw vf20, vf20, vf23
  ibne vi01, vi03, L10       |  muli.xyz vf14, vf14, I
  sq.xyzw vf16, -6(vi03)     |  itof0.xyz vf15, vf15
L11:
  iaddi vi05, vi05, -0x2     |  nop
  lq.xyzw vf11, -4(vi03)     |  nop
  lq.xyzw vf14, -3(vi03)     |  nop
  lq.xyzw vf17, -2(vi03)     |  nop
  lqi.xyzw vf20, vi05        |  nop
  iadd vi02, vi02, vi03      |  nop
  iaddi vi02, vi02, -0x4     |  nop
  iaddi vi03, vi03, -0x1     |  nop
  iaddi vi04, vi03, -0x3     |  nop
  ibeq vi02, vi03, L13       |  itof0.xyzw vf20, vf20
  nop                        |  itof0.xyzw vf11, vf11
  lqi.xyzw vf12, vi03        |  itof0.xyz vf14, vf14
  lqi.xyzw vf15, vi03        |  nop
  lqi.xyzw vf18, vi03        |  itof12.xy vf17, vf17
  lqi.xyzw vf21, vi05        |  mulx.xyzw vf20, vf20, vf23
  nop                        |  muli.xyz vf11, vf11, I
  ibeq vi02, vi03, L13       |  muli.xyz vf14, vf14, I
  nop                        |  itof0.xyzw vf12, vf12
  nop                        |  itof0.xyzw vf21, vf21
L12:
  lqi.xyzw vf13, vi03        |  itof0.xyz vf15, vf15
  lqi.xyzw vf16, vi03        |  nop
  lqi.xyzw vf19, vi03        |  itof12.xy vf18, vf18
  lqi.xyzw vf22, vi05        |  mulx.xyzw vf21, vf21, vf23
  sq.w vf17, 1(vi04)         |  nop
  sq.w vf14, 2(vi04)         |  nop
  sqi.xyzw vf11, vi04        |  nop
  sqi.xyz vf14, vi04         |  muli.xyz vf12, vf12, I
  sq.xyzw vf20, -3(vi05)     |  itof0.xyzw vf22, vf22
  ibeq vi02, vi03, L13       |  muli.xyz vf15, vf15, I
  sqi.xyz vf17, vi04         |  itof0.xyzw vf13, vf13
  lqi.xyzw vf11, vi03        |  itof0.xyz vf16, vf16
  lqi.xyzw vf14, vi03        |  nop
  lqi.xyzw vf17, vi03        |  itof12.xy vf19, vf19
  lqi.xyzw vf20, vi05        |  mulx.xyzw vf22, vf22, vf23
  sq.w vf18, 1(vi04)         |  nop
  sq.w vf15, 2(vi04)         |  nop
  sqi.xyzw vf12, vi04        |  nop
  sqi.xyz vf15, vi04         |  muli.xyz vf13, vf13, I
  sq.xyzw vf21, -3(vi05)     |  itof0.xyzw vf20, vf20
  ibeq vi02, vi03, L13       |  muli.xyz vf16, vf16, I
  sqi.xyz vf18, vi04         |  itof0.xyzw vf11, vf11
  lqi.xyzw vf12, vi03        |  itof0.xyz vf14, vf14
  lqi.xyzw vf15, vi03        |  nop
  lqi.xyzw vf18, vi03        |  itof12.xy vf17, vf17
  lqi.xyzw vf21, vi05        |  mulx.xyzw vf20, vf20, vf23
  sq.w vf19, 1(vi04)         |  nop
  sq.w vf16, 2(vi04)         |  nop
  sqi.xyzw vf13, vi04        |  nop
  sqi.xyz vf16, vi04         |  muli.xyz vf11, vf11, I
  sq.xyzw vf22, -3(vi05)     |  itof0.xyzw vf21, vf21
  ibne vi02, vi03, L12       |  muli.xyz vf14, vf14, I
  sqi.xyz vf19, vi04         |  itof0.xyzw vf12, vf12
L13:
  nop                        |  nop :e
  nop                        |  nop
L14:
  ilw.z vi01, 914(vi00)      |  nop
  xtop vi10                  |  nop
  lq.xyzw vf05, 0(vi10)      |  nop
  lq.xyzw vf06, 1(vi10)      |  nop
  iaddi vi01, vi01, -0x1     |  nop
  lq.xyzw vf07, 2(vi10)      |  nop
  ibne vi00, vi01, L15       |  nop
  lq.xyzw vf08, 3(vi10)      |  nop
  bal vi15, L4               |  nop
  nop                        |  nop
L15:
  lq.xyz vf12, 132(vi00)     |  nop
  lq.xyzw vf02, 4(vi10)      |  nop
  lq.xyzw vf03, 5(vi10)      |  nop
  lq.xyzw vf04, 6(vi10)      |  nop
  lq.xyzw vf16, 32(vi00)     |  subw.w vf13, vf00, vf00
  iaddiu vi08, vi00, 0x85    |  mulax.xyzw ACC, vf02, vf12
  lq.xy vf24, 33(vi00)       |  madday.xyzw ACC, vf03, vf12
  iaddiu vi09, vi00, 0x22    |  maddz.xyzw vf20, vf04, vf12
  nop                        |  mulaw.xyzw ACC, vf08, vf00
  nop                        |  maddax.xyzw ACC, vf05, vf16
  nop                        |  madday.xyzw ACC, vf06, vf16
  lq.w vf29, 4(vi10)         |  subw.z vf20, vf20, vf00
  nop                        |  maddz.xyz vf16, vf07, vf16
  lq.w vf19, 5(vi10)         |  subw.w vf02, vf00, vf00
  nop                        |  subw.w vf03, vf00, vf00
  nop                        |  addw.y vf31, vf00, vf29
  nop                        |  mul.xyz vf13, vf16, vf20
  -0.5                       |  ftoi4.w vf19, vf19 :i
  0.5                        |  addi.w vf02, vf02, I :i
  mtir vi02, vf16.w          |  addi.w vf03, vf03, I
  esum.xyzw P, vf13          |  mulz.xyz vf14, vf16, vf20
  lqi.xyz vf12, vi08         |  addw.x vf31, vf00, vf00
  nop                        |  addw.z vf24, vf00, vf00
  nop                        |  addw.z vf25, vf00, vf00
  lqi.xyzw vf17, vi09        |  addw.z vf26, vf00, vf00
  nop                        |  mulax.xyzw ACC, vf02, vf12
  lqi.xy vf25, vi09          |  madday.xyzw ACC, vf03, vf12
  nop                        |  maddz.xyzw vf21, vf04, vf12
  nop                        |  mulaw.xyzw ACC, vf08, vf00
  128.0                      |  maddax.xyzw ACC, vf05, vf17 :i
  esadd.xyz P, vf14          |  madday.xyzw ACC, vf06, vf17
  mfp.x vf13, P              |  muli.w vf31, vf29, I
  lq.xyzw vf09, 908(vi00)    |  subw.z vf21, vf21, vf00
  lq.xyzw vf10, 909(vi00)    |  maddz.xyz vf17, vf07, vf17
  nop                        |  subw.x vf31, vf31, vf29
  nop                        |  mulx.xy vf20, vf20, vf13
  nop                        |  ftoi0.w vf31, vf31
  nop                        |  mula.xy ACC, vf10, vf16
  0.5                        |  mul.xyz vf13, vf17, vf21 :i
  nop                        |  muli.y vf31, vf31, I
  256.0                      |  itof0.w vf31, vf31 :i
  mfp.x vf30, P              |  add.xy vf20, vf20, vf14
  mtir vi03, vf17.w          |  mulaw.zw ACC, vf10, vf00
  esum.xyzw P, vf13          |  mulz.xyz vf14, vf17, vf21
  lqi.xyz vf12, vi08         |  addw.z vf27, vf00, vf00
  nop                        |  addi.z vf31, vf00, I
  rsqrt Q, vf02.w, vf30.x    |  subw.z vf31, vf31, vf31
  nop                        |  maddz.xyzw vf18, vf09, vf16
  lqi.xyzw vf16, vi09        |  mulax.xyzw ACC, vf02, vf12
  lqi.xy vf26, vi09          |  madday.xyzw ACC, vf03, vf12
  nop                        |  maddz.xyzw vf22, vf04, vf12
  ilw.w vi12, 898(vi00)      |  mulaw.xyzw ACC, vf08, vf00
  lq.w vf30, 6(vi10)         |  maddax.xyzw ACC, vf05, vf16
  esadd.xyz P, vf14          |  madday.xyzw ACC, vf06, vf16
  mfp.x vf13, P              |  subw.z vf31, vf31, vf31
  lq.xyzw vf01, 7(vi10)      |  subw.z vf22, vf22, vf00
  iaddi vi10, vi10, 0x8      |  maddz.xyz vf16, vf07, vf16
  ilw.x vi14, 913(vi00)      |  addw.z vf20, vf00, vf00
  lqi.xyzw vf11, vi10        |  mulx.xy vf21, vf21, vf13
  div Q, vf00.w, vf18.w      |  mul.xy vf20, vf20, Q
  iaddiu vi11, vi00, 0x397   |  mula.xy ACC, vf10, vf17
  iadd vi14, vi14, vi12      |  mul.xyz vf13, vf16, vf22
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
  nop                        |  addw.xy vf20, vf20, vf03
  mfp.x vf30, P              |  add.xy vf21, vf21, vf14
  mtir vi04, vf16.w          |  mulaw.zw ACC, vf10, vf00
  esum.xyzw P, vf13          |  mulz.xyz vf14, vf16, vf22
  lqi.xyz vf12, vi08         |  nop
  mtir vi01, vf11.x          |  mul.xyz vf19, vf18, Q
  sqi.xyzw vf20, vi11        |  mul.xyz vf28, vf24, Q
  rsqrt Q, vf02.w, vf30.x    |  mul.xyz vf20, vf20, Q
  lq.xyz vf30, 770(vi01)     |  maddz.xyzw vf18, vf09, vf17
  lqi.xyzw vf17, vi09        |  mulax.xyzw ACC, vf02, vf12
  lqi.xyw vf27, vi09         |  madday.xyzw ACC, vf03, vf12
  iadd vi02, vi02, vi12      |  maddz.xyz vf23, vf04, vf12
  iadd vi06, vi02, vi13      |  mulaw.xyzw ACC, vf08, vf00
  sq.xyzw vf30, 1(vi02)      |  maddax.xyzw ACC, vf05, vf17
  esadd.xyz P, vf14          |  madday.xyzw ACC, vf06, vf17
  mfp.x vf13, P              |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi02)      |  maddz.xyz vf17, vf07, vf17
  sq.xyzw vf20, 0(vi06)      |  subw.z vf23, vf23, vf00
  sq.xyzw vf01, 1(vi06)      |  addw.z vf21, vf00, vf00
  sq.xyzw vf19, 2(vi02)      |  mulx.xy vf22, vf22, vf13
  div Q, vf00.w, vf18.w      |  mul.xy vf21, vf21, Q
  nop                        |  mula.xy ACC, vf10, vf16
  sq.xyzw vf19, 2(vi06)      |  nop
  mtir vi01, vf11.y          |  addw.xy vf21, vf21, vf03
  mfp.x vf30, P              |  add.xy vf22, vf22, vf14
  nop                        |  mulaw.zw ACC, vf10, vf00
  nop                        |  mulz.xyz vf14, vf17, vf23
  lqi.xyz vf12, vi08         |  mul.xyz vf19, vf18, Q
  sqi.xyzw vf21, vi11        |  mul.xyz vf28, vf25, Q
  rsqrt Q, vf02.w, vf30.x    |  mul.xyz vf21, vf21, Q
  lq.xyz vf30, 770(vi01)     |  maddz.xyzw vf18, vf09, vf16
  lqi.xyzw vf16, vi09        |  mul.xyz vf15, vf14, vf14
  nop                        |  mulax.xyzw ACC, vf02, vf12
  iadd vi03, vi03, vi12      |  madday.xyzw ACC, vf03, vf12
  iadd vi06, vi03, vi13      |  maddz.xyz vf20, vf04, vf12
  sq.xyzw vf30, 1(vi03)      |  addy.x vf15, vf15, vf15
  nop                        |  mul.xyz vf13, vf17, vf23
  nop                        |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi03)      |  nop
  sq.xyzw vf21, 0(vi06)      |  addz.x vf15, vf15, vf15
  sq.xyzw vf01, 1(vi06)      |  addw.z vf22, vf00, vf00
  sq.xyzw vf19, 2(vi03)      |  addy.x vf13, vf13, vf13
  div Q, vf00.w, vf18.w      |  mul.xy vf22, vf22, Q
  nop                        |  mulaw.xyzw ACC, vf08, vf00
  sq.xyzw vf19, 2(vi06)      |  maddax.xyzw ACC, vf05, vf16
  mtir vi01, vf11.z          |  addw.xy vf22, vf22, vf03
  nop                        |  addz.x vf13, vf13, vf13
  nop                        |  madday.xyzw ACC, vf06, vf16
  nop                        |  maddz.xyz vf16, vf07, vf16
  nop                        |  mul.xyz vf19, vf18, Q
  sqi.xyzw vf22, vi11        |  mul.xyz vf28, vf26, Q
  nop                        |  mul.xyz vf22, vf22, Q
  lq.xyz vf30, 770(vi01)     |  nop
  ilw.x vi14, 914(vi00)      |  nop
  rsqrt Q, vf02.w, vf15.x    |  mulx.xy vf23, vf23, vf13
  iadd vi04, vi04, vi12      |  nop
  iadd vi06, vi04, vi13      |  mula.xy ACC, vf10, vf17
  sq.xyzw vf30, 1(vi04)      |  mulaw.zw ACC, vf10, vf00
  ibne vi00, vi14, L17       |  add.xy vf23, vf23, vf14
  lqi.xyw vf24, vi09         |  ftoi4.xyz vf19, vf19
  ilw.y vi14, 913(vi00)      |  nop
  lqi.xyz vf12, vi08         |  subw.z vf20, vf20, vf00
  sq.xyzw vf28, 0(vi04)      |  nop
  sq.xyzw vf22, 0(vi06)      |  maddz.xyzw vf18, vf09, vf17
  sq.xyzw vf01, 1(vi06)      |  nop
  sq.xyzw vf19, 2(vi04)      |  mulax.xyzw ACC, vf02, vf12
  iadd vi14, vi14, vi12      |  madday.xyzw ACC, vf03, vf12
  b L27                      |  mulz.xyz vf14, vf16, vf20
  sq.xyzw vf19, 2(vi06)      |  maddz.xyz vf21, vf04, vf12
L17:
  ilw.y vi14, 914(vi00)      |  nop
  sq.xyzw vf28, 0(vi04)      |  nop
  sq.xyzw vf22, 0(vi06)      |  nop
  sq.xyzw vf01, 0(vi06)      |  nop
  sq.xyzw vf19, 2(vi04)      |  nop
  ibne vi00, vi14, L39       |  nop
  sq.xyzw vf19, 2(vi06)      |  nop
  b L32                      |  nop
  nop                        |  nop
L18:
  nop                        |  nop
  mtir vi01, vf11.y          |  addw.xy vf21, vf21, vf03
  mfp.x vf30, P              |  add.xy vf22, vf22, vf14
  mtir vi05, vf17.w          |  mulaw.zw ACC, vf10, vf00
  esum.xyzw P, vf13          |  mulz.xyz vf14, vf17, vf23
  lqi.xyzw vf12, vi08        |  mul.xyz vf19, vf18, Q
  sqi.xyzw vf21, vi11        |  mul.xyz vf28, vf25, Q
  rsqrt Q, vf02.w, vf30.x    |  mul.xyz vf21, vf21, Q
  lq.xyz vf30, 770(vi01)     |  maddz.xyzw vf18, vf09, vf16
  lqi.xyzw vf16, vi09        |  mulax.xyzw ACC, vf02, vf12
  lqi.xyw vf24, vi09         |  madday.xyzw ACC, vf03, vf12
  iadd vi03, vi03, vi12      |  maddz.xyz vf20, vf04, vf12
  iadd vi06, vi03, vi13      |  mulaw.xyzw ACC, vf08, vf00
  sq.xyzw vf30, 1(vi03)      |  maddax.xyzw ACC, vf05, vf16
  nop                        |  nop
  nop                        |  nop
  esadd.xyz P, vf14          |  madday.xyzw ACC, vf06, vf16
  mfp.x vf13, P              |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi03)      |  maddz.xyz vf16, vf07, vf16
  sq.xyzw vf21, 0(vi06)      |  subw.z vf20, vf20, vf00
  sq.xyzw vf01, 1(vi06)      |  addw.z vf22, vf00, vf00
  sq.xyzw vf19, 2(vi03)      |  mulx.xy vf23, vf23, vf13
  div Q, vf00.w, vf18.w      |  mul.xy vf22, vf22, Q
  nop                        |  mula.xy ACC, vf10, vf17
  sq.xyzw vf19, 2(vi06)      |  nop
  mtir vi01, vf11.z          |  addw.xy vf22, vf22, vf03
  nop                        |  nop
  nop                        |  nop
  mfp.x vf30, P              |  add.xy vf23, vf23, vf14
  nop                        |  mulaw.zw ACC, vf10, vf00
  nop                        |  mulz.xyz vf14, vf16, vf20
  lqi.xyzw vf12, vi08        |  mul.xyz vf19, vf18, Q
  sqi.xyzw vf22, vi11        |  mul.xyz vf28, vf26, Q
  rsqrt Q, vf02.w, vf30.x    |  mul.xyz vf22, vf22, Q
  lq.xyz vf30, 770(vi01)     |  maddz.xyzw vf18, vf09, vf17
  lqi.xyzw vf17, vi09        |  mul.xyz vf15, vf14, vf14
  nop                        |  mulax.xyzw ACC, vf02, vf12
  iadd vi04, vi04, vi12      |  madday.xyzw ACC, vf03, vf12
  iadd vi06, vi04, vi13      |  maddz.xyz vf21, vf04, vf12
  sq.xyzw vf30, 1(vi04)      |  addy.x vf15, vf15, vf15
  nop                        |  mul.xyz vf13, vf16, vf20
  nop                        |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi04)      |  nop
  sq.xyzw vf22, 0(vi06)      |  addz.x vf15, vf15, vf15
  sq.xyzw vf01, 1(vi06)      |  addw.z vf23, vf00, vf00
  sq.xyzw vf19, 2(vi04)      |  addy.x vf13, vf13, vf13
  div Q, vf00.w, vf18.w      |  mul.xy vf23, vf23, Q
  nop                        |  mulaw.xyzw ACC, vf08, vf00
  sq.xyzw vf19, 2(vi06)      |  maddax.xyzw ACC, vf05, vf17
  nop                        |  nop
  mtir vi01, vf11.w          |  addw.xy vf23, vf23, vf03
  nop                        |  addz.x vf13, vf13, vf13
  nop                        |  madday.xyzw ACC, vf06, vf17
  nop                        |  maddz.xyz vf17, vf07, vf17
  nop                        |  mul.xyz vf19, vf18, Q
  sqi.xyzw vf23, vi11        |  mul.xyz vf28, vf27, Q
  nop                        |  mul.xyz vf23, vf23, Q
  lq.xyz vf30, 770(vi01)     |  nop
  ilw.x vi14, 914(vi00)      |  nop
  rsqrt Q, vf02.w, vf15.x    |  mulx.xy vf20, vf20, vf13
  iadd vi05, vi05, vi12      |  nop
  iadd vi06, vi05, vi13      |  mula.xy ACC, vf10, vf16
  sq.xyzw vf30, 1(vi05)      |  mulaw.zw ACC, vf10, vf00
  ibne vi00, vi14, L19       |  add.xy vf20, vf20, vf14
  lqi.xyw vf25, vi09         |  ftoi4.xyz vf19, vf19
  ilw.y vi14, 913(vi00)      |  nop
  lqi.xyzw vf12, vi08        |  subw.z vf21, vf21, vf00
  sq.xyzw vf28, 0(vi05)      |  nop
  sq.xyzw vf23, 0(vi06)      |  maddz.xyzw vf18, vf09, vf16
  sq.xyzw vf01, 1(vi06)      |  nop
  sq.xyzw vf19, 2(vi05)      |  mulax.xyzw ACC, vf02, vf12
  iadd vi14, vi14, vi12      |  madday.xyzw ACC, vf03, vf12
  b L24                      |  mulz.xyz vf14, vf17, vf21
  sq.xyzw vf19, 2(vi06)      |  maddz.xyz vf22, vf04, vf12
L19:
  ilw.y vi14, 914(vi00)      |  nop
  sq.xyzw vf28, 0(vi05)      |  nop
  sq.xyzw vf23, 0(vi06)      |  nop
  sq.xyzw vf01, 1(vi06)      |  nop
  sq.xyzw vf19, 2(vi05)      |  nop
  ibne vi00, vi14, L39       |  nop
  sq.xyzw vf19, 2(vi06)      |  nop
  b L32                      |  nop
  nop                        |  nop
L20:
  mtir vi01, vf11.z          |  addw.xy vf22, vf22, vf03
  mfp.x vf30, P              |  add.xy vf23, vf23, vf14
  mtir vi02, vf16.w          |  mulaw.zw ACC, vf10, vf00
  esum.xyzw P, vf13          |  mulz.xyz vf14, vf16, vf20
  lqi.xyzw vf12, vi08        |  mul.xyz vf19, vf18, Q
  sqi.xyzw vf22, vi11        |  mul.xyz vf28, vf26, Q
  rsqrt Q, vf02.w, vf30.x    |  mul.xyz vf22, vf22, Q
  lq.xyz vf30, 770(vi01)     |  maddz.xyzw vf18, vf09, vf17
  lqi.xyzw vf17, vi09        |  mulax.xyzw ACC, vf02, vf12
  lqi.xyw vf25, vi09         |  madday.xyzw ACC, vf03, vf12
  iadd vi04, vi04, vi12      |  maddz.xyz vf21, vf04, vf12
  iadd vi06, vi04, vi13      |  mulaw.xyzw ACC, vf08, vf00
  sq.xyzw vf30, 1(vi04)      |  maddax.xyzw ACC, vf05, vf17
  esadd.xyz P, vf14          |  madday.xyzw ACC, vf06, vf17
  mfp.x vf13, P              |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi04)      |  maddz.xyz vf17, vf07, vf17
  sq.xyzw vf22, 0(vi06)      |  subw.z vf21, vf21, vf00
  sq.xyzw vf01, 1(vi06)      |  addw.z vf23, vf00, vf00
  sq.xyzw vf19, 2(vi04)      |  mulx.xy vf20, vf20, vf13
  div Q, vf00.w, vf18.w      |  mul.xy vf23, vf23, Q
  nop                        |  mula.xy ACC, vf10, vf16
  sq.xyzw vf19, 2(vi06)      |  nop
  mtir vi01, vf11.w          |  addw.xy vf23, vf23, vf03
  mfp.x vf30, P              |  add.xy vf20, vf20, vf14
  nop                        |  mulaw.zw ACC, vf10, vf00
  nop                        |  mulz.xyz vf14, vf17, vf21
  lqi.xyzw vf12, vi08        |  mul.xyz vf19, vf18, Q
  sqi.xyzw vf23, vi11        |  mul.xyz vf28, vf27, Q
  rsqrt Q, vf02.w, vf30.x    |  mul.xyz vf23, vf23, Q
  lq.xyz vf30, 770(vi01)     |  maddz.xyzw vf18, vf09, vf16
  lqi.xyzw vf16, vi09        |  mul.xyz vf15, vf14, vf14
  nop                        |  mulax.xyzw ACC, vf02, vf12
  iadd vi05, vi05, vi12      |  madday.xyzw ACC, vf03, vf12
  iadd vi06, vi05, vi13      |  maddz.xyz vf22, vf04, vf12
  sq.xyzw vf30, 1(vi05)      |  addy.x vf15, vf15, vf15
  nop                        |  mul.xyz vf13, vf17, vf21
  lqi.xyzw vf11, vi10        |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi05)      |  nop
  sq.xyzw vf23, 0(vi06)      |  addz.x vf15, vf15, vf15
  sq.xyzw vf01, 1(vi06)      |  addw.z vf20, vf00, vf00
  sq.xyzw vf19, 2(vi05)      |  addy.x vf13, vf13, vf13
  div Q, vf00.w, vf18.w      |  mul.xy vf20, vf20, Q
  nop                        |  mulaw.xyzw ACC, vf08, vf00
  sq.xyzw vf19, 2(vi06)      |  maddax.xyzw ACC, vf05, vf16
  mtir vi01, vf11.x          |  addw.xy vf20, vf20, vf03
  nop                        |  addz.x vf13, vf13, vf13
  nop                        |  madday.xyzw ACC, vf06, vf16
  nop                        |  maddz.xyz vf16, vf07, vf16
  nop                        |  mul.xyz vf19, vf18, Q
  sqi.xyzw vf20, vi11        |  mul.xyz vf28, vf24, Q
  nop                        |  mul.xyz vf20, vf20, Q
  lq.xyz vf30, 770(vi01)     |  nop
  ilw.x vi14, 914(vi00)      |  nop
  rsqrt Q, vf02.w, vf15.x    |  mulx.xy vf21, vf21, vf13
  iadd vi02, vi02, vi12      |  nop
  iadd vi06, vi02, vi13      |  mula.xy ACC, vf10, vf17
  sq.xyzw vf30, 1(vi02)      |  mulaw.zw ACC, vf10, vf00
  ibne vi00, vi14, L21       |  add.xy vf21, vf21, vf14
  lqi.xyw vf26, vi09         |  ftoi4.xyz vf19, vf19
  ilw.y vi14, 913(vi00)      |  subw.z vf22, vf22, vf00
  sq.xyzw vf28, 0(vi02)      |  nop
  sq.xyzw vf20, 0(vi06)      |  maddz.xyzw vf18, vf09, vf17
  sq.xyzw vf01, 1(vi06)      |  nop
  sq.xyzw vf19, 2(vi02)      |  mulax.xyzw ACC, vf02, vf12
  iadd vi14, vi14, vi12      |  madday.xyzw ACC, vf03, vf12
  b L25                      |  mulz.xyz vf14, vf16, vf22
  sq.xyzw vf19, 2(vi06)      |  maddz.xyz vf23, vf04, vf12
L21:
  ilw.y vi14, 914(vi00)      |  nop
  sq.xyzw vf28, 0(vi02)      |  nop
  sq.xyzw vf20, 0(vi06)      |  nop
  sq.xyzw vf01, 1(vi06)      |  nop
  sq.xyzw vf19, 2(vi02)      |  nop
  ibne vi00, vi14, L39       |  nop
  sq.xyzw vf19, 2(vi06)      |  nop
  b L32                      |  nop
  nop                        |  nop
L22:
  mtir vi01, vf11.w          |  addw.xy vf23, vf23, vf03
  mfp.x vf30, P              |  add.xy vf20, vf20, vf14
  mtir vi03, vf17.w          |  mulaw.zw ACC, vf10, vf00
  esum.xyzw P, vf13          |  mulz.xyz vf14, vf17, vf21
  lqi.xyzw vf12, vi08        |  mul.xyz vf19, vf18, Q
  sqi.xyzw vf23, vi11        |  mul.xyz vf28, vf27, Q
  rsqrt Q, vf02.w, vf30.x    |  mul.xyz vf23, vf23, Q
  lq.xyz vf30, 770(vi01)     |  maddz.xyzw vf18, vf09, vf16
  lqi.xyzw vf16, vi09        |  mulax.xyzw ACC, vf02, vf12
  lqi.xyw vf26, vi09         |  madday.xyzw ACC, vf03, vf12
  iadd vi05, vi05, vi12      |  maddz.xyz vf22, vf04, vf12
  iadd vi06, vi05, vi13      |  mulaw.xyzw ACC, vf08, vf00
  sq.xyzw vf30, 1(vi05)      |  maddax.xyzw ACC, vf05, vf16
  esadd.xyz P, vf14          |  madday.xyzw ACC, vf06, vf16
  mfp.x vf13, P              |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi05)      |  maddz.xyz vf16, vf07, vf16
  sq.xyzw vf23, 0(vi06)      |  subw.z vf22, vf22, vf00
  sq.xyzw vf01, 1(vi06)      |  addw.z vf20, vf00, vf00
  sq.xyzw vf19, 2(vi05)      |  mulx.xy vf21, vf21, vf13
  div Q, vf00.w, vf18.w      |  mul.xy vf20, vf20, Q
  lqi.xyzw vf11, vi10        |  mula.xy ACC, vf10, vf17
  sq.xyzw vf19, 2(vi06)      |  nop
  nop                        |  addw.xy vf20, vf20, vf03
  mfp.x vf30, P              |  add.xy vf21, vf21, vf14
  nop                        |  mulaw.zw ACC, vf10, vf00
  mtir vi01, vf11.x          |  mulz.xyz vf14, vf16, vf22
  lqi.xyzw vf12, vi08        |  mul.xyz vf19, vf18, Q
  sqi.xyzw vf20, vi11        |  mul.xyz vf28, vf24, Q
  rsqrt Q, vf02.w, vf30.x    |  mul.xyz vf20, vf20, Q
  lq.xyz vf30, 770(vi01)     |  maddz.xyzw vf18, vf09, vf17
  lqi.xyzw vf17, vi09        |  mul.xyz vf15, vf14, vf14
  nop                        |  mulax.xyzw ACC, vf02, vf12
  iadd vi02, vi02, vi12      |  madday.xyzw ACC, vf03, vf12
  iadd vi06, vi02, vi13      |  maddz.xyz vf23, vf04, vf12
  sq.xyzw vf30, 1(vi02)      |  addy.x vf15, vf15, vf15
  nop                        |  mul.xyz vf13, vf16, vf22
  nop                        |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi02)      |  nop
  sq.xyzw vf20, 0(vi06)      |  addz.x vf15, vf15, vf15
  sq.xyzw vf01, 1(vi06)      |  addw.z vf21, vf00, vf00
  sq.xyzw vf19, 2(vi02)      |  addy.x vf13, vf13, vf13
  div Q, vf00.w, vf18.w      |  mul.xy vf21, vf21, Q
  nop                        |  mulaw.xyzw ACC, vf08, vf00
  sq.xyzw vf19, 2(vi06)      |  maddax.xyzw ACC, vf05, vf17
  mtir vi01, vf11.y          |  addw.xy vf21, vf21, vf03
  nop                        |  addz.x vf13, vf13, vf13
  nop                        |  madday.xyzw ACC, vf06, vf17
  nop                        |  maddz.xyz vf17, vf07, vf17
  nop                        |  mul.xyz vf19, vf18, Q
  sqi.xyzw vf21, vi11        |  mul.xyz vf28, vf25, Q
  nop                        |  mul.xyz vf21, vf21, Q
  lq.xyz vf30, 770(vi01)     |  nop
  ilw.x vi14, 914(vi00)      |  nop
  rsqrt Q, vf02.w, vf15.x    |  mulx.xy vf22, vf22, vf13
  iadd vi03, vi03, vi12      |  nop
  iadd vi06, vi03, vi13      |  mula.xy ACC, vf10, vf16
  sq.xyzw vf30, 1(vi03)      |  mulaw.zw ACC, vf10, vf00
  ibne vi00, vi14, L23       |  add.xy vf22, vf22, vf14
  lqi.xyw vf27, vi09         |  ftoi4.xyz vf19, vf19
  ilw.y vi14, 913(vi00)      |  nop
  lqi.xyzw vf12, vi08        |  subw.z vf23, vf23, vf00
  sq.xyzw vf28, 0(vi03)      |  nop
  sq.xyzw vf21, 0(vi06)      |  maddz.xyzw vf18, vf09, vf16
  sq.xyzw vf01, 1(vi06)      |  nop
  sq.xyzw vf19, 2(vi03)      |  mulax.xyzw ACC, vf02, vf12
  iadd vi14, vi14, vi12      |  madday.xyzw ACC, vf03, vf12
  b L26                      |  mulz.xyz vf14, vf17, vf23
  sq.xyzw vf19, 2(vi06)      |  maddz.xyz vf20, vf04, vf12
L23:
  ilw.y vi14, 914(vi00)      |  nop
  sq.xyzw vf28, 0(vi03)      |  nop
  sq.xyzw vf21, 0(vi06)      |  nop
  sq.xyzw vf01, 1(vi06)      |  nop
  sq.xyzw vf19, 2(vi03)      |  nop
  ibne vi00, vi14, L39       |  nop
  sq.xyzw vf19, 2(vi06)      |  nop
  b L32                      |  nop
  nop                        |  nop
L24:
  lqi.xyzw vf11, vi10        |  nop
  div Q, vf00.w, vf18.w      |  mul.xy vf20, vf20, Q
  mtir vi02, vf16.w          |  addw.z vf20, vf00, vf00
  mtir vi07, vf24.w          |  mul.xyz vf15, vf14, vf14
  lqi.xyzw vf16, vi09        |  mul.xyz vf13, vf17, vf21
  mtir vi01, vf11.x          |  addw.xy vf20, vf20, vf03
  iadd vi02, vi02, vi12      |  mulaw.xyzw ACC, vf08, vf00
  iadd vi06, vi02, vi13      |  addy.x vf15, vf15, vf15
  iadd vi07, vi07, vi12      |  maddax.xyzw ACC, vf05, vf16
  sqi.xyzw vf20, vi11        |  addy.x vf13, vf13, vf13
  lq.xyz vf30, 770(vi01)     |  madday.xyzw ACC, vf06, vf16
  iadd vi15, vi07, vi13      |  addz.x vf15, vf15, vf15
  sq.xyzw vf01, 1(vi06)      |  maddz.xyz vf16, vf07, vf16
  sq.xyzw vf01, 1(vi15)      |  addz.x vf13, vf13, vf13
  sq.xyzw vf30, 1(vi02)      |  mul.xyz vf19, vf18, Q
  rsqrt Q, vf02.w, vf15.x    |  mul.xyz vf28, vf24, Q
  sq.xyzw vf30, 1(vi07)      |  mul.xyz vf20, vf20, Q
  lqi.xyw vf26, vi09         |  mulx.xy vf21, vf21, vf13
  lqi.xyzw vf12, vi08        |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi02)      |  mula.xy ACC, vf10, vf17
  sq.xyzw vf28, 0(vi07)      |  mulaw.zw ACC, vf10, vf00
  sq.xyzw vf20, 0(vi06)      |  add.xy vf21, vf21, vf14
  sq.xyzw vf20, 0(vi15)      |  subw.z vf22, vf22, vf00
  sq.xyzw vf19, 2(vi02)      |  maddz.xyzw vf18, vf09, vf17
  sq.xyzw vf19, 2(vi06)      |  mulax.xyzw ACC, vf02, vf12
  sq.xyzw vf19, 2(vi07)      |  madday.xyzw ACC, vf03, vf12
  ibeq vi14, vi02, L28       |  mulz.xyz vf14, vf16, vf22
  sq.xyzw vf19, 2(vi15)      |  maddz.xyz vf23, vf04, vf12
L25:
  div Q, vf00.w, vf18.w      |  mul.xy vf21, vf21, Q
  mtir vi02, vf17.w          |  addw.z vf21, vf00, vf00
  mtir vi07, vf25.w          |  mul.xyz vf15, vf14, vf14
  lqi.xyzw vf17, vi09        |  mul.xyz vf13, vf16, vf22
  mtir vi01, vf11.y          |  addw.xy vf21, vf21, vf03
  iadd vi02, vi02, vi12      |  mulaw.xyzw ACC, vf08, vf00
  iadd vi06, vi02, vi13      |  addy.x vf15, vf15, vf15
  iadd vi07, vi07, vi12      |  maddax.xyzw ACC, vf05, vf17
  sqi.xyzw vf21, vi11        |  addy.x vf13, vf13, vf13
  lq.xyz vf30, 770(vi01)     |  madday.xyzw ACC, vf06, vf17
  iadd vi15, vi07, vi13      |  addz.x vf15, vf15, vf15
  sq.xyzw vf01, 1(vi06)      |  maddz.xyz vf17, vf07, vf17
  sq.xyzw vf01, 1(vi15)      |  addz.x vf13, vf13, vf13
  sq.xyzw vf30, 1(vi02)      |  mul.xyz vf19, vf18, Q
  rsqrt Q, vf02.w, vf15.x    |  mul.xyz vf28, vf25, Q
  sq.xyzw vf30, 1(vi07)      |  mul.xyz vf21, vf21, Q
  lqi.xyw vf27, vi09         |  mulx.xy vf22, vf22, vf13
  lqi.xyzw vf12, vi08        |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi02)      |  mula.xy ACC, vf10, vf16
  sq.xyzw vf28, 0(vi07)      |  mulaw.zw ACC, vf10, vf00
  sq.xyzw vf21, 0(vi06)      |  add.xy vf22, vf22, vf14
  sq.xyzw vf21, 0(vi15)      |  subw.z vf23, vf23, vf00
  sq.xyzw vf19, 2(vi02)      |  maddz.xyzw vf18, vf09, vf16
  sq.xyzw vf19, 2(vi06)      |  mulax.xyzw ACC, vf02, vf12
  sq.xyzw vf19, 2(vi07)      |  madday.xyzw ACC, vf03, vf12
  ibeq vi14, vi02, L29       |  mulz.xyz vf14, vf17, vf23
  sq.xyzw vf19, 2(vi15)      |  maddz.xyz vf20, vf04, vf12
L26:
  div Q, vf00.w, vf18.w      |  mul.xy vf22, vf22, Q
  mtir vi02, vf16.w          |  addw.z vf22, vf00, vf00
  mtir vi07, vf26.w          |  mul.xyz vf15, vf14, vf14
  lqi.xyzw vf16, vi09        |  mul.xyz vf13, vf17, vf23
  mtir vi01, vf11.z          |  addw.xy vf22, vf22, vf03
  iadd vi02, vi02, vi12      |  mulaw.xyzw ACC, vf08, vf00
  iadd vi06, vi02, vi13      |  addy.x vf15, vf15, vf15
  iadd vi07, vi07, vi12      |  maddax.xyzw ACC, vf05, vf16
  sqi.xyzw vf22, vi11        |  addy.x vf13, vf13, vf13
  lq.xyz vf30, 770(vi01)     |  madday.xyzw ACC, vf06, vf16
  iadd vi15, vi07, vi13      |  addz.x vf15, vf15, vf15
  sq.xyzw vf01, 1(vi06)      |  maddz.xyz vf16, vf07, vf16
  sq.xyzw vf01, 1(vi15)      |  addz.x vf13, vf13, vf13
  sq.xyzw vf30, 1(vi02)      |  mul.xyz vf19, vf18, Q
  rsqrt Q, vf02.w, vf15.x    |  mul.xyz vf28, vf26, Q
  sq.xyzw vf30, 1(vi07)      |  mul.xyz vf22, vf22, Q
  lqi.xyw vf24, vi09         |  mulx.xy vf23, vf23, vf13
  lqi.xyzw vf12, vi08        |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi02)      |  mula.xy ACC, vf10, vf17
  sq.xyzw vf28, 0(vi07)      |  mulaw.zw ACC, vf10, vf00
  sq.xyzw vf22, 0(vi06)      |  add.xy vf23, vf23, vf14
  sq.xyzw vf22, 0(vi15)      |  subw.z vf20, vf20, vf00
  sq.xyzw vf19, 2(vi02)      |  maddz.xyzw vf18, vf09, vf17
  sq.xyzw vf19, 2(vi06)      |  mulax.xyzw ACC, vf02, vf12
  sq.xyzw vf19, 2(vi07)      |  madday.xyzw ACC, vf03, vf12
  ibeq vi14, vi02, L30       |  mulz.xyz vf14, vf16, vf20
  sq.xyzw vf19, 2(vi15)      |  maddz.xyz vf21, vf04, vf12
L27:
  div Q, vf00.w, vf18.w      |  mul.xy vf23, vf23, Q
  mtir vi02, vf17.w          |  addw.z vf23, vf00, vf00
  mtir vi07, vf27.w          |  mul.xyz vf15, vf14, vf14
  lqi.xyzw vf17, vi09        |  mul.xyz vf13, vf16, vf20
  mtir vi01, vf11.w          |  addw.xy vf23, vf23, vf03
  iadd vi02, vi02, vi12      |  mulaw.xyzw ACC, vf08, vf00
  iadd vi06, vi02, vi13      |  addy.x vf15, vf15, vf15
  iadd vi07, vi07, vi12      |  maddax.xyzw ACC, vf05, vf17
  sqi.xyzw vf23, vi11        |  addy.x vf13, vf13, vf13
  lq.xyz vf30, 770(vi01)     |  madday.xyzw ACC, vf06, vf17
  iadd vi15, vi07, vi13      |  addz.x vf15, vf15, vf15
  sq.xyzw vf01, 1(vi06)      |  maddz.xyz vf17, vf07, vf17
  sq.xyzw vf01, 1(vi15)      |  addz.x vf13, vf13, vf13
  sq.xyzw vf30, 1(vi02)      |  mul.xyz vf19, vf18, Q
  rsqrt Q, vf02.w, vf15.x    |  mul.xyz vf28, vf27, Q
  sq.xyzw vf30, 1(vi07)      |  mul.xyz vf23, vf23, Q
  lqi.xyw vf25, vi09         |  mulx.xy vf20, vf20, vf13
  lqi.xyzw vf12, vi08        |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi02)      |  mula.xy ACC, vf10, vf16
  sq.xyzw vf28, 0(vi07)      |  mulaw.zw ACC, vf10, vf00
  sq.xyzw vf23, 0(vi06)      |  add.xy vf20, vf20, vf14
  sq.xyzw vf23, 0(vi15)      |  subw.z vf21, vf21, vf00
  sq.xyzw vf19, 2(vi02)      |  maddz.xyzw vf18, vf09, vf16
  sq.xyzw vf19, 2(vi06)      |  mulax.xyzw ACC, vf02, vf12
  sq.xyzw vf19, 2(vi07)      |  madday.xyzw ACC, vf03, vf12
  ibne vi14, vi02, L24       |  mulz.xyz vf14, vf17, vf21
  sq.xyzw vf19, 2(vi15)      |  maddz.xyz vf22, vf04, vf12
  lqi.xyzw vf11, vi10        |  nop
  div Q, vf00.w, vf18.w      |  mul.xy vf20, vf20, Q
  mtir vi02, vf16.w          |  addw.z vf20, vf00, vf00
  mtir vi07, vf24.w          |  mul.xyz vf15, vf14, vf14
  nop                        |  mul.xyz vf13, vf17, vf21
  mtir vi01, vf11.x          |  addw.xy vf20, vf20, vf03
  iadd vi02, vi02, vi12      |  nop
  iadd vi06, vi02, vi13      |  addy.x vf15, vf15, vf15
  iadd vi07, vi07, vi12      |  nop
  sqi.xyzw vf20, vi11        |  addy.x vf13, vf13, vf13
  lq.xyz vf30, 770(vi01)     |  nop
  iadd vi15, vi07, vi13      |  addz.x vf15, vf15, vf15
  sq.xyzw vf01, 1(vi06)      |  nop
  sq.xyzw vf01, 1(vi15)      |  addz.x vf13, vf13, vf13
  sq.xyzw vf30, 1(vi02)      |  mul.xyz vf19, vf18, Q
  rsqrt Q, vf02.w, vf15.x    |  mul.xyz vf28, vf24, Q
  sq.xyzw vf30, 1(vi07)      |  mul.xyz vf20, vf20, Q
  nop                        |  mulx.xy vf21, vf21, vf13
  nop                        |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi02)      |  mula.xy ACC, vf10, vf17
  sq.xyzw vf28, 0(vi07)      |  mulaw.zw ACC, vf10, vf00
  sq.xyzw vf20, 0(vi06)      |  add.xy vf21, vf21, vf14
  sq.xyzw vf20, 0(vi15)      |  nop
  sq.xyzw vf19, 2(vi02)      |  nop
  sq.xyzw vf19, 2(vi06)      |  maddz.xyzw vf18, vf09, vf17
  sq.xyzw vf19, 2(vi07)      |  nop
  nop                        |  nop
  sq.xyzw vf19, 2(vi15)      |  nop
  div Q, vf00.w, vf18.w      |  mul.xy vf21, vf21, Q
  mtir vi02, vf17.w          |  addw.z vf21, vf00, vf00
  mtir vi07, vf25.w          |  nop
  nop                        |  nop
  mtir vi01, vf11.y          |  addw.xy vf21, vf21, vf03
  iadd vi02, vi02, vi12      |  nop
  iadd vi06, vi02, vi13      |  nop
  iadd vi07, vi07, vi12      |  nop
  sqi.xyzw vf21, vi11        |  nop
  lq.xyz vf30, 770(vi01)     |  nop
  iadd vi15, vi07, vi13      |  nop
  sq.xyzw vf01, 1(vi06)      |  nop
  sq.xyzw vf01, 1(vi15)      |  nop
  sq.xyzw vf30, 1(vi02)      |  mul.xyz vf19, vf18, Q
  nop                        |  mul.xyz vf28, vf25, Q
  sq.xyzw vf30, 1(vi07)      |  mul.xyz vf21, vf21, Q
  nop                        |  nop
  ilw.y vi14, 914(vi00)      |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi02)      |  nop
  sq.xyzw vf28, 0(vi07)      |  nop
  sq.xyzw vf21, 0(vi06)      |  nop
  sq.xyzw vf21, 0(vi15)      |  nop
  sq.xyzw vf19, 2(vi02)      |  nop
  sq.xyzw vf19, 2(vi06)      |  nop
  sq.xyzw vf19, 2(vi07)      |  nop
  ibne vi00, vi14, L39       |  nop
  sq.xyzw vf19, 2(vi15)      |  nop
  b L31                      |  nop
  nop                        |  nop
L28:
  div Q, vf00.w, vf18.w      |  mul.xy vf21, vf21, Q
  mtir vi02, vf17.w          |  addw.z vf21, vf00, vf00
  mtir vi07, vf25.w          |  mul.xyz vf15, vf14, vf14
  nop                        |  mul.xyz vf13, vf16, vf22
  mtir vi01, vf11.y          |  addw.xy vf21, vf21, vf03
  iadd vi02, vi02, vi12      |  nop
  iadd vi06, vi02, vi13      |  addy.x vf15, vf15, vf15
  iadd vi07, vi07, vi12      |  nop
  sqi.xyzw vf21, vi11        |  addy.x vf13, vf13, vf13
  lq.xyz vf30, 770(vi01)     |  nop
  iadd vi15, vi07, vi13      |  addz.x vf15, vf15, vf15
  sq.xyzw vf01, 1(vi06)      |  nop
  sq.xyzw vf01, 1(vi15)      |  addz.x vf13, vf13, vf13
  sq.xyzw vf30, 1(vi02)      |  mul.xyz vf19, vf18, Q
  rsqrt Q, vf02.w, vf15.x    |  mul.xyz vf28, vf25, Q
  sq.xyzw vf30, 1(vi07)      |  mul.xyz vf21, vf21, Q
  nop                        |  mulx.xy vf22, vf22, vf13
  nop                        |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi02)      |  mula.xy ACC, vf10, vf16
  sq.xyzw vf28, 0(vi07)      |  mulaw.zw ACC, vf10, vf00
  sq.xyzw vf21, 0(vi06)      |  add.xy vf22, vf22, vf14
  sq.xyzw vf21, 0(vi15)      |  nop
  sq.xyzw vf19, 2(vi02)      |  nop
  sq.xyzw vf19, 2(vi06)      |  maddz.xyzw vf18, vf09, vf16
  sq.xyzw vf19, 2(vi07)      |  nop
  nop                        |  nop
  sq.xyzw vf19, 2(vi15)      |  nop
  div Q, vf00.w, vf18.w      |  mul.xy vf22, vf22, Q
  mtir vi02, vf16.w          |  addw.z vf22, vf00, vf00
  mtir vi07, vf26.w          |  nop
  nop                        |  nop
  mtir vi01, vf11.z          |  addw.xy vf22, vf22, vf03
  iadd vi02, vi02, vi12      |  nop
  iadd vi06, vi02, vi13      |  nop
  iadd vi07, vi07, vi12      |  nop
  sqi.xyzw vf22, vi11        |  nop
  lq.xyz vf30, 770(vi01)     |  nop
  iadd vi15, vi07, vi13      |  nop
  sq.xyzw vf01, 1(vi06)      |  nop
  sq.xyzw vf01, 1(vi15)      |  nop
  sq.xyzw vf30, 1(vi02)      |  mul.xyz vf19, vf18, Q
  nop                        |  mul.xyz vf28, vf26, Q
  sq.xyzw vf30, 1(vi07)      |  mul.xyz vf22, vf22, Q
  nop                        |  nop
  ilw.y vi14, 914(vi00)      |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi02)      |  nop
  sq.xyzw vf28, 0(vi07)      |  nop
  sq.xyzw vf22, 0(vi06)      |  nop
  sq.xyzw vf22, 0(vi15)      |  nop
  sq.xyzw vf19, 2(vi02)      |  nop
  sq.xyzw vf19, 2(vi06)      |  nop
  sq.xyzw vf19, 2(vi07)      |  nop
  ibne vi00, vi14, L39       |  nop
  sq.xyzw vf19, 2(vi15)      |  nop
  b L31                      |  nop
  nop                        |  nop
L29:
  div Q, vf00.w, vf18.w      |  mul.xy vf22, vf22, Q
  mtir vi02, vf16.w          |  addw.z vf22, vf00, vf00
  mtir vi07, vf26.w          |  mul.xyz vf15, vf14, vf14
  nop                        |  mul.xyz vf13, vf17, vf23
  mtir vi01, vf11.z          |  addw.xy vf22, vf22, vf03
  iadd vi02, vi02, vi12      |  nop
  iadd vi06, vi02, vi13      |  addy.x vf15, vf15, vf15
  iadd vi07, vi07, vi12      |  nop
  sqi.xyzw vf22, vi11        |  addy.x vf13, vf13, vf13
  lq.xyz vf30, 770(vi01)     |  nop
  iadd vi15, vi07, vi13      |  addz.x vf15, vf15, vf15
  sq.xyzw vf01, 1(vi06)      |  nop
  sq.xyzw vf01, 1(vi15)      |  addz.x vf13, vf13, vf13
  sq.xyzw vf30, 1(vi02)      |  mul.xyz vf19, vf18, Q
  rsqrt Q, vf02.w, vf15.x    |  mul.xyz vf28, vf26, Q
  sq.xyzw vf30, 1(vi07)      |  mul.xyz vf22, vf22, Q
  nop                        |  mulx.xy vf23, vf23, vf13
  nop                        |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi02)      |  mula.xy ACC, vf10, vf17
  sq.xyzw vf28, 0(vi07)      |  mulaw.zw ACC, vf10, vf00
  sq.xyzw vf22, 0(vi06)      |  add.xy vf23, vf23, vf14
  sq.xyzw vf22, 0(vi15)      |  nop
  sq.xyzw vf19, 2(vi02)      |  nop
  sq.xyzw vf19, 2(vi06)      |  maddz.xyzw vf18, vf09, vf17
  sq.xyzw vf19, 2(vi07)      |  nop
  nop                        |  nop
  sq.xyzw vf19, 2(vi15)      |  nop
  div Q, vf00.w, vf18.w      |  mul.xy vf23, vf23, Q
  mtir vi02, vf17.w          |  addw.z vf23, vf00, vf00
  mtir vi07, vf27.w          |  nop
  nop                        |  nop
  mtir vi01, vf11.w          |  addw.xy vf23, vf23, vf03
  iadd vi02, vi02, vi12      |  nop
  iadd vi06, vi02, vi13      |  nop
  iadd vi07, vi07, vi12      |  nop
  sqi.xyzw vf23, vi11        |  nop
  lq.xyz vf30, 770(vi01)     |  nop
  iadd vi15, vi07, vi13      |  nop
  sq.xyzw vf01, 1(vi06)      |  nop
  sq.xyzw vf01, 1(vi15)      |  nop
  sq.xyzw vf30, 1(vi02)      |  mul.xyz vf19, vf18, Q
  nop                        |  mul.xyz vf28, vf27, Q
  sq.xyzw vf30, 1(vi07)      |  mul.xyz vf23, vf23, Q
  nop                        |  nop
  ilw.y vi14, 914(vi00)      |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi02)      |  nop
  sq.xyzw vf28, 0(vi07)      |  nop
  sq.xyzw vf23, 0(vi06)      |  nop
  sq.xyzw vf23, 0(vi15)      |  nop
  sq.xyzw vf19, 2(vi02)      |  nop
  sq.xyzw vf19, 2(vi06)      |  nop
  sq.xyzw vf19, 2(vi07)      |  nop
  ibne vi00, vi14, L39       |  nop
  sq.xyzw vf19, 2(vi15)      |  nop
  b L31                      |  nop
  nop                        |  nop
L30:
  nop                        |  nop
  div Q, vf00.w, vf18.w      |  mul.xy vf23, vf23, Q
  mtir vi02, vf17.w          |  addw.z vf23, vf00, vf00
  mtir vi07, vf27.w          |  mul.xyz vf15, vf14, vf14
  nop                        |  mul.xyz vf13, vf16, vf20
  mtir vi01, vf11.w          |  addw.xy vf23, vf23, vf03
  iadd vi02, vi02, vi12      |  nop
  iadd vi06, vi02, vi13      |  addy.x vf15, vf15, vf15
  iadd vi07, vi07, vi12      |  nop
  sqi.xyzw vf23, vi11        |  addy.x vf13, vf13, vf13
  lq.xyz vf30, 770(vi01)     |  nop
  iadd vi15, vi07, vi13      |  addz.x vf15, vf15, vf15
  sq.xyzw vf01, 1(vi06)      |  nop
  sq.xyzw vf01, 1(vi15)      |  addz.x vf13, vf13, vf13
  sq.xyzw vf30, 1(vi02)      |  mul.xyz vf19, vf18, Q
  rsqrt Q, vf02.w, vf15.x    |  mul.xyz vf28, vf27, Q
  sq.xyzw vf30, 1(vi07)      |  mul.xyz vf23, vf23, Q
  nop                        |  mulx.xy vf20, vf20, vf13
  nop                        |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi02)      |  mula.xy ACC, vf10, vf16
  sq.xyzw vf28, 0(vi07)      |  mulaw.zw ACC, vf10, vf00
  sq.xyzw vf23, 0(vi06)      |  add.xy vf20, vf20, vf14
  sq.xyzw vf23, 0(vi15)      |  nop
  sq.xyzw vf19, 2(vi02)      |  nop
  sq.xyzw vf19, 2(vi06)      |  maddz.xyzw vf18, vf09, vf16
  sq.xyzw vf19, 2(vi07)      |  nop
  lqi.xyzw vf11, vi10        |  nop
  sq.xyzw vf19, 2(vi15)      |  nop
  div Q, vf00.w, vf18.w      |  mul.xy vf20, vf20, Q
  mtir vi02, vf16.w          |  addw.z vf20, vf00, vf00
  mtir vi07, vf24.w          |  nop
  nop                        |  nop
  mtir vi01, vf11.x          |  addw.xy vf20, vf20, vf03
  iadd vi02, vi02, vi12      |  nop
  iadd vi06, vi02, vi13      |  nop
  iadd vi07, vi07, vi12      |  nop
  sqi.xyzw vf20, vi11        |  nop
  lq.xyz vf30, 770(vi01)     |  nop
  iadd vi15, vi07, vi13      |  nop
  sq.xyzw vf01, 1(vi06)      |  nop
  sq.xyzw vf01, 1(vi15)      |  nop
  sq.xyzw vf30, 1(vi02)      |  mul.xyz vf19, vf18, Q
  nop                        |  mul.xyz vf28, vf24, Q
  sq.xyzw vf30, 1(vi07)      |  mul.xyz vf20, vf20, Q
  nop                        |  nop
  ilw.y vi14, 914(vi00)      |  ftoi4.xyz vf19, vf19
  sq.xyzw vf28, 0(vi02)      |  nop
  sq.xyzw vf28, 0(vi07)      |  nop
  sq.xyzw vf20, 0(vi06)      |  nop
  sq.xyzw vf20, 0(vi15)      |  nop
  sq.xyzw vf19, 2(vi02)      |  nop
  sq.xyzw vf19, 2(vi06)      |  nop
  sq.xyzw vf19, 2(vi07)      |  nop
  ibne vi00, vi14, L39       |  nop
  sq.xyzw vf19, 2(vi15)      |  nop
  b L31                      |  nop
  nop                        |  nop
L31:
  ilw.z vi14, 913(vi00)      |  nop
  iaddi vi08, vi08, -0x1     |  nop
  b L33                      |  nop
  nop                        |  nop
L32:
  ilw.z vi14, 913(vi00)      |  nop
  iaddi vi08, vi08, -0x2     |  nop
  iaddi vi09, vi09, -0x4     |  nop
  nop                        |  nop
L33:
  ibeq vi00, vi14, L36       |  nop
  nop                        |  nop
  lq.w vf14, 898(vi00)       |  subw.w vf13, vf13, vf13
  iadd vi14, vi14, vi12      |  addw.z vf28, vf00, vf00
  lqi.xyz vf29, vi08         |  nop
  lqi.xyzw vf20, vi09        |  nop
  lqi.xyz vf14, vi09         |  nop
  lqi.xyzw vf24, vi09        |  nop
  nop                        |  nop
  nop                        |  mulw.xyz vf20, vf20, vf29
  nop                        |  mulax.xyz ACC, vf02, vf29
  nop                        |  madday.xyz ACC, vf03, vf29
  nop                        |  maddz.xyz vf17, vf04, vf29
  nop                        |  add.xyzw vf20, vf20, vf14
  nop                        |  nop
  nop                        |  nop
  nop                        |  nop
  nop                        |  mulaw.xyzw ACC, vf08, vf00
  nop                        |  maddax.xyzw ACC, vf05, vf20
  nop                        |  madday.xyzw ACC, vf06, vf20
  nop                        |  maddz.xyz vf20, vf07, vf20
  nop                        |  subw.z vf17, vf17, vf00
  nop                        |  nop
  nop                        |  nop
  nop                        |  nop
  nop                        |  nop
  nop                        |  nop
  nop                        |  mul.xyz vf13, vf17, vf20
  lqi.xyz vf29, vi08         |  nop
  nop                        |  nop
  nop                        |  nop
  esum.xyzw P, vf13          |  nop
  lqi.xyzw vf21, vi09        |  nop
  lqi.xyz vf14, vi09         |  nop
  lqi.xyzw vf25, vi09        |  nop
  nop                        |  nop
  nop                        |  nop
  nop                        |  mulw.xyz vf21, vf21, vf29
  nop                        |  nop
  nop                        |  mulax.xyz ACC, vf02, vf29
  nop                        |  mulz.xyz vf11, vf20, vf17
  nop                        |  add.xyzw vf21, vf21, vf14
  nop                        |  madday.xyz ACC, vf03, vf29
  nop                        |  maddz.xyz vf18, vf04, vf29
  nop                        |  nop
  esadd.xyz P, vf11          |  nop
  mfp.w vf12, P              |  nop
  nop                        |  mulaw.xyzw ACC, vf08, vf00
  nop                        |  maddax.xyzw ACC, vf05, vf21
  nop                        |  madday.xyzw ACC, vf06, vf21
  nop                        |  maddz.xyz vf21, vf07, vf21
  nop                        |  subw.z vf18, vf18, vf00
  nop                        |  nop
  nop                        |  nop
  nop                        |  nop
  nop                        |  nop
  nop                        |  nop
  mfp.w vf04, P              |  mul.xyz vf13, vf18, vf21
  lqi.xyz vf29, vi08         |  nop
  nop                        |  nop
  lqi.xyz vf11, vi10         |  nop
  esum.xyzw P, vf13          |  nop
  lqi.xyzw vf22, vi09        |  mulaz.xy ACC, vf20, vf17
  lqi.xyz vf14, vi09         |  nop
  lqi.xyzw vf26, vi09        |  nop
  mtir vi01, vf11.x          |  nop
  mtir vi02, vf11.y          |  mulw.xyz vf22, vf22, vf29
  mtir vi03, vf11.z          |  maddw.xy vf28, vf17, vf12
  rsqrt Q, vf02.w, vf04.w    |  nop
  nop                        |  mulax.xyz ACC, vf02, vf29
  nop                        |  add.xyzw vf22, vf22, vf14
  nop                        |  mulz.xyz vf11, vf21, vf18
  nop                        |  madday.xyz ACC, vf03, vf29
  nop                        |  maddz.xyz vf17, vf04, vf29
L34:
  mtir vi04, vf20.w          |  mulaw.zw ACC, vf10, vf00
  esadd.xyz P, vf11          |  mula.xy ACC, vf10, vf20
  mfp.w vf12, P              |  maddz.xyzw vf20, vf09, vf20
  lq.xyz vf30, 770(vi01)     |  mulaw.xyzw ACC, vf08, vf00
  lq.xyz vf15, 770(vi02)     |  maddax.xyzw ACC, vf05, vf22
  lq.xyz vf16, 770(vi03)     |  madday.xyzw ACC, vf06, vf22
  mtir vi01, vf24.z          |  maddz.xyz vf22, vf07, vf22
  mtir vi02, vf24.w          |  subw.z vf17, vf17, vf00
  iadd vi05, vi04, vi13      |  mul.xy vf28, vf28, Q
  sq.xyzw vf01, 1(vi05)      |  mulaw.xyzw ACC, vf15, vf31
  lq.xy vf12, 919(vi01)      |  maddaw.xyzw ACC, vf16, vf31
  lq.xy vf14, 919(vi02)      |  maddz.xyz vf30, vf30, vf31
  div Q, vf00.w, vf20.w      |  addw.xy vf28, vf28, vf03
  mfp.w vf04, P              |  mul.xyz vf13, vf17, vf22
  lqi.xyz vf29, vi08         |  mulay.xy ACC, vf12, vf31
  sq.xyzw vf30, 1(vi04)      |  madday.xy ACC, vf14, vf31
  lqi.xyz vf11, vi10         |  maddx.xy vf28, vf28, vf31
  esum.xyzw P, vf13          |  addw.z vf24, vf00, vf00
  lqi.xyzw vf23, vi09        |  mulaz.xy ACC, vf21, vf18
  lqi.xyz vf14, vi09         |  mul.xyz vf20, vf20, Q
  lqi.xyzw vf27, vi09        |  mul.xyz vf12, vf28, Q
  mtir vi01, vf11.x          |  mul.xyz vf24, vf24, Q
  mtir vi02, vf11.y          |  mulw.xyz vf23, vf23, vf29
  mtir vi03, vf11.z          |  ftoi4.xyz vf19, vf20
  rsqrt Q, vf02.w, vf04.w    |  maddw.xy vf28, vf18, vf12
  sq.xyzw vf12, 0(vi05)      |  mulax.xyz ACC, vf02, vf29
  sq.xyzw vf24, 0(vi04)      |  add.xyzw vf23, vf23, vf14
  sq.xyzw vf19, 2(vi04)      |  mulz.xyz vf11, vf22, vf17
  ibeq vi14, vi04, L35       |  madday.xyz ACC, vf03, vf29
  sq.xyzw vf19, 2(vi05)      |  maddz.xyz vf18, vf04, vf29
  mtir vi04, vf21.w          |  mulaw.zw ACC, vf10, vf00
  esadd.xyz P, vf11          |  mula.xy ACC, vf10, vf21
  mfp.w vf12, P              |  maddz.xyzw vf21, vf09, vf21
  lq.xyz vf30, 770(vi01)     |  mulaw.xyzw ACC, vf08, vf00
  lq.xyz vf15, 770(vi02)     |  maddax.xyzw ACC, vf05, vf23
  lq.xyz vf16, 770(vi03)     |  madday.xyzw ACC, vf06, vf23
  mtir vi01, vf25.z          |  maddz.xyz vf23, vf07, vf23
  mtir vi02, vf25.w          |  subw.z vf18, vf18, vf00
  iadd vi05, vi04, vi13      |  mul.xy vf28, vf28, Q
  sq.xyzw vf01, 1(vi05)      |  mulaw.xyzw ACC, vf15, vf31
  lq.xy vf12, 919(vi01)      |  maddaw.xyzw ACC, vf16, vf31
  lq.xy vf14, 919(vi02)      |  maddz.xyz vf30, vf30, vf31
  div Q, vf00.w, vf21.w      |  addw.xy vf28, vf28, vf03
  mfp.w vf04, P              |  mul.xyz vf13, vf18, vf23
  lqi.xyz vf29, vi08         |  mulay.xy ACC, vf12, vf31
  sq.xyzw vf30, 1(vi04)      |  madday.xy ACC, vf14, vf31
  lqi.xyz vf11, vi10         |  maddx.xy vf28, vf28, vf31
  esum.xyzw P, vf13          |  addw.z vf25, vf00, vf00
  lqi.xyzw vf20, vi09        |  mulaz.xy ACC, vf22, vf17
  lqi.xyz vf14, vi09         |  mul.xyz vf21, vf21, Q
  lqi.xyzw vf24, vi09        |  mul.xyz vf12, vf28, Q
  mtir vi01, vf11.x          |  mul.xyz vf25, vf25, Q
  mtir vi02, vf11.y          |  mulw.xyz vf20, vf20, vf29
  mtir vi03, vf11.z          |  ftoi4.xyz vf19, vf21
  rsqrt Q, vf02.w, vf04.w    |  maddw.xy vf28, vf17, vf12
  sq.xyzw vf12, 0(vi05)      |  mulax.xyz ACC, vf02, vf29
  sq.xyzw vf25, 0(vi04)      |  add.xyzw vf20, vf20, vf14
  sq.xyzw vf19, 2(vi04)      |  mulz.xyz vf11, vf23, vf18
  ibeq vi14, vi04, L35       |  madday.xyz ACC, vf03, vf29
  sq.xyzw vf19, 2(vi05)      |  maddz.xyz vf17, vf04, vf29
  mtir vi04, vf22.w          |  mulaw.zw ACC, vf10, vf00
  esadd.xyz P, vf11          |  mula.xy ACC, vf10, vf22
  mfp.w vf12, P              |  maddz.xyzw vf22, vf09, vf22
  lq.xyz vf30, 770(vi01)     |  mulaw.xyzw ACC, vf08, vf00
  lq.xyz vf15, 770(vi02)     |  maddax.xyzw ACC, vf05, vf20
  lq.xyz vf16, 770(vi03)     |  madday.xyzw ACC, vf06, vf20
  mtir vi01, vf26.z          |  maddz.xyz vf20, vf07, vf20
  mtir vi02, vf26.w          |  subw.z vf17, vf17, vf00
  iadd vi05, vi04, vi13      |  mul.xy vf28, vf28, Q
  sq.xyzw vf01, 1(vi05)      |  mulaw.xyzw ACC, vf15, vf31
  lq.xy vf12, 919(vi01)      |  maddaw.xyzw ACC, vf16, vf31
  lq.xy vf14, 919(vi02)      |  maddz.xyz vf30, vf30, vf31
  div Q, vf00.w, vf22.w      |  addw.xy vf28, vf28, vf03
  mfp.w vf04, P              |  mul.xyz vf13, vf17, vf20
  lqi.xyz vf29, vi08         |  mulay.xy ACC, vf12, vf31
  sq.xyzw vf30, 1(vi04)      |  madday.xy ACC, vf14, vf31
  lqi.xyz vf11, vi10         |  maddx.xy vf28, vf28, vf31
  esum.xyzw P, vf13          |  addw.z vf26, vf00, vf00
  lqi.xyzw vf21, vi09        |  mulaz.xy ACC, vf23, vf18
  lqi.xyz vf14, vi09         |  mul.xyz vf22, vf22, Q
  lqi.xyzw vf25, vi09        |  mul.xyz vf12, vf28, Q
  mtir vi01, vf11.x          |  mul.xyz vf26, vf26, Q
  mtir vi02, vf11.y          |  mulw.xyz vf21, vf21, vf29
  mtir vi03, vf11.z          |  ftoi4.xyz vf19, vf22
  rsqrt Q, vf02.w, vf04.w    |  maddw.xy vf28, vf18, vf12
  sq.xyzw vf12, 0(vi05)      |  mulax.xyz ACC, vf02, vf29
  sq.xyzw vf26, 0(vi04)      |  add.xyzw vf21, vf21, vf14
  sq.xyzw vf19, 2(vi04)      |  mulz.xyz vf11, vf20, vf17
  ibeq vi14, vi04, L35       |  madday.xyz ACC, vf03, vf29
  sq.xyzw vf19, 2(vi05)      |  maddz.xyz vf18, vf04, vf29
  mtir vi04, vf23.w          |  mulaw.zw ACC, vf10, vf00
  esadd.xyz P, vf11          |  mula.xy ACC, vf10, vf23
  mfp.w vf12, P              |  maddz.xyzw vf23, vf09, vf23
  lq.xyz vf30, 770(vi01)     |  mulaw.xyzw ACC, vf08, vf00
  lq.xyz vf15, 770(vi02)     |  maddax.xyzw ACC, vf05, vf21
  lq.xyz vf16, 770(vi03)     |  madday.xyzw ACC, vf06, vf21
  mtir vi01, vf27.z          |  maddz.xyz vf21, vf07, vf21
  mtir vi02, vf27.w          |  subw.z vf18, vf18, vf00
  iadd vi05, vi04, vi13      |  mul.xy vf28, vf28, Q
  sq.xyzw vf01, 1(vi05)      |  mulaw.xyzw ACC, vf15, vf31
  lq.xy vf12, 919(vi01)      |  maddaw.xyzw ACC, vf16, vf31
  lq.xy vf14, 919(vi02)      |  maddz.xyz vf30, vf30, vf31
  div Q, vf00.w, vf23.w      |  addw.xy vf28, vf28, vf03
  mfp.w vf04, P              |  mul.xyz vf13, vf18, vf21
  lqi.xyz vf29, vi08         |  mulay.xy ACC, vf12, vf31
  sq.xyzw vf30, 1(vi04)      |  madday.xy ACC, vf14, vf31
  lqi.xyz vf11, vi10         |  maddx.xy vf28, vf28, vf31
  esum.xyzw P, vf13          |  addw.z vf27, vf00, vf00
  lqi.xyzw vf22, vi09        |  mulaz.xy ACC, vf20, vf17
  lqi.xyz vf14, vi09         |  mul.xyz vf23, vf23, Q
  lqi.xyzw vf26, vi09        |  mul.xyz vf12, vf28, Q
  mtir vi01, vf11.x          |  mul.xyz vf27, vf27, Q
  mtir vi02, vf11.y          |  mulw.xyz vf22, vf22, vf29
  mtir vi03, vf11.z          |  ftoi4.xyz vf19, vf23
  rsqrt Q, vf02.w, vf04.w    |  maddw.xy vf28, vf17, vf12
  sq.xyzw vf12, 0(vi05)      |  mulax.xyz ACC, vf02, vf29
  sq.xyzw vf27, 0(vi04)      |  add.xyzw vf22, vf22, vf14
  sq.xyzw vf19, 2(vi04)      |  mulz.xyz vf11, vf21, vf18
  ibne vi14, vi04, L34       |  madday.xyz ACC, vf03, vf29
  sq.xyzw vf19, 2(vi05)      |  maddz.xyz vf17, vf04, vf29
L35:
  ilw.w vi14, 913(vi00)      |  nop
  nop                        |  nop
  nop                        |  nop
  nop                        |  nop
  ibeq vi00, vi14, L39       |  nop
  iaddi vi10, vi10, -0x1     |  nop
  iaddi vi08, vi08, -0x3     |  nop
  b L36                      |  nop
  iaddi vi09, vi09, -0x9     |  nop
  isw.x vi00, 976(vi00)      |  nop
  isw.x vi01, 977(vi00)      |  nop
  isw.x vi02, 978(vi00)      |  nop
  isw.x vi03, 979(vi00)      |  nop
  isw.x vi04, 980(vi00)      |  nop
  isw.x vi05, 981(vi00)      |  nop
  isw.x vi06, 982(vi00)      |  nop
  isw.x vi07, 983(vi00)      |  nop
  isw.x vi08, 984(vi00)      |  nop
  isw.x vi09, 985(vi00)      |  nop
  isw.x vi10, 986(vi00)      |  nop
  isw.x vi11, 987(vi00)      |  nop
  isw.x vi12, 988(vi00)      |  nop
  isw.x vi13, 989(vi00)      |  nop
  isw.x vi14, 990(vi00)      |  nop
  isw.x vi15, 991(vi00)      |  nop
  sq.xyzw vf00, 992(vi00)    |  nop
  sq.xyzw vf01, 993(vi00)    |  nop
  sq.xyzw vf02, 994(vi00)    |  nop
  sq.xyzw vf03, 995(vi00)    |  nop
  sq.xyzw vf04, 996(vi00)    |  nop
  sq.xyzw vf05, 997(vi00)    |  nop
  sq.xyzw vf06, 998(vi00)    |  nop
  sq.xyzw vf07, 999(vi00)    |  nop
  sq.xyzw vf08, 1000(vi00)   |  nop
  sq.xyzw vf09, 1001(vi00)   |  nop
  sq.xyzw vf10, 1002(vi00)   |  nop
  sq.xyzw vf11, 1003(vi00)   |  nop
  sq.xyzw vf12, 1004(vi00)   |  nop
  sq.xyzw vf13, 1005(vi00)   |  nop
  sq.xyzw vf14, 1006(vi00)   |  nop
  sq.xyzw vf15, 1007(vi00)   |  nop
  sq.xyzw vf16, 1008(vi00)   |  nop
  sq.xyzw vf17, 1009(vi00)   |  nop
  sq.xyzw vf18, 1010(vi00)   |  nop
  sq.xyzw vf19, 1011(vi00)   |  nop
  sq.xyzw vf20, 1012(vi00)   |  nop
  sq.xyzw vf21, 1013(vi00)   |  nop
  sq.xyzw vf22, 1014(vi00)   |  nop
  sq.xyzw vf23, 1015(vi00)   |  nop
  sq.xyzw vf24, 1016(vi00)   |  nop
  sq.xyzw vf25, 1017(vi00)   |  nop
  sq.xyzw vf26, 1018(vi00)   |  nop
  sq.xyzw vf27, 1019(vi00)   |  nop
  sq.xyzw vf28, 1020(vi00)   |  nop
  sq.xyzw vf29, 1021(vi00)   |  nop
  sq.xyzw vf30, 1022(vi00)   |  nop :e
  sq.xyzw vf31, 1023(vi00)   |  nop
L36:
  ilw.w vi14, 913(vi00)      |  nop
  lq.w vf04, 898(vi00)       |  nop
  nop                        |  nop
  nop                        |  addw.z vf18, vf00, vf00
  iadd vi14, vi14, vi12      |  nop
  nop                        |  nop
  lqi.xyzw vf23, vi09        |  nop
  lqi.xyz vf29, vi08         |  nop
  nop                        |  nop
  lqi.xyzw vf21, vi09        |  nop
  lqi.xyzw vf26, vi09        |  mulw.xyz vf23, vf23, vf29
  nop                        |  mulax.xyz ACC, vf02, vf29
  nop                        |  madday.xyzw ACC, vf03, vf29
  nop                        |  maddz.xyzw vf17, vf04, vf29
  nop                        |  add.xyz vf23, vf23, vf21
  nop                        |  addw.w vf23, vf23, vf04
  nop                        |  mulaw.xyzw ACC, vf08, vf00
  nop                        |  subw.z vf17, vf17, vf00
  nop                        |  maddax.xyzw ACC, vf05, vf23
  nop                        |  madday.xyzw ACC, vf06, vf23
  nop                        |  maddz.xyz vf23, vf07, vf23
  nop                        |  nop
  nop                        |  nop
  nop                        |  nop
  nop                        |  nop
  nop                        |  mul.xyz vf13, vf17, vf23
  nop                        |  mulz.xyz vf14, vf23, vf17
  nop                        |  nop
  nop                        |  nop
  nop                        |  addy.x vf13, vf13, vf13
  nop                        |  mul.xyz vf14, vf14, vf14
  nop                        |  nop
  nop                        |  nop
  nop                        |  addz.x vf13, vf13, vf13
  nop                        |  addy.x vf14, vf14, vf14
  nop                        |  nop
  nop                        |  nop
  nop                        |  nop
  nop                        |  mulax.xy ACC, vf17, vf13
  nop                        |  addz.x vf14, vf14, vf14
  nop                        |  nop
  lqi.xyzw vf24, vi09        |  maddz.xy vf18, vf23, vf17
  lqi.xyz vf29, vi08         |  nop
  rsqrt Q, vf02.w, vf14.x    |  nop
  lqi.xyzw vf22, vi09        |  nop
  lqi.xyzw vf27, vi09        |  mulw.xyz vf24, vf24, vf29
  nop                        |  mulax.xyz ACC, vf02, vf29
  lqi.xyzw vf11, vi10        |  madday.xyzw ACC, vf03, vf29
  nop                        |  maddz.xyzw vf17, vf04, vf29
  nop                        |  add.xyz vf24, vf24, vf22
  nop                        |  addw.w vf24, vf24, vf04
  nop                        |  mulaw.xyzw ACC, vf08, vf00
  nop                        |  subw.z vf17, vf17, vf00
  nop                        |  maddax.xyzw ACC, vf05, vf24
  nop                        |  madday.xyzw ACC, vf06, vf24
  mtir vi01, vf26.z          |  maddz.xyz vf24, vf07, vf24
  nop                        |  mula.xy ACC, vf10, vf23
  mtir vi02, vf26.w          |  mulaw.zw ACC, vf10, vf00
  nop                        |  mul.xy vf18, vf18, Q
  lq.xy vf12, 919(vi01)      |  maddz.xyzw vf25, vf09, vf23
  nop                        |  mul.xyz vf13, vf17, vf24
  lq.xy vf15, 919(vi02)      |  mulz.xyz vf14, vf24, vf17
  nop                        |  addw.xy vf18, vf18, vf03
  div Q, vf00.w, vf25.w      |  mulay.xy ACC, vf12, vf31
  nop                        |  addy.x vf13, vf13, vf13
  nop                        |  mul.xyz vf14, vf14, vf14
  nop                        |  madday.xy ACC, vf15, vf31
L37:
  nop                        |  addw.z vf26, vf00, vf00
  mtir vi02, vf11.y          |  maddx.xy vf18, vf18, vf31
  mtir vi01, vf11.x          |  addz.x vf13, vf13, vf13
  mtir vi03, vf11.z          |  addy.x vf14, vf14, vf14
  mtir vi04, vf23.w          |  mul.xyz vf25, vf25, Q
  lq.xyz vf11, 770(vi02)     |  mul.xyz vf16, vf18, Q
  lq.xyz vf30, 770(vi01)     |  mul.xyz vf12, vf26, Q
  lq.xyz vf20, 770(vi03)     |  mulax.xy ACC, vf17, vf13
  mtir vi06, vf21.w          |  addz.x vf14, vf14, vf14
  iadd vi05, vi04, vi13      |  ftoi4.xyz vf19, vf25
  lqi.xyzw vf23, vi09        |  maddz.xy vf18, vf24, vf17
  lqi.xyz vf29, vi08         |  mulaw.xyzw ACC, vf11, vf31
  rsqrt Q, vf02.w, vf14.x    |  maddaw.xyzw ACC, vf20, vf31
  lqi.xyzw vf21, vi09        |  maddz.xyz vf30, vf30, vf31
  lqi.xyzw vf26, vi09        |  mulw.xyz vf23, vf23, vf29
  iadd vi06, vi06, vi12      |  mulax.xyz ACC, vf02, vf29
  lqi.xyzw vf11, vi10        |  madday.xyzw ACC, vf03, vf29
  iadd vi07, vi06, vi13      |  maddz.xyzw vf17, vf04, vf29
  sq.xyzw vf12, 0(vi04)      |  add.xyz vf23, vf23, vf21
  sq.xyzw vf30, 1(vi04)      |  addw.w vf23, vf23, vf04
  sq.xyzw vf19, 2(vi04)      |  mulaw.xyzw ACC, vf08, vf00
  sq.xyzw vf16, 0(vi05)      |  subw.z vf17, vf17, vf00
  sq.xyzw vf01, 1(vi05)      |  maddax.xyzw ACC, vf05, vf23
  sq.xyzw vf19, 2(vi05)      |  madday.xyzw ACC, vf06, vf23
  mtir vi01, vf27.z          |  maddz.xyz vf23, vf07, vf23
  sq.xyzw vf12, 0(vi06)      |  mula.xy ACC, vf10, vf24
  mtir vi02, vf27.w          |  mulaw.zw ACC, vf10, vf00
  sq.xyzw vf30, 1(vi06)      |  mul.xy vf18, vf18, Q
  lq.xy vf12, 919(vi01)      |  maddz.xyzw vf25, vf09, vf24
  sq.xyzw vf19, 2(vi06)      |  mul.xyz vf13, vf17, vf23
  lq.xy vf15, 919(vi02)      |  mulz.xyz vf14, vf23, vf17
  sq.xyzw vf16, 0(vi07)      |  addw.xy vf18, vf18, vf03
  div Q, vf00.w, vf25.w      |  mulay.xy ACC, vf12, vf31
  sq.xyzw vf01, 1(vi07)      |  addy.x vf13, vf13, vf13
  ibeq vi14, vi04, L38       |  mul.xyz vf14, vf14, vf14
  sq.xyzw vf19, 2(vi07)      |  madday.xy ACC, vf15, vf31
  nop                        |  addw.z vf27, vf00, vf00
  mtir vi02, vf11.y          |  maddx.xy vf18, vf18, vf31
  mtir vi01, vf11.x          |  addz.x vf13, vf13, vf13
  mtir vi03, vf11.z          |  addy.x vf14, vf14, vf14
  mtir vi04, vf24.w          |  mul.xyz vf25, vf25, Q
  lq.xyz vf11, 770(vi02)     |  mul.xyz vf16, vf18, Q
  lq.xyz vf30, 770(vi01)     |  mul.xyz vf12, vf27, Q
  lq.xyz vf20, 770(vi03)     |  mulax.xy ACC, vf17, vf13
  mtir vi06, vf22.w          |  addz.x vf14, vf14, vf14
  iadd vi05, vi04, vi13      |  ftoi4.xyz vf19, vf25
  lqi.xyzw vf24, vi09        |  maddz.xy vf18, vf23, vf17
  lqi.xyz vf29, vi08         |  mulaw.xyzw ACC, vf11, vf31
  rsqrt Q, vf02.w, vf14.x    |  maddaw.xyzw ACC, vf20, vf31
  lqi.xyzw vf22, vi09        |  maddz.xyz vf30, vf30, vf31
  lqi.xyzw vf27, vi09        |  mulw.xyz vf24, vf24, vf29
  iadd vi06, vi06, vi12      |  mulax.xyz ACC, vf02, vf29
  lqi.xyzw vf11, vi10        |  madday.xyzw ACC, vf03, vf29
  iadd vi07, vi06, vi13      |  maddz.xyzw vf17, vf04, vf29
  sq.xyzw vf12, 0(vi04)      |  add.xyz vf24, vf24, vf22
  sq.xyzw vf30, 1(vi04)      |  addw.w vf24, vf24, vf04
  sq.xyzw vf19, 2(vi04)      |  mulaw.xyzw ACC, vf08, vf00
  sq.xyzw vf16, 0(vi05)      |  subw.z vf17, vf17, vf00
  sq.xyzw vf01, 1(vi05)      |  maddax.xyzw ACC, vf05, vf24
  sq.xyzw vf19, 2(vi05)      |  madday.xyzw ACC, vf06, vf24
  mtir vi01, vf26.z          |  maddz.xyz vf24, vf07, vf24
  sq.xyzw vf12, 0(vi06)      |  mula.xy ACC, vf10, vf23
  mtir vi02, vf26.w          |  mulaw.zw ACC, vf10, vf00
  sq.xyzw vf30, 1(vi06)      |  mul.xy vf18, vf18, Q
  lq.xy vf12, 919(vi01)      |  maddz.xyzw vf25, vf09, vf23
  sq.xyzw vf19, 2(vi06)      |  mul.xyz vf13, vf17, vf24
  lq.xy vf15, 919(vi02)      |  mulz.xyz vf14, vf24, vf17
  sq.xyzw vf16, 0(vi07)      |  addw.xy vf18, vf18, vf03
  div Q, vf00.w, vf25.w      |  mulay.xy ACC, vf12, vf31
  sq.xyzw vf01, 1(vi07)      |  addy.x vf13, vf13, vf13
  ibeq vi14, vi04, L38       |  mul.xyz vf14, vf14, vf14
  sq.xyzw vf19, 2(vi07)      |  madday.xy ACC, vf15, vf31
  nop                        |  addw.z vf26, vf00, vf00
  mtir vi02, vf11.y          |  maddx.xy vf18, vf18, vf31
  mtir vi01, vf11.x          |  addz.x vf13, vf13, vf13
  mtir vi03, vf11.z          |  addy.x vf14, vf14, vf14
  mtir vi04, vf23.w          |  mul.xyz vf25, vf25, Q
  lq.xyz vf11, 770(vi02)     |  mul.xyz vf16, vf18, Q
  lq.xyz vf30, 770(vi01)     |  mul.xyz vf12, vf26, Q
  lq.xyz vf20, 770(vi03)     |  mulax.xy ACC, vf17, vf13
  mtir vi06, vf21.w          |  addz.x vf14, vf14, vf14
  iadd vi05, vi04, vi13      |  ftoi4.xyz vf19, vf25
  lqi.xyzw vf23, vi09        |  maddz.xy vf18, vf24, vf17
  lqi.xyz vf29, vi08         |  mulaw.xyzw ACC, vf11, vf31
  rsqrt Q, vf02.w, vf14.x    |  maddaw.xyzw ACC, vf20, vf31
  lqi.xyzw vf21, vi09        |  maddz.xyz vf30, vf30, vf31
  lqi.xyzw vf26, vi09        |  mulw.xyz vf23, vf23, vf29
  iadd vi06, vi06, vi12      |  mulax.xyz ACC, vf02, vf29
  lqi.xyzw vf11, vi10        |  madday.xyzw ACC, vf03, vf29
  iadd vi07, vi06, vi13      |  maddz.xyzw vf17, vf04, vf29
  sq.xyzw vf12, 0(vi04)      |  add.xyz vf23, vf23, vf21
  sq.xyzw vf30, 1(vi04)      |  addw.w vf23, vf23, vf04
  sq.xyzw vf19, 2(vi04)      |  mulaw.xyzw ACC, vf08, vf00
  sq.xyzw vf16, 0(vi05)      |  subw.z vf17, vf17, vf00
  sq.xyzw vf01, 1(vi05)      |  maddax.xyzw ACC, vf05, vf23
  sq.xyzw vf19, 2(vi05)      |  madday.xyzw ACC, vf06, vf23
  mtir vi01, vf27.z          |  maddz.xyz vf23, vf07, vf23
  sq.xyzw vf12, 0(vi06)      |  mula.xy ACC, vf10, vf24
  mtir vi02, vf27.w          |  mulaw.zw ACC, vf10, vf00
  sq.xyzw vf30, 1(vi06)      |  mul.xy vf18, vf18, Q
  lq.xy vf12, 919(vi01)      |  maddz.xyzw vf25, vf09, vf24
  sq.xyzw vf19, 2(vi06)      |  mul.xyz vf13, vf17, vf23
  lq.xy vf15, 919(vi02)      |  mulz.xyz vf14, vf23, vf17
  sq.xyzw vf16, 0(vi07)      |  addw.xy vf18, vf18, vf03
  div Q, vf00.w, vf25.w      |  mulay.xy ACC, vf12, vf31
  sq.xyzw vf01, 1(vi07)      |  addy.x vf13, vf13, vf13
  ibeq vi14, vi04, L38       |  mul.xyz vf14, vf14, vf14
  sq.xyzw vf19, 2(vi07)      |  madday.xy ACC, vf15, vf31
  nop                        |  addw.z vf27, vf00, vf00
  mtir vi02, vf11.y          |  maddx.xy vf18, vf18, vf31
  mtir vi01, vf11.x          |  addz.x vf13, vf13, vf13
  mtir vi03, vf11.z          |  addy.x vf14, vf14, vf14
  mtir vi04, vf24.w          |  mul.xyz vf25, vf25, Q
  lq.xyz vf11, 770(vi02)     |  mul.xyz vf16, vf18, Q
  lq.xyz vf30, 770(vi01)     |  mul.xyz vf12, vf27, Q
  lq.xyz vf20, 770(vi03)     |  mulax.xy ACC, vf17, vf13
  mtir vi06, vf22.w          |  addz.x vf14, vf14, vf14
  iadd vi05, vi04, vi13      |  ftoi4.xyz vf19, vf25
  lqi.xyzw vf24, vi09        |  maddz.xy vf18, vf23, vf17
  lqi.xyz vf29, vi08         |  mulaw.xyzw ACC, vf11, vf31
  rsqrt Q, vf02.w, vf14.x    |  maddaw.xyzw ACC, vf20, vf31
  lqi.xyzw vf22, vi09        |  maddz.xyz vf30, vf30, vf31
  lqi.xyzw vf27, vi09        |  mulw.xyz vf24, vf24, vf29
  iadd vi06, vi06, vi12      |  mulax.xyz ACC, vf02, vf29
  lqi.xyzw vf11, vi10        |  madday.xyzw ACC, vf03, vf29
  iadd vi07, vi06, vi13      |  maddz.xyzw vf17, vf04, vf29
  sq.xyzw vf12, 0(vi04)      |  add.xyz vf24, vf24, vf22
  sq.xyzw vf30, 1(vi04)      |  addw.w vf24, vf24, vf04
  sq.xyzw vf19, 2(vi04)      |  mulaw.xyzw ACC, vf08, vf00
  sq.xyzw vf16, 0(vi05)      |  subw.z vf17, vf17, vf00
  sq.xyzw vf01, 1(vi05)      |  maddax.xyzw ACC, vf05, vf24
  sq.xyzw vf19, 2(vi05)      |  madday.xyzw ACC, vf06, vf24
  mtir vi01, vf26.z          |  maddz.xyz vf24, vf07, vf24
  sq.xyzw vf12, 0(vi06)      |  mula.xy ACC, vf10, vf23
  mtir vi02, vf26.w          |  mulaw.zw ACC, vf10, vf00
  sq.xyzw vf30, 1(vi06)      |  mul.xy vf18, vf18, Q
  lq.xy vf12, 919(vi01)      |  maddz.xyzw vf25, vf09, vf23
  sq.xyzw vf19, 2(vi06)      |  mul.xyz vf13, vf17, vf24
  lq.xy vf15, 919(vi02)      |  mulz.xyz vf14, vf24, vf17
  sq.xyzw vf16, 0(vi07)      |  addw.xy vf18, vf18, vf03
  div Q, vf00.w, vf25.w      |  mulay.xy ACC, vf12, vf31
  sq.xyzw vf01, 1(vi07)      |  addy.x vf13, vf13, vf13
  ibne vi14, vi04, L37       |  mul.xyz vf14, vf14, vf14
  sq.xyzw vf19, 2(vi07)      |  madday.xy ACC, vf15, vf31
L38:
  b L39                      |  nop
  nop                        |  nop
  nop                        |  nop :e
  nop                        |  nop
L39:
  lq.xyzw vf01, 898(vi00)    |  nop
  ilw.z vi02, 914(vi00)      |  nop
  ilw.z vi03, 915(vi00)      |  nop
  iaddi vi01, vi12, -0x1     |  nop
  xgkick vi01                |  nop
  mr32.xyzw vf01, vf01       |  nop
  iaddi vi02, vi02, 0x1      |  nop
  iaddi vi03, vi03, 0x1      |  nop
  isw.z vi02, 914(vi00)      |  nop
  isw.z vi03, 915(vi00)      |  nop :e
  sq.xyzw vf01, 898(vi00)    |  nop

```