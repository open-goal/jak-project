# Shrub EE code notes

## Looking through village1-vis.asm
The "prototype" main class is `prototype-bucket-shrub`. These are stored in a `prototype-inline-array-shrub`.

Example of a `prototype-bucket-shrub`:
```
    .type prototype-bucket-shrub
    .word L22404
    .word 0x0
    .word 0x20025
    .word L4630
    .word L4604
    .word L4617
    .word L4641
    .word 0x48a00000
    .word 0x48200000
```

The `.word L22404` is a string `"palmplant-top.mb"`.
The four `L4630`, `L4604`, `L4617`, `L4641` are the 4 `geometry`.  In this case, it looks like each is a different kind, but I don't think we can make any assumptions about this.

### Highest Detail `prototype-generic-shrub`
For now, let's ignore this because we don't have a generic renderer. It also really looks like no shrubs ever draw with generic in-game, but I could be wrong. It looks like every single shrub has this as their first geomery.

### 2nd Highest `prototype-shrubbery`
I think this is the "normal" shrub. Each prototype is made up of one or more `shrubbery`. Again, every shrub has this.

### 3rd Highest `prototype-trans-shrubbery`
Based on PCSX2, it looks like these are usually lower-res models that fade out as you move away.  Like the 2nd, they are made of `shrubbery`, and every shrub has one.

### 4th Highest `billboard`
These are the lowest res, always face toward the camera. Each `billboard` is just a single thing - no sub-parts. I suspect that each prototype can become a single rectangle "billboard", no matter how complicated the original shrub is.

I think we should work on `prototype-shrubbery` (and possibly the transparent one) first.  Maybe we can leave out the billboard and claim that we're "increasing the level of detail" :).

## Looking through `shrubbery.gc` decomp
Doing reverse order in the `ir2.asm` file.

### `login billboard`
No surprise here, billboard is a single rectangle, it can have only one texture, and there's exactly one `adgif-shader-login`.

### `mem-usage-shrub-walk`
This function recursively iterates over a `draw-node` tree, computing memory usage, and is a good example of how these `draw-node` trees work.  Each `draw-node` has a sphere.  Inside this sphere are between 1 and 8 "children", which are in an `inline-array`.  The children are usually `draw-node`, except for the leaf nodes, which are some other drawable type. In this case, they are `instance-shrubbery`. This is used for frustum culling. If a `draw-node` sphere is outside of the view of the camera, you can just skip all instances in that `draw-node`.

Unlike a normal tree, they rarely refer to a single `draw-node`, but instead a group of between 1 and 8 draw-nodes.  These are passed around as a `draw-node` (the first in the group) and an integer (the number in the group). This might seem weird, but simplifies the iteration logic. (if you don't know the type/size of the leaf node, you can't iterate over an inline-array of them, but you could call some virtual function that expects the object to be the first one in an `inline-array` with a size argument.)

This is how to recursively iterate over `draw-node` tree:
```lisp
    (dotimes (s1-0 arg1)
      (let ((a1-2 (-> s2-0 child-count)))
        (cond
          ((logtest? (-> s2-0 flags) 1) ;; flag means that we aren't a leaf.
           (mem-usage-shrub-walk (the-as draw-node (-> s2-0 child)) (the-as int a1-2) arg2 arg3)
           )
          (else
          	;; you are at an inline-array of leaves.
          	;; (-> s2-0 child) is an inline-array of a1-2 leaves
          	;; a1-2 is between 1 and 8.
          	)
          )
        )
        ;; move on to the next child draw node.
        (&+! s2-0 32)
      )
```

NOTE: `arg1` should be an int here.

We learned here that `shrub` does use the `draw-node` BVH system. (this was unclear because `shrub` does not appear to use the compressed visibility string based vis system, which is often used simultaneously with `draw-node`).

### `login generic-shrub-fragment`
`generic-shrub-fragment`'s have a variable number of textures. The `cnt-qwc` field refers to the total size of the `textures` array, as each `adgif-shader` is 5 qw.

### `login prototype-shrubbery`
From village1-vis, we know this is just logging in `shrubbery`s.

### `asize-of prototype-shrubbery`
The size calculation makes sense for an inline-array of `shrubbery`, with 1 `shrubbery` being inside the type, and the rest running off the end of the array.

### `login prototype-generic-shrub`
No surprises. Confirms that these aren't inline arrays (and we could tell from village1-vis anyway)

### `login shrubbery`
Gives us some idea of the layout of the "header". The first `u32` is the number of textures / 2.  Each `shrubbery` may have multiple textures.

### `login drawable-tree-instance-shrub`
Nothing interesting

### `shrub-num-tris`
This tells us the number of triangles in a `shrubbery` is `header[2] - 2*header[1]`. One of the most popular GS formats is "triangle strip".  The number of triangles in a triangle strip is `num_verts - 2`.  So, very likely `header[2]` is the total number of vertices, and `header[1]` is the number of strips

### `shrub-make-perspective-matrix`
This probably makes the matrix used to transform shrubbery. I'm not sure why yet, but they take the normal transformation (`camera-temp`) and modify it a bit. We may be able to get away with ignoring this. Ideally we just get the shrub positions (in the world coordinate system, just like with TIE/TFRAG), and then use the same logic we used for TIE/TFRAG.

### `shrub-init-view-data`.
Note: change `texture-giftag` in `shrub-view-data` to a `gs-gif-tag`.

This populates fields of a `shrub-view-data`, which will probably end up in VU memory.

There's two things here that are suspicious. First:
```lisp
  (set! (-> arg0 texture-giftag tag) (new 'static 'gif-tag64 :nloop #x1 :nreg #x4))
  (set! (-> arg0 texture-giftag regs) (new 'static 'gif-tag-regs
                                        :regs0 (gif-reg-id a+d)
                                        :regs1 (gif-reg-id a+d)
                                        :regs2 (gif-reg-id a+d)
                                        :regs3 (gif-reg-id a+d)
                                        )
        )
```
usually an `adgif-shader` is 5 qw's of a+d data. This sets up 4.  The fifth one is usually `gs-alpha`, and it seems reasonable that they could set this once, then not bother setting it again.

Second, they put `#x40a00000` in the 3rd word of the giftag:
```
(set! (-> arg0 texture-giftag word 3) (the-as uint #x40a00000))
```
as far as I can tell the GIF will ignore this. They might use it as a constant `5.0`. But it seems weird to put it here.

If you change the `score` so it uses the named fields, the rest of the constants are:
```
  (set! (-> arg0 tex-start-ptr) (the-as int 25167696.0))
  (set! (-> arg0 mtx-buf-ptr) (the-as int 8388608.0))
  (set! (-> arg0 fog-0) (-> *math-camera* pfog0))
  (set! (-> arg0 fog-1) (-> *math-camera* pfog1))
  (set! (-> arg0 fog-clamp x) (-> *math-camera* fog-min))
  (set! (-> arg0 fog-clamp y) (-> *math-camera* fog-max))
```
the fog stuff seems reasonable to me (it's involved in computing the perspective division and the GS fog coefficient, don't worry for now).

The `buf-ptr` stuff is very likely this awful trick they do. The VU doesn't have good instructions for common integer operations (like you'd use to mainpulate memory addresses), so they find floating point values, then when added/subtracted in certain ways, have the lower 16 bits of the float equal to the VU memory address.  Gross. Maybe the `5.0` is also part of this.


### `shrub-upload-view-data`
Note: I changed the type casts:
```
  "shrub-upload-view-data": [
    [[3, 16], "a0", "dma-packet"]
  ],

```

and got a very typical upload.

```
(defun shrub-upload-view-data ((arg0 dma-buffer))
  (let ((s5-0 3))
    (let* ((v1-0 arg0)
           (a0-1 (the-as object (-> v1-0 base)))
           )
      (set! (-> (the-as dma-packet a0-1) dma) (new 'static 'dma-tag :id (dma-tag-id cnt) :qwc s5-0))
      (set! (-> (the-as dma-packet a0-1) vif0) (new 'static 'vif-tag :imm #x404 :cmd (vif-cmd stcycl)))
      (set! (-> (the-as dma-packet a0-1) vif1) (new 'static 'vif-tag :cmd (vif-cmd unpack-v4-32) :num s5-0))
      (set! (-> v1-0 base) (&+ (the-as pointer a0-1) 16))
      )
    (shrub-init-view-data (the-as shrub-view-data (-> arg0 base)))
    (&+! (-> arg0 base) (* s5-0 16))
    )
  #f
  )
```

- `s5-0` this is the number of quadwords they will upload.
- `dma-tag`: `cnt`, means the data will come after the tag (and the next thing comes after that). `qwc` makes sense
- As part of a DMA tag, you get 2 VIF tags "for free". These are sent to the VIF.
- `vif0`: the `stcycl` sets the `cl`/`wl` register of VIF to `0x404` which is the mode for just copying stuff like normal (doesn't skip data)
- `vif1`: the `unpack` command means copy data to data memory. The `v4-32` is the mode for copying quadwords.

The `0x404 stcycl` and `v4-32` format is used if you just want to copy memory exactly, so it shows up a lot.  The other formats can do weird unpacking stuff, but are less common.


### `shrub-time`
Not really sure what this is. I noticed in PCSX2 there's a `time:` that shows up in instance info. Maybe it computes the number of VU cycles.


### `shrub-do-init-frame`
This adds stuff to the DMA list to get the hardware ready for shrub.

Note:
```
  "shrub-do-init-frame": [
    [[10, 21], "a0", "dma-packet"],
    [[24, 29], "a0", "dma-packet"],
    [33, "v1", "(pointer vif-tag)"],
    [[35, 41], "v1", "(pointer uint32)"],
    [42, "v1", "(pointer vif-tag)"],
    [[43, 51], "v1", "(pointer uint32)"],
    [52, "v1", "(pointer vif-tag)"],
    [54, "v1", "(pointer uint32)"]
  ],
```
All the data gets sent to the VIF. You can check the VIFcode reference part of the manual to see how long the data for each vifcode is.

upload the shrub program. In the PC port we usually set the VU program definitions to have a size of 0, then this function will do nothing.
```
(dma-buffer-add-vu-function arg0 shrub-vu1-block 1)
```

upload the shrub view data:
```
(shrub-upload-view-data arg0)
```

Add a DMA tag. Note that `qwc` is zero so this one just sends the two `vif` tags that are part of the dma tag:
```
    (set! (-> (the-as dma-packet a0-3) dma) (new 'static 'dma-tag :id (dma-tag-id cnt)))
    (set! (-> (the-as dma-packet a0-3) vif0) (new 'static 'vif-tag :cmd (vif-cmd mscalf) :msk #x1 :imm #x0))
    (set! (-> (the-as dma-packet a0-3) vif1) (new 'static 'vif-tag :cmd (vif-cmd flushe) :msk #x1))
    (set! (-> v1-0 base) (&+ (the-as pointer a0-3) 16))
```
The `mscalf` runs a VU1 program.  The `imm` is 0, so it runs starting at the beginning of VU1 program memory. Let's ignore this for now. It's probably an initialization program that sets up the VU memory and registers.  We'll look at it when we get to the actual drawing.

The `flushe` waits for that program to end.

Next it sets the `strow`/`stcol`/`stmask` registers of the VIF. These are used for unpacking settings.

### `shrub-init-frame`
Calls `shrub-do-init-frame` then sets the `gs-test` register.

### `shrub-upload-model`
Seems to upload a model
```
(function shrubbery dma-buffer int symbol)

and


  "shrub-upload-model": [
    [[17, 26], "a3", "dma-packet"],
    [[33, 41], "a0", "dma-packet"],
    [[47, 55], "a0", "dma-packet"]
  ],
```

The model data upload:
```
    (set! (-> (the-as dma-packet a3-0) dma)
          (new 'static 'dma-tag
            :id (dma-tag-id ref)
            :addr (-> arg0 obj)
            :qwc (+ (-> arg0 obj-qwc) (-> arg0 vtx-qwc) (-> arg0 col-qwc) (-> arg0 stq-qwc))
            )
          )
    (set! (-> (the-as dma-packet a3-0) vif0) (new 'static 'vif-tag :cmd (vif-cmd base) :imm *shrub-state*))
    (set! (-> (the-as dma-packet a3-0) vif1) (new 'static 'vif-tag :cmd (vif-cmd offset)))
    (set! (-> v1-0 base) (&+ (the-as pointer a3-0) 16))
    )
```
this tells us that there is a static DMA chain, starting at the `obj` field of the `shrubbery`. This includes `obj`, `vtx`, `col`, `stq`. The fact that it's a `ref` likely means the DMA will do one big transfer of all that data, then come back to the DMA buffer we're filling.

The use of the `base` with `offset = 0` indicates they aren't using the normal VU double buffering (the weird float tricker earlier was also a clue). They have insane buffering schemes so you can have one model uploading, another model generating GIF tags, and a third model (no longer in VU1 memory) having its GIF tags being processed by the GS.  Luckily we can mostly ignore this and just make non-buffered versions because there's no performance benefit to doing this on a PC.

Next, is:
```
  (cond
    ((= arg2 1)
     (let* ((v1-2 arg1)
            (a0-9 (the-as object (-> v1-2 base)))
            )
       (set! (-> (the-as dma-packet a0-9) dma) (new 'static 'dma-tag :id (dma-tag-id cnt)))
       (set! (-> (the-as dma-packet a0-9) vif0) (new 'static 'vif-tag))
       (set! (-> (the-as dma-packet a0-9) vif1) (new 'static 'vif-tag :cmd (vif-cmd mscal) :msk #x1 :imm #x11))
       (set! (-> v1-2 base) (&+ (the-as pointer a0-9) 16))
       )
     )
    (else
      (let* ((v1-3 arg1)
             (a0-11 (the-as object (-> v1-3 base)))
             )
        (set! (-> (the-as dma-packet a0-11) dma) (new 'static 'dma-tag :id (dma-tag-id cnt)))
        (set! (-> (the-as dma-packet a0-11) vif0) (new 'static 'vif-tag))
        (set! (-> (the-as dma-packet a0-11) vif1) (new 'static 'vif-tag :cmd (vif-cmd mscal) :msk #x1 :imm #x15))
        (set! (-> v1-3 base) (&+ (the-as pointer a0-11) 16))
        )
      )
    )
```
which adds a tag that runs a VU1 program. Either program starting at `0x11` or `0x15`, depending on the argument.

Then
```
(set! *shrub-state* (- 164 *shrub-state*))
```
Based on the use of `*shrub-state*`, I think it is a VU buffer management thing.

### `draw-inline-array-instance-shrub`
asm. I'll come back to this later. I assume this runs _before_ the prototype draw, and that it will build lists of instances in each bucket, to be processed by the next function.

### `draw-prototype-inline-array-shrub`
This one is tricky too, I'll come back later.


### `draw-drawable-tree-instance-shrub`
This is the main draw. It does:
- setup `instance-shrub-work` (likely used in the instance asm function)
- set the `next-clear` stuff to 0. This is just a shortcut fast way to reset lists of instances from the previous frame. Will make more sense once we see the previous two functions, I think.
- call `draw-inline-array-instance-shrub`
- call `draw-prototype-inline-array-shrub`
- a whole bunch of performance counters we can skip

### `draw drawable-tree-instance-shrub`
This connects the background system and the drawable/bsp system. When the draw engine executes, it calls this function, adding the drawable tree to `background-work`. When `finish-background` is called, it calls `draw-drawable-tree-instance-shrub` that does the real work

### `unpack-vis`
Having this do nothing makes the shrub system not part of the occlusion culling system.

### `collect-stats`
This shows that geometry 0 is `shrub-near`, geometry 1 is `shrub`, 2 is `trans-shrub`, and 3 is billboard.

It appears that near shrubs are drawn with the generic renderer (also looking at the prototype draw to confirm).

If this is the case, we can probably get away with ignoring geometry 0 and making a single shrub renderer that can handle the near case.  From playing in PCSX2, "near" doesn't mean higher resolution, it just means the shrub is partially behind the camera, or close to that.  If we port the rendering to opengl properly, we get this "for free".


## Shrubbery Header

- `[0] u32` - number of textures divided by 2.
- `[4] u32` - number of vertices to draw.
- `[8] u32` - number of triangle strips.