# Basic Process for Drawing

The first main part just gets the "drawable trees" added to the list.
When the level loads, it gets added to the background engine in `level-status-set!`.  When the background system runs, it executes this engine, which calls `draw` on the `bsp-header`.  Eventually `draw` is called on the tfrag tree, and it gets added to the `*background-work*` list.

The second main part builds DMA.
This happens from `finish-background`, called from `real-main-draw-hook`, called from the display loop in `main.gc`.
For each tree, it uplaods vis data, interpolates time-of-day colors, and runs `draw-drawable-tree-tfrag`.  This sets up DMA buffers and eventually calls `draw-inline-array-tfrag`, a crazy asm function that builds the DMA lists and likely makes decisions about which LOD to draw.  Within `draw-drawable-tree-tfrag`, there's also a call to `draw-inline-array-tfrag-near` that sets up DMA for the separate `near` renderer. 

The third main part runs on VU1 and actually draws.
The DMA chain is a bunch of UNPACKs, which load data to VU memory, with some MSCALs that start VU1 programs. There are a number of different routines.

# Visibility
The visibility information is not computed yet, but I believe this is properly handled in `draw-inline-array-tfrag`.  By modifying `upload-vis-bits` in `background.gc`, I can change what is drawn.  There is one mystery that the tfrag "stats" change, but have totally wrong numbers.  It almost seems like it is the correct stats for what is _behind_ the camera.

As far as I can tell there are 2 or 3 types of culling:
- precomputed visibility from this VIS files. I believe this uses the bsp to figure out which string to load.  This is done as part of the camera update and modifies the visibility strings.  The vis strings aren't loaded, the bsp update isn't running, and the camera update doesn't run, so this doesn't work at all.
- frustum culling in `draw-node-cull`. The camera update sets the view frustum planes, and draw-node-cull iterates through the draw node tree (up to 8 at a time, using the fancy MMI instructions).  The camera update doesn't run, so those planes aren't set. Also, the draw-node-cull function isn't ported yet.
- possibly additional culling in the `draw-inline-array-tfrag` function that builds dma list.  It at least looks at the bspheres.
- clipping in the VU1 program. This _appears_ to work correctly in my port, but doesn't fully work in the PS2 version - if you turn on "fix frustum", you get garbage.  But this might be a part of tfrag I don't have yet.

# Time of Day
There's a function `time-of-day-interp-colors-scratch` that computes the time-of-day lighting coloring. Right now it's not implemented at all.

That said, there is something else going wrong.  I see that there is a VU UNPACK operation that seems to be placing RGBA's in memory, but it places them on every other quadword.  The in-between quadwords aren't written at all, but are used as RGBA data, leading to random RGBA flashing if you try to use them.  I suspect one of the programs I'm skipping does some additional color calculations and puts stuff there - I stared at this for a while and couldn't figure it out.

# Different TFrag Renderers? Not really
There are different types of drawable trees that are all tfrag:
- `drawable-tree-tfrag`
- `drawable-tree-trans-tfrag`
- `drawable-tree-dirt-tfrag`
- `drawable-tree-ice-tfrag`
- `drawable-tree-lowres-tfrag`
- `drawable-tree-lowres-trans-tfrag`
As far as I can tell, they are all drawn in almost exactly the same way.  The different trees are just used:
- to give you an ordering. For example, everything in `drawable-tree-tfrag` is drawn before  `drawable-tree-ice-tfrag`.  This probably matters for transparent things.
- to have slightly different settings passed to the GS. As far as I can see the only differences are with ztest and alpha GS registers.
- to have different textures in VRAM. The `alpha` textures are uploaded after  `drawable-tree-tfrag`.

# Near vs Far
There are two separate tfrag renderers: near and far. They are separate VU1 programs.  The tfragments themselves are not specialized per renderer, but the DMA lists might be.

I am not sure, but I believe that both near and far versions are capable of rendering the full-detail mesh, and the main purpose of "near" is to perform scissoring on the VUs. I believe it can detect triangles that intersect the edge of the screen and divide them into multiple triangles, throwing away the parts that are off screen.  The "far" version can't do this.


# Different VU1 subroutines
The "far" render has many subroutines.  Currently I have only ported "program 6" and with a modification to remove about half the code. Internally, on VU1, tfrag double buffers GIF data.  One packed will be getting "XGKICK"ed while another is being built. They have a pretty complicated system for switching this buffer (it's not synced to the XTOP UNPACK double buffering stuff, like it was on sprite), but I removed this feature and could get of half the packet building code, which is mostly a duplicate.

This "program 6" is not capable of interpolating the mesh and just draws at one level of detail.  But it is capable of drawing at _different_ levels of detail (at least 2).  I suspect there are other programs for interpolating the mesh and doing color computations.

There is at least one other "drawing" program. I tried using program 6 when the game wanted other programs, and surprisingly it drew more stuff. But every now and then, things go wrong and it generates garbage data when running like this.

Example sequence (from start of frame): `[8, 10, 10, 10, 6, 6, 6...]`.

Program list:

- Program 0: init globals (TFrag.cpp handles this)
- Program 2: reset the value of VF04 (nothing uses this?), but only 1 instruction long.
- Program 4: unported drawing program (L112 transform, L79 kick) (unused?)
- Program 6: ported drawing program (L127 transform, L122 kick) (most common)
- Program 8: runs sub L12, L26, L48, L102 (2nd most common)
- Program 10: runs sub L12, L18, L102
- Program 12: same as program 6 (ported)
- Program 14: runs sub L12, L18, L25, L47, L102
- Program 16: runs sub L13, L17, L102
- Program 18: runs sub L13, L17, L84
- Program 20: same as program 6 (ported)
- Program 22: same as program 2 (unused?)
- Program 24: same as program 2 (unused?)

Sub List:
- L12/L13 (jal)
  - very short, no drawing. looks like transformation + int to float.  Has the camera matrix.


- L17/L18 (jal)
  - also transformations with cam matrix. Looks at the subdivide. Likely part of mesh interp. Has two versions for buffer (2nd starts at L22)

- L25/L26 (jal)
  - also transformations. L37 is 2nd buffer version

- L47
- L48
- L84
- L102

# Bugs
There are unimplemented features (visibility, lighting, other code paths in far, all of near, tfrags other than the main tree), and the performance is quite bad, but as far as I can tell everything works!

The one weird looking thing is the missing slivers in between mesheses with different LODs.  Not missing triangles, but when there's a vertex on the high-detail tfrag that should intesect somewhere in the middle of an edge on the low-detail one, the vertex is slightly in the wrong spot. I've seen both overlaps and holes.  I strongly suspect that the mesh interpolation programs should fix this, but aren't running yet. An adjacent high-detail and low-detail mesh _should_ have holes, if the two high-detail versions are "high-res" on their border.

# Performance
The EE performance is excellent. The "draw" function uses only 2 or 3% CPU.

The performance is very bad on the VU1/GS side. A wide view of sandover puts graphics at around 170%. A few things we can look at:

- A wide view of sandover is 11k triangles of tfrag and **1000** draw calls! We should figure out how to make this number much smaller. If the current approach does actually require this many draw calls, then we will need to change that. PCSX2 uses fewer draw calls, so it is at least possible.  We should also make better tools to figure out _why_ the draw call count is exploding.

- At least the far renderer always draws strips. We should probably use this to our advantage instead of using the very generic "DirectRenderer".

- The VU1 code is pipelined. It should be un-pipelined and rearranged to be a simple, small loop.

- We create GIF tags. We should go straight from the transformation loop to whatever format our renderer likes.

- Assuming we can do the thing above, we should try to move the transformation stuff to a shader.

- If that's still too slow, it may be possible to double buffer the UNPACK and rendering, and move UNPACK to a separate thread.  But this could get tricky to figure out what UNPACKs need to happen before render.


# Color Debug
It's possible to start with program 6. Which should have colors that aren't junk.

Here's a run with a single run of prog 6.

```
START vif -> NOP (mod 0)
vif -> STROW (mod 0)
vif -> STMOD 0b1 (mod 0)
vif -> UNPACK-V4-8: 4 addr: 117 us: true tops: true (mod 1)
vif -> STMOD 0b0 (mod 1)
vif -> UNPACK-V4-8: 3 addr: 129 us: false tops: true (mod 0)
vif -> UNPACK-V4-16: 5 addr: 0 us: true tops: true (mod 0)
vif -> UNPACK-V4-32: 10 addr: 9 us: false tops: true (mod 0)
vif -> STROW (mod 0)
vif -> STMOD 0b1 (mod 0)
vif -> UNPACK-V4-16: 14 addr: 75 us: true tops: true (mod 1)
vif -> STMOD 0b0 (mod 1)
vif -> STCYCL cl: 2 wl: 1 (mod 0)
vif -> UNPACK-V3-32: 8 addr: 19 us: false tops: true (mod 0)
vif -> STCYCL cl: 4 wl: 4 (mod 0)
vif -> NOP (mod 0)
START vif -> UNPACK-V4-8: 8 addr: 20 us: true tops: true (mod 0)
START vif -> MSCAL (mod 0)
B: vf20 store: [0 0 7f 43] <-- color stores. These are garbage.
B: vf21 store: [0 0 0 0]
B: vf22 store: [0 0 80 42]
B: vf23 store: [d0 a3 81 48]
.. garbage continues ...

```