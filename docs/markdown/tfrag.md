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
There's a function `time-of-day-interp-colors-scratch` that computes the time-of-day lighting coloring. The 8 w components of times are the multiplier for the 8 precomputed lighting maps.

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

Annoyingly, even tfragments that are at a single LOD, but border tfragments of a different LOD must have a special interpolation applied to them.

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
