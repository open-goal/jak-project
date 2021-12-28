# Porting Tfrag
Tfrag is the renderer for non-instanced background geometry. It's typically used for the floor and unique walls/level geometry. It has a level of detail system, and time of day lighting, optionaly transparancy and that's it. No other features.

The approach I took was to go slowly and understand the rendering code. I made two different "test" renderers that were slow but did things exactly the same way as in the PS2 version.  After this, I made a custom PC version called tfrag3. The key difference with tfrag3 is that there is an offline preprocessing step that reads the level data and outputs data in a good format for PC.

Trying to understand the rendering code is annoying, but I think it was worth it in the end.
- you can often leave out huge chunks of code. I never touched `tfrag-near` or most of the `tfrag` VU program.
- you can eventually figure out how move more to the GPU. For example, all clipping/scissoring and transformation is done on the GPU. This is faster (GPUs are good) and easier (OpenGL does it automatically if you set it up right, you don't have to do the math).
- you can rearrange things for better performance. Keeping the number of OpenGL draw calls down is probably the best thing we can do for performance.

But in order to understand the renderer, I had to start with a slow "emulation-like" port.

This document is divided into three parts:
1. PS2 rendering (in Jak).
2. Jak's `drawable` system
3. Tfrag-specific details

# Basics of PS2 Rendering
The main idea of the Jak rendering system is that there are always two frames in progress at a time.  One frame is being "rendered" meaning triangles are being transformed and rasterized and the VRAM is being written to.  The other frame is being "calculated", meaning the game is building the list of instructions to draw the frame.  The "calculation" happens from GOAL code, mostly on the EE, and builds a single giant "DMA chain".  At the end of a frame, the engine takes the full DMA chain that was built, and sends it to the rendering hardware. The rendering process is all "automatic" - once it gets the list of data it will run for an entire frame and do all of the drawing.

The EE user manual sections for DMAC, VPU (VU), and GIF are worth reading before trying to understand new rendering code.

Because the calculation and rendering happen simultaneously, the calculation cannot use the same hardware and memory as the rendering.  The following resources are used only by rendering:
- VU1
- VIF1
- GIF
- GS

The following resources are used only by calculation:
- VU0-macro mode (`vf` register on EE and `vadd` like instructions)
- VU0-micro mode
- VIF0
- The scratchpad

The following resources are shared:
- DMA controller. It can handle multiple transfers to different places at the same time, and there are no shared destinations, so there's no issue here.  Rendering uses only to VIF1. Calculation uses to VIF0, to scratchpad, and from scratchpad.
- The "global" DMA buffer. The calculation process fills this buffer and the rendering reads from it. There are two copies of this buffer and the engine will swap them automatically at the end of the frame. So one copy is always being filled while the other is being read and the graphics code mostly doesn't worry about this.
- DMA data inside of level data. The DMA list may include chunks of data that's part of the level data. In practice there's not much to be aware of here - the rendering process just reads this data. 

## DMA
The whole rendering system is driven by DMA. The DMA controller can copy data from main memory to different peripherals. If the destination is "busy", it will wait. So it doesn't just blindly dump data into things as fast as it can - it only sends data if the destination is ready to accept it. The DMA controller is controlled by "DMA tags". These contain a command like "transfer X bytes of data from address Y, then move on to the DMA tag at address Z". This allows the game to build up really complicated linked-lists of data to send.

The DMA list built for rendering is divided into buckets. See `dma-h.gc` for the bucket names. Individual renderers add data to buckets, and then the buckets are linked together in the order they are listed in that enum. Code like `tfrag` won't start DMA to VIF1 or deal with linking buckets - that is handled by the game engine.

However, code like `tfrag` may just set up its own transfers to/from SPR and VIF0 - these are free-to-use during the "calculation" step.

## The VIF
The VIF is "vector unit interface". There's one for VU0 and VU1 and they are (as far as I know) identical. The rendering DMA list is sent directly to VIF1. There are also "tags" that control the VIF. The general types of tags are:
- "take the following N bytes of data and copy it to VU data memory" (possibly with some fancy "unpacking" operation to move stuff around)
- "take the following N bytes of data and copy it to VU program memory" - to upload a new VU program
- "run a VU program starting at this address in VU program memory"
- "send this data **direct**ly to the GIF" This is called a "direct" transfer. It's typically used to set things up on the GS that will be constant for one specific renderer.

I haven't seen VIF0 really used much. The pattern for VIF1 is usually:
1. upload program
2. upload some constants
3. upload some model data
4. run program
5. repeat steps 3 and 4 many times

## VU programs
The VU programs are usually responsible for transforming vertices with the camera matrix, clipping, lighting calculations, etc. The output of a VU program is GIF packets containing the actual drawing information (transformed vertices, drawing settings, etc).

The usual pattern is that the VU program will build up a GIF packet, then use the `XGKICK` instruction with the address of the start of the packet. This will start transferring the packet directly to the GIF. The transfer happens in the background. The transfer will only be completed once all triangles are drawn - there's no buffer on the GIF/GS. A single packet can be pretty big and have many triangles.

For the tfrag1/tfrag2 renderers, I ported up to this part. Then, I sent the `xgkick` data to the `DirectRenderer` which can handle this format of data. It is not super fast, but it's nice for debugging. Being able to inspect this was helpful to understand how it works.

## VU buffer hell
Typically the VU programs have 4 buffers. There is a buffer for input and output data, and both are double buffered. This allows you to be uploading new data with DMA, transforming vertices, and sending data to the GIF all at the same time. 

All 4 buffers are in use at the same time.
1. Untransformed Data being uploaded from the DMA list to VU data. This happens automatically by DMA.
2. Untransformed Data being transformed by the VU1 program.
3. A GIF packet being built from the output of the transformation. This is written by the VU1 program.
4. A GIF packet currently being `XGKICK`ed.

Once 1 is full and 2 is totally used, these buffers are swapped. The same thing happens for 3 and 4.
In some renderers, these swaps are always done at the same time. For example `sprite`. This tends to use the built-in `xitop` instructions for managing double buffering.

In other renderers, the buffer swaps can happen at different times. This leads to awful code where you have 4 different versions of the same renderer for all possible combinations of which buffers are input/output. Storing the address of the input/output buffer in a variable can lead to extra instructions inside the transformation loops, which will significantly slow down.

## GIF
The GIF can receive commands like:
- "set the alpha blending mode to X"
- "use texture located at VRAM address Y"
- "draw a triangle"

It can't do any transformation or lighting calculations.


# Jak `drawable` system
There is a `drawable` system that's used to store things that can be "drawn". It uses a tree structure. So you can do something like
```
(draw some-level)
```
and it will recursively go through the entire level's tree of drawables and draw everything.  Note that there are a lot of tricks/hacks so not every `drawable` supports `draw` and some code may defer some `draw`s until later.

The lowest-level drawable for tfrag is `tfragment`. It makes sense to split up the level into "fragments" because the entire level is way too big to fit in the VU memory. Most of the time, you can't see every triangle in the level, so it makes sense to skip uploading the fragments that you know can't be seen.
There are thousands of these fragments. They tend to be ~ a few kB and each contains a chunk of data to upload to the VUs.  Note that `draw` is not called directly on `tfragment`, despite the fact that they are `drawable`s (more details later).  The `tfragment` is just a reference to some DMA data.

## Drawables for Tfrag

The top-level drawable type for an entire level is `bsp-header`. This is the type of the `-vis` file of the level's DGO, and has all the graphics data (not including textures). It is also a `drawable`.

Within `bsp-header` is a `drawable-tree-array`. As the name implies, this contains an array of `drawable-tree`. Usually there are 5-10 of these in a level. There will be a `drawable-tree` for each renderer. Or possibly a few per renderer, if that renderer supports different modes.  For example there's one for tfrag, one for transparent tfrag, one for tie, etc.

You can just check the type of the `drawable-tree` to see if it's for tfrag/tie etc. The tfrag types are:
- `drawable-tree-tfrag` (parent of the rest)
- `drawable-tree-trans-tfrag`
- `drawable-tree-dirt-tfrag`
- `drawable-tree-ice-tfrag`
- `drawable-tree-lowres-tfrag`
- `drawable-tree-lowres-trans-tfrag`

Each "tree" contains a bunch of `tfragment`s and a time of day color palette.  But they are stored in a really weird way. There is a bounding volume hierarchy of `tfragment`s. This is just a tree-like structure where each node stores a sphere, and all the node's children fit inside of that sphere. The nodes at each depth are stored in an array. The layout of this tree is designed to let them use some crazy assembly SIMD 8-at-a-time traversal of the tree, with minimal pointer-chasing and good memory access patterns.

Each tree has an array of `drawable-inline-array`s, storing all the nodes at a given depth. The last `drawable-inline-array` is actually a `drawable-inline-array-frag`, which is a wrapper around an inline array of `tfragment`s.

The other arrays are used to store tree nodes to organize these `tfragment`s.  Each node in the tree contains a `bsphere`. All tfrags below the node fit into this sphere.

The second to last array (if it exists) is a `drawable-inline-array-node`. This contains an inline array of `draw-node`. Each `draw-node` is the parent of between 1 and 8 `tfragment`s. They store a reference to the first child `tfragment` and a child count, and the children are just the ones that come after the first `tfragment` in memory.

The third to last array (if it exists) is also a `drawable-inline-array-node`, containing an inline array of `draw-node`. Each `draw-node` is the parent of between 1 and 8 `draw-node`s from the array mentioned above.  They store a reference to the first child `draw-node` and a child count, and the children are stored consecutively.

This pattern continues until you get a `drawable-inline-array-node` with 8 or fewer nodes at the top.

All the `draw-node` and `tfragment`s have ID numbers. These are used for the occlusion culling system. The visibility numbering is shared with all the other `drawable-tree`s in the `bsp-header`. The indices are given out consecutively, starting from the roots. Between depths, they are aligned to 32 elements, so there are some unused ids.  These IDs are the index of the bit in the visibility string.

With that out of the way, we can now go through the tfrag renderer

# Tfrag

The rough process for rendering is:
- "login" the data
- do "drawing" as part of the `drawable` system (on EE)
- do the real "draw"
- do culling
- compute time of day colors (or other precomputation)
- generate DMA lists
- unpack data to VU memory
- transform vertices
- clip
- build gs packets
- XGKICK

I expect that most other renderers will be pretty similar.

## Login
The tfrag data needs to be initialized before it can be used. You only have to do this once. This is called `login`, and it's a method of all `drawable`s. The level loader will call the `login` method of many things as part of the level load.  For `tfrag`, all I had to do was decompile the `login` methods, and it worked and I could completely ignore this until tfrag3.

It's possible to just call `login` on an entire level, but this probably takes too long, so the level loader will cleverly split it up over multiple frames.

It is from:
- `level-update`
- `load-continue`
- `level-update-after-load`
- various calls to `login`.


In the end, the only thing the `login` does for tfrag is:
```
(adgif-shader-login-no-remap (-> obj shader i))
```
for all the "shaders" in all the tfrags.  A "shader" is an `adgif-shader`, which is just some settings for the GS that tells it drawing modes, like which texture to use, blending modes, etc. The `tfrag` VU1 code will send these to the GIF as needed when drawing. A `tfragment` can have multiple shaders. There is a different shader per texture.

The actual "shader" object is just 5x quadwords that contain "adress + data" format data. The address tells the GIF which parameter to change and the "data" has the value of the parameter. Some of them are not set properly in the level data, and the `adgif-shader-login-no-remap` function updates them. For tfrag, the 5 addresses are always the same:

- `TEST_1`: this sets the alpha and z test settings. This is set properly in the level data and `login` doesn't touch it.
- `TEX0_1`: this has some texture parameters. This is 0 in the level data and is modified by `login`.
- `TEX1_1`: this has more texture parameters. In the level data this is has `0x120` as the value and the address is set to the texture ID of the texture. During `login`, the texture ID is looked up in the texture pool and `TEX0_1`/`TEX1_1` are set to point to the right VRAM address and have the right settings to use the texture.
- `MIPTBP1_1`: is mipmap settings. I ignore these because we do our own mipmapping.
- `CLAMP_1`: this has texture clamp settings. This is set properly in the level data.
- `ALPHA_1`: this has alpha blend settings. This is set properly in the level data.


## Calling the `draw` method
The `tfragment` at least pretends to use the `drawable` system, and the drawing is initiated by calling `draw` on the `drawable-tree-tfrag`. Getting this to actually be called took some digging - it uses some functions in later files that we haven't completed yet.

When the level is loaded, the `bsp-header` is added to the `*background-draw-engine*` by the level loader. The path to calling draw is:
- In `main.gc`, there is a `display-loop`. This has a while loop that runs once per frame and runs many systems.
- The `display-loop` calls `*draw-hook*`
- The `*draw-hook*` variable is set to `main-draw-hook`
- The `main-draw-hook` calls `real-main-draw-hook`
- The `real-main-draw-hook` calls `(execute-connections *background-draw-engine*`
- This "engine" calls the `add-bsp-drawable` function on the `bsp-header` for each loaded level.
- The `draw` method of `bsp-header` sets up some stuff on the scratchpad and some `vf` registers.
- The `draw` method of `bsp-header` calls `draw` on the `drawable-tree-array` (defined in parent class `drawable-group`)
- The `draw` method of `drawable-group` checks if the level is visible, and if so calls `draw` on each tree.
- The `draw` method of `drawable-tree-tfrag` simply adds the tree to a list of trees in `*background-work*`.


## Real "drawing"
Later on, in the `real-main-draw-hook`, there is a call to `finish-background`.

There's some stuff at the top of this function that's only used for the separate shrubbery renderer. It sets up some VU0 programs. I noticed that the stuff before tfrag drawing would overwrite this VU0 stuff so I ignored it for now.

The first thing that happens before any tfrag drawing is setting the `vf` registers to store the `math-camera` values. In OpenGOAL, the `vf` registers aren't saved between functions, so I had to manually use the `with-vf` macro with the `:rw 'write` flag to save these:
```lisp
 (let ((v1-48 *math-camera*))
        (with-vf (vf16 vf17 vf18 vf19 vf20 vf21 vf22 vf23 vf24 vf25 vf26 vf27 vf28 vf29 vf30 vf31)
                 :rw 'write
                 (.lvf vf16 (&-> v1-48 plane 0 quad))
                 (.lvf vf17 (&-> v1-48 plane 1 quad))
                 ;; ...
```
these will later be used in part of the drawing function. The `:rw 'write` flag will save these to a structure where we can read them later.

Then, for each tree:
```
(upload-vis-bits s1-0 gp-1 a2-4)
```
this uploads the visibility data to the scratchpad. The visibility data is stored at the end of the 16 kB scratchpad. The drawable with ID of `n` can look at the `n`-th bit of this data to determine if it is visible.  The visibility IDs are per level, and the drawing order of the `tfrag` will alternate between levels, so they upload this for each tree to draw.  It seems like you could skip this after the first upload if you detect that you're drawing multiple trees in the same level.  They do it for TIE and not TFRAG and don't know why. The visibility data is based on the position of the camera. Currently this doesn't work so I modified it to upload all 1's.

The modification to the code to use the scratchpad in OpenGOAL is:
```
  ;;(spad-vis (the-as (pointer uint128) (+ #x38b0 #x70000000)))
  (spad-vis (scratchpad-ptr uint128 :offset VISIBLE_LIST_SCRATCHPAD))
```
The `0x38b0` offset is just something we've noticed over time as being the location of the visible list, so there's a constant I made for it. (TODO: I think it's also `terrain-context work background vis-list`)

The hack for visibility is:
```lisp
;; TODO this is a hack.
(quad-copy! (-> arg0 vis-bits) (-> arg2 all-visible-list) (/ (+ (-> arg2 visible-list-length) 15) 16))
```
which actually modifies the level to say that everything is visbile. The `all-visible-list` is just a list which has `1` for every drawable that actually exists (I think, need to configm). There are some skipped ID's.


The next part of drawing is:
```
                  (when (not (or (zero? s0-0) (= s4-1 s0-0)))
                    (flush-cache 0)
                    (time-of-day-interp-colors-scratch (scratchpad-ptr rgba :offset 6160) s0-0 (-> s1-0 mood))
                    ;; remember the previous colors
                    (set! s4-1 s0-0)
```
where `s0-0` is the `time-of-day-pal` for the tfrag tree. It will skip interpolation if it is the same color palette that was just interpolated.

The `time-of-day-interp-colors-scratch` function uploads the colors from `s0-0` to the scratchpad at offset `6160`. It computes the correct colors for the time-of-day/lighting settings in the the level `s1-0`'s mood.  This function is pretty complicated, so I used MIPS2C.

### Time of Day Interp Colors Scratch
The very first attempts for TFRAG just skipped this function because it wasn't needed to debug the basic drawing functions. I manually set the lighting to `0.5` for all colors and `1.0` for alpha. I suspected that this stored the colors in the scratchpad. I assumed it would be fine if these garbage for a first test.

I noticed that this function does a few tricky things. It uses the scratchpad and it uses DMA.  I know it uses DMA because I saw:
```cpp
  c->lui(t0, 4096);                                 // lui t0, 4096
  // some stuff in between...
  c->ori(a1, t0, 54272);                            // ori a1, t0, 54272 = (0x1000D400) SPR TO
```
and this `0x1000D4000` is the address of the DMA control register for transferring to the scratchpad.  The scratchpad here is just used as a faster memory. And eventually the draw code will read the result from the scratchpad.

They really like this pattern of doing work on the scratchpad while DMA is running in the background, copying things to/from the scratchpad. In this case, they upload the palette to the scratchpad in chunks. As those uploads are running, they do math on the previous upload to blend together the colors for the chosen time of day.  To get optimal performance, they often count how many times they finish before the DMA is ready. When this happens, they increment a "wait" variable.

I modified scratchpad access like this:
```cpp
  c->lui(v1, 28672);                                // lui v1, 28672      0x7000
  // stuff in between skipped...
  //c->ori(v1, v1, 2064);                           // ori v1, v1, 2064 SPAD mods
  get_fake_spad_addr(v1, cache.fake_scratchpad_data, 2064, c);
```
the original code would set the address to an offset of 2064 in the scratchpad.

The first thing they do is wait for any in-progress DMA transfers to finish:
```cpp
  block_1:
  c->lw(t5, 0, a1);                                 // lw t5, 0(a1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(t5, t5, 256);                             // andi t5, t5, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t5) != 0;                          // bne t5, r0, L62
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely
```
which is reading and checking the DMA register in a loop. We can just get rid of this - we make all DMA instant.


I also modified the code that starts the transfer to just do a memcpy from the fake scratchpad. See the EE manual for details on what these registers mean. The `a1` register points to the control register for SPR TO DMA.
```cpp
  {
    // c->sw(t4, 16, a1);                                // sw t4, 16(a1)
    u32 madr = c->sgpr64(t4);
    c->daddiu(t3, t3, -32);                           // daddiu t3, t3, -32
    // c->sw(v1, 128, a1);                               // sw v1, 128(a1)
    u32 sadr = c->sgpr64(v1);
    c->addiu(t5, r0, 64);                             // addiu t5, r0, 64
    //c->sw(t5, 32, a1);                                // sw t5, 32(a1)
    u32 qwc = c->sgpr64(t5);
    c->addiu(t5, r0, 256);                            // addiu t5, r0, 256
    // c->sw(t5, 0, a1);                                 // sw t5, 0(a1)
    spad_to_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
    c->daddiu(t4, t4, 1024);                          // daddiu t4, t4, 1024
  }
  ```
This data is double buffered. One buffer is being filled from DMA while another is being processed. To swap buffers, they often use `xor` to toggle a bit. But this trick only works if our buffer has the same alignment as theirs up to the bit being toggled (otherwise their first `xor` might toggle a 0 to a 1, advancing the address, where ours does the opposite).
```
c->xori(v1, v1, 1024);                            // xori v1, v1, 1024
```

The actual processing is an annoying pipelined loop. The palette stores groups of 8 colors. The time of day system computes 8 weights, passed to this function. Each of the 8 colors is multiplied by the weight and added. This process is repeated for each group. There are usually 1024 or 2048 groups.  The tfragments are lit by indexing into these groups.  One important detail is that the r/g/b/a values are saturated so they don't overflow.

This part works using the MIPS2C function in the first tfrag renderers. In the third one, it was annoying to get this data to the C++ renderer, so I just recomputed it in C++. This also lets us manually override the time of day values for fun. The code is much simpler:
```cpp
void Tfrag3::interp_time_of_day_slow(const float weights[8],
                                     const std::vector<tfrag3::TimeOfDayColor>& in,
                                     math::Vector<u8, 4>* out) {
  for (size_t color = 0; color < in.size(); color++) {
    math::Vector4f result = math::Vector4f::zero();
    for (int component = 0; component < 8; component++) {
      result += in[color].rgba[component].cast<float>() * weights[component];
    }
    result[0] = std::min(result[0], 255.f);
    result[1] = std::min(result[1], 255.f);
    result[2] = std::min(result[2], 255.f);
    result[3] = std::min(result[3], 128.f);  // note: different for alpha!
    out[color] = result.cast<u8>();
  }
}
```

### The Call to Draw
There was another scratchpad use to patch up here. They often treat the scratchpad as a `terrain-context`. There are quite a few overlays here so sometimes you have to do some manual searching to figure it out.
```lisp
      (set! (-> (scratchpad-object terrain-context) bsp lev-index) (-> s1-0 index))
        
      (set! (-> *tfrag-work* min-dist z) 4095996000.0)
      ;; draw!
      (draw-drawable-tree-tfrag s2-0 s1-0)
      )
      ;; remember closest.
      (set! (-> *level* level (-> (scratchpad-object terrain-context) bsp lev-index) closest-object 0)
            (-> *tfrag-work* min-dist z)
            )
      )
```
the remembering closest is used for figuring out which mip levels of texture need uploading.


### Draw node culling
This is a part that I left out. I still haven't done it. But I suspect it looks at the position of the camera (stored in `vf` regs from earlier) and modifies the visibility data. I think it uses a "sphere in view frustum" check and traverses the tree of `draw-node`s. I think it only culls the `draw-node`s and not actually the `tfragment`s, and it modifies the visibility data in place.  It only culls the range of nodes that correspond to the tree we're drawing.

Later, on tfrag3, I did the culling in C++. (More on this later - it's done in a tricky way so that you can efficiently build a list of only the visible things to send to the GPU).


### DMA List Generation
The objective of the draw function is to generate a DMA list. This gets added to the entire DMA list for the frame and gets sent to the VIF. The DMA data is a list of instructions like:
- upload this data to the VU memory
- run this VU program
- change various settings related to the VU data upload.

The pattern used by tfrag is:
- Call `tfrag-init-buffer` once. This is unoptimized code that just sets things up.
- Call `draw-inline-array-tfrag`. This adds DMA per tfragment. It is super optimized.
- Call `tfrag-end-buffer`. This is unoptimized code that ends the DMA list for tfrag
