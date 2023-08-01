# Emerc

## Outline
It's one of two renderers used for foreground + environment mapping. There's also a generc + merc (mercneric) renderer.

As far as I know, the supported effects are:
- skinning, with up to 3 bones influencing each vertex, and per-vertex specification of bone weights
- up to 3 directional lights, plus an ambient light
- vertex colors
- texturing
- texture-based environment mapping (done per vertex, not fragment)

Our hope is to port the emerc renderer to PC, then use it for all rendering for envmapped foreground objects. I believe that `emerc` will be easier to understand than `mercneric`. The hope is that either `emerc` can be used for all models, or once we understand `emerc`, it will be straightforward to convert `mercneric`-only models to work with PC `emerc`.

The mercneric renderer handles partially offscreen stuff, and is believed to be slower than emerc. However, mercneric may use less VU1 time, in exchange for more EE time.

As far as I can tell, the way the game decides to use emerc only if all three of these conditions are true:
-  `emerc` effect bit is set in the model, indicating it can use `emerc`.
- we're an actor spawned by scene-player
- we're not in a frame range specified by `scissor-frame` in the scene info

The `emerc` bit is only there on high-resolution cutscene models.
Most of the time, there are no frames specified in `scissor-frame`. This makes sense, usually the actors are onscreen during cutscenes, and `emerc` seems quite tolerant of partially offscreen characters. (similar story in jak 1 - they were aggressive at letting merc draw offscreen instead of clipping triangles, likely because the clipping pipeline is so much slower).

In very rare cases, they manually specified a frame range for a character who is mostly offscreen (like daxter's feet are visible in frame 2324 of `city-krew-collection-intro`), and then the character is rendered with `mercneric`.

My guess is that they just used emerc by default everywhere. If a cutscene character is partially offscreen/behind the camera in a bad way that causes GS coordinates to overflow, this would draw garbage triangles, and they would manually annotate the frame range where this happened.

## Review of how all this gets called

### Setup
- A level containing `entity-actor`s is loaded
- The `level-update` method (called once per frame) in `entity.gc` calls `birth!` on `entity-actor`s that are visible and eligible to be spawned
- The newly created actor process is initialized by calling `init-from-entity!`, which is a method that all objects must implement.
- This method will eventually call `initialize-skeleton`, a method of the parent `process-drawable` class.
- This method creates a `draw-control` with `skeleton-group->draw-control`
- This method calls `setup-cspace-and-add`
- This method adds the process drawable to `*foreground-draw-engine*`, a list of processes to be drawn.
- The connection uses function `add-process-drawable`, which just calls the `dma-add-func` of the `draw-control`, which is `dma-add-process-drawable` by default

### Per-Frame Draw
- Game-objects are responsible for calling `ja-post`, or adding themselves to the matrix-engine list, or somehow coming up with `joint` transforms.

- main loop in `main.gc` calls `(*draw-hook*)`, which points to `real-main-draw-hook`. This function generates all DMA data for drawing.
- `foreground-engine-execute`
  - `foreground-init` (doesn't do anything emerc-related)
  - calls `execute-connections` on the engine, the `dma-add-process-drawable` for each object
    - various stuff for shadows/picking lights
    - generates `vu-lights` (light values in VU-friendly format)
    - picks LOD based on distances
    - sets texture masks to indidate to texture system which LODs of which textures will be used
    - determines if `close-to-screen` culling is needed.
    - call `foreground-draw`
      - add an entry to the `*bone-calculation-list*` to tell it to compute skinning matrices.
      - rotate lights to camera frame (note that merc only gets a perspective transform, transforming to camera frame is done in skinning calc to avoid a full affine transform on VU1)
      - there's some confusing logic for the renderer selection, but in the end it populates `merc-effect-bucket-info` including a color and a few flags.
      - calls `foreground-emerc`, which generates DMA data for `emerc` (asm func)
- `foreground-execute-cpu-vu0-engines`
  - runs bones, modifying the above DMA data to contain skinning matrices computed from joints.
- `display-frame-finish` called after all drawing
  - Calls `emerc-vu1-init-buffers`, which adds some init data to all used `emerc` buckets.

### Emerc DMA Generation
The call in GOAL:
```
(set! dma-ptr (foreground-emerc dc (-> (scratchpad-object foreground-work) regs mtxs) dma-ptr 29 19))
```
The arguments are:
- `draw-control`, which contains settings for drawing, and the actual merc geometry (called `geo`)
- a pointer to the "matrix area", which will contain skinning matrices computed by `bones`
- `dma-ptr`, a pointer to the DMA buffer to write data to
- 29, 19, likely addresses in the VU1 microprogram to start execution. Typically there is one program for the first run of the renderer, which initializes some VU1 registers/data memory, and then a slightly shorter program that skips the init step.

Before the asm, the rough breakdown is:
- a `draw-control` stores 4 geos, one for each lod (some may be unpopulated)
- Each `geo` is a `merc-ctrl`, which is an entire model
- Each `merc-ctrl` is made up `merc-effect`s
- Each `merc-ctrl` is made up of "fragment"s. Each fragment has a `frag-geo` (actual data needed in VU1) and `frag-ctrl` (metadata describing how to upload data to VU1)
- Each fragment has a few types of data:
  - `unsigned-four`: containing weights (u8), rgba (u8), addresses for crosscopy/samecopy. Unpacked [u8x4] to [u32x4] by VIF on upload to VU1.
  - `lump-four`: containing vertex data. Unpacked [u8x4] to [u32x4 + some_magic_constant] by VIF on upload to VU1. This unpack magically converts integers to floats.
  - `fp` data: containing a header, and "shaders" (giftags for setting up textures/settings). Copied directly by VIF.

The calling function `foreground-draw` sets flags (per effect) in the `merc-bucket-info` array. All emerc stuff gets `merc-path` set to 1.

High-level description of what it does.
Note that this is simplified from the assembly version, which combines some dma transfers shown here.
Also - this does not actually run any DMA or microprograms, it just generates a DMA chain that will do this later
On the next game frame, the giant DMA chain generated by all renderers will be submitted, and all these will run.
```cpp
// get the merc control for our level of detail (selected in drawable.gc)
MercControl& mc = draw_control.lod_set[draw_control.cur_lod].geo;

// loop over each "effect" in the merc control.
// The "effect" is the grouping for what can be sent to one renderer or another
for (int effect_idx = 0; effect_idx < mc.header.effect_count; effect_idx++) {
    MercEffect& merc_effect = mc.effect[effect_idx]; // merc data in the art group
    MercBuckedInfo& merc_effect_info = gForeground.merc_bucket_info[effect_idx]; // settings generated by foreground-draw

    if (merc_effect_info.disable_draw) {
        continue; // skip if disabled
    }

    if (merc_effect_info.merc_path != 1) {
        continue; // skip if not emerc (1 means emerc here)
    }

    // where we started writing dma for this effect
    u8* effect_dma_start = dma_ptr;

    // the source data (stored in the art group) that we'll be sending.
    u8* source_ptr = merc_effect.frag_geo;

    // loop over fragments
    for (int frag_idx = 0; frag_idx < merc_effect.frag_count; frag_idx++) {
        MercFragmentControl& frag_ctrl = merc_effect.frag_ctrl[frag_idx];
        // set the ROW register of the VIF.
        // when kRowAdd flag is given, the VIF will add these 4 values to each component of each quadword it writes out.
        // This is used as part of the process to go from u8's to floats
        // (they do some cool magic where they don't actually do int->float, they just add integers with VIF and
        //  do float math on VU1 and it works out somehow)
        dma_ptr = generate_vif_strow(dma_ptr, mc.header.st_vif_add, mc.header.st_vif_add, 0x47800000, 0x4b010000);

        // number of quadwords (16-byte words) in EE memory of unsigned_four data to send
        // unsigned_four data is stored as [u8, u8, u8, u8] and unpacked to [u32, u32, u32, u32].
        // the count variable is in units of 4 values. (4 bytes in EE memory, 16 bytes in VU1 memory)
        int u4_qwc_in_ee_mem = (frag_ctrl.unsigned_four_count + 3) / 4;

        int dest_addr_qw = 140;

        dma_ptr = generate_vif_unpack(dma_ptr
            kUnpackV4_8,             // unpack [u8, u8, u8, u8] to [u32, u32, u32, u32]
            kUnsigned,               // zero extend when unpacking
            dest_addr_qw,            // VU1 data address (in quadwords)
            kUseTop,                 // add value of TOP register to destination (VU1 program controls destination)
            source_ptr,              // source pointer
            u4_qwc_in_ee_mem,        // number of QW to transfer from EE memory
            frag_ctrl.unsigned_four_count,  // number of QW written to VU1 memory
            kNoRow,                  // do not add row
            );
        // note: to write 7 QW of data, the would have this in EE memory:
        // [v0, v1, v2, v3] (4 bytes)
        // [v4, v5, v6, XX] (4 bytes)
        // they would transfer 2 QW to vif (including 1 padding byte)
        // but you can tell VIF to unpack only 7 QW, and it will discard the padding.

        // advance source pointer to the next data (lump data)
        source_ptr += u4_qwc_in_ee_mem * 16;

        // advance dest pointer.
        dest_addr_qw += frag_ctrl.unsigned_four_count;

        // lump 4 is unpacked from [u8, u8, u8, u8] to [u32 + rx, u32 + ry, u32 + rz, u32 + rw]
        // where [rx, ry, rz, rw] are specified in ROW set above.
        int l4_qwc_in_ee_mem = (frag_ctrl.lump_four_count + 3) / 4;

        dma_ptr = generate_vif_unpack(dma_ptr
            kUnpackV4_8,             // unpack [u8, u8, u8, u8] to [u32, u32, u32, u32]
            kUnsigned,               // zero extend when unpacking
            dest_addr_qw,            // VU1 data address (in quadwords)
            kUseTop,                 // add value of TOP register to destination (VU1 program controls destination)
            source_ptr,              // source pointer
            l4_qwc_in_ee_mem,        // number of QW to transfer from EE memory
            frag_ctrl.lump_four_count,  // number of QW written to VU1 memory
            kAddRow                  // add the row value
            );

        // advance source pointer to the next data (lump data)
        source_ptr += l4_qwc_in_ee_mem * 16;

        // advance dest pointer.
        dest_addr_qw += frag_ctrl.unsigned_four_count;

        // send fp data.
        dma_ptr = generate_vif_unpack(dma_ptr
            kUnpackV4_32,            // just plain memcpy to VU1 memory
            kSigned,                 // no effect? they set it explicitly always, not sure why.
            dest_addr_qw,            // VU1 data address (in quadwords)
            kUseTop,                 // add value of TOP register to destination (VU1 program controls destination)
            source_ptr,              // source pointer
            fp_qwc,  // number of QW to transfer from EE memory
            frag_ctrl.fp_qwc,  // number of QW written to VU1 memory
            kNoRow                   // don't add the row value
            );

        // adavne source pointer
        source_ptr += frag_ctrl.fp_qwc * 16;

        // there's some special data shared between all fragments. We put this DMA after the DMA
        // for the first fragment as an optimization. We can write the first fragment of this effect
        // to VU1 data memory while VU1 is processing the last fragment of the previous effect.
        // This is ok because the per-fragment data is double buffered (controlled with the TOP register)
        // However, the shared data is not double buffered, and we must wait for the previous effect
        // to be fully done before transferring. We want to delay this as long as possible, so we
        // transfer the first per-fragment data of this effect before this part.
        if (frag_idx == 0) {
            // sneak some more data in lights
            auto lights = gForeground.merc_bucket_info.lights;
            lights.qws[1].w = ignore_alpha ? 0x3f85026b : 0x3f85026a;
            // copy the 7 qw of lights to the dma buffer now, setting up a transfer for them to go
            // to address 140 in VU1 (no TOP).
            // the previous code sets up these lights in VU format (vu-lights).

            dma_ptr = dma_memcpy_to_buffer_then_vu1(dma_ptr, 132, &lights, 7);
            // copy these 4 values to address 139 (copying them to the dma-buffer now)
            dma_ptr = dma_copy_to_buffer_then_vu1(dma_ptr, 139, merc_ctrl.header.xyz_scale, merc_ctrl.header.st_magic, merc_ctrl.header.st_out_a, merc_ctrl.header.st_out_b);

            // emerc new transfer - copying 1 qw color_fade (u8's unpacked to u32)
            dma_ptr = dma_copy_to_buffer_then_vu1(dma_ptr, 118, unpack_u8_to_u32(merc_effect_info.color_fade));

            AdgifShader* envmap_shader = DefaultEnvmapShader;
            if (merc_effect.extra_info && merc_effect.extra_info.shader_offset) { // nonzero check
                envmap_shader = ((u8*)&merc_effect.extra_info) + 16 * merc_effect.extra_info.shader_offset;
            }

            // 5 qw envmap shader
            dma_ptr = dma_copy_to_buffer_then_vu1(dma_ptr, 119, envmap_shader, 5 * 16);
        }


        // fragments will (most of the time) need new matrix data.
        // there are some cases where they can reuse some matrix data from previous fragments in the same
        // effect, so it's possible for there to be no matrices to transfer. But usually there are some
        for (int mat_xfer = 0; mat_xfer < frag_ctrl.max_xfer_count; mat_xfer++) {
            auto& info = frag_ctrl.mat_dest_data[mat_xfer];
            dma_ptr = dma_transfer_matrix(dma_ptr, info.matrix_dest, matrix_mem + sizeof(MercMatrix) * info.mattrix_number);
        }

        // finally, call program.
        dma_ptr = dma_mscal(frag_idx == 0 ? program_addr_1 : program_addr_2);


    }

    // a bunch of bucket patching crap
}
```

The actual asm:
```
L101:                      ;; function prologue
    daddiu sp, sp, -128
    sd ra, 0(sp)
    sq s0, 16(sp)
    sq s1, 32(sp)
    sq s2, 48(sp)
    sq s3, 64(sp)
    sq s4, 80(sp)
    sq s5, 96(sp)
    sq gp, 112(sp)

    ;; one-time setup for this "merc-control". A merc-control is a model (at a particular lod)
    ;; for a process-drawable.
    ;; using dc as the input draw-control (a constant)
    ;; using mc as (-> dc lod-set (-> dc cur-lod) geo), the merc-control we're drawing (a constant)
    ;; using t8 = mep as (-> mc effect <n>), one of the merc-effects in the merc-control (variable)
    ;; using t7 = mec (merc-effect counter), the number of remaining merc-counters
    ;; using t9 = mebp as (-> *foreground* merc-bucket-info effect <n>), one of the merc-bucket-info's filled out by
    ;;  the calling function, containing per-effect settings.
B0:
    or t7, a3, r0         ;; t7 = program-addr-1
    or v1, t0, r0         ;; v1 = program-addr-2
    lui t0, 4096          ;; t0 = 0x10000000
    lui t1, 18304         ;; t1 = 0x47800000
    daddiu t0, t0, 1      ;; t0 = 0x10000001
    dsll32 t1, t1, 0      ;; t1 = 0x47800000'00000000
    lui a3, 12288         ;; a3 = 0x30000000
    lui t8, 19201         ;; t8 = 0x4b010000
    pcpyld t0, a3, t0     ;; t0 = 0x00000000'30000000'00000000'10000001 (STROW)
    lbu a3, 78(a0)        ;; a3 = (-> dc cur-lod)
    pcpyld t1, t8, t1     ;; t1 = 0x00000000'4b010000'47800000'00000000
    lui t2, 28160         ;; t2 = 0x6e000000
    addiu t8, r0, 8       ;; t8 = 8
    multu3 a3, a3, t8     ;; a3 = (* 8 (-> dc cur-lod))
    lui t3, 1280          ;; t3 = 0x05000000
    lui t4, 27648         ;; t4 = 0x6c000000
    dsll32 t2, t2, 0      ;; t2 = 0x6e000000'00000000
    dsll32 t4, t4, 0      ;; t4 = 0x6c000000'00000000
    daddu t4, t4, t3      ;; t4 = 0x6c000000'05000000
    daddu t3, t2, t3      ;; t3 = 0x6e000000'05000000
    daddiu t3, t3, 1      ;; t3 = 0x6e000000'05000001
    daddu a0, a3, a0      ;; a0 = (+ dc (* 8 (-> dc cur-lod)))
    pcpyld t2, t2, r0     ;; t2 = 0x6e000000'00000000'00000000'00000000 (unpack-v4-8, no change to row)
    lw a0, 28(a0)         ;; a0 = (-> dc lod-set (-> dc cur-lod) geo) ;; a merc-ctrl
    pcpyld t3, t3, r0     ;; t3 = 0x6e000000'05000001'00000000'00000000 (unpack-v4-8, row add)
    pcpyld t4, t4, r0     ;; t4 = 0x6c000000'05000000'00000000'00000000 (unpack-v4-32, disable row add)
    lui t5, 12288         ;; t5 = 0x30000000
    lui t6, 4096          ;; t6 = 0x10000000
    daddiu t5, t5, 7      ;; t5 = 0x30000007
    lui t8, 5120          ;; t8 = 0x14000000
    lui a3, 27655         ;; a3 = 0x6c070000
    daddu t7, t8, t7      ;; t7 = 0x14000000 + program-addr-1
    dsll32 a3, a3, 0      ;; a3 = 0x6c070000'00000000
    dsll32 t8, t7, 0      ;; t8 = (0x14000000 + program-addr-1) << 32
    pcpyld t5, a3, t5     ;; t5 = 0x6c070000'00000000'00000000'30000007
    lwu t7, 52(a0)        ;; t7 = (-> mc effect-count)
    pcpyld t6, t8, t6     ;; t6 = ((0x14000000 + program-addr-1) << 32) << 64 + 0x00000000'10000000
    daddiu t8, a0, 156    ;; t8 = (-> mc effect 0) = mep "merc effect pointer"
    beq t7, r0, L109      ;; branch if there's no effects (I think this is buggy and jumps to the wrong spot)
    lw a3, *foreground*(s7) ;; a3 = *foreground*

B1:
    daddiu t9, a3, 2508 ;; t9 = (-> *foreground* merc-bucket-info effect 0)
B2:

   ;; TOP of per-effect loop
   ;; (I've marked lines with stats if they are just for computing statistics)
L102:
    lbu a3, 6(t9)              ;; a3 = (-> mebp disable-draw)
    or ra, a2, r0              ;; ra = start-of-dma-for-this-effect
    lbu gp, 4(t9)              ;; gp = (-> mebp merc-path)
    bne a3, r0, L109           ;; jump to next effect if this is disabled.
    lw a3, *merc-global-stats*(s7) ;; a3 = mgs

B3:
    daddiu a3, a3, 16          ;; a3 = (-> *merc-global-stats* emerc)
    daddiu gp, gp, -1          ;; check if `merc-path` is 1, skip this fragment if it's something else
    sll r0, r0, 0
    bne gp, r0, L109
    lhu s4, 2(a3)              ;; stats.fragments

B4:
    lhu s3, 18(t8)             ;; s3 = (-> mep frag-count)
    lwu gp, 4(a3)              ;; stats
    lhu s5, 22(t8)             ;; s5 = (-> mep tri-count)
    daddu s4, s4, s3           ;; stats
    lwu s3, 8(a3)              ;; stats
    lhu s2, 24(t8)             ;; s2 = (-> mep dvert-count)
    daddu gp, gp, s5           ;; stats
    sh s4, 2(a3)               ;; stats
    sw gp, 4(a3)               ;; stats
    daddu s5, s3, s2           ;; stats
    lwu t2, 0(t8)              ;; t2 = (-> mep frag-geo)
    lwu gp, 4(t8)              ;; gp = (-> mep frag-ctrl)
    lui s4, 12288              ;; 0x30000000
    dsll32 t2, t2, 0           ;; (-> mep frag-geo) << 32
    sw s5, 8(a3)               ;; stats
    or t2, t2, s4              ;; t2 = ((-> mep frag-geo) << 32) + 0x30000000 (upper 64-bits still have dma tmpl)
    lhu s5, 18(t8)             ;; s5 = (-> mep frag-count)
    addiu s4, r0, 0            ;; s4 = 0
    beq s5, r0, L109           ;; skip to next effect if no frags in this effect.
    sll r0, r0, 0

B5:
    sll r0, r0, 0

    ;; top of per-fragment loop.
    ;; s4 = current-frag-idx
    ;; s5 = num-frags
    ;; a2 = dma-ptr
    ;; DMA memory layout
    ;;      lower-bits                               higher bits
    ;; 0   [dmatag-lower, dmatag-upper, strow-viftag, ROW_X      ] ;; transfer 1 qw, immediately after this
    ;; 1   [ROW_Y       , ROW_Z       , ROW_W       , nop-viftag ] ;; the qw transferred by 0
    ;; 2   [dmatag-lower, dmatag-upper, nop         , unpack-v4-8] ;; (unsigned4's)
    ;; 3   [dmatag-lower, dmatag-upper, strow 1     , unpack-v4-8] ;; lumps
    ;; 4   [dmatag-lower, dmatag-upper, strow 0     , unpack-v4-32]
B6:
L103:
    lbu s0, 0(gp)             ;; s0 = frag-ctrl.unsigned-four-count (number of 4xu8's in memory)
    sll r0, r0, 0
    lbu s2, 1(gp)             ;; s2 = frag-ctrl.lump-four-count
    xori s1, r0, 49292        ;; s1 = 0xc08c
    lbu s3, 2(gp)             ;; s3 = frag-ctrl.fp-qwc
    daddiu v0, s0, 3          ;; v0 = u4count + 3
    lw a3, 44(a0)             ;; a3 = header.st-vif-add
    srl v0, v0, 2             ;; v0 = (u4count + 3) / 4
    sq t0, 0(a2)              ;; set DMA qw 0 (dmatag-strow only)
    xor t2, t2, v0            ;; set dma qwc
    sq t2, 32(a2)             ;; store dma line 2.
    xor t2, t2, v0            ;; unset dma qwc
    sh s1, 44(a2)             ;; set addr for unpack (tops + unsigned bits)
    daddu s1, s1, s0          ;; unpdate qwc for next unpack
    sb s0, 46(a2)             ;; set qwc for unpack
    dsll32 s0, v0, 4          ;; v0 = (u4-ee-qwc << 36)
    daddu t3, t2, s0          ;; t3 = dma-tag templ
    daddiu s0, s2, 3          ;; s0 = l4c + 3
    sw a3, 12(a2)             ;; ROW_X = header.st-vif-add
    srl s0, s0, 2             ;; s0 /= 4
    sq t1, 16(a2)             ;; ROW_Z, W
    xor t3, t3, s0            ;; set dma qwc
    sq t3, 48(a2)             ;; store dma templ 3
    xor t3, t3, s0            ;; unset dma qwc
    sh s1, 60(a2)             ;; set vif unpack
    daddu s1, s1, s2          ;; next dest
    sb s2, 62(a2)             ;; store.
    dsll32 s2, s0, 4          ;; s2 = dma-src-inc shifted
    sw a3, 16(a2)             ;; ROW Y
    daddu t4, t3, s2          ;; unpack-v4-32 tmpl
    xor t4, t4, s3            ;; set qwc in dma tmpl
    xori a3, s1, 16384        ;; turn off sign extension in unpack
    sq t4, 64(a2)             ;; store dma 4
    xor t4, t4, s3            ;; unset qwc
    sb s3, 78(a2)             ;; set qwc in unpack
    dsll32 s3, s3, 4          ;; qwc -> bytes
    sh a3, 76(a2)             ;; set unpack
    daddu t2, t4, s3          ;; ?? (maybe reset t2 tmpl)
    lbu s3, 3(gp)             ;; s3 = mat-xfer-count
    daddiu gp, gp, 4          ;; next fragment control
    bne s4, r0, L105          ;; do B7, B8, B9, B10 only on first fragment
    daddiu a2, a2, 80         ;; advance DMA ptr.

B7:
    sd t6, 0(a2)              ;; weirdo dma generation code (somebody had too much fun here)
    addiu s2, r0, 8           ;; transfer 8 qw
    sd t6, 8(a2)              ;; more weird crap
    lui a3, 27656             ;; 0x6c08
    sb s2, 0(a2)              ;; transfer 8 qw
    daddiu a3, a3, 132        ;; to 140
    lw s2, *foreground*(s7)   ;; fg
    daddiu s2, s2, 2384       ;; s2 = merc-bucket-info array
    sw a3, 12(a2)             ;; unpack to 140
    lq a3, 0(s2)              ;; a3 = lights 0
    lq s1, 16(s2)             ;; s1 = lights 1
    lq s0, 32(s2)             ;; s0 = lights 2
    lq v0, 48(s2)             ;; v0 = lights 3
    sq a3, 16(a2)             ;; store lights
    sq s1, 32(a2)
    sq s0, 48(a2)
    sq v0, 64(a2)
    lq a3, 64(s2)             ;; lights again
    lq s1, 80(s2)
    lq s0, 96(s2)             ;; lights 6
    lui v0, 16261
    lq s2, 28(a0)
    daddiu v0, v0, 619        ;; v0 = 0x3f85026b
    sq a3, 80(a2)             ;; light store
    lbu a3, 5(t9)             ;; a3 = ignore-alpha
    sq s1, 96(a2)             ;; lights
    sq s0, 112(a2)            ;; last lights
    dsubu a3, v0, a3          ;; compute ignore alpha
    sq s2, 128(a2)            ;; header
    sw a3, 28(a2)             ;; light[1].w
    daddiu a2, a2, 144        ;; inc dma
    sd t6, 0(a2)
    addiu s2, r0, 6
    sd t6, 8(a2)
    lui a3, 27654             ;; 0x6C06
    sb s2, 0(a2)
    daddiu a3, a3, 118
    sw a3, 12(a2)       ;; to 124
    lw a3, 0(t9)        ;; a3 = color fade
    pextlb a3, r0, a3   ;; unpack u8 to u32's
    pextlh a3, r0, a3
    sq a3, 16(a2)       ;; store color fade
    lw a3, *default-envmap-shader*(s7) ;; envmap ptr.
    lw s2, 28(t8)       ;; merc-extra-info
    beq s2, r0, L104
    sll r0, r0, 0

B8:
    lbu s1, 1(s2)
    beq s1, r0, L104
    sll r0, r0, 0

B9:
    sll a3, s1, 4
    addu a3, s2, a3
B10:
L104:
    lq s2, 0(a3) ;; copy shader to dma buff
    lq s1, 16(a3)
    lq s0, 32(a3)
    lq v0, 48(a3)
    lq a3, 64(a3)
    sq s2, 32(a2)
    sq s1, 48(a2)
    sq s0, 64(a2)
    sq v0, 80(a2)
    sq a3, 96(a2)
    daddiu a2, a2, 112

    ;; after first time per-effect stuff
B11:
L105:
    beq s3, r0, L107
    addiu s2, r0, 128 ;; s2 = 128 (matrix size)

B12:
    lbu a3, 0(gp) ;; get mat number
    sll r0, r0, 0
B13:
L106:
    multu3 s1, a3, s2 ;; s1 = matrix offset in ee world
    sq t5, 0(a2)      ;; mat transfer tmplate
    lbu s0, 1(gp)     ;; mat dest
    daddiu gp, gp, 2  ;; gp = next mat transfer
    lbu a3, 0(gp)     ;; a3 = next matrix offset
    daddiu s3, s3, -1 ;; dec remaining
    sb s0, 12(a2)     ;; store dest
    daddiu a2, a2, 16 ;; inc dma
    daddu s1, s1, a1  ;; compute matrix pointer
    sll r0, r0, 0
    bne s3, r0, L106
    sw s1, -12(a2)

B14:
L107:
    sq t6, 0(a2)
    daddiu a2, a2, 16
    bne s4, r0, L108
    daddiu s4, s4, 1

B15:
    or a3, v1, r0 ;; execute program (1 for first round, 2 for later ones)
    sb a3, -4(a2)
B16:
L108:
    bne s4, s5, L103 ;; loop frag
    sll r0, r0, 0

B17: ;; patching crap, based on texture index now. should document eventually...
    lui s5, 28672
    lbu a3, 26(t8)
    addiu gp, r0, 48
    lw s5, 52(s5)
    mult3 a3, a3, gp
    sll r0, r0, 0
    daddu a3, s5, a3
    sll r0, r0, 0
    lw gp, 12(a3)
    sll r0, r0, 0
    lw s5, 16(a3)
    lui s4, 8192
    sq r0, 0(a2)
    movz gp, ra, gp
    sw s4, 0(a2)
    or s4, a2, r0
    sw gp, 12(a3)
    daddiu a2, a2, 16
    beq s5, r0, L109
    sw s4, 16(a3)

B18:
    sll r0, r0, 0
    sw ra, 4(s5)
B19:
L109:
    daddiu t8, t8, 32
    daddiu t9, t9, 8
    daddiu t7, t7, -1
    bne t7, r0, L102 ;; loop effect
    sll r0, r0, 0

B20:
    or v0, a2, r0
    ld ra, 0(sp)
    lq gp, 112(sp)
    lq s5, 96(sp)
    lq s4, 80(sp)
    lq s3, 64(sp)
    lq s2, 48(sp)
    lq s1, 32(sp)
    lq s0, 16(sp)
    jr ra
    daddiu sp, sp, 128

    sll r0, r0, 0
    sll r0, r0, 0
```

### Summary of above
Overall, it's very similar to merc. There's some extra data transfered:
- the "low memory" stuff setup in `emerc.gc` is 1 QW longer (an extra "`unperspect` QW")
- the `rgba color-fade` is transferred to non-double-buffered memory (like lights) (1 Qw, unpack to u32's)
- 5 QW shader for envmapping (either `*default-envmap-shader*` or one provided in merc extra info)

emerc data appears backward compatible with merc, which makes sense:
- emerc falls back to merc if it's too far away to envmap
- we put emerc stuff through merc code (blending and stuff is wrong, but the geometry comes out right)

The promising thing is that we don't seem to need much extra information to do environment mapping.
I kinda though we'd need another set of texture coordinates, but I don't see where that enters yet.

If all we need is the shader, plus tint values, it would be easy to do this for any model that succeeds with merc.

## EMERC VU1 constants
Triangle strip giftag - same as normal merc exactly (in the normal no-alpha case)
```lisp
(set! (-> s5-0 tri-strip-gif tag)
      (new 'static 'gif-tag64
        :pre #x1
        :prim (new 'static 'gs-prim :prim (gs-prim-type tri-strip) :iip #x1 :tme #x1 :fge #x1)
        :nreg #x3
        )
      )
(set! (-> s5-0 tri-strip-gif regs)
      (new 'static 'gif-tag-regs :regs0 (gif-reg-id st) :regs1 (gif-reg-id rgbaq) :regs2 (gif-reg-id xyzf2))
      )
;; word 3 gets set to #x303e4000
```

### Program list
- `0`: per-frame init
- `19`: effect init
- `29`: process frag

### Memory map
All in 16-byte quadword addresses.
```
Low memory: after DMA
[0    ] : tri-strip-gif (st, rgbaq, xyzf2), no abe, 0x303e4000 in word 3, same as merc.
[1    ] : adgif-shader giftag (giftag for 5 a+d's)
[2    ] : hvdf-offset
[3 - 7] : perspective matrix (only perspective project, no rotation/translation). 3 gets set to persp_vector
[7    ] : fog (pfog0, fog-min, fog-max, 0.0)
[8    ] : unperspect (1/P(0, 0), 1/P(1, 1), 0.5, 1/P(2, 3))

Low memory: after inits (both frame and effect)
[0    ] : tri-strip-gif (st, rgbaq, xyzf2), no abe, 0x303e4000 in word 3, same as merc.
[1    ] : adgif-shader giftag (giftag for 5 a+d's)
[2    ] : hvdf-offset
[3    ] : P_mult = [low.P(0, 0), low.P(1, 1), low.P(2, 2), low.P(2, 3)]
[4    ] : P_add = [low.P(3, 0), low.P(3, 1), low.P(3, 2), low.P(3, 3)]
[5    ] : P_mult_scale = P_mult * header.xyz-scale
[7    ] : fog (pfog0, fog-min, fog-max, 0.0)
[8    ] : unperspect (1/P(0, 0), 1/P(1, 1), 0.5, 1/P(2, 3))

```

### Summary of math

The "transformed vertex" refers to the vertex before perspective divide, and pfog0 multiply.
The "transformed normal" is the rotated normal, after normalization.
```
vf08 = transformed
vf23 = unperspect
vf14 = rgba-fade
vf24 = normal st

mul.xyzw vf09, vf08, vf23 ;; do unperspect

subw.z vf10, vf10, vf00 ;; subtract 1 from z

addw.z vf09, vf00, vf09 ;; xyww the unperspected thing

mul.xyz vf15, vf09, vf10 ;;

adday.xyzw vf15, vf15

maddz.x vf15, vf21, vf15

div Q, vf15.x, vf10.z

mulaw.xyzw ACC, vf09, vf00

mul.xyzw vf09, vf08, vf23

madd.xyzw vf10, vf10, Q

eleng.xyz P, vf10

mfp.w vf10, P

div Q, vf23.z, vf10.w

addaz.xyzw vf00, vf23

madd.xyzw vf10, vf10, Q

mulz.xy vf24, vf10, vf24 ;; mul tex by q

;; new rgba
sq.xyzw vf14, 443(vi10)

;;
vf24
```


### VU1 Program: init (per frame)
```
  lq.xyzw vf01, 7(vi00)      |  nop
  lq.xyzw vf25, 3(vi00)      |  nop
  lq.xyzw vf26, 4(vi00)      |  nop
  lq.xyzw vf27, 5(vi00)      |  nop
  lq.xyzw vf28, 6(vi00)      |  nop
  lq.xyzw vf08, 8(vi00)      |  nop
  mr32.xyzw vf01, vf01       |  nop
  move.y vf25, vf26          |  nop
  move.zw vf25, vf27         |  nop
  sq.xyzw vf25, 3(vi00)      |  nop
  sq.xyzw vf08, 124(vi00)    |  nop
  2048.0                     |  nop :i
  255.0                      |  maxi.x vf17, vf00, I :i
  -65537.0                   |  maxi.y vf17, vf00, I :i
  mr32.xyzw vf02, vf01       |  minii.z vf17, vf00, I
  lq.xyzw vf22, 2(vi00)      |  minii.z vf18, vf00, I
  0.003921569                |  minii.z vf19, vf00, I :i
  sq.xyzw vf28, 4(vi00)      |  minii.w vf29, vf00, I :e
  mr32.xyzw vf03, vf02       |  nop
```

Simplified code (`??`'s are either garbage, or some value that isn't important later on). Leaving out stores to low memory documented in the Memory Map section.
```
vf01 = [??, ??, ??, low.pfog0]
vf02 = [??, ??, ??, low.fog_min]
vf03 = [??, ??, ??, low.fog_max]
vf17 = [2048., 255., -65537., ??]
vf22 = low_in.hvdf_offset
```

### VU1 Program: init (per effect)
Note that this continues directly into the per-frag program, to match the note in frag == 0 case in the dma generation part.

```
  lq.xyzw vf25, 139(vi00)    |  nop
  lq.xyzw vf26, 3(vi00)      |  nop
  lq.xyz vf01, 132(vi00)     |  nop
  lq.xyz vf02, 133(vi00)     |  nop
  lq.xyz vf03, 134(vi00)     |  addy.xy vf19, vf00, vf25
  lq.xyzw vf04, 135(vi00)    |  mulx.xyzw vf26, vf26, vf25
  lq.xyzw vf05, 136(vi00)    |  nop
  lq.xyzw vf06, 137(vi00)    |  nop
  lq.xyzw vf07, 138(vi00)    |  nop
  sq.xyzw vf26, 5(vi00)      |  nop ;; P_mult_scale store.
```

Simplified code (note: some of this stuff set later)
```
vf25 = [xyz-scale, st-magic, st-out-a, st-out-b];
vf26 = low.P_mult * xyz-scale;
vf01 = [lt0.xyz, pfog0]
vf02 = [lt1.xyz, fog-min]
vf03 = [lt2.xyz, fog-max]
vf19 = [st-magic, st-magic, -65537, xyz-add.z];
vf04 = lt0_color;
vf05 = lt1_color;
vf06 = lt2_color;
```

### VU1 Program: per-fragment, pre-looping init
```
;; reg setup stuff
  lq.xyzw vf28, 139(vi00)    |  minix.xyzw vf15, vf00, vf00    ;; vf28 = merc-ctrl-header, vf15 = [0, 0, 0, 0]
  xtop vi15                  |  nop                            ;; vi15 = 0 (output buffer)
  iaddiu vi12, vi15, 0x8c    |  nop                            ;; vi12 = xtop + 140 (merc-byte-header, u4)
  nop                        |  nop                            ;; in merc was a branch for st-a/st-b select.
  ilwr.w vi03, vi12          |  maxz.xy vf18, vf00, vf28       ;; set vf18.xy = [st-out-a, st-out-a] (for a buffer)
  iaddiu vi15, vi00, 0x173   |  nop                            ;; vi15 = xtop + 371
  lq.xyzw vf14, 0(vi00)      |  nop                            ;; vf14 = tri-strip-gif-tag
  nop                        |  nop                            ;; in merc was fadeout
  iadd vi03, vi03, vi12      |  nop                            ;; st-output location = st-out-a + xtop + 140
  ilwr.w vi09, vi03          |  nop          ;; vi09 = fp-header u8's [shader-cnt, kick-off, kick-step, hword-cnt]
  lqi.xyzw vf27, vi03        |  nop          ;; vf27 = xyz-add
  ilw.x vi04, 1(vi12)        |  nop          ;; vi04 = mat1-cnt
  iaddiu vi05, vi00, 0x7f    |  addw.xyz vf15, vf15, vf00  ;; vf15 = [1, 1, 1, 0], vi05 = 0x7f
  iand vi09, vi09, vi05      |  nop                        ;; mask to get vi09 = shader-cnt
  ilw.y vi06, 1(vi12)        |  miniz.w vf19, vf00, vf27   ;; setup vf19, vi06 = mat2-cnt
  nop                        |  miniy.w vf18, vf00, vf27   ;; setup vf18, merc had branch for no strips.
  ilwr.z vi01, vi12          |  minix.w vf17, vf00, vf27   ;; vi01 = lump-off

;;  vf17 = [2048, 255, -65537, xyz-add.x]
;;  vf18 = [st-out-X, st-out-X, -65537, xyz-add.y] (X = a if xtop = 0, X = b otherwise)
;;  vf19 = [st-magic, st-magic, -65537, xyz-add.z]

;; shader setup (not envmap)
  lq.xyzw vf13, 1(vi00)      |  nop     ;; vf13 = adgif gif tag.
  ilwr.w vi02, vi03          |  nop     ;; vi02 = shader control word 0 (dest offset)
  lqi.xyzw vf08, vi03        |  nop     ;; load shader data
  lqi.xyzw vf09, vi03        |  nop
  lqi.xyzw vf10, vi03        |  nop
  lqi.xyzw vf11, vi03        |  nop
  lqi.xyzw vf12, vi03        |  nop
  iadd vi02, vi02, vi15      |  nop     ;; compute destination
  mtir vi08, vf09.w          |  nop     ;; eop stuff (not sure this makes sense in 1-shader emerc)
  sqi.xyzw vf13, vi02        |  nop     ;; store adgif gif tag
  sqi.xyzw vf08, vi02        |  nop     ;; shader store 1
  sqi.xyzw vf09, vi02        |  nop     ;; shader store 2
  mfir.x vf14, vi08          |  nop     ;; set eop bit in giftag template
  sqi.xyzw vf10, vi02        |  nop     ;; shader store 3
  sqi.xyzw vf11, vi02        |  nop     ;; shader store 4
  sqi.xyzw vf12, vi02        |  nop     ;; shader store 5
  sq.xyzw vf14, 0(vi02)      |  nop     ;; store end giftag

;; matrix warmup
  lq.xyzw vf28, 3(vi00)      |  nop     ;; vf28 = persp-diag
  ilw.y vi08, 3(vi12)        |  nop     ;; vi08 = mat-slot.0
  lq.xyzw vf16, 5(vi00)      |  nop     ;; vf16 = scaled-persp-diag
  lq.xyzw vf20, 4(vi00)      |  nop     ;; vf20 = persp-off
  ilw.z vi09, 3(vi12)        |  mul.xyzw vf27, vf28, vf15  ;; vf27 = [pdx, pdy, pdz, 0], vi09 = mat-slot.1
  ior vi11, vi08, vi00       |  mul.xyzw vf28, vf28, vf00  ;; vf28 = [0, 0, 0, pdw], vi11 = vi08 = mat-slot.0
  ibeq vi00, vi08, L2        |  mul.xyzw vf15, vf16, vf15  ;; vf15 = [spdx, spdy, spdz, 0], skip if slot = 0
  iaddi vi13, vi12, 0x3      |  mul.xyzw vf16, vf16, vf00  ;; vi13 = mat-slot-ptr, vf16 = [0, 0, 0, spdw]
```
- mostly same as merc
- always picks `st-a`, merc had a branch here based on state of `xtop`.
- no fade out flag stuff

### Matrix multiply loop
Premultiplies uploaded matrices by perspective. Only does matrices that were uploaded this time.
Same as merc, so skipping.

### The rest of it
- Transformed vertex (before perspective divide and pfog0 multiply is store back over `lump[2]`)
- Transformed normal is stored over `rgba`
```
L2: (L14 in og merc)
;; Pipelining Start for vertex transform
  ilw.x vi02, 3(vi12)        |  nop  ;; vi02 = perc-off
  ibeq vi00, vi04, L13       |  nop  ;; goto L13 if mat1 count is 0
  iadd vi01, vi01, vi12      |  nop  ;; vi01 = lump.

;; Pipelining start for matrix 1's
  ilwr.x vi08, vi01          |  nop ;; vi08 = lump[0].x = mat-0?
  lqi.xyzw vf08, vi01        |  nop
  lqi.xyzw vf11, vi01        |  nop
  lqi.xyzw vf14, vi01        |  nop ;; vf14 = lump[2] = [texs, text, nrmz, posz]
  lq.xyz vf29, 4(vi08)       |  nop
  lq.xyz vf30, 5(vi08)       |  add.zw vf08, vf08, vf17
  lq.xyzw vf31, 6(vi08)      |  add.xyzw vf11, vf11, vf18
  iaddi vi04, vi04, -0x1     |  add.xyzw vf14, vf14, vf19
  iadd vi02, vi02, vi12      |  nop
  lqi.xyzw vf24, vi02        |  mulaz.xyzw ACC, vf29, vf08
  mtir vi10, vf11.x          |  maddaz.xyzw ACC, vf30, vf11
  mtir vi13, vf11.y          |  maddz.xyz vf11, vf31, vf14
  lq.xyzw vf25, 0(vi08)      |  nop
  lq.xyzw vf26, 1(vi08)      |  itof0.xyzw vf24, vf24
  lq.xyzw vf27, 2(vi08)      |  nop
  erleng.xyz P, vf11         |  nop
  lq.xyzw vf28, 3(vi08)      |  mulaw.xyzw ACC, vf25, vf08
  nop                        |  maddaw.xyzw ACC, vf26, vf11 ;; modified from merc, no mercprime crap
  mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  lqi.xyzw vf09, vi01        |  nop
  ilwr.y vi03, vi12          |  nop
  ilw.z vi07, 1(vi12)        |  nop
  lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  lqi.xyzw vf15, vi01        |  nop
  mtir vi08, vf09.x          |  nop ;; mercprime stuff in og.

  ;; CHANGE: transformed vf08 (pre perspective divide, pfog mult)
  ;; is stored back! over lop lump[2] (texs, text, nrmz, posz)
  sq.xyzw vf08, -4(vi01)     |  miniw.w vf08, vf08, vf01

  iadd vi03, vi03, vi12      |  nop
  div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  iadd vi04, vi04, vi03      |  add.xyzw vf12, vf12, vf18
  lq.xyz vf29, 4(vi08)       |  add.xyzw vf15, vf15, vf19
  lq.xyz vf30, 5(vi08)       |  nop
  iadd vi06, vi06, vi04      |  nop
  lq.xyzw vf31, 6(vi08)      |  nop
  lq.xyzw vf25, 0(vi08)      |  nop
  lq.xyzw vf26, 1(vi08)      |  mul.xyz vf08, vf08, Q
  mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  mtir vi14, vf12.y          |  nop
  lq.xyzw vf27, 2(vi08)      |  nop
  lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22  ;; load rgba, hvdf offset
  iadd vi07, vi07, vi06      |  mulaz.xyzw ACC, vf29, vf09
  lq.xyzw vf28, 3(vi08)      |  maddaz.xyzw ACC, vf30, vf12
  mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  nop                        |  nop
  1024.0                     |  miniw.w vf08, vf08, vf03 :i
  nop                        |  mulaw.xyzw ACC, vf25, vf09  ;; modified, no mercprime branch
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20  ;;
  erleng.xyz P, vf12         |  maxi.xy vf08, vf08, I       ;; like mercprimt path (L82 in og merc)
  3072.0                     |  nop :i
  nop                        |  minii.xy vf08, vf08, I

;; CHANGE store back normal over RGBA.
  sq.xyzw vf11, -1(vi03)     |  maddaw.xyzw ACC, vf26, vf12
  mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  lqi.xyzw vf10, vi01        |  mulax.xyzw ACC, vf01, vf11
  ibne vi04, vi03, L4        |  madday.xyzw ACC, vf02, vf11 ;; branch to L4, pipelined mat 1
  nop                        |  maddz.xyzw vf11, vf03, vf11
  ibne vi06, vi03, L17       |  nop
  nop                        |  nop
  b L52                      |  nop
  nop                        |  nop

  ;; pipelined mat 1 loop start
L3: (L16 in og)
  sq.xyzw vf11, -1(vi03)     |  nop                             ;; normal store back
  3072.0                     |  mulax.xyzw ACC, vf01, vf11 :i   ;; mercprime crap
  lqi.xyzw vf10, vi01        |  minii.xy vf08, vf08, I
  sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
  ;; pipelined mat 1 entry point
L4: (L17 in og)
  lqi.xyzw vf13, vi01        |  add.xyzw vf09, vf09, vf28
  lqi.xyzw vf16, vi01        |  maxw.w vf08, vf08, vf02
  mtir vi08, vf10.x          |  itof0.xyzw vf23, vf23
  ilw.y vi09, -9(vi01)       |  maxx.xyzw vf11, vf11, vf00
  sq.xyzw vf09, -4(vi01)     |  miniw.w vf09, vf09, vf01
  div Q, vf01.w, vf09.w      |  add.zw vf10, vf10, vf17
  move.xyzw vf21, vf08       |  add.xyzw vf13, vf13, vf18
  lq.xyz vf29, 4(vi08)       |  add.xyzw vf16, vf16, vf19
  lq.xyz vf30, 5(vi08)       |  mulax.xyzw ACC, vf04, vf11
  ibgtz vi09, L5             |  madday.xyzw ACC, vf05, vf11
  lq.xyzw vf31, 6(vi08)      |  maddaz.xyzw ACC, vf06, vf11
  nop                        |  addx.w vf21, vf21, vf17
L5: (L18 in og)
  lq.xyzw vf25, 0(vi08)      |  maddw.xyzw vf11, vf07, vf00
  lq.xyzw vf26, 1(vi08)      |  mul.xyz vf09, vf09, Q
  mtir vi12, vf13.x          |  mul.xyzw vf15, vf15, Q
  mtir vi15, vf13.y          |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf27, 2(vi08)      |  mul.xyzw vf11, vf11, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf09, vf09, vf22
  ibne vi00, vi09, L6        |  mulaz.xyzw ACC, vf29, vf10
  sq.xyzw vf21, 2(vi10)      |  maddaz.xyzw ACC, vf30, vf13
  nop                        |  ftoi4.xyzw vf21, vf08
L6: (L19 in og)
  mfp.w vf20, P              |  maddz.xyz vf13, vf31, vf16
  sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  sq.xyzw vf14, 0(vi13)      |  miniw.w vf09, vf09, vf03
  sq.xyzw vf21, 2(vi13)      |  mulaw.xyzw ACC, vf25, vf10
  lq.xyzw vf28, 3(vi08)      |  mulw.xyzw vf12, vf12, vf20
  1024.0                     |  ftoi0.xyzw vf11, vf11 :i
  erleng.xyz P, vf13         |  maxi.xy vf09, vf09, I
  ibne vi04, vi03, L7        |  maddaw.xyzw ACC, vf26, vf13
  mr32.z vf16, vf00          |  maddw.xyzw vf10, vf27, vf16
  ibne vi06, vi03, L22       |  nop
  ilw.y vi09, -6(vi01)       |  nop
  ibne vi07, vi03, L57       |  nop
  nop                        |  nop
  b L67                      |  nop
  nop                        |  nop
L7: (L20 in og)
  sq.xyzw vf12, -1(vi03)     |  nop
  3072.0                     |  mulax.xyzw ACC, vf01, vf12 :i
  lqi.xyzw vf08, vi01        |  minii.xy vf09, vf09, I
  sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  lqi.xyzw vf11, vi01        |  add.xyzw vf10, vf10, vf28
  lqi.xyzw vf14, vi01        |  maxw.w vf09, vf09, vf02
  mtir vi08, vf08.x          |  itof0.xyzw vf23, vf23
  ilw.y vi09, -9(vi01)       |  maxx.xyzw vf12, vf12, vf00
  sq.xyzw vf10, -4(vi01)     |  miniw.w vf10, vf10, vf01
  div Q, vf01.w, vf10.w      |  add.zw vf08, vf08, vf17
  move.xyzw vf21, vf09       |  add.xyzw vf11, vf11, vf18
  lq.xyz vf29, 4(vi08)       |  add.xyzw vf14, vf14, vf19
  lq.xyz vf30, 5(vi08)       |  mulax.xyzw ACC, vf04, vf12
  ibgtz vi09, L8             |  madday.xyzw ACC, vf05, vf12
  lq.xyzw vf31, 6(vi08)      |  maddaz.xyzw ACC, vf06, vf12
  nop                        |  addx.w vf21, vf21, vf17
L8: (L21 in og)
  lq.xyzw vf25, 0(vi08)      |  maddw.xyzw vf12, vf07, vf00
  lq.xyzw vf26, 1(vi08)      |  mul.xyz vf10, vf10, Q
  mtir vi10, vf11.x          |  mul.xyzw vf16, vf16, Q
  mtir vi13, vf11.y          |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf27, 2(vi08)      |  mul.xyzw vf12, vf12, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf10, vf10, vf22
  ibne vi00, vi09, L9        |  mulaz.xyzw ACC, vf29, vf08
  sq.xyzw vf21, 2(vi11)      |  maddaz.xyzw ACC, vf30, vf11
  nop                        |  ftoi4.xyzw vf21, vf09
L9: (L22 in og)
  mfp.w vf20, P              |  maddz.xyz vf11, vf31, vf14
  sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  sq.xyzw vf15, 0(vi14)      |  miniw.w vf10, vf10, vf03
  sq.xyzw vf21, 2(vi14)      |  mulaw.xyzw ACC, vf25, vf08
  lq.xyzw vf28, 3(vi08)      |  mulw.xyzw vf13, vf13, vf20
  1024.0                     |  ftoi0.xyzw vf12, vf12 :i
  erleng.xyz P, vf11         |  maxi.xy vf10, vf10, I
  ibne vi04, vi03, L10       |  maddaw.xyzw ACC, vf26, vf11
  mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  ibne vi06, vi03, L27       |  nop
  ilw.y vi09, -6(vi01)       |  nop
  ibne vi07, vi03, L62       |  nop
  nop                        |  nop
  b L72                      |  nop
  nop                        |  nop
L10: (L23 in og)
  sq.xyzw vf13, -1(vi03)     |  nop
  3072.0                     |  mulax.xyzw ACC, vf01, vf13 :i
  lqi.xyzw vf09, vi01        |  minii.xy vf10, vf10, I
  sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  lqi.xyzw vf15, vi01        |  maxw.w vf10, vf10, vf02
  mtir vi08, vf09.x          |  itof0.xyzw vf23, vf23
  ilw.y vi09, -9(vi01)       |  maxx.xyzw vf13, vf13, vf00
  sq.xyzw vf08, -4(vi01)     |  miniw.w vf08, vf08, vf01
  div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  move.xyzw vf21, vf10       |  add.xyzw vf12, vf12, vf18
  lq.xyz vf29, 4(vi08)       |  add.xyzw vf15, vf15, vf19
  lq.xyz vf30, 5(vi08)       |  mulax.xyzw ACC, vf04, vf13
  ibgtz vi09, L11            |  madday.xyzw ACC, vf05, vf13
  lq.xyzw vf31, 6(vi08)      |  maddaz.xyzw ACC, vf06, vf13
  nop                        |  addx.w vf21, vf21, vf17
L11: (L24 in og)
  lq.xyzw vf25, 0(vi08)      |  maddw.xyzw vf13, vf07, vf00
  lq.xyzw vf26, 1(vi08)      |  mul.xyz vf08, vf08, Q
  mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  mtir vi14, vf12.y          |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf27, 2(vi08)      |  mul.xyzw vf13, vf13, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
  ibne vi00, vi09, L12       |  mulaz.xyzw ACC, vf29, vf09
  sq.xyzw vf21, 2(vi12)      |  maddaz.xyzw ACC, vf30, vf12
  nop                        |  ftoi4.xyzw vf21, vf10
L12: (L25 in og)
  mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  sq.xyzw vf16, 0(vi15)      |  miniw.w vf08, vf08, vf03
  sq.xyzw vf21, 2(vi15)      |  mulaw.xyzw ACC, vf25, vf09
  lq.xyzw vf28, 3(vi08)      |  mulw.xyzw vf11, vf11, vf20
  1024.0                     |  ftoi0.xyzw vf13, vf13 :i
  erleng.xyz P, vf12         |  maxi.xy vf08, vf08, I
  ibne vi04, vi03, L3        |  maddaw.xyzw ACC, vf26, vf12
  mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  ibne vi06, vi03, L16       |  nop
  ilw.y vi09, -6(vi01)       |  nop
  ibne vi07, vi03, L51       |  nop
  nop                        |  nop
  b L77                      |  nop
  nop                        |  nop

L13 (L26 in og merc):
 ;; pipeline startup for mat 2's (assuming you have no mat1's)
  ibeq vi00, vi06, L47       |  nop
  iadd vi02, vi02, vi12      |  nop
  lqi.xyzw vf08, vi01        |  nop
  lqi.xyzw vf24, vi02        |  nop
  lqi.xyzw vf11, vi01        |  nop
  lqi.xyzw vf14, vi01        |  nop
  mtir vi10, vf08.x          |  nop
  mtir vi13, vf08.y          |  itof0.xyzw vf24, vf24
  iaddi vi06, vi06, -0x1     |  add.zw vf08, vf08, vf17
  nop                        |  add.xyzw vf11, vf11, vf18
  iand vi10, vi10, vi05      |  add.xyzw vf14, vf14, vf19
  nop                        |  mulw.xyzw vf24, vf24, vf29
  iand vi13, vi13, vi05      |  nop
  lq.xyzw vf20, 0(vi10)      |  nop
  lq.xyzw vf25, 0(vi13)      |  nop
  lq.xyzw vf23, 1(vi10)      |  nop
  lq.xyzw vf26, 1(vi13)      |  nop
  lq.xyzw vf20, 2(vi10)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf27, 2(vi13)      |  maddy.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 3(vi10)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf28, 3(vi13)      |  maddy.xyzw vf26, vf26, vf24
  lq.xyzw vf20, 4(vi10)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi13)       |  maddy.xyzw vf27, vf27, vf24
  lq.xyzw vf23, 5(vi10)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi13)       |  maddy.xyzw vf28, vf28, vf24
  lq.xyzw vf20, 6(vi10)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf31, 6(vi13)      |  maddy.xyz vf29, vf29, vf24
  mtir vi10, vf11.x          |  mulax.xyzw ACC, vf23, vf24
  mtir vi13, vf11.y          |  maddy.xyz vf30, vf30, vf24
  nop                        |  mulax.xyzw ACC, vf20, vf24
  nop                        |  maddy.xyzw vf31, vf31, vf24
  nop                        |  mulaz.xyzw ACC, vf29, vf08
  nop                        |  maddaz.xyzw ACC, vf30, vf11
  nop                        |  maddz.xyz vf11, vf31, vf14
  nop                        |  nop
  nop                        |  nop
  nop                        |  mulaw.xyzw ACC, vf25, vf08
  nop                        |  nop
  erleng.xyz P, vf11         |  nop
  nop                        |  maddaw.xyzw ACC, vf26, vf11
  mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  lqi.xyzw vf09, vi01        |  nop
  ilwr.y vi03, vi12          |  nop
  ilw.z vi07, 1(vi12)        |  nop
  lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  lqi.xyzw vf15, vi01        |  nop
  mtir vi11, vf09.x          |  nop
  mtir vi14, vf09.y          |  nop
  sq.xyzw vf08, -4(vi01)     |  miniw.w vf08, vf08, vf01
  div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  iadd vi03, vi03, vi12      |  add.xyzw vf12, vf12, vf18
  iand vi11, vi11, vi05      |  add.xyzw vf15, vf15, vf19
  iadd vi06, vi06, vi03      |  nop
  iadd vi07, vi07, vi06      |  nop
  iand vi14, vi14, vi05      |  nop
  ibne vi05, vi11, L14       |  nop
  iaddiu vi08, vi00, 0x23a   |  mul.xyz vf08, vf08, Q
  mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  mtir vi14, vf12.y          |  nop
  b L15                      |  nop
  lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
L14: (L28 in og)
  lq.xyzw vf20, 0(vi11)      |  mul.xyzw vf14, vf14, Q
  lq.xyzw vf25, 0(vi14)      |  nop
  lq.xyzw vf23, 1(vi11)      |  nop
  lq.xyzw vf26, 1(vi14)      |  add.xyzw vf08, vf08, vf22
  lq.xyzw vf20, 2(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyzw vf27, 2(vi14)      |  maddw.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 3(vi11)      |  mulaz.xyzw ACC, vf23, vf24
  lq.xyzw vf28, 3(vi14)      |  maddw.xyzw vf26, vf26, vf24
  lq.xyzw vf20, 4(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi14)       |  maddw.xyzw vf27, vf27, vf24
  lq.xyzw vf23, 5(vi11)      |  mulaz.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi14)       |  maddw.xyzw vf28, vf28, vf24
  lq.xyzw vf20, 6(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyzw vf31, 6(vi14)      |  maddw.xyz vf29, vf29, vf24
  lqi.xyzw vf23, vi02        |  mulaz.xyzw ACC, vf23, vf24
  mtir vi11, vf12.x          |  maddw.xyz vf30, vf30, vf24
  mtir vi14, vf12.y          |  mulaz.xyzw ACC, vf20, vf24
  iaddiu vi08, vi00, 0x18c   |  maddw.xyzw vf31, vf31, vf24
  lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
L15: (L29 in og)
  nop                        |  mulaz.xyzw ACC, vf29, vf09
  nop                        |  maddaz.xyzw ACC, vf30, vf12
  mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  nop                        |  nop
  1024.0                     |  miniw.w vf08, vf08, vf03 :i
  nop                        |  mulaw.xyzw ACC, vf25, vf09
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20
  erleng.xyz P, vf12         |  maxi.xy vf08, vf08, I
  3072.0                     |  nop :i
  sq.xyzw vf11, -1(vi03)     |  minii.xy vf08, vf08, I
  ibeq vi06, vi03, L50       |  maddaw.xyzw ACC, vf26, vf12
  mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  lqi.xyzw vf10, vi01        |  mulax.xyzw ACC, vf01, vf11
  jr vi08                    |  madday.xyzw ACC, vf02, vf11
  nop                        |  maddz.xyzw vf11, vf03, vf11
L16: (L30 in og)
  sq.xyzw vf11, -1(vi03)     |  nop
  3072.0                     |  mulax.xyzw ACC, vf01, vf11 :i
  lqi.xyzw vf10, vi01        |  minii.xy vf08, vf08, I
  sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
L17: (L31 in og)
  lqi.xyzw vf13, vi01        |  add.xyzw vf09, vf09, vf28
  lqi.xyzw vf16, vi01        |  maxw.w vf08, vf08, vf02
  mtir vi12, vf10.x          |  itof0.xyzw vf23, vf23
  mtir vi15, vf10.y          |  maxx.xyzw vf11, vf11, vf00
  sq.xyzw vf09, -4(vi01)     |  miniw.w vf09, vf09, vf01
  div Q, vf01.w, vf09.w      |  add.zw vf10, vf10, vf17
  move.xyzw vf21, vf08       |  add.xyzw vf13, vf13, vf18
  iand vi12, vi12, vi05      |  add.xyzw vf16, vf16, vf19
  nop                        |  mulax.xyzw ACC, vf04, vf11
  ibgtz vi09, L18            |  madday.xyzw ACC, vf05, vf11
  iand vi15, vi15, vi05      |  maddaz.xyzw ACC, vf06, vf11
  nop                        |  addx.w vf21, vf21, vf17
L18: (L32 in og)
  ibne vi05, vi12, L19       |  maddw.xyzw vf11, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf09, vf09, Q
  mtir vi12, vf13.x          |  mul.xyzw vf15, vf15, Q
  mtir vi15, vf13.y          |  ftoi4.xyzw vf21, vf21
  b L20                      |  mul.xyzw vf11, vf11, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf09, vf09, vf22
L19: (L33 in og)
  lq.xyzw vf20, 0(vi12)      |  mul.xyzw vf15, vf15, Q
  nop                        |  mulw.xyzw vf24, vf24, vf29
  lq.xyzw vf25, 0(vi15)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf23, 1(vi12)      |  mul.xyzw vf11, vf11, vf23
  lq.xyzw vf26, 1(vi15)      |  add.xyzw vf09, vf09, vf22
  lq.xyzw vf20, 2(vi12)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf27, 2(vi15)      |  maddy.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 3(vi12)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf28, 3(vi15)      |  maddy.xyzw vf26, vf26, vf24
  lq.xyzw vf20, 4(vi12)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi15)       |  maddy.xyzw vf27, vf27, vf24
  lq.xyzw vf23, 5(vi12)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi15)       |  maddy.xyzw vf28, vf28, vf24
  lq.xyzw vf20, 6(vi12)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf31, 6(vi15)      |  maddy.xyz vf29, vf29, vf24
  mtir vi12, vf13.x          |  mulax.xyzw ACC, vf23, vf24
  mtir vi15, vf13.y          |  maddy.xyz vf30, vf30, vf24
  b L35                      |  mulax.xyzw ACC, vf20, vf24
  lqi.xyzw vf23, vi03        |  maddy.xyzw vf31, vf31, vf24
L20: (L34 in og)
  ibgez vi09, L21            |  mulaz.xyzw ACC, vf29, vf10
  sq.xyzw vf21, 2(vi10)      |  maddaz.xyzw ACC, vf30, vf13
  nop                        |  ftoi4.xyzw vf21, vf08
L21: (L35 in og)
  mfp.w vf20, P              |  maddz.xyz vf13, vf31, vf16
  sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  sq.xyzw vf14, 0(vi13)      |  miniw.w vf09, vf09, vf03
  sq.xyzw vf21, 2(vi13)      |  mulaw.xyzw ACC, vf25, vf10
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf12, vf12, vf20
  1024.0                     |  ftoi0.xyzw vf11, vf11 :i
  erleng.xyz P, vf13         |  maxi.xy vf09, vf09, I
  ibne vi06, vi03, L22       |  maddaw.xyzw ACC, vf26, vf13
  mr32.z vf16, vf00          |  maddw.xyzw vf10, vf27, vf16
  ibne vi07, vi03, L57       |  nop
  nop                        |  nop
  b L67                      |  nop
  nop                        |  nop
L22: (L36 in og)
  sq.xyzw vf12, -1(vi03)     |  nop
  3072.0                     |  mulax.xyzw ACC, vf01, vf12 :i
  lqi.xyzw vf08, vi01        |  minii.xy vf09, vf09, I
  sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  lqi.xyzw vf11, vi01        |  add.xyzw vf10, vf10, vf28
  lqi.xyzw vf14, vi01        |  maxw.w vf09, vf09, vf02
  mtir vi10, vf08.x          |  itof0.xyzw vf23, vf23
  mtir vi13, vf08.y          |  maxx.xyzw vf12, vf12, vf00
  sq.xyzw vf10, -4(vi01)     |  miniw.w vf10, vf10, vf01
  div Q, vf01.w, vf10.w      |  add.zw vf08, vf08, vf17
  move.xyzw vf21, vf09       |  add.xyzw vf11, vf11, vf18
  iand vi10, vi10, vi05      |  add.xyzw vf14, vf14, vf19
  nop                        |  mulax.xyzw ACC, vf04, vf12
  ibgtz vi09, L23            |  madday.xyzw ACC, vf05, vf12
  iand vi13, vi13, vi05      |  maddaz.xyzw ACC, vf06, vf12
  nop                        |  addx.w vf21, vf21, vf17
L23: (L37 in og)
  ibne vi05, vi10, L24       |  maddw.xyzw vf12, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf10, vf10, Q
  mtir vi10, vf11.x          |  mul.xyzw vf16, vf16, Q
  mtir vi13, vf11.y          |  ftoi4.xyzw vf21, vf21
  b L25                      |  mul.xyzw vf12, vf12, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf10, vf10, vf22
L24: (L38 in og)
  lq.xyzw vf20, 0(vi10)      |  mul.xyzw vf16, vf16, Q
  nop                        |  mulw.xyzw vf24, vf24, vf29
  lq.xyzw vf25, 0(vi13)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf23, 1(vi10)      |  mul.xyzw vf12, vf12, vf23
  lq.xyzw vf26, 1(vi13)      |  add.xyzw vf10, vf10, vf22
  lq.xyzw vf20, 2(vi10)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf27, 2(vi13)      |  maddy.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 3(vi10)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf28, 3(vi13)      |  maddy.xyzw vf26, vf26, vf24
  lq.xyzw vf20, 4(vi10)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi13)       |  maddy.xyzw vf27, vf27, vf24
  lq.xyzw vf23, 5(vi10)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi13)       |  maddy.xyzw vf28, vf28, vf24
  lq.xyzw vf20, 6(vi10)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf31, 6(vi13)      |  maddy.xyz vf29, vf29, vf24
  mtir vi10, vf11.x          |  mulax.xyzw ACC, vf23, vf24
  mtir vi13, vf11.y          |  maddy.xyz vf30, vf30, vf24
  b L40                      |  mulax.xyzw ACC, vf20, vf24
  lqi.xyzw vf23, vi03        |  maddy.xyzw vf31, vf31, vf24
L25: (L39 in og)
  ibgez vi09, L26            |  mulaz.xyzw ACC, vf29, vf08
  sq.xyzw vf21, 2(vi11)      |  maddaz.xyzw ACC, vf30, vf11
  nop                        |  ftoi4.xyzw vf21, vf09
L26: (L40 in og)
  mfp.w vf20, P              |  maddz.xyz vf11, vf31, vf14
  sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  sq.xyzw vf15, 0(vi14)      |  miniw.w vf10, vf10, vf03
  sq.xyzw vf21, 2(vi14)      |  mulaw.xyzw ACC, vf25, vf08
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf13, vf13, vf20
  1024.0                     |  ftoi0.xyzw vf12, vf12 :i
  erleng.xyz P, vf11         |  maxi.xy vf10, vf10, I
  ibne vi06, vi03, L27       |  maddaw.xyzw ACC, vf26, vf11
  mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  ibne vi07, vi03, L62       |  nop
  nop                        |  nop
  b L72                      |  nop
  nop                        |  nop
L27: (L41 in og)
  sq.xyzw vf13, -1(vi03)     |  nop
  3072.0                     |  mulax.xyzw ACC, vf01, vf13 :i
  lqi.xyzw vf09, vi01        |  minii.xy vf10, vf10, I
  sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  lqi.xyzw vf15, vi01        |  maxw.w vf10, vf10, vf02
  mtir vi11, vf09.x          |  itof0.xyzw vf23, vf23
  mtir vi14, vf09.y          |  maxx.xyzw vf13, vf13, vf00
  sq.xyzw vf08, -4(vi01)     |  miniw.w vf08, vf08, vf01
  div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  move.xyzw vf21, vf10       |  add.xyzw vf12, vf12, vf18
  iand vi11, vi11, vi05      |  add.xyzw vf15, vf15, vf19
  nop                        |  mulax.xyzw ACC, vf04, vf13
  ibgtz vi09, L28            |  madday.xyzw ACC, vf05, vf13
  iand vi14, vi14, vi05      |  maddaz.xyzw ACC, vf06, vf13
  nop                        |  addx.w vf21, vf21, vf17
L28: (L42 in og)
  ibne vi05, vi11, L29       |  maddw.xyzw vf13, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf08, vf08, Q
  mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  mtir vi14, vf12.y          |  ftoi4.xyzw vf21, vf21
  b L30                      |  mul.xyzw vf13, vf13, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
L29: (L43 in og)
  lq.xyzw vf20, 0(vi11)      |  mul.xyzw vf14, vf14, Q
  nop                        |  mulw.xyzw vf24, vf24, vf29
  lq.xyzw vf25, 0(vi14)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf23, 1(vi11)      |  mul.xyzw vf13, vf13, vf23
  lq.xyzw vf26, 1(vi14)      |  add.xyzw vf08, vf08, vf22
  lq.xyzw vf20, 2(vi11)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf27, 2(vi14)      |  maddy.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 3(vi11)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf28, 3(vi14)      |  maddy.xyzw vf26, vf26, vf24
  lq.xyzw vf20, 4(vi11)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi14)       |  maddy.xyzw vf27, vf27, vf24
  lq.xyzw vf23, 5(vi11)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi14)       |  maddy.xyzw vf28, vf28, vf24
  lq.xyzw vf20, 6(vi11)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf31, 6(vi14)      |  maddy.xyz vf29, vf29, vf24
  mtir vi11, vf12.x          |  mulax.xyzw ACC, vf23, vf24
  mtir vi14, vf12.y          |  maddy.xyz vf30, vf30, vf24
  b L45                      |  mulax.xyzw ACC, vf20, vf24
  lqi.xyzw vf23, vi03        |  maddy.xyzw vf31, vf31, vf24
L30: (L44 in og)
  ibgez vi09, L31            |  mulaz.xyzw ACC, vf29, vf09
  sq.xyzw vf21, 2(vi12)      |  maddaz.xyzw ACC, vf30, vf12
  nop                        |  ftoi4.xyzw vf21, vf10
L31: (L45 in og)
  mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  sq.xyzw vf16, 0(vi15)      |  miniw.w vf08, vf08, vf03
  sq.xyzw vf21, 2(vi15)      |  mulaw.xyzw ACC, vf25, vf09
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20
  1024.0                     |  ftoi0.xyzw vf13, vf13 :i
  erleng.xyz P, vf12         |  maxi.xy vf08, vf08, I
  ibne vi06, vi03, L16       |  maddaw.xyzw ACC, vf26, vf12
  mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  ibne vi07, vi03, L51       |  nop
  nop                        |  nop
  b L77                      |  nop
  nop                        |  nop
L32: (L46 in og)
  sq.xyzw vf11, -1(vi03)     |  nop
  3072.0                     |  mulax.xyzw ACC, vf01, vf11 :i
  lqi.xyzw vf10, vi01        |  minii.xy vf08, vf08, I
  sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
  lqi.xyzw vf13, vi01        |  add.xyzw vf09, vf09, vf28
  lqi.xyzw vf16, vi01        |  maxw.w vf08, vf08, vf02
  mtir vi12, vf10.x          |  itof0.xyzw vf23, vf23
  mtir vi15, vf10.y          |  maxx.xyzw vf11, vf11, vf00
  sq.xyzw vf09, -4(vi01)     |  miniw.w vf09, vf09, vf01
  div Q, vf01.w, vf09.w      |  add.zw vf10, vf10, vf17
  move.xyzw vf21, vf08       |  add.xyzw vf13, vf13, vf18
  iand vi12, vi12, vi05      |  add.xyzw vf16, vf16, vf19
  nop                        |  mulax.xyzw ACC, vf04, vf11
  ibgtz vi09, L33            |  madday.xyzw ACC, vf05, vf11
  iand vi15, vi15, vi05      |  maddaz.xyzw ACC, vf06, vf11
  nop                        |  addx.w vf21, vf21, vf17
L33: (L47 in og)
  ibne vi05, vi12, L34       |  maddw.xyzw vf11, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf09, vf09, Q
  mtir vi12, vf13.x          |  mul.xyzw vf15, vf15, Q
  mtir vi15, vf13.y          |  ftoi4.xyzw vf21, vf21
  b L35                      |  mul.xyzw vf11, vf11, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf09, vf09, vf22
L34: (L48 in og)
  lq.xyzw vf20, 0(vi12)      |  mul.xyzw vf15, vf15, Q
  lq.xyzw vf25, 0(vi15)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf23, 1(vi12)      |  mul.xyzw vf11, vf11, vf23
  lq.xyzw vf26, 1(vi15)      |  add.xyzw vf09, vf09, vf22
  lq.xyzw vf20, 2(vi12)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyzw vf27, 2(vi15)      |  maddw.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 3(vi12)      |  mulaz.xyzw ACC, vf23, vf24
  lq.xyzw vf28, 3(vi15)      |  maddw.xyzw vf26, vf26, vf24
  lq.xyzw vf20, 4(vi12)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi15)       |  maddw.xyzw vf27, vf27, vf24
  lq.xyzw vf23, 5(vi12)      |  mulaz.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi15)       |  maddw.xyzw vf28, vf28, vf24
  lq.xyzw vf20, 6(vi12)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyzw vf31, 6(vi15)      |  maddw.xyz vf29, vf29, vf24
  lqi.xyzw vf23, vi02        |  mulaz.xyzw ACC, vf23, vf24
  mtir vi12, vf13.x          |  maddw.xyz vf30, vf30, vf24
  mtir vi15, vf13.y          |  mulaz.xyzw ACC, vf20, vf24
  b L20                      |  maddw.xyzw vf31, vf31, vf24
  lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
L35: (L49 in og)
  ibgez vi09, L36            |  mulaz.xyzw ACC, vf29, vf10
  sq.xyzw vf21, 2(vi10)      |  maddaz.xyzw ACC, vf30, vf13
  nop                        |  ftoi4.xyzw vf21, vf08
L36: (L50 in og)
  mfp.w vf20, P              |  maddz.xyz vf13, vf31, vf16
  sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  sq.xyzw vf14, 0(vi13)      |  miniw.w vf09, vf09, vf03
  sq.xyzw vf21, 2(vi13)      |  mulaw.xyzw ACC, vf25, vf10
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf12, vf12, vf20
  1024.0                     |  ftoi0.xyzw vf11, vf11 :i
  erleng.xyz P, vf13         |  maxi.xy vf09, vf09, I
  ibne vi06, vi03, L37       |  maddaw.xyzw ACC, vf26, vf13
  mr32.z vf16, vf00          |  maddw.xyzw vf10, vf27, vf16
  ibne vi07, vi03, L57       |  nop
  nop                        |  nop
  b L67                      |  nop
  nop                        |  nop
L37: (L51 in og)
  sq.xyzw vf12, -1(vi03)     |  nop
  3072.0                     |  mulax.xyzw ACC, vf01, vf12 :i
  lqi.xyzw vf08, vi01        |  minii.xy vf09, vf09, I
  sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  lqi.xyzw vf11, vi01        |  add.xyzw vf10, vf10, vf28
  lqi.xyzw vf14, vi01        |  maxw.w vf09, vf09, vf02
  mtir vi10, vf08.x          |  itof0.xyzw vf23, vf23
  mtir vi13, vf08.y          |  maxx.xyzw vf12, vf12, vf00
  sq.xyzw vf10, -4(vi01)     |  miniw.w vf10, vf10, vf01
  div Q, vf01.w, vf10.w      |  add.zw vf08, vf08, vf17
  move.xyzw vf21, vf09       |  add.xyzw vf11, vf11, vf18
  iand vi10, vi10, vi05      |  add.xyzw vf14, vf14, vf19
  nop                        |  mulax.xyzw ACC, vf04, vf12
  ibgtz vi09, L38            |  madday.xyzw ACC, vf05, vf12
  iand vi13, vi13, vi05      |  maddaz.xyzw ACC, vf06, vf12
  nop                        |  addx.w vf21, vf21, vf17
L38: (L52 in og)
  ibne vi05, vi10, L39       |  maddw.xyzw vf12, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf10, vf10, Q
  mtir vi10, vf11.x          |  mul.xyzw vf16, vf16, Q
  mtir vi13, vf11.y          |  ftoi4.xyzw vf21, vf21
  b L40                      |  mul.xyzw vf12, vf12, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf10, vf10, vf22
L39: (L53 in og)
  lq.xyzw vf20, 0(vi10)      |  mul.xyzw vf16, vf16, Q
  lq.xyzw vf25, 0(vi13)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf23, 1(vi10)      |  mul.xyzw vf12, vf12, vf23
  lq.xyzw vf26, 1(vi13)      |  add.xyzw vf10, vf10, vf22
  lq.xyzw vf20, 2(vi10)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyzw vf27, 2(vi13)      |  maddw.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 3(vi10)      |  mulaz.xyzw ACC, vf23, vf24
  lq.xyzw vf28, 3(vi13)      |  maddw.xyzw vf26, vf26, vf24
  lq.xyzw vf20, 4(vi10)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi13)       |  maddw.xyzw vf27, vf27, vf24
  lq.xyzw vf23, 5(vi10)      |  mulaz.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi13)       |  maddw.xyzw vf28, vf28, vf24
  lq.xyzw vf20, 6(vi10)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyzw vf31, 6(vi13)      |  maddw.xyz vf29, vf29, vf24
  lqi.xyzw vf23, vi02        |  mulaz.xyzw ACC, vf23, vf24
  mtir vi10, vf11.x          |  maddw.xyz vf30, vf30, vf24
  mtir vi13, vf11.y          |  mulaz.xyzw ACC, vf20, vf24
  b L25                      |  maddw.xyzw vf31, vf31, vf24
  lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
L40: (L54 in og)
  ibgez vi09, L41            |  mulaz.xyzw ACC, vf29, vf08
  sq.xyzw vf21, 2(vi11)      |  maddaz.xyzw ACC, vf30, vf11
  nop                        |  ftoi4.xyzw vf21, vf09
L41: (L55 in og)
  mfp.w vf20, P              |  maddz.xyz vf11, vf31, vf14
  sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  sq.xyzw vf15, 0(vi14)      |  miniw.w vf10, vf10, vf03
  sq.xyzw vf21, 2(vi14)      |  mulaw.xyzw ACC, vf25, vf08
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf13, vf13, vf20
  1024.0                     |  ftoi0.xyzw vf12, vf12 :i
  erleng.xyz P, vf11         |  maxi.xy vf10, vf10, I
  ibne vi06, vi03, L42       |  maddaw.xyzw ACC, vf26, vf11
  mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  ibne vi07, vi03, L62       |  nop
  nop                        |  nop
  b L72                      |  nop
  nop                        |  nop
L42: (L56 in og)
  sq.xyzw vf13, -1(vi03)     |  nop
  3072.0                     |  mulax.xyzw ACC, vf01, vf13 :i
  lqi.xyzw vf09, vi01        |  minii.xy vf10, vf10, I
  sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  lqi.xyzw vf15, vi01        |  maxw.w vf10, vf10, vf02
  mtir vi11, vf09.x          |  itof0.xyzw vf23, vf23
  mtir vi14, vf09.y          |  maxx.xyzw vf13, vf13, vf00
  sq.xyzw vf08, -4(vi01)     |  miniw.w vf08, vf08, vf01
  div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  move.xyzw vf21, vf10       |  add.xyzw vf12, vf12, vf18
  iand vi11, vi11, vi05      |  add.xyzw vf15, vf15, vf19
  nop                        |  mulax.xyzw ACC, vf04, vf13
  ibgtz vi09, L43            |  madday.xyzw ACC, vf05, vf13
  iand vi14, vi14, vi05      |  maddaz.xyzw ACC, vf06, vf13
  nop                        |  addx.w vf21, vf21, vf17
L43: (L57 in og)
  ibne vi05, vi11, L44       |  maddw.xyzw vf13, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf08, vf08, Q
  mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  mtir vi14, vf12.y          |  ftoi4.xyzw vf21, vf21
  b L45                      |  mul.xyzw vf13, vf13, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
L44: (L58 in og)
  lq.xyzw vf20, 0(vi11)      |  mul.xyzw vf14, vf14, Q
  lq.xyzw vf25, 0(vi14)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf23, 1(vi11)      |  mul.xyzw vf13, vf13, vf23
  lq.xyzw vf26, 1(vi14)      |  add.xyzw vf08, vf08, vf22
  lq.xyzw vf20, 2(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyzw vf27, 2(vi14)      |  maddw.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 3(vi11)      |  mulaz.xyzw ACC, vf23, vf24
  lq.xyzw vf28, 3(vi14)      |  maddw.xyzw vf26, vf26, vf24
  lq.xyzw vf20, 4(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi14)       |  maddw.xyzw vf27, vf27, vf24
  lq.xyzw vf23, 5(vi11)      |  mulaz.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi14)       |  maddw.xyzw vf28, vf28, vf24
  lq.xyzw vf20, 6(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  lq.xyzw vf31, 6(vi14)      |  maddw.xyz vf29, vf29, vf24
  lqi.xyzw vf23, vi02        |  mulaz.xyzw ACC, vf23, vf24
  mtir vi11, vf12.x          |  maddw.xyz vf30, vf30, vf24
  mtir vi14, vf12.y          |  mulaz.xyzw ACC, vf20, vf24
  b L30                      |  maddw.xyzw vf31, vf31, vf24
  lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
L45: (L59 in og)
  ibgez vi09, L46            |  mulaz.xyzw ACC, vf29, vf09
  sq.xyzw vf21, 2(vi12)      |  maddaz.xyzw ACC, vf30, vf12
  nop                        |  ftoi4.xyzw vf21, vf10
L46: (L60 in og)
  mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  sq.xyzw vf16, 0(vi15)      |  miniw.w vf08, vf08, vf03
  sq.xyzw vf21, 2(vi15)      |  mulaw.xyzw ACC, vf25, vf09
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20
  1024.0                     |  ftoi0.xyzw vf13, vf13 :i
  erleng.xyz P, vf12         |  maxi.xy vf08, vf08, I
  ibne vi06, vi03, L32       |  maddaw.xyzw ACC, vf26, vf12
  mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  ibne vi07, vi03, L57       |  nop
  nop                        |  nop
  b L77                      |  nop
  nop                        |  nop

;; mat 3
L47:
  lqi.xyzw vf08, vi01        |  nop
  lqi.xyzw vf24, vi02        |  nop
  lqi.xyzw vf11, vi01        |  nop
  lqi.xyzw vf14, vi01        |  nop
  mtir vi10, vf08.x          |  nop
  mtir vi13, vf08.y          |  itof0.xyzw vf24, vf24
  nop                        |  add.zw vf08, vf08, vf17
  nop                        |  add.xyzw vf11, vf11, vf18
  iand vi10, vi10, vi05      |  add.xyzw vf14, vf14, vf19
  ilw.w vi08, -1(vi02)       |  mulw.xyzw vf24, vf24, vf29
  iand vi13, vi13, vi05      |  nop
  lq.xyzw vf20, 0(vi10)      |  nop
  lq.xyzw vf31, 0(vi13)      |  nop
  lq.xyzw vf25, 0(vi08)      |  nop
  lq.xyzw vf23, 1(vi10)      |  nop
  lq.xyzw vf20, 1(vi13)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf26, 1(vi08)      |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 2(vi10)      |  maddz.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 2(vi13)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf27, 2(vi08)      |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 3(vi10)      |  maddz.xyzw vf26, vf26, vf24
  lq.xyzw vf31, 3(vi13)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf28, 3(vi08)      |  madday.xyzw ACC, vf23, vf24
  lq.xyzw vf23, 4(vi10)      |  maddz.xyzw vf27, vf27, vf24
  lq.xyzw vf20, 4(vi13)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi08)       |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 5(vi10)      |  maddz.xyzw vf28, vf28, vf24
  lq.xyzw vf23, 5(vi13)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi08)       |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 6(vi10)      |  maddz.xyz vf29, vf29, vf24
  lq.xyzw vf22, 6(vi13)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 6(vi08)      |  madday.xyzw ACC, vf23, vf24
  lqi.xyzw vf23, vi02        |  maddz.xyz vf30, vf30, vf24
  mtir vi10, vf11.x          |  mulax.xyzw ACC, vf20, vf24
  mtir vi13, vf11.y          |  madday.xyzw ACC, vf22, vf24
  lq.xyzw vf22, 2(vi00)      |  maddz.xyzw vf31, vf31, vf24
  nop                        |  itof0.xyzw vf24, vf23
  nop                        |  mulaz.xyzw ACC, vf29, vf08
  nop                        |  maddaz.xyzw ACC, vf30, vf11
  nop                        |  maddz.xyz vf11, vf31, vf14
  nop                        |  nop
  nop                        |  nop
  nop                        |  mulaw.xyzw ACC, vf25, vf08
  nop                        |  nop
  erleng.xyz P, vf11         |  nop
  nop                        |  maddaw.xyzw ACC, vf26, vf11
  mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  lqi.xyzw vf09, vi01        |  nop
  ilwr.y vi03, vi12          |  nop
  ilw.z vi07, 1(vi12)        |  nop
  lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  lqi.xyzw vf15, vi01        |  nop
  mtir vi11, vf09.x          |  nop
  mtir vi14, vf09.y          |  nop
  sq.xyzw vf08, -4(vi01)     |  miniw.w vf08, vf08, vf01
  div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  iadd vi03, vi03, vi12      |  add.xyzw vf12, vf12, vf18
  iand vi11, vi11, vi05      |  add.xyzw vf15, vf15, vf19
  ilw.w vi08, -1(vi02)       |  nop
  iadd vi07, vi07, vi03      |  nop
  iand vi14, vi14, vi05      |  nop
  ibne vi05, vi11, L48       |  nop
  iaddi vi07, vi07, -0x1     |  mul.xyz vf08, vf08, Q
  mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  mtir vi14, vf12.y          |  nop
  b L49                      |  nop
  lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
L48:
  lq.xyzw vf20, 0(vi11)      |  mul.xyzw vf14, vf14, Q
  nop                        |  mulw.xyzw vf24, vf24, vf29
  lq.xyzw vf31, 0(vi14)      |  nop
  lq.xyzw vf25, 0(vi08)      |  nop
  lq.xyzw vf23, 1(vi11)      |  add.xyzw vf08, vf08, vf22
  lq.xyzw vf20, 1(vi14)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf26, 1(vi08)      |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 2(vi11)      |  maddz.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 2(vi14)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf27, 2(vi08)      |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 3(vi11)      |  maddz.xyzw vf26, vf26, vf24
  lq.xyzw vf31, 3(vi14)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf28, 3(vi08)      |  madday.xyzw ACC, vf23, vf24
  lq.xyzw vf23, 4(vi11)      |  maddz.xyzw vf27, vf27, vf24
  lq.xyzw vf20, 4(vi14)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi08)       |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 5(vi11)      |  maddz.xyzw vf28, vf28, vf24
  lq.xyzw vf23, 5(vi14)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi08)       |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 6(vi11)      |  maddz.xyz vf29, vf29, vf24
  lq.xyzw vf22, 6(vi14)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 6(vi08)      |  madday.xyzw ACC, vf23, vf24
  lqi.xyzw vf23, vi02        |  maddz.xyz vf30, vf30, vf24
  mtir vi11, vf12.x          |  mulax.xyzw ACC, vf20, vf24
  mtir vi14, vf12.y          |  madday.xyzw ACC, vf22, vf24
  lq.xyzw vf22, 2(vi00)      |  maddz.xyzw vf31, vf31, vf24
  lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
L49:
  nop                        |  mulaz.xyzw ACC, vf29, vf09
  nop                        |  maddaz.xyzw ACC, vf30, vf12
  mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  nop                        |  nop
  1024.0                     |  miniw.w vf08, vf08, vf03 :i
  nop                        |  mulaw.xyzw ACC, vf25, vf09
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20
  erleng.xyz P, vf12         |  maxi.xy vf08, vf08, I
  3072.0                     |  nop :i
  sq.xyzw vf11, -1(vi03)     |  minii.xy vf08, vf08, I
  nop                        |  maddaw.xyzw ACC, vf26, vf12
  mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
L50:
  lqi.xyzw vf10, vi01        |  mulax.xyzw ACC, vf01, vf11
  b L52                      |  madday.xyzw ACC, vf02, vf11
  nop                        |  maddz.xyzw vf11, vf03, vf11
L51:
  sq.xyzw vf11, -1(vi03)     |  nop
  3072.0                     |  mulax.xyzw ACC, vf01, vf11 :i
  lqi.xyzw vf10, vi01        |  minii.xy vf08, vf08, I
  sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
L52:
  lqi.xyzw vf13, vi01        |  add.xyzw vf09, vf09, vf28
  lqi.xyzw vf16, vi01        |  maxw.w vf08, vf08, vf02
  mtir vi12, vf10.x          |  itof0.xyzw vf23, vf23
  mtir vi15, vf10.y          |  maxx.xyzw vf11, vf11, vf00
  sq.xyzw vf09, -4(vi01)     |  miniw.w vf09, vf09, vf01
  div Q, vf01.w, vf09.w      |  add.zw vf10, vf10, vf17
  move.xyzw vf21, vf08       |  add.xyzw vf13, vf13, vf18
  iand vi12, vi12, vi05      |  add.xyzw vf16, vf16, vf19
  ilw.w vi08, -1(vi02)       |  mulax.xyzw ACC, vf04, vf11
  ibgtz vi09, L53            |  madday.xyzw ACC, vf05, vf11
  iand vi15, vi15, vi05      |  maddaz.xyzw ACC, vf06, vf11
  nop                        |  addx.w vf21, vf21, vf17
L53:
  ibne vi05, vi12, L54       |  maddw.xyzw vf11, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf09, vf09, Q
  mtir vi12, vf13.x          |  mul.xyzw vf15, vf15, Q
  mtir vi15, vf13.y          |  ftoi4.xyzw vf21, vf21
  b L55                      |  mul.xyzw vf11, vf11, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf09, vf09, vf22
L54:
  lq.xyzw vf20, 0(vi12)      |  mul.xyzw vf15, vf15, Q
  nop                        |  mulw.xyzw vf24, vf24, vf29
  lq.xyzw vf31, 0(vi15)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf25, 0(vi08)      |  mul.xyzw vf11, vf11, vf23
  lq.xyzw vf23, 1(vi12)      |  add.xyzw vf09, vf09, vf22
  lq.xyzw vf20, 1(vi15)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf26, 1(vi08)      |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 2(vi12)      |  maddz.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 2(vi15)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf27, 2(vi08)      |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 3(vi12)      |  maddz.xyzw vf26, vf26, vf24
  lq.xyzw vf31, 3(vi15)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf28, 3(vi08)      |  madday.xyzw ACC, vf23, vf24
  lq.xyzw vf23, 4(vi12)      |  maddz.xyzw vf27, vf27, vf24
  lq.xyzw vf20, 4(vi15)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi08)       |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 5(vi12)      |  maddz.xyzw vf28, vf28, vf24
  lq.xyzw vf23, 5(vi15)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi08)       |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 6(vi12)      |  maddz.xyz vf29, vf29, vf24
  lq.xyzw vf22, 6(vi15)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 6(vi08)      |  madday.xyzw ACC, vf23, vf24
  lqi.xyzw vf23, vi02        |  maddz.xyz vf30, vf30, vf24
  mtir vi12, vf13.x          |  mulax.xyzw ACC, vf20, vf24
  mtir vi15, vf13.y          |  madday.xyzw ACC, vf22, vf24
  lq.xyzw vf22, 2(vi00)      |  maddz.xyzw vf31, vf31, vf24
  lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
L55: (L70 in og)
  ibgez vi09, L56            |  mulaz.xyzw ACC, vf29, vf10
  sq.xyzw vf21, 2(vi10)      |  maddaz.xyzw ACC, vf30, vf13
  nop                        |  ftoi4.xyzw vf21, vf08
L56:
  mfp.w vf20, P              |  maddz.xyz vf13, vf31, vf16
  sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  sq.xyzw vf14, 0(vi13)      |  miniw.w vf09, vf09, vf03
  sq.xyzw vf21, 2(vi13)      |  mulaw.xyzw ACC, vf25, vf10
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf12, vf12, vf20
  1024.0                     |  ftoi0.xyzw vf11, vf11 :i
  erleng.xyz P, vf13         |  maxi.xy vf09, vf09, I
  ibeq vi07, vi03, L67       |  maddaw.xyzw ACC, vf26, vf13
  mr32.z vf16, vf00          |  maddw.xyzw vf10, vf27, vf16
L57:
  sq.xyzw vf12, -1(vi03)     |  nop
  3072.0                     |  mulax.xyzw ACC, vf01, vf12 :i
  lqi.xyzw vf08, vi01        |  minii.xy vf09, vf09, I
  sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  lqi.xyzw vf11, vi01        |  add.xyzw vf10, vf10, vf28
  lqi.xyzw vf14, vi01        |  maxw.w vf09, vf09, vf02
  mtir vi10, vf08.x          |  itof0.xyzw vf23, vf23
  mtir vi13, vf08.y          |  maxx.xyzw vf12, vf12, vf00
  sq.xyzw vf10, -4(vi01)     |  miniw.w vf10, vf10, vf01
  div Q, vf01.w, vf10.w      |  add.zw vf08, vf08, vf17
  move.xyzw vf21, vf09       |  add.xyzw vf11, vf11, vf18
  iand vi10, vi10, vi05      |  add.xyzw vf14, vf14, vf19
  ilw.w vi08, -1(vi02)       |  mulax.xyzw ACC, vf04, vf12
  ibgtz vi09, L58            |  madday.xyzw ACC, vf05, vf12
  iand vi13, vi13, vi05      |  maddaz.xyzw ACC, vf06, vf12
  nop                        |  addx.w vf21, vf21, vf17
L58:
  ibne vi05, vi10, L59       |  maddw.xyzw vf12, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf10, vf10, Q
  mtir vi10, vf11.x          |  mul.xyzw vf16, vf16, Q
  mtir vi13, vf11.y          |  ftoi4.xyzw vf21, vf21
  b L60                      |  mul.xyzw vf12, vf12, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf10, vf10, vf22
L59:
  lq.xyzw vf20, 0(vi10)      |  mul.xyzw vf16, vf16, Q
  nop                        |  mulw.xyzw vf24, vf24, vf29
  lq.xyzw vf31, 0(vi13)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf25, 0(vi08)      |  mul.xyzw vf12, vf12, vf23
  lq.xyzw vf23, 1(vi10)      |  add.xyzw vf10, vf10, vf22
  lq.xyzw vf20, 1(vi13)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf26, 1(vi08)      |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 2(vi10)      |  maddz.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 2(vi13)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf27, 2(vi08)      |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 3(vi10)      |  maddz.xyzw vf26, vf26, vf24
  lq.xyzw vf31, 3(vi13)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf28, 3(vi08)      |  madday.xyzw ACC, vf23, vf24
  lq.xyzw vf23, 4(vi10)      |  maddz.xyzw vf27, vf27, vf24
  lq.xyzw vf20, 4(vi13)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi08)       |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 5(vi10)      |  maddz.xyzw vf28, vf28, vf24
  lq.xyzw vf23, 5(vi13)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi08)       |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 6(vi10)      |  maddz.xyz vf29, vf29, vf24
  lq.xyzw vf22, 6(vi13)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 6(vi08)      |  madday.xyzw ACC, vf23, vf24
  lqi.xyzw vf23, vi02        |  maddz.xyz vf30, vf30, vf24
  mtir vi10, vf11.x          |  mulax.xyzw ACC, vf20, vf24
  mtir vi13, vf11.y          |  madday.xyzw ACC, vf22, vf24
  lq.xyzw vf22, 2(vi00)      |  maddz.xyzw vf31, vf31, vf24
  lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
L60:
  ibgez vi09, L61            |  mulaz.xyzw ACC, vf29, vf08
  sq.xyzw vf21, 2(vi11)      |  maddaz.xyzw ACC, vf30, vf11
  nop                        |  ftoi4.xyzw vf21, vf09
L61:
  mfp.w vf20, P              |  maddz.xyz vf11, vf31, vf14
  sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  sq.xyzw vf15, 0(vi14)      |  miniw.w vf10, vf10, vf03
  sq.xyzw vf21, 2(vi14)      |  mulaw.xyzw ACC, vf25, vf08
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf13, vf13, vf20
  1024.0                     |  ftoi0.xyzw vf12, vf12 :i
  erleng.xyz P, vf11         |  maxi.xy vf10, vf10, I
  ibeq vi07, vi03, L72       |  maddaw.xyzw ACC, vf26, vf11
  mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
L62:
  sq.xyzw vf13, -1(vi03)     |  nop
  3072.0                     |  mulax.xyzw ACC, vf01, vf13 :i
  lqi.xyzw vf09, vi01        |  minii.xy vf10, vf10, I
  sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  lqi.xyzw vf15, vi01        |  maxw.w vf10, vf10, vf02
  mtir vi11, vf09.x          |  itof0.xyzw vf23, vf23
  mtir vi14, vf09.y          |  maxx.xyzw vf13, vf13, vf00
  sq.xyzw vf08, -4(vi01)     |  miniw.w vf08, vf08, vf01
  div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  move.xyzw vf21, vf10       |  add.xyzw vf12, vf12, vf18
  iand vi11, vi11, vi05      |  add.xyzw vf15, vf15, vf19
  ilw.w vi08, -1(vi02)       |  mulax.xyzw ACC, vf04, vf13
  ibgtz vi09, L63            |  madday.xyzw ACC, vf05, vf13
  iand vi14, vi14, vi05      |  maddaz.xyzw ACC, vf06, vf13
  nop                        |  addx.w vf21, vf21, vf17
L63:
  ibne vi05, vi11, L64       |  maddw.xyzw vf13, vf07, vf00
  ilw.x vi09, -9(vi01)       |  mul.xyz vf08, vf08, Q
  mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  mtir vi14, vf12.y          |  ftoi4.xyzw vf21, vf21
  b L65                      |  mul.xyzw vf13, vf13, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
L64:
  lq.xyzw vf20, 0(vi11)      |  mul.xyzw vf14, vf14, Q
  nop                        |  mulw.xyzw vf24, vf24, vf29
  lq.xyzw vf31, 0(vi14)      |  ftoi4.xyzw vf21, vf21
  lq.xyzw vf25, 0(vi08)      |  mul.xyzw vf13, vf13, vf23
  lq.xyzw vf23, 1(vi11)      |  add.xyzw vf08, vf08, vf22
  lq.xyzw vf20, 1(vi14)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyzw vf26, 1(vi08)      |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 2(vi11)      |  maddz.xyzw vf25, vf25, vf24
  lq.xyzw vf23, 2(vi14)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyzw vf27, 2(vi08)      |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 3(vi11)      |  maddz.xyzw vf26, vf26, vf24
  lq.xyzw vf31, 3(vi14)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf28, 3(vi08)      |  madday.xyzw ACC, vf23, vf24
  lq.xyzw vf23, 4(vi11)      |  maddz.xyzw vf27, vf27, vf24
  lq.xyzw vf20, 4(vi14)      |  mulax.xyzw ACC, vf20, vf24
  lq.xyz vf29, 4(vi08)       |  madday.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 5(vi11)      |  maddz.xyzw vf28, vf28, vf24
  lq.xyzw vf23, 5(vi14)      |  mulax.xyzw ACC, vf23, vf24
  lq.xyz vf30, 5(vi08)       |  madday.xyzw ACC, vf20, vf24
  lq.xyzw vf20, 6(vi11)      |  maddz.xyz vf29, vf29, vf24
  lq.xyzw vf22, 6(vi14)      |  mulax.xyzw ACC, vf31, vf24
  lq.xyzw vf31, 6(vi08)      |  madday.xyzw ACC, vf23, vf24
  lqi.xyzw vf23, vi02        |  maddz.xyz vf30, vf30, vf24
  mtir vi11, vf12.x          |  mulax.xyzw ACC, vf20, vf24
  mtir vi14, vf12.y          |  madday.xyzw ACC, vf22, vf24
  lq.xyzw vf22, 2(vi00)      |  maddz.xyzw vf31, vf31, vf24
  lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
L65:
  ibgez vi09, L66            |  mulaz.xyzw ACC, vf29, vf09
  sq.xyzw vf21, 2(vi12)      |  maddaz.xyzw ACC, vf30, vf12
  nop                        |  ftoi4.xyzw vf21, vf10
L66: (L80 in og)
  mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  sq.xyzw vf16, 0(vi15)      |  miniw.w vf08, vf08, vf03
  sq.xyzw vf21, 2(vi15)      |  mulaw.xyzw ACC, vf25, vf09
  ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20
  1024.0                     |  ftoi0.xyzw vf13, vf13 :i
  erleng.xyz P, vf12         |  maxi.xy vf08, vf08, I
  ibne vi07, vi03, L51       |  maddaw.xyzw ACC, vf26, vf12
  mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  b L77                      |  nop
  nop                        |  nop

;;;;;;;;;;; OG merc has a bunch of merc prime alternate paths here.

;;;; next we have 3x pipeline exits.
;;

L67:
  3072.0                     |  mulax.xyzw ACC, vf01, vf12 :i
  sq.xyzw vf12, -1(vi03)     |  minii.xy vf09, vf09, I
  sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  iaddiu vi05, vi00, 0x173   |  add.xyzw vf10, vf10, vf28
  lq.xyzw vf26, 1(vi00)      |  maxw.w vf09, vf09, vf02
  iaddi vi08, vi00, 0x1      |  itof0.xyzw vf23, vf23
  isw.x vi08, -2(vi05)       |  maxx.xyzw vf12, vf12, vf00
  sq.xyzw vf10, -1(vi01)     |  miniw.w vf10, vf10, vf01
  div Q, vf01.w, vf10.w      |  nop
  move.xyzw vf21, vf09       |  nop
  iaddiu vi08, vi00, 0x42    |  nop
  isw.z vi08, -1(vi05)       |  mulax.xyzw ACC, vf04, vf12
  ibgtz vi09, L68            |  madday.xyzw ACC, vf05, vf12
  isw.x vi00, -1(vi05)       |  maddaz.xyzw ACC, vf06, vf12
  nop                        |  addx.w vf21, vf21, vf17
L68:
  sq.yzw vf26, -2(vi05)      |  maddw.xyzw vf12, vf07, vf00
  ilw.x vi09, -6(vi01)       |  mul.xyz vf10, vf10, Q
  iaddiu vi08, vi00, 0x171   |  mul.xyzw vf16, vf16, Q
  nop                        |  ftoi4.xyzw vf21, vf21
  nop                        |  mul.xyzw vf12, vf12, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf10, vf10, vf22
  ibgez vi09, L69            |  nop
  sq.xyzw vf21, 2(vi11)      |  nop
  nop                        |  ftoi4.xyzw vf21, vf09
L69:
  mfp.w vf20, P              |  nop
  sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  sq.xyzw vf15, 0(vi14)      |  miniw.w vf10, vf10, vf03
  sq.xyzw vf21, 2(vi14)      |  nop
  ilw.y vi09, -3(vi01)       |  mulw.xyzw vf13, vf13, vf20
  1024.0                     |  ftoi0.xyzw vf12, vf12 :i
  nop                        |  maxi.xy vf10, vf10, I
  nop                        |  nop
  3072.0                     |  mulax.xyzw ACC, vf01, vf13 :i
  sq.xyzw vf13, -1(vi03)     |  minii.xy vf10, vf10, I
  sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  nop                        |  nop
  nop                        |  maxw.w vf10, vf10, vf02
  nop                        |  itof0.xyzw vf23, vf23
  nop                        |  maxx.xyzw vf13, vf13, vf00
  nop                        |  nop
  move.xyzw vf21, vf10       |  nop
  nop                        |  nop
  nop                        |  mulax.xyzw ACC, vf04, vf13
  ibgtz vi09, L70            |  madday.xyzw ACC, vf05, vf13
  nop                        |  maddaz.xyzw ACC, vf06, vf13
  nop                        |  addx.w vf21, vf21, vf17
L70:
  nop                        |  maddw.xyzw vf13, vf07, vf00
  ilw.x vi09, -3(vi01)       |  nop
  xtop vi05                  |  nop
  iaddiu vi05, vi05, 0x8c    |  ftoi4.xyzw vf21, vf21
  ilwr.z vi01, vi05          |  mul.xyzw vf13, vf13, vf23
  ilwr.y vi03, vi05          |  nop
  ibgez vi09, L71            |  nop
  sq.xyzw vf21, 2(vi12)      |  nop
  nop                        |  ftoi4.xyzw vf21, vf10
L71:
  nop                        |  nop
  sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  sq.xyzw vf16, 0(vi15)      |  nop
  sq.xyzw vf21, 2(vi15)      |  nop
  nop                        |  nop
  nop                        |  ftoi0.xyzw vf13, vf13
  lq.xyzw vf23, 124(vi00)    |  nop
  iadd vi01, vi01, vi05      |  nop
  iadd vi03, vi03, vi05      |  nop
  sq.xyzw vf13, 1(vi12)      |  nop
  b L82                      |  nop
  sq.xyzw vf13, 1(vi15)      |


L72:
  3072.0                     |  mulax.xyzw ACC, vf01, vf13 :i
  sq.xyzw vf13, -1(vi03)     |  minii.xy vf10, vf10, I
  sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  iaddiu vi05, vi00, 0x173   |  add.xyzw vf08, vf08, vf28
  lq.xyzw vf26, 1(vi00)      |  maxw.w vf10, vf10, vf02
  iaddi vi08, vi00, 0x1      |  itof0.xyzw vf23, vf23
  isw.x vi08, -2(vi05)       |  maxx.xyzw vf13, vf13, vf00
  sq.xyzw vf08, -1(vi01)     |  miniw.w vf08, vf08, vf01
  div Q, vf01.w, vf08.w      |  nop
  move.xyzw vf21, vf10       |  nop
  iaddiu vi08, vi00, 0x42    |  nop
  isw.z vi08, -1(vi05)       |  mulax.xyzw ACC, vf04, vf13
  ibgtz vi09, L73            |  madday.xyzw ACC, vf05, vf13
  isw.x vi00, -1(vi05)       |  maddaz.xyzw ACC, vf06, vf13
  nop                        |  addx.w vf21, vf21, vf17
L73:
  sq.yzw vf26, -2(vi05)      |  maddw.xyzw vf13, vf07, vf00
  ilw.x vi09, -6(vi01)       |  mul.xyz vf08, vf08, Q
  iaddiu vi08, vi00, 0x171   |  mul.xyzw vf14, vf14, Q
  nop                        |  ftoi4.xyzw vf21, vf21
  nop                        |  mul.xyzw vf13, vf13, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
  ibgez vi09, L74            |  nop
  sq.xyzw vf21, 2(vi12)      |  nop
  nop                        |  ftoi4.xyzw vf21, vf10
L74:
  mfp.w vf20, P              |  nop
  sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  sq.xyzw vf16, 0(vi15)      |  miniw.w vf08, vf08, vf03
  sq.xyzw vf21, 2(vi15)      |  nop
  ilw.y vi09, -3(vi01)       |  mulw.xyzw vf11, vf11, vf20
  1024.0                     |  ftoi0.xyzw vf13, vf13 :i
  nop                        |  maxi.xy vf08, vf08, I
  nop                        |  nop
  3072.0                     |  mulax.xyzw ACC, vf01, vf11 :i
  sq.xyzw vf11, -1(vi03)     |  minii.xy vf08, vf08, I
  sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
  nop                        |  nop
  nop                        |  maxw.w vf08, vf08, vf02
  nop                        |  itof0.xyzw vf23, vf23
  nop                        |  maxx.xyzw vf11, vf11, vf00
  nop                        |  nop
  move.xyzw vf21, vf08       |  nop
  nop                        |  nop
  nop                        |  mulax.xyzw ACC, vf04, vf11
  ibgtz vi09, L75            |  madday.xyzw ACC, vf05, vf11
  nop                        |  maddaz.xyzw ACC, vf06, vf11
  nop                        |  addx.w vf21, vf21, vf17
L75:
  nop                        |  maddw.xyzw vf11, vf07, vf00
  ilw.x vi09, -3(vi01)       |  nop
  xtop vi05                  |  nop
  iaddiu vi05, vi05, 0x8c    |  ftoi4.xyzw vf21, vf21
  ilwr.z vi01, vi05          |  mul.xyzw vf11, vf11, vf23
  ilwr.y vi03, vi05          |  nop
  ibgez vi09, L76            |  nop
  sq.xyzw vf21, 2(vi10)      |  nop
  nop                        |  ftoi4.xyzw vf21, vf08
L76:
  nop                        |  nop
  sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  sq.xyzw vf14, 0(vi13)      |  nop
  sq.xyzw vf21, 2(vi13)      |  nop
  nop                        |  nop
  nop                        |  ftoi0.xyzw vf11, vf11
  lq.xyzw vf23, 124(vi00)    |  nop
  iadd vi01, vi01, vi05      |  nop
  iadd vi03, vi03, vi05      |  nop
  sq.xyzw vf11, 1(vi10)      |  nop
  b L82                      |  nop
  sq.xyzw vf11, 1(vi13)      |  nop



L77:
  3072.0                     |  mulax.xyzw ACC, vf01, vf11 :i
  sq.xyzw vf11, -1(vi03)     |  minii.xy vf08, vf08, I
  sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
  iaddiu vi05, vi00, 0x173   |  add.xyzw vf09, vf09, vf28
  lq.xyzw vf26, 1(vi00)      |  maxw.w vf08, vf08, vf02
  iaddi vi08, vi00, 0x1      |  itof0.xyzw vf23, vf23
  isw.x vi08, -2(vi05)       |  maxx.xyzw vf11, vf11, vf00
  sq.xyzw vf09, -1(vi01)     |  miniw.w vf09, vf09, vf01
  div Q, vf01.w, vf09.w      |  nop
  move.xyzw vf21, vf08       |  nop
  iaddiu vi08, vi00, 0x42    |  nop
  isw.z vi08, -1(vi05)       |  mulax.xyzw ACC, vf04, vf11
  ibgtz vi09, L78            |  madday.xyzw ACC, vf05, vf11
  isw.x vi00, -1(vi05)       |  maddaz.xyzw ACC, vf06, vf11
  nop                        |  addx.w vf21, vf21, vf17
L78:
  sq.yzw vf26, -2(vi05)      |  maddw.xyzw vf11, vf07, vf00
  ilw.x vi09, -6(vi01)       |  mul.xyz vf09, vf09, Q
  iaddiu vi08, vi00, 0x171   |  mul.xyzw vf15, vf15, Q ;; vi08 = 0x171: output location (fixed?)
  nop                        |  ftoi4.xyzw vf21, vf21
  nop                        |  mul.xyzw vf11, vf11, vf23
  lqi.xyzw vf23, vi03        |  add.xyzw vf09, vf09, vf22
  ibgez vi09, L79            |  nop
  sq.xyzw vf21, 2(vi10)      |  nop
  nop                        |  ftoi4.xyzw vf21, vf08
L79:
  mfp.w vf20, P              |  nop
  sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  sq.xyzw vf14, 0(vi13)      |  miniw.w vf09, vf09, vf03
  sq.xyzw vf21, 2(vi13)      |  nop
  ilw.y vi09, -3(vi01)       |  mulw.xyzw vf12, vf12, vf20
  1024.0                     |  ftoi0.xyzw vf11, vf11 :i
  nop                        |  maxi.xy vf09, vf09, I
  nop                        |  nop
  3072.0                     |  mulax.xyzw ACC, vf01, vf12 :i
  sq.xyzw vf12, -1(vi03)     |  minii.xy vf09, vf09, I
  sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  nop                        |  nop
  nop                        |  maxw.w vf09, vf09, vf02
  nop                        |  itof0.xyzw vf23, vf23
  nop                        |  maxx.xyzw vf12, vf12, vf00
  nop                        |  nop
  move.xyzw vf21, vf09       |  nop
  nop                        |  nop
  nop                        |  mulax.xyzw ACC, vf04, vf12
  ibgtz vi09, L80            |  madday.xyzw ACC, vf05, vf12
  nop                        |  maddaz.xyzw ACC, vf06, vf12
  nop                        |  addx.w vf21, vf21, vf17
L80:
  nop                        |  maddw.xyzw vf12, vf07, vf00
  ilw.x vi09, -3(vi01)       |  nop
  xtop vi05                  |  nop
  iaddiu vi05, vi05, 0x8c    |  ftoi4.xyzw vf21, vf21          ;; vi05 = byte-header
  ilwr.z vi01, vi05          |  mul.xyzw vf12, vf12, vf23      ;; vi01 = lump
  ilwr.y vi03, vi05          |  nop                            ;; vi03 = rgba
  ibgez vi09, L81            |  nop
  sq.xyzw vf21, 2(vi11)      |  nop
  nop                        |  ftoi4.xyzw vf21, vf09
L81:
  nop                        |  nop
  sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  sq.xyzw vf15, 0(vi14)      |  nop
  sq.xyzw vf21, 2(vi14)      |  nop
  nop                        |  nop
  nop                        |  ftoi0.xyzw vf12, vf12
  lq.xyzw vf23, 124(vi00)    |  nop                          ;; unperspect
  iadd vi01, vi01, vi05      |  nop                          ;; lump
  iadd vi03, vi03, vi05      |  nop                          ;; rgba
  sq.xyzw vf12, 1(vi11)      |  nop
  sq.xyzw vf12, 1(vi14)      |  nop

;; COMMON finish part

L82:
  xgkick vi08                |  nop                         ;; normal draw?

;; pipeline startup for envmap math
  lq.xyzw vf08, 2(vi01)      |  nop                         ;; vf08 = transformed vert
  lqi.xyzw vf10, vi03        |  nop                         ;; vf10 = transformed normal
  ilw.x vi04, 1(vi05)        |  nop                         ;; vi04 = mat1-cnt
  ilw.y vi06, 1(vi05)        |  nop                         ;; vi06 = mat2-cnt
  ilw.z vi07, 1(vi05)        |  mul.xyzw vf09, vf08, vf23   ;; vi07 = mat3-cnt, unperspect the vert
  iadd vi04, vi04, vi06      |  subw.z vf10, vf10, vf00     ;; vi04 = mat1-cnt + mat2-cnt, refl1
  iaddi vi01, vi01, 0x3      |  nop                         ;; step lump
  iadd vi04, vi04, vi07      |  nop                         ;; vi04 = mat1 + mat2 + mat3 counts
  iadd vi02, vi03, vi04      |  addw.z vf09, vf00, vf09     ;; vi02 = end rgba, vert1
  iaddi vi02, vi02, 0x2      |  nop                         ;; end rgba more
  lq.xyzw vf14, 118(vi00)    |  maxw.xyzw vf21, vf00, vf00  ;; vf14 = rgba-fade, vf21 = [1, 1, 1, 1]
  lq.xyzw vf26, 371(vi00)    |  nop                         ;; vf26 = the giftag
  nop                        |  mul.xyz vf15, vf09, vf10    ;; multiply
  lq.xyzw vf27, 119(vi00)    |  nop                         ;; vf27 = e-adgif0
  nop                        |  nop
  lq.xyzw vf28, 120(vi00)    |  nop                         ;; vf28 = e-adgif1
  nop                        |  adday.xyzw vf15, vf15
  lq.xyzw vf31, 121(vi00)    |  maddz.x vf15, vf21, vf15    ;; vf31 = e-adgif2
  nop                        |  nop
  sq.xyzw vf26, 813(vi00)    |  nop                         ;; store giftag
  lq.xyzw vf08, 2(vi01)      |  nop ;; pipe
  lqi.xyzw vf11, vi03        |  nop ;; pipe
  div Q, vf15.x, vf10.z      |  nop                         ;; div
  sq.xyzw vf27, 814(vi00)    |  mulaw.xyzw ACC, vf09, vf00  ;; store e-ad0, mul
  nop                        |  mul.xyzw vf09, vf08, vf23   ;; pipe
  sq.xyzw vf28, 815(vi00)    |  subw.z vf11, vf11, vf00     ;; store e-ad1, pipe
  iaddi vi01, vi01, 0x3      |  nop ;; pipe
  sq.xyzw vf31, 816(vi00)    |  nop                         ;; store e-ad2
  nop                        |  addw.z vf09, vf00, vf09     ;; pipe
  lq.xyzw vf26, 0(vi00)      |  madd.xyzw vf10, vf10, Q     ;; vf26 = tristrip giftag, madd
  nop                        |  nop
  lq.xyzw vf27, 122(vi00)    |  nop                         ;; vf27 = e-ad3
  nop                        |  mul.xyz vf15, vf09, vf11    ;; pipe
  eleng.xyz P, vf10          |  nop ;; len
  lq.xyzw vf28, 123(vi00)    |  nop  ;; vf28 = e-ad4
  nop                        |  nop
  lq.xyzw vf31, 377(vi00)    |  adday.xyzw vf15, vf15 ;; vf31 = old tristrip???
  nop                        |  maddz.x vf15, vf21, vf15 ;; pipe
  mr32.xyzw vf26, vf26       |  nop ;; rotate tristrip template
  nop                        |  nop
  lq.xyzw vf08, 2(vi01)      |  nop ;; pipe
  lqi.xyzw vf12, vi03        |  nop ;; pipe
  div Q, vf15.x, vf11.z      |  nop ;; pipe
  mr32.xyzw vf26, vf26       |  mulaw.xyzw ACC, vf09, vf00 ;; rotate | pipe
  sq.xyzw vf27, 817(vi00)    |  mul.xyzw vf09, vf08, vf23  ;; store adgif3 | pipe
  lq.xyzw vf25, -5(vi01)     |  subw.z vf12, vf12, vf00    ;; vf25 = lump[1] | pipe
  iaddi vi01, vi01, 0x3      |  nop                        ;; pipe
  sq.xyzw vf28, 818(vi00)    |  nop                        ;; e-ad4 store
  nop                        |  addw.z vf09, vf00, vf09    ;; pipe
  sq.xyzw vf31, 819(vi00)    |  madd.xyzw vf11, vf11, Q    ;; tristrip store | pipe
  nop                        |  nop
  mfp.w vf10, P              |  nop
  sq.y vf26, 819(vi00)       |  mul.xyz vf15, vf09, vf12   ;; set abe | pipe
  eleng.xyz P, vf11          |  nop
  nop                        |  nop
  div Q, vf23.z, vf10.w      |  nop ;; NOT PIPE (!)
  nop                        |  adday.xyzw vf15, vf15 ;; pipe
  nop                        |  maddz.x vf15, vf21, vf15 ;; pipe
  nop                        |  nop
  nop                        |  add.xyzw vf25, vf25, vf18 ;; lump dest stuff
L83:
  lq.xyzw vf08, 2(vi01)      |  nop                        ;; pipe
  lqi.xyzw vf13, vi03        |  addaz.xyzw vf00, vf23
  div Q, vf15.x, vf12.z      |  madd.xyzw vf10, vf10, Q
  mtir vi10, vf25.x          |  mulaw.xyzw ACC, vf09, vf00
  mtir vi13, vf25.y          |  mul.xyzw vf09, vf08, vf23
  lq.xyzw vf25, -5(vi01)     |  subw.z vf13, vf13, vf00
  ;;
  iaddi vi01, vi01, 0x3      |  nop
  lq.xyzw vf24, 0(vi10)      |  nop
  lq.xyzw vf16, 2(vi10)      |  addw.z vf09, vf00, vf09
  lq.xyzw vf20, 2(vi13)      |  madd.xyzw vf12, vf12, Q
  sq.xyzw vf14, 443(vi10)    |  nop
  mfp.w vf11, P              |  nop
  sq.xyzw vf14, 443(vi13)    |  mul.xyz vf15, vf09, vf13
  eleng.xyz P, vf12          |  mulz.xy vf24, vf10, vf24
  sq.xyzw vf16, 444(vi10)    |  nop
  div Q, vf23.z, vf11.w      |  nop
  sq.xyzw vf20, 444(vi13)    |  adday.xyzw vf15, vf15
  sq.xyzw vf24, 442(vi10)    |  maddz.x vf15, vf21, vf15
  ibeq vi02, vi03, L84       |  nop
  sq.xyzw vf24, 442(vi13)    |  add.xyzw vf25, vf25, vf18
  lq.xyzw vf08, 2(vi01)      |  nop
  lqi.xyzw vf10, vi03        |  addaz.xyzw vf00, vf23
  div Q, vf15.x, vf13.z      |  madd.xyzw vf11, vf11, Q
  mtir vi10, vf25.x          |  mulaw.xyzw ACC, vf09, vf00
  mtir vi13, vf25.y          |  mul.xyzw vf09, vf08, vf23
  lq.xyzw vf25, -5(vi01)     |  subw.z vf10, vf10, vf00
  iaddi vi01, vi01, 0x3      |  nop
  lq.xyzw vf24, 0(vi10)      |  nop
  lq.xyzw vf16, 2(vi10)      |  addw.z vf09, vf00, vf09
  lq.xyzw vf20, 2(vi13)      |  madd.xyzw vf13, vf13, Q
  sq.xyzw vf14, 443(vi10)    |  nop
  mfp.w vf12, P              |  nop
  sq.xyzw vf14, 443(vi13)    |  mul.xyz vf15, vf09, vf10
  eleng.xyz P, vf13          |  mulz.xy vf24, vf11, vf24
  sq.xyzw vf16, 444(vi10)    |  nop
  div Q, vf23.z, vf12.w      |  nop
  sq.xyzw vf20, 444(vi13)    |  adday.xyzw vf15, vf15
  sq.xyzw vf24, 442(vi10)    |  maddz.x vf15, vf21, vf15
  ibeq vi02, vi03, L84       |  nop
  sq.xyzw vf24, 442(vi13)    |  add.xyzw vf25, vf25, vf18
  lq.xyzw vf08, 2(vi01)      |  nop
  lqi.xyzw vf11, vi03        |  addaz.xyzw vf00, vf23
  div Q, vf15.x, vf10.z      |  madd.xyzw vf12, vf12, Q
  mtir vi10, vf25.x          |  mulaw.xyzw ACC, vf09, vf00
  mtir vi13, vf25.y          |  mul.xyzw vf09, vf08, vf23
  lq.xyzw vf25, -5(vi01)     |  subw.z vf11, vf11, vf00
  iaddi vi01, vi01, 0x3      |  nop
  lq.xyzw vf24, 0(vi10)      |  nop
  lq.xyzw vf16, 2(vi10)      |  addw.z vf09, vf00, vf09
  lq.xyzw vf20, 2(vi13)      |  madd.xyzw vf10, vf10, Q
  sq.xyzw vf14, 443(vi10)    |  nop
  mfp.w vf13, P              |  nop
  sq.xyzw vf14, 443(vi13)    |  mul.xyz vf15, vf09, vf11
  eleng.xyz P, vf10          |  mulz.xy vf24, vf12, vf24
  sq.xyzw vf16, 444(vi10)    |  nop
  div Q, vf23.z, vf13.w      |  nop
  sq.xyzw vf20, 444(vi13)    |  adday.xyzw vf15, vf15
  sq.xyzw vf24, 442(vi10)    |  maddz.x vf15, vf21, vf15
  ibeq vi02, vi03, L84       |  nop
  sq.xyzw vf24, 442(vi13)    |  add.xyzw vf25, vf25, vf18
  lq.xyzw vf08, 2(vi01)      |  nop
  lqi.xyzw vf12, vi03        |  addaz.xyzw vf00, vf23
  div Q, vf15.x, vf11.z      |  madd.xyzw vf13, vf13, Q
  mtir vi10, vf25.x          |  mulaw.xyzw ACC, vf09, vf00
  mtir vi13, vf25.y          |  mul.xyzw vf09, vf08, vf23
  lq.xyzw vf25, -5(vi01)     |  subw.z vf12, vf12, vf00
  iaddi vi01, vi01, 0x3      |  nop
  lq.xyzw vf24, 0(vi10)      |  nop
  lq.xyzw vf16, 2(vi10)      |  addw.z vf09, vf00, vf09
  lq.xyzw vf20, 2(vi13)      |  madd.xyzw vf11, vf11, Q
  sq.xyzw vf14, 443(vi10)    |  nop
  mfp.w vf10, P              |  nop
  sq.xyzw vf14, 443(vi13)    |  mul.xyz vf15, vf09, vf12
  eleng.xyz P, vf11          |  mulz.xy vf24, vf13, vf24
  sq.xyzw vf16, 444(vi10)    |  nop
  div Q, vf23.z, vf10.w      |  nop
  sq.xyzw vf20, 444(vi13)    |  adday.xyzw vf15, vf15
  sq.xyzw vf24, 442(vi10)    |  maddz.x vf15, vf21, vf15
  ibne vi02, vi03, L83       |  nop
  sq.xyzw vf24, 442(vi13)    |  add.xyzw vf25, vf25, vf18
L84:
  iaddiu vi08, vi00, 0x32d   |  nop
  xgkick vi08                |  nop
  nop                        |  nop :e
  nop                        |  nop

```
