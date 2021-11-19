#include "extract_tfrag.h"
#include "common/dma/dma.h"
#include "common/util/assert.h"
#include "decompiler/util/Error.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"

namespace decompiler {
/*!
 * Get the index of the first draw node in an array. Works for node or tfrag.
 */
u16 get_first_idx(const level_tools::DrawableInlineArray* array) {
  auto as_tfrags = dynamic_cast<const level_tools::DrawableInlineArrayTFrag*>(array);
  auto as_nodes = dynamic_cast<const level_tools::DrawableInlineArrayNode*>(array);
  if (as_tfrags) {
    return as_tfrags->tfragments.at(0).id;
  } else if (as_nodes) {
    return as_nodes->draw_nodes.at(0).id;
  } else {
    assert(false);
  }
}

/*!
 * Verify node indices follow the patterns we expect. Takes start as the expected first,
 * writes the end.
 */
bool verify_node_indices_from_array(const level_tools::DrawableInlineArray* array,
                                    u16 start,
                                    u16* end) {
  auto as_tfrags = dynamic_cast<const level_tools::DrawableInlineArrayTFrag*>(array);
  auto as_nodes = dynamic_cast<const level_tools::DrawableInlineArrayNode*>(array);

  if (as_tfrags) {
    for (auto& elt : as_tfrags->tfragments) {
      if (elt.id != start) {
        fmt::print("bad frag: exp {} got {}\n", start, elt.id);
        return false;
      }
      start++;
    }
    *end = start;
    return true;
  } else if (as_nodes) {
    for (auto& elt : as_nodes->draw_nodes) {
      if (elt.id != start) {
        fmt::print("bad node: exp {} got {}\n", start, elt.id);
        return false;
      }
      start++;
    }
    *end = start;
    return true;
  } else {
    fmt::print("bad node array type: {}\n", array->my_type());
    return false;
  }
}

/*!
 * Verify all node indices in a tree.
 */
bool verify_node_indices(const level_tools::DrawableTreeTfrag* tree) {
  u16 start = get_first_idx(tree->arrays.at(0).get());
  for (auto& array : tree->arrays) {
    if (!verify_node_indices_from_array(array.get(), start, &start)) {
      return false;
    }
    start = (start + 31) & ~(31);
  }
  return true;
}

/*!
 * Extract the visibility tree.
 * This does not insert nodes for the bottom level.
 */
VisNodeTree extract_vis_data(const level_tools::DrawableTreeTfrag* tree, u16 first_child) {
  VisNodeTree result;
  result.first_child_node = first_child;
  result.last_child_node = first_child;
  result.vis_nodes.resize(first_child);

  // may run 0 times, if there are only children.
  for (int i = 0; i < ((int)tree->arrays.size()) - 1; i++) {
    bool expecting_leaves = i == ((int)tree->arrays.size()) - 2;

    auto array =
        dynamic_cast<const level_tools::DrawableInlineArrayNode*>(tree->arrays.at(i).get());
    assert(array);
    u16 idx = first_child;
    for (auto& elt : array->draw_nodes) {
      auto& vis = result.vis_nodes.at(elt.id);
      assert(vis.num_kids == 0xff);
      for (int j = 0; j < 4; j++) {
        vis.bsphere[j] = elt.bsphere.data[j];
      }
      vis.num_kids = elt.child_count;
      vis.flags = elt.flags;
      assert(vis.flags == expecting_leaves ? 0 : 1);
      assert(vis.num_kids > 0);
      assert(vis.num_kids <= 8);
      assert(elt.children.size() == vis.num_kids);
      if (expecting_leaves) {
        for (int leaf = 0; leaf < (int)vis.num_kids; leaf++) {
          auto l = dynamic_cast<level_tools::TFragment*>(elt.children.at(leaf).get());
          assert(l);

          assert(idx == l->id);

          assert(l->id >= result.first_child_node);
          result.last_child_node = std::max((u16)l->id, result.last_child_node);
          idx++;
        }

      } else {
        u16 arr_idx = 0;
        for (int child = 0; child < (int)vis.num_kids; child++) {
          auto l = dynamic_cast<level_tools::DrawNode*>(elt.children.at(child).get());
          assert(l);
          if (child == 0) {
            arr_idx = l->id;
          } else {
            assert(arr_idx < l->id);
            arr_idx = l->id;
          }
          assert(l->id < result.first_child_node);
        }
      }
    }
  }

  return result;
}

// The tfrag drawing process consists of:
// - loading occlusion culling data (skipping, at least for now in the PC port)
// - running draw-inline-array-tfrag and draw-inline-array-tfrag-near to generate DMA lists
// - VU1 VIF unpacks to load data
// - VU1 MSCALs to render. There are several programs. We have ported program 6 which can render
//   tfrags at a fixed LOD, including the highest LOD.

// our strategy is to figure out how the game would generate the highest LOD upload for all tfrags
// normally there's culling/not drawing near ones with the far renderer/etc decided in the
// draw-inline-array-tfrag functions.  But we don't want any of that.

/*
(deftype tfragment (drawable)
  (
   (color-index       uint16                       :offset 6)
   (debug-data        tfragment-debug-data         :offset 8)
   (color-indices     uint32                       :offset 12)
   (colors            uint32                       :offset 12)
   (dma-chain         uint32              3       :offset-assert 32)
   (dma-common        uint32                       :offset 32)
   (dma-level-0       uint32                       :offset 32)
   (dma-base          uint32                       :offset 36)
   (dma-level-1       uint32                       :offset 40)
   (dma-qwc           uint8              4       :offset 44)
   (shader            (inline-array adgif-shader)                       :offset 48)
   (num-shaders       uint8                        :offset 52)
   (num-base-colors   uint8                        :offset 53)
   (num-level0-colors uint8                        :offset 54)
   (num-level1-colors uint8                        :offset 55)
   (color-offset      uint8                        :offset 56)
   (color-count       uint8                        :offset 57)
   (pad0              uint8                        :offset 58)
   (pad1              uint8                        :offset 59)
   (generic           generic-tfragment            :offset-assert 60)
   (generic-u32       uint32            :offset 60) ;; added
   )
  :method-count-assert 18
  :size-assert         #x40
  :flag-assert         #x1200000040
  )


(deftype tfrag-work (structure)
  ((base-tmpl             dma-packet :inline :offset-assert 0)
   (level-0-tmpl          dma-packet :inline :offset-assert 16)
   (common-tmpl           dma-packet :inline :offset-assert 32)
   (level-1-tmpl          dma-packet :inline :offset-assert 48)
   (color-tmpl            dma-packet :inline :offset-assert 64)
   (frag-dists            vector     :inline :offset-assert 80)
   (max-dist              vector     :inline :offset-assert 96)
   (min-dist              vector     :inline :offset-assert 112)
   (color-ptr             vector4w   :inline :offset-assert 128)
   (tr-stat-tfrag         tr-stat            :offset-assert 144)
   (tr-stat-tfrag-near    tr-stat            :offset-assert 148)
   (vu1-enable-tfrag      int32              :offset-assert 152)
   (vu1-enable-tfrag-near int32              :offset-assert 156)
   (cur-vis-bits          uint32             :offset-assert 160)
   (end-vis-bits          uint32             :offset-assert 164)
   (src-ptr               uint32             :offset-assert 168)
   (last-call             uint32             :offset-assert 172)
   (dma-buffer            basic              :offset-assert 176)
   (test-id               uint32             :offset-assert 180)
   (wait-from-spr         uint32             :offset-assert 184)
   (wait-to-spr           uint32             :offset-assert 188)
   (near-wait-from-spr    uint32             :offset-assert 192)
   (near-wait-to-spr      uint32             :offset-assert 196)
   )
  :method-count-assert 9
  :size-assert         #xc8
  :flag-assert         #x9000000c8
  )
 */

// base vifs:
// ??
// t3

// l0:
// ??
// t3

// common:
// ??
// t3

// color
// ??
// 12 sb color-offset
// 14 sb num colors, 4 aligned.

/*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; .function draw-inline-array-tfrag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; there's two double-buffered spad buffers + culling data

   ;; arguments:
   ;; a0 - occlusion cull list (on spad)
   ;; a1 - tfrags
   ;; a2 - num tfrags
   ;; a3 - dma buf

   ;; constants:
   ;; t0 = *tfrag-work*
   ;; t1 = SPR FROM
   ;; t2 = 0x14000000 ??
   ;; t4 = SPR TO

   ;; vars:
   ;; v1 = ptr to dma buffer data
   ;; t5 = SPR BUFFER 0 (tfrags)
   ;; a3 = SPR BUFFER 1
   ;; t3 = ?? (init to 0)
   ;; t6 = spr buffer 1 use (qwc)

   ;; vf3 = frag-dists
   ;; vf1 = (1, 1, 1, 1)
   ;; vf2 = bsphere
B0:
L40:
TFRAG INITIALIZE
 ;; set up constants
    daddiu sp, sp, -128
    sd ra, 0(sp)
    sq s0, 16(sp)
    sq s1, 32(sp)
    sq s2, 48(sp)
    sq s3, 64(sp)
    sq s4, 80(sp)
    sq s5, 96(sp)
    sq gp, 112(sp)
    lui t2, 5120     = (0x14000000), a constant (mscal)
    lw v1, 4(a3)     (-> dma-buf base)
    lui t3, 4096     = (0x10000000)
    lui t1, 4096     = (0x10000000)
    sync.l
    cache dxwbin v1, 0
    sync.l
    cache dxwbin v1, 1
    sync.l
    lw t0, *tfrag-work*(s7)
    ori t4, t3, 54272           = (0x1000D400) SPR TO
    ori t1, t1, 53248           = (0x1000D000) SPR FROM
    lui t5, 28672               = (0x70000000)
    lqc2 vf3, 80(t0)            = (-> *tfrag-work* frag-dists)
    sw a3, 176(t0)              (set! (-> *tfrag-work* dma-buffer) dma-ptr)
    ori a3, t5, 2064            setup buffer 1
    addiu t3, r0, 0             t3 = 0
    ori t5, t5, 1040            setup buffer 0
    vmaxw.xyzw vf1, vf0, vf0    vf1 = (1, 1, 1, 1)
    lh t7, 0(a0)                vis cull load
    lqc2 vf4, 96(t0)            max-dist
    addiu a1, a1, -4            remove basic offset
    addiu t6, r0, 0             t6 = 0
    or ra, a3, r0               ra = SPAD BUFFER 1


SKIP TO FIRST VISIBLE
 ;; skips ahead until we find some visible tfrags.
B1:
L41:
    bne t7, r0, L42
    sll r0, r0, 0

B2:
    addiu a0, a0, 2     ;; + 16 bits in the vis list
    addiu a1, a1, 1024  ;; 16 * 0x40 = 1024 bytes in tfrag list
    daddiu a2, a2, -16  ;; num tfrags -= 16
    lh t7, 0(a0)        ;; next vis
    blez a2, L69_CLEANUP ;; no visible tfrags, abort!
    sll r0, r0, 0

B3:
    beq r0, r0, L41 ;; keep looking
    sll r0, r0, 0

WAIT_FOR_PREV_DMA
 ;; waits for any previously running spad dma transfer to end
B4:
L42:
    lw t7, 0(t4)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t7, t7, 256
    sll r0, r0, 0
    bne t7, r0, L42
    sll r0, r0, 0

INIT_FIRST_SPAD_TO
 ;; initializes the first scratchpad upload of tfrags
B5:
    sw a1, 16(t4)       ;; madr = a1
    xori t7, t5, 1024   ;; t7 = upload addr of tfrags double buffer
    sw t7, 128(t4)      ;; sadr
    addiu t7, r0, 64    ;; 64 qw = 16 tfrags
    sw t7, 32(t4)       ;; qwc
    addiu t7, r0, 256   ;; go
    sw t7, 0(t4)        ;; go!
    sll r0, r0, 0

TFRAG MAIN LOOP TOP
B6:
L43:
    or gp, a0, r0                     ;; gp = temp addr of vis list
    xori t5, t5, 1024                 ;; toggle to addr of upload tfrags
    daddiu a0, a0, 2                  ;; advance vis list ptr (16 tfrags)
    or t9, a0, r0                     ;; t9 = temp addr of next vis list
    or t8, t5, r0                     ;; t8 = tfrags to use

  ;; next, let's find next block of visible tfrags so we can start it's dma early
    daddiu t7, a2, -16                ;; t7 = tfrags left after this loop
    bgtz t7, L45                      ;; if we have them left, jump
    lh t7, 0(a0)                      ;; and load their vis

B7:
    beq r0, r0, L48                   ;; none left, skip dma kickoff.
    sll r0, r0, 0

B8:
L44:
    daddiu a2, a2, -16                ;; skip invisible block (dec tfrag counter)
    addiu a0, a0, 2                   ;; increment vis list
    blez a2, L48                      ;; did we get to the end of the tfrag list?
    lh t7, 0(a0)                      ;; check vis again.

B9:
    sll r0, r0, 0
    sll r0, r0, 0
B10:
L45:
    beq t7, r0, L44                  ;; we have tfrags left. if 0, they are all hidden, so loop
    addiu a1, a1, 1024               ;; and advance upload pointer (not done at all yet)

 ;; we reach here if we have tfrags left after this block.
 ;; so let's upload the next ones to the scratchpad so they are ready by next time.
B11:
L46:
    lw t7, 0(t4)      ;; make sure to-spr is done
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t7, t7, 256
    sll r0, r0, 0
    beq t7, r0, L47
    sll r0, r0, 0

B12:
    sll r0, r0, 0
    lw t7, 188(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu t7, t7, 1 ;; counting how many times we wait
    sll r0, r0, 0
    sw t7, 188(t0)
    beq r0, r0, L46
    sll r0, r0, 0

B13:
L47:
    sw a1, 16(t4)     ;; start the to!
    xori t7, t5, 1024
    sw t7, 128(t4)
    addiu t7, r0, 64
    sw t7, 32(t4)
    addiu t7, r0, 256
    beq r0, r0, L49 ;; skip ahead
    sw t7, 0(t4)

 ;; only reach here if we dont have any more spr to's
 ;; still need to sync the to for the block we're about to process
B14:
L48:
    lw t7, 0(t4)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi t7, t7, 256
    sll r0, r0, 0
    beq t7, r0, L49
    sll r0, r0, 0

B15:
    sll r0, r0, 0
    lw t7, 188(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu t7, t7, 1
    sll r0, r0, 0
    sw t7, 188(t0)
    beq r0, r0, L48
    sll r0, r0, 0

 ;; common op start
 ;; at this point:
 ;;  t8 is our spad tfrag buffer, with 16 tfrags. at least 1 is visible.
 ;;  gp is our vis-list pointer
 ;; we run through this loop 2x, each time doing 8 tfrags.

B16:
L49:
    lb t7, 0(gp)      ;; load first 8 frag vis bits
    addiu gp, gp, 1   ;; inc vis bit ptr.
    sll r0, r0, 0
    sw gp, 160(t0)    ;; store cur-vis-bits
    bne t7, r0, L50   ;; are any visible in the first 8?
    sw t9, 164(t0)    ;; set end-vis-bits (why?)

B17: ;; none are visible
    daddiu a2, a2, -8  ;; dec tfrags
    addiu t8, t8, 512  ;; skip tfrags
    beq r0, r0, L65    ;; skip ahead!
    sll r0, r0, 0

B18:
L50:
    addiu t9, r0, 128    ;; vis mask init (gets shifted in each run of the 8-loop)
    lqc2 vf2, 16(t8)     ;; bsphere load
B19:
L51:
    daddiu gp, t6, -124  ;; are we full of stuff in buffer 1?
    sll r0, r0, 0
    blez gp, L54
    sll r0, r0, 0

B20:
L52:
    lw ra, 0(t1)       ;; wait for spr-from
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi ra, ra, 256
    sll r0, r0, 0
    beq ra, r0, L53
    sll r0, r0, 0

B21:
    sll r0, r0, 0     ;; count it
    lw ra, 184(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu ra, ra, 1
    sll r0, r0, 0
    sw ra, 184(t0)
    beq r0, r0, L52
    sll r0, r0, 0

B22:
L53:
    sw a3, 128(t1)       ;; kick off the next spr-from.
    xori a3, a3, 6144
    sw v1, 16(t1)        ;; to the dma-buf
    sll ra, t6, 4
    addu v1, v1, ra      ;; add qwc
    or ra, a3, r0        ;; ra is the spad-side dma buffer to write to
    sw t6, 32(t1)        ;; qwc
    addiu t6, r0, 256
    sw t6, 0(t1)         ;; go!
    addiu t6, r0, 0      ;; reset use.

 ;; actually building dma.
B23:
L54:
    and gp, t7, t9               ;; vis check
    vmulax.xyzw acc, vf16, vf2   ;; plane ?
    beq gp, r0, L64_8loop_reject ;; vis check failed, reject!
    lwu gp, 36(t8)                      ;; DMA BASE (chain) -------------

B24:
    vmadday.xyzw acc, vf17, vf2 ;; plane
    lbu s5, 45(t8)                     ;; DMA QWC1
    vmaddaz.xyzw acc, vf18, vf2 ;; plane
    sw gp, 4(t0)                       ;; base tmpl set addr
    vmsubaw.xyzw acc, vf19, vf0 ;; plane
    sh s5, 0(t0)                       ;; base tmpl set qwc
    vmaddw.xyzw vf5, vf1, vf2   ;; plane
    lwu gp, 32(t8)                     ;; DMA level0 -------------
    vmulaw.xyzw acc, vf27, vf0  ;; camrot
    lbu s5, 47(t8)                     ;; DMA QWC3
    vmaddax.xyzw acc, vf24, vf2 ;; camrot
    sw gp, 20(t0)                      ;; l0 tmpl set addr
    vmadday.xyzw acc, vf25, vf2 ;; camrot
    sh s5, 16(t0)                      ;; l0 tmpl set qwc
    vmaddaz.xyzw acc, vf26, vf2 ;; camrot
    lwu gp, 32(t8)                     ;; DMA common --------------
    qmfc2.i s5, vf5             ;; plane
    lbu s4, 44(t8)                     ;; DMA QWC0
    vmaddw.xyzw vf6, vf1, vf2   ;; ??
    sw gp, 36(t0)                      ;; common tmpl set addr
    vmsubw.xyzw vf8, vf1, vf2   ;; ??
    sh s4, 32(t0)                      ;; common tmpl set qwc
    pcgtw s5, r0, s5            ;; plane check
    lwu gp, 40(t8)                     ;; DMA level1 -------------
    ppach s5, r0, s5            ;; plane check
    lbu s4, 46(t8)                     ;; DMA QWC2
    vaddz.xyzw vf6, vf3, vf6    ;; dist
    sw gp, 52(t0)                      ;; l1 tmpl addr
    vaddz.xyzw vf7, vf3, vf8    ;; dist
    sw t3, 12(t0)                      ;; !!! set a vif on base, 0 on the first round, at least.
    bne s5, r0, L63_8loop_reject_tog_vis
    sh s4, 48(t0)                      ;; l1 tmpl qwc

B25:
    vmini.xyzw vf4, vf4, vf8 ;; max dist
    sw t3, 28(t0)            ;; !!! set a vif on l0
    sll r0, r0, 0
    lbu s5, 53(t8)           ;; s5 = num-base-colors
    qmfc2.i gp, vf6          ;; dist
    sw t3, 44(t0)            ;; !!! set a vif on common
    qmfc2.i s3, vf7          ;; dist
    lbu s4, 56(t8)           ;; s4 = color-offset
    pcgtw s2, r0, gp         ;; dist
    lw gp, 12(t8)            ;; gp = colors-ptr
    pcgtw s3, r0, s3         ;; dist
    sb s4, 76(t0)            ;; store color-offset
    pinteh s4, s2, s3        ;; dist
    lbu s2, 54(t8)           ;; s2 = num-level0-colors
    ppacb s3, r0, s4         ;; dist
    lbu s1, 55(t8)           ;; s1 = num-level1-colors
    beq s3, r0, L56          ;; jump if dist fails?
    dsrl32 s4, s3, 8         ;; s4 is the level or something?

B26:
    beq s2, r0, L56          ;; if we have no level0 colors, use base
    sll r0, r0, 0

B27:
    beq s1, r0, L55          ;; if we have no level1 colors, use level0
    dsrl s5, s3, 16

B28:
    beq s5, r0, L55          ;; possible l1 skip based on lod
    dsrl32 s5, s3, 24

B29:
    bne s5, r0, L64_8loop_reject     ;; possible all skip based on lod.
    addiu s5, s1, 3                  ;; s5 = num-level1-colors + 3

B30:                  ;; level 1 color setup
    sra s4, s5, 2   ;; s4  = (num_color + 3) >> 4
    or s5, s1, r0   ;; s5  = (num_color)
    sll t3, s4, 2   ;; t3 = num colors, 4 aligned
    sh s4, 64(t0)   ;; color-tmpl qwc.
    sll r0, r0, 0
    sb t3, 78(t0)   ;; vif store
    daddiu t6, t6, 3 ;; use 3 qw's of global dma.
    lq s2, 32(t0) ;; load the common-tmpl!
    sll r0, r0, 0
    lq s1, 48(t0) ;; load the l1 tmpl!
    sll r0, r0, 0
    lq t3, 64(t0) ;; load the color tmpl!
    sq s2, 0(ra)  ;; store the common!
    sll r0, r0, 0
    sq s1, 16(ra) ;; store the l1!
    dsrl32 s2, s3, 16
    sq t3, 32(ra) ;; store the color
    daddiu ra, ra, 48 ;; advance the dma buffer pointer
    bne s2, r0, L57
    ori t3, t2, 18    ;; is this.. program 18?

B31:
    dsrl32 t3, s3, 8
    sll r0, r0, 0
    bne t3, r0, L57
    ori t3, t2, 16

B32:
    beq r0, r0, L57
    ori t3, t2, 14

B33:
L55:
    bne s4, r0, L64_8loop_reject
    addiu s5, s2, 3 ;; l0 colors + 3

B34:  l0 color setup
    sra s4, s5, 2 ;;  >> 2
    or s5, s2, r0 ;; s5 = qwc
    sll t3, s4, 2 ;; << 2
    sh s4, 64(t0) ;; color-tmpl qwc
    sll r0, r0, 0
    sb t3, 78(t0)  ;; vif store for unpack??
    daddiu t6, t6, 2 ;; only 2 qw's
    lq s2, 16(t0) ;; l0 tmpl
    sll r0, r0, 0
    lq t3, 64(t0) ;; color tmp
    sq s2, 0(ra)
    dsrl s3, s3, 8
    sq t3, 16(ra)
    daddiu ra, ra, 32
    bne s3, r0, L57
    ori t3, t2, 10

B35:
    beq r0, r0, L57
    ori t3, t2, 8

B36:
L56:
    bne s4, r0, L64_8loop_reject
    addiu s4, s5, 3 ;; base colors + 3

B37:
    sra s4, s4, 2
    sll r0, r0, 0
    sll t3, s4, 2
    sh s4, 64(t0)
    sll r0, r0, 0
    sb t3, 78(t0)
    ori t3, t2, 6
    lq s3, 0(t0) ;; base
    daddiu t6, t6, 2
    lq s2, 64(t0)  ;; color
    sq s3, 0(ra)
    sll r0, r0, 0
    sq s2, 16(ra)
    daddiu ra, ra, 32

;; END of the color setup
B38:
L57: ;; another opportunity to do some spad swappin
    addiu s3, r0, 127 ;; s3 = 127
    daddu s2, t6, s4  ;; s2 = dma-use + color qwc
    dsubu s3, s3, s2
    sll r0, r0, 0
    bgez s3, L60
    sll r0, r0, 0

B39:
L58:
    lw ra, 0(t1)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi ra, ra, 256
    sll r0, r0, 0
    beq ra, r0, L59
    sll r0, r0, 0

B40:
    sll r0, r0, 0
    lw ra, 184(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu ra, ra, 1
    sll r0, r0, 0
    sw ra, 184(t0)
    beq r0, r0, L58
    sll r0, r0, 0

B41:
L59:
    sw a3, 128(t1)
    xori a3, a3, 6144
    sw v1, 16(t1)
    sll ra, t6, 4
    addu v1, v1, ra
    or ra, a3, r0
    sw t6, 32(t1)
    addiu t6, r0, 256
    sw t6, 0(t1)
    addiu t6, r0, 0
B42:

L60:
    daddu t6, t6, s4   ;; add color imm's
    sw t8, 168(t0)     ;; back up tfrag... not enough regs
    ld s4, 0(gp)       ;; colors ptr
    daddiu t8, gp, 8   ;; t8 = colors + 8
    daddiu gp, s5, -4  ;; colors + 3 - 4
    lq s5, 128(t0)     ;; color-ptr
    pextlh s4, r0, s4  ;; expand
    mfc1 r0, f31
    paddw s2, s4, s5   ;; add to colors ptr
    mfc1 r0, f31
    lw s4, 0(s2)       ;;
    dsra32 s3, s2, 0
    lw s3, 0(s3)
    pcpyud s1, s2, s2
    lw s2, 0(s1)
    dsra32 s1, s1, 0
    blez gp, L62
    lw s1, 0(s1)

B43:
L61:
    ld s0, 0(t8)
    daddiu ra, ra, 16
    daddiu t8, t8, 8
    sw s4, -16(ra)
    daddiu gp, gp, -4
    sw s3, -12(ra)
    pextlh s4, r0, s0
    sw s2, -8(ra)
    paddw s2, s4, s5
    sw s1, -4(ra)
    lw s4, 0(s2)
    dsra32 s3, s2, 0
    lw s3, 0(s3)
    pcpyud s1, s2, s2
    lw s2, 0(s1)
    dsra32 s1, s1, 0
    bgtz gp, L61
    lw s1, 0(s1)

B44:
L62:
    daddiu ra, ra, 16
    lw t8, 168(t0)
    sll r0, r0, 0
    sw s4, -16(ra)
    sll r0, r0, 0
    sw s3, -12(ra)
    sll r0, r0, 0
    sw s2, -8(ra)
    sll r0, r0, 0
    sw s1, -4(ra)
B45:
L63_8loop_reject_tog_vis:
    xor t7, t7, t9 ;; update vis
    sll r0, r0, 0
B46:
L64_8loop_reject:
    daddiu t8, t8, 64
    srl t9, t9, 1
    addiu a2, a2, -1
    sll r0, r0, 0
    bne t9, r0, L51
    lqc2 vf2, 16(t8)

B47:
L65:
    sll r0, r0, 0
    lw gp, 160(t0)
    sll r0, r0, 0
    lw t9, 164(t0)
    bne gp, t9, L49
    sb t7, -1(gp)

B48:
    bgtz a2, L43_MAIN_LOOP_TOP
    sll r0, r0, 0

B49:
    beq t6, r0, L68
    sll r0, r0, 0

B50:
L66:
    lw a0, 0(t1)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi a0, a0, 256
    sll r0, r0, 0
    beq a0, r0, L67
    sll r0, r0, 0

B51:
    sll r0, r0, 0
    lw a0, 184(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu a0, a0, 1
    sll r0, r0, 0
    sw a0, 184(t0)
    beq r0, r0, L66
    sll r0, r0, 0

B52:
L67:
    sw a3, 128(t1)
    xori a0, a3, 6144
    sw v1, 16(t1)
    sll a1, t6, 4
    addu v1, v1, a1
    or a0, a0, r0
    sw t6, 32(t1)
    addiu a0, r0, 256
    sw a0, 0(t1)
    addiu a0, r0, 0
B53:
L68:
    lw a0, 0(t1)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    andi a0, a0, 256
    sll r0, r0, 0
    beq a0, r0, L69_CLEANUP
    sll r0, r0, 0

B54:
    sll r0, r0, 0
    lw a0, 184(t0)
    sll r0, r0, 0
    sll r0, r0, 0
    sll r0, r0, 0
    daddiu a0, a0, 1
    sll r0, r0, 0
    sw a0, 184(t0)
    beq r0, r0, L68
    sll r0, r0, 0

B55:
L69_CLEANUP:
    lw a0, 176(t0)
    sll r0, r0, 0
    sw t3, 172(t0)
    sll r0, r0, 0
    sqc2 vf4, 112(t0)
    sll r0, r0, 0
    sw v1, 4(a0)
    sll r0, r0, 0
    or v0, r0, r0
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
    sll r0, r0, 0
 */

struct TFragExtractStats {
  int num_l1 = 0;
  int num_l0 = 0;
  int num_base = 0;
};

struct UnpackState {
  int wl = 4;
  int cl = 4;
  u32 row[4] = {0, 0, 0, 0};
  bool row_init = false;
  u8 stmod = 0;
};

namespace {

int handle_unpack_v4_8_mode0(const VifCode& code,
                             const u8* dma,
                             int offset_word,
                             int cl,
                             int wl,
                             u8* out) {
  VifCodeUnpack unpack(code);
  assert(unpack.use_tops_flag);
  int offset = offset_word * 4;

  // CL x (num/WL)+(num%WL)

  if (unpack.is_unsigned) {
    // note: formulas below assume this!
    assert(cl == 2);
    assert(wl == 1);
    assert(code.num);
    for (int i = 0; i < code.num; i++) {
      // write every other qw
      int dest_qw = unpack.addr_qw + 2 * i;
      assert(dest_qw <= 328);
      u32 qw[4];
      qw[0] = dma[offset++];
      qw[1] = dma[offset++];
      qw[2] = dma[offset++];
      qw[3] = dma[offset++];
      memcpy(out + (dest_qw * 16), qw, 16);
    }
  } else {
    // note: formulas below assume this!
    assert(cl == 4);
    assert(wl == 4);
    assert(code.num);
    for (int i = 0; i < code.num; i++) {
      // write every other qw
      int dest_qw = unpack.addr_qw + i;
      assert(dest_qw <= 328);
      s32 qw[4];
      qw[0] = (s8)dma[offset++];
      qw[1] = (s8)dma[offset++];
      qw[2] = (s8)dma[offset++];
      qw[3] = (s8)dma[offset++];
      memcpy(out + (dest_qw * 16), qw, 16);
    }
  }

  assert((offset % 4) == 0);
  return offset / 4;
}

int handle_unpack_v4_8_mode1(const VifCode& code,
                             const u8* dma,
                             int offset_word,
                             int cl,
                             int wl,
                             const u32 row[4],
                             u8* out) {
  VifCodeUnpack unpack(code);
  assert(unpack.use_tops_flag);
  int offset = offset_word * 4;
  // CL x (num/WL)+(num%WL)

  if (unpack.is_unsigned) {
    // note: formulas below assume this!
    assert(cl == 4);
    assert(wl == 4);
    assert(code.num);
    for (int i = 0; i < code.num; i++) {
      // write every other qw
      int dest_qw = unpack.addr_qw + i;
      assert(dest_qw <= 328);
      u32 qw[4];
      qw[0] = row[0] + dma[offset++];
      qw[1] = row[1] + dma[offset++];
      qw[2] = row[2] + dma[offset++];
      qw[3] = row[3] + dma[offset++];
      memcpy(out + (dest_qw * 16), qw, 16);
    }
  } else {
    // note: formulas below assume this!
    assert(cl == 4);
    assert(wl == 4);
    assert(code.num);
    for (int i = 0; i < code.num; i++) {
      // write every other qw
      int dest_qw = unpack.addr_qw + i;
      assert(dest_qw <= 328);
      s32 qw[4];
      qw[0] = row[0] + (s8)dma[offset++];
      qw[1] = row[1] + (s8)dma[offset++];
      qw[2] = row[2] + (s8)dma[offset++];
      qw[3] = row[3] + (s8)dma[offset++];
      memcpy(out + (dest_qw * 16), qw, 16);
    }
  }

  assert((offset % 4) == 0);
  return offset / 4;
}

template <typename T, typename U>
T deref_ptr(const U* x) {
  T result;
  memcpy(&result, x, sizeof(T));
  return result;
}

int handle_unpack_v4_16_mode0(const VifCode& code,
                              const u8* dma,
                              int offset_word,
                              int cl,
                              int wl,
                              u8* vu_mem) {
  VifCodeUnpack unpack(code);
  assert(unpack.use_tops_flag);
  assert(unpack.is_unsigned);

  // note: formulas below assume this!
  assert(cl == 4);
  assert(wl == 4);

  int offset = offset_word * 4;

  assert(code.num);
  for (int i = 0; i < code.num; i++) {
    // write every other qw
    int dest_qw = unpack.addr_qw + i;
    assert(dest_qw <= 328);
    u32 qw[4];
    qw[0] = deref_ptr<u16>(dma + offset);
    offset += 2;
    qw[1] = deref_ptr<u16>(dma + offset);
    offset += 2;
    qw[2] = deref_ptr<u16>(dma + offset);
    offset += 2;
    qw[3] = deref_ptr<u16>(dma + offset);
    offset += 2;
    memcpy(vu_mem + (dest_qw * 16), qw, 16);
  }
  assert((offset % 4) == 0);
  return offset / 4;
}

int handle_unpack_v4_16_mode1(const VifCode& code,
                              const u8* dma,
                              int offset_word,
                              int cl,
                              int wl,
                              const u32 row[4],
                              u8* vu_mem) {
  VifCodeUnpack unpack(code);
  assert(unpack.use_tops_flag);
  assert(unpack.is_unsigned);

  // note: formulas below assume this!
  assert(cl == 4);
  assert(wl == 4);

  assert(code.num);
  int offset = offset_word * 4;
  for (int i = 0; i < code.num; i++) {
    // write every other qw
    int dest_qw = unpack.addr_qw + i;
    assert(dest_qw <= 328);
    u32 qw[4];
    qw[0] = row[0] + (u32)deref_ptr<u16>(dma + offset);
    offset += 2;
    qw[1] = row[1] + (u32)deref_ptr<u16>(dma + offset);
    offset += 2;
    qw[2] = row[2] + (u32)deref_ptr<u16>(dma + offset);
    offset += 2;
    qw[3] = row[3] + (u32)deref_ptr<u16>(dma + offset);
    offset += 2;

    // fmt::print("  unpack rgba?: {:x} {:x} {:x} {:x}\n", qw[0], qw[1], qw[2], qw[3]);
    memcpy(vu_mem + (dest_qw * 16), qw, 16);
  }
  assert((offset % 4) == 0);
  return offset / 4;
}

int handle_unpack_v3_32(const VifCode& code,
                        const u8* dma,
                        int offset_word,
                        int cl,
                        int wl,
                        u8* vu_mem) {
  VifCodeUnpack unpack(code);
  assert(unpack.use_tops_flag);
  assert(!unpack.is_unsigned);

  // note: formulas below assume this!
  assert(cl == 2);
  assert(wl == 1);

  assert(code.num);
  int offset = offset_word * 4;
  for (int i = 0; i < code.num; i++) {
    // write every other qw
    int dest_qw = unpack.addr_qw + i * 2;
    assert(dest_qw <= 328);
    u32 qw[4];
    qw[0] = deref_ptr<u32>(dma + offset);
    offset += 4;
    qw[1] = deref_ptr<u32>(dma + offset);
    offset += 4;
    qw[2] = deref_ptr<u32>(dma + offset);
    offset += 4;
    qw[3] = 0x80;  // this can be anything...
    memcpy(vu_mem + (dest_qw * 16), qw, 16);
  }
  assert((offset % 4) == 0);
  return offset / 4;
}

int handle_unpack_v4_32(const VifCode& code,
                        const u8* dma,
                        int offset_word,
                        int cl,
                        int wl,
                        u8* vu_mem) {
  VifCodeUnpack unpack(code);
  assert(unpack.use_tops_flag);
  assert(!unpack.is_unsigned);

  // note: formulas below assume this!
  assert(cl == 4);
  assert(wl == 4);
  int offset = offset_word * 4;
  assert(code.num);
  for (int i = 0; i < code.num; i++) {
    // write every other qw
    int dest_qw = unpack.addr_qw + i;
    assert(dest_qw <= 328);
    u32 qw[4];
    qw[0] = deref_ptr<u32>(dma + offset);
    offset += 4;
    qw[1] = deref_ptr<u32>(dma + offset);
    offset += 4;
    qw[2] = deref_ptr<u32>(dma + offset);
    offset += 4;
    qw[3] = deref_ptr<u32>(dma + offset);
    offset += 4;
    memcpy(vu_mem + (dest_qw * 16), qw, 16);
  }
  assert((offset % 4) == 0);
  return offset / 4;

  //  u8* write_base = get_upload_buffer();
  //  assert(code.num + unpack.addr_qw <= 328);
  //  memcpy(write_base + (unpack.addr_qw * 16), dma.data + offset, code.num * 16);
  //  return offset + code.num * 16;
}

}  // namespace

void emulate_chain(UnpackState& state, u32 max_words, const u32* start, u8* vu_mem) {
  u32 word = 0;

  while (word < max_words) {
    VifCode code(start[word]);
    word++;
    switch (code.kind) {
      case VifCode::Kind::STROW:
        state.row_init = true;
        memcpy(state.row, start + word, 16);
        word += 4;
        break;
      case VifCode::Kind::STMOD:
        if (state.stmod == 0) {
          assert(code.immediate == 1);
        } else {
          assert(state.stmod == 1);
          assert(code.immediate == 0 || code.immediate == 1);  // kinda weird.
        }
        state.stmod = code.immediate;
        break;
      case VifCode::Kind::UNPACK_V4_8:
        if (state.stmod == 0) {
          word = handle_unpack_v4_8_mode0(code, (const u8*)start, word, state.cl, state.wl, vu_mem);
        } else if (state.stmod == 1) {
          assert(state.row_init);
          word = handle_unpack_v4_8_mode1(code, (const u8*)start, word, state.cl, state.wl,
                                          state.row, vu_mem);
        } else {
          assert(false);
        }
        break;
      case VifCode::Kind::UNPACK_V4_16:
        if (state.stmod == 0) {
          word =
              handle_unpack_v4_16_mode0(code, (const u8*)start, word, state.cl, state.wl, vu_mem);
        } else if (state.stmod == 1) {
          assert(state.row_init);
          word = handle_unpack_v4_16_mode1(code, (const u8*)start, word, state.cl, state.wl,
                                           state.row, vu_mem);
        } else {
          assert(false);
        }
        break;
      case VifCode::Kind::UNPACK_V4_32:
        assert(state.stmod == 0);
        word = handle_unpack_v4_32(code, (const u8*)start, word, state.cl, state.wl, vu_mem);
        break;
      case VifCode::Kind::UNPACK_V3_32:
        assert(state.stmod == 0);
        word = handle_unpack_v3_32(code, (const u8*)start, word, state.cl, state.wl, vu_mem);
        break;
      case VifCode::Kind::NOP:
        break;
      case VifCode::Kind::STCYCL:

      {
        VifCodeStcycl ss(code.immediate);
        state.cl = ss.cl;
        state.wl = ss.wl;
      }

      break;
      default:
        throw Error("unknown vif: {}", code.print());
    }
  }
}

void emulate_dma_building_for_tfrag(const level_tools::TFragment& frag,
                                    std::vector<u8>& vu_mem,
                                    TFragExtractStats* stats) {
  UnpackState state;
  // all the templates do this...
  state.wl = 4;
  state.cl = 4;

  if (frag.num_level0_colors == 0) {
    // we're using base
    assert(frag.num_level1_colors == 0);
    stats->num_base++;
    emulate_chain(state, frag.dma_qwc[1], (const u32*)frag.dma_base.data(), vu_mem.data());

  } else if (frag.num_level1_colors == 0) {
    stats->num_l0++;
    emulate_chain(state, frag.dma_qwc[3], (const u32*)frag.dma_common_and_level0.data(),
                  vu_mem.data());
  } else {
    stats->num_l1++;
    // common
    emulate_chain(state, frag.dma_qwc[0], (const u32*)frag.dma_common_and_level0.data(),
                  vu_mem.data());
    // l1
    emulate_chain(state, frag.dma_qwc[2], (const u32*)frag.dma_level1.data(), vu_mem.data());
  }
}

void emulate_tfrags(const std::vector<level_tools::TFragment>& frags) {
  TFragExtractStats stats;

  std::vector<u8> vu_mem;
  vu_mem.resize(16 * 1024);

  for (auto& frag : frags) {
    emulate_dma_building_for_tfrag(frag, vu_mem, &stats);
  }
  fmt::print("l1: {}, l0: {}, base: {}\n", stats.num_l1, stats.num_l0, stats.num_base);
}

ExtractedTFragmentTree extract_tfrag(const level_tools::DrawableTreeTfrag* tree) {
  ExtractedTFragmentTree result;
  assert(tree->length == (int)tree->arrays.size());
  fmt::print("tree has {} arrays\n", tree->length);
  assert(tree->length > 0);

  auto last_array = tree->arrays.back().get();
  fmt::print("last_array: {}\n", last_array->my_type());

  auto as_tfrag_array = dynamic_cast<level_tools::DrawableInlineArrayTFrag*>(last_array);
  assert(as_tfrag_array);
  assert(as_tfrag_array->length == (int)as_tfrag_array->tfragments.size());
  assert(as_tfrag_array->length > 0);
  u16 idx = as_tfrag_array->tfragments.front().id;
  for (auto& elt : as_tfrag_array->tfragments) {
    assert(elt.id == idx);
    idx++;
  }
  bool ok = verify_node_indices(tree);
  assert(ok);

  result.vis_nodes = extract_vis_data(tree, as_tfrag_array->tfragments.front().id);
  assert(result.vis_nodes.last_child_node + 1 == idx);

  emulate_tfrags(as_tfrag_array->tfragments);
  return result;
}
}  // namespace decompiler