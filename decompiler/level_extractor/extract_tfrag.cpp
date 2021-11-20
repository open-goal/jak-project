#include "extract_tfrag.h"
#include "common/dma/dma.h"
#include "common/util/assert.h"
#include "decompiler/util/Error.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"

namespace decompiler {
namespace {
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

// The actual "tfrag" type. This only lives on the EE and gets converted to DMA data.
// some of the DMA data is pointers to chains that live in the static level data.
// other is colors that are looked up from the palette (on the EE!) then thrown in the
// double-buffered frame global buffer.

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

// This is the temp/debug structure used for the EE code

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
    lw gp, 12(t8)            ;; gp = colors-indices
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
    daddu t6, t6, s4      ;; add color imm's to dma buffer length
    sw t8, 168(t0)        ;; back up tfrag... not enough regs
    ld s4, 0(gp)          ;; load color-indices (u64 = u16 x 4)
    daddiu t8, gp, 8      ;; inc colors ptr
    daddiu gp, s5, -4     ;; gp is color counter. we're using the rounded up to 4 color count.
    lq s5, 128(t0)        ;; color-ptr x4
    pextlh s4, r0, s4     ;; expand packed u16's to u32's
    mfc1 r0, f31          ;; nop
    paddw s2, s4, s5      ;; add to color pointers
    mfc1 r0, f31
    lw s4, 0(s2)          ;; s4 = colors[0]
    dsra32 s3, s2, 0
    lw s3, 0(s3)          ;; s5 = colors[1]
    pcpyud s1, s2, s2
    lw s2, 0(s1)          ;; s2 = colors[2]
    dsra32 s1, s1, 0
    blez gp, L62
    lw s1, 0(s1)          ;; s1 = colors[3]

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


    Notes on the VU program
  vi03 is a pointer to an "address book" - a sequence of addresses
  vi02 contains addresses in this book
  from these xyw are loaded for vf28 (v3-32, with 2, 1)
    xy are floats. w is address of next vertex data.

  vi08 is a pointer to adgifs?
  vi09 is a pointer to some data like [vi12, ?, ?, vi13] ??

  vi12 counter, started negative?
  vi13 is adgif offset?

  vi04 is a pointer to tri-data:
    - vertex (w = 128.0?)
    - ?? (vf20)

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
    fmt::print("{}\n", code.print());
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

  assert(word == max_words);
  assert(state.stmod == 0);
}

struct TFragColorUnpack {
  std::vector<math::Vector<u16, 4>> data;
  u32 unpack_qw_addr = 0;
};

void emulate_dma_building_for_tfrag(const level_tools::TFragment& frag,
                                    std::vector<u8>& vu_mem,
                                    TFragColorUnpack& color_indices,
                                    TFragExtractStats* stats) {
  UnpackState state;
  // all the templates do this...
  state.wl = 4;
  state.cl = 4;

  // do the "canned" unpacks
  if (frag.num_level0_colors == 0) {
    // we're using base
    assert(frag.num_level1_colors == 0);
    stats->num_base++;
    emulate_chain(state, frag.dma_qwc[1] * 4, (const u32*)frag.dma_base.data(), vu_mem.data());

  } else if (frag.num_level1_colors == 0) {
    stats->num_l0++;
    emulate_chain(state, frag.dma_qwc[3] * 4, (const u32*)frag.dma_common_and_level0.data(),
                  vu_mem.data());
  } else {
    stats->num_l1++;
    // common
    emulate_chain(state, frag.dma_qwc[0] * 4, (const u32*)frag.dma_common_and_level0.data(),
                  vu_mem.data());

    state.wl = 4;
    state.cl = 4;
    // l1
    emulate_chain(state, frag.dma_qwc[2] * 4, (const u32*)frag.dma_level1.data(), vu_mem.data());
  }

  // the colors are copied to the dma-buffer directly.
  // these change per-frame based on time of day lighting.
  // 64 = color-tmp

  // sb 12, color-offset
  // sh 0, color qwc (round up, divide by 4)
  // sb 14, num-colors

  // the actual colors go in
  // color-tmpl is
  // :dma (new 'static 'dma-tag :id (dma-tag-id cnt))
  // :vif0 (new 'static 'vif-tag :imm #x102 :cmd (vif-cmd stcycl))       ;; cl = 2, wl = 1

  // :vif1 (new 'static 'vif-tag :imm #xc000 :cmd (vif-cmd unpack-v4-8)) ;; flg, unsigned
  color_indices.data.resize(frag.color_indices.size() / 2);
  color_indices.unpack_qw_addr = frag.color_offset;
  int i = 0;

  // we don't actually know the colors, so for now just put them like this.
  for (auto& color : color_indices.data) {
    color[0] = frag.color_indices.at(i) & 0xffff;
    color[1] = frag.color_indices.at(i) >> 16;
    i++;
    color[2] = frag.color_indices.at(i) & 0xffff;
    color[3] = frag.color_indices.at(i) >> 16;
    i++;
  }
}

/*

 // val = [0.5, 1.0, 2048.0 0.0]
 // adgif = gif tag for 5x a+d's
 // strgif = the gif tag for drawing
 //   pre = (new 'static 'gs-prim :prim (gs-prim-type tri-strip) :iip #x1 :tme #x1 :fge #x1 :abe
 arg1)
 //   regs = st, rgbaq, xyzf2
 // hvdf = changes
 // hmge = changes
 // fog = changes

 // common tfrag setup
  lq.xyzw vf02, 657(vi00)    |  nop                              ;;
  lq.xyzw vf05, 660(vi00)    |  addw.z vf28, vf00, vf00
  lq.xyzw vf06, 658(vi00)    |  nop
  lq.xyzw vf10, 661(vi00)    |  nop
  lq.xyzw vf11, 662(vi00)    |  nop
  lq.xyzw vf01, 656(vi00)    |  addz.z vf28, vf28, vf02
  ilw.w vi08, 4(vi14)        |  nop
  ilw.z vi09, 4(vi14)        |  nop
  ilw.y vi03, 3(vi14)        |  nop
  fcset 0x0                  |  nop
  iaddi vi07, vi00, -0x1     |  nop
  lq.xyzw vf04, 5(vi14)      |  mulw.xyzw vf16, vf00, vf00
  lq.xyzw vf07, 6(vi14)      |  mulw.xyzw vf17, vf00, vf00
  ibne vi00, vi14, L136      |  mulw.xyzw vf18, vf00, vf00
  lq.xyzw vf08, 7(vi14)      |  mulw.xyzw vf19, vf00, vf00

 */

struct VuMemWrapper {
  VuMemWrapper(const std::vector<u8>& vu_mem) : mem(&vu_mem) {}
  const std::vector<u8>* mem = nullptr;
  u16 ilw_data(int offset, int xyzw) {
    u16 result;

    assert(offset < 328);
    assert(offset >= 0);
    int mem_offset = (xyzw * 4) + (offset * 16);
    memcpy(&result, mem->data() + mem_offset, 2);
    return result;
  }

  math::Vector4f load_vector_data(int offset) {
    math::Vector4f result;
    // offset = offset & 0x3ff;  // not super happy with this...
    assert(offset < 328);
    assert(offset >= 0);
    memcpy(&result, mem->data() + (offset * 16), 16);
    return result;
  }
};

using math::Vector3f;
using math::Vector4f;

float u32_2_float(u32 x) {
  float y;
  memcpy(&y, &x, 4);
  return y;
}

u32 float_2_u32(float x) {
  u32 y;
  memcpy(&y, &x, 4);
  return y;
}

Vector4f itof0(const Vector4f& vec) {
  Vector4f result;
  for (int i = 0; i < 4; i++) {
    s32 val;
    memcpy(&val, vec.data() + i, 4);
    result[i] = val;
  }
  return result;
}

Vector4f ftoi4(const Vector4f& vec) {
  Vector4f result;
  for (int i = 0; i < 4; i++) {
    s32 f = vec[i] * 16.f;
    float val;
    memcpy(&val, &f, 4);
    result[i] = val;
  }
  return result;
}

struct TFragVertexData {
  Vector4f pre_cam_trans_pos;
  Vector3f stq;  // stq?
  Vector4f rgba;

  // pos = cam.rot * pctp.xyz + cam.trans
  // q = fog.x / pctp.w
  // pos *= q
  // pos += hvdf_offset
  // pos.w = min(pos.w, fog.z)
  // pos.w = max(pow.w, fog.y)
  // pos.w += fog.w

  // unk *= q
};

struct TFragDraw {
  u8 adgif_data[16 * 5];
  u32 dvert = 0;
  std::vector<TFragVertexData> verts;
};

template <bool DEBUG>
bool emulate_kick_subroutine(VuMemWrapper& mem,
                             TFragDraw& current_draw,
                             std::vector<TFragDraw>& all_draws,
                             u16& vi05_end_of_vert_kick_data,
                             u16& vi06_kick_zone_ptr,
                             u16& vi07,
                             u16& vi08_adgif_base,
                             u16& vi09_draw_addr_book,
                             u16& vi10_start_of_vert_kick_data,
                             u16& vi12_vert_count,
                             u16& vi13_adgifs,
                             u16& vf24_u16) {
  // KICK ZONE!
  // we reach here if we need to issue strgif/adgifs before the next vertex.

  //  L122:
  //  fcset 0x0
  // m_clip_and_3ffff = false;  // ??
  //  iaddi vi07, vi00, -0x1
  vi07 = -1;  // not actually used?
  //  iblez vi12, L123
  //  iaddi vi09, vi09, 0x1
  vi09_draw_addr_book++;  // on to the next chunk
  //  fmt::print("VI09 now {}\n", vars.vi09);

  // no need for new adgifs, just a new strgif.
  if (((s16)vi12_vert_count) > 0) {
    //  ior vi10, vi06, vi00
    vi10_start_of_vert_kick_data = vi06_kick_zone_ptr;  // start of next chunk
    //  iadd vi01, vi12, vi12
    u16 vi01 = vi12_vert_count + vi12_vert_count;
    //  iadd vi01, vi01, vi12
    vi01 += vi12_vert_count;  // qw of verts (not including 1 qw of strgif)
    //  iadd vi05, vi06, vi01
    vi05_end_of_vert_kick_data = vi06_kick_zone_ptr + vi01;  // last qw to write before coming here
    //  sqi.xyzw vf06, vi06
    // store_gif_kick_zone(vars.vi06_kick_zone_ptr, m_tfrag_data.str_gif);
    vi06_kick_zone_ptr++;
    //  isw.x vi12, -1(vi06)
    // store_u32_kick_zone(vars.vi12, vars.vi06_kick_zone_ptr - 1, 0); // store verts
    current_draw.dvert = vi12_vert_count;
    //  jr vi15
    //  ilwr.x vi12, vi09
    vi12_vert_count = mem.ilw_data(vi09_draw_addr_book, 0);  // next vert count!
    if (DEBUG) {
      fmt::print("continue with this adgif, but new strgif. next {} verts (kick zone now {})\n",
                 vi12_vert_count, vi06_kick_zone_ptr);
    }
    //    fmt::print("didn't kick, vi12 now {}\n", vars.vi12);
    return false;
  }

  //  L123:
  //  ilw.y vi01, -1(vi09)
  u16 vi01 = mem.ilw_data(vi09_draw_addr_book - 1, 1);  // ?
  //  ilw.z vi13, -1(vi09)
  vi13_adgifs = mem.ilw_data(vi09_draw_addr_book - 1, 2);  // load new adgif addr
  //  fmt::print("VI09 loads: {} {}\n", m_ptrs.vi01, vars.vi13);
  //  ibeq vi00, vi12, L126
  //  ilwr.x vi14, vi10
  //  fmt::print("val is {}: {}\n", vars.vi10, ilw_kick_zone(vars.vi10, 0));
  // vars.vi14 = mem.ilw_kick_zone(vi10_start_of_vert_kick_data, 0); old vert count
  if (vi12_vert_count != 0) {
    //  ibltz vi01, L124
    //  iaddiu vi12, vi12, 0x80
    vi12_vert_count += 0x80;
    if (((s16)vi01) >= 0) {
      all_draws.push_back(current_draw);
      current_draw.verts.clear();
      //  iadd vi13, vi13, vi08
      vi13_adgifs += vi08_adgif_base;
      //  lqi.xyzw vf29, vi13
      auto vf29_adg0 = mem.load_vector_data(vi13_adgifs++);
      //  lqi.xyzw vf30, vi13
      auto vf30_adg1 = mem.load_vector_data(vi13_adgifs++);
      //  lqi.xyzw vf31, vi13
      auto vf31_adg2 = mem.load_vector_data(vi13_adgifs++);
      //  sqi.xyzw vf05, vi06
      // store_gif_kick_zone(vars.vi06_kick_zone_ptr++, m_tfrag_data.ad_gif);
      vi06_kick_zone_ptr++;

      //  sqi.xyzw vf29, vi06
      // store_vector_kick_zone(vars.vi06_kick_zone_ptr++, vars.vf29);
      memcpy(current_draw.adgif_data, vf29_adg0.data(), 16);
      vi06_kick_zone_ptr++;

      //  sqi.xyzw vf30, vi06
      memcpy(current_draw.adgif_data + 16, vf30_adg1.data(), 16);
      vi06_kick_zone_ptr++;

      //  sqi.xyzw vf31, vi06
      memcpy(current_draw.adgif_data + 32, vf31_adg2.data(), 16);
      vi06_kick_zone_ptr++;

      //  lqi.xyzw vf29, vi13
      vf29_adg0 = mem.load_vector_data(vi13_adgifs++);
      //  lqi.xyzw vf30, vi13
      vf30_adg1 = mem.load_vector_data(vi13_adgifs++);
      //  iadd vi01, vi12, vi12
      vi01 = vi12_vert_count + vi12_vert_count;
      //  iadd vi01, vi01, vi12
      vi01 += vi12_vert_count;
      //  sqi.xyzw vf29, vi06
      memcpy(current_draw.adgif_data + (16 * 3), vf29_adg0.data(), 16);
      vi06_kick_zone_ptr++;
      //  sqi.xyzw vf30, vi06
      memcpy(current_draw.adgif_data + (16 * 4), vf30_adg1.data(), 16);
      vi06_kick_zone_ptr++;
      //  ior vi10, vi06, vi00
      vi10_start_of_vert_kick_data = vi06_kick_zone_ptr;
      //  iadd vi05, vi06, vi01
      vi05_end_of_vert_kick_data = vi06_kick_zone_ptr + vi01;
      //  sqi.xyzw vf06, vi06
      // store_gif_kick_zone(vars.vi06_kick_zone_ptr++, m_tfrag_data.str_gif);
      vi06_kick_zone_ptr++;
      //  isw.x vi12, -1(vi06)
      // store_u32_kick_zone(vars.vi12, vars.vi06_kick_zone_ptr - 1, 0);
      current_draw.dvert = vi12_vert_count;
      //  jr vi15
      //  ilwr.x vi12, vi09
      vi12_vert_count = mem.ilw_data(vi09_draw_addr_book, 0);
      fmt::print("done with adgifs but not packet, now moving on to another with {}\n",
                 (s16)vi12_vert_count);
      //      fmt::print("didn't kick 2, vi12 now {}\n", vars.vi12);
      return false;
    }

    //  L124:
    //  mtir vi01, vf24.w
    vi01 = vf24_u16;
    //  mtir vi06, vf03.y
    vi06_kick_zone_ptr = 0;
    //  mr32.xyzw vf03, vf03

    //  iadd vi14, vi14, vi11
    // vars.vi14 += vars.vi11; sets eop, ignore.

    //  ibgez vi13, L125
    //  iswr.x vi14, vi10
    //    fmt::print("kick zone store: {}\n", vars.vi14);
    // store_u32_kick_zone(vars.vi14, vars.vi10, 0); set eop.
    if (((s16)vi13_adgifs) < 0) {
      //  xgkick vi01
      all_draws.push_back(current_draw);
      current_draw.verts.clear();
      //  ior vi10, vi06, vi00
      vi10_start_of_vert_kick_data =
          vi06_kick_zone_ptr;  // xgkick delay slots, doesn't seem to matter.
      //  mfir.w vf24, vi06
      vf24_u16 = vi06_kick_zone_ptr;
      //  iadd vi01, vi12, vi12
      vi01 = vi12_vert_count + vi12_vert_count;
      //  iadd vi01, vi01, vi12
      vi01 += vi12_vert_count;
      //  iadd vi05, vi06, vi01
      vi05_end_of_vert_kick_data = vi06_kick_zone_ptr + vi01;
      //  sqi.xyzw vf06, vi06
      // store_gif_kick_zone(vars.vi06_kick_zone_ptr++, m_tfrag_data.str_gif);
      vi06_kick_zone_ptr++;
      //  isw.x vi12, -1(vi06)
      // store_u32_kick_zone(vars.vi12, vars.vi06_kick_zone_ptr - 1, 0);
      //  jr vi15
      //  ilwr.x vi12, vi09
      vi12_vert_count = mem.ilw_data(vi09_draw_addr_book, 0);
      return false;
    }

    //  L125:
    //  iadd vi13, vi13, vi08
    vi13_adgifs += vi08_adgif_base;
    //  xgkick vi01
    all_draws.push_back(current_draw);
    current_draw.verts.clear();

    //  lqi.xyzw vf29, vi13
    auto vf29_adg0 = mem.load_vector_data(vi13_adgifs++);
    //  lqi.xyzw vf30, vi13
    auto vf30_adg1 = mem.load_vector_data(vi13_adgifs++);
    //  lqi.xyzw vf31, vi13
    auto vf31_adg2 = mem.load_vector_data(vi13_adgifs++);
    //  mfir.w vf24, vi06
    vf24_u16 = vi06_kick_zone_ptr;
    //  sqi.xyzw vf05, vi06
    // store_gif_kick_zone(vars.vi06_kick_zone_ptr++, m_tfrag_data.ad_gif);
    vi06_kick_zone_ptr++;

    //  sqi.xyzw vf29, vi06
    // store_vector_kick_zone(vars.vi06_kick_zone_ptr++, vars.vf29);
    memcpy(current_draw.adgif_data, vf29_adg0.data(), 16);
    vi06_kick_zone_ptr++;

    //  sqi.xyzw vf30, vi06
    memcpy(current_draw.adgif_data + 16, vf30_adg1.data(), 16);
    vi06_kick_zone_ptr++;

    //  sqi.xyzw vf31, vi06
    memcpy(current_draw.adgif_data + 32, vf31_adg2.data(), 16);
    vi06_kick_zone_ptr++;

    //  lqi.xyzw vf29, vi13
    vf29_adg0 = mem.load_vector_data(vi13_adgifs++);
    //  lqi.xyzw vf30, vi13
    vf30_adg1 = mem.load_vector_data(vi13_adgifs++);
    //  iadd vi01, vi12, vi12
    vi01 = vi12_vert_count + vi12_vert_count;
    //  iadd vi01, vi01, vi12
    vi01 += vi12_vert_count;
    //  sqi.xyzw vf29, vi06
    memcpy(current_draw.adgif_data + (16 * 3), vf29_adg0.data(), 16);
    vi06_kick_zone_ptr++;
    //  sqi.xyzw vf30, vi06
    memcpy(current_draw.adgif_data + (16 * 4), vf30_adg1.data(), 16);
    vi06_kick_zone_ptr++;
    //  nop
    //  ior vi10, vi06, vi00
    vi10_start_of_vert_kick_data = vi06_kick_zone_ptr;
    //  iadd vi05, vi06, vi01
    vi05_end_of_vert_kick_data = vi06_kick_zone_ptr + vi01;
    //  sqi.xyzw vf06, vi06
    // store_gif_kick_zone(vars.vi06_kick_zone_ptr++, m_tfrag_data.str_gif);
    vi06_kick_zone_ptr++;
    //  isw.x vi12, -1(vi06)
    // store_u32_kick_zone(vars.vi12, vars.vi06_kick_zone_ptr - 1, 0);
    //  jr vi15
    //  ilwr.x vi12, vi09
    vi12_vert_count = mem.ilw_data(vi09_draw_addr_book, 0);
    //    fmt::print("did kick, vi12 now {}\n", vars.vi12);
    return false;
  }

  //  L126:
  //  mtir vi01, vf24.w
  // m_ptrs.vi01 = float_2_u32(vars.vf24.w());
  //  mr32.xyzw vf03, vf03
  //  auto temp = m_ptrs.vf03_x;
  //  m_ptrs.vf03_x = m_ptrs.vf03_y;
  //  m_ptrs.vf03_y = m_ptrs.vf03_z;
  //  m_ptrs.vf03_z = m_ptrs.vf03_w;
  //  m_ptrs.vf03_w = temp;
  all_draws.push_back(current_draw);
  current_draw.verts.clear();
  //  iadd vi14, vi14, vi11
  //  fmt::print("before add: {}\n", vars.vi14);
  //  vars.vi14 += vars.vi11;
  //  iswr.x vi14, vi10
  //  fmt::print("kick zone store: {}\n", vars.vi14);
  //  store_u32_kick_zone(vars.vi14, vars.vi10, 0);
  //  lq.xyzw vf04, 664(vi00)
  // todo don't think I needed that load of ambient
  //  XGKICK<DEBUG>(m_ptrs.vi01, render_state, prof);
  //  xgkick vi01
  //  nop                        |  nop :e
  return true;
}

template <bool DEBUG>
void emulate_tfrag_execution(const level_tools::TFragment& frag,
                             VuMemWrapper& mem,
                             TFragColorUnpack& color_indices,
                             TFragExtractStats* stats) {
  fmt::print("tfrag exec. offset of colors = {}\n", color_indices.unpack_qw_addr);
  std::vector<TFragDraw> all_draws;
  TFragDraw current_draw;

  TFragVertexData vertex_pipeline[4];

  u16 vi14 = 0;

  float vf28_z = 2049;

  //  ilw.w vi08, 4(vi14)        |  nop
  u16 vi08_adgif_base = mem.ilw_data(4 + vi14, 3);  // is an address, v4/32 unpack.
  //  fmt::print("------------- VI08 init: {}\n", vars.vi08);
  //  ilw.z vi09, 4(vi14)        |  nop
  u16 vi09_draw_addr_book =
      mem.ilw_data(4 + vi14, 2);  // is an input address, v4/8 unpack (seems small?)
  //  ilw.y vi03, 3(vi14)        |  nop
  u16 vi03_vert_addr_book = mem.ilw_data(
      3 + vi14,
      1);  // is an input address (v4-8 with strow). a list of addresses for v4-16's with strow

  //  fmt::print("-------VI03 init: {}\n", vars.vi03);

  if (DEBUG) {
    // small, like 9, 54, 66
    level_tools::PrintSettings settings;
    settings.print_tfrag = true;
    fmt::print("{}\n", frag.print(settings, 0));
    fmt::print("ints: {} {} {}\n", vi08_adgif_base, vi09_draw_addr_book, vi03_vert_addr_book);
  }

  // fmt::print("vi09: #x{:x} ({})\n", vars.vi09, vars.vi14);

  //  fcset 0x0                  |  nop
  //  iaddi vi07, vi00, -0x1     |  nop
  u16 vi07 = -1;

  //  lq.xyzw vf04, 5(vi14)      |  mulw.xyzw vf16, vf00, vf00
  // inputs.vf04_cam_mat_x = load_vector_data(vars.vi14 + 5);
  Vector4f vf16_scaled_pos_0 = Vector4f(0, 0, 0, 1);

  //  lq.xyzw vf07, 6(vi14)      |  mulw.xyzw vf17, vf00, vf00
  // inputs.vf07_cam_mat_y = load_vector_data(vars.vi14 + 6);
  Vector4f vf17_scaled_pos_1 = Vector4f(0, 0, 0, 1);

  //  ibne vi00, vi14, L136      |  mulw.xyzw vf18, vf00, vf00
  Vector4f vf18_scaled_pos_2 = Vector4f(0, 0, 0, 1);
  //  lq.xyzw vf08, 7(vi14)      |  mulw.xyzw vf19, vf00, vf00
  Vector4f vf19_scaled_pos_3 = Vector4f(0, 0, 0, 1);
  // inputs.vf08_cam_mat_z = load_vector_data(vars.vi14 + 7);

  ////////////////////////////////////////////////////////////////////////////////////////

  //  ilwr.x vi02, vi03          |  nop
  assert(vi03_vert_addr_book < 328);                            // should be a buffer 0 addr
  u16 vi02_pre_vtx_ptr = mem.ilw_data(vi03_vert_addr_book, 0);  // is an addr? v4/16 with strom
  if (DEBUG) {
    fmt::print("vi02-warmup 0: {}\n", vi02_pre_vtx_ptr);
  }

  //  lq.xyzw vf09, 8(vi14)      |  nop
  // vars.vf09_cam_trans = load_vector_data(vars.vi14 + 8);

  // correct addrs
  //  iadd vi08, vi08, vi14      |  nop
  //  iadd vi09, vi09, vi14      |  nop

  //  lq.xyw vf28, 0(vi02)       |  nop
  auto vf28_load_temp = mem.load_vector_data(vi02_pre_vtx_ptr);
  float vf28_x = vf28_load_temp.x();
  float vf28_y = vf28_load_temp.y();
  float vf28_w_addr_of_next_vtx = vf28_load_temp.w();  // addr, of v3-32, with 2, 1

  if (DEBUG) {
    fmt::print("vf28 load 0: x_f {} y_f {} z_u32 {}\n", vf28_x, vf28_y,
               float_2_u32(vf28_w_addr_of_next_vtx));
  };

  // they rotate vi06 to alternate the kick zone buffer.
  // it's loaded with [a, b, a, b]
  //  mtir vi06, vf03.x          |  nop
  // vars.vi06_kick_zone_ptr = m_ptrs.vf03_x;
  u16 vi06_kick_zone_ptr = 0;  // moving kick zone to 0.

  //  ilwr.x vi12, vi09          |  nop
  u16 vi12_vert_count = mem.ilw_data(vi09_draw_addr_book, 0);  // some sort of counter?
  if (DEBUG) {
    fmt::print("vi12: 0x{:x}\n", vi12_vert_count);
  }

  //  ilwr.z vi13, vi09          |  nop
  u16 vi13_adgifs = mem.ilw_data(vi09_draw_addr_book, 2);
  if (DEBUG) {
    fmt::print("vi13: 0x{:x}\n", vi13_adgifs);
  }

  //  mtir vi04, vf28.w          |  subz.xyz vf24, vf28, vf02
  u16 vi04_vtx_ptr =
      float_2_u32(vf28_w_addr_of_next_vtx);  // addr, of v3-32, with 2, 1 VERTEX POINTER
  Vector4f vf24_stq_0;
  vf24_stq_0.x() = vf28_x - 2048.f;
  vf24_stq_0.y() = vf28_y - 2048.f;
  vf24_stq_0.z() = vf28_z - 2048.f;
  vf24_stq_0.w() = 0;  // set later.

  //  iaddiu vi11, vi00, 0x4000  |  nop
  u16 vi11 = 0x4000;

  //  iaddiu vi11, vi11, 0x4000  |  nop
  vi11 += 0x4000;

  //  ilwr.y vi02, vi03          |  nop
  vi02_pre_vtx_ptr = mem.ilw_data(vi03_vert_addr_book, 1);
  if (DEBUG) {
    fmt::print("vi02-warmup 1: {}\n", vi02_pre_vtx_ptr);
  }

  // vertex load
  //  lq.xyzw vf12, 0(vi04)      |  nop
  // integer vertex position
  Vector4f vf12_vtx_pos_0 = mem.load_vector_data(vi04_vtx_ptr);

  //  lq.xyzw vf20, 1(vi04)      |  nop
  // ??? something with the vertex.
  Vector4f vf20_vtx_rgba_0 = mem.load_vector_data(vi04_vtx_ptr + 1);

  //  iaddiu vi12, vi12, 0x80    |  nop
  vi12_vert_count += 0x80;  // ??

  //  iadd vi13, vi13, vi08      |  nop
  vi13_adgifs += vi08_adgif_base;

  //  lq.xyw vf28, 0(vi02)       |  itof0.xyzw vf12, vf12
  vf28_load_temp = mem.load_vector_data(vi02_pre_vtx_ptr);
  vf28_x = vf28_load_temp.x();
  vf28_y = vf28_load_temp.y();
  vf28_w_addr_of_next_vtx = vf28_load_temp.w();  // addr, of v3-32, with 2, 1
  vf12_vtx_pos_0 = itof0(vf12_vtx_pos_0);

  if (DEBUG) {
    fmt::print("vf28 load 0: x_f {} y_f {} w_u32 {}\n", vf28_x, vf28_y,
               float_2_u32(vf28_w_addr_of_next_vtx));
    fmt::print("vtx w0: {}\n", vf12_vtx_pos_0.to_string_aligned());
  };

  //  mfir.w vf24, vi06          |  nop
  // remember the start of the kick zone, I guess?
  u16 vf24_w_u16 = vi06_kick_zone_ptr;

  //  lqi.xyzw vf29, vi13        |  nop
  auto vf29_adgif0 = mem.load_vector_data(vi13_adgifs);
  vi13_adgifs++;

  //  lqi.xyzw vf30, vi13        |  nop
  auto vf30_adgif1 = mem.load_vector_data(vi13_adgifs);
  vi13_adgifs++;

  //  lqi.xyzw vf31, vi13        |  nop
  auto vf31_adgif2 = mem.load_vector_data(vi13_adgifs);
  vi13_adgifs++;

  //  sqi.xyzw vf05, vi06        |  subz.xyz vf25, vf28, vf02
  // store_gif_kick_zone(vars.vi06_kick_zone_ptr, m_tfrag_data.ad_gif); sets the adgif header.
  vi06_kick_zone_ptr++;
  Vector4f vf25_stq_1;
  vf25_stq_1.x() = vf28_x - 2048.f;
  vf25_stq_1.y() = vf28_y - 2048.f;
  vf25_stq_1.z() = vf28_z - 2048.f;
  vf25_stq_1.w() = 0;  // set later.

  //  sqi.xyzw vf29, vi06        |  mulaw.xyzw ACC, vf09, vf00
  // store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf29);
  memcpy(current_draw.adgif_data, vf29_adgif0.data(), 16);
  vi06_kick_zone_ptr++;

  //  mtir vi04, vf28.w          |  nop
  vi04_vtx_ptr = float_2_u32(vf28_w_addr_of_next_vtx);

  //  sqi.xyzw vf30, vi06        |  maddax.xyzw ACC, vf04, vf12
  memcpy(current_draw.adgif_data + 16, vf30_adgif1.data(), 16);
  vi06_kick_zone_ptr++;

  //  sqi.xyzw vf31, vi06        |  nop
  memcpy(current_draw.adgif_data + 32, vf31_adgif2.data(), 16);
  vi06_kick_zone_ptr++;

  //  ilwr.z vi02, vi03          |  nop
  vi02_pre_vtx_ptr = mem.ilw_data(vi03_vert_addr_book, 2);
  if (DEBUG) {
    fmt::print("pre-vtx-vi02-warmup 2: {}\n", vi02_pre_vtx_ptr);
  }

  //  lq.xyzw vf13, 0(vi04)      |  madday.xyzw ACC, vf07, vf12
  Vector4f vf13_vtx_pos_1 = mem.load_vector_data(vi04_vtx_ptr);
  // acc += in.vf07_cam_mat_y * vars.vf12_root_pos_0.y();

  //  lq.xyzw vf21, 1(vi04)      |  maddz.xyzw vf12, vf08, vf12
  Vector4f vf21_vtx_unk_1 = mem.load_vector_data(vi04_vtx_ptr + 1);
  // vars.vf12_root_pos_0 = acc + in.vf08_cam_mat_z * vars.vf12_root_pos_0.z();

  //  lqi.xyzw vf29, vi13        |  nop
  vf29_adgif0 = mem.load_vector_data(vi13_adgifs);
  vi13_adgifs++;

  //  lqi.xyzw vf30, vi13        |  nop
  vf30_adgif1 = mem.load_vector_data(vi13_adgifs);
  vi13_adgifs++;

  //  lq.xyw vf28, 0(vi02)       |  itof0.xyzw vf13, vf13
  vf28_load_temp = mem.load_vector_data(vi02_pre_vtx_ptr);
  vf28_x = vf28_load_temp.x();
  vf28_y = vf28_load_temp.y();
  vf28_w_addr_of_next_vtx = vf28_load_temp.w();  // addr, of v3-32, with 2, 1
  vf13_vtx_pos_1 = itof0(vf13_vtx_pos_1);

  //  div Q, vf01.x, vf12.w      |  mul.xyzw vf16, vf12, vf11
  // float q = m_tfrag_data.fog.x() / vars.vf12_root_pos_0.w();
  // vars.vf16_scaled_pos_0 = vars.vf12_root_pos_0.elementwise_multiply(m_tfrag_data.hmge_scale);

  //  sqi.xyzw vf29, vi06        |  nop
  memcpy(current_draw.adgif_data + (16 * 3), vf29_adgif0.data(), 16);
  vi06_kick_zone_ptr++;

  //  sqi.xyzw vf30, vi06        |  nop
  memcpy(current_draw.adgif_data + (16 * 4), vf30_adgif1.data(), 16);
  vi06_kick_zone_ptr++;

  //  iadd vi01, vi12, vi12      |  subz.xyz vf26, vf28, vf02
  u16 vi01 = vi12_vert_count + vi12_vert_count;
  Vector4f vf26_stq_2;
  vf26_stq_2.x() = vf28_x - 2048.f;
  vf26_stq_2.y() = vf28_y - 2048.f;
  vf26_stq_2.z() = vf28_z - 2048.f;
  vf26_stq_2.w() = 0;  // set later.

  //  iadd vi01, vi01, vi12      |  mulaw.xyzw ACC, vf09, vf00
  vi01 += vi12_vert_count;  // vi01 is now vi12 * 3, so qwc for gs vert data (3 regs/vert)

  //  mtir vi04, vf28.w          |  nop
  vi04_vtx_ptr = float_2_u32(vf28_w_addr_of_next_vtx);

  //  iadd vi05, vi06, vi01      |  maddax.xyzw ACC, vf04, vf13
  u16 vi05_end_of_vert_kick_data = vi06_kick_zone_ptr + vi01;
  fmt::print("num verts in chunk: {}\n", vi12_vert_count);

  //  ior vi10, vi06, vi00       |  mul.xyz vf12, vf12, Q
  u16 vi10_start_of_vert_kick_data = vi06_kick_zone_ptr;
  //  vars.vf12_root_pos_0.x() *= q;
  //  vars.vf12_root_pos_0.y() *= q;
  //  vars.vf12_root_pos_0.z() *= q;

  //  ilwr.w vi02, vi03          |  mul.xyz vf24, vf24, Q
  vi02_pre_vtx_ptr = mem.ilw_data(vi03_vert_addr_book, 3);
  if (DEBUG) {
    fmt::print("pre-vtx-vi02-warmup 3: {}\n", vi02_pre_vtx_ptr);
  }

  //  lq.xyzw vf14, 0(vi04)      |  madday.xyzw ACC, vf07, vf13
  Vector4f vf14_vtx_pos_2 = mem.load_vector_data(vi04_vtx_ptr);

  //  lq.xyzw vf22, 1(vi04)      |  maddz.xyzw vf13, vf08, vf13
  Vector4f vf22_vtx_rgba_2 = mem.load_vector_data(vi04_vtx_ptr + 1);

  //  sqi.xyzw vf06, vi06        |  add.xyzw vf12, vf12, vf10
  vi06_kick_zone_ptr++;
  // vars.vf12_root_pos_0 += m_tfrag_data.hvdf_offset; not precomputed

  //  isw.x vi12, -1(vi06)       |  nop
  // store_u32_kick_zone(vars.vi12, vars.vi06_kick_zone_ptr - 1, 0);
  // stores dvert count
  current_draw.dvert = vi12_vert_count;

  //  lq.xyw vf28, 0(vi02)       |  itof0.xyzw vf14, vf14
  vf28_load_temp = mem.load_vector_data(vi02_pre_vtx_ptr);
  vf28_x = vf28_load_temp.x();
  vf28_y = vf28_load_temp.y();
  vf28_w_addr_of_next_vtx = vf28_load_temp.w();  // addr, of v3-32, with 2, 1
  vf14_vtx_pos_2 = itof0(vf14_vtx_pos_2);

  //  div Q, vf01.x, vf13.w      |  mul.xyzw vf17, vf13, vf11
  // m_q = m_tfrag_data.fog.x() / vars.vf13_root_pos_1.w();
  // vars.vf17_scaled_pos_1 = vars.vf13_root_pos_1.elementwise_multiply(m_tfrag_data.hmge_scale);

  //  iaddi vi09, vi09, 0x1      |  miniz.w vf12, vf12, vf01
  vi09_draw_addr_book++;

  //  ilwr.x vi12, vi09          |  clipw.xyz vf16, vf16
  vi12_vert_count = mem.ilw_data(vi09_draw_addr_book, 0);
  fmt::print("next block (out of warmup) dvert: {}\n", vi12_vert_count);
  // m_clip_and_3ffff = clip_xyz_plus_minus(vars.vf16_scaled_pos_0);

  Vector4f vf27_vtx_stq_3;
  Vector4f vf23_vtx_rgba_3;
  Vector4f vf15_vtx_pos_3;

  while (true) {
    //////////// L128

    // Part 0 for X
    //  iaddi vi03, vi03, 0x1      |  subz.xyz vf27, vf28, vf02
    vi03_vert_addr_book++;
    vf27_vtx_stq_3.x() = vf28_x - 2048.f;
    vf27_vtx_stq_3.y() = vf28_y - 2048.f;
    vf27_vtx_stq_3.z() = vf28_z - 2048.f;

    //  iaddi vi07, vi07, 0x1      |  mulaw.xyzw ACC, vf09, vf00
    vi07++;  // ?
    // m_acc = vars.vf09_cam_trans;

    //  mtir vi04, vf28.w          |  maxy.w vf12, vf12, vf01
    vi04_vtx_ptr = float_2_u32(vf28_w_addr_of_next_vtx);
    // vars.vf12_root_pos_0.w() = std::max(vars.vf12_root_pos_0.w(), m_tfrag_data.fog.y());

    //  fcand vi01, 0x3ffff        |  maddax.xyzw ACC, vf04, vf14
    // m_acc += in.vf04_cam_mat_x * vars.vf14_loop_pos_0.x();
    // fcand already calculated

    //  ibeq vi00, vi01, L129      |  mul.xyz vf13, vf13, Q
    // branch made after next instr
    //    vars.vf13_root_pos_1.x() *= m_q;
    //    vars.vf13_root_pos_1.y() *= m_q;
    //    vars.vf13_root_pos_1.z() *= m_q;

    //  ilwr.x vi02, vi03          |  mul.xyz vf25, vf25, Q
    vi02_pre_vtx_ptr = mem.ilw_data(vi03_vert_addr_book, 0);

    // skipped if we take the branch
    //  nop                        |  addw.w vf12, vf12, vf01
    //    if (m_clip_and_3ffff) {
    //      vars.vf12_root_pos_0.w() += m_tfrag_data.fog.w();
    //    }

    //////////////////  L129

    // Part 1 for X
    //  lq.xyzw vf15, 0(vi04)      |  madday.xyzw ACC, vf07, vf14
    vf15_vtx_pos_3 = mem.load_vector_data(vi04_vtx_ptr);
    // m_acc += in.vf07_cam_mat_y * vars.vf14_loop_pos_0.y();

    //  lq.xyzw vf23, 1(vi04)      |  maddz.xyzw vf14, vf08, vf14
    vf23_vtx_rgba_3 = mem.load_vector_data(vi04_vtx_ptr + 1);
    // vars.vf14_loop_pos_0 = m_acc + in.vf08_cam_mat_z * vars.vf14_loop_pos_0.z();

    //  sqi.xyz vf24, vi06         |  add.xyzw vf13, vf13, vf10
    // store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf24);
    vertex_pipeline[0].stq[0] = vf24_stq_0.x();
    vertex_pipeline[0].stq[1] = vf24_stq_0.y();
    vertex_pipeline[0].stq[2] = vf24_stq_0.z();
    vi06_kick_zone_ptr++;
    // vars.vf13_root_pos_1 += m_tfrag_data.hvdf_offset;

    //  sqi.xyzw vf20, vi06        |  ftoi4.xyzw vf12, vf12
    // store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf20);
    vertex_pipeline[0].rgba = vf20_vtx_rgba_0;
    // leaving out ftoi4
    vi06_kick_zone_ptr++;
    // vars.vf12_root_pos_0 = ftoi4(vars.vf12_root_pos_0);

    //  lq.xyw vf28, 0(vi02)       |  itof0.xyzw vf15, vf15
    if (vi02_pre_vtx_ptr < 328) {  // HACK added
      vf28_load_temp = mem.load_vector_data(vi02_pre_vtx_ptr);
      vf28_x = vf28_load_temp.x();
      vf28_y = vf28_load_temp.y();
      if (float_2_u32(vf28_load_temp.w()) < 328) {
        vf28_w_addr_of_next_vtx = vf28_load_temp.w();
      }
    }
    vf15_vtx_pos_3 = itof0(vf15_vtx_pos_3);

    //  div Q, vf01.x, vf14.w      |  mul.xyzw vf18, vf14, vf11
    // m_q = m_tfrag_data.fog.x() / vars.vf14_loop_pos_0.w();
    // vars.vf18_scaled_pos_2 = vars.vf14_loop_pos_0.elementwise_multiply(m_tfrag_data.hmge_scale);

    //  ibeq vi05, vi06, L133      |  miniz.w vf13, vf13, vf01
    bool take_branch = (vi05_end_of_vert_kick_data == vi06_kick_zone_ptr);
    //  fmt::print("L129 prog: {} {}\n", vars.vi05, vars.vi06_kick_zone_ptr);
    // vars.vf13_root_pos_1.w() = std::min(vars.vf13_root_pos_1.w(), m_tfrag_data.fog.z());

    //  sqi.xyzw vf12, vi06        |  clipw.xyz vf17, vf17
    vertex_pipeline[0].pre_cam_trans_pos = vf12_vtx_pos_0;  // todo move down?
    //  fmt::print("C: vf12 store: {}\n", int_vec_debug(vars.vf12_root_pos_0));
    current_draw.verts.push_back(vertex_pipeline[0]);
    vi06_kick_zone_ptr++;
    // m_clip_and_3ffff = clip_xyz_plus_minus(vars.vf17_scaled_pos_1);

    if (take_branch) {
      // kick zone is full, time for another kick
      if (emulate_kick_subroutine<DEBUG>(mem, current_draw, all_draws, vi05_end_of_vert_kick_data,
                                         vi06_kick_zone_ptr, vi07, vi08_adgif_base,
                                         vi09_draw_addr_book, vi10_start_of_vert_kick_data,
                                         vi12_vert_count, vi13_adgifs, vf24_w_u16)) {
        return;
      }
    }

    fmt::print("6a1 {} / {}\n", vi05_end_of_vert_kick_data, vi06_kick_zone_ptr);
    //////////////////////// L6A1

    // part 1 for 1
    //  nop                        |  subz.xyz vf24, vf28, vf02
    vf24_stq_0.x() = vf28_x - 2048.f;
    vf24_stq_0.y() = vf28_y - 2048.f;
    vf24_stq_0.z() = vf28_z - 2048.f;

    //  iaddi vi07, vi07, 0x1      |  mulaw.xyzw ACC, vf09, vf00
    vi07++;
    // m_acc = vars.vf09_cam_trans;

    //  mtir vi04, vf28.w          |  maxy.w vf13, vf13, vf01
    vi04_vtx_ptr = float_2_u32(vf28_w_addr_of_next_vtx);
    // vars.vf13_root_pos_1.w() = std::max(vars.vf13_root_pos_1.w(), m_tfrag_data.fog.y());

    //  fcand vi01, 0x3ffff        |  maddax.xyzw ACC, vf04, vf15
    // m_acc += in.vf04_cam_mat_x * vars.vf15_loop_pos_1.x();
    // fcand already calculated

    //  ibeq vi00, vi01, L130      |  mul.xyz vf14, vf14, Q
    //    vars.vf14_loop_pos_0.x() *= m_q;
    //    vars.vf14_loop_pos_0.y() *= m_q;
    //    vars.vf14_loop_pos_0.z() *= m_q;

    //  ilwr.y vi02, vi03          |  mul.xyz vf26, vf26, Q
    vi02_pre_vtx_ptr = mem.ilw_data(vi03_vert_addr_book, 1);
    //    vars.vf26.x() *= m_q;
    //    vars.vf26.y() *= m_q;
    //    vars.vf26.z() *= m_q;

    //  nop                        |  addw.w vf13, vf13, vf0
    //    if (m_clip_and_3ffff) {
    //      vars.vf13_root_pos_1.w() += m_tfrag_data.fog.w();
    //    }

    //////////////////////////////// L130

    //  lq.xyzw vf12, 0(vi04)      |  madday.xyzw ACC, vf07, vf15
    vf12_vtx_pos_0 = mem.load_vector_data(vi04_vtx_ptr);
    // m_acc += in.vf07_cam_mat_y * vars.vf15_loop_pos_1.y();

    //  lq.xyzw vf20, 1(vi04)      |  maddz.xyzw vf15, vf08, vf15
    vf20_vtx_rgba_0 = mem.load_vector_data(vi04_vtx_ptr + 1);
    // vars.vf15_loop_pos_1 = m_acc + in.vf08_cam_mat_z * vars.vf15_loop_pos_1.z();

    //  sqi.xyzw vf25, vi06        |  add.xyzw vf14, vf14, vf10
    // store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf25);
    vertex_pipeline[1].stq[0] = vf25_stq_1[0];
    vertex_pipeline[1].stq[1] = vf25_stq_1[1];
    vertex_pipeline[1].stq[2] = vf25_stq_1[2];
    //  fmt::print("A: vf25 store: {}\n", vars.vf25.to_string_aligned());
    vi06_kick_zone_ptr++;
    // vf14 += m_tfrag_data.hvdf_offset;

    //  sqi.xyzw vf21, vi06        |  ftoi4.xyzw vf13, vf13
    // store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf21);
    vertex_pipeline[1].rgba = vf21_vtx_unk_1;
    // fmt::print("B: vf21 store: {}\n", int_vec_debug(vars.vf21));
    vi06_kick_zone_ptr++;
    // vars.vf13_root_pos_1 = ftoi4(vars.vf13_root_pos_1);

    //  lq.xyw vf28, 0(vi02)       |  itof0.xyzw vf12, vf12
    if (vi02_pre_vtx_ptr < 328) {  // HACK added
      vf28_load_temp = mem.load_vector_data(vi02_pre_vtx_ptr);
      vf28_x = vf28_load_temp.x();
      vf28_y = vf28_load_temp.y();
      if (float_2_u32(vf28_load_temp.w()) < 328) {
        vf28_w_addr_of_next_vtx = vf28_load_temp.w();
      }
    }

    // vars.vf12_root_pos_0 = itof0(vars.vf12_root_pos_0);
    vf12_vtx_pos_0 = itof0(vf12_vtx_pos_0);

    //  div Q, vf01.x, vf15.w      |  mul.xyzw vf19, vf15, vf11
    // m_q = m_tfrag_data.fog.x() / vars.vf15_loop_pos_1.w();
    // vars.vf19_scaled_pos_3 = vars.vf15_loop_pos_1.elementwise_multiply(m_tfrag_data.hmge_scale);

    //  ibeq vi05, vi06, L134      |  miniz.w vf14, vf14, vf01
    take_branch = (vi05_end_of_vert_kick_data == vi06_kick_zone_ptr);
    // vars.vf14_loop_pos_0.w() = std::min(vars.vf14_loop_pos_0.w(), m_tfrag_data.fog.z());

    //  sqi.xyzw vf13, vi06        |  clipw.xyz vf18, vf18
    // store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf13_root_pos_1);
    vertex_pipeline[1].pre_cam_trans_pos = vf13_vtx_pos_1;
    current_draw.verts.push_back(vertex_pipeline[1]);
    //  fmt::print("C: vf13 store: {}\n", int_vec_debug(vars.vf13_root_pos_1));
    vi06_kick_zone_ptr++;
    // m_clip_and_3ffff = clip_xyz_plus_minus(vars.vf18_scaled_pos_2);

    if (take_branch) {
      // kick zone is full, time for another kick
      if (emulate_kick_subroutine<DEBUG>(mem, current_draw, all_draws, vi05_end_of_vert_kick_data,
                                         vi06_kick_zone_ptr, vi07, vi08_adgif_base,
                                         vi09_draw_addr_book, vi10_start_of_vert_kick_data,
                                         vi12_vert_count, vi13_adgifs, vf24_w_u16)) {
        return;
      }
    }
    fmt::print("6b0 {} / {}\n", vi05_end_of_vert_kick_data, vi06_kick_zone_ptr);

    /////////////////// L6B0
    //  nop                        |  subz.xyz vf25, vf28, vf02
    vf25_stq_1.x() = vf28_x - 2048.f;
    vf25_stq_1.y() = vf28_y - 2048.f;
    vf25_stq_1.z() = vf28_z - 2048.f;

    //  iaddi vi07, vi07, 0x1      |  mulaw.xyzw ACC, vf09, vf00
    vi07++;
    // m_acc = vars.vf09_cam_trans;

    //  mtir vi04, vf28.w          |  maxy.w vf14, vf14, vf01
    vi04_vtx_ptr = float_2_u32(vf28_w_addr_of_next_vtx);
    // vars.vf14_loop_pos_0.w() = std::max(vars.vf14_loop_pos_0.w(), m_tfrag_data.fog.y());

    //  fcand vi01, 0x3ffff        |  maddax.xyzw ACC, vf04, vf12
    // m_acc += in.vf04_cam_mat_x * vars.vf12_root_pos_0.x();
    // fcand already calculated

    //  ibeq vi00, vi01, L131      |  mul.xyz vf15, vf15, Q
    //    vars.vf15_loop_pos_1.x() *= m_q;
    //    vars.vf15_loop_pos_1.y() *= m_q;
    //    vars.vf15_loop_pos_1.z() *= m_q;

    //  ilwr.z vi02, vi03          |  mul.xyz vf27, vf27, Q
    vi02_pre_vtx_ptr = mem.ilw_data(vi03_vert_addr_book, 2);
    //    vars.vf27.x() *= m_q;
    //    vars.vf27.y() *= m_q;
    //    vars.vf27.z() *= m_q;

    //  nop                        |  addw.w vf14, vf14, vf01
    //    if (m_clip_and_3ffff) {
    //      vars.vf14_loop_pos_0.w() += m_tfrag_data.fog.w();
    //    }

    ///////////////////// L131
    //  lq.xyzw vf13, 0(vi04)      |  madday.xyzw ACC, vf07, vf12
    vf13_vtx_pos_1 = mem.load_vector_data(vi04_vtx_ptr);
    // m_acc += in.vf07_cam_mat_y * vars.vf12_root_pos_0.y();

    //  lq.xyzw vf21, 1(vi04)      |  maddz.xyzw vf12, vf08, vf12
    vf21_vtx_unk_1 = mem.load_vector_data(vi04_vtx_ptr + 1);
    //  fmt::print("vf21 load from: {}\n", vars.vi04 + 1);
    // vars.vf12_root_pos_0 = m_acc + in.vf08_cam_mat_z * vars.vf12_root_pos_0.z();

    //  sqi.xyzw vf26, vi06        |  add.xyzw vf15, vf15, vf10
    // store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf26);
    vertex_pipeline[2].stq[0] = vf26_stq_2[0];
    vertex_pipeline[2].stq[1] = vf26_stq_2[1];
    vertex_pipeline[2].stq[2] = vf26_stq_2[2];
    //  fmt::print("A: vf26 store: {}\n", vars.vf26.to_string_aligned());
    vi06_kick_zone_ptr++;
    // vars.vf15_loop_pos_1 += m_tfrag_data.hvdf_offset;

    //  sqi.xyzw vf22, vi06        |  ftoi4.xyzw vf14, vf14
    // store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf22);
    vertex_pipeline[2].rgba = vf22_vtx_rgba_2;
    // fmt::print("B: vf22 store: {}\n", int_vec_debug(vars.vf22));
    vi06_kick_zone_ptr++;
    // vars.vf14_loop_pos_0 = ftoi4(vars.vf14_loop_pos_0);

    //  lq.xyw vf28, 0(vi02)       |  itof0.xyzw vf13, vf13
    if (vi02_pre_vtx_ptr < 328) {  // HACK added
      vf28_load_temp = mem.load_vector_data(vi02_pre_vtx_ptr);
      vf28_x = vf28_load_temp.x();
      vf28_y = vf28_load_temp.y();
      if (float_2_u32(vf28_load_temp.w()) < 328) {
        vf28_w_addr_of_next_vtx = vf28_load_temp.w();
      }
    }
    vf13_vtx_pos_1 = itof0(vf13_vtx_pos_1);

    //  div Q, vf01.x, vf12.w      |  mul.xyzw vf16, vf12, vf11
    // m_q = m_tfrag_data.fog.x() / vars.vf12_root_pos_0.w();
    // vars.vf16_scaled_pos_0 = vars.vf12_root_pos_0.elementwise_multiply(m_tfrag_data.hmge_scale);

    //  ibeq vi05, vi06, L135      |  miniz.w vf15, vf15, vf01
    take_branch = (vi05_end_of_vert_kick_data == vi06_kick_zone_ptr);
    // vars.vf15_loop_pos_1.w() = std::min(vars.vf15_loop_pos_1.w(), m_tfrag_data.fog.z());

    //  sqi.xyzw vf14, vi06        |  clipw.xyz vf19, vf19
    // store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf14_loop_pos_0);
    vertex_pipeline[2].pre_cam_trans_pos = vf14_vtx_pos_2;
    current_draw.verts.push_back(vertex_pipeline[2]);

    //  fmt::print("C: vf14 store: {}\n", int_vec_debug(vars.vf14_loop_pos_0));
    vi06_kick_zone_ptr++;
    // m_clip_and_3ffff = clip_xyz_plus_minus(vars.vf19_scaled_pos_3);

    if (take_branch) {
      // kick zone is full, time for another kick
      if (emulate_kick_subroutine<DEBUG>(mem, current_draw, all_draws, vi05_end_of_vert_kick_data,
                                         vi06_kick_zone_ptr, vi07, vi08_adgif_base,
                                         vi09_draw_addr_book, vi10_start_of_vert_kick_data,
                                         vi12_vert_count, vi13_adgifs, vf24_w_u16)) {
        return;
      };
    }

    fmt::print("6bf {} / {}\n", vi05_end_of_vert_kick_data, vi06_kick_zone_ptr);

    ///////////////////// 6bf
    //  nop                        |  subz.xyz vf26, vf28, vf02
    vf26_stq_2.x() = vf28_x - 2048.f;
    vf26_stq_2.y() = vf28_y - 2048.f;
    vf26_stq_2.z() = vf28_z - 2048.f;

    //  iaddi vi07, vi07, 0x1      |  mulaw.xyzw ACC, vf09, vf00
    vi07++;
    // m_acc = vars.vf09_cam_trans;

    //  mtir vi04, vf28.w          |  maxy.w vf15, vf15, vf01
    vi04_vtx_ptr = float_2_u32(vf28_w_addr_of_next_vtx);  // L131 previously
    // assert(vars.vi04 != 0xbeef);             // hit
    // vars.vf15_loop_pos_1.w() = std::max(vars.vf15_loop_pos_1.w(), m_tfrag_data.fog.y());

    //  fcand vi01, 0x3ffff        |  maddax.xyzw ACC, vf04, vf13
    // m_acc += in.vf04_cam_mat_x * vars.vf13_root_pos_1.x();

    //  ibeq vi00, vi01, L132      |  mul.xyz vf12, vf12, Q
    //    vars.vf12_root_pos_0.x() *= m_q;
    //    vars.vf12_root_pos_0.y() *= m_q;
    //    vars.vf12_root_pos_0.z() *= m_q;

    //  ilwr.w vi02, vi03          |  mul.xyz vf24, vf24, Q
    vi02_pre_vtx_ptr = mem.ilw_data(vi03_vert_addr_book, 3);
    //    vars.vf24.x() *= m_q;
    //    vars.vf24.y() *= m_q;
    //    vars.vf24.z() *= m_q;

    //  nop                        |  addw.w vf15, vf15, vf01
    //    if (m_clip_and_3ffff) {
    //      vars.vf15_loop_pos_1.w() += m_tfrag_data.fog.w();
    //    }
    //
    ///////////////////////////////  L132
    //  lq.xyzw vf14, 0(vi04)      |  madday.xyzw ACC, vf07, vf13
    // vars.vf14_loop_pos_0 = load_vector_data(vars.vi04);  // bad here, in L0x6BF_PART0_W prev
    vf14_vtx_pos_2 = mem.load_vector_data(vi04_vtx_ptr);
    // m_acc += in.vf07_cam_mat_y * vars.vf13_root_pos_1.y();

    //  lq.xyzw vf22, 1(vi04)      |  maddz.xyzw vf13, vf08, vf13
    vf22_vtx_rgba_2 = mem.load_vector_data(vi04_vtx_ptr + 1);
    // vars.vf13_root_pos_1 = m_acc + in.vf08_cam_mat_z * vars.vf13_root_pos_1.z();

    //  sqi.xyzw vf27, vi06        |  add.xyzw vf12, vf12, vf10
    // store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf27);
    vertex_pipeline[3].stq[0] = vf27_vtx_stq_3[0];
    vertex_pipeline[3].stq[1] = vf27_vtx_stq_3[1];
    vertex_pipeline[3].stq[2] = vf27_vtx_stq_3[2];
    //  fmt::print("A: vf27 store: {}\n", vars.vf27.to_string_aligned());
    vi06_kick_zone_ptr++;
    // vars.vf12_root_pos_0 += m_tfrag_data.hvdf_offset;

    //  sqi.xyzw vf23, vi06        |  ftoi4.xyzw vf15, vf15
    // store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf23);
    vertex_pipeline[3].rgba = vf23_vtx_rgba_3;

    // fmt::print("B: vf23 store: {}\n", int_vec_debug(vars.vf23));
    vi06_kick_zone_ptr++;
    // vars.vf15_loop_pos_1 = ftoi4(vars.vf15_loop_pos_1);

    //  lq.xyw vf28, 0(vi02)       |  itof0.xyzw vf14, vf14
    if (vi02_pre_vtx_ptr < 328) {  // HACK added
      vf28_load_temp = mem.load_vector_data(vi02_pre_vtx_ptr);
      vf28_x = vf28_load_temp.x();
      vf28_y = vf28_load_temp.y();
      if (float_2_u32(vf28_load_temp.w()) < 328) {
        vf28_w_addr_of_next_vtx = vf28_load_temp.w();
      }
    }
    vf14_vtx_pos_2 = itof0(vf14_vtx_pos_2);

    //  div Q, vf01.x, vf13.w      |  mul.xyzw vf17, vf13, vf11
    // m_q = m_tfrag_data.fog.x() / vars.vf13_root_pos_1.w();
    // vars.vf17_scaled_pos_1 = vars.vf13_root_pos_1.elementwise_multiply(m_tfrag_data.hmge_scale);

    //  ibne vi05, vi06, L128      |  miniz.w vf12, vf12, vf01
    take_branch = (vi05_end_of_vert_kick_data != vi06_kick_zone_ptr);
    //  fmt::print("kick check: {} {}\n", vars.vi05, vars.vi06_kick_zone_ptr);
    // vars.vf12_root_pos_0.w() = std::min(vars.vf12_root_pos_0.w(), m_tfrag_data.fog.z());

    //  sqi.xyzw vf15, vi06        |  clipw.xyz vf16, vf16
    // store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf15_loop_pos_1);
    vertex_pipeline[3].pre_cam_trans_pos = vf15_vtx_pos_3;
    current_draw.verts.push_back(vertex_pipeline[3]);
    vi06_kick_zone_ptr++;
    //  fmt::print("C: vf15 store: {}\n", int_vec_debug(vars.vf15_loop_pos_1));
    // m_clip_and_3ffff = clip_xyz_plus_minus(vars.vf16_scaled_pos_0);

    if (!take_branch) {
      if (emulate_kick_subroutine<DEBUG>(mem, current_draw, all_draws, vi05_end_of_vert_kick_data,
                                         vi06_kick_zone_ptr, vi07, vi08_adgif_base,
                                         vi09_draw_addr_book, vi10_start_of_vert_kick_data,
                                         vi12_vert_count, vi13_adgifs, vf24_w_u16)) {
        return;
      }
    }

    //assert(false);
  }
}

void emulate_tfrags(const std::vector<level_tools::TFragment>& frags) {
  TFragExtractStats stats;

  std::vector<u8> vu_mem;
  vu_mem.resize(16 * 1024);

  for (auto& frag : frags) {
    TFragColorUnpack color_indices;
    emulate_dma_building_for_tfrag(frag, vu_mem, color_indices, &stats);
    VuMemWrapper mem(vu_mem);
    emulate_tfrag_execution<true>(frag, mem, color_indices, &stats);
  }
  fmt::print("l1: {}, l0: {}, base: {}\n", stats.num_l1, stats.num_l0, stats.num_base);
}
}  // namespace

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