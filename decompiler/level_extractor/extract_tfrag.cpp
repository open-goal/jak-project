#include "extract_tfrag.h"

#include "common/custom_data/pack_helpers.h"
#include "common/dma/dma.h"
#include "common/dma/gs.h"
#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"

#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "decompiler/util/Error.h"

namespace decompiler {
namespace {

static constexpr int GEOM_MAX = 3;

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
    ASSERT(false);
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
        lg::error("bad frag: exp {} got {}", start, elt.id);
        return false;
      }
      start++;
    }
    *end = start;
    return true;
  } else if (as_nodes) {
    for (auto& elt : as_nodes->draw_nodes) {
      if (elt.id != start) {
        lg::error("bad node: exp {} got {}", start, elt.id);
        return false;
      }
      start++;
    }
    *end = start;
    return true;
  } else {
    lg::error("bad node array type: {}", array->my_type());
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

  if (tree->arrays.size() == 0) {
  } else if (tree->arrays.size() == 1) {
    auto array =
        dynamic_cast<const level_tools::DrawableInlineArrayTFrag*>(tree->arrays.at(0).get());
    ASSERT(array);
    result.first_root = array->tfragments.at(0).id;
    result.num_roots = array->tfragments.size();
    result.only_children = true;
  } else {
    auto array =
        dynamic_cast<const level_tools::DrawableInlineArrayNode*>(tree->arrays.at(0).get());
    ASSERT(array);
    result.first_root = array->draw_nodes.at(0).id;
    result.num_roots = array->draw_nodes.size();
    result.only_children = false;
  }

  result.vis_nodes.resize(first_child - result.first_root);

  // may run 0 times, if there are only children.
  for (int i = 0; i < ((int)tree->arrays.size()) - 1; i++) {
    bool expecting_leaves = i == ((int)tree->arrays.size()) - 2;

    auto array =
        dynamic_cast<const level_tools::DrawableInlineArrayNode*>(tree->arrays.at(i).get());
    ASSERT(array);
    u16 idx = first_child;
    for (auto& elt : array->draw_nodes) {
      auto& vis = result.vis_nodes.at(elt.id - result.first_root);
      ASSERT(vis.num_kids == 0xff);
      for (int j = 0; j < 4; j++) {
        vis.bsphere[j] = elt.bsphere.data[j];
      }
      vis.num_kids = elt.child_count;
      vis.flags = elt.flags;
      vis.my_id = elt.id;
      ASSERT(vis.flags == expecting_leaves ? 0 : 1);
      ASSERT(vis.num_kids > 0);
      ASSERT(vis.num_kids <= 8);
      ASSERT(elt.children.size() == vis.num_kids);
      if (expecting_leaves) {
        for (int leaf = 0; leaf < (int)vis.num_kids; leaf++) {
          auto l = dynamic_cast<level_tools::TFragment*>(elt.children.at(leaf).get());
          ASSERT(l);

          ASSERT(idx == l->id);

          ASSERT(l->id >= result.first_child_node);
          if (leaf == 0) {
            vis.child_id = l->id;
          }
          result.last_child_node = std::max((u16)l->id, result.last_child_node);
          idx++;
        }

      } else {
        u16 arr_idx = 0;
        for (int child = 0; child < (int)vis.num_kids; child++) {
          auto l = dynamic_cast<level_tools::DrawNode*>(elt.children.at(child).get());
          ASSERT(l);
          if (child == 0) {
            arr_idx = l->id;
          } else {
            ASSERT(arr_idx < l->id);
            arr_idx = l->id;
          }
          if (child == 0) {
            vis.child_id = l->id;
          }

          ASSERT(l->id < result.first_child_node);
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
  ASSERT(unpack.use_tops_flag);
  int offset = offset_word * 4;

  // CL x (num/WL)+(num%WL)

  if (unpack.is_unsigned) {
    // note: formulas below assume this!
    ASSERT(cl == 2);
    ASSERT(wl == 1);
    ASSERT(code.num);
    for (int i = 0; i < code.num; i++) {
      // write every other qw
      int dest_qw = unpack.addr_qw + 2 * i;
      ASSERT(dest_qw <= 328);
      u32 qw[4];
      qw[0] = dma[offset++];
      qw[1] = dma[offset++];
      qw[2] = dma[offset++];
      qw[3] = dma[offset++];
      memcpy(out + (dest_qw * 16), qw, 16);
    }
  } else {
    // note: formulas below assume this!
    ASSERT(cl == 4);
    ASSERT(wl == 4);
    ASSERT(code.num);
    for (int i = 0; i < code.num; i++) {
      // write every other qw
      int dest_qw = unpack.addr_qw + i;
      ASSERT(dest_qw <= 328);
      s32 qw[4];
      qw[0] = (s8)dma[offset++];
      qw[1] = (s8)dma[offset++];
      qw[2] = (s8)dma[offset++];
      qw[3] = (s8)dma[offset++];
      memcpy(out + (dest_qw * 16), qw, 16);
    }
  }

  ASSERT((offset % 4) == 0);
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
  ASSERT(unpack.use_tops_flag);
  int offset = offset_word * 4;
  // CL x (num/WL)+(num%WL)

  if (unpack.is_unsigned) {
    // note: formulas below assume this!
    ASSERT(cl == 4);
    ASSERT(wl == 4);
    ASSERT(code.num);
    for (int i = 0; i < code.num; i++) {
      // write every other qw
      int dest_qw = unpack.addr_qw + i;
      ASSERT(dest_qw <= 328);
      u32 qw[4];
      qw[0] = row[0] + dma[offset++];
      qw[1] = row[1] + dma[offset++];
      qw[2] = row[2] + dma[offset++];
      qw[3] = row[3] + dma[offset++];
      memcpy(out + (dest_qw * 16), qw, 16);
    }
  } else {
    // note: formulas below assume this!
    ASSERT(cl == 4);
    ASSERT(wl == 4);
    ASSERT(code.num);
    for (int i = 0; i < code.num; i++) {
      // write every other qw
      int dest_qw = unpack.addr_qw + i;
      ASSERT(dest_qw <= 328);
      s32 qw[4];
      qw[0] = row[0] + (s8)dma[offset++];
      qw[1] = row[1] + (s8)dma[offset++];
      qw[2] = row[2] + (s8)dma[offset++];
      qw[3] = row[3] + (s8)dma[offset++];
      memcpy(out + (dest_qw * 16), qw, 16);
    }
  }

  ASSERT((offset % 4) == 0);
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
  ASSERT(unpack.use_tops_flag);
  ASSERT(unpack.is_unsigned);

  // note: formulas below assume this!
  ASSERT(cl == 4);
  ASSERT(wl == 4);

  int offset = offset_word * 4;

  ASSERT(code.num);
  for (int i = 0; i < code.num; i++) {
    // write every other qw
    int dest_qw = unpack.addr_qw + i;
    ASSERT(dest_qw <= 328);
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
  ASSERT((offset % 4) == 0);
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
  ASSERT(unpack.use_tops_flag);
  ASSERT(unpack.is_unsigned);

  // note: formulas below assume this!
  ASSERT(cl == 4);
  ASSERT(wl == 4);

  ASSERT(code.num);
  int offset = offset_word * 4;
  for (int i = 0; i < code.num; i++) {
    // write every other qw
    int dest_qw = unpack.addr_qw + i;
    ASSERT(dest_qw <= 328);
    u32 qw[4];
    qw[0] = row[0] + (u32)deref_ptr<u16>(dma + offset);
    offset += 2;
    qw[1] = row[1] + (u32)deref_ptr<u16>(dma + offset);
    offset += 2;
    qw[2] = row[2] + (u32)deref_ptr<u16>(dma + offset);
    offset += 2;
    qw[3] = row[3] + (u32)deref_ptr<u16>(dma + offset);
    offset += 2;

    // lg::print("  unpack rgba?: {:x} {:x} {:x} {:x}\n", qw[0], qw[1], qw[2], qw[3]);
    memcpy(vu_mem + (dest_qw * 16), qw, 16);
  }
  ASSERT((offset % 4) == 0);
  return offset / 4;
}

int handle_unpack_v3_32(const VifCode& code,
                        const u8* dma,
                        int offset_word,
                        int cl,
                        int wl,
                        u8* vu_mem) {
  VifCodeUnpack unpack(code);
  ASSERT(unpack.use_tops_flag);
  ASSERT(!unpack.is_unsigned);

  // note: formulas below assume this!
  ASSERT(cl == 2);
  ASSERT(wl == 1);

  ASSERT(code.num);
  int offset = offset_word * 4;
  for (int i = 0; i < code.num; i++) {
    // write every other qw
    int dest_qw = unpack.addr_qw + i * 2;
    ASSERT(dest_qw <= 328);
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
  ASSERT((offset % 4) == 0);
  return offset / 4;
}

int handle_unpack_v4_32(const VifCode& code,
                        const u8* dma,
                        int offset_word,
                        int cl,
                        int wl,
                        u8* vu_mem) {
  VifCodeUnpack unpack(code);
  ASSERT(unpack.use_tops_flag);
  ASSERT(!unpack.is_unsigned);

  // note: formulas below assume this!
  ASSERT(cl == 4);
  ASSERT(wl == 4);
  int offset = offset_word * 4;
  ASSERT(code.num);
  for (int i = 0; i < code.num; i++) {
    // write every other qw
    int dest_qw = unpack.addr_qw + i;
    ASSERT(dest_qw <= 328);
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
  ASSERT((offset % 4) == 0);
  return offset / 4;

  //  u8* write_base = get_upload_buffer();
  //  ASSERT(code.num + unpack.addr_qw <= 328);
  //  memcpy(write_base + (unpack.addr_qw * 16), dma.data + offset, code.num * 16);
  //  return offset + code.num * 16;
}

}  // namespace

void emulate_chain(UnpackState& state, u32 max_words, const u32* start, u8* vu_mem) {
  u32 word = 0;

  while (word < max_words) {
    VifCode code(start[word]);
    word++;
    // lg::print("{}\n", code.print());
    switch (code.kind) {
      case VifCode::Kind::STROW:
        state.row_init = true;
        memcpy(state.row, start + word, 16);
        word += 4;
        break;
      case VifCode::Kind::STMOD:
        if (state.stmod == 0) {
          ASSERT(code.immediate == 1);
        } else {
          ASSERT(state.stmod == 1);
          ASSERT(code.immediate == 0 || code.immediate == 1);  // kinda weird.
        }
        state.stmod = code.immediate;
        break;
      case VifCode::Kind::UNPACK_V4_8:
        if (state.stmod == 0) {
          word = handle_unpack_v4_8_mode0(code, (const u8*)start, word, state.cl, state.wl, vu_mem);
        } else if (state.stmod == 1) {
          ASSERT(state.row_init);
          word = handle_unpack_v4_8_mode1(code, (const u8*)start, word, state.cl, state.wl,
                                          state.row, vu_mem);
        } else {
          ASSERT(false);
        }
        break;
      case VifCode::Kind::UNPACK_V4_16:
        if (state.stmod == 0) {
          word =
              handle_unpack_v4_16_mode0(code, (const u8*)start, word, state.cl, state.wl, vu_mem);
        } else if (state.stmod == 1) {
          ASSERT(state.row_init);
          word = handle_unpack_v4_16_mode1(code, (const u8*)start, word, state.cl, state.wl,
                                           state.row, vu_mem);
        } else {
          ASSERT(false);
        }
        break;
      case VifCode::Kind::UNPACK_V4_32:
        ASSERT(state.stmod == 0);
        word = handle_unpack_v4_32(code, (const u8*)start, word, state.cl, state.wl, vu_mem);
        break;
      case VifCode::Kind::UNPACK_V3_32:
        ASSERT(state.stmod == 0);
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

  ASSERT(word == max_words);
  ASSERT(state.stmod == 0);
}

struct TFragColorUnpack {
  // std::vector<math::Vector<u16, 4>> data;
  std::vector<u16> indices;
  u32 unpack_qw_addr = 0;

  u16 load_color_idx(u32 qw) {
    if (qw < unpack_qw_addr) {
      return 0xffff;
    }
    int past = qw - unpack_qw_addr;
    past /= 2;
    if (past < (int)indices.size()) {
      return indices.at(past);
    } else {
      return 0xffff;
    }
  }
};

void emulate_dma_building_for_tfrag(int geom,
                                    const level_tools::TFragment& frag,
                                    std::vector<u8>& vu_mem,
                                    TFragColorUnpack& color_indices,
                                    TFragExtractStats* stats) {
  UnpackState state;
  // all the templates do this...
  state.wl = 4;
  state.cl = 4;

  // do the "canned" unpacks
  if (frag.num_level0_colors == 0 || geom == 2) {
    // we're using base
    // ASSERT(frag.num_level1_colors == 0);
    stats->num_base++;
    emulate_chain(state, frag.dma_qwc[1] * 4, (const u32*)frag.dma_base.data(), vu_mem.data());

  } else if (frag.num_level1_colors == 0 || geom == 1) {
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
  color_indices.indices = frag.color_indices;
  color_indices.unpack_qw_addr = frag.color_offset;
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

    ASSERT(offset < 328);
    ASSERT(offset >= 0);
    int mem_offset = (xyzw * 4) + (offset * 16);
    memcpy(&result, mem->data() + mem_offset, 2);
    return result;
  }

  math::Vector4f load_vector_data(int offset) {
    math::Vector4f result;
    // offset = offset & 0x3ff;  // not super happy with this...
    ASSERT(offset < 328);
    ASSERT(offset >= 0);
    memcpy(&result, mem->data() + (offset * 16), 16);
    return result;
  }
};

using math::Vector3f;
using math::Vector4f;

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

struct TFragVertexData {
  Vector4f pre_cam_trans_pos;
  Vector3f stq;  // stq?
  u16 rgba;      // unlike actual tfrag, these are still indices
  bool end_of_strip = false;

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

  u16 tpage = 0;
  u16 tex_in_page = 0;

  u16 tfrag_id = 0;

  DrawMode mode;

  u64 get_adgif_val(int adgif) {
    u64 result;
    memcpy(&result, adgif_data + (adgif * 16), 8);
    return result;
  }

  u64 get_adgif_upper(int adgif) {
    u64 result;
    memcpy(&result, adgif_data + (adgif * 16) + 8, 8);
    return result;
  }

  GsRegisterAddress get_adgif_addr(int adgif) {
    return (GsRegisterAddress)(u8)get_adgif_upper(adgif);
  }

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
  //  lg::print("VI09 now {}\n", vars.vi09);

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
      lg::debug("continue with this adgif, but new strgif. next {} verts (kick zone now {})",
                vi12_vert_count, vi06_kick_zone_ptr);
    }
    //    lg::print("didn't kick, vi12 now {}\n", vars.vi12);
    all_draws.push_back(current_draw);
    current_draw.verts.clear();
    return false;
  }

  //  L123:
  //  ilw.y vi01, -1(vi09)
  u16 vi01 = mem.ilw_data(vi09_draw_addr_book - 1, 1);  // ?
  //  ilw.z vi13, -1(vi09)
  vi13_adgifs = mem.ilw_data(vi09_draw_addr_book - 1, 2);  // load new adgif addr
  //  lg::print("VI09 loads: {} {}\n", m_ptrs.vi01, vars.vi13);
  //  ibeq vi00, vi12, L126
  //  ilwr.x vi14, vi10
  //  lg::print("val is {}: {}\n", vars.vi10, ilw_kick_zone(vars.vi10, 0));
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
      if (DEBUG) {
        lg::debug("done with adgifs but not packet, now moving on to another with {}",
                  (s16)vi12_vert_count);
      }
      //      lg::print("didn't kick 2, vi12 now {}\n", vars.vi12);
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
    //    lg::print("kick zone store: {}\n", vars.vi14);
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
    //    lg::print("did kick, vi12 now {}\n", vars.vi12);
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
  //  lg::print("before add: {}\n", vars.vi14);
  //  vars.vi14 += vars.vi11;
  //  iswr.x vi14, vi10
  //  lg::print("kick zone store: {}\n", vars.vi14);
  //  store_u32_kick_zone(vars.vi14, vars.vi10, 0);
  //  lq.xyzw vf04, 664(vi00)
  // todo don't think I needed that load of ambient
  //  XGKICK<DEBUG>(m_ptrs.vi01, render_state, prof);
  //  xgkick vi01
  //  nop                        |  nop :e
  return true;
}

template <bool DEBUG>
std::vector<TFragDraw> emulate_tfrag_execution(const level_tools::TFragment& frag,
                                               VuMemWrapper& mem,
                                               TFragColorUnpack& color_indices,
                                               TFragExtractStats* /*stats*/) {
  // lg::print("tfrag exec. offset of colors = {}\n", color_indices.unpack_qw_addr);
  std::vector<TFragDraw> all_draws;
  TFragDraw current_draw;

  TFragVertexData vertex_pipeline[4];

  u16 vi14 = 0;

  float vf28_z = 2049;

  //  ilw.w vi08, 4(vi14)        |  nop
  u16 vi08_adgif_base = mem.ilw_data(4 + vi14, 3);  // is an address, v4/32 unpack.
  //  lg::print("------------- VI08 init: {}\n", vars.vi08);
  //  ilw.z vi09, 4(vi14)        |  nop
  u16 vi09_draw_addr_book =
      mem.ilw_data(4 + vi14, 2);  // is an input address, v4/8 unpack (seems small?)
  //  ilw.y vi03, 3(vi14)        |  nop
  u16 vi03_vert_addr_book = mem.ilw_data(
      3 + vi14,
      1);  // is an input address (v4-8 with strow). a list of addresses for v4-16's with strow

  //  lg::print("-------VI03 init: {}\n", vars.vi03);

  if (DEBUG) {
    // small, like 9, 54, 66
    level_tools::PrintSettings settings;
    settings.print_tfrag = true;
    lg::debug("{}", frag.print(settings, 0));
    lg::debug("ints: {} {} {}", vi08_adgif_base, vi09_draw_addr_book, vi03_vert_addr_book);
  }

  // lg::print("vi09: #x{:x} ({})\n", vars.vi09, vars.vi14);

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
  ASSERT(vi03_vert_addr_book < 328);                            // should be a buffer 0 addr
  u16 vi02_pre_vtx_ptr = mem.ilw_data(vi03_vert_addr_book, 0);  // is an addr? v4/16 with strom
  if (DEBUG) {
    lg::debug("vi02-warmup 0: {}", vi02_pre_vtx_ptr);
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
    lg::debug("vf28 load 0: x_f {} y_f {} z_u32 {}", vf28_x, vf28_y,
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
    lg::debug("vi12: 0x{:x}", vi12_vert_count);
  }

  //  ilwr.z vi13, vi09          |  nop
  u16 vi13_adgifs = mem.ilw_data(vi09_draw_addr_book, 2);
  if (DEBUG) {
    lg::debug("vi13: 0x{:x}", vi13_adgifs);
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
  [[maybe_unused]] u16 vi11 = 0x4000;

  //  iaddiu vi11, vi11, 0x4000  |  nop
  vi11 += 0x4000;

  //  ilwr.y vi02, vi03          |  nop
  vi02_pre_vtx_ptr = mem.ilw_data(vi03_vert_addr_book, 1);
  if (DEBUG) {
    lg::debug("vi02-warmup 1: {}", vi02_pre_vtx_ptr);
  }

  // vertex load
  //  lq.xyzw vf12, 0(vi04)      |  nop
  // integer vertex position
  Vector4f vf12_vtx_pos_0 = mem.load_vector_data(vi04_vtx_ptr);

  //  lq.xyzw vf20, 1(vi04)      |  nop
  // ??? something with the vertex.
  // Vector4f vf20_vtx_rgba_0 = mem.load_vector_data(vi04_vtx_ptr + 1);
  u16 vf20_vtx_rgba_0 = color_indices.load_color_idx(vi04_vtx_ptr + 1);

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
    lg::debug("vf28 load 0: x_f {} y_f {} w_u32 {}", vf28_x, vf28_y,
              float_2_u32(vf28_w_addr_of_next_vtx));
    lg::debug("vtx w0: {}", vf12_vtx_pos_0.to_string_aligned());
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
    lg::debug("pre-vtx-vi02-warmup 2: {}", vi02_pre_vtx_ptr);
  }

  //  lq.xyzw vf13, 0(vi04)      |  madday.xyzw ACC, vf07, vf12
  Vector4f vf13_vtx_pos_1 = mem.load_vector_data(vi04_vtx_ptr);
  // acc += in.vf07_cam_mat_y * vars.vf12_root_pos_0.y();

  //  lq.xyzw vf21, 1(vi04)      |  maddz.xyzw vf12, vf08, vf12
  // Vector4f vf21_vtx_unk_1 = mem.load_vector_data(vi04_vtx_ptr + 1);
  u16 vf21_vtx_rgba_1 = color_indices.load_color_idx(vi04_vtx_ptr + 1);

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

  //  ior vi10, vi06, vi00       |  mul.xyz vf12, vf12, Q
  u16 vi10_start_of_vert_kick_data = vi06_kick_zone_ptr;
  //  vars.vf12_root_pos_0.x() *= q;
  //  vars.vf12_root_pos_0.y() *= q;
  //  vars.vf12_root_pos_0.z() *= q;

  //  ilwr.w vi02, vi03          |  mul.xyz vf24, vf24, Q
  vi02_pre_vtx_ptr = mem.ilw_data(vi03_vert_addr_book, 3);
  if (DEBUG) {
    lg::debug("pre-vtx-vi02-warmup 3: {}", vi02_pre_vtx_ptr);
  }

  //  lq.xyzw vf14, 0(vi04)      |  madday.xyzw ACC, vf07, vf13
  Vector4f vf14_vtx_pos_2 = mem.load_vector_data(vi04_vtx_ptr);

  //  lq.xyzw vf22, 1(vi04)      |  maddz.xyzw vf13, vf08, vf13
  // Vector4f vf22_vtx_rgba_2 = mem.load_vector_data(vi04_vtx_ptr + 1);
  u16 vf22_vtx_rgba_2 = color_indices.load_color_idx(vi04_vtx_ptr + 1);

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
  // m_clip_and_3ffff = clip_xyz_plus_minus(vars.vf16_scaled_pos_0);

  Vector4f vf27_vtx_stq_3;
  u16 vf23_vtx_rgba_3;
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
    vf23_vtx_rgba_3 = color_indices.load_color_idx(vi04_vtx_ptr + 1);
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
    //  lg::print("L129 prog: {} {}\n", vars.vi05, vars.vi06_kick_zone_ptr);
    // vars.vf13_root_pos_1.w() = std::min(vars.vf13_root_pos_1.w(), m_tfrag_data.fog.z());

    //  sqi.xyzw vf12, vi06        |  clipw.xyz vf17, vf17
    vertex_pipeline[0].pre_cam_trans_pos = vf12_vtx_pos_0;  // todo move down?
    //  lg::print("C: vf12 store: {}\n", int_vec_debug(vars.vf12_root_pos_0));
    current_draw.verts.push_back(vertex_pipeline[0]);
    vi06_kick_zone_ptr++;
    // m_clip_and_3ffff = clip_xyz_plus_minus(vars.vf17_scaled_pos_1);

    if (take_branch) {
      // kick zone is full, time for another kick
      if (emulate_kick_subroutine<DEBUG>(mem, current_draw, all_draws, vi05_end_of_vert_kick_data,
                                         vi06_kick_zone_ptr, vi07, vi08_adgif_base,
                                         vi09_draw_addr_book, vi10_start_of_vert_kick_data,
                                         vi12_vert_count, vi13_adgifs, vf24_w_u16)) {
        goto end;
      }
    }

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
    vf20_vtx_rgba_0 = color_indices.load_color_idx(vi04_vtx_ptr + 1);
    // vars.vf15_loop_pos_1 = m_acc + in.vf08_cam_mat_z * vars.vf15_loop_pos_1.z();

    //  sqi.xyzw vf25, vi06        |  add.xyzw vf14, vf14, vf10
    // store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf25);
    vertex_pipeline[1].stq[0] = vf25_stq_1[0];
    vertex_pipeline[1].stq[1] = vf25_stq_1[1];
    vertex_pipeline[1].stq[2] = vf25_stq_1[2];
    //  lg::print("A: vf25 store: {}\n", vars.vf25.to_string_aligned());
    vi06_kick_zone_ptr++;
    // vf14 += m_tfrag_data.hvdf_offset;

    //  sqi.xyzw vf21, vi06        |  ftoi4.xyzw vf13, vf13
    // store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf21);
    vertex_pipeline[1].rgba = vf21_vtx_rgba_1;
    // lg::print("B: vf21 store: {}\n", int_vec_debug(vars.vf21));
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
    //  lg::print("C: vf13 store: {}\n", int_vec_debug(vars.vf13_root_pos_1));
    vi06_kick_zone_ptr++;
    // m_clip_and_3ffff = clip_xyz_plus_minus(vars.vf18_scaled_pos_2);

    if (take_branch) {
      // kick zone is full, time for another kick
      if (emulate_kick_subroutine<DEBUG>(mem, current_draw, all_draws, vi05_end_of_vert_kick_data,
                                         vi06_kick_zone_ptr, vi07, vi08_adgif_base,
                                         vi09_draw_addr_book, vi10_start_of_vert_kick_data,
                                         vi12_vert_count, vi13_adgifs, vf24_w_u16)) {
        goto end;
      }
    }

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
    vf21_vtx_rgba_1 = color_indices.load_color_idx(vi04_vtx_ptr + 1);
    //  lg::print("vf21 load from: {}\n", vars.vi04 + 1);
    // vars.vf12_root_pos_0 = m_acc + in.vf08_cam_mat_z * vars.vf12_root_pos_0.z();

    //  sqi.xyzw vf26, vi06        |  add.xyzw vf15, vf15, vf10
    // store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf26);
    vertex_pipeline[2].stq[0] = vf26_stq_2[0];
    vertex_pipeline[2].stq[1] = vf26_stq_2[1];
    vertex_pipeline[2].stq[2] = vf26_stq_2[2];
    //  lg::print("A: vf26 store: {}\n", vars.vf26.to_string_aligned());
    vi06_kick_zone_ptr++;
    // vars.vf15_loop_pos_1 += m_tfrag_data.hvdf_offset;

    //  sqi.xyzw vf22, vi06        |  ftoi4.xyzw vf14, vf14
    // store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf22);
    vertex_pipeline[2].rgba = vf22_vtx_rgba_2;
    // lg::print("B: vf22 store: {}\n", int_vec_debug(vars.vf22));
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

    //  lg::print("C: vf14 store: {}\n", int_vec_debug(vars.vf14_loop_pos_0));
    vi06_kick_zone_ptr++;
    // m_clip_and_3ffff = clip_xyz_plus_minus(vars.vf19_scaled_pos_3);

    if (take_branch) {
      // kick zone is full, time for another kick
      if (emulate_kick_subroutine<DEBUG>(mem, current_draw, all_draws, vi05_end_of_vert_kick_data,
                                         vi06_kick_zone_ptr, vi07, vi08_adgif_base,
                                         vi09_draw_addr_book, vi10_start_of_vert_kick_data,
                                         vi12_vert_count, vi13_adgifs, vf24_w_u16)) {
        goto end;
      };
    }

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
    // ASSERT(vars.vi04 != 0xbeef);             // hit
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
    vf22_vtx_rgba_2 = color_indices.load_color_idx(vi04_vtx_ptr + 1);
    // vars.vf13_root_pos_1 = m_acc + in.vf08_cam_mat_z * vars.vf13_root_pos_1.z();

    //  sqi.xyzw vf27, vi06        |  add.xyzw vf12, vf12, vf10
    // store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf27);
    vertex_pipeline[3].stq[0] = vf27_vtx_stq_3[0];
    vertex_pipeline[3].stq[1] = vf27_vtx_stq_3[1];
    vertex_pipeline[3].stq[2] = vf27_vtx_stq_3[2];
    //  lg::print("A: vf27 store: {}\n", vars.vf27.to_string_aligned());
    vi06_kick_zone_ptr++;
    // vars.vf12_root_pos_0 += m_tfrag_data.hvdf_offset;

    //  sqi.xyzw vf23, vi06        |  ftoi4.xyzw vf15, vf15
    // store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf23);
    vertex_pipeline[3].rgba = vf23_vtx_rgba_3;

    // lg::print("B: vf23 store: {}\n", int_vec_debug(vars.vf23));
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
    //  lg::print("kick check: {} {}\n", vars.vi05, vars.vi06_kick_zone_ptr);
    // vars.vf12_root_pos_0.w() = std::min(vars.vf12_root_pos_0.w(), m_tfrag_data.fog.z());

    //  sqi.xyzw vf15, vi06        |  clipw.xyz vf16, vf16
    // store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf15_loop_pos_1);
    vertex_pipeline[3].pre_cam_trans_pos = vf15_vtx_pos_3;
    current_draw.verts.push_back(vertex_pipeline[3]);
    vi06_kick_zone_ptr++;
    //  lg::print("C: vf15 store: {}\n", int_vec_debug(vars.vf15_loop_pos_1));
    // m_clip_and_3ffff = clip_xyz_plus_minus(vars.vf16_scaled_pos_0);

    if (!take_branch) {
      if (emulate_kick_subroutine<DEBUG>(mem, current_draw, all_draws, vi05_end_of_vert_kick_data,
                                         vi06_kick_zone_ptr, vi07, vi08_adgif_base,
                                         vi09_draw_addr_book, vi10_start_of_vert_kick_data,
                                         vi12_vert_count, vi13_adgifs, vf24_w_u16)) {
        goto end;
      }
    }

    // ASSERT(false);
  }

end:
  [[maybe_unused]] int total_dvert = 0;
  for (auto& draw : all_draws) {
    total_dvert += draw.verts.size();
    draw.tfrag_id = frag.id;
  }

  return all_draws;
}

std::string debug_dump_to_obj(const std::vector<TFragDraw>& draws) {
  std::vector<Vector4f> verts;
  std::vector<math::Vector<float, 2>> tcs;
  std::vector<math::Vector<int, 3>> faces;

  for (auto& draw : draws) {
    // add verts...
    ASSERT(draw.verts.size() >= 3);

    int vert_idx = 0;

    int vtx_idx_queue[3];

    int q_idx = 0;
    int startup = 0;
    while (vert_idx < (int)draw.verts.size()) {
      verts.push_back(draw.verts.at(vert_idx).pre_cam_trans_pos / 65536);
      tcs.push_back(
          math::Vector<float, 2>{draw.verts.at(vert_idx).stq.x(), draw.verts.at(vert_idx).stq.y()});
      vert_idx++;
      vtx_idx_queue[q_idx++] = verts.size();

      // wrap the index
      if (q_idx == 3) {
        q_idx = 0;
      }

      // bump the startup
      if (startup < 3) {
        startup++;
      }

      if (startup >= 3) {
        faces.push_back(math::Vector<int, 3>{vtx_idx_queue[0], vtx_idx_queue[1], vtx_idx_queue[2]});
      }
    }
  }

  std::string result;
  for (auto& vert : verts) {
    result += fmt::format("v {} {} {}\n", vert.x(), vert.y(), vert.z());
  }
  for (auto& tc : tcs) {
    result += fmt::format("vt {} {}\n", tc.x(), tc.y());
  }
  for (auto& face : faces) {
    result += fmt::format("f {}/{} {}/{} {}/{}\n", face.x(), face.x(), face.y(), face.y(), face.z(),
                          face.z());
  }

  return result;
}

void update_mode_from_alpha1(u64 val, DrawMode& mode) {
  GsAlpha reg(val);
  if (reg.a_mode() == GsAlpha::BlendMode::SOURCE && reg.b_mode() == GsAlpha::BlendMode::DEST &&
      reg.c_mode() == GsAlpha::BlendMode::SOURCE && reg.d_mode() == GsAlpha::BlendMode::DEST) {
    // (Cs - Cd) * As + Cd
    // Cs * As  + (1 - As) * Cd
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_SRC_DST);

  } else if (reg.a_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.b_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED &&
             reg.c_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.d_mode() == GsAlpha::BlendMode::DEST) {
    // (Cs - 0) * As + Cd
    // Cs * As + (1) * CD
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_0_SRC_DST);
  } else if (reg.a_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.b_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED &&
             reg.c_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED &&
             reg.d_mode() == GsAlpha::BlendMode::DEST) {
    ASSERT(reg.fix() == 128);
    // Cv = (Cs - 0) * FIX + Cd
    // if fix = 128, it works out to 1.0
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_0_FIX_DST);
    // src plus dest
  } else {
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_SRC_DST);
    ASSERT_MSG(false, fmt::format("unsupported blend: a {} b {} c {} d {}", (int)reg.a_mode(),
                                  (int)reg.b_mode(), (int)reg.c_mode(), (int)reg.d_mode()));
  }
}

void update_mode_from_test1(u64 val, DrawMode& mode) {
  // ate, atst, aref, afail, date, datm, zte, ztest
  GsTest test(val);

  // ATE
  mode.set_at(test.alpha_test_enable());

  // ATST
  switch (test.alpha_test()) {
    case GsTest::AlphaTest::ALWAYS:
      mode.set_alpha_test(DrawMode::AlphaTest::ALWAYS);
      break;
    case GsTest::AlphaTest::GEQUAL:
      mode.set_alpha_test(DrawMode::AlphaTest::GEQUAL);
      break;
    case GsTest::AlphaTest::NEVER:
      mode.set_alpha_test(DrawMode::AlphaTest::NEVER);
      break;
    default:
      mode.set_alpha_test(DrawMode::AlphaTest::ALWAYS);
      ASSERT_MSG(false, fmt::format("Alpha test: {} not supported", (int)test.alpha_test()));
  }

  // AREF
  mode.set_aref(test.aref());

  // AFAIL
  mode.set_alpha_fail(test.afail());

  // DATE
  ASSERT(test.date() == false);

  // DATM
  // who cares, if date is off

  // ZTE
  mode.set_zt(test.zte());

  // ZTST
  mode.set_depth_test(test.ztest());
}

u32 remap_texture(u32 original, const std::vector<level_tools::TextureRemap>& map) {
  auto masked = original & 0xffffff00;
  for (auto& t : map) {
    if (t.original_texid == masked) {
      lg::info("OKAY! remapped!");
      return t.new_texid | 20;
    }
  }
  return original;
}

void process_draw_mode(std::vector<TFragDraw>& all_draws,
                       const std::vector<level_tools::TextureRemap>& map,
                       tfrag3::TFragmentTreeKind tree_kind,
                       bool disable_atest_for_normal_tfrag) {
  // set up the draw mode based on the code in background.gc and tfrag-methods.gc
  DrawMode mode;
  mode.set_alpha_test(DrawMode::AlphaTest::GEQUAL);
  mode.enable_depth_write();

  mode.enable_at();
  mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_SRC_DST);

  // note: includes the test reg, and also the str-gif template prim abe.
  switch (tree_kind) {
    case tfrag3::TFragmentTreeKind::NORMAL:
    case tfrag3::TFragmentTreeKind::LOWRES:
      if (disable_atest_for_normal_tfrag) {
        mode.enable_at();                                  // :ate #x1
        mode.set_alpha_test(DrawMode::AlphaTest::ALWAYS);  // :atst (gs-atest greater-equal)
        mode.set_aref(0x0);                                // :aref #x26
        mode.set_alpha_fail(GsTest::AlphaFail::KEEP);
        mode.enable_zt();                            // :zte #x1
        mode.set_depth_test(GsTest::ZTest::GEQUAL);  // :ztst (gs-ztest greater-equal))
        mode.disable_ab();
      } else {
        mode.enable_at();                                  // :ate #x1
        mode.set_alpha_test(DrawMode::AlphaTest::GEQUAL);  // :atst (gs-atest greater-equal)
        mode.set_aref(0x26);                               // :aref #x26
        mode.set_alpha_fail(GsTest::AlphaFail::KEEP);
        mode.enable_zt();                            // :zte #x1
        mode.set_depth_test(GsTest::ZTest::GEQUAL);  // :ztst (gs-ztest greater-equal))
        mode.disable_ab();
      }

      break;
    case tfrag3::TFragmentTreeKind::TRANS:
    case tfrag3::TFragmentTreeKind::LOWRES_TRANS:
      mode.enable_at();                                  // :ate #x1
      mode.set_alpha_test(DrawMode::AlphaTest::GEQUAL);  // :atst (gs-atest greater-equal)
      mode.set_aref(0x7e);                               // :aref #x7e
      mode.set_alpha_fail(GsTest::AlphaFail::FB_ONLY);
      mode.enable_zt();                            // :zte #x1
      mode.set_depth_test(GsTest::ZTest::GEQUAL);  // :ztst (gs-ztest greater-equal))
      mode.enable_ab();
      break;
    case tfrag3::TFragmentTreeKind::DIRT:
      // (new 'static 'gs-test :ate #x1 :afail #x1 :zte #x1 :ztst (gs-ztest greater-equal))
      mode.enable_at();
      mode.set_alpha_test(DrawMode::AlphaTest::NEVER);
      mode.set_alpha_fail(GsTest::AlphaFail::FB_ONLY);
      mode.set_aref(0);
      mode.enable_zt();
      mode.set_depth_test(GsTest::ZTest::GEQUAL);
      mode.enable_ab();
      break;
    case tfrag3::TFragmentTreeKind::ICE:
      mode.enable_at();                                  // :ate #x1
      mode.set_alpha_test(DrawMode::AlphaTest::ALWAYS);  // :atst (gs-atest always)
      mode.set_alpha_fail(GsTest::AlphaFail::FB_ONLY);   // :afail #x1
      mode.set_aref(0);
      mode.enable_zt();                            // :zte #x1
      mode.set_depth_test(GsTest::ZTest::GEQUAL);  // :ztst (gs-ztest greater-equal)
      mode.enable_ab();
      break;
    case tfrag3::TFragmentTreeKind::WATER:
      // (new 'static 'gs-test :ate #x1 :afail #x1 :zte #x1 :ztst (gs-ztest greater-equal))
      mode.enable_at();
      mode.set_alpha_test(DrawMode::AlphaTest::NEVER);
      mode.set_alpha_fail(GsTest::AlphaFail::FB_ONLY);
      mode.set_aref(0);
      mode.enable_zt();
      mode.set_depth_test(GsTest::ZTest::GEQUAL);
      mode.enable_ab();
      break;
    default:
      ASSERT(false);
  }

  for (auto& draw : all_draws) {
    for (int ad_idx = 0; ad_idx < 5; ad_idx++) {
      auto addr = draw.get_adgif_addr(ad_idx);
      u64 val = draw.get_adgif_val(ad_idx);
      switch (addr) {
        case GsRegisterAddress::TEST_1:
          ASSERT(false);
          update_mode_from_test1(val, mode);
          break;
        case GsRegisterAddress::TEX0_1:
          ASSERT(val == 0 || val == 0x8'0000'0000);
          if (val == 0x8'0000'0000) {
            mode.set_decal(true);
          } else {
            mode.set_decal(false);
          }
          mode.set_tcc(false);
          break;
        case GsRegisterAddress::TEX1_1:
          ASSERT(val == 0x120);  // some flag
          {
            u32 original_tex = draw.get_adgif_upper(ad_idx);
            u32 new_tex = remap_texture(original_tex, map);
            if (original_tex != new_tex) {
              lg::info("map from 0x{:x} to 0x{:x}", original_tex, new_tex);
            }
            u32 tpage = new_tex >> 20;
            u32 tidx = (new_tex >> 8) & 0b1111'1111'1111;
            // lg::print("texture: {} : {}\n", tpage, tidx);
            draw.tpage = tpage;
            draw.tex_in_page = tidx;
          }

          break;
        case GsRegisterAddress::MIPTBP1_1:
          break;
        case GsRegisterAddress::CLAMP_1:
          if (!(val == 0b101 || val == 0 || val == 1 || val == 0b100)) {
            ASSERT_MSG(false, fmt::format("clamp: 0x{:x}", val));
          }

          mode.set_clamp_s_enable(val & 0b1);
          mode.set_clamp_t_enable(val & 0b100);
          break;
        case GsRegisterAddress::ALPHA_1:
          update_mode_from_alpha1(val, mode);
          break;
        default:
          lg::info("Address {} ({}) is not supported in process_draw_mode",
                   register_address_name(addr), ad_idx);
      }
    }
    draw.mode = mode;
  }
}

struct TFragStrip {
  std::vector<TFragVertexData> verts;
  u16 tfrag_id = 0;
};

struct GroupedDraw {
  DrawMode mode;
  u16 tpage;
  u16 tex_id;
  std::vector<TFragStrip> strips;
};

std::map<u32, std::vector<GroupedDraw>> make_draw_groups(std::vector<TFragDraw>& all_draws) {
  std::map<u32, std::vector<GroupedDraw>> result;

  for (auto& draw : all_draws) {
    u32 tex_combo = (((u32)draw.tpage) << 16) | draw.tex_in_page;
    auto& group_list = result[tex_combo];

    bool added = false;
    for (auto& existing_group : group_list) {
      if (draw.mode == existing_group.mode) {
        existing_group.strips.push_back({draw.verts, draw.tfrag_id});
        added = true;
        break;
      }
    }
    if (!added) {
      GroupedDraw new_group;
      new_group.mode = draw.mode;
      new_group.tpage = draw.tpage;
      new_group.tex_id = draw.tex_in_page;
      new_group.strips.push_back({draw.verts, draw.tfrag_id});
      group_list.push_back(new_group);
    }
  }

  [[maybe_unused]] int dc = 0;
  for (auto& group_list : result) {
    for (auto& group : group_list.second) {
      (void)group;
      dc++;
    }
  }

  // lg::print("    grouped to get {} draw calls\n", dc);

  return result;
}

s32 find_or_add_texture_to_level(u32 combo_tex_id,
                                 std::vector<tfrag3::Texture>& texture_pool,
                                 const TextureDB& tdb,
                                 const std::vector<std::pair<int, int>>& expected_missing_textures,
                                 const std::string& level_name) {
  // first, let's see if we have a texture for this.
  s32 tfrag3_tex_id = INT32_MAX;
  for (u32 i = 0; i < texture_pool.size(); i++) {
    if (texture_pool[i].combo_id == combo_tex_id) {
      tfrag3_tex_id = i;
      break;
    }
  }

  if (tfrag3_tex_id == INT32_MAX) {
    // nope. we are a new texture.
    auto tex_it = tdb.textures.find(combo_tex_id);
    if (tex_it == tdb.textures.end()) {
      int tpage = combo_tex_id >> 16;
      int idx = combo_tex_id & 0xffff;
      bool ok_to_miss =
          std::find(expected_missing_textures.begin(), expected_missing_textures.end(),
                    std::make_pair(tpage, idx)) != expected_missing_textures.end();
      if (ok_to_miss) {
        // we're missing a texture, just use the first one.
        tex_it = tdb.textures.begin();
      } else {
        ASSERT_MSG(
            false,
            fmt::format("texture {} wasn't found. make sure it is loaded somehow. You may need "
                        "to include "
                        "ART.DGO or GAME.DGO in addition to the level DGOs for shared textures."
                        "tpage is {}. id is {} (0x{:x}) for level {}",
                        combo_tex_id, combo_tex_id >> 16, combo_tex_id & 0xffff,
                        combo_tex_id & 0xffff, level_name));
      }
    }
    tfrag3_tex_id = texture_pool.size();
    texture_pool.emplace_back();
    auto& new_tex = texture_pool.back();
    new_tex.combo_id = combo_tex_id;
    new_tex.w = tex_it->second.w;
    new_tex.h = tex_it->second.h;
    new_tex.debug_name = tex_it->second.name;
    new_tex.debug_tpage_name = tdb.tpage_names.at(tex_it->second.page);
    new_tex.data = tex_it->second.rgba_bytes;
  }

  // map animated textures to the animation slot.
  const auto& level_tex = texture_pool.at(tfrag3_tex_id);
  const auto& it = tdb.animated_tex_output_to_anim_slot.find(level_tex.debug_name);
  if (it != tdb.animated_tex_output_to_anim_slot.end()) {
    // lg::warn("tfrag3 animated texture: {}", level_tex.debug_name);
    return -int(it->second) - 1;
  }

  return tfrag3_tex_id;
}

void make_tfrag3_data(std::map<u32, std::vector<GroupedDraw>>& draws,
                      tfrag3::TfragTree& tree_out,
                      std::vector<tfrag3::PreloadedVertex>& vertices,
                      std::vector<tfrag3::Texture>& texture_pool,
                      const TextureDB& tdb,
                      const std::vector<std::pair<int, int>>& expected_missing_textures,
                      const std::string& level_name) {
  // we will set:
  // draws
  // color_indices_per_vertex
  // and link textures.

  for (auto& [combo_tex_id, draw_list] : draws) {
    s32 tfrag3_tex_id = find_or_add_texture_to_level(combo_tex_id, texture_pool, tdb,
                                                     expected_missing_textures, level_name);
    // now, add draws
    for (auto& draw : draw_list) {
      tfrag3::StripDraw tdraw;
      tdraw.mode = draw.mode;
      tdraw.tree_tex_id = tfrag3_tex_id;

      for (auto& strip : draw.strips) {
        tfrag3::StripDraw::VisGroup vgroup;
        ASSERT(strip.tfrag_id < UINT16_MAX);
        vgroup.vis_idx_in_pc_bvh = strip.tfrag_id;  // associate with the tfrag for culling
        vgroup.num_inds = strip.verts.size() + 1;   // one for the primitive restart!
        vgroup.num_tris = strip.verts.size() - 2;

        tdraw.num_triangles += strip.verts.size() - 2;
        tfrag3::StripDraw::VertexRun run;
        run.vertex0 = vertices.size();
        run.length = strip.verts.size();
        for (auto& vert : strip.verts) {
          // convert vert.
          tfrag3::PreloadedVertex vtx;
          vtx.x = vert.pre_cam_trans_pos.x();
          vtx.y = vert.pre_cam_trans_pos.y();
          vtx.z = vert.pre_cam_trans_pos.z();
          vtx.s = vert.stq.x();
          vtx.t = vert.stq.y();
          // because this is true, we can remove a divide in the shader
          ASSERT(vert.stq.z() == 1.f);
          vtx.color_index = vert.rgba / 4;
          // ASSERT((vert.rgba >> 2) < 1024); spider cave has 2048?
          ASSERT((vert.rgba & 3) == 0);

          vertices.push_back(vtx);
        }
        tdraw.runs.push_back(run);
        tdraw.vis_groups.push_back(vgroup);
      }

      tree_out.draws.push_back(tdraw);
    }
  }
}

void emulate_tfrags(int geom,
                    const std::vector<level_tools::TFragment>& frags,
                    const std::string& debug_name,
                    const std::vector<level_tools::TextureRemap>& map,
                    tfrag3::Level& level_out,
                    tfrag3::TfragTree& tree_out,
                    std::vector<tfrag3::PreloadedVertex>& vertices,
                    const TextureDB& tdb,
                    const std::vector<std::pair<int, int>>& expected_missing_textures,
                    bool dump_level,
                    const std::string& level_name,
                    bool disable_alpha_test_in_normal) {
  TFragExtractStats stats;

  std::vector<u8> vu_mem;
  vu_mem.resize(16 * 1024);

  std::vector<TFragDraw> all_draws;

  for (auto& frag : frags) {
    TFragColorUnpack color_indices;
    emulate_dma_building_for_tfrag(geom, frag, vu_mem, color_indices, &stats);
    VuMemWrapper mem(vu_mem);
    auto draws = emulate_tfrag_execution<false>(frag, mem, color_indices, &stats);
    all_draws.insert(all_draws.end(), draws.begin(), draws.end());
  }

  process_draw_mode(all_draws, map, tree_out.kind, disable_alpha_test_in_normal);
  auto groups = make_draw_groups(all_draws);

  make_tfrag3_data(groups, tree_out, vertices, level_out.textures, tdb, expected_missing_textures,
                   level_name);

  if (dump_level) {
    auto debug_out = debug_dump_to_obj(all_draws);
    auto file_path =
        file_util::get_file_path({"debug_out", fmt::format("tfrag-{}.obj", debug_name)});
    file_util::create_dir_if_needed_for_file(file_path);
    file_util::write_text_file(file_path, debug_out);
  }
}

void extract_time_of_day(const level_tools::DrawableTreeTfrag* tree, tfrag3::TfragTree& out) {
  out.colors.resize(tree->time_of_day.height);
  for (int i = 0; i < (int)tree->time_of_day.height; i++) {
    for (int j = 0; j < 8; j++) {
      memcpy(out.colors[i].rgba[j].data(), &tree->time_of_day.colors[i * 8 + j], 4);
    }
  }
}

void merge_groups(std::vector<tfrag3::StripDraw::VisGroup>& grps) {
  std::vector<tfrag3::StripDraw::VisGroup> result;
  result.push_back(grps.at(0));
  for (size_t i = 1; i < grps.size(); i++) {
    if (grps[i].vis_idx_in_pc_bvh == result.back().vis_idx_in_pc_bvh) {
      result.back().num_inds += grps[i].num_inds;
      result.back().num_tris += grps[i].num_tris;
    } else {
      result.push_back(grps[i]);
    }
  }
  std::swap(result, grps);
}

}  // namespace

void extract_tfrag(const level_tools::DrawableTreeTfrag* tree,
                   const std::string& debug_name,
                   const std::vector<level_tools::TextureRemap>& map,
                   const TextureDB& tex_db,
                   const std::vector<std::pair<int, int>>& expected_missing_textures,
                   tfrag3::Level& out,
                   bool dump_level,
                   const std::string& level_name,
                   bool disable_atest_in_normal) {
  // go through 3 lods(?)
  for (int geom = 0; geom < GEOM_MAX; ++geom) {
    tfrag3::TfragTree this_tree;
    if (tree->my_type() == "drawable-tree-tfrag") {
      this_tree.kind = tfrag3::TFragmentTreeKind::NORMAL;
    } else if (tree->my_type() == "drawable-tree-dirt-tfrag") {
      this_tree.kind = tfrag3::TFragmentTreeKind::DIRT;
    } else if (tree->my_type() == "drawable-tree-ice-tfrag") {
      this_tree.kind = tfrag3::TFragmentTreeKind::ICE;
    } else if (tree->my_type() == "drawable-tree-lowres-tfrag") {
      this_tree.kind = tfrag3::TFragmentTreeKind::LOWRES;
    } else if (tree->my_type() == "drawable-tree-trans-tfrag") {
      this_tree.kind = tfrag3::TFragmentTreeKind::TRANS;
    } else if (tree->my_type() == "drawable-tree-tfrag-trans") {
      this_tree.kind = tfrag3::TFragmentTreeKind::TRANS;
    } else if (tree->my_type() == "drawable-tree-tfrag-water") {
      this_tree.kind = tfrag3::TFragmentTreeKind::WATER;
    } else {
      ASSERT_MSG(false, fmt::format("unknown tfrag tree kind: {}", tree->my_type()));
    }

    ASSERT(tree->length == (int)tree->arrays.size());
    ASSERT(tree->length > 0);

    auto last_array = tree->arrays.back().get();

    auto as_tfrag_array = dynamic_cast<level_tools::DrawableInlineArrayTFrag*>(last_array);
    ASSERT(as_tfrag_array);
    ASSERT(as_tfrag_array->length == (int)as_tfrag_array->tfragments.size());
    ASSERT(as_tfrag_array->length > 0);
    u16 idx = as_tfrag_array->tfragments.front().id;
    for (auto& elt : as_tfrag_array->tfragments) {
      ASSERT(elt.id == idx);
      idx++;
    }
    bool ok = verify_node_indices(tree);
    ASSERT(ok);
    // lg::print("    tree has {} arrays and {} tfragments\n", tree->length,
    // as_tfrag_array->length);

    auto vis_nodes = extract_vis_data(tree, as_tfrag_array->tfragments.front().id);
    this_tree.bvh.first_leaf_node = vis_nodes.first_child_node;
    this_tree.bvh.last_leaf_node = vis_nodes.last_child_node;
    this_tree.bvh.num_roots = vis_nodes.num_roots;
    this_tree.bvh.only_children = vis_nodes.only_children;
    this_tree.bvh.first_root = vis_nodes.first_root;
    this_tree.bvh.vis_nodes = std::move(vis_nodes.vis_nodes);

    std::unordered_map<int, int> tfrag_parents;
    // for (auto& node : this_tree.vis_nodes) {
    for (size_t node_idx = 0; node_idx < this_tree.bvh.vis_nodes.size(); node_idx++) {
      const auto& node = this_tree.bvh.vis_nodes[node_idx];
      if (node.flags == 0) {
        for (int i = 0; i < node.num_kids; i++) {
          tfrag_parents[node.child_id + i] = node_idx;
        }
      }
    }
    //  ASSERT(result.vis_nodes.last_child_node + 1 == idx);

    std::vector<tfrag3::PreloadedVertex> vertices;
    emulate_tfrags(geom, as_tfrag_array->tfragments, debug_name, map, out, this_tree, vertices,
                   tex_db, expected_missing_textures, dump_level, level_name,
                   disable_atest_in_normal);
    pack_tfrag_vertices(&this_tree.packed_vertices, vertices);
    extract_time_of_day(tree, this_tree);

    for (auto& draw : this_tree.draws) {
      for (auto& str : draw.vis_groups) {
        auto it = tfrag_parents.find(str.vis_idx_in_pc_bvh);
        if (it == tfrag_parents.end()) {
          str.vis_idx_in_pc_bvh = UINT16_MAX;
        } else {
          str.vis_idx_in_pc_bvh = it->second;
        }
      }
      merge_groups(draw.vis_groups);
    }
    out.tfrag_trees[geom].push_back(this_tree);
  }
}
}  // namespace decompiler
