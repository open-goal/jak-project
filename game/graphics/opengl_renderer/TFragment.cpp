#include "TFragment.h"

#include "third-party/imgui/imgui.h"
#include "game/graphics/opengl_renderer/dma_helpers.h"

namespace {
bool looks_like_tfragment_dma(const DmaFollower& follow) {
  return follow.current_tag_vifcode0().kind == VifCode::Kind::STCYCL;
}
}  // namespace

TFragment::TFragment(const std::string& name, BucketId my_id) : BucketRenderer(name, my_id) {
  for (auto& buf : m_buffered_data) {
    for (auto& x : buf.pad) {
      x = 0;
    }
  }
}

void TFragment::render(DmaFollower& dma,
                       SharedRenderState* render_state,
                       ScopedProfilerNode& prof) {
  m_debug_string.clear();
  m_frag_debug.clear();
  m_stats = {};

  // First thing should be a NEXT with two nops.
  auto data0 = dma.read_and_advance();
  assert(data0.vif1() == 0);
  assert(data0.vif0() == 0);
  assert(data0.size_bytes == 0);

  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    // renderer didn't run, let's just get out of here.
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    assert(dma.current_tag_offset() == render_state->next_bucket);
    return;
  }

  handle_initialization(dma, render_state);

  if (m_extra_debug) {
    ImGui::Begin(fmt::format("{} extra", m_name).c_str());
  }
  while (looks_like_tfragment_dma(dma)) {
    m_stats.tfrag_dma_packets++;
    auto frag = dma.read_and_advance();
    m_stats.tfrag_bytes += frag.size_bytes;
    if (m_extra_debug) {
      handle_tfrag<true>(frag, render_state);
    } else {
      handle_tfrag<false>(frag, render_state);
    }
  }
  if (m_extra_debug) {
    ImGui::End();
  }

  m_debug_string += fmt::format("fail: {}\n", dma.current_tag_vifcode0().print());

  while (dma.current_tag_offset() != render_state->next_bucket) {
    auto tag = dma.current_tag().print();
    auto data = dma.read_and_advance();
    m_debug_string +=
        fmt::format("DMA {} {} bytes, {}\n", tag, data.size_bytes, data.vifcode0().print());
  }
}
void TFragment::draw_debug_window() {
  ImGui::Separator();
  ImGui::Checkbox("Extra Debug", &m_extra_debug);
  ImGui::Text("packets: %d", m_stats.tfrag_dma_packets);
  ImGui::Text("frag bytes: %d", m_stats.tfrag_bytes);
  ImGui::Text("errors: %d", m_stats.error_packets);
  for (int prog = 0; prog < 12; prog++) {
    ImGui::Text("  prog %d: %d calls\n", prog, m_stats.per_program[prog].calls);
  }
  ImGui::TextUnformatted(m_debug_string.data());
}

void TFragment::handle_initialization(DmaFollower& dma, SharedRenderState* render_state) {
  // Set up test (different between different renderers)
  auto setup_test = dma.read_and_advance();
  assert(setup_test.vif0() == 0);
  assert(setup_test.vifcode1().kind == VifCode::Kind::DIRECT);
  assert(setup_test.vifcode1().immediate == 2);
  assert(setup_test.size_bytes == 32);
  memcpy(m_test_setup, setup_test.data, 32);

  // matrix 0
  auto mat0_upload = dma.read_and_advance();
  unpack_to_stcycl(&m_buffered_data[0].pad[TFragDataMem::TFragMatrix0 * 16], mat0_upload,
                   VifCode::Kind::UNPACK_V4_32, 4, 4, 64, TFragDataMem::TFragMatrix0, false, false);
  m_debug_string += fmt::format("Matrix 0:\n {}\n", m_matrix_0.to_string_aligned());

  // matrix 1
  auto mat1_upload = dma.read_and_advance();
  unpack_to_stcycl(&m_buffered_data[1].pad[TFragDataMem::TFragMatrix0 * 16], mat1_upload,
                   VifCode::Kind::UNPACK_V4_32, 4, 4, 64, TFragDataMem::TFragMatrix1, false, false);
  m_debug_string += fmt::format("Matrix 1:\n {}\n", m_matrix_1.to_string_aligned());

  // data
  auto data_upload = dma.read_and_advance();
  unpack_to_stcycl(&m_tfrag_data, data_upload, VifCode::Kind::UNPACK_V4_32, 4, 4, sizeof(TFragData),
                   TFragDataMem::TFragFrameData, false, false);
  m_debug_string += fmt::format("Frame Data:\n {}\n", m_tfrag_data.print());

  // call the setup program
  auto mscal_setup = dma.read_and_advance();
  verify_mscal(mscal_setup, TFragProgMem::TFragSetup);

  // iaddiu vi14, vi00, 0x2a0   |  nop
  m_ptrs.vi14 = 0x2a0;  // todo constant
  // iaddiu vi01, vi00, 0x350   |  nop
  m_ptrs.vi01 = 0x350;  // todo constant
  // mfir.x vf03, vi14          |  nop
  m_ptrs.vf03_x = m_ptrs.vi14;
  // mfir.y vf03, vi01          |  nop
  m_ptrs.vf03_y = m_ptrs.vi01;
  // mfir.z vf03, vi14          |  nop
  m_ptrs.vf03_z = m_ptrs.vi14;
  // mfir.w vf03, vi01          |  nop :e
  m_ptrs.vf03_w = m_ptrs.vi01;
  // lq.xyzw vf04, 664(vi00)    |  nop
  m_globals.vf04_ambient = m_tfrag_data.ambient;  // TODO get rid?

  // setup double buffering.
  auto db_setup = dma.read_and_advance();
  assert(db_setup.size_bytes == 0);
  assert(db_setup.vifcode0().kind == VifCode::Kind::BASE &&
         db_setup.vifcode0().immediate == Buffer0_Start);
  assert(db_setup.vifcode1().kind == VifCode::Kind::OFFSET &&
         db_setup.vifcode1().immediate == (Buffer1_Start - Buffer0_Start));
}

template <bool DEBUG>
void TFragment::handle_tfrag(const DmaTransfer& dma, SharedRenderState* render_state) {
  auto first_vif = dma.vifcode0();
  auto second_vif = dma.vifcode1();
  if (DEBUG) {
    ImGui::Separator();
    ImGui::Text("tf: %d sz %d", m_stats.tfrag_dma_packets, dma.size_bytes);
    ImGui::Text(" vif: %s", first_vif.print().c_str());
    ImGui::Text(" vif: %s", second_vif.print().c_str());
  }

  // first VIF should be a STCYCL
  assert(first_vif.kind == VifCode::Kind::STCYCL);
  VifCodeStcycl stcycl(first_vif.immediate);

  // this is our state for running through the DMA data
  int cl = stcycl.cl;
  int wl = stcycl.wl;
  int offset_into_data = 0;
  bool row_init = false;
  u32 row[4];
  u8 stmod = 0;

  // next can be one of:
  // - NOP, UNPACK, MSCAL

  switch (second_vif.kind) {
    case VifCode::Kind::NOP:
      // do nothing!
      break;
    case VifCode::Kind::UNPACK_V4_8:
      offset_into_data = handle_unpack_v4_8_mode0(second_vif, dma, offset_into_data, cl, wl);
      break;
    case VifCode::Kind::MSCAL:
      handle_mscal<DEBUG>(second_vif);
      break;
    default:
      fmt::print("unknown second vif in tfragment: {}\n", second_vif.print());
      assert(false);
  }

  bool ok = true;
  while (ok && offset_into_data < (int)dma.size_bytes) {
    assert((offset_into_data % 4) == 0);
    auto vif = dma.read_val<u32>(offset_into_data);
    offset_into_data += 4;

    auto code = VifCode(vif);
    switch (code.kind) {
      case VifCode::Kind::UNPACK_V4_16:
        if (DEBUG) {
          ImGui::Text(" vif: %s (m %d)", code.print().c_str(), stmod);
        }
        if (stmod == 0) {
          offset_into_data = handle_unpack_v4_16_mode0(code, dma, offset_into_data, cl, wl);
        } else if (stmod == 1) {
          assert(row_init);
          offset_into_data = handle_unpack_v4_16_mode1(code, dma, offset_into_data, cl, wl, row);
        } else {
          assert(false);
        }
        break;
      case VifCode::Kind::UNPACK_V4_32:
        if (DEBUG) {
          ImGui::Text(" vif: %s", code.print().c_str());
        }
        assert(stmod == 0);
        offset_into_data = handle_unpack_v4_32(code, dma, offset_into_data, cl, wl);
        break;
      case VifCode::Kind::UNPACK_V4_8:
        if (DEBUG) {
          ImGui::Text(" vif: %s", code.print().c_str());
        }
        if (stmod == 0) {
          offset_into_data = handle_unpack_v4_8_mode0(code, dma, offset_into_data, cl, wl);
        } else if (stmod == 1) {
          assert(row_init);
          offset_into_data = handle_unpack_v4_8_mode1(code, dma, offset_into_data, cl, wl, row);
        } else {
          assert(false);
        }

        break;
      case VifCode::Kind::UNPACK_V3_32:
        if (DEBUG) {
          ImGui::Text(" vif: %s", code.print().c_str());
        }
        assert(stmod == 0);
        offset_into_data = handle_unpack_v3_32(code, dma, offset_into_data, cl, wl);
        break;
      case VifCode::Kind::STROW:
        row_init = true;
        memcpy(row, dma.data + offset_into_data, 16);
        offset_into_data += 16;
        if (DEBUG) {
          Vector4f vec;
          memcpy(&vec, row, 16);
          ImGui::Text(" row: %s", vec.to_string_aligned().c_str());
        }
        break;
      case VifCode::Kind::STMOD:
        if (DEBUG) {
          ImGui::Text(" stmod %d\n", code.immediate);
        }
        if (stmod == 0) {
          assert(code.immediate == 1);
        } else {
          assert(stmod == 1);
          assert(code.immediate == 0 || code.immediate == 1);  // kinda weird.
        }
        stmod = code.immediate;
        break;
      case VifCode::Kind::STCYCL:
        if (DEBUG) {
          ImGui::Text(" vif: %s", code.print().c_str());
        }
        {
          VifCodeStcycl ss(code.immediate);
          cl = ss.cl;
          wl = ss.wl;
        }

        break;
      case VifCode::Kind::NOP:
        if (DEBUG) {
          ImGui::Text(" NOP");
        }
        break;
      default:
        ok = false;
        if (DEBUG) {
          ImGui::TextColored(ImVec4(0.8, 0.3, 0.3, 1.0), "unhandled vif: %s", code.print().c_str());
        }
        break;
    }
  }
  if (!ok) {
    m_stats.error_packets++;
  } else {
    ImGui::Text("END");
    assert(stmod == 0);
  }
}

int TFragment::handle_unpack_v4_8_mode0(const VifCode& code,
                                        const DmaTransfer& dma,
                                        int offset,
                                        int cl,
                                        int wl) {
  VifCodeUnpack unpack(code);
  assert(unpack.use_tops_flag);

  // CL x (num/WL)+(num%WL)

  u8* write_base = get_upload_buffer();

  if (unpack.is_unsigned) {
    // note: formulas below assume this!
    assert(cl == 2);
    assert(wl == 1);
    for (int i = 0; i < code.num; i++) {
      // write every other qw
      int dest_qw = unpack.addr_qw + 2 * i;
      assert(dest_qw <= 328);
      u32 qw[4];
      qw[0] = dma.read_val<u8>(offset++);
      qw[1] = dma.read_val<u8>(offset++);
      qw[2] = dma.read_val<u8>(offset++);
      qw[3] = dma.read_val<u8>(offset++);
      memcpy(write_base + (dest_qw * 16), qw, 16);
    }
  } else {
    // note: formulas below assume this!
    assert(cl == 4);
    assert(wl == 4);
    for (int i = 0; i < code.num; i++) {
      // write every other qw
      int dest_qw = unpack.addr_qw + i;
      assert(dest_qw <= 328);
      s32 qw[4];
      qw[0] = dma.read_val<s8>(offset++);
      qw[1] = dma.read_val<s8>(offset++);
      qw[2] = dma.read_val<s8>(offset++);
      qw[3] = dma.read_val<s8>(offset++);
      memcpy(write_base + (dest_qw * 16), qw, 16);
    }
  }

  return offset;
}

int TFragment::handle_unpack_v4_8_mode1(const VifCode& code,
                                        const DmaTransfer& dma,
                                        int offset,
                                        int cl,
                                        int wl,
                                        const u32 row[4]) {
  VifCodeUnpack unpack(code);
  assert(unpack.use_tops_flag);

  // CL x (num/WL)+(num%WL)

  u8* write_base = get_upload_buffer();

  if (unpack.is_unsigned) {
    // note: formulas below assume this!
    assert(cl == 4);
    assert(wl == 4);
    for (int i = 0; i < code.num; i++) {
      // write every other qw
      int dest_qw = unpack.addr_qw + i;
      assert(dest_qw <= 328);
      u32 qw[4];
      qw[0] = row[0] + dma.read_val<u8>(offset++);
      qw[1] = row[1] + dma.read_val<u8>(offset++);
      qw[2] = row[2] + dma.read_val<u8>(offset++);
      qw[3] = row[3] + dma.read_val<u8>(offset++);
      memcpy(write_base + (dest_qw * 16), qw, 16);
    }
  } else {
    // note: formulas below assume this!
    assert(cl == 4);
    assert(wl == 4);
    for (int i = 0; i < code.num; i++) {
      // write every other qw
      int dest_qw = unpack.addr_qw + i;
      assert(dest_qw <= 328);
      s32 qw[4];
      qw[0] = row[0] + dma.read_val<s8>(offset++);
      qw[1] = row[1] + dma.read_val<s8>(offset++);
      qw[2] = row[2] + dma.read_val<s8>(offset++);
      qw[3] = row[3] + dma.read_val<s8>(offset++);
      memcpy(write_base + (dest_qw * 16), qw, 16);
    }
  }

  return offset;
}

int TFragment::handle_unpack_v4_16_mode0(const VifCode& code,
                                         const DmaTransfer& dma,
                                         int offset,
                                         int cl,
                                         int wl) {
  VifCodeUnpack unpack(code);
  assert(unpack.use_tops_flag);
  assert(unpack.is_unsigned);

  // note: formulas below assume this!
  assert(cl == 4);
  assert(wl == 4);

  u8* write_base = get_upload_buffer();
  for (int i = 0; i < code.num; i++) {
    // write every other qw
    int dest_qw = unpack.addr_qw + i;
    assert(dest_qw <= 328);
    u32 qw[4];
    qw[0] = dma.read_val<u16>(offset);
    offset += 2;
    qw[1] = dma.read_val<u16>(offset);
    offset += 2;
    qw[2] = dma.read_val<u16>(offset);
    offset += 2;
    qw[3] = dma.read_val<u16>(offset);
    offset += 2;
    memcpy(write_base + (dest_qw * 16), qw, 16);
  }
  return offset;
}

int TFragment::handle_unpack_v4_16_mode1(const VifCode& code,
                                         const DmaTransfer& dma,
                                         int offset,
                                         int cl,
                                         int wl,
                                         const u32 row[4]) {
  VifCodeUnpack unpack(code);
  assert(unpack.use_tops_flag);
  assert(unpack.is_unsigned);

  // note: formulas below assume this!
  assert(cl == 4);
  assert(wl == 4);

  u8* write_base = get_upload_buffer();
  for (int i = 0; i < code.num; i++) {
    // write every other qw
    int dest_qw = unpack.addr_qw + i;
    assert(dest_qw <= 328);
    u32 qw[4];
    qw[0] = row[0] + dma.read_val<u16>(offset);
    offset += 2;
    qw[1] = row[1] + dma.read_val<u16>(offset);
    offset += 2;
    qw[2] = row[2] + dma.read_val<u16>(offset);
    offset += 2;
    qw[3] = row[3] + dma.read_val<u16>(offset);
    offset += 2;
    memcpy(write_base + (dest_qw * 16), qw, 16);
  }
  return offset;
}

int TFragment::handle_unpack_v3_32(const VifCode& code,
                                   const DmaTransfer& dma,
                                   int offset,
                                   int cl,
                                   int wl) {
  VifCodeUnpack unpack(code);
  assert(unpack.use_tops_flag);
  assert(!unpack.is_unsigned);

  // note: formulas below assume this!
  assert(cl == 2);
  assert(wl == 1);

  u8* write_base = get_upload_buffer();
  for (int i = 0; i < code.num; i++) {
    // write every other qw
    int dest_qw = unpack.addr_qw + i * 2;
    assert(dest_qw <= 328);
    u32 qw[4];
    qw[0] = dma.read_val<u32>(offset);
    offset += 4;
    qw[1] = dma.read_val<u32>(offset);
    offset += 4;
    qw[2] = dma.read_val<u32>(offset);
    offset += 4;
    qw[3] = 0;  // this can be anything... but it seems like it tries to load from it sometimes?
    memcpy(write_base + (dest_qw * 16), qw, 16);
  }
  return offset;
}

int TFragment::handle_unpack_v4_32(const VifCode& code,
                                   const DmaTransfer& dma,
                                   int offset,
                                   int cl,
                                   int wl) {
  VifCodeUnpack unpack(code);
  assert(unpack.use_tops_flag);
  assert(!unpack.is_unsigned);

  // note: formulas below assume this!
  assert(cl == 4);
  assert(wl == 4);
  u8* write_base = get_upload_buffer();
  for (int i = 0; i < code.num; i++) {
    // write every other qw
    int dest_qw = unpack.addr_qw + i * 2;
    assert(dest_qw <= 328);
    u32 qw[4];
    qw[0] = dma.read_val<u32>(offset);
    offset += 4;
    qw[1] = dma.read_val<u32>(offset);
    offset += 4;
    qw[2] = dma.read_val<u32>(offset);
    offset += 4;
    qw[3] = dma.read_val<u32>(offset);
    offset += 4;
    memcpy(write_base + (dest_qw * 16), qw, 16);
  }
  return offset;

  //  u8* write_base = get_upload_buffer();
  //  assert(code.num + unpack.addr_qw <= 328);
  //  memcpy(write_base + (unpack.addr_qw * 16), dma.data + offset, code.num * 16);
  //  return offset + code.num * 16;
}

template <bool DEBUG>
void TFragment::handle_mscal(const VifCode& code) {
  if (DEBUG) {
    ImGui::TextColored(ImVec4(0.3, 0.8, 0.3, 1.0), "MSCAL: %d", code.immediate);
  }

  int prog_id = code.immediate / 2;
  if (prog_id >= 12) {
    fmt::print("bad program: {}\n", prog_id);
    assert(false);
  }
  m_stats.per_program[prog_id].calls++;

  switch (code.immediate) {
    case 6:
      exec_program_6<DEBUG>();
      break;
    default:
      if (DEBUG) {
        ImGui::TextColored(ImVec4(0.8, 0.8, 0.3, 1.0), "  UNHANDLED");
        m_stats.error_mscals++;
      }
      break;
  }
}

void TFragment::flip_buffers() {
  m_uploading_buffer ^= 1;
}

u16 TFragment::ilw_data(int offset, int xyzw) {
  u16 result;
  if (m_uploading_buffer == 0) {
    offset -= Buffer1_Start;
  }
  assert(offset < Buffer1_Start);
  assert(offset >= 0);
  int mem_offset = (xyzw * 4) + (offset * 16);
  memcpy(&result, get_processing_buffer() + mem_offset, 2);
  return result;
}

Vector4f TFragment::load_vector_data(int offset) {
  Vector4f result;
  if (m_uploading_buffer == 0) {
    offset -= Buffer1_Start;
  }
  offset = offset & 0x3ff;  // not super happy with this...
  assert(offset < Buffer1_Start);
  assert(offset >= 0);
  memcpy(&result, get_processing_buffer() + (offset * 16), 16);
  return result;
}

void TFragment::store_vector_kick_zone(int offset, const Vector4f& vec) {
  assert(offset >= TFragDataMem::TFragKickZone);
  assert(offset < 1024);
  memcpy(&m_kick_data.pad[(offset - TFragDataMem::TFragKickZone) * 16], &vec.data()[0], 16);
}

u16 TFragment::ilw_kick_zone(int offset, int xyzw) {
  assert(offset >= TFragDataMem::TFragKickZone);
  assert(offset < 1024);
  u16 result;
  int mem_offset = (xyzw * 4) + (offset * 16);
  memcpy(&result, m_kick_data.pad + mem_offset, 2);
  return result;
}

template <bool DEBUG>
void TFragment::exec_program_6() {
  fmt::print("exec 6\n");
  flip_buffers();
  // VF02 is VAL always
  // VF05 is ADGIF always
  // VF06 is STRGIF always
  // VF10 is HVDF offset always
  // VF11 is hmge_scale always
  // VF01 is fog always

  // SETUP
  // first, load globals from TFragData (these never change, so we'll just use them from TFragData)
  //  lq.xyzw vf02, 657(vi00)    |  nop
  //  lq.xyzw vf05, 660(vi00)    |  addw.z vf28, vf00, vf00 (done later)
  //  lq.xyzw vf06, 658(vi00)    |  nop
  //  lq.xyzw vf10, 661(vi00)    |  nop
  //  lq.xyzw vf11, 662(vi00)    |  nop
  //  lq.xyzw vf01, 656(vi00)    |  addz.z vf28, vf28, vf02 (done later)

  // there are two main versions, one for each double-buffer.
  // I'm not sure why these need to be different yet.
  // but, just in case it actually matters and we actually need to swap addresses for some reason
  // (like addresses baked in to the data)
  // we're going to split into two functions.
  // these inputs will be given to either. (these are const)
  Prog6Inputs inputs;

  // non-const
  Prog6Vars vars;
  if (m_uploading_buffer == 1) {
    vars.vi14 = 0;
  } else {
    vars.vi14 = Buffer1_Start;
  }

  vars.vf28.z() = 1.f;
  vars.vf28.z() += m_tfrag_data.val.z();

  //  ilw.w vi08, 4(vi14)        |  nop
  vars.vi08 = ilw_data(4 + vars.vi14, 3);
  //  ilw.z vi09, 4(vi14)        |  nop
  vars.vi09 = ilw_data(4 + vars.vi14, 2);
  //  ilw.y vi03, 3(vi14)        |  nop
  vars.vi03 = ilw_data(3 + vars.vi14, 1);

  fmt::print("-------VI03 init: {}\n", vars.vi03);

  if (DEBUG) {
    // small, like 9, 54, 66
    ImGui::Text("ints: %d %d %d", vars.vi08, vars.vi09, vars.vi03);
  }

  //  fcset 0x0                  |  nop
  //  iaddi vi07, vi00, -0x1     |  nop
  vars.vi07 = -1;

  //  lq.xyzw vf04, 5(vi14)      |  mulw.xyzw vf16, vf00, vf00
  inputs.vf04_cam_mat_x = load_vector_data(vars.vi14 + 5);
  vars.vf16_scaled_pos_0 = Vector4f(0, 0, 0, 1);

  //  lq.xyzw vf07, 6(vi14)      |  mulw.xyzw vf17, vf00, vf00
  inputs.vf07_cam_mat_y = load_vector_data(vars.vi14 + 6);
  vars.vf17_scaled_pos_1 = Vector4f(0, 0, 0, 1);

  //  ibne vi00, vi14, L136      |  mulw.xyzw vf18, vf00, vf00
  vars.vf18_scaled_pos_2 = Vector4f(0, 0, 0, 1);
  //  lq.xyzw vf08, 7(vi14)      |  mulw.xyzw vf19, vf00, vf00
  vars.vf19_scaled_pos_3 = Vector4f(0, 0, 0, 1);
  inputs.vf08_cam_mat_z = load_vector_data(vars.vi14 + 7);

  if (m_uploading_buffer == 1) {
    // vi14 = 0 version
    exec_program_6_process_first<DEBUG>(inputs, vars);
  } else {
    // L136
    assert(false);
    exec_program_6_process_second<DEBUG>(inputs, vars);
  }

  // because we're doing everything in-sync (no background kicking or uploading),
  // I _think_ it's fine to just do
  flip_buffers();
  // and now we only have to implement one half of program 6?
}

namespace {
u32 float_2_u32(float x) {
  u32 y;
  memcpy(&y, &x, 4);
  return y;
}

float u32_2_float(u32 x) {
  float y;
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

bool clip_xyz_plus_minus(const Vector4f& pt) {
  float pw = std::abs(pt.w());
  float mw = -pw;
  for (int i = 0; i < 3; i++) {
    if (pt[i] > pw) {
      return true;
    }
    if (pt[i] < mw) {
      return true;
    }
  }
  return false;
}
}  // namespace

void TFragment::store_gif_kick_zone(int offset, const GifTag& tag) {
  assert(offset >= TFragDataMem::TFragKickZone);
  assert(offset < 1024);
  memcpy(&m_kick_data.pad[(offset - TFragDataMem::TFragKickZone) * 16], &tag, 16);
}

void TFragment::store_u32_kick_zone(u32 value, int qw, int xyzw) {
  assert(qw >= TFragDataMem::TFragKickZone);
  assert(qw < 1024);
  memcpy(&m_kick_data.pad[(xyzw * 4) + (qw - TFragDataMem::TFragKickZone) * 16], &value, 4);
}

template <bool DEBUG>
void TFragment::exec_program_6_process_first(const Prog6Inputs& in, Prog6Vars& vars) {
  // SETUP BLOCK

  //  ilwr.x vi02, vi03          |  nop
  assert(vars.vi03 < TFragDataMem::Buffer1_Start);  // should be a buffer 0 addr
  vars.vi02 = ilw_data(vars.vi03, 0);
  fmt::print("--------- initial vi02.x: {}\n", vars.vi02);

  //  lq.xyzw vf09, 8(vi14)      |  nop
  vars.vf09_cam_trans = load_vector_data(vars.vi14 + 8);

  // stupid, this is 0.
  //  iadd vi08, vi08, vi14      |  nop
  vars.vi08 += vars.vi14;
  //  iadd vi09, vi09, vi14      |  nop
  vars.vi09 += vars.vi14;

  //  lq.xyw vf28, 0(vi02)       |  nop
  if (DEBUG) {
    ImGui::Text("vi02: %d", vars.vi02);
  }
  auto vf28_load_temp = load_vector_data(vars.vi02);
  vars.vf28.x() = vf28_load_temp.x();
  vars.vf28.y() = vf28_load_temp.y();
  vars.vf28.w() = vf28_load_temp.w();

  //  mtir vi06, vf03.x          |  nop
  vars.vi06_kick_zone_ptr = m_ptrs.vf03_x;

  //  ilwr.x vi12, vi09          |  nop
  vars.vi12 = ilw_data(vars.vi09, 0);
  fmt::print("--------- initial vi12: {}\n", vars.vi12);

  //  ilwr.z vi13, vi09          |  nop
  vars.vi13 = ilw_data(vars.vi03, 2);

  //  mtir vi04, vf28.w          |  subz.xyz vf24, vf28, vf02
  vars.vi04 = float_2_u32(vars.vf28.w());
  vars.vf24 = vars.vf28 - m_tfrag_data.val.z();  // only xyz, but what sets w??

  //  iaddiu vi11, vi00, 0x4000  |  nop
  vars.vi11 = 0x4000;

  //  iaddiu vi11, vi11, 0x4000  |  nop
  vars.vi11 += 0x4000;

  //  ilwr.y vi02, vi03          |  nop
  vars.vi02 = ilw_data(vars.vi03, 1);
  fmt::print("--------- initial vi02.y: {}\n", vars.vi02);

  //  lq.xyzw vf12, 0(vi04)      |  nop
  if (DEBUG) {
    ImGui::Text("vi04: %d", vars.vi04);
  }
  vars.vf12_root_pos_0 = load_vector_data(vars.vi04);

  //  lq.xyzw vf20, 1(vi04)      |  nop
  vars.vf20 = load_vector_data(vars.vi04 + 1);

  //  iaddiu vi12, vi12, 0x80    |  nop
  vars.vi12 += 0x80;

  //  iadd vi13, vi13, vi08      |  nop
  vars.vi13 += vars.vi08;

  //  lq.xyw vf28, 0(vi02)       |  itof0.xyzw vf12, vf12
  vf28_load_temp = load_vector_data(vars.vi02);
  vars.vf28.x() = vf28_load_temp.x();
  vars.vf28.y() = vf28_load_temp.y();
  vars.vf28.w() = vf28_load_temp.w();
  vars.vf12_root_pos_0 = itof0(vars.vf12_root_pos_0);

  fmt::print("root 12 setup: {}\n", vars.vf12_root_pos_0.to_string_aligned());

  //  mfir.w vf24, vi06          |  nop
  vars.vf24.w() = u32_2_float(vars.vi06_kick_zone_ptr);

  //  lqi.xyzw vf29, vi13        |  nop
  vars.vf29 = load_vector_data(vars.vi13);
  vars.vi13++;

  //  lqi.xyzw vf30, vi13        |  nop
  vars.vf30 = load_vector_data(vars.vi13);
  vars.vi13++;

  //  lqi.xyzw vf31, vi13        |  nop
  vars.vf31 = load_vector_data(vars.vi13);
  vars.vi13++;

  //  sqi.xyzw vf05, vi06        |  subz.xyz vf25, vf28, vf02
  if (DEBUG) {
    ImGui::Text("vi06: %d", vars.vi06_kick_zone_ptr);
  }
  store_gif_kick_zone(vars.vi06_kick_zone_ptr, m_tfrag_data.ad_gif);
  vars.vi06_kick_zone_ptr++;
  vars.vf25 = vars.vf28 - m_tfrag_data.val.z();

  //  sqi.xyzw vf29, vi06        |  mulaw.xyzw ACC, vf09, vf00
  store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf29);
  vars.vi06_kick_zone_ptr++;
  Vector4f acc = vars.vf09_cam_trans;

  //  mtir vi04, vf28.w          |  nop
  vars.vi04 = float_2_u32(vars.vf28.w());

  //  sqi.xyzw vf30, vi06        |  maddax.xyzw ACC, vf04, vf12
  store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf30);
  vars.vi06_kick_zone_ptr++;
  acc += in.vf04_cam_mat_x * vars.vf12_root_pos_0.x();

  //  sqi.xyzw vf31, vi06        |  nop
  store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf31);
  vars.vi06_kick_zone_ptr++;

  //  ilwr.z vi02, vi03          |  nop
  vars.vi02 = ilw_data(vars.vi03, 2);
  fmt::print("--------- initial vi02.z: {}\n", vars.vi02);

  //  lq.xyzw vf13, 0(vi04)      |  madday.xyzw ACC, vf07, vf12
  vars.vf13_root_pos_1 = load_vector_data(vars.vi04);
  acc += in.vf07_cam_mat_y * vars.vf12_root_pos_0.y();

  //  lq.xyzw vf21, 1(vi04)      |  maddz.xyzw vf12, vf08, vf12
  vars.vf21 = load_vector_data(vars.vi04 + 1);
  vars.vf12_root_pos_0 = acc + in.vf08_cam_mat_z * vars.vf12_root_pos_0.z();
  fmt::print("root 12 setup cam: {}\n", in.vf08_cam_mat_z.to_string_aligned());

  //  lqi.xyzw vf29, vi13        |  nop
  vars.vf29 = load_vector_data(vars.vi13);
  vars.vi13++;

  //  lqi.xyzw vf30, vi13        |  nop
  vars.vf30 = load_vector_data(vars.vi13);
  vars.vi13++;

  //  lq.xyw vf28, 0(vi02)       |  itof0.xyzw vf13, vf13
  vf28_load_temp = load_vector_data(vars.vi02);
  vars.vf28.x() = vf28_load_temp.x();
  vars.vf28.y() = vf28_load_temp.y();
  vars.vf28.w() = vf28_load_temp.w();
  vars.vf13_root_pos_1 = itof0(vars.vf13_root_pos_1);

  //  div Q, vf01.x, vf12.w      |  mul.xyzw vf16, vf12, vf11
  float q = m_tfrag_data.fog.x() / vars.vf12_root_pos_0.w();
  vars.vf16_scaled_pos_0 = vars.vf12_root_pos_0.elementwise_multiply(m_tfrag_data.hmge_scale);

  //  sqi.xyzw vf29, vi06        |  nop
  store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf29);
  vars.vi06_kick_zone_ptr++;

  //  sqi.xyzw vf30, vi06        |  nop
  store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf30);
  vars.vi06_kick_zone_ptr++;

  //  iadd vi01, vi12, vi12      |  subz.xyz vf26, vf28, vf02
  m_ptrs.vi01 = vars.vi12 + vars.vi12;
  vars.vf26 = vars.vf28 - m_tfrag_data.val.z();

  //  iadd vi01, vi01, vi12      |  mulaw.xyzw ACC, vf09, vf00
  m_ptrs.vi01 += vars.vi12;
  acc = vars.vf09_cam_trans;

  //  mtir vi04, vf28.w          |  nop
  vars.vi04 = float_2_u32(vars.vf28.w());

  //  iadd vi05, vi06, vi01      |  maddax.xyzw ACC, vf04, vf13
  vars.vi05 = vars.vi06_kick_zone_ptr + m_ptrs.vi01;
  acc += in.vf04_cam_mat_x * vars.vf13_root_pos_1.x();

  //  ior vi10, vi06, vi00       |  mul.xyz vf12, vf12, Q
  vars.vi10 = vars.vi06_kick_zone_ptr;
  vars.vf12_root_pos_0.x() *= q;
  vars.vf12_root_pos_0.y() *= q;
  vars.vf12_root_pos_0.z() *= q;

  //  ilwr.w vi02, vi03          |  mul.xyz vf24, vf24, Q
  vars.vi02 = ilw_data(vars.vi03, 3);
  fmt::print("--------- initial vi02.w: {}\n", vars.vi02);
  vars.vf24.x() *= q;
  vars.vf24.y() *= q;
  vars.vf24.z() *= q;

  //  lq.xyzw vf14, 0(vi04)      |  madday.xyzw ACC, vf07, vf13
  vars.vf14_loop_pos_0 = load_vector_data(vars.vi04);
  acc += in.vf07_cam_mat_y * vars.vf13_root_pos_1.y();

  //  lq.xyzw vf22, 1(vi04)      |  maddz.xyzw vf13, vf08, vf13
  vars.vf22 = load_vector_data(vars.vi04 + 1);
  vars.vf13_root_pos_1 = acc + in.vf08_cam_mat_z * vars.vf13_root_pos_1.z();

  //  sqi.xyzw vf06, vi06        |  add.xyzw vf12, vf12, vf10
  store_gif_kick_zone(vars.vi06_kick_zone_ptr, m_tfrag_data.str_gif);
  vars.vi06_kick_zone_ptr++;
  vars.vf12_root_pos_0 += m_tfrag_data.hvdf_offset;

  //  isw.x vi12, -1(vi06)       |  nop
  store_u32_kick_zone(vars.vi12, vars.vi06_kick_zone_ptr - 1, 0);
  if (DEBUG) {
    ImGui::Text("strgif mod: %d", vars.vi12);  // maybe number of tris or something?
  }

  //  lq.xyw vf28, 0(vi02)       |  itof0.xyzw vf14, vf14
  vf28_load_temp = load_vector_data(vars.vi02);
  vars.vf28.x() = vf28_load_temp.x();
  vars.vf28.y() = vf28_load_temp.y();
  vars.vf28.w() = vf28_load_temp.w();
  vars.vf14_loop_pos_0 = itof0(vars.vf14_loop_pos_0);

  //  div Q, vf01.x, vf13.w      |  mul.xyzw vf17, vf13, vf11
  m_q = m_tfrag_data.fog.x() / vars.vf13_root_pos_1.w();
  vars.vf17_scaled_pos_1 = vars.vf13_root_pos_1.elementwise_multiply(m_tfrag_data.hmge_scale);

  //  iaddi vi09, vi09, 0x1      |  miniz.w vf12, vf12, vf01
  vars.vi09++;
  vars.vf12_root_pos_0.w() = std::min(vars.vf12_root_pos_0.w(), m_tfrag_data.fog.z());

  //  ilwr.x vi12, vi09          |  clipw.xyz vf16, vf16
  vars.vi12 = ilw_data(vars.vi09, 0);
  m_clip_and_3ffff = clip_xyz_plus_minus(vars.vf16_scaled_pos_0);
  fmt::print("BEFORE STARTING LOOP: new vi12 is 0x{:x}\n", vars.vi12);

  // starting here, the control flow does crazy stuff, so we have this weird state machine:
  m_next_block = TFragJumper::L128_PART0_X;
  while (m_next_block != TFragJumper::END_PROGRAM) {
    fmt::print("block {}\n", (int)m_next_block);
    switch (m_next_block) {
      case L128_PART0_X:
        exec_jumper_L128<DEBUG>(in, vars);
        break;
      case L129_PART1_X:
        exec_jumper_L129<DEBUG>(in, vars);
        break;
      case L0x6A1_PART0_Y:
        exec_jumper_L6A1<DEBUG>(in, vars);
        break;
      case L130_PART1_Y:
        exec_jumper_L130<DEBUG>(in, vars);
        break;
      case L0x6B0_PART0_Z:
        exec_jumper_L6B0<DEBUG>(in, vars);
        break;
      case L131_PART1_Z:
        exec_jumper_L131<DEBUG>(in, vars);
        break;
      case L0x6BF_PART0_W:
        exec_jumper_L6BF<DEBUG>(in, vars);
        break;
      case L132_PART1_W:
        exec_jumper_L132<DEBUG>(in, vars);
        break;
      case L122_KICK:
        exec_jumper_L122<DEBUG>(in, vars);
        break;
      default:
        assert(false);
    }
  }
}

template <bool DEBUG>
void TFragment::exec_jumper_L128(const Prog6Inputs& in, Prog6Vars& vars) {
  // Part 0 for X
  //  iaddi vi03, vi03, 0x1      |  subz.xyz vf27, vf28, vf02
  vars.vi03++;
  fmt::print("vi03 INC: {}\n", vars.vi03);
  Vector4f vf27_temp = vars.vf28 - m_tfrag_data.val.z();
  vars.vf27.x() = vf27_temp.x();
  vars.vf27.y() = vf27_temp.y();
  vars.vf27.z() = vf27_temp.z();

  //  iaddi vi07, vi07, 0x1      |  mulaw.xyzw ACC, vf09, vf00
  vars.vi07++;
  m_acc = vars.vf09_cam_trans;

  //  mtir vi04, vf28.w          |  maxy.w vf12, vf12, vf01
  vars.vi04 = float_2_u32(vars.vf28.w());
  vars.vf12_root_pos_0.w() = std::max(vars.vf12_root_pos_0.w(), m_tfrag_data.fog.y());

  //  fcand vi01, 0x3ffff        |  maddax.xyzw ACC, vf04, vf14
  m_acc += in.vf04_cam_mat_x * vars.vf14_loop_pos_0.x();
  // fcand already calculated

  //  ibeq vi00, vi01, L129      |  mul.xyz vf13, vf13, Q
  // branch made after next instr
  vars.vf13_root_pos_1.x() *= m_q;
  vars.vf13_root_pos_1.y() *= m_q;
  vars.vf13_root_pos_1.z() *= m_q;

  //  ilwr.x vi02, vi03          |  mul.xyz vf25, vf25, Q
  vars.vi02 = ilw_data(vars.vi03, 0);
  fmt::print("--------- next vi02.x: {}\n", vars.vi02);
  vars.vf25.x() *= m_q;
  vars.vf25.y() *= m_q;
  vars.vf25.z() *= m_q;

  fmt::print("l128 branch: {}\n", m_clip_and_3ffff);
  // skipped if we take the branch
  //  nop                        |  addw.w vf12, vf12, vf01
  if (m_clip_and_3ffff) {
    vars.vf12_root_pos_0.w() += m_tfrag_data.fog.w();
  }

  // todo: inline exec_jumper_129 here?
  m_next_block = TFragJumper::L129_PART1_X;
}

template <bool DEBUG>
void TFragment::exec_jumper_L6A1(const Prog6Inputs& in, Prog6Vars& vars) {
  // part 1 for 1
  //  nop                        |  subz.xyz vf24, vf28, vf02
  Vector4f vf24_temp = vars.vf28 - m_tfrag_data.val.z();
  vars.vf24.x() = vf24_temp.x();
  vars.vf24.y() = vf24_temp.y();
  vars.vf24.z() = vf24_temp.z();

  //  iaddi vi07, vi07, 0x1      |  mulaw.xyzw ACC, vf09, vf00
  vars.vi07++;
  m_acc = vars.vf09_cam_trans;

  //  mtir vi04, vf28.w          |  maxy.w vf13, vf13, vf01
  vars.vi04 = float_2_u32(vars.vf28.w());
  vars.vf13_root_pos_1.w() = std::max(vars.vf13_root_pos_1.w(), m_tfrag_data.fog.y());

  //  fcand vi01, 0x3ffff        |  maddax.xyzw ACC, vf04, vf15
  m_acc += in.vf04_cam_mat_x * vars.vf15_loop_pos_1.x();
  // fcand already calculated

  //  ibeq vi00, vi01, L130      |  mul.xyz vf14, vf14, Q
  vars.vf14_loop_pos_0.x() *= m_q;
  vars.vf14_loop_pos_0.y() *= m_q;
  vars.vf14_loop_pos_0.z() *= m_q;

  //  ilwr.y vi02, vi03          |  mul.xyz vf26, vf26, Q
  vars.vi02 = ilw_data(vars.vi03, 1);
  fmt::print("--------- next vi02.y: {}\n", vars.vi02);
  vars.vf26.x() *= m_q;
  vars.vf26.y() *= m_q;
  vars.vf26.z() *= m_q;

  fmt::print("l6a1 branch: {}\n", m_clip_and_3ffff);
  //  nop                        |  addw.w vf13, vf13, vf0
  if (m_clip_and_3ffff) {
    vars.vf13_root_pos_1.w() += m_tfrag_data.fog.w();
  }

  // todo: inline here?
  m_next_block = TFragJumper::L130_PART1_Y;
}

template <bool DEBUG>
void TFragment::exec_jumper_L6B0(const Prog6Inputs& in, Prog6Vars& vars) {
  //  nop                        |  subz.xyz vf25, vf28, vf02
  Vector4f vf25_temp = vars.vf28 - m_tfrag_data.val.z();
  vars.vf25.x() = vf25_temp.x();
  vars.vf25.y() = vf25_temp.y();
  vars.vf25.z() = vf25_temp.z();

  //  iaddi vi07, vi07, 0x1      |  mulaw.xyzw ACC, vf09, vf00
  vars.vi07++;
  m_acc = vars.vf09_cam_trans;

  //  mtir vi04, vf28.w          |  maxy.w vf14, vf14, vf01
  vars.vi04 = float_2_u32(vars.vf28.w());
  vars.vf14_loop_pos_0.w() = std::max(vars.vf14_loop_pos_0.w(), m_tfrag_data.fog.y());

  //  fcand vi01, 0x3ffff        |  maddax.xyzw ACC, vf04, vf12
  m_acc += in.vf04_cam_mat_x * vars.vf12_root_pos_0.x();
  // fcand already calculated

  //  ibeq vi00, vi01, L131      |  mul.xyz vf15, vf15, Q
  vars.vf15_loop_pos_1.x() *= m_q;
  vars.vf15_loop_pos_1.y() *= m_q;
  vars.vf15_loop_pos_1.z() *= m_q;

  //  ilwr.z vi02, vi03          |  mul.xyz vf27, vf27, Q
  vars.vi02 = ilw_data(vars.vi03, 2);
  fmt::print("--------- next vi02.z: {}\n", vars.vi02);
  vars.vf27.x() *= m_q;
  vars.vf27.y() *= m_q;
  vars.vf27.z() *= m_q;

  fmt::print("l6b0 branch: {}\n", m_clip_and_3ffff);
  //  nop                        |  addw.w vf14, vf14, vf01
  if (m_clip_and_3ffff) {
    vars.vf14_loop_pos_0.w() += m_tfrag_data.fog.w();
  }

  // todo: inline here?
  m_next_block = TFragJumper::L131_PART1_Z;
}

template <bool DEBUG>
void TFragment::exec_jumper_L6BF(const Prog6Inputs& in, Prog6Vars& vars) {
  //  nop                        |  subz.xyz vf26, vf28, vf02
  Vector4f vf26_temp = vars.vf28 - m_tfrag_data.val.z();
  vars.vf26.x() = vf26_temp.x();
  vars.vf26.y() = vf26_temp.y();
  vars.vf26.z() = vf26_temp.z();

  //  iaddi vi07, vi07, 0x1      |  mulaw.xyzw ACC, vf09, vf00
  vars.vi07++;
  m_acc = vars.vf09_cam_trans;

  //  mtir vi04, vf28.w          |  maxy.w vf15, vf15, vf01
  vars.vi04 = float_2_u32(vars.vf28.w());  // L131 previously
  assert(vars.vi04 != 0xbeef);             // hit
  vars.vf15_loop_pos_1.w() = std::max(vars.vf15_loop_pos_1.w(), m_tfrag_data.fog.y());

  //  fcand vi01, 0x3ffff        |  maddax.xyzw ACC, vf04, vf13
  m_acc += in.vf04_cam_mat_x * vars.vf13_root_pos_1.x();

  //  ibeq vi00, vi01, L132      |  mul.xyz vf12, vf12, Q
  vars.vf12_root_pos_0.x() *= m_q;
  vars.vf12_root_pos_0.y() *= m_q;
  vars.vf12_root_pos_0.z() *= m_q;

  //  ilwr.w vi02, vi03          |  mul.xyz vf24, vf24, Q
  vars.vi02 = ilw_data(vars.vi03, 3);
  fmt::print("--------- next vi02.w: {}\n", vars.vi02);
  vars.vf24.x() *= m_q;
  vars.vf24.y() *= m_q;
  vars.vf24.z() *= m_q;

  fmt::print("l6bf branch: {}\n", m_clip_and_3ffff);
  //  nop                        |  addw.w vf15, vf15, vf01
  if (m_clip_and_3ffff) {
    vars.vf15_loop_pos_1.w() += m_tfrag_data.fog.w();
  }

  // todo: inline here?
  m_next_block = TFragJumper::L132_PART1_W;
}

template <bool DEBUG>
void TFragment::exec_jumper_L129(const Prog6Inputs& in, Prog6Vars& vars) {
  // Part 1 for X
  //  lq.xyzw vf15, 0(vi04)      |  madday.xyzw ACC, vf07, vf14
  vars.vf15_loop_pos_1 = load_vector_data(vars.vi04);
  m_acc += in.vf07_cam_mat_y * vars.vf14_loop_pos_0.y();

  //  lq.xyzw vf23, 1(vi04)      |  maddz.xyzw vf14, vf08, vf14
  vars.vf23 = load_vector_data(vars.vi04 + 1);
  vars.vf14_loop_pos_0 = m_acc + in.vf08_cam_mat_z * vars.vf14_loop_pos_0.z();

  //  sqi.xyz vf24, vi06         |  add.xyzw vf13, vf13, vf10
  store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf24);
  vars.vi06_kick_zone_ptr++;
  vars.vf13_root_pos_1 += m_tfrag_data.hvdf_offset;

  //  sqi.xyzw vf20, vi06        |  ftoi4.xyzw vf12, vf12
  store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf20);
  vars.vi06_kick_zone_ptr++;
  vars.vf12_root_pos_0 = ftoi4(vars.vf12_root_pos_0);

  //  lq.xyw vf28, 0(vi02)       |  itof0.xyzw vf15, vf15
  if (vars.vi05 > vars.vi06_kick_zone_ptr + 9) { // HACK added
    auto vf28_load_temp = load_vector_data(vars.vi02);
    vars.vf28.x() = vf28_load_temp.x();
    vars.vf28.y() = vf28_load_temp.y();
    vars.vf28.w() = vf28_load_temp.w();
  }
  vars.vf15_loop_pos_1 = itof0(vars.vf15_loop_pos_1);

  //  div Q, vf01.x, vf14.w      |  mul.xyzw vf18, vf14, vf11
  m_q = m_tfrag_data.fog.x() / vars.vf14_loop_pos_0.w();
  vars.vf18_scaled_pos_2 = vars.vf14_loop_pos_0.elementwise_multiply(m_tfrag_data.hmge_scale);

  //  ibeq vi05, vi06, L133      |  miniz.w vf13, vf13, vf01
  bool take_branch = (vars.vi05 == vars.vi06_kick_zone_ptr);
  fmt::print("L129 prog: {} {}\n", vars.vi05, vars.vi06_kick_zone_ptr);
  vars.vf13_root_pos_1.w() = std::min(vars.vf13_root_pos_1.w(), m_tfrag_data.fog.z());

  //  sqi.xyzw vf12, vi06        |  clipw.xyz vf17, vf17
  store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf12_root_pos_0);
  vars.vi06_kick_zone_ptr++;
  m_clip_and_3ffff = clip_xyz_plus_minus(vars.vf17_scaled_pos_1);

  if (take_branch) {
    // kick zone is full, time for another kick
    m_ret_block = TFragJumper::L0x6A1_PART0_Y;
    m_next_block = TFragJumper::L122_KICK;
  } else {
    // kick zone is not full. we should continue
    m_next_block = TFragJumper::L0x6A1_PART0_Y;
  }
}

template <bool DEBUG>
void TFragment::exec_jumper_L130(const Prog6Inputs& in, Prog6Vars& vars) {
  //  lq.xyzw vf12, 0(vi04)      |  madday.xyzw ACC, vf07, vf15
  vars.vf12_root_pos_0 = load_vector_data(vars.vi04);
  m_acc += in.vf07_cam_mat_y * vars.vf15_loop_pos_1.y();

  //  lq.xyzw vf20, 1(vi04)      |  maddz.xyzw vf15, vf08, vf15
  vars.vf20 = load_vector_data(vars.vi04 + 1);
  vars.vf15_loop_pos_1 = m_acc + in.vf08_cam_mat_z * vars.vf15_loop_pos_1.z();

  //  sqi.xyzw vf25, vi06        |  add.xyzw vf14, vf14, vf10
  store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf25);
  vars.vi06_kick_zone_ptr++;
  vars.vf14_loop_pos_0 += m_tfrag_data.hvdf_offset;

  //  sqi.xyzw vf21, vi06        |  ftoi4.xyzw vf13, vf13
  store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf21);
  vars.vi06_kick_zone_ptr++;
  vars.vf12_root_pos_0 = ftoi4(vars.vf12_root_pos_0);

  //  lq.xyw vf28, 0(vi02)       |  itof0.xyzw vf12, vf12
  if (vars.vi05 > vars.vi06_kick_zone_ptr + 6) { // HACK added
    auto vf28_load_temp = load_vector_data(vars.vi02);
    vars.vf28.x() = vf28_load_temp.x();
    vars.vf28.y() = vf28_load_temp.y();
    vars.vf28.w() = vf28_load_temp.w();
  }

  vars.vf12_root_pos_0 = itof0(vars.vf12_root_pos_0);

  //  div Q, vf01.x, vf15.w      |  mul.xyzw vf19, vf15, vf11
  m_q = m_tfrag_data.fog.x() / vars.vf15_loop_pos_1.w();
  vars.vf19_scaled_pos_3 = vars.vf15_loop_pos_1.elementwise_multiply(m_tfrag_data.hmge_scale);

  //  ibeq vi05, vi06, L134      |  miniz.w vf14, vf14, vf01
  bool take_branch = (vars.vi05 == vars.vi06_kick_zone_ptr);
  fmt::print("L130 prog: {} {}\n", vars.vi05, vars.vi06_kick_zone_ptr);
  vars.vf14_loop_pos_0.w() = std::min(vars.vf14_loop_pos_0.w(), m_tfrag_data.fog.z());

  //  sqi.xyzw vf13, vi06        |  clipw.xyz vf18, vf18
  store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf13_root_pos_1);
  vars.vi06_kick_zone_ptr++;
  m_clip_and_3ffff = clip_xyz_plus_minus(vars.vf18_scaled_pos_2);

  if (take_branch) {
    // kick zone is full, time for another kick (L134)
    m_ret_block = TFragJumper::L0x6B0_PART0_Z;
    m_next_block = TFragJumper::L122_KICK;
  } else {
    // kick zone is not full. we should continue
    m_next_block = TFragJumper::L0x6B0_PART0_Z;
  }
}

template <bool DEBUG>
void TFragment::exec_jumper_L131(const Prog6Inputs& in, Prog6Vars& vars) {
  //  lq.xyzw vf13, 0(vi04)      |  madday.xyzw ACC, vf07, vf12
  vars.vf13_root_pos_1 = load_vector_data(vars.vi04);
  m_acc += in.vf07_cam_mat_y * vars.vf12_root_pos_0.y();

  //  lq.xyzw vf21, 1(vi04)      |  maddz.xyzw vf12, vf08, vf12
  vars.vf21 = load_vector_data(vars.vi04 + 1);
  vars.vf12_root_pos_0 = m_acc + in.vf08_cam_mat_z * vars.vf12_root_pos_0.z();

  //  sqi.xyzw vf26, vi06        |  add.xyzw vf15, vf15, vf10
  store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf26);
  vars.vi06_kick_zone_ptr++;
  vars.vf15_loop_pos_1 += m_tfrag_data.hvdf_offset;

  //  sqi.xyzw vf22, vi06        |  ftoi4.xyzw vf14, vf14
  store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf22);
  vars.vi06_kick_zone_ptr++;
  vars.vf14_loop_pos_0 = ftoi4(vars.vf14_loop_pos_0);

  //  lq.xyw vf28, 0(vi02)       |  itof0.xyzw vf13, vf13
  fmt::print("vi02: 0x{:x}\n", vars.vi02);
  if (vars.vi05 > vars.vi06_kick_zone_ptr + 3) {  // HACK added
    auto vf28_load_temp = load_vector_data(vars.vi02);
    vars.vf28.x() = vf28_load_temp.x();
    vars.vf28.y() = vf28_load_temp.y();
    vars.vf28.w() = vf28_load_temp.w();
  }
  vars.vf13_root_pos_1 = itof0(vars.vf13_root_pos_1);

  //  div Q, vf01.x, vf12.w      |  mul.xyzw vf16, vf12, vf11
  m_q = m_tfrag_data.fog.x() / vars.vf12_root_pos_0.w();
  vars.vf16_scaled_pos_0 = vars.vf12_root_pos_0.elementwise_multiply(m_tfrag_data.hmge_scale);

  //  ibeq vi05, vi06, L135      |  miniz.w vf15, vf15, vf01
  fmt::print("L131 prog: {} {}\n", vars.vi05, vars.vi06_kick_zone_ptr);
  bool take_branch = (vars.vi05 == vars.vi06_kick_zone_ptr);
  vars.vf15_loop_pos_1.w() = std::min(vars.vf15_loop_pos_1.w(), m_tfrag_data.fog.z());

  //  sqi.xyzw vf14, vi06        |  clipw.xyz vf19, vf19
  store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf14_loop_pos_0);
  vars.vi06_kick_zone_ptr++;
  m_clip_and_3ffff = clip_xyz_plus_minus(vars.vf19_scaled_pos_3);

  if (take_branch) {
    // kick zone is full, time for another kick (L135)
    m_ret_block = TFragJumper::L0x6BF_PART0_W;
    m_next_block = TFragJumper::L122_KICK;
  } else {
    // kick zone is not full. we should continue
    m_next_block = TFragJumper::L0x6BF_PART0_W;
  }
}

template <bool DEBUG>
void TFragment::exec_jumper_L132(const Prog6Inputs& in, Prog6Vars& vars) {
  //  lq.xyzw vf14, 0(vi04)      |  madday.xyzw ACC, vf07, vf13
  vars.vf14_loop_pos_0 = load_vector_data(vars.vi04);  // bad here, in L0x6BF_PART0_W prev
  m_acc += in.vf07_cam_mat_y * vars.vf13_root_pos_1.y();

  //  lq.xyzw vf22, 1(vi04)      |  maddz.xyzw vf13, vf08, vf13
  vars.vf22 = load_vector_data(vars.vi04 + 1);
  vars.vf13_root_pos_1 = m_acc + in.vf08_cam_mat_z * vars.vf13_root_pos_1.z();

  //  sqi.xyzw vf27, vi06        |  add.xyzw vf12, vf12, vf10
  store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf27);
  vars.vi06_kick_zone_ptr++;
  vars.vf12_root_pos_0 += m_tfrag_data.hvdf_offset;

  //  sqi.xyzw vf23, vi06        |  ftoi4.xyzw vf15, vf15
  store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf23);
  vars.vi06_kick_zone_ptr++;
  vars.vf15_loop_pos_1 = ftoi4(vars.vf15_loop_pos_1);

  //  lq.xyw vf28, 0(vi02)       |  itof0.xyzw vf14, vf14
  if (vars.vi05 > vars.vi06_kick_zone_ptr) {  // HACK added
    auto vf28_load_temp = load_vector_data(vars.vi02);
    vars.vf28.x() = vf28_load_temp.x();
    vars.vf28.y() = vf28_load_temp.y();
    vars.vf28.w() = vf28_load_temp.w();
  }
  vars.vf14_loop_pos_0 = itof0(vars.vf14_loop_pos_0);

  //  div Q, vf01.x, vf13.w      |  mul.xyzw vf17, vf13, vf11
  m_q = m_tfrag_data.fog.x() / vars.vf13_root_pos_1.w();
  vars.vf17_scaled_pos_1 = vars.vf13_root_pos_1.elementwise_multiply(m_tfrag_data.hmge_scale);

  //  ibne vi05, vi06, L128      |  miniz.w vf12, vf12, vf01
  fmt::print("L133 prog: {} {}\n", vars.vi05, vars.vi06_kick_zone_ptr);
  bool take_branch = (vars.vi05 != vars.vi06_kick_zone_ptr);
  vars.vf12_root_pos_0.w() = std::min(vars.vf12_root_pos_0.w(), m_tfrag_data.fog.z());

  //  sqi.xyzw vf15, vi06        |  clipw.xyz vf16, vf16
  store_vector_kick_zone(vars.vi06_kick_zone_ptr, vars.vf15_loop_pos_1);
  vars.vi06_kick_zone_ptr++;
  m_clip_and_3ffff = clip_xyz_plus_minus(vars.vf16_scaled_pos_0);

  if (take_branch) {
    // kick zone isn't full, go to top of loop:
    m_next_block = TFragJumper::L128_PART0_X;
  } else {
    // kick zone is full, kick then restart
    m_next_block = TFragJumper::L122_KICK;
    m_ret_block = TFragJumper::L128_PART0_X;
  }

  //  b L122                     |  nop
  //  iaddiu vi15, vi00, 0x692   |  nop                      ;; L128
}

template <bool DEBUG>
void TFragment::exec_jumper_L122(const Prog6Inputs& in, Prog6Vars& vars) {
  // KICK ZONE!
  //  L122:
  //  fcset 0x0
  m_clip_and_3ffff = false;  // ??
                             //  iaddi vi07, vi00, -0x1
  vars.vi07 = -1;
  fmt::print("KICK blocks: vi12 = 0x{:x}\n", vars.vi12);
  //  iblez vi12, L123
  //  iaddi vi09, vi09, 0x1
  vars.vi09++;
  if (((s16)vars.vi12) > 0) {
    //  ior vi10, vi06, vi00
    vars.vi10 = vars.vi06_kick_zone_ptr;
    //  iadd vi01, vi12, vi12
    m_ptrs.vi01 = vars.vi12 + vars.vi12;
    //  iadd vi01, vi01, vi12
    m_ptrs.vi01 += vars.vi12;
    //  iadd vi05, vi06, vi01
    vars.vi05 = vars.vi06_kick_zone_ptr + m_ptrs.vi01;
    //  sqi.xyzw vf06, vi06
    store_gif_kick_zone(vars.vi06_kick_zone_ptr, m_tfrag_data.str_gif);
    vars.vi06_kick_zone_ptr++;
    //  isw.x vi12, -1(vi06)
    store_u32_kick_zone(vars.vi12, vars.vi06_kick_zone_ptr - 1, 0);
    //  jr vi15
    //  ilwr.x vi12, vi09
    vars.vi12 = ilw_data(vars.vi09, 0);
    fmt::print("didn't kick, vi12 now {}\n", vars.vi12);
    m_next_block = m_ret_block;
    return;
  }

  //  L123:
  //  ilw.y vi01, -1(vi09)
  m_ptrs.vi01 = ilw_data(vars.vi09 - 1, 1);
  //  ilw.z vi13, -1(vi09)
  vars.vi13 = ilw_data(vars.vi09 - 1, 2);
  //  ibeq vi00, vi12, L126
  //  ilwr.x vi14, vi10
  vars.vi14 = ilw_kick_zone(vars.vi10, 0);
  if (vars.vi12 != 0) {
    //  ibltz vi01, L124
    //  iaddiu vi12, vi12, 0x80
    vars.vi12 += 0x80;
    if (((s16)m_ptrs.vi01) >= 0) {
      //  iadd vi13, vi13, vi08
      vars.vi13 += vars.vi08;
      //  lqi.xyzw vf29, vi13
      vars.vf29 = load_vector_data(vars.vi13++);
      //  lqi.xyzw vf30, vi13
      vars.vf30 = load_vector_data(vars.vi13++);
      //  lqi.xyzw vf31, vi13
      vars.vf31 = load_vector_data(vars.vi13++);
      //  sqi.xyzw vf05, vi06
      store_gif_kick_zone(vars.vi06_kick_zone_ptr++, m_tfrag_data.ad_gif);
      //  sqi.xyzw vf29, vi06
      store_vector_kick_zone(vars.vi06_kick_zone_ptr++, vars.vf29);
      //  sqi.xyzw vf30, vi06
      store_vector_kick_zone(vars.vi06_kick_zone_ptr++, vars.vf30);
      //  sqi.xyzw vf31, vi06
      store_vector_kick_zone(vars.vi06_kick_zone_ptr++, vars.vf31);
      //  lqi.xyzw vf29, vi13
      vars.vf29 = load_vector_data(vars.vi13++);
      //  lqi.xyzw vf30, vi13
      vars.vf30 = load_vector_data(vars.vi13++);
      //  iadd vi01, vi12, vi12
      m_ptrs.vi01 = vars.vi12 + vars.vi12;
      //  iadd vi01, vi01, vi12
      m_ptrs.vi01 += vars.vi12;
      //  sqi.xyzw vf29, vi06
      store_vector_kick_zone(vars.vi06_kick_zone_ptr++, vars.vf29);
      //  sqi.xyzw vf30, vi06
      store_vector_kick_zone(vars.vi06_kick_zone_ptr++, vars.vf30);
      //  ior vi10, vi06, vi00
      vars.vi10 = vars.vi06_kick_zone_ptr;
      //  iadd vi05, vi06, vi01
      vars.vi05 = vars.vi06_kick_zone_ptr + m_ptrs.vi01;
      //  sqi.xyzw vf06, vi06
      store_gif_kick_zone(vars.vi06_kick_zone_ptr++, m_tfrag_data.str_gif);
      //  isw.x vi12, -1(vi06)
      store_u32_kick_zone(vars.vi12, vars.vi06_kick_zone_ptr - 1, 0);
      //  jr vi15
      //  ilwr.x vi12, vi09
      vars.vi12 = ilw_data(vars.vi09, 0);
      fmt::print("didn't kick 2, vi12 now {}\n", vars.vi12);
      m_next_block = m_ret_block;
      return;
    }

    //  L124:
    //  mtir vi01, vf24.w
    m_ptrs.vi01 = float_2_u32(vars.vf24.w());
    //  mtir vi06, vf03.y
    vars.vi06_kick_zone_ptr = m_ptrs.vf03_y;
    //  mr32.xyzw vf03, vf03
    auto temp = m_ptrs.vf03_x;
    m_ptrs.vf03_x = m_ptrs.vf03_y;
    m_ptrs.vf03_y = m_ptrs.vf03_z;
    m_ptrs.vf03_z = m_ptrs.vf03_w;
    m_ptrs.vf03_w = temp;

    //  iadd vi14, vi14, vi11
    vars.vi14 += vars.vi11;

    //  ibgez vi13, L125
    //  iswr.x vi14, vi10
    store_u32_kick_zone(vars.vi14, vars.vi10, 0);
    if (((s16)vars.vi13) < 0) {
      //  xgkick vi01
      XGKICK<DEBUG>(m_ptrs.vi01);
      //  ior vi10, vi06, vi00
      vars.vi10 = vars.vi06_kick_zone_ptr;  // xgkick delay slots, doesn't seem to matter.
      //  mfir.w vf24, vi06
      vars.vf24.w() = u32_2_float(vars.vi06_kick_zone_ptr);
      //  iadd vi01, vi12, vi12
      m_ptrs.vi01 = vars.vi12 + vars.vi12;
      //  iadd vi01, vi01, vi12
      m_ptrs.vi01 += vars.vi12;
      //  iadd vi05, vi06, vi01
      vars.vi05 = vars.vi06_kick_zone_ptr + m_ptrs.vi01;
      //  sqi.xyzw vf06, vi06
      store_gif_kick_zone(vars.vi06_kick_zone_ptr++, m_tfrag_data.str_gif);
      //  isw.x vi12, -1(vi06)
      store_u32_kick_zone(vars.vi12, vars.vi06_kick_zone_ptr - 1, 0);
      //  jr vi15
      //  ilwr.x vi12, vi09
      vars.vi12 = ilw_data(vars.vi09, 0);
      fmt::print("didn't kick 3, vi12 now {}\n", vars.vi12);
      m_next_block = m_ret_block;
      return;
    }

    //  L125:
    //  iadd vi13, vi13, vi08
    vars.vi13 += vars.vi08;
    //  xgkick vi01
    XGKICK<DEBUG>(m_ptrs.vi01);
    //  lqi.xyzw vf29, vi13
    vars.vf29 = load_vector_data(vars.vi13++);
    //  lqi.xyzw vf30, vi13
    vars.vf30 = load_vector_data(vars.vi13++);
    //  lqi.xyzw vf31, vi13
    vars.vf31 = load_vector_data(vars.vi13++);
    //  mfir.w vf24, vi06
    vars.vf24.w() = u32_2_float(vars.vi06_kick_zone_ptr);
    //  sqi.xyzw vf05, vi06
    store_gif_kick_zone(vars.vi06_kick_zone_ptr++, m_tfrag_data.ad_gif);
    //  sqi.xyzw vf29, vi06
    store_vector_kick_zone(vars.vi06_kick_zone_ptr++, vars.vf29);
    //  sqi.xyzw vf30, vi06
    store_vector_kick_zone(vars.vi06_kick_zone_ptr++, vars.vf30);
    //  sqi.xyzw vf31, vi06
    store_vector_kick_zone(vars.vi06_kick_zone_ptr++, vars.vf31);
    //  lqi.xyzw vf29, vi13
    vars.vf29 = load_vector_data(vars.vi13++);
    //  lqi.xyzw vf30, vi13
    vars.vf30 = load_vector_data(vars.vi13++);
    //  iadd vi01, vi12, vi12
    m_ptrs.vi01 = vars.vi12 + vars.vi12;
    //  iadd vi01, vi01, vi12
    m_ptrs.vi01 += vars.vi12;
    //  sqi.xyzw vf29, vi06
    store_vector_kick_zone(vars.vi06_kick_zone_ptr++, vars.vf29);
    //  sqi.xyzw vf30, vi06
    store_vector_kick_zone(vars.vi06_kick_zone_ptr++, vars.vf30);
    //  nop
    //  ior vi10, vi06, vi00
    vars.vi10 = vars.vi06_kick_zone_ptr;
    //  iadd vi05, vi06, vi01
    vars.vi05 = vars.vi06_kick_zone_ptr + m_ptrs.vi01;
    //  sqi.xyzw vf06, vi06
    store_gif_kick_zone(vars.vi06_kick_zone_ptr++, m_tfrag_data.str_gif);
    //  isw.x vi12, -1(vi06)
    store_u32_kick_zone(vars.vi12, vars.vi06_kick_zone_ptr - 1, 0);
    //  jr vi15
    //  ilwr.x vi12, vi09
    vars.vi12 = ilw_data(vars.vi09, 0);
    fmt::print("did kick, vi12 now {}\n", vars.vi12);
    m_next_block = m_ret_block;
    return;
  }

  //  L126:
  //  mtir vi01, vf24.w
  m_ptrs.vi01 = float_2_u32(vars.vf24.w());
  //  mr32.xyzw vf03, vf03
  auto temp = m_ptrs.vf03_x;
  m_ptrs.vf03_x = m_ptrs.vf03_y;
  m_ptrs.vf03_y = m_ptrs.vf03_z;
  m_ptrs.vf03_z = m_ptrs.vf03_w;
  m_ptrs.vf03_w = temp;
  //  iadd vi14, vi14, vi11
  vars.vi14 += vars.vi11;
  //  iswr.x vi14, vi10
  store_u32_kick_zone(vars.vi14, vars.vi10, 0);
  //  lq.xyzw vf04, 664(vi00)
  // todo don't think I needed that load of ambient
  XGKICK<DEBUG>(m_ptrs.vi01);
  //  xgkick vi01
  //  nop                        |  nop :e
  m_next_block = END_PROGRAM;

  //  nop                        |  nop
}

template <bool DEBUG>
void TFragment::XGKICK(u32 addr) {
  if (DEBUG) {
    ImGui::Text("XGKICK: %d", addr);
  }
}

template <bool DEBUG>
void TFragment::exec_program_6_process_second(const Prog6Inputs& in, Prog6Vars& vars) {}

std::string TFragData::print() const {
  std::string result;
  result += fmt::format("fog: {}\n", fog.to_string_aligned());
  result += fmt::format("val: {}\n", val.to_string_aligned());
  result += fmt::format("str-gif: {}\n", str_gif.print());
  result += fmt::format("fan-gif: {}\n", fan_gif.print());
  result += fmt::format("ad-gif: {}\n", ad_gif.print());
  result += fmt::format("hvdf_offset: {}\n", hvdf_offset.to_string_aligned());
  result += fmt::format("hmge_scale: {}\n", hmge_scale.to_string_aligned());
  result += fmt::format("invh_scale: {}\n", invh_scale.to_string_aligned());
  result += fmt::format("ambient: {}\n", ambient.to_string_aligned());
  result += fmt::format("guard: {}\n", guard.to_string_aligned());
  result += fmt::format("k0s[0]: {}\n", k0s[0].to_string_aligned());
  result += fmt::format("k0s[1]: {}\n", k0s[1].to_string_aligned());
  result += fmt::format("k1s[0]: {}\n", k1s[0].to_string_aligned());
  result += fmt::format("k1s[1]: {}\n", k1s[1].to_string_aligned());
  return result;
}
