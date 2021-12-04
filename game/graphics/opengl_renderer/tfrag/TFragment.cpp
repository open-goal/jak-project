#include "TFragment.h"

#include "third-party/imgui/imgui.h"
#include "game/graphics/opengl_renderer/dma_helpers.h"

namespace {
bool looks_like_tfragment_dma(const DmaFollower& follow) {
  return follow.current_tag_vifcode0().kind == VifCode::Kind::STCYCL;
}

bool looks_like_tfrag_init(const DmaFollower& follow) {
  return follow.current_tag_vifcode0().kind == VifCode::Kind::NOP &&
         follow.current_tag_vifcode1().kind == VifCode::Kind::DIRECT &&
         follow.current_tag_vifcode1().immediate == 2;
}
}  // namespace

TFragment::TFragment(const std::string& name,
                     BucketId my_id,
                     const std::vector<tfrag3::TFragmentTreeKind>& trees,
                     bool child_mode)
    : BucketRenderer(name, my_id),
      m_child_mode(child_mode),
      m_direct_renderer(fmt::format("{}.direct", name), my_id, 1024, DirectRenderer::Mode::NORMAL),
      m_buffered_renderer(my_id),
      m_tree_kinds(trees) {
  for (auto& buf : m_buffered_data) {
    for (auto& x : buf.pad) {
      x = 0xff;
    }
  }

  for (auto& x : m_kick_data.pad) {
    x = 0;
  }
}

constexpr const char* level_names[] = {"bea", "cit", "dar", "fin", "int", "jub", "jun", "fic",
                                       "lav", "mai", "mis", "ogr", "rob", "rol", "sno", "sub",
                                       "sun", "swa", "tit", "tra", "vi1", "vi2", "vi3"};

void TFragment::render(DmaFollower& dma,
                       SharedRenderState* render_state,
                       ScopedProfilerNode& prof) {
  m_debug_string.clear();
  m_frag_debug.clear();
  if (m_use_buffered_renderer) {
    m_buffered_renderer.reset_state();
  } else {
    m_direct_renderer.reset_state();
  }

  m_stats = {};

  if (!m_enabled) {
    while (dma.current_tag_offset() != render_state->next_bucket) {
      dma.read_and_advance();
    }
    return;
  }

  // First thing should be a NEXT with two nops.
  // unless we are a child, in which case our parent took this already.
  if (!m_child_mode) {
    auto data0 = dma.read_and_advance();
    assert(data0.vif1() == 0);
    assert(data0.vif0() == 0);
    assert(data0.size_bytes == 0);
  }

  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    // renderer didn't run, let's just get out of here.
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    assert(dma.current_tag_offset() == render_state->next_bucket);
    return;
  }

  if (m_extra_debug) {
    ImGui::Begin(fmt::format("{} extra", m_name).c_str());
  }

  if (m_use_tfrag3) {
    std::string level_name;
    while (looks_like_tfrag_init(dma)) {
      handle_initialization(dma, render_state, prof);
      if (level_name.empty()) {
        level_name = m_pc_port_data.level_name;
      } else if (level_name != m_pc_port_data.level_name) {
        assert(false);
      }

      while (looks_like_tfragment_dma(dma)) {
        dma.read_and_advance();
      }
    }

    while (dma.current_tag_offset() != render_state->next_bucket) {
      dma.read_and_advance();
    }

    assert(!level_name.empty());
    m_tfrag3.setup_for_level(level_name, render_state);
    Tfrag3::RenderSettings settings;
    settings.hvdf_offset = m_tfrag_data.hvdf_offset;
    settings.fog_x = m_tfrag_data.fog.x();
    memcpy(settings.math_camera.data(), &m_buffered_data[0].pad[TFragDataMem::TFragMatrix0 * 16],
           64);
    settings.tree_idx = 0;

    for (int i = 0; i < 4; i++) {
      settings.planes[i] = m_pc_port_data.planes[i];
    }

    if (m_override_time_of_day) {
      for (int i = 0; i < 8; i++) {
        settings.time_of_day_weights[i] = m_time_of_days[i];
      }
    } else {
      for (int i = 0; i < 8; i++) {
        settings.time_of_day_weights[i] =
            2 * (0xff & m_pc_port_data.itimes[i / 2].data()[2 * (i % 2)]) / 127.f;
      }
    }

    auto t3prof = prof.make_scoped_child("t3");
    m_tfrag3.render_matching_trees(m_tree_kinds, settings, render_state, t3prof);

  } else {
    while (looks_like_tfrag_init(dma)) {
      m_debug_string += "------------- START!\n";
      handle_initialization(dma, render_state, prof);
      int count = 0;
      // fmt::print("---------------------------------------START\n");

      while (looks_like_tfragment_dma(dma)) {
        m_stats.tfrag_dma_packets++;
        auto frag = dma.read_and_advance();
        m_stats.tfrag_bytes += frag.size_bytes;

        if (m_extra_debug) {
          handle_tfrag<true>(frag, render_state, prof);
        } else {
          handle_tfrag<false>(frag, render_state, prof);
        }
        if (m_max_draw >= 0 && count++ > m_max_draw) {
          break;
        }
      }

      if (dma.current_tag().qwc == 3) {
        dma.read_and_advance();
      }
      if (dma.current_tag().qwc == 0) {
        dma.read_and_advance();
      }
    }
  }

  if (m_extra_debug) {
    ImGui::End();
  }

  m_debug_string += fmt::format("fail: {}\n", dma.current_tag().print());

  if (m_use_buffered_renderer) {
    m_buffered_renderer.flush(render_state, prof);
  } else {
    m_direct_renderer.flush_pending(render_state, prof);
  }

  while (dma.current_tag_offset() != render_state->next_bucket) {
    auto tag = dma.current_tag().print();
    auto data = dma.read_and_advance();
    m_debug_string +=
        fmt::format("DMA {} {} bytes, {}\n", tag, data.size_bytes, data.vifcode0().print());
  }

  if (m_hack_test_many_levels) {
    for (int i = 0; i < HackManyLevels::NUM_LEVELS; i++) {
      if (m_many_level_render.level_enables[i]) {
        m_many_level_render.level_renderers[i].setup_for_level(level_names[i], render_state);
        Tfrag3::RenderSettings settings;
        settings.hvdf_offset = m_tfrag_data.hvdf_offset;
        settings.fog_x = m_tfrag_data.fog.x();
        memcpy(settings.math_camera.data(),
               &m_buffered_data[0].pad[TFragDataMem::TFragMatrix0 * 16], 64);
        settings.tree_idx = 0;
        for (int j = 0; j < 8; j++) {
          settings.time_of_day_weights[j] = m_time_of_days[j];
        }

        auto t3prof = prof.make_scoped_child(level_names[i]);

        m_many_level_render.level_renderers[i].debug_render_all_trees_nolores(settings,
                                                                              render_state, t3prof);
      }
    }
  }
}
void TFragment::draw_debug_window() {
  ImGui::Separator();
  ImGui::Checkbox("Extra Debug", &m_extra_debug);
  ImGui::InputInt("Max Draw", &m_max_draw);
  ImGui::SameLine();
  if (ImGui::Button("All")) {
    m_max_draw = -1;
  }
  ImGui::Checkbox("Manual Time of Day", &m_override_time_of_day);
  if (m_override_time_of_day) {
    for (int i = 0; i < 8; i++) {
      ImGui::SliderFloat(fmt::format("{}", i).c_str(), m_time_of_days + i, 0.f, 1.f);
    }
  }

  ImGui::Checkbox("Hack Test Many (danger)", &m_hack_test_many_levels);
  if (m_hack_test_many_levels) {
    for (int i = 0; i < HackManyLevels::NUM_LEVELS; i++) {
      ImGui::Checkbox(level_names[i], &m_many_level_render.level_enables[i]);
    }
  }

  ImGui::Checkbox("Use TFRAG3", &m_use_tfrag3);
  if (!m_use_tfrag3) {
    ImGui::Checkbox("Use Buffered Renderer", &m_use_buffered_renderer);
    ImGui::Checkbox("Skip MSCAL", &m_skip_mscals);
    ImGui::Checkbox("Skip XGKICK", &m_skip_xgkick);
    ImGui::Checkbox("Prog8 hack", &m_prog8_with_prog6);
    ImGui::Checkbox("Prog10 hack", &m_prog10_with_prog6);
    ImGui::Checkbox("Prog18 hack", &m_prog18_with_prog6);
    ImGui::Checkbox("Others with prog6", &m_all_with_prog6);
    ImGui::Text("packets: %d", m_stats.tfrag_dma_packets);
    ImGui::Text("frag bytes: %d", m_stats.tfrag_bytes);
    ImGui::Text("errors: %d", m_stats.error_packets);
    for (int prog = 0; prog < 12; prog++) {
      ImGui::Text("  prog %d: %d calls\n", prog, m_stats.per_program[prog].calls);
    }

    if (!m_use_buffered_renderer && ImGui::TreeNode("direct")) {
      m_direct_renderer.draw_debug_window();
      ImGui::TreePop();
    }

    if (m_use_buffered_renderer && ImGui::TreeNode("buffered")) {
      m_buffered_renderer.draw_debug_window();
      ImGui::TreePop();
    }
  } else {
    m_tfrag3.draw_debug_window();
  }

  ImGui::TextUnformatted(m_debug_string.data());
}

void TFragment::handle_initialization(DmaFollower& dma,
                                      SharedRenderState* render_state,
                                      ScopedProfilerNode& prof) {
  // Set up test (different between different renderers)
  auto setup_test = dma.read_and_advance();
  assert(setup_test.vif0() == 0);
  assert(setup_test.vifcode1().kind == VifCode::Kind::DIRECT);
  assert(setup_test.vifcode1().immediate == 2);
  assert(setup_test.size_bytes == 32);
  memcpy(m_test_setup, setup_test.data, 32);
  if (m_use_buffered_renderer) {
    m_buffered_renderer.add_gif_data_sized(m_test_setup, 32);
  } else {
    m_direct_renderer.render_gif(m_test_setup, 32, render_state, prof);
  }

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

  auto pc_port_data = dma.read_and_advance();
  assert(pc_port_data.size_bytes == sizeof(PcPortData));
  memcpy(&m_pc_port_data, pc_port_data.data, sizeof(PcPortData));
  m_pc_port_data.level_name[11] = '\0';

  for (int i = 0; i < 4; i++) {
    m_debug_string += fmt::format("p[{}]: {}\n", i, m_pc_port_data.planes[i].to_string_aligned());
  }

  for (int i = 0; i < 4; i++) {
    m_debug_string += fmt::format("t[{}]: {:x} {:x} {:x} {:x}\n", i, m_pc_port_data.itimes[i].x(),
                                  m_pc_port_data.itimes[i].y(), m_pc_port_data.itimes[i].z(),
                                  m_pc_port_data.itimes[i].w());
  }

  m_debug_string +=
      fmt::format("level: {}, tree: {}\n", m_pc_port_data.level_name, m_pc_port_data.tree_idx);

  // setup double buffering.
  auto db_setup = dma.read_and_advance();
  assert(db_setup.size_bytes == 0);
  assert(db_setup.vifcode0().kind == VifCode::Kind::BASE &&
         db_setup.vifcode0().immediate == Buffer0_Start);
  assert(db_setup.vifcode1().kind == VifCode::Kind::OFFSET &&
         db_setup.vifcode1().immediate == (Buffer1_Start - Buffer0_Start));
}

template <bool DEBUG>
void TFragment::handle_tfrag(const DmaTransfer& dma,
                             SharedRenderState* render_state,
                             ScopedProfilerNode& prof) {
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

  // fmt::print("START vif -> {} (mod {})\n", second_vif.print(), stmod);
  switch (second_vif.kind) {
    case VifCode::Kind::NOP:
      // do nothing!
      break;
    case VifCode::Kind::UNPACK_V4_8:
      offset_into_data = handle_unpack_v4_8_mode0(second_vif, dma, offset_into_data, cl, wl);
      break;
    case VifCode::Kind::MSCAL:
      if (!m_skip_mscals) {
        handle_mscal<DEBUG>(second_vif, render_state, prof);
      }
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
    // fmt::print("vif -> {} (mod {}) {}/{} #x{:x}\n", code.print(), stmod, offset_into_data,
    // dma.size_bytes, dma.data_offset);
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
          // fmt::print("  row: {}\n", vec.to_string_aligned().c_str());
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
    if (DEBUG) {
      ImGui::Text("END");
    }

    assert(stmod == 0);
  }
}

template <bool DEBUG>
void TFragment::handle_mscal(const VifCode& code,
                             SharedRenderState* render_state,
                             ScopedProfilerNode& prof) {
  if (DEBUG) {
    ImGui::TextColored(ImVec4(0.3, 0.8, 0.3, 1.0), "MSCAL: %d", code.immediate);
  }

  int prog_id = code.immediate / 2;
  if (prog_id >= NUM_PROGRAMS) {
    fmt::print("bad program: {}\n", prog_id);
    assert(false);
  }
  m_stats.per_program[prog_id].calls++;

  switch (code.immediate) {
    case 12:
    case 6:
      exec_program_6<DEBUG>(render_state, prof);
      break;
    case 8:
      if (m_prog8_with_prog6) {
        exec_program_6<DEBUG>(render_state, prof);
      } else {
        m_stats.error_mscals++;
      }
      break;
    case 10:
      if (m_prog10_with_prog6) {
        exec_program_6<DEBUG>(render_state, prof);
      } else {
        m_stats.error_mscals++;
      }
      break;
    case 18:
      if (m_prog18_with_prog6) {
        exec_program_6<DEBUG>(render_state, prof);
      } else {
        m_stats.error_mscals++;
      }
      break;
    default:
      if (m_all_with_prog6) {
        exec_program_6<DEBUG>(render_state, prof);
      } else {
        m_stats.error_mscals++;
        if (DEBUG) {
          ImGui::TextColored(ImVec4(0.8, 0.8, 0.3, 1.0), "  UNHANDLED");
        }
      }

      break;
  }
}

void TFragment::flip_buffers() {
  m_uploading_buffer ^= 1;
}

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
