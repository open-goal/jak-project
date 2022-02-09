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
                     bool child_mode,
                     int level_id)
    : BucketRenderer(name, my_id),
      m_child_mode(child_mode),
      m_tree_kinds(trees),
      m_level_id(level_id) {
  for (auto& buf : m_buffered_data) {
    for (auto& x : buf.pad) {
      x = 0xff;
    }
  }
}

constexpr const char* level_names[] = {"bea", "cit", "dar", "fin", "int", "jub", "jun", "fic",
                                       "lav", "mai", "mis", "ogr", "rob", "rol", "sno", "sub",
                                       "sun", "swa", "tit", "tra", "vi1", "vi2", "vi3"};

void TFragment::render(DmaFollower& dma,
                       SharedRenderState* render_state,
                       ScopedProfilerNode& prof) {
  m_debug_string.clear();

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
    ASSERT(data0.vif1() == 0);
    ASSERT(data0.vif0() == 0);
    ASSERT(data0.size_bytes == 0);
  }

  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    // renderer didn't run, let's just get out of here.
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    ASSERT(dma.current_tag_offset() == render_state->next_bucket);
    return;
  }

  if (m_my_id == BucketId::TFRAG_LEVEL0) {
    DmaTransfer transfers[2];

    transfers[0] = dma.read_and_advance();
    auto next0 = dma.read_and_advance();
    ASSERT(next0.size_bytes == 0);
    transfers[1] = dma.read_and_advance();
    auto next1 = dma.read_and_advance();
    ASSERT(next1.size_bytes == 0);

    for (int i = 0; i < 2; i++) {
      if (transfers[i].size_bytes == 128 * 16) {
        if (render_state->use_occlusion_culling) {
          render_state->occlusion_vis[i].valid = true;
          memcpy(render_state->occlusion_vis[i].data, transfers[i].data, 128 * 16);
        }
      } else {
        ASSERT(transfers[i].size_bytes == 16);
      }
    }
  }

  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    // renderer didn't run, let's just get out of here.
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    ASSERT(dma.current_tag_offset() == render_state->next_bucket);
    return;
  }

  std::string level_name;
  while (looks_like_tfrag_init(dma)) {
    handle_initialization(dma);
    if (level_name.empty()) {
      level_name = m_pc_port_data.level_name;
    } else if (level_name != m_pc_port_data.level_name) {
      ASSERT(false);
    }

    while (looks_like_tfragment_dma(dma)) {
      dma.read_and_advance();
    }
  }

  while (dma.current_tag_offset() != render_state->next_bucket) {
    dma.read_and_advance();
  }

  ASSERT(!level_name.empty());
  {
    m_tfrag3.setup_for_level(m_tree_kinds, level_name, render_state);
    TfragRenderSettings settings;
    settings.hvdf_offset = m_tfrag_data.hvdf_offset;
    settings.fog_x = m_tfrag_data.fog.x();
    memcpy(settings.math_camera.data(), &m_buffered_data[0].pad[TFragDataMem::TFragMatrix0 * 16],
           64);
    settings.tree_idx = 0;
    if (render_state->occlusion_vis[m_level_id].valid) {
      settings.occlusion_culling = render_state->occlusion_vis[m_level_id].data;
    }

    for (int i = 0; i < 4; i++) {
      settings.planes[i] = m_pc_port_data.planes[i];
      render_state->camera_planes[i] = m_pc_port_data.planes[i];
    }
    render_state->has_camera_planes = true;

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
  }

  m_debug_string += fmt::format("fail: {}\n", dma.current_tag().print());

  while (dma.current_tag_offset() != render_state->next_bucket) {
    auto tag = dma.current_tag().print();
    auto data = dma.read_and_advance();
    m_debug_string +=
        fmt::format("DMA {} {} bytes, {}\n", tag, data.size_bytes, data.vifcode0().print());
  }

  if (m_hack_test_many_levels) {
    std::vector<tfrag3::TFragmentTreeKind> all_kinds = {
        tfrag3::TFragmentTreeKind::NORMAL, tfrag3::TFragmentTreeKind::TRANS,
        tfrag3::TFragmentTreeKind::DIRT,   tfrag3::TFragmentTreeKind::ICE,
        tfrag3::TFragmentTreeKind::LOWRES, tfrag3::TFragmentTreeKind::LOWRES_TRANS};
    for (int i = 0; i < HackManyLevels::NUM_LEVELS; i++) {
      if (m_many_level_render.level_enables[i]) {
        if (!m_many_level_render.tfrag_level_renderers[i]) {
          m_many_level_render.tfrag_level_renderers[i] = std::make_unique<Tfrag3>();
        }
        if (!m_many_level_render.tie_level_renderers[i]) {
          m_many_level_render.tie_level_renderers[i] = std::make_unique<Tie3>("tie", m_my_id, 0);
        }
        m_many_level_render.tfrag_level_renderers[i]->setup_for_level(all_kinds, level_names[i],
                                                                      render_state);
        m_many_level_render.tie_level_renderers[i]->setup_for_level(level_names[i], render_state);
        TfragRenderSettings settings;
        settings.hvdf_offset = m_tfrag_data.hvdf_offset;
        settings.fog_x = m_tfrag_data.fog.x();
        memcpy(settings.math_camera.data(),
               &m_buffered_data[0].pad[TFragDataMem::TFragMatrix0 * 16], 64);
        settings.tree_idx = 0;
        for (int j = 0; j < 8; j++) {
          settings.time_of_day_weights[j] = m_time_of_days[j];
        }

        auto t3prof = prof.make_scoped_child(level_names[i]);

        m_many_level_render.tfrag_level_renderers[i]->debug_render_all_trees_nolores(
            settings, render_state, t3prof);
        m_many_level_render.tie_level_renderers[i]->render_all_trees(settings, render_state,
                                                                     t3prof);
      }
    }
  }

  if (m_hack_scrambler) {
    render_state->loader.hack_scramble_textures();
    m_hack_scrambler = false;
  }
}

void TFragment::draw_debug_window() {
  if (ImGui::Button("Scrambler")) {
    m_hack_scrambler = true;
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

  m_tfrag3.draw_debug_window();

  ImGui::TextUnformatted(m_debug_string.data());
}

void TFragment::handle_initialization(DmaFollower& dma) {
  // Set up test (different between different renderers)
  auto setup_test = dma.read_and_advance();
  ASSERT(setup_test.vif0() == 0);
  ASSERT(setup_test.vifcode1().kind == VifCode::Kind::DIRECT);
  ASSERT(setup_test.vifcode1().immediate == 2);
  ASSERT(setup_test.size_bytes == 32);
  memcpy(m_test_setup, setup_test.data, 32);

  // matrix 0
  auto mat0_upload = dma.read_and_advance();
  unpack_to_stcycl(&m_buffered_data[0].pad[TFragDataMem::TFragMatrix0 * 16], mat0_upload,
                   VifCode::Kind::UNPACK_V4_32, 4, 4, 64, TFragDataMem::TFragMatrix0, false, false);

  // matrix 1
  auto mat1_upload = dma.read_and_advance();
  unpack_to_stcycl(&m_buffered_data[1].pad[TFragDataMem::TFragMatrix0 * 16], mat1_upload,
                   VifCode::Kind::UNPACK_V4_32, 4, 4, 64, TFragDataMem::TFragMatrix1, false, false);

  // data
  auto data_upload = dma.read_and_advance();
  unpack_to_stcycl(&m_tfrag_data, data_upload, VifCode::Kind::UNPACK_V4_32, 4, 4, sizeof(TFragData),
                   TFragDataMem::TFragFrameData, false, false);
  m_debug_string += fmt::format("Frame Data:\n {}\n", m_tfrag_data.print());

  // call the setup program
  auto mscal_setup = dma.read_and_advance();
  verify_mscal(mscal_setup, TFragProgMem::TFragSetup);

  auto pc_port_data = dma.read_and_advance();
  ASSERT(pc_port_data.size_bytes == sizeof(TfragPcPortData));
  memcpy(&m_pc_port_data, pc_port_data.data, sizeof(TfragPcPortData));
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
  ASSERT(db_setup.size_bytes == 0);
  ASSERT(db_setup.vifcode0().kind == VifCode::Kind::BASE &&
         db_setup.vifcode0().immediate == Buffer0_Start);
  ASSERT(db_setup.vifcode1().kind == VifCode::Kind::OFFSET &&
         db_setup.vifcode1().immediate == (Buffer1_Start - Buffer0_Start));
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
