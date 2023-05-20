#include "TFragment.h"

#include "game/graphics/opengl_renderer/dma_helpers.h"

#include "third-party/imgui/imgui.h"

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
                     int my_id,
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

void TFragment::render(DmaFollower& dma,
                       SharedRenderState* render_state,
                       ScopedProfilerNode& prof) {
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
    ASSERT(data0.vifcode1().kind == VifCode::Kind::NOP);
    ASSERT(data0.vif0() == 0 || data0.vifcode0().kind == VifCode::Kind::MARK);
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

  if (m_my_id == render_state->bucket_for_vis_copy &&
      dma.current_tag_vifcode1().kind == VifCode::Kind::PC_PORT) {
    DmaTransfer transfers[20];

    for (int i = 0; i < render_state->num_vis_to_copy; i++) {
      transfers[i] = dma.read_and_advance();
      auto next0 = dma.read_and_advance();
      ASSERT(next0.size_bytes == 0);
    }

    for (int i = 0; i < render_state->num_vis_to_copy; i++) {
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

  if (level_name.empty()) {
    return;
  }
  {
    m_tfrag3.setup_for_level(m_tree_kinds, level_name, render_state);
    TfragRenderSettings settings;
    settings.hvdf_offset = m_tfrag_data.hvdf_offset;
    settings.fog = m_tfrag_data.fog;
    memcpy(settings.math_camera.data(), &m_buffered_data[0].pad[TFragDataMem::TFragMatrix0 * 16],
           64);
    settings.tree_idx = 0;
    if (render_state->occlusion_vis[m_level_id].valid) {
      settings.occlusion_culling = render_state->occlusion_vis[m_level_id].data;
    }

    update_render_state_from_pc_settings(render_state, m_pc_port_data);

    for (int i = 0; i < 4; i++) {
      settings.planes[i] = m_pc_port_data.planes[i];
      settings.itimes[i] = m_pc_port_data.itimes[i];
    }

    auto t3prof = prof.make_scoped_child("t3");
    m_tfrag3.render_matching_trees(m_tfrag3.lod(), m_tree_kinds, settings, render_state, t3prof);
  }

  while (dma.current_tag_offset() != render_state->next_bucket) {
    auto tag = dma.current_tag().print();
    dma.read_and_advance();
  }
}

void TFragment::draw_debug_window() {
  m_tfrag3.draw_debug_window();
}

void TFragment::init_shaders(ShaderLibrary& shaders) {
  m_tfrag3.init_shaders(shaders);
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

  // call the setup program
  auto mscal_setup = dma.read_and_advance();
  verify_mscal(mscal_setup, TFragProgMem::TFragSetup);

  auto pc_port_data = dma.read_and_advance();
  ASSERT(pc_port_data.size_bytes == sizeof(TfragPcPortData));
  memcpy(&m_pc_port_data, pc_port_data.data, sizeof(TfragPcPortData));
  m_pc_port_data.level_name[11] = '\0';

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
