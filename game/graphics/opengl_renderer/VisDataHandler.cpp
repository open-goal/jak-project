#include "VisDataHandler.h"

#include "third-party/imgui/imgui.h"

VisDataHandler::VisDataHandler(const std::string& name, int my_id) : BucketRenderer(name, my_id) {}

void VisDataHandler::draw_debug_window() {
  ImGui::Checkbox("Count Visible?", &m_count_vis);
  for (int i = 0; i < kMaxLevels; i++) {
    const auto& stats = m_stats[i];
    if (stats.has_vis) {
      if (m_count_vis) {
        ImGui::Text("[%d] %d visible", i, stats.num_visible);
      } else {
        ImGui::Text("[%d] has vis", i);
      }
    }
  }
}

int bitcount(const u8* data) {
  int result = 0;
  for (int byte = 0; byte < 16 * 128; byte++) {
    u8 val = data[byte];
    for (int i = 0; i < 8; i++) {
      if (val & 1) {
        result++;
      }
      val >>= 1;
    }
  }
  return result;
}

void VisDataHandler::render(DmaFollower& dma,
                            SharedRenderState* render_state,
                            ScopedProfilerNode& /*prof*/) {
  // reset stats
  for (auto& stats : m_stats) {
    stats = LevelStats();
  }
  if (dma.current_tag_offset() == render_state->next_bucket) {
    return;
  }

  // skip bucket start
  dma.read_and_advance();

  if (dma.current_tag_offset() == render_state->next_bucket) {
    return;
  }

  for (int i = 0; i < render_state->num_vis_to_copy; i++) {
    auto vis_data = dma.read_and_advance();
    u32 vif0 = vis_data.vif0();
    memcpy(render_state->fog_color.data(), &vif0, 4);
    if (vis_data.vifcode1().kind != VifCode::Kind::PC_PORT) {
      break;
    }
    if (vis_data.size_bytes == 128 * 16 && render_state->use_occlusion_culling) {
      memcpy(render_state->occlusion_vis[i].data, vis_data.data, 128 * 16);
      render_state->occlusion_vis[i].valid = true;
      m_stats[i].has_vis = true;
      if (m_count_vis) {
        m_stats[i].num_visible = bitcount(render_state->occlusion_vis[i].data);
      } else {
        m_stats[i].num_visible = -1;
      }
    } else {
      m_stats[i].has_vis = false;
      m_stats[i].num_visible = -1;
    }

    auto next = dma.read_and_advance();
    ASSERT(next.size_bytes == 0);
  }

  while (dma.current_tag_offset() != render_state->next_bucket) {
    dma.read_and_advance();
    ASSERT(false);
  }
}
