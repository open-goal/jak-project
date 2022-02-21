#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"

class GenericRenderer: public BucketRenderer {
 public:
  GenericRenderer(const std::string& name, BucketId my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;
 private:
  u32 unpack32_4(const VifCodeUnpack& up, const u8* data, u32 imm);
  u32 unpack32_3(const VifCodeUnpack& up, const u8* data, u32 imm);
  u32 unpack8_4(const VifCodeUnpack& up, const u8* data, u32 imm);
  u32 unpack16_2(const VifCodeUnpack& up, const u8* data, u32 imm);

  void mscal(int imm);
  void handle_dma_stream(const u8* data, u32 bytes);
  int m_skipped_tags = 0;
  DirectRenderer m_direct;
  std::string m_debug;

  struct {
    u32 row[4];
    u32 stcycl = 0;
  } m_vu;

  struct alignas(16) BufferMemory {
    u8 data[1024 * 16];
  } m_buffer;
};
