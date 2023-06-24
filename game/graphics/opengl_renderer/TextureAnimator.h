#pragma once

#include <unordered_map>
#include <vector>

#include "common/dma/dma_chain_read.h"
#include "common/dma/gs.h"
#include "common/math/Vector.h"


struct VramEntry {
  enum class Kind { CLUT16_16 } kind;
  std::vector<u8> data;
};

struct DestinationTextureEntry {
  int tex_width;
  int tex_height;
  int dest_texture_address;
  GsTest test;
  GsAlpha alpha;
  bool clamp_u;
  bool clamp_v;

  math::Vector<u8, 4> rgba_clear;
};

class TextureAnimator {
 public:
  void handle_texture_anim_data(DmaFollower& dma, const u8* ee_mem);

 private:
  void handle_upload_clut_16_16(const DmaTransfer& tf, const u8* ee_mem);
  void handle_erase_dest(DmaFollower& dma);

  std::unordered_map<u32, VramEntry> m_vram_entries;
  std::vector<DestinationTextureEntry> m_result_textures;
};
