#pragma once

#include "common/dma/dma_chain_read.h"

class TextureAnimator {
 public:
  void handle_texture_anim_data(DmaFollower& dma);
};
