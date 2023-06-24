#include "TextureAnimator.h"

void TextureAnimator::handle_texture_anim_data(DmaFollower& dma) {
  auto vif0 = dma.current_tag_vifcode0();
  while (vif0.kind != VifCode::Kind::PC_PORT || vif0.immediate != 13) {
    dma.read_and_advance();
    vif0 = dma.current_tag_vifcode0();
  }
  dma.read_and_advance();
}