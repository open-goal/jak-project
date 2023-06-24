#include "TextureAnimator.h"

/*!
  (dma-buffer-add-gs-set-flusha dma-buf
              (scissor-1 (new 'static 'gs-scissor :scax1 (+ tex-width -1) :scay1 (+ tex-height -1)))
              (xyoffset-1 (new 'static 'gs-xy-offset :ofx #x8000 :ofy #x8000))
              ;; setting fbp here is what makes it draw to the texture, not frambeuffer.
              (frame-1 (new 'static 'gs-frame :fbw (/ (+ tex-width 63) 64) :fbp fbp-for-tex))
              (test-1 (-> anim test))
              (alpha-1 (-> anim alpha))
              (clamp-1 (-> anim clamp))
              (texa (new 'static 'gs-texa :ta0 #x80 :ta1 #x80))
              (zbuf-1 (new 'static 'gs-zbuf :zbp #x130 :psm (gs-psm ct24) :zmsk #x1))
              (texflush 0)
              )
 */

/*!
 * If the DMA is the initial set up for writing to a dest texture, get the data, advance dma, and
 * return true. Otherwise, return false and do not advance dma.
 */
bool try_getting_initial_texture_context(DestinationTextureEntry* out, DmaFollower& dma) {
  auto dmatag = dma.current_tag();
  if (dmatag.qwc != 10) {
    return false;
  }

  auto vif0 = dma.current_tag_vifcode0();
  if (vif0.kind != VifCode::Kind::FLUSHA) {
    return false;
  }

  auto vif1 = dma.current_tag_vifcode1();
  if (vif1.kind != VifCode::Kind::DIRECT) {
    return false;
  }

  return true;
}

enum PcTextureAnimCodes { FINISH_ARRAY = 13, ERASE_DEST_TEXTURE = 14, UPLOAD_CLUT_16_16 = 15 };

/*
(deftype texture-anim-pc-upload (structure)
  ((data pointer)
   (width uint32)
   (height uint32)
   (dest uint32)
   )
  )
 */

struct TextureAnimPcUpload {
  u32 data;
  u32 width;
  u32 height;
  u32 dest;
};

void TextureAnimator::handle_upload_clut_16_16(const DmaTransfer& tf, const u8* ee_mem) {
  printf("[tex anim] upload clut 16 16\n");
  ASSERT(tf.size_bytes == sizeof(TextureAnimPcUpload));
  auto* upload = (const TextureAnimPcUpload*)(tf.data);
  ASSERT(upload->width == 16);
  ASSERT(upload->height == 16);
  printf("  dest is 0x%x\n", upload->dest);
  auto& vram = m_vram_entries[upload->dest];
  vram.kind = VramEntry::Kind::CLUT16_16;
  vram.data.resize(16 * 16 * 4);
  memcpy(vram.data.data(), ee_mem + upload->data, vram.data.size());
}

void TextureAnimator::handle_erase_dest(DmaFollower& dma) {
  printf("[tex anim] erase destination texture\n");
  auto& out = m_result_textures.emplace_back();

  // first transfer will be a bunch of ad
  {
    auto ad_transfer = dma.read_and_advance();
    ASSERT(ad_transfer.size_bytes == 10 * 16);
    ASSERT(ad_transfer.vifcode0().kind == VifCode::Kind::FLUSHA);
    ASSERT(ad_transfer.vifcode1().kind == VifCode::Kind::DIRECT);
    const u64* ad_data = (u64*)(ad_transfer.data + 16);

    // for (int i = 0; i < 9; i++) {
    // printf(" ad: 0x%lx 0x%lx\n", ad_data[i * 2], ad_data[i * 2 + 1]);
    // }
    // 0 (scissor-1 (new 'static 'gs-scissor :scax1 (+ tex-width -1) :scay1 (+ tex-height -1)))
    ASSERT(ad_data[0 * 2 + 1] == (int)GsRegisterAddress::SCISSOR_1);
    GsScissor scissor(ad_data[0]);
    out.tex_width = scissor.x1() + 1;
    out.tex_height = scissor.y1() + 1;
    printf(" size: %d x %d\n", out.tex_width, out.tex_height);

    // 1 (xyoffset-1 (new 'static 'gs-xy-offset :ofx #x8000 :ofy #x8000))
    // 2 (frame-1 (new 'static 'gs-frame :fbw (/ (+ tex-width 63) 64) :fbp fbp-for-tex))
    ASSERT(ad_data[2 * 2 + 1] == (int)GsRegisterAddress::FRAME_1);
    GsFrame frame(ad_data[2 * 2]);
    out.dest_texture_address = 32 * frame.fbp();
    printf(" dest: 0x%x\n", out.dest_texture_address);

    // 3 (test-1 (-> anim test))
    ASSERT(ad_data[2 * 3 + 1] == (int)GsRegisterAddress::TEST_1);
    out.test = GsTest(ad_data[3 * 2]);
    fmt::print(" test: {}\n", out.test.print());

    // 4 (alpha-1 (-> anim alpha))
    ASSERT(ad_data[2 * 4 + 1] == (int)GsRegisterAddress::ALPHA_1);
    out.alpha = GsAlpha(ad_data[4 * 2]);
    fmt::print(" alpha: {}\n", out.alpha.print());

    // 5 (clamp-1 (-> anim clamp))
    ASSERT(ad_data[2 * 5 + 1] == (int)GsRegisterAddress::CLAMP_1);
    u64 creg = ad_data[5 * 2];
    out.clamp_u = creg & 0b001;
    out.clamp_v = creg & 0b100;
    u64 mask = ~0b101;
    ASSERT((creg & mask) == 0);
    fmt::print(" clamp: {} {}\n", out.clamp_u, out.clamp_v);

    // 6 (texa (new 'static 'gs-texa :ta0 #x80 :ta1 #x80))
    // 7 (zbuf-1 (new 'static 'gs-zbuf :zbp #x130 :psm (gs-psm ct24) :zmsk #x1))
    // 8 (texflush 0)
  }

  // next transfer is the erase. This is done with alpha blending off
  {
    auto clear_transfer = dma.read_and_advance();
    ASSERT(clear_transfer.size_bytes == 16 * 4);
    math::Vector<u32, 4> rgba_u32;
    memcpy(rgba_u32.data(), clear_transfer.data + 16, 16);
    out.rgba_clear = rgba_u32.cast<u8>();
    fmt::print(" clear: {}\n", out.rgba_clear.to_string_hex_byte());
  }
}

void TextureAnimator::handle_texture_anim_data(DmaFollower& dma, const u8* ee_mem) {
  printf("animator\n");
  m_result_textures.clear();

  while (true) {
    auto tf = dma.read_and_advance();
    auto vif0 = tf.vifcode0();
    if (vif0.kind == VifCode::Kind::PC_PORT) {
      switch (vif0.immediate) {
        case UPLOAD_CLUT_16_16:
          handle_upload_clut_16_16(tf, ee_mem);
          break;
        case ERASE_DEST_TEXTURE:
          handle_erase_dest(dma);
          break;
        case FINISH_ARRAY:
          return;
        default:
          fmt::print("bad imm: {}\n", vif0.immediate);
          ASSERT_NOT_REACHED();
      }
    } else {
      printf("[tex anim] unhandled\n");
    }
  }

  // TODO: do something with the result textures.
}