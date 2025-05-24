#include "TextureConverter.h"

#include "common/texture/texture_conversion.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"

#include "fmt/core.h"

TextureConverter::TextureConverter() {
  m_vram.resize(4 * 1024 * 1024);
}

void TextureConverter::upload(const u8* data, u32 dest, u32 size_vram_words) {
  // all textures are copied to vram 128 pixels wide, regardless of actual width
  int copy_width = 128;
  // scale the copy height to be whatever it needs to be to transfer the right amount of data.
  int copy_height = size_vram_words / copy_width;

  for (int y = 0; y < copy_height; y++) {
    for (int x = 0; x < copy_width; x++) {
      // VRAM address (bytes)
      auto addr32 = psmct32_addr(x, y, copy_width) + dest * 4;
      *(u32*)(m_vram.data() + addr32) = *((const u32*)(data) + (x + y * copy_width));
    }
  }
}

void TextureConverter::upload_width(const u8* data, u32 dest, u32 width, u32 height) {
  for (u32 y = 0; y < height; y++) {
    for (u32 x = 0; x < width; x++) {
      // VRAM address (bytes)
      auto addr32 = psmct32_addr(x, y, width) + dest * 256;
      *(u32*)(m_vram.data() + addr32) = *((const u32*)(data) + (x + y * width));
    }
  }
}

void TextureConverter::download_rgba8888(u8* result,
                                         u32 vram_addr,
                                         u32 goal_tex_width,
                                         u32 w,
                                         u32 h,
                                         u32 psm,
                                         u32 clut_psm,
                                         u32 clut_vram_addr,
                                         u32 expected_size_bytes) {
  u32 out_offset = 0;
  if (psm == int(PSM::PSMT8) && clut_psm == int(CPSM::PSMCT32)) {
    // width is like the TEX0 register, in 64 texel units.
    // not sure what the other widths are yet.
    int read_width = 64 * goal_tex_width;
    // loop over pixels in output texture image
    for (u32 y = 0; y < h; y++) {
      for (u32 x = 0; x < w; x++) {
        // read as the PSMT8 type. The dest field tells us a block offset.
        auto addr8 = psmt8_addr(x, y, read_width) + vram_addr * 256;
        u8 value = *(u8*)(m_vram.data() + addr8);

        // there's yet another scramble from the CLUT. The palette index turns into an X, Y value
        // See GS manual 2.7.3 CLUT Storage Mode, IDTEX8 in CSM1 mode.
        u32 clut_chunk = value / 16;
        u32 off_in_chunk = value % 16;
        u8 clx = 0, cly = 0;
        if (clut_chunk & 1) {
          clx = 8;
        }
        cly = (clut_chunk >> 1) * 2;
        if (off_in_chunk >= 8) {
          off_in_chunk -= 8;
          cly++;
        }
        clx += off_in_chunk;

        // the x, y CLUT value is looked up in PSMCT32 mode
        u32 clut_addr = psmct32_addr(clx, cly, 64) + clut_vram_addr * 256;
        u32 clut_value = *(u32*)(m_vram.data() + clut_addr);
        memcpy(result + out_offset, &clut_value, 4);
        out_offset += 4;
      }
    }

  } else if (psm == int(PSM::PSMT8) && clut_psm == int(CPSM::PSMCT16)) {
    // width is like the TEX0 register, in 64 texel units.
    // not sure what the other widths are yet.
    int read_width = 64 * goal_tex_width;

    // loop over pixels in output texture image
    for (u32 y = 0; y < h; y++) {
      for (u32 x = 0; x < w; x++) {
        // read as the PSMT8 type. The dest field tells us a block offset.
        auto addr8 = psmt8_addr(x, y, read_width) + vram_addr * 256;
        u8 value = *(u8*)(m_vram.data() + addr8);

        // there's yet another scramble from the CLUT. The palette index turns into an X, Y value
        // See GS manual 2.7.3 CLUT Storage Mode, IDTEX8 in CSM1 mode.
        u32 clut_chunk = value / 16;
        u32 off_in_chunk = value % 16;
        u8 clx = 0, cly = 0;
        if (clut_chunk & 1) {
          clx = 8;
        }
        cly = (clut_chunk >> 1) * 2;
        if (off_in_chunk >= 8) {
          off_in_chunk -= 8;
          cly++;
        }
        clx += off_in_chunk;

        // the x, y CLUT value is looked up in PSMCT32 mode
        u32 clut_addr = psmct16_addr(clx, cly, 64) + clut_vram_addr * 256;
        u32 clut_value = *(u16*)(m_vram.data() + clut_addr);
        u32 rgba32 = rgba16_to_rgba32(clut_value);
        memcpy(result + out_offset, &rgba32, 4);
        out_offset += 4;
      }
    }

  } else if (psm == int(PSM::PSMT4) && clut_psm == int(CPSM::PSMCT16)) {
    // width is like the TEX0 register, in 64 texel units.
    // not sure what the other widths are yet.
    int read_width = 64 * goal_tex_width;

    // loop over pixels in output texture image
    for (u32 y = 0; y < h; y++) {
      for (u32 x = 0; x < w; x++) {
        // read as the PSMT4 type, use half byte addressing
        auto addr4 = psmt4_addr_half_byte(x, y, read_width) + vram_addr * 512;

        // read (half bytes)
        u8 value = *(u8*)(m_vram.data() + addr4 / 2);
        if (addr4 & 1) {
          value >>= 4;
        } else {
          value = value & 0x0f;
        }

        // there's yet another scramble from the CLUT. The palette index turns into an X, Y value
        // See GS manual 2.7.3 CLUT Storage Mode, IDTEX4 in CSM1 mode.

        u8 clx = value & 0x7;
        u8 cly = value >> 3;

        // the x, y CLUT value is looked up in PSMCT16 mode
        u32 clut_addr = psmct16_addr(clx, cly, 64) + clut_vram_addr * 256;
        u32 clut_value = *(u16*)(m_vram.data() + clut_addr);
        u32 rgba32 = rgba16_to_rgba32(clut_value);
        memcpy(result + out_offset, &rgba32, 4);
        out_offset += 4;
      }
    }
  } else if (psm == int(PSM::PSMT4) && clut_psm == int(CPSM::PSMCT32)) {
    // width is like the TEX0 register, in 64 texel units.
    // not sure what the other widths are yet.
    int read_width = 64 * goal_tex_width;

    // loop over pixels in output texture image
    for (u32 y = 0; y < h; y++) {
      for (u32 x = 0; x < w; x++) {
        // read as the PSMT4 type, use half byte addressing
        auto addr4 = psmt4_addr_half_byte(x, y, read_width) + vram_addr * 512;

        // read (half bytes)
        u8 value = *(u8*)(m_vram.data() + addr4 / 2);
        if (addr4 & 1) {
          value >>= 4;
        } else {
          value = value & 0x0f;
        }

        // there's yet another scramble from the CLUT. The palette index turns into an X, Y value
        // See GS manual 2.7.3 CLUT Storage Mode, IDTEX4 in CSM1 mode.

        u8 clx = value & 0x7;
        u8 cly = value >> 3;

        // the x, y CLUT value is looked up in PSMCT16 mode
        u32 clut_addr = psmct32_addr(clx, cly, 64) + clut_vram_addr * 256;
        u32 clut_value = *(u32*)(m_vram.data() + clut_addr);
        //        fmt::print("{} {}\n", value, clut_value);
        memcpy(result + out_offset, &clut_value, 4);
        out_offset += 4;
      }
    }
  } else if (psm == int(PSM::PSMCT16) && clut_psm == 0) {
    // plain 16-bit texture
    // not a clut.
    // will store output pixels, rgba (8888)

    // width is like the TEX0 register, in 64 texel units.
    // not sure what the other widths are yet.
    int read_width = 64 * goal_tex_width;

    // loop over pixels in output texture image
    for (u32 y = 0; y < h; y++) {
      for (u32 x = 0; x < w; x++) {
        // read as the PSMT8 type. The dest field tells us a block offset.
        auto addr8 = psmct16_addr(x, y, read_width) + vram_addr * 256;
        u16 value = *(u16*)(m_vram.data() + addr8);
        u32 val32 = rgba16_to_rgba32(value);
        memcpy(result + out_offset, &val32, 4);
        out_offset += 4;
      }
    }
  }

  else {
    ASSERT(false);
  }

  ASSERT(out_offset == expected_size_bytes);
}
