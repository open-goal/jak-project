#include "TextureConverter.h"

#include "common/texture/texture_conversion.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"

#include "fmt/core.h"

TextureConverter::TextureConverter() {
  m_vram.resize(4 * 1024 * 1024);
}

void TextureConverter::upload(const u8* data, u32 dest, u32 width, u32 height, u32 size_vram_words) {
  // Validation (optional)
  if ((size_vram_words == 0 && (width == 0 || height == 0)) || data == nullptr) {
    throw std::invalid_argument(
        "Invalid parameters: Provide either size_vram_words or width and height, and ensure data "
        "is not null.");
  }

  // Calculate Width and Height from Parameters
  int copy_width = (size_vram_words > 0) ? 128 : width;
  int copy_height = (size_vram_words > 0) ? (size_vram_words / copy_width) : height;

  for (int y = 0; y < copy_height; y++) {
    for (int x = 0; x < copy_width; x++) {
      // VRAM address (bytes)
      auto addr32 = psmct32_addr(x, y, copy_width) + dest * ((size_vram_words > 0) ? 4 : 256);
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
  int read_width = 64 * goal_tex_width;

  // Helper for CLUT addressing
  auto calculate_clut_address = [&](u8 value, int clut_psm) -> u32 {
    u32 clut_chunk = value / 16;
    u32 off_in_chunk = value % 16;
    u8 clx = (clut_chunk & 1) ? 8 : 0;
    u8 cly = (clut_chunk >> 1) * 2;
    if (off_in_chunk >= 8) {
      off_in_chunk -= 8;
      cly++;
    }
    clx += off_in_chunk;
    if (clut_psm == int(CPSM::PSMCT32)) {
      return psmct32_addr(clx, cly, 64) + clut_vram_addr * 256;
    } else if (clut_psm == int(CPSM::PSMCT16)) {
      return psmct16_addr(clx, cly, 64) + clut_vram_addr * 256;
    }
    ASSERT(false);  // Invalid CLUT format
    return 0;
  };

  // Pixel processing function
  auto process_pixel = [&](u32 x, u32 y, u32 addr, int clut_psm) -> u32 {
    u8 value = *(u8*)(m_vram.data() + addr);
    u32 clut_addr = calculate_clut_address(value, clut_psm);
    if (clut_psm == int(CPSM::PSMCT32)) {
      return *(u32*)(m_vram.data() + clut_addr);
    } else if (clut_psm == int(CPSM::PSMCT16)) {
      return rgba16_to_rgba32(*(u16*)(m_vram.data() + clut_addr));
    }
    ASSERT(false);
    return 0;
  };

  // Main loop
  for (u32 y = 0; y < h; y++) {
    for (u32 x = 0; x < w; x++) {
      u32 addr = 0;
      if (psm == int(PSM::PSMT8)) {
        addr = psmt8_addr(x, y, read_width) + vram_addr * 256;
      } else if (psm == int(PSM::PSMT4)) {
        auto addr4 = psmt4_addr_half_byte(x, y, read_width) + vram_addr * 512;
        u8 value = *(u8*)(m_vram.data() + addr4 / 2);
        value = (addr4 & 1) ? (value >> 4) : (value & 0x0F);
        addr = value;
      } else if (psm == int(PSM::PSMCT16) && clut_psm == 0) {
        addr = psmct16_addr(x, y, read_width) + vram_addr * 256;
        u16 value = *(u16*)(m_vram.data() + addr);
        u32 rgba32 = rgba16_to_rgba32(value);
        memcpy(result + out_offset, &rgba32, 4);
        out_offset += 4;
        continue;
      }

      // Process pixel through CLUT
      u32 rgba_value = process_pixel(x, y, addr, clut_psm);
      memcpy(result + out_offset, &rgba_value, 4);
      out_offset += 4;
    }
  }

  ASSERT(out_offset == expected_size_bytes);
}
