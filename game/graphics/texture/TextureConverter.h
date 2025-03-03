#pragma once

#include <vector>

#include "common/common_types.h"

class TextureConverter {
 public:
  TextureConverter();

  /**
   * @brief Copies PS2 texture data into simulated VRAM.
   *
   * Copies texture data from the provided source pointer (`data`) into the simulated
   * PlayStation 2 VRAM (`m_vram`). The texture size can be defined using either
   * `width` and `height` or `size_vram_words`.
   *
   * @param data Pointer to the source texture data.
   * @param dest Destination address in the simulated VRAM.
   * @param width (Optional) Texture width in texels. Used if `size_vram_words` is not provided.
   * @param height (Optional) Texture height in texels. Used if `size_vram_words` is not provided.
   * @param size_vram_words (Optional) Texture size in VRAM words. Assumes a width of 128 texels.
   */
  void upload(const u8* data, u32 dest, u32 width = 0, u32 height = 0, u32 size_vram_words = 0);

  /**
   * @brief Converts PS2 texture data from simulated VRAM into RGBA8888 format.
   *
   * This function processes PS2 texture data stored in simulated VRAM and converts it
   * into the RGBA8888 format, storing the result in the provided buffer (`result`).
   * It supports multiple PS2 formats, including PSMT8, PSMT4, and PSMCT16, and handles
   * CLUT-based textures by accessing the appropriate color lookup table (CLUT).
   *
   * @param result Pointer to the output buffer where RGBA8888 data will be stored.
   * @param vram_addr Starting address in the simulated VRAM for the texture data.
   * @param goal_tex_width Width of the texture in 64-texel units, as defined by PS2 format.
   * @param w Width of the texture in texels.
   * @param h Height of the texture in texels.
   * @param psm PS2 pixel storage format (e.g., PSMT8, PSMT4, PSMCT16).
   * @param clut_psm PS2 CLUT format (e.g., PSMCT32, PSMCT16).
   * @param clut_vram_addr Address of the CLUT in simulated VRAM, if applicable.
   * @param expected_size_bytes Expected size of the output buffer in bytes for validation.
   */
  void download_rgba8888(u8* result,
                         u32 vram_addr,
                         u32 goal_tex_width,
                         u32 w,
                         u32 h,
                         u32 psm,
                         u32 clut_psm,
                         u32 clut_vram_addr,
                         u32 expected_size_bytes);


 private:
  std::vector<u8> m_vram;
};
