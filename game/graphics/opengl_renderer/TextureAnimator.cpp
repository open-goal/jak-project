#include "TextureAnimator.h"

#include "common/global_profiler/GlobalProfiler.h"
#include "common/log/log.h"
#include "common/texture/texture_slots.h"
#include "common/util/FileUtil.h"
#include "common/util/Timer.h"

#include "game/graphics/opengl_renderer/slime_lut.h"
#include "game/graphics/texture/TexturePool.h"

#include "third-party/imgui/imgui.h"

// #define dprintf(...) printf(__VA_ARGS__)
// #define dfmt(...) fmt::print(__VA_ARGS__)
#define dprintf(...)
#define dfmt(...)

// -- Texture Animations
// The game has a number of "texture animation arrays".
// On the original PS2, there wasn't enough VRAM to hold all textures for a frame, so they would
// upload a single "tpage" at a time. Along with this, they would dynamically generate some animated
// textures. Each tpage has an associated texture animation array.

// -- Our approach
// This part of the code has turned out to be pretty bad in terms of performance.
// We also have the challenge of actually getting all the renderers to look at the right animated
// textures, which is tricky because we rewrote them for jak 1, which doesn't have this.
//
// So there's a lot of tricks here to try to speed things up. We modified the GOAL code to work
// better with this code. We have three different approaches to handling a texture animation array:
// - Emulation (slowest). This basically pretends to be the PS2, and is the most flexible. It reads
//   the DMA and maps it to OpenGL operations right now it's used for the clouds (though slightly
//   wrong).
// - Clut-blending. This special cases animations which are just blends between CLUTs.
//   We optimize this by only doing work if the blend weights change (they didn't on PS2 because
//   they don't have the vram to store the texture). We also avoid the use of render-to-texture.
//   Jak's hair/skin/fingers always use this texture.
// - "Fixed Animation". For animations that use only basic features, we have a way to run them
//   entirely in C++. This avoids repeated switches between framebuffers, and lets us precompute
//   more stuff.

// -- OpenGL performance.
// So there's a lot of stupid-looking OpenGL stuff going on here.
// The motivation for this is to avoid an issue where some operations take about 5-10ms.
// As far as I can tell, this slow operation is actually the driver forcing this thread to sync
// with some internal stuff. It seems to be triggered on:
// - deleting a texture that was used on the previous frame (so in use by the driver thread).
//   this is a "safe" operation, but I suspect it forces the driver thread to synchronize).
// - glTexImage2D to modify a texture with a different sized texture. (likely hits same case as
//   above)

// TODO:
//  clouds aren't really working right. The final operation of move rb to ba is a guess.
//  then it's actually treated as a palette texture, but we don't really do this.
//  This breaks the fade-out/thresholding, and likely the colors. But it still looks vaguely like
//  clouds.
void debug_save_opengl_texture(const std::string& out, GLuint texture) {
  glBindTexture(GL_TEXTURE_2D, texture);
  int w, h;
  glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, &w);
  glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, &h);
  lg::print("saving texture with size {} x {}\n", w, h);
  std::vector<u8> data(w * h * 4);
  glGetTexImage(GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV, data.data());
  file_util::write_rgba_png(out, data.data(), w, h);
}

/*!
 * A simple list of preallocated textures by size. If a texture needs to be resized, it's faster
 * to swap to a different OpenGL texture from this pool than glTexImage2D with a different size.
 */
OpenGLTexturePool::OpenGLTexturePool(GameVersion version) {
  struct Alloc {
    u64 w, h, n;
  };
  // list of sizes to preallocate: {width, height, count}.
  PerGameVersion<std::vector<Alloc>> tex_allocs{{{4, 4, 2},
                                                 {4, 64, 2},
                                                 {16, 16, 5},
                                                 {32, 16, 1},
                                                 {32, 32, 10},
                                                 {32, 64, 1},
                                                 {64, 32, 6},
                                                 {64, 64, 30},
                                                 {64, 128, 4},
                                                 {128, 128, 10},
                                                 {256, 1, 2},
                                                 {256, 256, 7}},
                                                {{4, 4, 2},
                                                 {4, 64, 2},
                                                 {16, 16, 5},
                                                 {32, 16, 1},
                                                 {32, 32, 10},
                                                 {32, 64, 1},
                                                 {64, 32, 6},
                                                 {64, 64, 30},
                                                 {64, 128, 4},
                                                 {128, 128, 10},
                                                 {256, 1, 2},
                                                 {256, 256, 7}},
                                                {{4, 4, 3},
                                                 {4, 64, 6},
                                                 {16, 16, 5},
                                                 {32, 16, 1},
                                                 {32, 32, 20},
                                                 {32, 64, 1},
                                                 {64, 32, 15},
                                                 {64, 64, 85},
                                                 {64, 128, 4},
                                                 {128, 128, 185},
                                                 {256, 1, 2},
                                                 {256, 256, 7}}};
  for (const auto& a : tex_allocs[version]) {
    auto& l = textures[(a.w << 32) | a.h];
    l.resize(a.n);
    glGenTextures(a.n, l.data());
    for (auto t : l) {
      glBindTexture(GL_TEXTURE_2D, t);
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, a.w, a.h, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
                   nullptr);
    }
  }
}

OpenGLTexturePool::~OpenGLTexturePool() {
  for (auto& [_, l] : textures) {
    glDeleteTextures(l.size(), l.data());
  }
}

/*!
 * Get a preallocated texture with the given size, or fatal error if we are out.
 */
GLuint OpenGLTexturePool::allocate(u64 w, u64 h) {
  const u64 key = (w << 32) | h;
  const auto& it = textures.find(key);
  if (it == textures.end()) {
    // Note: this is a bit of an abuse to support both Japanese subtitles (variable size), and the
    // "emulated" cloud textures (preallocated to avoid the performance issue described at the top
    // of the file). For now, warn when this happens, just so we don't miss a case of this getting
    // spammed during normal gameplay (bad for performance). Note that all of this can get massively
    // simplified once clouds are moved to C++. This is just a hack to keep the current clouds
    // working. (they are wrong and slow, but look better than nothing)
    lg::warn("OpenGLTexturePool creating texture for {} x {}", w, h);
    GLuint slot;
    glGenTextures(1, &slot);
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, slot);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV, nullptr);
    return slot;
  }

  if (it->second.empty()) {
    lg::die("OpenGLTexturePool needs more entries for {} x {}", w, h);
  }

  auto ret = it->second.back();
  it->second.pop_back();
  return ret;
}

/*!
 * Return a texture to the pool. The size must be provided.
 */
void OpenGLTexturePool::free(GLuint texture, u64 w, u64 h) {
  textures[(w << 32) | h].push_back(texture);
}

/*!
 * Get an index-format texture from the given tfrag3 level.
 * In cases where multiple original-game-levels both provide a texture, but the data is different,
 * prefer the one from the given level.
 * This is slow and intended to be used an init time.
 */
const tfrag3::IndexTexture* itex_by_name(const tfrag3::Level* level,
                                         const std::string& name,
                                         const std::optional<std::string>& level_name) {
  const tfrag3::IndexTexture* ret = nullptr;
  for (const auto& t : level->index_textures) {
    bool match = t.name == name;
    if (level_name && match) {
      match =
          std::find(t.level_names.begin(), t.level_names.end(), *level_name) != t.level_names.end();
      if (!match && false) {
        lg::warn("rejecting {} because it wasn't in desired level {}, but was in:", t.name,
                 *level_name);
        for (auto& l : t.level_names) {
          lg::warn("  {}", l);
        }
      }
    }
    if (match) {
      if (ret) {
        lg::error("Multiple index textures named {}", name);
        ASSERT(ret->color_table == t.color_table);
        ASSERT(ret->index_data == t.index_data);
      }
      ret = &t;
    }
  }
  if (!ret) {
    lg::die("no index texture named {}", name);
  } else {
    // lg::info("got idx: {}", name);
  }
  return ret;
}

/*!
 * Get a RGBA format texture by name from the given tfrag3 level. Slow, and intended for init time.
 */
const tfrag3::Texture* tex_by_name(const tfrag3::Level* level, const std::string& name) {
  const tfrag3::Texture* ret = nullptr;
  for (const auto& t : level->textures) {
    if (t.debug_name == name) {
      if (ret) {
        lg::error("Multiple textures named {}", name);
        ASSERT(ret->data == t.data);
      }
      ret = &t;
    }
  }
  if (!ret) {
    lg::error("no texture named {}", name);
    for (const auto& t : level->textures) {
      lg::print("texture: {}\n", t.debug_name);
    }
    lg::die("no texture named {}", name);
  } else {
    // lg::info("got idx: {}", name);
  }
  return ret;
}

/*!
 * Get a texture animation slot index for the given name. Fatal error if there is no animated
 * texture slot with this name. Slow, and intended for init time.
 */
int output_slot_by_idx(GameVersion version, const std::string& name) {
  const std::vector<std::string>* v = nullptr;
  switch (version) {
    case GameVersion::Jak2:
      v = &jak2_animated_texture_slots();
      break;
    case GameVersion::Jak3:
      v = &jak3_animated_texture_slots();
      break;
    default:
    case GameVersion::Jak1:
      ASSERT_NOT_REACHED();
  }

  for (size_t i = 0; i < v->size(); i++) {
    if ((*v)[i] == name) {
      return i;
    }
  }
  ASSERT_NOT_REACHED();
}

/*!
 * Upload a texture and generate mipmaps. Assumes the usual RGBA format.
 */
void opengl_upload_texture(GLint dest, const void* data, int w, int h) {
  glBindTexture(GL_TEXTURE_2D, dest);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV, data);
  glGenerateMipmap(GL_TEXTURE_2D);
  float aniso = 0.0f;
  glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY, &aniso);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY, aniso);
  glBindTexture(GL_TEXTURE_2D, 0);
}

/*!
 * Upload a texture and generate mipmaps. Assumes the usual RGBA format.
 * The size of the destination and source texture don't need to match.
 */
void opengl_upload_resize_texture(FramebufferTexturePair& fbt,
                                  const void* data,
                                  const math::Vector2<int>& data_size,
                                  ShaderLibrary& shaders) {
  {
    FramebufferTexturePairContext ctx(fbt);
    GLuint temp_texture = 0;
    GLuint vao = 0;
    GLuint vertex_buffer = 0;
    glGenTextures(1, &temp_texture);
    glBindTexture(GL_TEXTURE_2D, temp_texture);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, data_size.x(), data_size.y(), 0, GL_RGBA,
                 GL_UNSIGNED_INT_8_8_8_8_REV, data);

    glGenVertexArrays(1, &vao);
    glGenBuffers(1, &vertex_buffer);
    glBindVertexArray(vao);

    struct Vertex {
      float x, y;
    };

    std::array<Vertex, 4> vertices = {
        Vertex{-1, -1},
        Vertex{-1, 1},
        Vertex{1, -1},
        Vertex{1, 1},
    };

    glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(Vertex) * 4, vertices.data(), GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0,               // location 0 in the shader
                          2,               // 2 floats per vert
                          GL_FLOAT,        // floats
                          GL_TRUE,         // normalized, ignored,
                          sizeof(Vertex),  //
                          nullptr          //
    );

    auto& shader = shaders[ShaderId::PLAIN_TEXTURE];
    shader.activate();
    glUniform1i(glGetUniformLocation(shader.id(), "tex_T0"), 0);
    glDisable(GL_BLEND);
    glDisable(GL_DEPTH_TEST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, temp_texture);
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
    glDeleteVertexArrays(1, &vao);
    glDeleteBuffers(1, &vertex_buffer);
    glDeleteTextures(1, &temp_texture);
  }
  glBindTexture(GL_TEXTURE_2D, fbt.texture());
  glGenerateMipmap(GL_TEXTURE_2D);
}

/*!
 * Utility class to grab CLUTs from the source textures, blend them, and produce a destination RGBA
 * texture using the index data in dest.
 */
ClutBlender::ClutBlender(const std::string& dest,
                         const std::array<std::string, 2>& sources,
                         const std::optional<std::string>& level_name,
                         const tfrag3::Level* level,
                         OpenGLTexturePool* tpool) {
  // find the destination texture
  m_dest = itex_by_name(level, dest, level_name);
  // find the clut source textures
  for (int i = 0; i < 2; i++) {
    m_cluts[i] = &itex_by_name(level, sources[i], level_name)->color_table;
    m_current_weights[i] = 0;
  }

  // opengl texture that we'll write to
  m_texture = tpool->allocate(m_dest->w, m_dest->h);
  m_temp_rgba.resize(m_dest->w * m_dest->h);

  // default to the first one.
  std::vector<float> init_weights(m_current_weights.size(), 0);
  init_weights.at(0) = 1.f;
  run(init_weights.data());
}

/*!
 * Blend cluts and create an output texture.
 */
GLuint ClutBlender::run(const float* weights) {
  bool needs_run = false;

  // check if weights changed or not.
  for (size_t i = 0; i < m_current_weights.size(); i++) {
    if (weights[i] != m_current_weights[i]) {
      needs_run = true;
      break;
    }
  }

  if (!needs_run) {
    return m_texture;
  }

  // update weights
  for (size_t i = 0; i < m_current_weights.size(); i++) {
    m_current_weights[i] = weights[i];
  }

  // blend cluts
  for (int i = 0; i < 256; i++) {
    math::Vector4f v = math::Vector4f::zero();
    for (size_t j = 0; j < m_current_weights.size(); j++) {
      v += (*m_cluts[j])[i].cast<float>() * m_current_weights[j];
    }
    m_temp_clut[i] = v.cast<u8>();
  }

  // do texture lookups
  for (size_t i = 0; i < m_temp_rgba.size(); i++) {
    memcpy(&m_temp_rgba[i], m_temp_clut[m_dest->index_data[i]].data(), 4);
  }

  // send to GPU.
  opengl_upload_texture(m_texture, m_temp_rgba.data(), m_dest->w, m_dest->h);

  return m_texture;
}

/*!
 * Utility class to show what happens if you take a PSM32 texture, upload it as PSM32, then read it
 * back as PSM8. Byte i from the input data ends up in destinations_per_byte[i].
 */
Psm32ToPsm8Scrambler::Psm32ToPsm8Scrambler(int w, int h, int write_tex_width, int read_tex_width) {
  struct InAddr {
    int x = -1, y = -1, c = -1;
  };
  struct OutAddr {
    int x = -1, y = -1;
  };

  std::vector<InAddr> vram_from_in(w * h * 4);
  std::vector<OutAddr> vram_from_out(w * h * 4);

  // loop over pixels in input
  for (int y = 0; y < h; y++) {
    for (int x = 0; x < w; x++) {
      int byte_addr = psmct32_addr(x, y, write_tex_width);
      for (int c = 0; c < 4; c++) {
        auto& s = vram_from_in.at(byte_addr + c);
        s.x = x;
        s.y = y;
        s.c = c;
      }
    }
  }

  // output
  for (int y = 0; y < h * 2; y++) {
    for (int x = 0; x < w * 2; x++) {
      int byte_addr = psmt8_addr(x, y, read_tex_width);
      auto& s = vram_from_out.at(byte_addr);
      s.x = x;
      s.y = y;
    }
  }

  destinations_per_byte.resize(4 * w * h);
  for (size_t i = 0; i < vram_from_out.size(); i++) {
    auto& in = vram_from_in.at(i);
    auto& out = vram_from_out.at(i);
    if (in.c >= 0) {
      destinations_per_byte.at(in.c + in.x * 4 + in.y * 4 * w) = out.x + out.y * w * 2;
    }
  }
}

const std::vector<std::string>& animated_texture_slots(GameVersion version) {
  switch (version) {
    case GameVersion::Jak2:
      return jak2_animated_texture_slots();
    case GameVersion::Jak3:
      return jak3_animated_texture_slots();
    default:
      ASSERT_NOT_REACHED();
  }
}

TextureAnimator::TextureAnimator(ShaderLibrary& shaders,
                                 const tfrag3::Level* common_level,
                                 GameVersion version)
    : m_common_level(common_level),
      m_opengl_texture_pool(version),
      m_psm32_to_psm8_8_8(8, 8, 8, 64),
      m_psm32_to_psm8_16_16(16, 16, 16, 64),
      m_psm32_to_psm8_32_32(32, 32, 16, 64),
      m_psm32_to_psm8_64_64(64, 64, 64, 64),
      m_version(version),
      m_sky_blend_texture(kFinalSkyTextureSize, kFinalSkyTextureSize, GL_UNSIGNED_INT_8_8_8_8_REV),
      m_sky_final_texture(kFinalSkyTextureSize, kFinalSkyTextureSize, GL_UNSIGNED_INT_8_8_8_8_REV),
      m_sky_hires_blend_texture(kFinalSkyHiresTextureSize,
                                kFinalSkyHiresTextureSize,
                                GL_UNSIGNED_INT_8_8_8_8_REV),
      m_sky_hires_final_texture(kFinalSkyHiresTextureSize,
                                kFinalSkyHiresTextureSize,
                                GL_UNSIGNED_INT_8_8_8_8_REV),
      m_slime_blend_texture(kFinalSlimeTextureSize,
                            kFinalSlimeTextureSize,
                            GL_UNSIGNED_INT_8_8_8_8_REV),
      m_slime_final_texture(kFinalSlimeTextureSize,
                            kFinalSlimeTextureSize,
                            GL_UNSIGNED_INT_8_8_8_8_REV),
      m_slime_final_scroll_texture(kFinalSlimeTextureSize,
                                   kFinalSlimeTextureSize,
                                   GL_UNSIGNED_INT_8_8_8_8_REV),
      m_shaders(&shaders) {
  glGenVertexArrays(1, &m_vao);
  glGenBuffers(1, &m_vertex_buffer);
  glBindVertexArray(m_vao);

  // The TextureAnimator does a lot of "draws" which are just a single quad, so we create a 4-vertex
  // buffer. It turns out that just storing the vertex index in the vertex, then indexing into a
  // uniform buffer is faster to update. (though this may be driver specific?)
  std::array<Vertex, 4> vertices = {Vertex{0, 0, 0, 0}, Vertex{1, 0, 0, 0}, Vertex{2, 0, 0, 0},
                                    Vertex{3, 0, 0, 0}};
  glBindBuffer(GL_ARRAY_BUFFER, m_vertex_buffer);
  // static draw - we don't update this buffer.
  glBufferData(GL_ARRAY_BUFFER, sizeof(Vertex) * 4, vertices.data(), GL_STATIC_DRAW);

  // single integer index parameter
  glEnableVertexAttribArray(0);
  glVertexAttribIPointer(0,               // location 0 in the shader
                         1,               // 1 per vertex
                         GL_INT,          //
                         sizeof(Vertex),  //
                         nullptr          //
  );
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);

  auto& shader = shaders[ShaderId::TEX_ANIM];
  m_shader_id = shader.id();
  m_uniforms.rgba = glGetUniformLocation(shader.id(), "rgba");
  m_uniforms.enable_tex = glGetUniformLocation(shader.id(), "enable_tex");
  m_uniforms.set_alpha = glGetUniformLocation(shader.id(), "set_alpha");
  m_uniforms.positions = glGetUniformLocation(shader.id(), "positions");
  m_uniforms.uvs = glGetUniformLocation(shader.id(), "uvs");
  m_uniforms.channel_scramble = glGetUniformLocation(shader.id(), "channel_scramble");
  m_uniforms.tcc = glGetUniformLocation(shader.id(), "tcc");
  m_uniforms.alpha_multiply = glGetUniformLocation(shader.id(), "alpha_multiply");
  m_uniforms.minimum = glGetUniformLocation(shader.id(), "minimum");
  m_uniforms.maximum = glGetUniformLocation(shader.id(), "maximum");
  m_uniforms.slime_scroll = glGetUniformLocation(shader.id(), "slime_scroll");

  // create a single "dummy texture" with all 0 data.
  // this is faster and easier than switching shaders to one without texturing, and is used
  // only rarely
  glGenTextures(1, &m_dummy_texture);
  glBindTexture(GL_TEXTURE_2D, m_dummy_texture);
  std::vector<u32> data(16 * 16);
  u32 c0 = 0xa0303030;
  u32 c1 = 0xa0e0e0e0;
  for (int i = 0; i < 16; i++) {
    for (int j = 0; j < 16; j++) {
      data[i * 16 + j] = (((i / 4) & 1) ^ ((j / 4) & 1)) ? c1 : c0;
    }
  }
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 16, 16, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
               data.data());
  glGenerateMipmap(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, 0);

  // create the slime LUT texture
  glGenTextures(1, &m_slime_lut_texture);
  glBindTexture(GL_TEXTURE_1D, m_slime_lut_texture);
  std::vector<u8> slime_data;
  for (auto x : kSlimeLutData) {
    slime_data.push_back(x * 255);
  }
  glTexImage1D(GL_TEXTURE_1D, 0, GL_RGBA, 256, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
               slime_data.data());
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glBindTexture(GL_TEXTURE_2D, 0);

  shader.activate();

  // generate CLUT table.
  for (int i = 0; i < 256; i++) {
    u32 clut_chunk = i / 16;
    u32 off_in_chunk = i % 16;
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
    m_index_to_clut_addr[i] = clx + cly * 16;
  }

  m_public_output_slots.resize(animated_texture_slots(m_version).size(), m_dummy_texture);
  m_private_output_slots = m_public_output_slots;
  m_output_debug_flags.resize(animated_texture_slots(m_version).size());

  // animation-specific stuff
  setup_texture_anims_common();
  switch (m_version) {
    case GameVersion::Jak2:
      setup_texture_anims_jak2();
      break;
    case GameVersion::Jak3:
      setup_texture_anims_jak3();
      break;
    default:
      ASSERT_NOT_REACHED();
  }
  setup_sky();

  // Patch up references to animated textures
  std::map<std::string, u64> name_to_slot;
  for (const auto& anim_array : m_fixed_anim_arrays) {
    for (const auto& anim : anim_array.anims) {
      name_to_slot[anim.def.tex_name] = anim.dest_slot;
    }
  }

  for (auto& anim_array : m_fixed_anim_arrays) {
    for (auto& anim : anim_array.anims) {
      for (size_t i = 0; i < anim.src_textures.size(); i++) {
        const auto& it = name_to_slot.find(anim.def.layers.at(i).tex_name);
        if (it != name_to_slot.end()) {
          lg::error("Patching ref to {} in {}", it->first, anim.def.tex_name);
          anim.src_textures[i].is_anim_slot = true;
          anim.src_textures[i].idx = it->second;
        }
      }
    }
  }
}

/*!
 * Add a fixed texture animator for the given definition. Returns an index that can later be used to
 * run it.
 */
int TextureAnimator::create_fixed_anim_array(const std::vector<FixedAnimDef>& defs) {
  int ret = m_fixed_anim_arrays.size();
  auto& anim_array = m_fixed_anim_arrays.emplace_back();

  for (const auto& def : defs) {
    auto& anim = anim_array.anims.emplace_back();
    anim.def = def;

    // set up the destination texture.
    anim.dest_slot = output_slot_by_idx(m_version, anim.def.tex_name);
    auto* dtex = tex_by_name(m_common_level, anim.def.tex_name);
    if (anim.def.override_size) {
      anim.fbt.emplace(anim.def.override_size->x(), anim.def.override_size->y(),
                       GL_UNSIGNED_INT_8_8_8_8_REV);
      // I think there's kind of a bug here - if the game accesses a resized texture before
      // the animation is run once, it can end up using the wrong size.
      // For PC, we just resize the texture to the new size at startup, which avoids the texture
      // changing sizes at runtime.
      opengl_upload_resize_texture(*anim.fbt, dtex->data.data(), {dtex->w, dtex->h}, *m_shaders);
    } else {
      anim.fbt.emplace(dtex->w, dtex->h, GL_UNSIGNED_INT_8_8_8_8_REV);
      opengl_upload_texture(anim.fbt->texture(), dtex->data.data(), dtex->w, dtex->h);
    }

    m_private_output_slots.at(anim.dest_slot) = anim.fbt->texture();

    // set up the source textures
    for (const auto& layer : def.layers) {
      auto* stex = tex_by_name(m_common_level, layer.tex_name);
      GLint gl_texture = m_opengl_texture_pool.allocate(stex->w, stex->h);
      FixedAnimSource src;
      src.idx = gl_texture;
      src.is_anim_slot = false;
      anim.src_textures.push_back(src);
      opengl_upload_texture(gl_texture, stex->data.data(), stex->w, stex->h);
    }

    // set up dynamic data
    anim.dynamic_data.resize(def.layers.size());
  }

  return ret;
}

void imgui_show_tex(GLuint tex) {
  glBindTexture(GL_TEXTURE_2D, tex);
  int w, h;
  glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, &w);
  glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, &h);
  ImGui::Image((void*)(u64)tex, ImVec2(w, h));
}

void TextureAnimator::draw_debug_window() {
  ImGui::Checkbox("fast-scrambler", &m_debug.use_fast_scrambler);

  ImGui::Text("Slime:");
  ImGui::Text("dests %d %d", m_debug_slime_input.dest, m_debug_slime_input.scroll_dest);
  for (int i = 0; i < 9; i++) {
    ImGui::Text("Time[%d]: %f", i, m_debug_slime_input.times[i]);
  }
  imgui_show_tex(m_slime_final_texture.texture());
  imgui_show_tex(m_slime_final_scroll_texture.texture());

  ImGui::Text("Sky:");
  ImGui::Text("Fog Height: %f", m_debug_sky_input.fog_height);
  ImGui::Text("Cloud minmax: %f %f", m_debug_sky_input.cloud_min, m_debug_sky_input.cloud_max);
  for (int i = 0; i < 11; i++) {
    ImGui::Text("Time[%d]: %f", i, m_debug_sky_input.times[i]);
  }
  ImGui::Text("Dest %d", m_debug_sky_input.cloud_dest);

  imgui_show_tex(m_sky_blend_texture.texture());
  imgui_show_tex(m_sky_final_texture.texture());

  imgui_show_tex(m_sky_hires_blend_texture.texture());
  imgui_show_tex(m_sky_hires_final_texture.texture());

  for (int i = 0; i < kNumSkyHiresNoiseLayers; ++i) {
    ImGui::Text("Noise Hires %d", i);
    imgui_show_tex(m_sky_hires_noise_textures[i].old_tex);
    ImGui::SameLine();
    imgui_show_tex(m_sky_hires_noise_textures[i].new_tex);
  }

  auto& slots = animated_texture_slots(m_version);
  for (size_t i = 0; i < slots.size(); i++) {
    ImGui::Text("Slot %d %s (%d)", (int)i, slots[i].c_str(), (int)m_private_output_slots[i]);
    imgui_show_tex(m_private_output_slots[i]);
    ImGui::Checkbox(fmt::format("mark {}", i).c_str(), &m_output_debug_flags.at(i).b);
  }
  glBindTexture(GL_TEXTURE_2D, 0);
}

void TextureAnimator::copy_private_to_public() {
  auto& slots = animated_texture_slots(m_version);
  for (size_t i = 0; i < slots.size(); i++) {
    if (m_output_debug_flags[i].b) {
      m_public_output_slots[i] = m_dummy_texture;
    } else {
      m_public_output_slots[i] = m_private_output_slots[i];
    }
  }
}

/*!
 * Create a clut-blending animator. Returns an index that can later be used to run it.
 */
int TextureAnimator::create_clut_blender_group(const std::vector<std::string>& textures,
                                               const std::string& suffix0,
                                               const std::string& suffix1,
                                               const std::optional<std::string>& dgo,
                                               bool add_to_pool) {
  int ret = m_clut_blender_groups.size();
  m_clut_blender_groups.emplace_back();
  add_to_clut_blender_group(ret, textures, suffix0, suffix1, dgo);
  if (add_to_pool) {
    m_clut_blender_groups.back().move_to_pool = true;
  }
  return ret;
}

/*!
 * Add a texture to an existing blender group created with create_clut_blender_group.
 */
void TextureAnimator::add_to_clut_blender_group(int idx,
                                                const std::vector<std::string>& textures,
                                                const std::string& suffix0,
                                                const std::string& suffix1,
                                                const std::optional<std::string>& dgo) {
  auto& grp = m_clut_blender_groups.at(idx);
  for (auto& prefix : textures) {
    grp.blenders.emplace_back(prefix,
                              std::array<std::string, 2>{prefix + suffix0, prefix + suffix1}, dgo,
                              m_common_level, &m_opengl_texture_pool);
    grp.outputs.push_back(output_slot_by_idx(m_version, prefix));
    m_private_output_slots.at(grp.outputs.back()) = grp.blenders.back().texture();
  }
}

TextureAnimator::~TextureAnimator() {
  glDeleteVertexArrays(1, &m_vao);
  glDeleteBuffers(1, &m_vertex_buffer);
  glDeleteTextures(1, &m_dummy_texture);
}

GLuint TextureAnimator::get_by_slot(int idx) {
  ASSERT(idx >= 0 && idx < (int)m_public_output_slots.size());
  return m_public_output_slots[idx];
}

// IDs sent from GOAL telling us what texture operation to perform.
enum class PcTextureAnimCodesJak2 : u16 {
  FINISH_ARRAY = 13,
  ERASE_DEST_TEXTURE = 14,
  UPLOAD_CLUT_16_16 = 15,
  GENERIC_UPLOAD = 16,
  SET_SHADER = 17,
  DRAW = 18,
  DARKJAK = 22,
  PRISON_JAK = 23,
  ORACLE_JAK = 24,
  NEST_JAK = 25,
  KOR_TRANSFORM = 26,
  SKULL_GEM = 27,
  BOMB = 28,
  CAS_CONVEYOR = 29,
  SECURITY = 30,
  WATERFALL = 31,
  WATERFALL_B = 32,
  LAVA = 33,
  LAVA_B = 34,
  STADIUMB = 35,
  FORTRESS_PRIS = 36,
  FORTRESS_WARP = 37,
  METKOR = 38,
  SHIELD = 39,
  KREW_HOLO = 40,
  CLOUDS_AND_FOG = 41,
  SLIME = 42,
  CLOUDS_HIRES = 43,
};

struct FixedAnimInfoJak2 {
  PcTextureAnimCodesJak2 code = (PcTextureAnimCodesJak2)0;
  std::string name;
  int anim_array_idx = -1;
};

FixedAnimInfoJak2 anim_code_to_info(PcTextureAnimCodesJak2 code, const TextureAnimator& animator) {
  ASSERT(code > (PcTextureAnimCodesJak2)0);
  FixedAnimInfoJak2 anim;
  anim.code = code;
  switch (code) {
    case PcTextureAnimCodesJak2::SKULL_GEM: {
      anim.name = "skull-gem";
      anim.anim_array_idx = animator.m_skull_gem_fixed_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak2::BOMB: {
      anim.name = "bomb";
      anim.anim_array_idx = animator.m_bomb_fixed_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak2::CAS_CONVEYOR: {
      anim.name = "cas-conveyor";
      anim.anim_array_idx = animator.m_cas_conveyor_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak2::SECURITY: {
      anim.name = "security";
      anim.anim_array_idx = animator.m_security_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak2::WATERFALL: {
      anim.name = "waterfall";
      anim.anim_array_idx = animator.m_waterfall_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak2::WATERFALL_B: {
      anim.name = "waterfall-b";
      anim.anim_array_idx = animator.m_waterfall_b_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak2::LAVA: {
      anim.name = "lava";
      anim.anim_array_idx = animator.m_lava_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak2::LAVA_B: {
      anim.name = "lava-b";
      anim.anim_array_idx = animator.m_lava_b_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak2::STADIUMB: {
      anim.name = "stadiumb";
      anim.anim_array_idx = animator.m_stadiumb_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak2::FORTRESS_PRIS: {
      anim.name = "fort-pris";
      anim.anim_array_idx = animator.m_fortress_pris_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak2::FORTRESS_WARP: {
      anim.name = "fort-warp";
      anim.anim_array_idx = animator.m_fortress_warp_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak2::METKOR: {
      anim.name = "metkor";
      anim.anim_array_idx = animator.m_metkor_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak2::SHIELD: {
      anim.name = "shield";
      anim.anim_array_idx = animator.m_shield_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak2::KREW_HOLO: {
      anim.name = "krew-holo";
      anim.anim_array_idx = animator.m_krew_holo_anim_array_idx;
    } break;
    default:
      anim.name = "unknown";
      lg::error("Unknown texture anim code {}", (int)code);
  }
  return anim;
}

enum class PcTextureAnimCodesJak3 : u16 {
  FINISH_ARRAY = 13,
  ERASE_DEST_TEXTURE = 14,
  UPLOAD_CLUT_16_16 = 15,
  GENERIC_UPLOAD = 16,
  SET_SHADER = 17,
  DRAW = 18,
  DARKJAK = 22,
  DARKJAK_HIGHRES = 23,
  SKULL_GEM = 27,
  DEFAULT_WATER = 28,
  DEFAULT_WARP = 29,
  TEMPLEA_WATER = 30,
  TEMPLEA_WARP = 31,
  TEMPLEB_WARP = 32,
  TEMPLEC_WATER = 33,
  SEWC_WATER = 34,
  SEWD_WATER = 35,
  SEWE_WATER = 36,
  SEWG_WATER = 37,
  SEWH_WATER = 38,
  SEWI_WATER = 39,
  SEWJ_WATER = 40,
  SEWL_WATER = 41,
  SEWM_WATER = 42,
  SEWN_WATER = 43,
  DESRESC_WARP = 44,
  CTYSLUMB_WATER = 45,
  NSTB_QUICKSAND = 46,
  CTYSLUMC_WATER = 47,
  FACTORYC_ALPHA = 48,
  HFRAG = 49,
  HANGA_SPRITE = 50,
  HANGA_WATER = 51,
  DESERTD_WATER = 52,
  LMHCITYB_TFRAG = 53,
  TOWERB_WATER = 54,
  COMB_FIELD = 55,
  WASSTADA_ALPHA = 56,
  FACTORYB_WATER = 57,
  LMHCITYA_TFRAG = 58,
  MHCITYA_PRIS = 59,
  RUBBLEA_WATER = 60,
  RUBBLEA2_WATER = 61,
  RUBBLEB_WATER = 62,
  RUBBLEC_WATER = 63,
  FORESTA_WATER = 64,
  FORESTB_WATER = 65,
  LFORPLNT_PRIS = 66,
  LTNFXHIP = 67,
  LGUNNORM_WATER = 68,
  LJKDXVIN = 69,
  SECURITY = 70,
  WASPAL_WATER = 71,
  MINED_TFRAG = 72,
  VOLCANOX_WARP = 73,
  TEMPLEX_WATER = 74,
  VOLCANOA_WATER = 75,
  DESHOVER = 76,
  CLOUDS_AND_FOG = 77,
  CLOUDS_HIRES = 78,
};

struct FixedAnimInfoJak3 {
  PcTextureAnimCodesJak3 code = (PcTextureAnimCodesJak3)0;
  std::string name;
  int anim_array_idx = -1;
};

FixedAnimInfoJak3 anim_code_to_info(PcTextureAnimCodesJak3 code, const TextureAnimator& animator) {
  ASSERT(code > (PcTextureAnimCodesJak3)0);
  FixedAnimInfoJak3 anim;
  anim.code = code;
  switch (code) {
    case PcTextureAnimCodesJak3::SKULL_GEM: {
      anim.name = "skull-gem";
      anim.anim_array_idx = animator.m_skull_gem_fixed_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::DEFAULT_WATER: {
      anim.name = "default-water";
      anim.anim_array_idx = animator.m_default_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::DEFAULT_WARP: {
      anim.name = "default-warp";
      anim.anim_array_idx = animator.m_default_warp_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::TEMPLEA_WATER: {
      anim.name = "templea-water";
      anim.anim_array_idx = animator.m_templea_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::TEMPLEA_WARP: {
      anim.name = "templea-warp";
      anim.anim_array_idx = animator.m_templea_warp_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::TEMPLEB_WARP: {
      anim.name = "templeb-warp";
      anim.anim_array_idx = animator.m_templeb_warp_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::TEMPLEC_WATER: {
      anim.name = "templec-water";
      anim.anim_array_idx = animator.m_templec_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::SEWC_WATER: {
      anim.name = "sewc-water";
      anim.anim_array_idx = animator.m_sewc_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::SEWD_WATER: {
      anim.name = "sewd-water";
      anim.anim_array_idx = animator.m_sewd_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::SEWE_WATER: {
      anim.name = "sewe-water";
      anim.anim_array_idx = animator.m_sewe_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::SEWG_WATER: {
      anim.name = "sewg-water";
      anim.anim_array_idx = animator.m_sewg_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::SEWH_WATER: {
      anim.name = "sewh-water";
      anim.anim_array_idx = animator.m_sewh_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::SEWI_WATER: {
      anim.name = "sewi-water";
      anim.anim_array_idx = animator.m_sewi_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::SEWJ_WATER: {
      anim.name = "sewj-water";
      anim.anim_array_idx = animator.m_sewj_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::SEWL_WATER: {
      anim.name = "sewl-water";
      anim.anim_array_idx = animator.m_sewl_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::SEWM_WATER: {
      anim.name = "sewm-water";
      anim.anim_array_idx = animator.m_sewm_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::SEWN_WATER: {
      anim.name = "sewn-water";
      anim.anim_array_idx = animator.m_sewn_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::DESRESC_WARP: {
      anim.name = "desresc-warp";
      anim.anim_array_idx = animator.m_desresc_warp_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::CTYSLUMB_WATER: {
      anim.name = "ctyslumb-water";
      anim.anim_array_idx = animator.m_ctyslumb_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::NSTB_QUICKSAND: {
      anim.name = "nstb-quicksand";
      anim.anim_array_idx = animator.m_nstb_quicksand_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::CTYSLUMC_WATER: {
      anim.name = "ctyslumc-water";
      anim.anim_array_idx = animator.m_ctyslumc_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::FACTORYC_ALPHA: {
      anim.name = "factoryc-alpha";
      anim.anim_array_idx = animator.m_factoryc_alpha_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::HFRAG: {
      anim.name = "hfrag";
      anim.anim_array_idx = animator.m_hfrag_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::HANGA_SPRITE: {
      anim.name = "hanga-sprite";
      anim.anim_array_idx = animator.m_hanga_sprite_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::HANGA_WATER: {
      anim.name = "hanga-water";
      anim.anim_array_idx = animator.m_hanga_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::DESERTD_WATER: {
      anim.name = "desertd-water";
      anim.anim_array_idx = animator.m_desertd_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::LMHCITYB_TFRAG: {
      anim.name = "lmhcityb-tfrag";
      anim.anim_array_idx = animator.m_lmhcityb_tfrag_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::TOWERB_WATER: {
      anim.name = "towerb-water";
      anim.anim_array_idx = animator.m_towerb_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::COMB_FIELD: {
      anim.name = "comb-field";
      anim.anim_array_idx = animator.m_comb_field_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::WASSTADA_ALPHA: {
      anim.name = "wasstada-alpha";
      anim.anim_array_idx = animator.m_wasstada_alpha_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::FACTORYB_WATER: {
      anim.name = "factoryb-water";
      anim.anim_array_idx = animator.m_factoryb_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::LMHCITYA_TFRAG: {
      anim.name = "lmhcitya-tfrag";
      anim.anim_array_idx = animator.m_lmhcitya_tfrag_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::MHCITYA_PRIS: {
      anim.name = "mhcitya-pris";
      anim.anim_array_idx = animator.m_mhcitya_pris_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::RUBBLEA_WATER: {
      anim.name = "rubblea-water";
      anim.anim_array_idx = animator.m_rubblea_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::RUBBLEA2_WATER: {
      anim.name = "rubblea2-water";
      anim.anim_array_idx = animator.m_rubblea2_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::RUBBLEB_WATER: {
      anim.name = "rubbleb-water";
      anim.anim_array_idx = animator.m_rubbleb_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::RUBBLEC_WATER: {
      anim.name = "rubblec-water";
      anim.anim_array_idx = animator.m_rubblec_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::FORESTA_WATER: {
      anim.name = "foresta-water";
      anim.anim_array_idx = animator.m_foresta_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::FORESTB_WATER: {
      anim.name = "forestb-water";
      anim.anim_array_idx = animator.m_forestb_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::LFORPLNT_PRIS: {
      anim.name = "lforplnt-pris";
      anim.anim_array_idx = animator.m_lforplnt_pris_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::LTNFXHIP: {
      anim.name = "ltnfxhip";
      anim.anim_array_idx = animator.m_ltnfxhip_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::LGUNNORM_WATER: {
      anim.name = "lgunnorm-water";
      anim.anim_array_idx = animator.m_lgunnorm_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::LJKDXVIN: {
      anim.name = "ljkdxvin";
      anim.anim_array_idx = animator.m_ljkdxvin_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::SECURITY: {
      anim.name = "security";
      anim.anim_array_idx = animator.m_security_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::WASPAL_WATER: {
      anim.name = "waspal-water";
      anim.anim_array_idx = animator.m_waspal_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::MINED_TFRAG: {
      anim.name = "mined-tfrag";
      anim.anim_array_idx = animator.m_mined_tfrag_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::VOLCANOX_WARP: {
      anim.name = "volcanox-warp";
      anim.anim_array_idx = animator.m_volcanox_warp_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::TEMPLEX_WATER: {
      anim.name = "templex-water";
      anim.anim_array_idx = animator.m_templex_water_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::VOLCANOA_WATER: {
      anim.name = "volcanoa-water";
      anim.anim_array_idx = animator.m_volcanoa_anim_array_idx;
    } break;
    case PcTextureAnimCodesJak3::DESHOVER: {
      anim.name = "deshover";
      anim.anim_array_idx = animator.m_deshover_anim_array_idx;
    } break;
    default:
      anim.name = "unknown";
      lg::error("Unknown texture anim code {}", (int)code);
  }
  return anim;
}

// metadata for an upload from GOAL memory
struct TextureAnimPcUpload {
  u32 data;  // goal pointer
  u16 width;
  u16 height;
  u32 dest;  // tbp address
  // PS2 texture format of the _upload_ that was used.
  // note that the data can be any format. They upload stuff in the wrong format sometimes, as
  // an optimization (ps2 is fastest at psmct32)
  u8 format;
  u8 force_to_gpu;
  u8 pad[2];
};
static_assert(sizeof(TextureAnimPcUpload) == 16);

// metadata for an operation that operates on a source/destination texture.
struct TextureAnimPcTransform {
  u32 src_tbp;
  u32 dst_tbp;
  u32 pad0;
  u32 pad1;
};

/*!
 * Main function to run texture animations from DMA. Updates textures in the pool.
 */
void TextureAnimator::handle_texture_anim_data(DmaFollower& dma,
                                               const u8* ee_mem,
                                               TexturePool* texture_pool,
                                               u64 frame_idx) {
  dprintf("animator\n");
  m_current_shader = {};
  glBindVertexArray(m_vao);
  glBindBuffer(GL_ARRAY_BUFFER, m_vertex_buffer);
  glUseProgram(m_shader_id);
  glDepthMask(GL_FALSE);
  for (auto& t : m_in_use_temp_textures) {
    m_opengl_texture_pool.free(t.tex, t.w, t.h);
  }
  m_in_use_temp_textures.clear();  // reset temp texture allocator.
  m_force_to_gpu.clear();
  m_skip_tbps.clear();

  // loop over DMA, and do the appropriate texture operations.
  // this will fill out m_textures, which is keyed on TBP.
  // as much as possible, we keep around buffers/textures.
  // this will also record which tbp's have been "erased", for the next step.
  bool done = false;
  while (!done) {
    u32 offset = dma.current_tag_offset();
    auto tf = dma.read_and_advance();
    auto vif0 = tf.vifcode0();
    if (vif0.kind == VifCode::Kind::PC_PORT) {
      switch (this->m_version) {
        case GameVersion::Jak2:
          switch (static_cast<PcTextureAnimCodesJak2>(vif0.immediate)) {
            case PcTextureAnimCodesJak2::UPLOAD_CLUT_16_16: {
              auto p = scoped_prof("clut-16-16");
              handle_upload_clut_16_16(tf, ee_mem);
            } break;
            case PcTextureAnimCodesJak2::ERASE_DEST_TEXTURE: {
              auto p = scoped_prof("erase");
              handle_erase_dest(dma);
            } break;
            case PcTextureAnimCodesJak2::GENERIC_UPLOAD: {
              auto p = scoped_prof("generic-upload");
              handle_generic_upload(tf, ee_mem);
            } break;
            case PcTextureAnimCodesJak2::SET_SHADER: {
              auto p = scoped_prof("set-shader");
              handle_set_shader(dma);
            } break;
            case PcTextureAnimCodesJak2::DRAW: {
              auto p = scoped_prof("draw");
              handle_draw(dma, *texture_pool);
            } break;
            case PcTextureAnimCodesJak2::FINISH_ARRAY:
              done = true;
              break;
            case PcTextureAnimCodesJak2::DARKJAK: {
              auto p = scoped_prof("darkjak");
              run_clut_blender_group(tf, m_darkjak_clut_blender_idx, frame_idx, texture_pool);
            } break;
            case PcTextureAnimCodesJak2::PRISON_JAK: {
              auto p = scoped_prof("prisonjak");
              run_clut_blender_group(tf, m_jakb_prison_clut_blender_idx, frame_idx, texture_pool);
            } break;
            case PcTextureAnimCodesJak2::ORACLE_JAK: {
              auto p = scoped_prof("oraclejak");
              run_clut_blender_group(tf, m_jakb_oracle_clut_blender_idx, frame_idx, texture_pool);
            } break;
            case PcTextureAnimCodesJak2::NEST_JAK: {
              auto p = scoped_prof("nestjak");
              run_clut_blender_group(tf, m_jakb_nest_clut_blender_idx, frame_idx, texture_pool);
            } break;
            case PcTextureAnimCodesJak2::KOR_TRANSFORM: {
              auto p = scoped_prof("kor");
              run_clut_blender_group(tf, m_kor_transform_clut_blender_idx, frame_idx, texture_pool);
            } break;
            case PcTextureAnimCodesJak2::SKULL_GEM:
            case PcTextureAnimCodesJak2::BOMB:
            case PcTextureAnimCodesJak2::CAS_CONVEYOR:
            case PcTextureAnimCodesJak2::SECURITY:
            case PcTextureAnimCodesJak2::WATERFALL:
            case PcTextureAnimCodesJak2::WATERFALL_B:
            case PcTextureAnimCodesJak2::LAVA:
            case PcTextureAnimCodesJak2::LAVA_B:
            case PcTextureAnimCodesJak2::STADIUMB:
            case PcTextureAnimCodesJak2::FORTRESS_PRIS:
            case PcTextureAnimCodesJak2::FORTRESS_WARP:
            case PcTextureAnimCodesJak2::METKOR:
            case PcTextureAnimCodesJak2::SHIELD:
            case PcTextureAnimCodesJak2::KREW_HOLO: {
              auto anim =
                  anim_code_to_info(static_cast<PcTextureAnimCodesJak2>(vif0.immediate), *this);
              auto p = scoped_prof(anim.name.c_str());
              run_fixed_animation_array(anim.anim_array_idx, tf, texture_pool);
              break;
            }
            case PcTextureAnimCodesJak2::CLOUDS_AND_FOG:
            case PcTextureAnimCodesJak2::CLOUDS_HIRES: {
              auto p = scoped_prof("clouds-and-fog");
              handle_clouds_and_fog(tf, texture_pool,
                                    vif0.immediate == (u16)PcTextureAnimCodesJak2::CLOUDS_HIRES);
            } break;
            case PcTextureAnimCodesJak2::SLIME: {
              auto p = scoped_prof("slime");
              handle_slime(tf, texture_pool);
              break;
            }
            default:
              lg::print("bad imm: {}\n", vif0.immediate);
              ASSERT_NOT_REACHED();
          }
          break;
        case GameVersion::Jak3:
          switch (static_cast<PcTextureAnimCodesJak3>(vif0.immediate)) {
            case PcTextureAnimCodesJak3::UPLOAD_CLUT_16_16: {
              auto p = scoped_prof("clut-16-16");
              handle_upload_clut_16_16(tf, ee_mem);
            } break;
            case PcTextureAnimCodesJak3::ERASE_DEST_TEXTURE: {
              auto p = scoped_prof("erase");
              handle_erase_dest(dma);
            } break;
            case PcTextureAnimCodesJak3::GENERIC_UPLOAD: {
              auto p = scoped_prof("generic-upload");
              handle_generic_upload(tf, ee_mem);
            } break;
            case PcTextureAnimCodesJak3::SET_SHADER: {
              auto p = scoped_prof("set-shader");
              handle_set_shader(dma);
            } break;
            case PcTextureAnimCodesJak3::DRAW: {
              auto p = scoped_prof("draw");
              handle_draw(dma, *texture_pool);
            } break;
            case PcTextureAnimCodesJak3::FINISH_ARRAY:
              done = true;
              break;
            case PcTextureAnimCodesJak3::DARKJAK: {
              auto p = scoped_prof("darkjak");
              run_clut_blender_group(tf, m_darkjak_clut_blender_idx, frame_idx, texture_pool);
            } break;
            case PcTextureAnimCodesJak3::DARKJAK_HIGHRES: {
              auto p = scoped_prof("darkjak-highres");
              run_clut_blender_group(tf, m_darkjak_highres_clut_blender_idx, frame_idx,
                                     texture_pool);
            } break;
            case PcTextureAnimCodesJak3::SKULL_GEM:
            case PcTextureAnimCodesJak3::DEFAULT_WATER:
            case PcTextureAnimCodesJak3::DEFAULT_WARP:
            case PcTextureAnimCodesJak3::TEMPLEA_WATER:
            case PcTextureAnimCodesJak3::TEMPLEA_WARP:
            case PcTextureAnimCodesJak3::TEMPLEB_WARP:
            case PcTextureAnimCodesJak3::TEMPLEC_WATER:
            case PcTextureAnimCodesJak3::SEWC_WATER:
            case PcTextureAnimCodesJak3::SEWD_WATER:
            case PcTextureAnimCodesJak3::SEWE_WATER:
            case PcTextureAnimCodesJak3::SEWG_WATER:
            case PcTextureAnimCodesJak3::SEWH_WATER:
            case PcTextureAnimCodesJak3::SEWI_WATER:
            case PcTextureAnimCodesJak3::SEWJ_WATER:
            case PcTextureAnimCodesJak3::SEWL_WATER:
            case PcTextureAnimCodesJak3::SEWM_WATER:
            case PcTextureAnimCodesJak3::SEWN_WATER:
            case PcTextureAnimCodesJak3::DESRESC_WARP:
            case PcTextureAnimCodesJak3::CTYSLUMB_WATER:
            case PcTextureAnimCodesJak3::NSTB_QUICKSAND:
            case PcTextureAnimCodesJak3::CTYSLUMC_WATER:
            case PcTextureAnimCodesJak3::FACTORYC_ALPHA:
            case PcTextureAnimCodesJak3::HFRAG:
            case PcTextureAnimCodesJak3::HANGA_SPRITE:
            case PcTextureAnimCodesJak3::HANGA_WATER:
            case PcTextureAnimCodesJak3::DESERTD_WATER:
            case PcTextureAnimCodesJak3::LMHCITYB_TFRAG:
            case PcTextureAnimCodesJak3::TOWERB_WATER:
            case PcTextureAnimCodesJak3::COMB_FIELD:
            case PcTextureAnimCodesJak3::WASSTADA_ALPHA:
            case PcTextureAnimCodesJak3::FACTORYB_WATER:
            case PcTextureAnimCodesJak3::LMHCITYA_TFRAG:
            case PcTextureAnimCodesJak3::MHCITYA_PRIS:
            case PcTextureAnimCodesJak3::RUBBLEA_WATER:
            case PcTextureAnimCodesJak3::RUBBLEA2_WATER:
            case PcTextureAnimCodesJak3::RUBBLEB_WATER:
            case PcTextureAnimCodesJak3::RUBBLEC_WATER:
            case PcTextureAnimCodesJak3::FORESTA_WATER:
            case PcTextureAnimCodesJak3::FORESTB_WATER:
            case PcTextureAnimCodesJak3::LFORPLNT_PRIS:
            case PcTextureAnimCodesJak3::LTNFXHIP:
            case PcTextureAnimCodesJak3::LGUNNORM_WATER:
            case PcTextureAnimCodesJak3::LJKDXVIN:
            case PcTextureAnimCodesJak3::SECURITY:
            case PcTextureAnimCodesJak3::WASPAL_WATER:
            case PcTextureAnimCodesJak3::MINED_TFRAG:
            case PcTextureAnimCodesJak3::VOLCANOX_WARP:
            case PcTextureAnimCodesJak3::TEMPLEX_WATER:
            case PcTextureAnimCodesJak3::VOLCANOA_WATER:
            case PcTextureAnimCodesJak3::DESHOVER: {
              auto anim =
                  anim_code_to_info(static_cast<PcTextureAnimCodesJak3>(vif0.immediate), *this);
              auto p = scoped_prof(anim.name.c_str());
              run_fixed_animation_array(anim.anim_array_idx, tf, texture_pool);
              break;
            }
            case PcTextureAnimCodesJak3::CLOUDS_AND_FOG:
            case PcTextureAnimCodesJak3::CLOUDS_HIRES: {
              auto p = scoped_prof("clouds-and-fog");
              handle_clouds_and_fog(tf, texture_pool,
                                    vif0.immediate == (u16)PcTextureAnimCodesJak3::CLOUDS_HIRES);
              break;
            }
            default:
              lg::print("bad imm: {}\n", vif0.immediate);
              ASSERT_NOT_REACHED();
          }
          break;
        default:
          lg::print("unsupported game version {}\n", (int)this->m_version);
          ASSERT_NOT_REACHED();
      }
    } else {
      lg::print("[tex anim] unhandled VIF in main loop\n");
      lg::print("{} {}\n", vif0.print(), tf.vifcode1().print());
      lg::print("dma address 0x{:x}\n", offset);
      ASSERT_NOT_REACHED();
    }
  }

  // The steps above will populate m_textures with some combination of GPU/CPU textures.
  // we need to make sure that all final textures end up on the GPU, if desired. (todo: move this to
  // happen somewhere else)?
  for (auto tbp : m_force_to_gpu) {
    auto p = scoped_prof("force-to-gpu");
    force_to_gpu(tbp);
  }

  // Loop over textures and put them in the pool if needed
  for (auto& [tbp, entry] : m_textures) {
    if (entry.kind != VramEntry::Kind::GPU) {
      // not on the GPU, we can't put it in the texture pool.
      // if it was skipped by the above step, this is just some temporary texture we don't need
      // (hopefully)
      // (TODO: could flag these somehow?)
      continue;
    }

    if (std::find(m_skip_tbps.begin(), m_skip_tbps.end(), tbp) != m_skip_tbps.end()) {
      continue;
    }
    dprintf("end processing on %d\n", tbp);

    // in the ideal case, the texture processing code will just modify the OpenGL texture in-place.
    // however, if the size changes, or we need to add a new texture, we have additional work to
    // do.

    if (entry.needs_pool_update) {
      if (entry.pool_gpu_tex) {
        // we have a GPU texture in the pool, but we need to change the actual texture.
        auto p = scoped_prof("pool-update");
        ASSERT(entry.pool_gpu_tex);
        // change OpenGL texture in the pool
        texture_pool->update_gl_texture(entry.pool_gpu_tex, entry.tex_width, entry.tex_height,
                                        entry.tex.value().texture());
        // set as the active texture in this vram slot (other textures can be loaded for
        // different part of the frame that we need to replace). This is a fast operation.
        texture_pool->move_existing_to_vram(entry.pool_gpu_tex, tbp);
        entry.needs_pool_update = false;
        dprintf("update texture %d\n", tbp);
      } else {
        // this is the first time we use a texture in this slot, so we need to create it.
        // should happen only once per TBP.
        auto p = scoped_prof("pool-create");
        TextureInput in;
        in.gpu_texture = entry.tex.value().texture();
        in.w = entry.tex_width;
        in.h = entry.tex_height;
        in.debug_page_name = "PC-ANIM";
        in.debug_name = std::to_string(tbp);
        in.id = get_id_for_tbp(texture_pool, tbp, 99);
        entry.pool_gpu_tex = texture_pool->give_texture_and_load_to_vram(in, tbp);
        entry.needs_pool_update = false;
        dprintf("create texture %d\n", tbp);
      }
    } else {
      // ideal case: OpenGL texture modified in place, just have to simulate "upload".
      auto p = scoped_prof("pool-move");
      texture_pool->move_existing_to_vram(entry.pool_gpu_tex, tbp);
      dprintf("no change %d\n", tbp);
    }
  }

  glDepthMask(GL_TRUE);
  glEnable(GL_DEPTH_TEST);
  glColorMask(true, true, true, true);
  copy_private_to_public();
}

/*!
 * Make sure that this texture is a GPU texture. If not, convert it.
 * GPU textures don't support CLUT, so this should be done at the last possible point in time, as
 * CLUT effects can no longer be applied to the texture after this happens.
 */
void TextureAnimator::force_to_gpu(int tbp) {
  auto& entry = m_textures.at(tbp);
  switch (entry.kind) {
    default:
      lg::print("unhandled non-gpu conversion: {} (tbp = {})\n", (int)entry.kind, tbp);
      ASSERT_NOT_REACHED();
    case VramEntry::Kind::CLUT16_16_IN_PSM32:
      // HACK: never convert known CLUT textures to GPU.
      // The main loop will incorrectly flag CLUT textures as final ones because we can't tell
      // the difference. So hopefully this is just an optimization. But we'll have to revisit if
      // they use texture data as both texture/clut.
      dprintf("suspicious clut...\n");
      break;
    case VramEntry::Kind::GPU:
      break;  // already on the gpu.
    case VramEntry::Kind::GENERIC_PSM32: {
      int tw = entry.tex_width;
      int th = entry.tex_height;
      setup_vram_entry_for_gpu_texture(tw, th, tbp);
      glBindTexture(GL_TEXTURE_2D, entry.tex.value().texture());
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tw, th, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
                   entry.data.data());
      glBindTexture(GL_TEXTURE_2D, 0);
      entry.kind = VramEntry::Kind::GPU;
    } break;
    case VramEntry::Kind::GENERIC_PSMT8: {
      // we have data that was uploaded in PSMT8 format. Assume that it will also be read in this
      // format. Convert to normal format.
      int tw = entry.tex_width;
      int th = entry.tex_height;
      std::vector<u32> rgba_data(tw * th);

      {
        auto p = scoped_prof("convert");
        // the CLUT is usually uploaded in PSM32 format, as a 16x16.
        const u32* clut = get_clut_16_16_psm32(entry.cbp);
        for (int r = 0; r < th; r++) {
          for (int c = 0; c < tw; c++) {
            rgba_data[c + r * tw] = clut[m_index_to_clut_addr[entry.data[c + r * tw]]];
          }
        }
      }

      // do OpenGL tricks to make sure this entry is set up to hold a texture with the size.
      // will also set flags for updating the pool
      setup_vram_entry_for_gpu_texture(tw, th, tbp);
      // load the texture.
      glBindTexture(GL_TEXTURE_2D, entry.tex.value().texture());
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tw, th, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
                   rgba_data.data());
      glBindTexture(GL_TEXTURE_2D, 0);
      entry.kind = VramEntry::Kind::GPU;
    } break;
    case VramEntry::Kind::GENERIC_PSMT4: {
      int tw = entry.tex_width;
      int th = entry.tex_height;
      std::vector<u32> rgba_data(tw * th);
      {
        auto p = scoped_prof("convert");
        // for psmt4, we don't use the special 16x16 case
        const auto& clut_lookup = m_textures.find(entry.cbp);
        ASSERT(clut_lookup != m_textures.end());
        ASSERT(clut_lookup->second.kind == VramEntry::Kind::GENERIC_PSM32);
        auto* clut = (const u32*)clut_lookup->second.data.data();

        for (int px = 0; px < (int)rgba_data.size(); ++px) {
          u8 val = entry.data[px / 2];
          int idx = px & 1 ? val >> 4 : val & 0xf;
          // no m_index_to_clut_addr mapping for the 4-bit index.
          rgba_data[px] = clut[idx];
        }
      }
      setup_vram_entry_for_gpu_texture(tw, th, tbp);
      glBindTexture(GL_TEXTURE_2D, entry.tex.value().texture());
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tw, th, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
                   rgba_data.data());
      glBindTexture(GL_TEXTURE_2D, 0);
      entry.kind = VramEntry::Kind::GPU;
    } break;
  }
}

/*!
 * Get a pool texture ID for this texture. For now, there's just a unique ID per TBP.
 * The only purpose is to avoid putting all the textures with the same ID, which is a slow-path
 * in the pool (which is optimized for only a few textures with the same ID at most).
 */
PcTextureId TextureAnimator::get_id_for_tbp(TexturePool* pool, u64 tbp, u64 other_id) {
  const auto& it = m_ids_by_vram.find(tbp | (other_id << 32));
  if (it == m_ids_by_vram.end()) {
    auto ret = pool->allocate_pc_port_texture(m_version);
    m_ids_by_vram[tbp] = ret;
    return ret;
  } else {
    return it->second;
  }
}

void TextureAnimator::run_clut_blender_group(DmaTransfer& tf,
                                             int idx,
                                             u64 frame_idx,
                                             TexturePool* texture_pool) {
  auto& blenders = m_clut_blender_groups.at(idx);
  const int tbps_size = sizeof(u32) * 4 * ((blenders.blenders.size() + 3) / 4);
  float f;
  ASSERT(tf.size_bytes == 16 + tbps_size);
  memcpy(&f, tf.data, sizeof(float));
  float weights[2] = {1.f - f, f};
  blenders.last_updated_frame = frame_idx;
  for (size_t i = 0; i < blenders.blenders.size(); i++) {
    m_private_output_slots[blenders.outputs[i]] = blenders.blenders[i].run(weights);
  }

  const u32* tbps = (const u32*)(tf.data + 16);

  // give to the pool for renderers that don't know how to access this directly
  if (blenders.move_to_pool) {
    for (size_t i = 0; i < blenders.blenders.size(); i++) {
      auto& blender = blenders.blenders[i];
      const u32 tbp = tbps[i];
      if (tbp == UINT32_MAX) {
        continue;
      }
      ASSERT(tbp < 0x40000);
      m_skip_tbps.push_back(tbp);  // known to be an output texture.
      if (blender.pool_gpu_tex) {
        // TODO: handle debug checkbox.
        texture_pool->move_existing_to_vram(blender.pool_gpu_tex, tbp);
        ASSERT(texture_pool->lookup(tbp).value() == blender.texture());
      } else {
        TextureInput in;
        in.gpu_texture = blender.texture();
        in.w = blender.w();
        in.h = blender.h();
        in.debug_page_name = "PC-ANIM";
        in.debug_name = std::to_string(tbp);
        in.id = get_id_for_tbp(texture_pool, tbp, idx);
        blender.pool_gpu_tex = texture_pool->give_texture_and_load_to_vram(in, tbp);
      }
    }
  }
}

void TextureAnimator::handle_clouds_and_fog(const DmaTransfer& tf,
                                            TexturePool* texture_pool,
                                            bool hires) {
  ASSERT(tf.size_bytes >= sizeof(SkyInput));
  SkyInput input;
  memcpy(&input, tf.data, sizeof(SkyInput));
  auto tex = run_clouds(input, hires);
  auto& gpu_tex = hires ? m_sky_hires_pool_gpu_tex : m_sky_pool_gpu_tex;

  if (gpu_tex) {
    texture_pool->move_existing_to_vram(gpu_tex, input.cloud_dest);
    ASSERT((int)texture_pool->lookup(input.cloud_dest).value() == tex);
  } else {
    TextureInput in;
    in.gpu_texture = tex;
    in.w = hires ? kFinalSkyHiresTextureSize : kFinalSkyTextureSize;
    in.h = hires ? kFinalSkyHiresTextureSize : kFinalSkyTextureSize;
    in.debug_page_name = "PC-ANIM";
    in.debug_name = hires ? "clouds-hires" : "clouds";
    in.id = get_id_for_tbp(texture_pool, input.cloud_dest, 777);
    gpu_tex = texture_pool->give_texture_and_load_to_vram(in, input.cloud_dest);
  }
}

void TextureAnimator::handle_slime(const DmaTransfer& tf, TexturePool* texture_pool) {
  ASSERT(tf.size_bytes >= sizeof(SlimeInput));
  SlimeInput input;
  memcpy(&input, tf.data, sizeof(SlimeInput));

  run_slime(input);

  {
    auto no_scroll_tex = m_slime_final_texture.texture();
    if (m_slime_pool_gpu_tex) {
      texture_pool->move_existing_to_vram(m_slime_pool_gpu_tex, input.dest);
      ASSERT(texture_pool->lookup(input.dest).value() == no_scroll_tex);
    } else {
      TextureInput in;
      in.gpu_texture = no_scroll_tex;
      in.w = kFinalSlimeTextureSize;
      in.h = kFinalSlimeTextureSize;
      in.debug_page_name = "PC-ANIM";
      in.debug_name = "slime";
      in.id = get_id_for_tbp(texture_pool, input.dest, 778);
      m_slime_pool_gpu_tex = texture_pool->give_texture_and_load_to_vram(in, input.dest);
    }
    m_private_output_slots.at(m_slime_output_slot) = no_scroll_tex;
  }

  {
    auto tex = m_slime_final_scroll_texture.texture();
    if (m_slime_scroll_pool_gpu_tex) {
      texture_pool->move_existing_to_vram(m_slime_scroll_pool_gpu_tex, input.scroll_dest);
      ASSERT(texture_pool->lookup(input.scroll_dest).value() == tex);
    } else {
      TextureInput in;
      in.gpu_texture = tex;
      in.w = kFinalSlimeTextureSize;
      in.h = kFinalSlimeTextureSize;
      in.debug_page_name = "PC-ANIM";
      in.debug_name = "slime-scroll";
      in.id = get_id_for_tbp(texture_pool, input.dest, 779);
      m_slime_scroll_pool_gpu_tex =
          texture_pool->give_texture_and_load_to_vram(in, input.scroll_dest);
    }
    m_private_output_slots.at(m_slime_scroll_output_slot) = tex;
  }
}

void TextureAnimator::clear_stale_textures(u64 frame_idx) {
  for (auto& group : m_clut_blender_groups) {
    if (frame_idx > group.last_updated_frame) {
      for (auto& blender : group.blenders) {
        if (!blender.at_default()) {
          float weights[2] = {1, 0};
          blender.run(weights);
        }
      }
    }
  }
}

/*!
 * Create an entry for a 16x16 clut texture upload. Leaves it on the CPU.
 * They upload cluts as PSM32, so there's no funny addressing stuff, other than
 * the CLUT indexing scramble stuff.
 */
void TextureAnimator::handle_upload_clut_16_16(const DmaTransfer& tf, const u8* ee_mem) {
  dprintf("[tex anim] upload clut 16 16\n");
  ASSERT(tf.size_bytes == sizeof(TextureAnimPcUpload));
  auto* upload = (const TextureAnimPcUpload*)(tf.data);
  ASSERT(upload->width == 16);
  ASSERT(upload->height == 16);
  dprintf("  dest is 0x%x\n", upload->dest);
  auto& vram = m_textures[upload->dest];
  vram.reset();
  vram.kind = VramEntry::Kind::CLUT16_16_IN_PSM32;
  vram.data.resize(16 * 16 * 4);
  vram.tex_width = upload->width;
  vram.tex_height = upload->height;
  memcpy(vram.data.data(), ee_mem + upload->data, vram.data.size());
  if (m_tex_looking_for_clut) {
    m_tex_looking_for_clut->cbp = upload->dest;
    m_tex_looking_for_clut = nullptr;
  }
}

/*!
 * Create an entry for any texture upload. Leaves it on the CPU, as we may do fancy scramble stuff.
 */
void TextureAnimator::handle_generic_upload(const DmaTransfer& tf, const u8* ee_mem) {
  dprintf("[tex anim] upload generic @ 0x%lx\n", tf.data - ee_mem);
  ASSERT(tf.size_bytes == sizeof(TextureAnimPcUpload));
  auto* upload = (const TextureAnimPcUpload*)(tf.data);
  dprintf(" size %d x %d\n", upload->width, upload->height);
  dprintf(" dest is 0x%x\n", upload->dest);
  auto& vram = m_textures[upload->dest];
  vram.reset();

  switch (upload->format) {
    case (int)GsTex0::PSM::PSMCT32:
      vram.kind = VramEntry::Kind::GENERIC_PSM32;
      vram.data.resize(upload->width * upload->height * 4);
      vram.tex_width = upload->width;
      vram.tex_height = upload->height;
      memcpy(vram.data.data(), ee_mem + upload->data, vram.data.size());
      if (m_tex_looking_for_clut) {
        m_tex_looking_for_clut->cbp = upload->dest;
      }
      m_tex_looking_for_clut = nullptr;
      if (upload->force_to_gpu) {
        m_force_to_gpu.insert(upload->dest);
      }
      break;
    case (int)GsTex0::PSM::PSMT8:
      vram.kind = VramEntry::Kind::GENERIC_PSMT8;
      vram.data.resize(upload->width * upload->height);
      vram.tex_width = upload->width;
      vram.tex_height = upload->height;
      memcpy(vram.data.data(), ee_mem + upload->data, vram.data.size());
      m_tex_looking_for_clut = &vram;
      if (upload->force_to_gpu) {
        m_force_to_gpu.insert(upload->dest);
      }
      break;
    case (int)GsTex0::PSM::PSMT4:
      vram.kind = VramEntry::Kind::GENERIC_PSMT4;
      vram.data.resize(upload->width * upload->height);
      vram.tex_width = upload->width;
      vram.tex_height = upload->height;
      memcpy(vram.data.data(), ee_mem + upload->data, vram.data.size());
      m_tex_looking_for_clut = &vram;
      if (upload->force_to_gpu) {
        m_force_to_gpu.insert(upload->dest);
      }
      break;
    default:
      lg::print("Unhandled format: {}\n", upload->format);
      ASSERT_NOT_REACHED();
  }
}

/*!
 * Handle the initialization of an animated texture. This fills the entire texture with a solid
 * color. We set up a GPU texture here - drawing operations are done on the GPU, so we'd never
 * need this solid color on the CPU. Also sets a bunch of GS state for the shaders.
 * These may be modified by animation functions, but most of the time they aren't.
 */
void TextureAnimator::handle_erase_dest(DmaFollower& dma) {
  // auto& out = m_new_dest_textures.emplace_back();
  VramEntry* entry = nullptr;

  // first transfer will be a bunch of ad (modifies the shader)
  {
    auto ad_transfer = dma.read_and_advance();
    ASSERT(ad_transfer.size_bytes == 10 * 16);
    ASSERT(ad_transfer.vifcode0().kind == VifCode::Kind::FLUSHA);
    ASSERT(ad_transfer.vifcode1().kind == VifCode::Kind::DIRECT);
    const u64* ad_data = (const u64*)(ad_transfer.data + 16);

    // for (int i = 0; i < 9; i++) {
    // dprintf(" ad: 0x%lx 0x%lx\n", ad_data[i * 2], ad_data[i * 2 + 1]);
    // }
    // 0 (scissor-1 (new 'static 'gs-scissor :scax1 (+ tex-width -1) :scay1 (+ tex-height -1)))
    ASSERT(ad_data[0 * 2 + 1] == (int)GsRegisterAddress::SCISSOR_1);
    GsScissor scissor(ad_data[0]);
    int tex_width = scissor.x1() + 1;
    int tex_height = scissor.y1() + 1;
    dprintf(" size: %d x %d\n", tex_width, tex_height);

    // 1 (xyoffset-1 (new 'static 'gs-xy-offset :ofx #x8000 :ofy #x8000))
    // 2 (frame-1 (new 'static 'gs-frame :fbw (/ (+ tex-width 63) 64) :fbp fbp-for-tex))
    ASSERT(ad_data[2 * 2 + 1] == (int)GsRegisterAddress::FRAME_1);
    GsFrame frame(ad_data[2 * 2]);
    int dest_texture_address = 32 * frame.fbp();
    dprintf(" dest: 0x%x\n", dest_texture_address);

    // 3 (test-1 (-> anim test))
    ASSERT(ad_data[2 * 3 + 1] == (int)GsRegisterAddress::TEST_1);
    m_current_shader.test = GsTest(ad_data[3 * 2]);
    dfmt(" test: {}", m_current_shader.test.print());

    // 4 (alpha-1 (-> anim alpha))
    ASSERT(ad_data[2 * 4 + 1] == (int)GsRegisterAddress::ALPHA_1);
    m_current_shader.alpha = GsAlpha(ad_data[4 * 2]);
    dfmt(" alpha: {}\n", m_current_shader.alpha.print());

    // 5 (clamp-1 (-> anim clamp))
    ASSERT(ad_data[2 * 5 + 1] == (int)GsRegisterAddress::CLAMP_1);
    u64 creg = ad_data[5 * 2];
    m_current_shader.clamp_u = creg & 0b001;
    m_current_shader.clamp_v = creg & 0b100;
    u64 mask = ~0b101;
    ASSERT((creg & mask) == 0);
    dfmt(" clamp: {} {}\n", m_current_shader.clamp_u, m_current_shader.clamp_v);

    // 6 (texa (new 'static 'gs-texa :ta0 #x80 :ta1 #x80))
    // 7 (zbuf-1 (new 'static 'gs-zbuf :zbp #x130 :psm (gs-psm ct24) :zmsk #x1))
    // 8 (texflush 0)

    // get the entry set up for being a GPU texture.
    entry = setup_vram_entry_for_gpu_texture(tex_width, tex_height, dest_texture_address);
  }

  // next transfer is the erase. This is done with alpha blending off
  auto clear_transfer = dma.read_and_advance();
  ASSERT(clear_transfer.size_bytes == 16 * 4);
  math::Vector<u32, 4> rgba_u32;
  memcpy(rgba_u32.data(), clear_transfer.data + 16, 16);
  dfmt(" clear: {}\n", rgba_u32.to_string_hex_byte());

  // create the opengl output texture.

  // do the clear:
  {
    FramebufferTexturePairContext ctxt(entry->tex.value());
    float positions[3 * 4] = {0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0};
    glUniform3fv(m_uniforms.positions, 4, positions);
    glUniform1i(m_uniforms.enable_tex, 0);
    glUniform4f(m_uniforms.rgba, rgba_u32[0], rgba_u32[1], rgba_u32[2], rgba_u32[3]);
    glUniform4i(m_uniforms.channel_scramble, 0, 1, 2, 3);
    glBindTexture(GL_TEXTURE_2D, m_dummy_texture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glDisable(GL_BLEND);
    glDisable(GL_DEPTH_TEST);
    glColorMask(true, true, true, true);
    // write the exact specified alpha (texture holds game-style alphas)
    glUniform1f(m_uniforms.alpha_multiply, 1.f);
    {
      auto p = scoped_prof("erase-draw");
      glDrawArrays(GL_TRIANGLE_FAN, 0, 4);
    }
  }

  // set as active
  m_current_dest_tbp = entry->dest_texture_address;
  m_force_to_gpu.insert(entry->dest_texture_address);
}

/*!
 * ADGIF shader update
 */
void TextureAnimator::handle_set_shader(DmaFollower& dma) {
  dprintf("[tex anim] set shader\n");
  auto ad_transfer = dma.read_and_advance();
  const int num_regs = (ad_transfer.size_bytes - 16) / 16;
  ASSERT(ad_transfer.vifcode0().kind == VifCode::Kind::NOP ||
         ad_transfer.vifcode0().kind == VifCode::Kind::FLUSHA);
  ASSERT(ad_transfer.vifcode1().kind == VifCode::Kind::DIRECT);
  const u64* ad_data = (const u64*)(ad_transfer.data + 16);

  for (int i = 0; i < num_regs; i++) {
    u64 addr = ad_data[i * 2 + 1];
    u64 data = ad_data[i * 2];

    switch (GsRegisterAddress(addr)) {
      case GsRegisterAddress::TEX0_1:
        m_current_shader.tex0 = GsTex0(data);
        m_current_shader.source_texture_set = true;
        dfmt(" tex0: {}", m_current_shader.tex0.print());
        break;
      case GsRegisterAddress::TEX1_1:
        m_current_shader.tex1 = GsTex1(data);
        dfmt(" tex1: {}", m_current_shader.tex1.print());
        break;
      case GsRegisterAddress::TEST_1:
        m_current_shader.test = GsTest(data);
        dfmt(" test: {}", m_current_shader.test.print());
        break;
      case GsRegisterAddress::ALPHA_1:
        m_current_shader.alpha = GsAlpha(data);
        dfmt(" alpha: {}\n", m_current_shader.alpha.print());
        break;
      case GsRegisterAddress::CLAMP_1:
        m_current_shader.clamp_u = data & 0b001;
        m_current_shader.clamp_v = data & 0b100;
        ASSERT((data & (~(u64(0b101)))) == 0);
        dfmt(" clamp: {} {}\n", m_current_shader.clamp_u, m_current_shader.clamp_v);
        break;
      default:
        dfmt("unknown reg {}\n", addr);
        ASSERT_NOT_REACHED();
    }
  }
}

/*!
 * Do a draw to a destination texture.
 */
void TextureAnimator::handle_draw(DmaFollower& dma, TexturePool& texture_pool) {
  // NOTE: assuming ABE set from the template here. If this function is used for other templates,
  // we'll need to actually check.
  dprintf("[tex anim] Draw\n");
  DrawData draw_data;
  auto draw_xfer = dma.read_and_advance();
  ASSERT(draw_xfer.size_bytes == sizeof(DrawData));
  memcpy(&draw_data, draw_xfer.data, sizeof(DrawData));

  if (m_current_shader.source_texture_set) {
    // find the destination we draw to. It should have been erased previously, making it a GPU
    // texture
    auto& dest_te = m_textures.at(m_current_dest_tbp);
    ASSERT(dest_te.kind == VramEntry::Kind::GPU);

    // set up context to draw to this one
    FramebufferTexturePairContext ctxt(*dest_te.tex);

    // get the source texture
    GLuint gpu_texture;
    {
      auto p = scoped_prof("make-tex");
      gpu_texture = make_or_get_gpu_texture_for_current_shader(texture_pool);
    }

    // use ADGIF shader data to set OpenGL state
    bool writes_alpha =
        set_up_opengl_for_shader(m_current_shader, gpu_texture, true);  // ABE forced on here.

    // set up uniform buffers for the coordinates for this draw.
    set_uniforms_from_draw_data(draw_data, dest_te.tex_width, dest_te.tex_height);

    ASSERT(dest_te.tex);

    if (writes_alpha) {
      glColorMask(true, true, true, false);
      glUniform1f(m_uniforms.alpha_multiply, 2.f);
      glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
      glColorMask(false, false, false, true);
      glUniform1f(m_uniforms.alpha_multiply, 1.f);
      glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
    } else {
      // we don't write alpha out. So apply alpha multiplier for blending.
      glUniform1f(m_uniforms.alpha_multiply, 1.f);
      glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
    }

    // debug_save_opengl_texture("opengl_draw_result.png", dest_te.tex->texture());
    // debug_save_opengl_texture("opengl_test.png", gpu_texture);
  } else {
    ASSERT_NOT_REACHED();
  }
}

/*!
 * Using the current shader settings, load the CLUT table to the texture coverter "VRAM".
 */
void TextureAnimator::load_clut_to_converter() {
  const auto& clut_lookup = m_textures.find(m_current_shader.tex0.cbp());
  if (clut_lookup == m_textures.end()) {
    lg::print("set shader referenced an unknown clut texture in {}\n", m_current_shader.tex0.cbp());
    ASSERT_NOT_REACHED();
  }

  switch (clut_lookup->second.kind) {
    case VramEntry::Kind::CLUT16_16_IN_PSM32:
      m_converter.upload_width(clut_lookup->second.data.data(), m_current_shader.tex0.cbp(), 16,
                               16);
      break;
    default:
      lg::print("unhandled clut source kind: {}\n", (int)clut_lookup->second.kind);
      ASSERT_NOT_REACHED();
  }
}

GLuint TextureAnimator::make_temp_gpu_texture(const u32* data, u32 width, u32 height) {
  GLuint gl_tex = m_opengl_texture_pool.allocate(width, height);
  m_in_use_temp_textures.push_back(TempTexture{gl_tex, width, height});
  glBindTexture(GL_TEXTURE_2D, gl_tex);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
               data);
  glBindTexture(GL_TEXTURE_2D, 0);
  return gl_tex;
}

/*!
 * Read the current shader settings, and get/create/setup a GPU texture for the source texture.
 */
GLuint TextureAnimator::make_or_get_gpu_texture_for_current_shader(TexturePool& texture_pool) {
  u32 tbp = m_current_shader.tex0.tbp0();
  const auto& lookup = m_textures.find(m_current_shader.tex0.tbp0());
  if (lookup == m_textures.end()) {
    auto tpool = texture_pool.lookup(tbp);
    if (tpool.has_value()) {
      return *tpool;
    }
    // lg::print("referenced an unknown texture in {}\n", tbp);
    lg::error("unknown texture in {} (0x{:x})", tbp, tbp);
    return texture_pool.get_placeholder_texture();

    // ASSERT_NOT_REACHED();
  }

  auto* vram_entry = &lookup->second;

  // see what format the source is
  switch (vram_entry->kind) {
    case VramEntry::Kind::GPU:
      // already on the GPU, just return it.
      return lookup->second.tex->texture();
      // data on the CPU, in PSM32
    case VramEntry::Kind::GENERIC_PSM32:
      // see how we're reading it:
      switch (m_current_shader.tex0.psm()) {
        // reading as a different format, needs scrambler.
        case GsTex0::PSM::PSMT8: {
          auto p = scoped_prof("scrambler");
          int w = 1 << m_current_shader.tex0.tw();
          int h = 1 << m_current_shader.tex0.th();
          ASSERT(w == vram_entry->tex_width * 2);
          ASSERT(h == vram_entry->tex_height * 2);
          ASSERT(m_current_shader.tex0.tbw() == 1);
          std::vector<u32> rgba_data(w * h);

          const auto& clut_lookup = m_textures.find(m_current_shader.tex0.cbp());
          if (clut_lookup == m_textures.end()) {
            lg::print("set shader referenced an unknown clut texture in {}\n",
                      m_current_shader.tex0.cbp());
            ASSERT_NOT_REACHED();
          }

          switch (clut_lookup->second.kind) {
            case VramEntry::Kind::CLUT16_16_IN_PSM32:
              break;
            default:
              lg::print("unhandled clut source kind: {}\n", (int)clut_lookup->second.kind);
              ASSERT_NOT_REACHED();
          }

          const u32* clut_u32s = (const u32*)clut_lookup->second.data.data();

          if (w == 8 && h == 8 && m_debug.use_fast_scrambler) {
            ASSERT_NOT_REACHED();
          } else if (w == 16 && h == 16) {
            for (int i = 0; i < 16 * 16; i++) {
              memcpy(&rgba_data[m_psm32_to_psm8_8_8.destinations_per_byte[i]],
                     &clut_u32s[m_clut_table.addrs[vram_entry->data[i]]], 4);
            }
          } else if (w == 32 && h == 32 && m_debug.use_fast_scrambler) {
            for (int i = 0; i < 32 * 32; i++) {
              rgba_data[m_psm32_to_psm8_16_16.destinations_per_byte[i]] =
                  clut_u32s[m_clut_table.addrs[vram_entry->data[i]]];
            }
          } else if (w == 64 && h == 64 && m_debug.use_fast_scrambler) {
            for (int i = 0; i < 64 * 64; i++) {
              rgba_data[m_psm32_to_psm8_32_32.destinations_per_byte[i]] =
                  clut_u32s[m_clut_table.addrs[vram_entry->data[i]]];
            }
          } else if (w == 128 && h == 128 && m_debug.use_fast_scrambler) {
            for (int i = 0; i < 128 * 128; i++) {
              rgba_data[m_psm32_to_psm8_64_64.destinations_per_byte[i]] =
                  clut_u32s[m_clut_table.addrs[vram_entry->data[i]]];
            }
          } else {
            Timer timer;
            m_converter.upload_width(vram_entry->data.data(), m_current_shader.tex0.tbp0(),
                                     vram_entry->tex_width, vram_entry->tex_height);

            // also needs clut lookup
            load_clut_to_converter();
            {
              m_converter.download_rgba8888(
                  (u8*)rgba_data.data(), m_current_shader.tex0.tbp0(), m_current_shader.tex0.tbw(),
                  w, h, (int)m_current_shader.tex0.psm(), (int)m_current_shader.tex0.cpsm(),
                  m_current_shader.tex0.cbp(), rgba_data.size() * 4);
              //              file_util::write_rgba_png("out.png", rgba_data.data(), 1 <<
              //              m_current_shader.tex0.tw(),
              //                                        1 << m_current_shader.tex0.th());
              lg::print("Scrambler took the slow path {} x {} took {:.3f} ms\n", w, h,
                        timer.getMs());
            }
          }
          auto ret = make_temp_gpu_texture(rgba_data.data(), w, h);
          // debug_save_opengl_texture(fmt::format("tex_{}.png", w), ret);
          return ret;

          ASSERT_NOT_REACHED();
        } break;
        default:
          lg::print("unhandled source texture format {}\n", (int)m_current_shader.tex0.psm());
          ASSERT_NOT_REACHED();
      }
      break;
    case VramEntry::Kind::CLUT16_16_IN_PSM32:
      ASSERT_NOT_REACHED();

      break;
    default:
      ASSERT_NOT_REACHED();
  }
}

bool TextureAnimator::set_up_opengl_for_shader(const ShaderContext& shader,
                                               std::optional<GLuint> texture,
                                               bool prim_abe) {
  if (texture) {
    glBindTexture(GL_TEXTURE_2D, *texture);
    glUniform1i(m_uniforms.enable_tex, 1);
  } else {
    glBindTexture(GL_TEXTURE_2D, m_dummy_texture);
    glUniform1i(m_uniforms.enable_tex, 0);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  }
  // tex0
  u32 tcc = shader.tex0.tcc();
  ASSERT(tcc == 1 || tcc == 0);
  glUniform1i(m_uniforms.tcc, tcc);

  ASSERT(shader.tex0.tfx() == GsTex0::TextureFunction::MODULATE);
  // tex1
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER,
                  shader.tex1.mmag() ? GL_LINEAR : GL_NEAREST);
  switch (shader.tex1.mmin()) {
    case 0:
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      break;
    case 1:
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      break;
    default:
      ASSERT_NOT_REACHED();
  }

  [[maybe_unused]] bool do_alpha_test = false;
  bool alpha_test_mask_alpha_trick = false;
  bool alpha_test_mask_depth_trick = false;

  // test
  if (shader.test.alpha_test_enable()) {
    auto atst = shader.test.alpha_test();
    if (atst == GsTest::AlphaTest::ALWAYS) {
      do_alpha_test = false;
      // atest effectively disabled - everybody passes.
    } else if (atst == GsTest::AlphaTest::NEVER) {
      // everybody fails. They use alpha test to mask out some channel
      do_alpha_test = false;

      switch (shader.test.afail()) {
        case GsTest::AlphaFail::RGB_ONLY:
          alpha_test_mask_alpha_trick = true;
          break;
        case GsTest::AlphaFail::FB_ONLY:
          alpha_test_mask_depth_trick = true;
          break;
        default:
          ASSERT_NOT_REACHED();
      }

    } else {
      ASSERT_NOT_REACHED();
    }
  } else {
    do_alpha_test = false;
  }

  bool writes_alpha = true;
  if (alpha_test_mask_alpha_trick) {
    writes_alpha = false;
    glColorMask(true, true, true, false);
  } else {
    glColorMask(true, true, true, true);
  }

  if (alpha_test_mask_depth_trick) {
    glDepthMask(GL_FALSE);
  } else {
    glDepthMask(GL_TRUE);
  }

  ASSERT(shader.test.date() == false);
  // DATM
  ASSERT(shader.test.zte() == true);  // required
  switch (shader.test.ztest()) {
    case GsTest::ZTest::ALWAYS:
      glDisable(GL_DEPTH_TEST);
      break;
    default:
      ASSERT(false);
  }

  if (shader.clamp_u) {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  } else {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  }

  if (shader.clamp_v) {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  } else {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  }

  if (prim_abe) {
    auto blend_a = shader.alpha.a_mode();
    auto blend_b = shader.alpha.b_mode();
    auto blend_c = shader.alpha.c_mode();
    auto blend_d = shader.alpha.d_mode();
    glEnable(GL_BLEND);

    // 0 2 0 1
    if (blend_a == GsAlpha::BlendMode::SOURCE && blend_b == GsAlpha::BlendMode::ZERO_OR_FIXED &&
        blend_c == GsAlpha::BlendMode::SOURCE && blend_d == GsAlpha::BlendMode::DEST) {
      glBlendEquation(GL_FUNC_ADD);
      glBlendFuncSeparate(GL_SRC_ALPHA, GL_ONE, GL_ONE, GL_ZERO);
    } else if (blend_a == GsAlpha::BlendMode::SOURCE && blend_b == GsAlpha::BlendMode::DEST &&
               blend_c == GsAlpha::BlendMode::SOURCE && blend_d == GsAlpha::BlendMode::DEST) {
      glBlendEquation(GL_FUNC_ADD);
      glBlendFuncSeparate(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, GL_ONE, GL_ZERO);
    } else {
      lg::print("unhandled blend: {} {} {} {}\n", (int)blend_a, (int)blend_b, (int)blend_c,
                (int)blend_d);
      ASSERT_NOT_REACHED();
    }

  } else {
    glDisable(GL_BLEND);
  }
  glUniform4i(m_uniforms.channel_scramble, 0, 1, 2, 3);
  return writes_alpha;
}

/*!
 * Set up this texture as a GPU texture. This does a few things:
 * - sets the Kind to GPU
 * - makes sure the texture resource points to a valid OpenGL texture of the right size, without
 *   triggering the resize/delete sync issue mentioned above.
 * - sets flags to indicate if this GPU texture needs to be updated in the pool.
 */
VramEntry* TextureAnimator::setup_vram_entry_for_gpu_texture(int w, int h, int tbp) {
  auto pp = scoped_prof("setup-vram-entry");
  const auto& existing_dest = m_textures.find(tbp);

  // see if we have an existing OpenGL texture at all
  bool existing_opengl = existing_dest != m_textures.end() && existing_dest->second.tex.has_value();

  // see if we can reuse it (same size)
  bool can_reuse = true;
  if (existing_opengl) {
    if (existing_dest->second.tex->height() != h || existing_dest->second.tex->width() != w) {
      dprintf(" can't reuse, size mismatch\n");
      can_reuse = false;
    }
  } else {
    dprintf(" can't reuse, first time using this address\n");
    can_reuse = false;
  }

  VramEntry* entry = nullptr;
  if (can_reuse) {
    // texture is the right size, just use it again.
    entry = &existing_dest->second;
  } else {
    if (existing_opengl) {
      // we have a texture, but it's the wrong type. Remember that we need to update the pool
      entry = &existing_dest->second;
      entry->needs_pool_update = true;
    } else {
      // create the entry. Also need to update the pool
      entry = &m_textures[tbp];
      entry->reset();
      entry->needs_pool_update = true;
    }

    // if we already have a texture, try to swap it with an OpenGL texture of the right size.
    if (entry->tex.has_value()) {
      // gross
      m_opengl_texture_pool.free(entry->tex->texture(), entry->tex->width(), entry->tex->height());
      entry->tex->update_texture_size(w, h);
      entry->tex->update_texture_unsafe(m_opengl_texture_pool.allocate(w, h));
    } else {
      entry->tex.emplace(w, h, GL_UNSIGNED_INT_8_8_8_8_REV);
    }
  }

  entry->kind = VramEntry::Kind::GPU;
  entry->tex_width = w;
  entry->tex_height = h;
  entry->dest_texture_address = tbp;
  return entry;
}

/*!
 * Get a 16x16 CLUT texture, stored in psm32 (in-memory format, not vram). Fatal if it doesn't
 * exist.
 */
const u32* TextureAnimator::get_clut_16_16_psm32(int cbp) {
  const auto& clut_lookup = m_textures.find(cbp);
  if (clut_lookup == m_textures.end()) {
    lg::print("get_clut_16_16_psm32 referenced an unknown clut texture in {}\n", cbp);
    ASSERT_NOT_REACHED();
  }

  if (clut_lookup->second.kind != VramEntry::Kind::CLUT16_16_IN_PSM32) {
    ASSERT_NOT_REACHED();
  }

  return (const u32*)clut_lookup->second.data.data();
}

void TextureAnimator::set_up_opengl_for_fixed(const FixedLayerDef& def,
                                              std::optional<GLint> texture) {
  if (texture) {
    glBindTexture(GL_TEXTURE_2D, *texture);
    glUniform1i(m_uniforms.enable_tex, 1);
  } else {
    glBindTexture(GL_TEXTURE_2D, m_dummy_texture);
    glUniform1i(m_uniforms.enable_tex, 0);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  }
  // tex0
  // assuming default-texture-anim-layer-func, which sets 1.
  glUniform1i(m_uniforms.tcc, 1);

  // ASSERT(shader.tex0.tfx() == GsTex0::TextureFunction::MODULATE);
  // tex1
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  glColorMask(def.channel_masks[0], def.channel_masks[1], def.channel_masks[2],
              def.channel_masks[3]);
  if (def.z_test) {
    ASSERT_NOT_REACHED();
  } else {
    glDisable(GL_DEPTH_TEST);
  }

  if (def.clamp_u) {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  } else {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  }

  if (def.clamp_v) {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  } else {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  }

  if (def.blend_enable) {
    auto blend_a = def.blend_modes[0];
    auto blend_b = def.blend_modes[1];
    auto blend_c = def.blend_modes[2];
    auto blend_d = def.blend_modes[3];
    glEnable(GL_BLEND);

    // 0 2 0 1
    if (blend_a == GsAlpha::BlendMode::SOURCE && blend_b == GsAlpha::BlendMode::ZERO_OR_FIXED &&
        blend_c == GsAlpha::BlendMode::SOURCE && blend_d == GsAlpha::BlendMode::DEST) {
      glBlendEquation(GL_FUNC_ADD);
      glBlendFuncSeparate(GL_SRC_ALPHA, GL_ONE, GL_ONE, GL_ZERO);
    } else if (blend_a == GsAlpha::BlendMode::SOURCE && blend_b == GsAlpha::BlendMode::DEST &&
               blend_c == GsAlpha::BlendMode::SOURCE && blend_d == GsAlpha::BlendMode::DEST) {
      glBlendEquation(GL_FUNC_ADD);
      glBlendFuncSeparate(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, GL_ONE, GL_ZERO);
    } else if (blend_a == GsAlpha::BlendMode::SOURCE &&
               blend_b == GsAlpha::BlendMode::ZERO_OR_FIXED &&
               blend_c == GsAlpha::BlendMode::ZERO_OR_FIXED &&
               blend_d == GsAlpha::BlendMode::ZERO_OR_FIXED && def.blend_fix == 128) {
      glBlendFuncSeparate(GL_ONE, GL_ZERO, GL_ONE, GL_ZERO);
    } else if (blend_a == GsAlpha::BlendMode::SOURCE &&
               blend_b == GsAlpha::BlendMode::ZERO_OR_FIXED &&
               blend_c == GsAlpha::BlendMode::ZERO_OR_FIXED &&
               blend_d == GsAlpha::BlendMode::DEST) {
      // 0, 2, 2, 1
      // (Cs - 0) * FIX / 128 + Cd
      // So source is fix / 128, and dest is 1
      float color = (float)def.blend_fix / 128.f;
      glBlendColor(color, color, color, color);
      glBlendFuncSeparate(GL_CONSTANT_COLOR, GL_ONE, GL_ONE, GL_ZERO);
    } else {
      lg::print("unhandled blend: {} {} {} {}\n", (int)blend_a, (int)blend_b, (int)blend_c,
                (int)blend_d);
      ASSERT_NOT_REACHED();
    }

  } else {
    glDisable(GL_BLEND);
  }
  glUniform4i(m_uniforms.channel_scramble, 0, 1, 2, 3);
}

namespace {
void set_uniform(GLuint uniform, const math::Vector<float, 4>& vf) {
  glUniform4f(uniform, vf.x(), vf.y(), vf.z(), vf.w());
}

void convert_gs_position_to_vec3(float* out, const math::Vector<u32, 4>& in, int w, int h) {
  out[0] = ((((float)in.x()) / 16.f) - 2048.f) / (float)w;
  out[1] = ((((float)in.y()) / 16.f) - 2048.f) / (float)h;
  out[2] = 0;  // in.z();  // don't think it matters
}

void convert_gs_uv_to_vec2(float* out, const math::Vector<float, 4>& in) {
  out[0] = in.x();
  out[1] = in.y();
}
}  // namespace

void TextureAnimator::set_uniforms_from_draw_data(const DrawData& dd, int dest_w, int dest_h) {
  set_uniform(m_uniforms.rgba, dd.color.cast<float>());

  float pos[3 * 4 + 1];
  convert_gs_position_to_vec3(pos, dd.pos0, dest_w, dest_h);
  convert_gs_position_to_vec3(pos + 3, dd.pos1, dest_w, dest_h);
  convert_gs_position_to_vec3(pos + 6, dd.pos2, dest_w, dest_h);
  convert_gs_position_to_vec3(pos + 9, dd.pos3, dest_w, dest_h);
  glUniform3fv(m_uniforms.positions, 4, pos);
  //  for (int i = 0; i < 4; i++) {
  //    lg::print("fan vp {}: {:.3f} {:.3f} {:.3f}\n", i, pos[i * 3], pos[1 + i * 3], pos[2 + i *
  //    3]);
  //  }

  float uv[2 * 4];
  convert_gs_uv_to_vec2(uv, dd.st0);
  convert_gs_uv_to_vec2(uv + 2, dd.st1);
  convert_gs_uv_to_vec2(uv + 4, dd.st2);
  convert_gs_uv_to_vec2(uv + 6, dd.st3);
  glUniform2fv(m_uniforms.uvs, 4, uv);
  //  for (int i = 0; i < 4; i++) {
  //    lg::print("fan vt {}: {:.3f} {:.3f} \n", i, uv[i * 2], uv[1 + i * 2]);
  //  }
}

void TextureAnimator::run_fixed_animation_array(int idx,
                                                const DmaTransfer& transfer,
                                                TexturePool* texture_pool) {
  auto& array = m_fixed_anim_arrays.at(idx);

  // sanity check size:
  size_t expected_size_bytes = 0;
  for (auto& a : array.anims) {
    expected_size_bytes += 16;
    expected_size_bytes += 16 * 10 * a.dynamic_data.size();
  }
  ASSERT(transfer.size_bytes == expected_size_bytes);

  const u8* data_in = transfer.data;

  for (size_t i = 0; i < array.anims.size(); i++) {
    auto& anim = array.anims[i];
    float time = 0;
    u32 tbp = UINT32_MAX;
    memcpy(&time, data_in, sizeof(float));
    memcpy(&tbp, data_in + 4, sizeof(u32));
    data_in += 16;

    // update parameters for layers:
    for (auto& layer : anim.dynamic_data) {
      memcpy(&layer.start_vals, data_in, sizeof(LayerVals));
      data_in += sizeof(LayerVals);
      memcpy(&layer.end_vals, data_in, sizeof(LayerVals));
      data_in += sizeof(LayerVals);
    }

    // run layers
    run_fixed_animation(anim, time);

    // give to the pool for renderers that don't know how to access this directly
    if (anim.def.move_to_pool) {
      ASSERT(tbp < 0x40000);
      m_skip_tbps.push_back(tbp);  // known to be an output texture.
      if (anim.pool_gpu_tex) {
        // if the debug checkbox is checked, replace the texture with red.
        if (m_output_debug_flags.at(anim.dest_slot).b) {
          FramebufferTexturePairContext ctxt(*anim.fbt);
          glColorMask(true, true, true, true);
          glClearColor(1.0, 0.0, 0.0, 0.5);
          glClear(GL_COLOR_BUFFER_BIT);
        }

        texture_pool->move_existing_to_vram(anim.pool_gpu_tex, tbp);
        ASSERT(texture_pool->lookup(tbp).value() == anim.fbt->texture());
      } else {
        TextureInput in;
        in.gpu_texture = anim.fbt->texture();
        in.w = anim.fbt->width();
        in.h = anim.fbt->height();
        in.debug_page_name = "PC-ANIM";
        in.debug_name = std::to_string(tbp);
        in.id = get_id_for_tbp(texture_pool, tbp, idx);
        anim.pool_gpu_tex = texture_pool->give_texture_and_load_to_vram(in, tbp);
      }
    }
  }
}

template <typename T>
void interpolate_1(float interp, T* out, const T& in_start, const T& in_end) {
  *out = in_start + (in_end - in_start) * interp;
}

void interpolate_layer_values(float interp,
                              LayerVals* out,
                              const LayerVals& start,
                              const LayerVals& end) {
  interpolate_1(interp, &out->color, start.color, end.color);
  interpolate_1(interp, &out->scale, start.scale, end.scale);
  interpolate_1(interp, &out->offset, start.offset, end.offset);
  interpolate_1(interp, &out->st_scale, start.st_scale, end.st_scale);
  interpolate_1(interp, &out->st_offset, start.st_offset, end.st_offset);
  interpolate_1(interp, &out->qs, start.qs, end.qs);
  interpolate_1(interp, &out->rot, start.rot, end.rot);
  interpolate_1(interp, &out->st_rot, start.st_rot, end.st_rot);
}

void TextureAnimator::set_draw_data_from_interpolated(DrawData* result,
                                                      const LayerVals& vals,
                                                      int w,
                                                      int h) {
  result->color = (vals.color * 128.f).cast<u32>();
  math::Vector2f pos_scale(vals.scale.x() * w, vals.scale.y() * h);
  math::Vector2f pos_offset(2048.f + (vals.offset.x() * w), 2048.f + (vals.offset.y() * h));
  math::Vector2f st_scale = vals.st_scale;
  math::Vector2f st_offset = vals.st_offset;
  const std::array<math::Vector2f, 4> fixed_corners = {
      math::Vector2f{-0.5, -0.5}, math::Vector2f{0.5, -0.5}, math::Vector2f{-0.5, 0.5},
      math::Vector2f{0.5, 0.5}};
  auto pos_corners = fixed_corners;

  if (vals.rot) {
    const float rotation_radians = 2.f * M_PI * vals.rot / 65536.f;
    const float sine = std::sin(rotation_radians);
    const float cosine = std::cos(rotation_radians);
    math::Vector2f vx(sine, cosine);
    math::Vector2f vy(cosine, -sine);
    for (auto& corner : pos_corners) {
      corner = vx * corner.x() + vy * corner.y();
    }
  }
  math::Vector2f sts[4];
  math::Vector2<u32> poss[4];

  for (int i = 0; i < 4; i++) {
    poss[i] = ((pos_corners[i].elementwise_multiply(pos_scale) + pos_offset) * 16.f).cast<u32>();
  }

  if (vals.st_rot != 0) {
    const float rotation_radians = 2.f * M_PI * vals.st_rot / 65536.f;
    const float sine = std::sin(rotation_radians);
    const float cosine = std::cos(rotation_radians);
    math::Vector2f vx(sine, cosine);
    math::Vector2f vy(cosine, -sine);
    for (int i = 0; i < 4; i++) {
      math::Vector2f corner = fixed_corners[i].elementwise_multiply(st_scale);
      sts[i] = st_offset + vx * corner.x() + vy * corner.y();
    }
  } else {
    for (int i = 0; i < 4; i++) {
      sts[i] = fixed_corners[i].elementwise_multiply(st_scale) + st_offset;
    }
  }

  result->st0.x() = sts[0].x();
  result->st0.y() = sts[0].y();
  result->st1.x() = sts[1].x();
  result->st1.y() = sts[1].y();
  result->st2.x() = sts[2].x();
  result->st2.y() = sts[2].y();
  result->st3.x() = sts[3].x();
  result->st3.y() = sts[3].y();

  result->pos0.x() = poss[0].x();
  result->pos0.y() = poss[0].y();
  result->pos1.x() = poss[1].x();
  result->pos1.y() = poss[1].y();
  result->pos2.x() = poss[2].x();
  result->pos2.y() = poss[2].y();
  result->pos3.x() = poss[3].x();
  result->pos3.y() = poss[3].y();
}

void TextureAnimator::run_fixed_animation(FixedAnim& anim, float time) {
  {
    FramebufferTexturePairContext ctxt(anim.fbt.value());
    // Clear
    {
      float positions[3 * 4] = {0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0};
      glUniform3fv(m_uniforms.positions, 4, positions);
      glUniform1i(m_uniforms.enable_tex, 0);
      glUniform4f(m_uniforms.rgba, anim.def.color[0], anim.def.color[1], anim.def.color[2],
                  anim.def.color[3]);
      glUniform4i(m_uniforms.channel_scramble, 0, 1, 2, 3);
      glBindTexture(GL_TEXTURE_2D, m_dummy_texture);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glDisable(GL_BLEND);
      glDisable(GL_DEPTH_TEST);
      glColorMask(true, true, true, true);
      glUniform1f(m_uniforms.alpha_multiply, 1.f);
      glDrawArrays(GL_TRIANGLE_FAN, 0, 4);
    }

    LayerVals interpolated_values;
    DrawData draw_data;
    // Loop over layers
    for (size_t layer_idx = 0; layer_idx < anim.def.layers.size(); layer_idx++) {
      auto& layer_def = anim.def.layers[layer_idx];
      auto& layer_dyn = anim.dynamic_data[layer_idx];
      // skip layer if out the range when it is active
      if (time < layer_def.start_time || time > layer_def.end_time) {
        continue;
      }

      if (layer_def.disable) {
        continue;
      }

      // interpolate
      interpolate_layer_values(
          (time - layer_def.start_time) / (layer_def.end_time - layer_def.start_time),
          &interpolated_values, layer_dyn.start_vals, layer_dyn.end_vals);

      // shader setup
      const auto& src = anim.src_textures.at(layer_idx);
      if (src.is_anim_slot) {
        set_up_opengl_for_fixed(layer_def, m_private_output_slots.at(src.idx));
      } else {
        set_up_opengl_for_fixed(layer_def, src.idx);
      }

      set_draw_data_from_interpolated(&draw_data, interpolated_values, anim.fbt->width(),
                                      anim.fbt->height());
      set_uniforms_from_draw_data(draw_data, anim.fbt->width(), anim.fbt->height());

      if (true) {  // todo
        glColorMask(true, true, true, false);
        glUniform1f(m_uniforms.alpha_multiply, 2.f);
        glUniform1i(m_uniforms.set_alpha, 0);
        glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
        glColorMask(false, false, false, true);
        if (anim.def.set_alpha) {
          glUniform1i(m_uniforms.set_alpha, 1);
          glUniform4f(m_uniforms.rgba, draw_data.color.x(), draw_data.color.x(),
                      draw_data.color.x(), 128.f);
        }
        glUniform1f(m_uniforms.alpha_multiply, 1.f);
        glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
      } else {
        // we don't write alpha out. So apply alpha multiplier for blending.
        glUniform1f(m_uniforms.alpha_multiply, 2.f);
        glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
      }
    }
  }
  // Finish
  m_private_output_slots.at(anim.dest_slot) = anim.fbt->texture();
  glBindTexture(GL_TEXTURE_2D, anim.fbt->texture());
  glGenerateMipmap(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, 0);
}

// initial values of the random table for cloud texture generation.
constexpr Vector16ub kInitialRandomTable[TextureAnimator::kRandomTableSize] = {
    {0x20, 0x19, 0x18, 0x17, 0x16, 0x15, 0x14, 0x13, 0x12, 0x11, 0x10, 0x89, 0x67, 0x45, 0x23, 0x1},
    {0x37, 0x82, 0x87, 0x23, 0x78, 0x87, 0x4, 0x32, 0x97, 0x91, 0x48, 0x98, 0x30, 0x38, 0x89, 0x87},
    {0x62, 0x47, 0x2, 0x62, 0x78, 0x92, 0x28, 0x90, 0x81, 0x47, 0x72, 0x28, 0x83, 0x29, 0x71, 0x68},
    {0x28, 0x61, 0x17, 0x62, 0x87, 0x74, 0x38, 0x12, 0x83, 0x9, 0x78, 0x12, 0x76, 0x31, 0x72, 0x80},
    {0x39, 0x72, 0x98, 0x34, 0x72, 0x98, 0x69, 0x78, 0x65, 0x71, 0x98, 0x83, 0x97, 0x23, 0x98, 0x1},
    {0x97, 0x38, 0x72, 0x98, 0x23, 0x87, 0x23, 0x98, 0x93, 0x72, 0x98, 0x20, 0x81, 0x29, 0x10,
     0x62},
    {0x28, 0x75, 0x38, 0x82, 0x99, 0x30, 0x72, 0x87, 0x83, 0x9, 0x14, 0x98, 0x10, 0x43, 0x87, 0x29},
    {0x87, 0x23, 0x0, 0x87, 0x18, 0x98, 0x12, 0x98, 0x10, 0x98, 0x21, 0x83, 0x90, 0x37, 0x62,
     0x71}};

/*!
 * Update dest and random_table.
 */
int make_noise_texture(u8* dest, Vector16ub* random_table, int dim, int random_index_in) {
  ASSERT(dim % 16 == 0);
  const int qw_per_row = dim / 16;
  for (int row = 0; row < dim; row++) {
    for (int qw_in_row = 0; qw_in_row < qw_per_row; qw_in_row++) {
      const int row_start_qwi = row * qw_per_row;
      Vector16ub* rand_rows[4] = {
          random_table + (random_index_in + 0) % TextureAnimator::kRandomTableSize,
          random_table + (random_index_in + 3) % TextureAnimator::kRandomTableSize,
          random_table + (random_index_in + 5) % TextureAnimator::kRandomTableSize,
          random_table + (random_index_in + 7) % TextureAnimator::kRandomTableSize,
      };
      const int qwi = row_start_qwi + qw_in_row;
      *rand_rows[3] = *rand_rows[0] + *rand_rows[1] + *rand_rows[2];
      memcpy(dest + 16 * qwi, rand_rows[3]->data(), 16);
      random_index_in = (random_index_in + 1) % TextureAnimator::kRandomTableSize;
    }
  }
  return random_index_in;
}

int update_opengl_noise_texture(GLuint texture,
                                u8* temp,
                                Vector16ub* random_table,
                                int dim,
                                int random_index_in) {
  int ret = make_noise_texture(temp, random_table, dim, random_index_in);
  glBindTexture(GL_TEXTURE_2D, texture);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, dim, dim, 0, GL_RED, GL_UNSIGNED_BYTE, temp);
  glGenerateMipmap(GL_TEXTURE_2D);
  return ret;
}

void debug_save_opengl_u8_texture(const std::string& out, GLuint texture) {
  glBindTexture(GL_TEXTURE_2D, texture);
  int w, h;
  glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, &w);
  glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, &h);
  lg::print("saving texture with size {} x {}\n", w, h);
  std::vector<u8> data_r(w * h);
  glGetTexImage(GL_TEXTURE_2D, 0, GL_RED, GL_UNSIGNED_BYTE, data_r.data());
  std::vector<u8> data(w * h * 4);
  for (int i = 0; i < w * h; i++) {
    data[i * 4] = data_r[i];
    data[i * 4 + 1] = data_r[i];
    data[i * 4 + 2] = data_r[i];
    data[i * 4 + 3] = 255;
  }
  file_util::write_rgba_png(out, data.data(), w, h);
}

void TextureAnimator::setup_sky() {
  // sky
  // initialize random table with values from the game.
  for (int i = 0; i < kRandomTableSize; i++) {
    m_random_table[i] = kInitialRandomTable[i];
  }

  {
    for (int i = 0, dim = kFinalSkyTextureSize >> (kNumSkyNoiseLayers - 1); i < kNumSkyNoiseLayers;
         i++, dim *= 2) {
      auto& tex = m_sky_noise_textures[i];
      tex.temp_data.resize(dim * dim);
      tex.dim = dim;
      glGenTextures(1, &tex.new_tex);
      m_random_index = update_opengl_noise_texture(tex.new_tex, tex.temp_data.data(),
                                                   m_random_table, dim, m_random_index);
      glGenTextures(1, &tex.old_tex);
      m_random_index = update_opengl_noise_texture(tex.old_tex, tex.temp_data.data(),
                                                   m_random_table, dim, m_random_index);
    }
  }
  {
    for (int i = 0, dim = kFinalSkyHiresTextureSize >> (kNumSkyHiresNoiseLayers - 1);
         i < kNumSkyHiresNoiseLayers; i++, dim *= 2) {
      auto& tex = m_sky_hires_noise_textures[i];
      tex.temp_data.resize(dim * dim);
      tex.dim = dim;
      glGenTextures(1, &tex.new_tex);
      m_random_index = update_opengl_noise_texture(tex.new_tex, tex.temp_data.data(),
                                                   m_random_table, dim, m_random_index);
      glGenTextures(1, &tex.old_tex);
      m_random_index = update_opengl_noise_texture(tex.old_tex, tex.temp_data.data(),
                                                   m_random_table, dim, m_random_index);
    }
  }
}

GLint TextureAnimator::run_clouds(const SkyInput& input, bool hires) {
  m_debug_sky_input = input;

  // anim 0 creates a clut with rgba = 128, 128, 128, i, at tbp = (24 * 32)
  // (this has alphas from 0 to 256).
  // This step is eliminated on OpenGL because we don't need this simple ramp CLUT.

  // the next anim uses that clut with noise textures.
  // so we expect those textures to have values like (128, 128, 128, x) where 0 <= x <= 255.
  // (in OpenGL, we create these with a single-channel texture, with that channel in 0 - 1)

  // this repeats for different resolutions (4 times in total)

  // Next, these are blended together into a single texture
  // The blend mode is 0, 2, 0, 1
  // [(CSource - 0) * Asource] >> 7 + CDest
  // in the PS2, CSource is 128, so the >> 7 cancels entirely.

  // note that in hires mode (this is new to opengoal), the max size is upped to 256 and different
  // textures get used
  auto& blend_tex = hires ? m_sky_hires_blend_texture : m_sky_blend_texture;
  auto& final_tex = hires ? m_sky_hires_final_texture : m_sky_final_texture;

  int times_idx = 0;
  // Anim 0:
  // this create a 16x16 CLUT with RGB = 128, 128, 128 and alpha = i
  // (texture-anim-alpha-ramp-clut-init)
  // it's uploaded 24 * 32 = 768. (texture-anim-alpha-ramp-clut-upload)
  times_idx++;
  {
    FramebufferTexturePairContext ctxt(blend_tex);
    glClearColor(0.0, 0.0, 0.0, 0.0);
    glClear(GL_COLOR_BUFFER_BIT);
    glUniform1i(m_uniforms.tcc, 1);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glColorMask(true, true, true, true);
    glDisable(GL_DEPTH_TEST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glEnable(GL_BLEND);
    glBlendEquation(GL_FUNC_ADD);
    glBlendFuncSeparate(GL_ONE, GL_ONE, GL_ZERO, GL_ZERO);
    glUniform4i(m_uniforms.channel_scramble, 0, 0, 0, 0);
    glUniform1f(m_uniforms.alpha_multiply, 1.f);
    glUniform1i(m_uniforms.enable_tex, 1);

    float positions[3 * 4] = {0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0};
    glUniform3fv(m_uniforms.positions, 4, positions);
    float uv[2 * 4] = {0, 0, 1, 0, 1, 1, 0, 1};
    glUniform2fv(m_uniforms.uvs, 4, uv);

    // Anim 1:
    // noise (16x16)
    // while (noise_layer_idx) {
    NoiseTexturePair* noise_textures = hires ? m_sky_hires_noise_textures : m_sky_noise_textures;
    int noise_layer_amt = hires ? kNumSkyHiresNoiseLayers : kNumSkyNoiseLayers;
    for (int noise_layer_idx = 0; noise_layer_idx < noise_layer_amt; noise_layer_idx++) {
      const float new_time = input.times[times_idx];
      auto& ntp = noise_textures[noise_layer_idx];

      ntp.max_time = input.max_times[noise_layer_idx];
      ntp.scale = input.scales[noise_layer_idx];

      if (new_time < ntp.last_time) {
        std::swap(ntp.new_tex, ntp.old_tex);
        m_random_index = update_opengl_noise_texture(ntp.new_tex, ntp.temp_data.data(),
                                                     m_random_table, ntp.dim, m_random_index);
      }
      ntp.last_time = new_time;
      float new_interp = ntp.last_time / ntp.max_time;

      glBindTexture(GL_TEXTURE_2D, ntp.new_tex);
      float s = new_interp * ntp.scale * 128.f;
      set_uniform(m_uniforms.rgba, math::Vector4f(s, s, s, 256));
      glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

      glBindTexture(GL_TEXTURE_2D, ntp.old_tex);
      s = (1.f - new_interp) * ntp.scale * 128.f;
      set_uniform(m_uniforms.rgba, math::Vector4f(s, s, s, 256));
      glDrawArrays(GL_TRIANGLE_FAN, 0, 4);
      times_idx++;
    }
  }

  FramebufferTexturePairContext ctxt(final_tex);
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT);
  glUniform1i(m_uniforms.enable_tex, 2);
  glBindTexture(GL_TEXTURE_2D, blend_tex.texture());
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glUniform1f(m_uniforms.minimum, input.cloud_min);
  glUniform1f(m_uniforms.maximum, input.cloud_max);
  glDisable(GL_BLEND);
  glDrawArrays(GL_TRIANGLE_FAN, 0, 4);
  glBindTexture(GL_TEXTURE_2D, final_tex.texture());
  glGenerateMipmap(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, 0);
  return final_tex.texture();
}

void TextureAnimator::run_slime(const SlimeInput& input) {
  m_debug_slime_input = input;

  int times_idx = 0;
  times_idx++;
  {
    FramebufferTexturePairContext ctxt(m_slime_blend_texture);
    glClearColor(0.0, 0.0, 0.0, 0.0);
    glClear(GL_COLOR_BUFFER_BIT);
    glUniform1i(m_uniforms.tcc, 1);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glColorMask(true, true, true, true);
    glDisable(GL_DEPTH_TEST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glEnable(GL_BLEND);
    glBlendEquation(GL_FUNC_ADD);
    glBlendFuncSeparate(GL_ONE, GL_ONE, GL_ZERO, GL_ZERO);
    glUniform4i(m_uniforms.channel_scramble, 0, 0, 0, 0);
    glUniform1f(m_uniforms.alpha_multiply, 1.f);
    glUniform1i(m_uniforms.enable_tex, 1);
    float positions[3 * 4] = {0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0};
    glUniform3fv(m_uniforms.positions, 4, positions);
    float uv[2 * 4] = {0, 0, 1, 0, 1, 1, 0, 1};
    glUniform2fv(m_uniforms.uvs, 4, uv);

    glActiveTexture(GL_TEXTURE10);
    glBindTexture(GL_TEXTURE_1D, m_slime_lut_texture);
    glActiveTexture(GL_TEXTURE0);

    // Anim 1:
    // noise (16x16)
    // while (noise_layer_idx) {
    for (int noise_layer_idx = 0; noise_layer_idx < kNumSlimeNoiseLayers; noise_layer_idx++) {
      const float new_time = input.times[times_idx];
      auto& ntp = m_slime_noise_textures[noise_layer_idx];

      if (new_time < ntp.last_time) {
        std::swap(ntp.new_tex, ntp.old_tex);
        m_random_index = update_opengl_noise_texture(ntp.new_tex, ntp.temp_data.data(),
                                                     m_random_table, ntp.dim, m_random_index);
      }
      ntp.last_time = new_time;
      float new_interp = ntp.last_time / ntp.max_time;

      glBindTexture(GL_TEXTURE_2D, ntp.new_tex);
      float s = new_interp * ntp.scale * 128.f;
      set_uniform(m_uniforms.rgba, math::Vector4f(s, s, s, 256));
      glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

      glBindTexture(GL_TEXTURE_2D, ntp.old_tex);
      s = (1.f - new_interp) * ntp.scale * 128.f;
      set_uniform(m_uniforms.rgba, math::Vector4f(s, s, s, 256));
      glDrawArrays(GL_TRIANGLE_FAN, 0, 4);
      times_idx++;
    }
  }

  {
    FramebufferTexturePairContext ctxt(m_slime_final_texture);
    glClearColor(0.0, 0.0, 0.0, 0.0);
    glClear(GL_COLOR_BUFFER_BIT);
    glUniform1i(m_uniforms.enable_tex, 3);
    glUniform1f(m_uniforms.slime_scroll, 0);
    glBindTexture(GL_TEXTURE_2D, m_slime_blend_texture.texture());
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glDisable(GL_BLEND);
    glDrawArrays(GL_TRIANGLE_FAN, 0, 4);
  }
  glBindTexture(GL_TEXTURE_2D, m_slime_final_texture.texture());
  glGenerateMipmap(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, 0);

  {
    FramebufferTexturePairContext ctxt(m_slime_final_scroll_texture);
    glClearColor(0.0, 0.0, 0.0, 0.0);
    glClear(GL_COLOR_BUFFER_BIT);
    glUniform1i(m_uniforms.enable_tex, 3);
    float scroll = input.times[8] / 1200.f;
    glUniform1f(m_uniforms.slime_scroll, scroll);
    glBindTexture(GL_TEXTURE_2D, m_slime_blend_texture.texture());
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glDisable(GL_BLEND);
    glDrawArrays(GL_TRIANGLE_FAN, 0, 4);
  }
  glBindTexture(GL_TEXTURE_2D, m_slime_final_scroll_texture.texture());
  glGenerateMipmap(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, 0);
}
