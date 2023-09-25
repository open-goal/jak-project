#include "TextureAnimator.h"

#include "common/global_profiler/GlobalProfiler.h"
#include "common/texture/texture_slots.h"
#include "common/util/FileUtil.h"
#include "common/util/Timer.h"

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

/*!
 * A simple list of preallocated textures by size. If a texture needs to be resized, it's faster
 * to swap to a different OpenGL texture from this pool than glTexImage2D with a different size.
 */
OpenGLTexturePool::OpenGLTexturePool() {
  struct Alloc {
    u64 w, h, n;
  };
  // list of sizes to preallocate: {width, height, count}.
  for (const auto& a : std::vector<Alloc>{{4, 4, 2},
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
                                          {256, 256, 7}}) {
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
      fmt::print("texture: {}\n", t.debug_name);
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

TextureAnimator::TextureAnimator(ShaderLibrary& shaders, const tfrag3::Level* common_level)
    : m_common_level(common_level),
      m_psm32_to_psm8_8_8(8, 8, 8, 64),
      m_psm32_to_psm8_16_16(16, 16, 16, 64),
      m_psm32_to_psm8_32_32(32, 32, 16, 64),
      m_psm32_to_psm8_64_64(64, 64, 64, 64),
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
                                   GL_UNSIGNED_INT_8_8_8_8_REV) {
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

  m_public_output_slots.resize(jak2_animated_texture_slots().size(), m_dummy_texture);
  m_private_output_slots = m_public_output_slots;
  m_output_debug_flags.resize(jak2_animated_texture_slots().size());

  // animation-specific stuff
  setup_texture_anims();

  setup_sky();
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
    anim.dest_slot = output_slot_by_idx(GameVersion::Jak2, anim.def.tex_name);
    auto* dtex = tex_by_name(m_common_level, anim.def.tex_name);
    if (anim.def.override_size) {
      anim.fbt.emplace(anim.def.override_size->x(), anim.def.override_size->y(),
                       GL_UNSIGNED_INT_8_8_8_8_REV);
    } else {
      anim.fbt.emplace(dtex->w, dtex->h, GL_UNSIGNED_INT_8_8_8_8_REV);
      opengl_upload_texture(anim.fbt->texture(), dtex->data.data(), dtex->w, dtex->h);
    }

    m_private_output_slots.at(anim.dest_slot) = anim.fbt->texture();

    // set up the source textures
    for (const auto& layer : def.layers) {
      auto* stex = tex_by_name(m_common_level, layer.tex_name);
      GLint gl_texture = m_opengl_texture_pool.allocate(stex->w, stex->h);
      anim.src_textures.push_back(gl_texture);
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

  auto& slots = jak2_animated_texture_slots();
  for (size_t i = 0; i < slots.size(); i++) {
    ImGui::Text("Slot %d %s (%d)", (int)i, slots[i].c_str(), (int)m_private_output_slots[i]);
    imgui_show_tex(m_private_output_slots[i]);
    ImGui::Checkbox(fmt::format("mark {}", i).c_str(), &m_output_debug_flags.at(i).b);
  }
  glBindTexture(GL_TEXTURE_2D, 0);
}

void TextureAnimator::copy_private_to_public() {
  auto& slots = jak2_animated_texture_slots();
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
                                               const std::optional<std::string>& dgo) {
  int ret = m_clut_blender_groups.size();
  m_clut_blender_groups.emplace_back();
  add_to_clut_blender_group(ret, textures, suffix0, suffix1, dgo);
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
    grp.outputs.push_back(output_slot_by_idx(GameVersion::Jak2, prefix));
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
enum PcTextureAnimCodes {
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
      switch (vif0.immediate) {
        case UPLOAD_CLUT_16_16: {
          auto p = scoped_prof("clut-16-16");
          handle_upload_clut_16_16(tf, ee_mem);
        } break;
        case ERASE_DEST_TEXTURE: {
          auto p = scoped_prof("erase");
          handle_erase_dest(dma);
        } break;
        case GENERIC_UPLOAD: {
          auto p = scoped_prof("generic-upload");
          handle_generic_upload(tf, ee_mem);
        } break;
        case SET_SHADER: {
          auto p = scoped_prof("set-shader");
          handle_set_shader(dma);
        } break;
        case DRAW: {
          auto p = scoped_prof("draw");
          handle_draw(dma, *texture_pool);
        } break;
        case FINISH_ARRAY:
          done = true;
          break;
        case DARKJAK: {
          auto p = scoped_prof("darkjak");
          run_clut_blender_group(tf, m_darkjak_clut_blender_idx, frame_idx);
        } break;
        case PRISON_JAK: {
          auto p = scoped_prof("prisonjak");
          run_clut_blender_group(tf, m_jakb_prison_clut_blender_idx, frame_idx);
        } break;
        case ORACLE_JAK: {
          auto p = scoped_prof("oraclejak");
          run_clut_blender_group(tf, m_jakb_oracle_clut_blender_idx, frame_idx);
        } break;
        case NEST_JAK: {
          auto p = scoped_prof("nestjak");
          run_clut_blender_group(tf, m_jakb_nest_clut_blender_idx, frame_idx);
        } break;
        case KOR_TRANSFORM: {
          auto p = scoped_prof("kor");
          run_clut_blender_group(tf, m_kor_transform_clut_blender_idx, frame_idx);
        } break;
        case SKULL_GEM: {
          auto p = scoped_prof("skull-gem");
          run_fixed_animation_array(m_skull_gem_fixed_anim_array_idx, tf, texture_pool);
        } break;
        case BOMB: {
          auto p = scoped_prof("bomb");
          run_fixed_animation_array(m_bomb_fixed_anim_array_idx, tf, texture_pool);
        } break;
        case CAS_CONVEYOR: {
          auto p = scoped_prof("cas-conveyor");
          run_fixed_animation_array(m_cas_conveyor_anim_array_idx, tf, texture_pool);
        } break;
        case SECURITY: {
          auto p = scoped_prof("security");
          run_fixed_animation_array(m_security_anim_array_idx, tf, texture_pool);
        } break;
        case WATERFALL: {
          auto p = scoped_prof("waterfall");
          run_fixed_animation_array(m_waterfall_anim_array_idx, tf, texture_pool);
        } break;
        case WATERFALL_B: {
          auto p = scoped_prof("waterfall-b");
          run_fixed_animation_array(m_waterfall_b_anim_array_idx, tf, texture_pool);
        } break;
        case LAVA: {
          auto p = scoped_prof("lava");
          run_fixed_animation_array(m_lava_anim_array_idx, tf, texture_pool);
        } break;
        case LAVA_B: {
          auto p = scoped_prof("lava-b");
          run_fixed_animation_array(m_lava_b_anim_array_idx, tf, texture_pool);
        } break;
        case STADIUMB: {
          auto p = scoped_prof("stadiumb");
          run_fixed_animation_array(m_stadiumb_anim_array_idx, tf, texture_pool);
        } break;
        case FORTRESS_PRIS: {
          auto p = scoped_prof("fort-pris");
          run_fixed_animation_array(m_fortress_pris_anim_array_idx, tf, texture_pool);
        } break;
        case FORTRESS_WARP: {
          auto p = scoped_prof("fort-warp");
          run_fixed_animation_array(m_fortress_warp_anim_array_idx, tf, texture_pool);
        } break;
        case METKOR: {
          auto p = scoped_prof("metkor");
          run_fixed_animation_array(m_metkor_anim_array_idx, tf, texture_pool);
        } break;
        case SHIELD: {
          auto p = scoped_prof("shield");
          run_fixed_animation_array(m_shield_anim_array_idx, tf, texture_pool);
        } break;
        case KREW_HOLO: {
          auto p = scoped_prof("krew-holo");
          run_fixed_animation_array(m_krew_holo_anim_array_idx, tf, texture_pool);
        } break;
        case CLOUDS_AND_FOG:
        case CLOUDS_HIRES: {
          auto p = scoped_prof("clouds-and-fog");
          handle_clouds_and_fog(tf, texture_pool, vif0.immediate == CLOUDS_HIRES);
        } break;
        case SLIME: {
          auto p = scoped_prof("slime");
          handle_slime(tf, texture_pool);
        } break;
        default:
          fmt::print("bad imm: {}\n", vif0.immediate);
          ASSERT_NOT_REACHED();
      }
    } else {
      printf("[tex anim] unhandled VIF in main loop\n");
      fmt::print("{} {}\n", vif0.print(), tf.vifcode1().print());
      fmt::print("dma address 0x{:x}\n", offset);
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
      printf("unhandled non-gpu conversion: %d (tbp = %d)\n", (int)entry.kind, tbp);
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
    auto ret = pool->allocate_pc_port_texture(GameVersion::Jak2);
    m_ids_by_vram[tbp] = ret;
    return ret;
  } else {
    return it->second;
  }
}

void debug_save_opengl_texture(const std::string& out, GLuint texture) {
  glBindTexture(GL_TEXTURE_2D, texture);
  int w, h;
  glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, &w);
  glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, &h);
  fmt::print("saving texture with size {} x {}\n", w, h);
  std::vector<u8> data(w * h * 4);
  glGetTexImage(GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV, data.data());
  file_util::write_rgba_png(out, data.data(), w, h);
}

void TextureAnimator::run_clut_blender_group(DmaTransfer& tf, int idx, u64 frame_idx) {
  float f;
  ASSERT(tf.size_bytes == 16);
  memcpy(&f, tf.data, sizeof(float));
  float weights[2] = {1.f - f, f};
  auto& blender = m_clut_blender_groups.at(idx);
  blender.last_updated_frame = frame_idx;
  for (size_t i = 0; i < blender.blenders.size(); i++) {
    m_private_output_slots[blender.outputs[i]] = blender.blenders[i].run(weights);
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
      fmt::print("Unhandled format: {}\n", upload->format);
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
    printf("set shader referenced an unknown clut texture in %d\n", m_current_shader.tex0.cbp());
    ASSERT_NOT_REACHED();
  }

  switch (clut_lookup->second.kind) {
    case VramEntry::Kind::CLUT16_16_IN_PSM32:
      m_converter.upload_width(clut_lookup->second.data.data(), m_current_shader.tex0.cbp(), 16,
                               16);
      break;
    default:
      printf("unhandled clut source kind: %d\n", (int)clut_lookup->second.kind);
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
    // printf("referenced an unknown texture in %d\n", tbp);
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
            printf("set shader referenced an unknown clut texture in %d\n",
                   m_current_shader.tex0.cbp());
            ASSERT_NOT_REACHED();
          }

          switch (clut_lookup->second.kind) {
            case VramEntry::Kind::CLUT16_16_IN_PSM32:
              break;
            default:
              printf("unhandled clut source kind: %d\n", (int)clut_lookup->second.kind);
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
              printf("Scrambler took the slow path %d x %d took %.3f ms\n", w, h, timer.getMs());
            }
          }
          auto ret = make_temp_gpu_texture(rgba_data.data(), w, h);
          // debug_save_opengl_texture(fmt::format("tex_{}.png", w), ret);
          return ret;

          ASSERT_NOT_REACHED();
        } break;
        default:
          fmt::print("unhandled source texture format {}\n", (int)m_current_shader.tex0.psm());
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
      fmt::print("unhandled blend: {} {} {} {}\n", (int)blend_a, (int)blend_b, (int)blend_c,
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
    printf("get_clut_16_16_psm32 referenced an unknown clut texture in %d\n", cbp);
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
    } else {
      fmt::print("unhandled blend: {} {} {} {}\n", (int)blend_a, (int)blend_b, (int)blend_c,
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
  //    fmt::print("fan vp {}: {:.3f} {:.3f} {:.3f}\n", i, pos[i * 3], pos[1 + i * 3], pos[2 + i *
  //    3]);
  //  }

  float uv[2 * 4];
  convert_gs_uv_to_vec2(uv, dd.st0);
  convert_gs_uv_to_vec2(uv + 2, dd.st1);
  convert_gs_uv_to_vec2(uv + 4, dd.st2);
  convert_gs_uv_to_vec2(uv + 6, dd.st3);
  glUniform2fv(m_uniforms.uvs, 4, uv);
  //  for (int i = 0; i < 4; i++) {
  //    fmt::print("fan vt {}: {:.3f} {:.3f} \n", i, uv[i * 2], uv[1 + i * 2]);
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
  ASSERT(vals.rot == 0);
  result->color = (vals.color * 128.f).cast<u32>();
  math::Vector2f pos_scale(vals.scale.x() * w, vals.scale.y() * h);
  math::Vector2f pos_offset(2048.f + (vals.offset.x() * w), 2048.f + (vals.offset.y() * h));
  math::Vector2f st_scale = vals.st_scale;
  math::Vector2f st_offset = vals.st_offset;
  const math::Vector2f corners[4] = {math::Vector2f{-0.5, -0.5}, math::Vector2f{0.5, -0.5},
                                     math::Vector2f{-0.5, 0.5}, math::Vector2f{0.5, 0.5}};
  math::Vector2f sts[4];
  math::Vector2<u32> poss[4];

  for (int i = 0; i < 4; i++) {
    poss[i] = ((corners[i].elementwise_multiply(pos_scale) + pos_offset) * 16.f).cast<u32>();
  }

  if (vals.st_rot != 0) {
    const float rotation_radians = 2.f * M_PI * vals.st_rot / 65536.f;
    const float sine = std::sin(rotation_radians);
    const float cosine = std::cos(rotation_radians);
    math::Vector2f vx(sine, cosine);
    math::Vector2f vy(cosine, -sine);
    for (int i = 0; i < 4; i++) {
      math::Vector2f corner = corners[i].elementwise_multiply(st_scale);
      sts[i] = st_offset + vx * corner.x() + vy * corner.y();
    }
  } else {
    for (int i = 0; i < 4; i++) {
      sts[i] = corners[i].elementwise_multiply(st_scale) + st_offset;
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

      // interpolate
      interpolate_layer_values(
          (time - layer_def.start_time) / (layer_def.end_time - layer_def.start_time),
          &interpolated_values, layer_dyn.start_vals, layer_dyn.end_vals);

      // shader setup
      set_up_opengl_for_fixed(layer_def, anim.src_textures.at(layer_idx));

      set_draw_data_from_interpolated(&draw_data, interpolated_values, anim.fbt->width(),
                                      anim.fbt->height());
      set_uniforms_from_draw_data(draw_data, anim.fbt->width(), anim.fbt->height());

      if (true) {  // todo
        glColorMask(true, true, true, false);
        glUniform1f(m_uniforms.alpha_multiply, 2.f);
        glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
        glColorMask(false, false, false, true);
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

void TextureAnimator::setup_texture_anims() {
  // DARKJAK
  m_darkjak_clut_blender_idx = create_clut_blender_group(
      {"jakbsmall-eyebrow", "jakbsmall-face", "jakbsmall-finger", "jakbsmall-hair"}, "-norm",
      "-dark", {});

  // PRISON
  // MISSING EYELID
  m_jakb_prison_clut_blender_idx = create_clut_blender_group(
      {"jak-orig-arm-formorph", "jak-orig-eyebrow-formorph", "jak-orig-finger-formorph"}, "-start",
      "-end", "LDJAKBRN.DGO");
  add_to_clut_blender_group(m_jakb_prison_clut_blender_idx,
                            {"jakb-facelft", "jakb-facert", "jakb-hairtrans"}, "-norm", "-dark",
                            "LDJAKBRN.DGO");

  // ORACLE
  // MISSING FINGER
  m_jakb_oracle_clut_blender_idx = create_clut_blender_group(
      {"jakb-eyebrow", "jakb-eyelid", "jakb-facelft", "jakb-facert", "jakb-hairtrans"}, "-norm",
      "-dark", "ORACLE.DGO");

  // NEST
  // MISSING FINGER
  m_jakb_nest_clut_blender_idx = create_clut_blender_group(
      {"jakb-eyebrow", "jakb-eyelid", "jakb-facelft", "jakb-facert", "jakb-hairtrans"}, "-norm",
      "-dark", "NEB.DGO");

  // KOR (doesn't work??)
  m_kor_transform_clut_blender_idx = create_clut_blender_group(
      {
          // "kor-eyeeffect-formorph",
          // "kor-hair-formorph",
          // "kor-head-formorph",
          // "kor-head-formorph-noreflect",
          // "kor-lowercaps-formorph",
          // "kor-uppercaps-formorph",
      },
      "-start", "-end", {});

  // Skull Gem
  {
    FixedAnimDef skull_gem;
    skull_gem.move_to_pool = true;
    skull_gem.tex_name = "skull-gem-dest";
    skull_gem.color = math::Vector4<u8>{0, 0, 0, 0x80};
    // overriden in texture-finish.gc
    skull_gem.override_size = math::Vector2<int>(32, 32);

    auto& skull_gem_0 = skull_gem.layers.emplace_back();
    skull_gem_0.end_time = 300.;
    skull_gem_0.tex_name = "skull-gem-alpha-00";
    skull_gem_0.set_blend_b2_d1();
    skull_gem_0.set_no_z_write_no_z_test();

    auto& skull_gem_1 = skull_gem.layers.emplace_back();
    skull_gem_1.end_time = 300.;
    skull_gem_1.tex_name = "skull-gem-alpha-01";
    skull_gem_1.set_blend_b2_d1();
    skull_gem_1.set_no_z_write_no_z_test();

    auto& skull_gem_2 = skull_gem.layers.emplace_back();
    skull_gem_2.end_time = 300.;
    skull_gem_2.tex_name = "skull-gem-alpha-02";
    skull_gem_2.set_blend_b2_d1();
    skull_gem_2.set_no_z_write_no_z_test();

    m_skull_gem_fixed_anim_array_idx = create_fixed_anim_array({skull_gem});
  }

  // Bomb
  {
    FixedAnimDef bomb;
    bomb.tex_name = "bomb-gradient";
    bomb.color = math::Vector4<u8>{0, 0, 0, 0x80};

    auto& bomb_0 = bomb.layers.emplace_back();
    bomb_0.end_time = 300.;
    bomb_0.tex_name = "bomb-gradient-rim";
    bomb_0.set_blend_b2_d1();
    bomb_0.set_no_z_write_no_z_test();
    // :test (new 'static 'gs-test :ate #x1 :afail #x3 :zte #x1 :ztst (gs-ztest always))
    bomb_0.channel_masks[3] = false;  // no alpha writes.

    auto& bomb_1 = bomb.layers.emplace_back();
    bomb_1.end_time = 300.;
    bomb_1.tex_name = "bomb-gradient-flames";
    bomb_1.set_blend_b2_d1();
    bomb_1.set_no_z_write_no_z_test();
    // :test (new 'static 'gs-test :ate #x1 :afail #x3 :zte #x1 :ztst (gs-ztest always))
    bomb_1.channel_masks[3] = false;  // no alpha writes.

    m_bomb_fixed_anim_array_idx = create_fixed_anim_array({bomb});
  }

  // CAS conveyor
  {
    FixedAnimDef conveyor_0;
    conveyor_0.tex_name = "cas-conveyor-dest";
    conveyor_0.color = math::Vector4<u8>(0, 0, 0, 0x80);
    conveyor_0.override_size = math::Vector2<int>(64, 32);
    auto& c0 = conveyor_0.layers.emplace_back();
    c0.set_blend_b2_d1();
    c0.set_no_z_write_no_z_test();
    // :test (new 'static 'gs-test :ate #x1 :afail #x3 :zte #x1 :ztst (gs-ztest always))
    c0.channel_masks[3] = false;  // no alpha writes.
    c0.end_time = 300.;
    c0.tex_name = "cas-conveyor";

    FixedAnimDef conveyor_1;
    conveyor_1.tex_name = "cas-conveyor-dest-01";
    conveyor_1.color = math::Vector4<u8>(0, 0, 0, 0x80);
    conveyor_1.override_size = math::Vector2<int>(64, 32);
    auto& c1 = conveyor_1.layers.emplace_back();
    c1.set_blend_b2_d1();
    c1.set_no_z_write_no_z_test();
    // :test (new 'static 'gs-test :ate #x1 :afail #x3 :zte #x1 :ztst (gs-ztest always))
    c1.channel_masks[3] = false;  // no alpha writes.
    c1.end_time = 300.;
    c1.tex_name = "cas-conveyor";

    FixedAnimDef conveyor_2;
    conveyor_2.tex_name = "cas-conveyor-dest-02";
    conveyor_2.color = math::Vector4<u8>(0, 0, 0, 0x80);
    conveyor_2.override_size = math::Vector2<int>(64, 32);
    auto& c2 = conveyor_2.layers.emplace_back();
    c2.set_blend_b2_d1();
    c2.set_no_z_write_no_z_test();
    // :test (new 'static 'gs-test :ate #x1 :afail #x3 :zte #x1 :ztst (gs-ztest always))
    c2.channel_masks[3] = false;  // no alpha writes.
    c2.end_time = 300.;
    c2.tex_name = "cas-conveyor";

    FixedAnimDef conveyor_3;
    conveyor_3.tex_name = "cas-conveyor-dest-03";
    conveyor_3.color = math::Vector4<u8>(0, 0, 0, 0x80);
    conveyor_3.override_size = math::Vector2<int>(64, 32);
    auto& c3 = conveyor_3.layers.emplace_back();
    c3.set_blend_b2_d1();
    c3.set_no_z_write_no_z_test();
    // :test (new 'static 'gs-test :ate #x1 :afail #x3 :zte #x1 :ztst (gs-ztest always))
    c3.channel_masks[3] = false;  // no alpha writes.
    c3.end_time = 300.;
    c3.tex_name = "cas-conveyor";

    m_cas_conveyor_anim_array_idx =
        create_fixed_anim_array({conveyor_0, conveyor_1, conveyor_2, conveyor_3});
  }

  // SECURITY
  {
    FixedAnimDef env;
    env.color = math::Vector4<u8>(0, 0, 0, 0x80);
    env.tex_name = "security-env-dest";
    for (int i = 0; i < 2; i++) {
      auto& env1 = env.layers.emplace_back();
      env1.tex_name = "security-env-uscroll";
      //    :test (new 'static 'gs-test :ate #x1 :afail #x3 :zte #x1 :ztst (gs-ztest always))
      env1.set_no_z_write_no_z_test();
      env1.channel_masks[3] = false;  // no alpha writes.
      //    :alpha (new 'static 'gs-alpha :b #x2 :d #x1)
      env1.set_blend_b2_d1();
      env1.end_time = 4800.f;
    }

    FixedAnimDef dot;
    dot.color = math::Vector4<u8>(0, 0, 0, 0x80);
    dot.tex_name = "security-dot-dest";

    auto& cwhite = dot.layers.emplace_back();
    cwhite.set_blend_b2_d1();
    cwhite.set_no_z_write_no_z_test();
    cwhite.tex_name = "common-white";
    cwhite.end_time = 4800.f;

    for (int i = 0; i < 2; i++) {
      auto& dsrc = dot.layers.emplace_back();
      dsrc.set_blend_b2_d1();
      dsrc.set_no_z_write_no_z_test();
      dsrc.end_time = 600.f;
      dsrc.tex_name = "security-dot-src";
    }

    m_security_anim_array_idx = create_fixed_anim_array({env, dot});
  }

  // WATERFALL
  {
    FixedAnimDef waterfall;
    waterfall.color = math::Vector4<u8>(0, 0, 0, 0x80);
    waterfall.tex_name = "waterfall-dest";
    for (int i = 0; i < 4; i++) {
      auto& src = waterfall.layers.emplace_back();
      src.set_blend_b1_d1();
      src.set_no_z_write_no_z_test();
      src.end_time = 450.f;
      src.tex_name = "waterfall";
    }
    m_waterfall_anim_array_idx = create_fixed_anim_array({waterfall});
  }

  {
    FixedAnimDef waterfall;
    waterfall.color = math::Vector4<u8>(0, 0, 0, 0x80);
    waterfall.tex_name = "waterfall-dest";
    for (int i = 0; i < 4; i++) {
      auto& src = waterfall.layers.emplace_back();
      src.set_blend_b1_d1();
      src.set_no_z_write_no_z_test();
      src.end_time = 450.f;
      src.tex_name = "waterfall";
    }
    m_waterfall_b_anim_array_idx = create_fixed_anim_array({waterfall});
  }

  // LAVA
  {
    FixedAnimDef lava;
    lava.color = math::Vector4<u8>(0, 0, 0, 0x80);
    lava.tex_name = "dig-lava-01-dest";
    for (int i = 0; i < 2; i++) {
      auto& src = lava.layers.emplace_back();
      src.set_blend_b1_d1();
      src.set_no_z_write_no_z_test();
      src.end_time = 3600.f;
      src.tex_name = "dig-lava-01";
    }
    m_lava_anim_array_idx = create_fixed_anim_array({lava});
  }

  {
    FixedAnimDef lava;
    lava.color = math::Vector4<u8>(0, 0, 0, 0x80);
    lava.tex_name = "dig-lava-01-dest";
    for (int i = 0; i < 2; i++) {
      auto& src = lava.layers.emplace_back();
      src.set_blend_b1_d1();
      src.set_no_z_write_no_z_test();
      src.end_time = 3600.f;
      src.tex_name = "dig-lava-01";
    }
    m_lava_b_anim_array_idx = create_fixed_anim_array({lava});
  }

  // Stadiumb
  {
    FixedAnimDef def;
    def.color = math::Vector4<u8>(0, 0, 0, 0x80);
    def.tex_name = "stdmb-energy-wall-01-dest";
    for (int i = 0; i < 2; i++) {
      auto& src = def.layers.emplace_back();
      src.set_blend_b1_d1();
      src.set_no_z_write_no_z_test();
      src.end_time = 300.f;
      src.tex_name = "stdmb-energy-wall-01";
    }
    m_stadiumb_anim_array_idx = create_fixed_anim_array({def});
  }

  // Fortress pris
  {
    FixedAnimDef l_tread;
    l_tread.color = math::Vector4<u8>(0, 0, 0, 0x80);
    l_tread.tex_name = "robotank-tread-l-dest";
    auto& l_src = l_tread.layers.emplace_back();
    l_src.set_blend_b1_d1();
    l_src.set_no_z_write_no_z_test();
    l_src.channel_masks[3] = false;  // no alpha writes.
    l_src.end_time = 1.f;
    l_src.tex_name = "robotank-tread";

    FixedAnimDef r_tread;
    r_tread.color = math::Vector4<u8>(0, 0, 0, 0x80);
    r_tread.tex_name = "robotank-tread-r-dest";
    auto& r_src = r_tread.layers.emplace_back();
    r_src.set_blend_b1_d1();
    r_src.set_no_z_write_no_z_test();
    r_src.channel_masks[3] = false;  // no alpha writes.
    r_src.end_time = 1.f;
    r_src.tex_name = "robotank-tread";

    m_fortress_pris_anim_array_idx = create_fixed_anim_array({l_tread, r_tread});
  }

  // Fortress Warp
  {
    FixedAnimDef def;
    def.color = math::Vector4<u8>(0, 0, 0, 0x80);
    def.move_to_pool = true;
    def.tex_name = "fort-roboscreen-dest";
    auto& src = def.layers.emplace_back();
    src.set_blend_b2_d1();
    src.channel_masks[3] = false;  // no alpha writes.
    src.set_no_z_write_no_z_test();
    src.end_time = 300.f;
    src.tex_name = "fort-roboscreen-env";
    m_fortress_warp_anim_array_idx = create_fixed_anim_array({def});
  }

  // metkor
  {
    FixedAnimDef def;
    def.color = math::Vector4<u8>(0, 0, 0, 0x80);
    def.tex_name = "squid-env-rim-dest";
    def.move_to_pool = true;
    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "metkor-head-env-noise";
    }
    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "metkor-head-env-scan";
    }
    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "metkor-head-env-rim";
    }
    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "metkor-head-env-rim";
    }
    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "environment-phong-rim";
    }
    m_metkor_anim_array_idx = create_fixed_anim_array({def});
  }

  // shield
  {
    FixedAnimDef def;
    def.color = math::Vector4<u8>(0, 0, 0, 0x80);
    def.tex_name = "squid-env-rim-dest";
    def.move_to_pool = true;

    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "common-white";
    }

    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "squid-env-uscroll";
    }

    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "squid-env-uscroll";
    }

    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "squid-env-rim-src";
    }

    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "squid-env-rim-src";
    }
    m_shield_anim_array_idx = create_fixed_anim_array({def});
  }

  // krew
  {
    FixedAnimDef def;
    def.color = math::Vector4<u8>(0, 0, 0, 0x80);
    def.tex_name = "krew-holo-dest";
    def.move_to_pool = true;
    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "metkor-head-env-noise";
    }
    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "metkor-head-env-scan";
    }
    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "metkor-head-env-rim";
    }
    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "metkor-head-env-rim";
    }
    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "metkor-phong-env";
    }
    m_krew_holo_anim_array_idx = create_fixed_anim_array({def});
  }
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
  fmt::print("saving texture with size {} x {}\n", w, h);
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

  {
    m_slime_output_slot = output_slot_by_idx(GameVersion::Jak2, "cas-toxic-slime-dest");
    m_slime_scroll_output_slot =
        output_slot_by_idx(GameVersion::Jak2, "cas-toxic-slime-scroll-dest");
    const float max_times[4] = {600.f, 300.f, 150.f, 75.f};
    const float scales[4] = {0.55, 0.6, 0.3, 0.1f};
    for (int i = 0, dim = kFinalSlimeTextureSize >> (kNumSlimeNoiseLayers - 1);
         i < kNumSlimeNoiseLayers; i++, dim *= 2) {
      auto& tex = m_slime_noise_textures[i];
      tex.temp_data.resize(dim * dim);
      tex.max_time = max_times[i];
      tex.scale = scales[i];
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
