#include "TextureAnimator.h"

#include "common/global_profiler/GlobalProfiler.h"
#include "common/texture/texture_slots.h"
#include "common/util/FileUtil.h"
#include "common/util/Timer.h"

#include "game/graphics/texture/TexturePool.h"

//#define dprintf(...) printf(__VA_ARGS__)
//#define dfmt(...) fmt::print(__VA_ARGS__)
#define dprintf(...)
#define dfmt(...)

// So there's a lot of stupid-looking OpenGL stuff going on here.
// The motivation for this is to avoid an issue where some operations take about 5-10ms.
// As far as I can tell, this slow operation is actually the driver forcing this thread to sync
// with some internal stuff. It seems to be triggered on:
// - deleting a texture that's in use
// - glTexImage2D to modify a texture with a different sized texture.

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
  // list of sizes to preallocate.
  for (const auto& a : std::vector<Alloc>{{16, 16, 5},  //
                                          {32, 16, 1},
                                          {32, 32, 5},
                                          {32, 64, 1},
                                          {64, 64, 8},
                                          {64, 128, 4},
                                          {128, 128, 5},
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

GLuint OpenGLTexturePool::allocate(u64 w, u64 h) {
  const auto& it = textures.find((w << 32) | h);
  if (it == textures.end()) {
    lg::die("OpenGLTexturePool needs entries for {} x {}", w, h);
  }

  if (it->second.empty()) {
    lg::die("OpenGLTexturePool needs more entries for {} x {}", w, h);
  }

  auto ret = it->second.back();
  it->second.pop_back();
  return ret;
}

void OpenGLTexturePool::free(GLuint texture, u64 w, u64 h) {
  textures[(w << 32) | h].push_back(texture);
}

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

ClutBlender::ClutBlender(const std::string& dest,
                         const std::vector<std::string>& sources,
                         const std::optional<std::string>& level_name,
                         const tfrag3::Level* level,
                         OpenGLTexturePool* tpool) {
  m_dest = itex_by_name(level, dest, level_name);
  for (const auto& sname : sources) {
    m_cluts.push_back(&itex_by_name(level, sname, level_name)->color_table);
    m_current_weights.push_back(0);
  }
  m_texture = tpool->allocate(m_dest->w, m_dest->h);
  m_temp_rgba.resize(m_dest->w * m_dest->h);

  std::vector<float> init_weights(m_current_weights.size(), 0);
  init_weights.at(0) = 1.f;
  run(init_weights.data());
}

GLuint ClutBlender::run(const float* weights) {
  bool needs_run = false;

  for (size_t i = 0; i < m_current_weights.size(); i++) {
    if (weights[i] != m_current_weights[i]) {
      needs_run = true;
      break;
    }
  }

  if (!needs_run) {
    return m_texture;
  }

  for (size_t i = 0; i < m_current_weights.size(); i++) {
    m_current_weights[i] = weights[i];
  }

  for (int i = 0; i < 256; i++) {
    math::Vector4f v = math::Vector4f::zero();
    for (size_t j = 0; j < m_current_weights.size(); j++) {
      v += (*m_cluts[j])[i].cast<float>() * m_current_weights[j];
    }
    m_temp_clut[i] = v.cast<u8>();
  }

  for (int i = 0; i < m_temp_rgba.size(); i++) {
    memcpy(&m_temp_rgba[i], m_temp_clut[m_dest->index_data[i]].data(), 4);
  }

  glBindTexture(GL_TEXTURE_2D, m_texture);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, m_dest->w, m_dest->h, 0, GL_RGBA,
               GL_UNSIGNED_INT_8_8_8_8_REV, m_temp_rgba.data());
  glGenerateMipmap(GL_TEXTURE_2D);
  float aniso = 0.0f;
  glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY, &aniso);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY, aniso);
  glBindTexture(GL_TEXTURE_2D, 0);

  return m_texture;
}

TextureAnimator::TextureAnimator(ShaderLibrary& shaders, const tfrag3::Level* common_level)
    : m_common_level(common_level) {
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

  // create a single "dummy texture" with all 0 data.
  // this is faster and easier than switching shaders to one without texturing, and is used
  // only rarely
  glGenTextures(1, &m_dummy_texture);
  glBindTexture(GL_TEXTURE_2D, m_dummy_texture);
  std::vector<u8> data(16 * 16 * 4);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 16, 16, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
               data.data());
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

  m_output_slots.resize(jak2_animated_texture_slots().size(), m_dummy_texture);

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
}

int TextureAnimator::create_clut_blender_group(const std::vector<std::string>& textures,
                                               const std::string& suffix0,
                                               const std::string& suffix1,
                                               const std::optional<std::string>& dgo) {
  int ret = m_clut_blender_groups.size();
  m_clut_blender_groups.emplace_back();
  add_to_clut_blender_group(ret, textures, suffix0, suffix1, dgo);
  return ret;
}

void TextureAnimator::add_to_clut_blender_group(int idx,
                                                const std::vector<std::string>& textures,
                                                const std::string& suffix0,
                                                const std::string& suffix1,
                                                const std::optional<std::string>& dgo) {
  auto& grp = m_clut_blender_groups.at(idx);
  for (auto& prefix : textures) {
    grp.blenders.emplace_back(prefix, std::vector<std::string>{prefix + suffix0, prefix + suffix1},
                              dgo, m_common_level, &m_opengl_texture_pool);
    grp.outputs.push_back(output_slot_by_idx(GameVersion::Jak2, prefix));
    m_output_slots.at(grp.outputs.back()) = grp.blenders.back().texture();
  }
}

TextureAnimator::~TextureAnimator() {
  glDeleteVertexArrays(1, &m_vao);
  glDeleteBuffers(1, &m_vertex_buffer);
  glDeleteTextures(1, &m_dummy_texture);
}

GLuint TextureAnimator::get_by_slot(int idx) {
  ASSERT(idx >= 0 && idx < (int)m_output_slots.size());
  return m_output_slots[idx];
}

// IDs sent from GOAL telling us what texture operation to perform.
enum PcTextureAnimCodes {
  FINISH_ARRAY = 13,
  ERASE_DEST_TEXTURE = 14,
  UPLOAD_CLUT_16_16 = 15,
  GENERIC_UPLOAD = 16,
  SET_SHADER = 17,
  DRAW = 18,
  MOVE_RG_TO_BA = 19,
  SET_CLUT_ALPHA = 20,
  COPY_CLUT_ALPHA = 21,
  DARKJAK = 22,
  PRISON_JAK = 23,
  ORACLE_JAK = 24,
  NEST_JAK = 25,
  KOR_TRANSFORM = 26
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
  u8 pad[3];
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
                                               TexturePool* texture_pool) {
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
  m_erased_on_this_frame.clear();

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
        case MOVE_RG_TO_BA: {
          auto p = scoped_prof("rg-to-ba");
          handle_rg_to_ba(tf);
        } break;
        case SET_CLUT_ALPHA: {
          auto p = scoped_prof("set-clut-alpha");
          handle_set_clut_alpha(tf);
        } break;
        case COPY_CLUT_ALPHA: {
          auto p = scoped_prof("copy-clut-alpha");
          handle_copy_clut_alpha(tf);
        } break;
        case DARKJAK: {
          auto p = scoped_prof("darkjak");
          run_clut_blender_group(tf, m_darkjak_clut_blender_idx);
        } break;
        case PRISON_JAK: {
          auto p = scoped_prof("prisonjak");
          run_clut_blender_group(tf, m_jakb_prison_clut_blender_idx);
        } break;
        case ORACLE_JAK: {
          auto p = scoped_prof("oraclejak");
          run_clut_blender_group(tf, m_jakb_oracle_clut_blender_idx);
        } break;
        case NEST_JAK: {
          auto p = scoped_prof("nestjak");
          run_clut_blender_group(tf, m_jakb_nest_clut_blender_idx);
        } break;
        case KOR_TRANSFORM: {
          auto p = scoped_prof("kor");
          run_clut_blender_group(tf, m_kor_transform_clut_blender_idx);
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
  // we need to make sure that all final textures end up on the GPU. For now, we detect this by
  // seeing if the "erase" operation ran on an tbp, indicating that it was cleared, which is
  // always done to all textures by the GOAL code.
  for (auto tbp : m_erased_on_this_frame) {
    auto p = scoped_prof("handle-one-erased");
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
        in.id = get_id_for_tbp(texture_pool, tbp);
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
  }
}

/*!
 * Get a pool texture ID for this texture. For now, there's just a unique ID per TBP.
 * The only purpose is to avoid putting all the textures with the same ID, which is a slow-path
 * in the pool (which is optimized for only a few textures with the same ID at most).
 */
PcTextureId TextureAnimator::get_id_for_tbp(TexturePool* pool, u32 tbp) {
  const auto& it = m_ids_by_vram.find(tbp);
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

/*!
 * Copy rg channels to ba from src to dst.
 * The PS2 implementation is confusing, and this is just a guess at how it works.
 */
void TextureAnimator::handle_rg_to_ba(const DmaTransfer& tf) {
  dprintf("[tex anim] rg -> ba\n");
  ASSERT(tf.size_bytes == sizeof(TextureAnimPcTransform));
  auto* data = (const TextureAnimPcTransform*)(tf.data);
  dprintf("  src: %d, dest: %d\n", data->src_tbp, data->dst_tbp);
  const auto& src = m_textures.find(data->src_tbp);
  const auto& dst = m_textures.find(data->dst_tbp);
  if (src != m_textures.end() && dst != m_textures.end()) {
    ASSERT(src->second.kind == VramEntry::Kind::GPU);
    ASSERT(dst->second.kind == VramEntry::Kind::GPU);
    ASSERT(src->second.tex.value().texture() != dst->second.tex.value().texture());
    FramebufferTexturePairContext ctxt(dst->second.tex.value());
    float positions[3 * 4] = {0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0};
    float uvs[2 * 4] = {0, 0, 1, 0, 1, 1, 0, 1};
    glUniform3fv(m_uniforms.positions, 4, positions);
    glUniform2fv(m_uniforms.uvs, 4, uvs);
    glUniform1i(m_uniforms.enable_tex, 1);
    glUniform4f(m_uniforms.rgba, 256, 256, 256, 128);  // TODO - seems wrong.
    glUniform4i(m_uniforms.channel_scramble, 0, 1, 0, 1);
    glBindTexture(GL_TEXTURE_2D, src->second.tex.value().texture());
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glDisable(GL_BLEND);
    glDisable(GL_DEPTH_TEST);
    glColorMask(true, true, true, true);
    glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

  } else {
    ASSERT_NOT_REACHED();
  }

  //  const auto& vram_src = m_vram_entries.find(data->src_tbp);
  //  if (vram_src != m_vram_entries.end()) {
  //    ASSERT(vram_src->second.kind == VramEntry::Kind::GENERIC_PSM32);
  //    // no idea if this is right, but lets try.
  //    int w = vram_src->second.width;
  //    int h = vram_src->second.height;
  //    u8* tdata = vram_src->second.data_psm32.data();
  //
  //    // file_util::write_rgba_png("./before_transform.png", tdata, w, h);
  //
  //    for (int i = 0; i < w * h; i++) {
  //      tdata[i * 4 + 2] = tdata[i * 4];
  //      tdata[i * 4 + 3] = tdata[i * 4 + 1];
  //    }
  //
  //    // file_util::write_rgba_png("./after_transform.png", tdata, w, h);
  //
  //  } else {
  //    ASSERT_NOT_REACHED();
  //  }
}

void TextureAnimator::handle_set_clut_alpha(const DmaTransfer& tf) {
  ASSERT_NOT_REACHED();
  dprintf("[tex anim] set clut alpha\n");
  ASSERT(tf.size_bytes == sizeof(TextureAnimPcTransform));
  auto* data = (const TextureAnimPcTransform*)(tf.data);
  dprintf("  src: %d, dest: %d\n", data->src_tbp, data->dst_tbp);
  const auto& tex = m_textures.find(data->dst_tbp);
  ASSERT(tex != m_textures.end());

  ASSERT(tex->second.kind == VramEntry::Kind::GPU);
  FramebufferTexturePairContext ctxt(tex->second.tex.value());
  float positions[3 * 4] = {0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0};
  float uvs[2 * 4] = {0, 0, 1, 0, 1, 1, 0, 1};
  glUniform3fv(m_uniforms.positions, 4, positions);
  glUniform2fv(m_uniforms.uvs, 4, uvs);
  glUniform1i(m_uniforms.enable_tex, 0);  // NO TEXTURE!
  glUniform4f(m_uniforms.rgba, 128, 128, 128, 128);
  glUniform4i(m_uniforms.channel_scramble, 0, 1, 2, 3);
  glBindTexture(GL_TEXTURE_2D, m_dummy_texture);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glDisable(GL_BLEND);
  glDisable(GL_DEPTH_TEST);
  glColorMask(false, false, false, true);
  glDrawArrays(GL_TRIANGLE_FAN, 0, 4);
  glColorMask(true, true, true, true);
}

void TextureAnimator::handle_copy_clut_alpha(const DmaTransfer& tf) {
  ASSERT_NOT_REACHED();
  dprintf("[tex anim] __copy__ clut alpha\n");
  ASSERT(tf.size_bytes == sizeof(TextureAnimPcTransform));
  auto* data = (const TextureAnimPcTransform*)(tf.data);
  dprintf("  src: %d, dest: %d\n", data->src_tbp, data->dst_tbp);
  const auto& dst_tex = m_textures.find(data->dst_tbp);
  const auto& src_tex = m_textures.find(data->src_tbp);
  ASSERT(dst_tex != m_textures.end());
  if (src_tex == m_textures.end()) {
    lg::error("Skipping copy clut alpha because source texture at {} wasn't found", data->src_tbp);
    return;
  }
  ASSERT(src_tex != m_textures.end());

  ASSERT(dst_tex->second.kind == VramEntry::Kind::GPU);
  ASSERT(src_tex->second.kind == VramEntry::Kind::GPU);

  FramebufferTexturePairContext ctxt(dst_tex->second.tex.value());
  float positions[3 * 4] = {0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0};
  float uvs[2 * 4] = {0, 0, 1, 0, 1, 1, 0, 1};
  glUniform3fv(m_uniforms.positions, 4, positions);
  glUniform2fv(m_uniforms.uvs, 4, uvs);
  glUniform1i(m_uniforms.enable_tex, 1);
  glUniform4f(m_uniforms.rgba, 128, 128, 128, 128);  // TODO - seems wrong.
  glUniform4i(m_uniforms.channel_scramble, 0, 1, 2, 3);
  glBindTexture(GL_TEXTURE_2D, src_tex->second.tex.value().texture());
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glDisable(GL_BLEND);
  glDisable(GL_DEPTH_TEST);
  glColorMask(false, false, false, true);
  glDrawArrays(GL_TRIANGLE_FAN, 0, 4);
  glColorMask(true, true, true, true);
}

void TextureAnimator::run_clut_blender_group(DmaTransfer& tf, int idx) {
  float f;
  ASSERT(tf.size_bytes == 16);
  memcpy(&f, tf.data, sizeof(float));
  float weights[2] = {1.f - f, f};
  auto& blender = m_clut_blender_groups.at(idx);
  for (size_t i = 0; i < blender.blenders.size(); i++) {
    m_output_slots[blender.outputs[i]] = blender.blenders[i].run(weights);
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
      m_tex_looking_for_clut = nullptr;
      break;
    case (int)GsTex0::PSM::PSMT8:
      vram.kind = VramEntry::Kind::GENERIC_PSMT8;
      vram.data.resize(upload->width * upload->height);
      vram.tex_width = upload->width;
      vram.tex_height = upload->height;
      memcpy(vram.data.data(), ee_mem + upload->data, vram.data.size());
      m_tex_looking_for_clut = &vram;
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
  dprintf("[tex anim] erase destination texture\n");
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
    {
      auto p = scoped_prof("erase-draw");
      glDrawArrays(GL_TRIANGLE_FAN, 0, 4);
    }
  }

  // set as active
  m_current_dest_tbp = entry->dest_texture_address;
  m_erased_on_this_frame.insert(entry->dest_texture_address);
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
    set_up_opengl_for_shader(m_current_shader, gpu_texture, true);  // ABE forced on here.

    // set up uniform buffers for the coordinates for this draw.
    set_uniforms_from_draw_data(draw_data, dest_te.tex_width, dest_te.tex_height);

    ASSERT(dest_te.tex);
    // draw!
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
    // debug_save_opengl_texture("opengl_draw_result.png", dest_te.tex->texture());
    // debug_save_opengl_texture("opengl_test.png", gpu_texture);
  } else {
    ASSERT_NOT_REACHED();
  }
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
          int w = 1 << m_current_shader.tex0.tw();
          int h = 1 << m_current_shader.tex0.th();
          ASSERT(w == vram_entry->tex_width * 2);
          ASSERT(h == vram_entry->tex_height * 2);

          Timer timer;
          m_converter.upload_width(vram_entry->data.data(), m_current_shader.tex0.tbp0(),
                                   vram_entry->tex_width, vram_entry->tex_height);

          // also needs clut lookup
          load_clut_to_converter();
          {
            std::vector<u32> rgba_data(w * h);
            m_converter.download_rgba8888(
                (u8*)rgba_data.data(), m_current_shader.tex0.tbp0(), m_current_shader.tex0.tbw(), w,
                h, (int)m_current_shader.tex0.psm(), (int)m_current_shader.tex0.cpsm(),
                m_current_shader.tex0.cbp(), rgba_data.size() * 4);
            //              file_util::write_rgba_png("out.png", rgba_data.data(), 1 <<
            //              m_current_shader.tex0.tw(),
            //                                        1 << m_current_shader.tex0.th());
            dprintf("processing %d x %d took %.3f ms\n", w, h, timer.getMs());
            return make_temp_gpu_texture(rgba_data.data(), w, h);
          }

          ASSERT_NOT_REACHED();
        } break;
        default:
          fmt::print("unhandled source texture format {}\n", (int)m_current_shader.tex0.psm());
          ASSERT_NOT_REACHED();
      }
      break;
    case VramEntry::Kind::CLUT16_16_IN_PSM32:
      ASSERT_NOT_REACHED();

      /*
    case VramEntry::Kind::GENERIC_PSMT8: {
      fmt::print("drawing: {}\n", (int)m_current_shader.tex0.psm());
      ASSERT(m_current_shader.tex0.psm() == GsTex0::PSM::PSMT8);
      ASSERT(m_current_shader.tex0.cpsm() == 0);  // psm32.
      int tw = 1 << m_current_shader.tex0.tw();
      int th = 1 << m_current_shader.tex0.th();
      ASSERT(tw == vram_entry->tex_width);
      ASSERT(th == vram_entry->tex_height);
      std::vector<u32> rgba_data(tw * th);
      const u32* clut = get_current_clut_16_16_psm32();
      for (int r = 0; r < th; r++) {
        for (int c = 0; c < tw; c++) {
          rgba_data[c + r * tw] = clut[vram_entry->data[c + r * tw]];
        }
      }
      return make_temp_gpu_texture(rgba_data.data(), tw, th);
    }
       */

      break;
    default:
      ASSERT_NOT_REACHED();
  }
}

void TextureAnimator::set_up_opengl_for_shader(const ShaderContext& shader,
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

  bool do_alpha_test = false;
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

  if (alpha_test_mask_alpha_trick) {
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
