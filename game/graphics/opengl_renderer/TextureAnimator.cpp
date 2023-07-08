#include "TextureAnimator.h"

#include "common/global_profiler/GlobalProfiler.h"
#include "common/util/FileUtil.h"
#include "common/util/Timer.h"

#include "game/graphics/texture/TexturePool.h"

#define dprintf(...) printf(__VA_ARGS__)
#define dfmt(...) fmt::print(__VA_ARGS__)

TextureAnimator::TextureAnimator(ShaderLibrary& shaders) {
  glGenVertexArrays(1, &m_vao);
  glGenBuffers(1, &m_vertex_buffer);
  glBindVertexArray(m_vao);
  std::array<Vertex, 4> vertices = {Vertex{0, 0, 0, 0}, Vertex{1, 0, 0, 0}, Vertex{2, 0, 0, 0},
                                    Vertex{3, 0, 0, 0}};
  glBindBuffer(GL_ARRAY_BUFFER, m_vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(Vertex) * 4, vertices.data(), GL_STATIC_DRAW);

  glEnableVertexAttribArray(0);
  glVertexAttribIPointer(0,               // location 0 in the shader
                         1,               // 1 per vertex
                         GL_INT,          // floats
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

  {
    glGenTextures(1, &m_dummy_texture);
    glBindTexture(GL_TEXTURE_2D, m_dummy_texture);
    std::vector<u8> data(16 * 16 * 4);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 16, 16, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
                 data.data());
    glBindTexture(GL_TEXTURE_2D, 0);

    m_gl_textures.resize(32);
    glGenTextures(m_gl_textures.size(), m_gl_textures.data());
  }
}

TextureAnimator::~TextureAnimator() {
  glDeleteVertexArrays(1, &m_vao);
  glDeleteBuffers(1, &m_vertex_buffer);
}

enum PcTextureAnimCodes {
  FINISH_ARRAY = 13,
  ERASE_DEST_TEXTURE = 14,
  UPLOAD_CLUT_16_16 = 15,
  GENERIC_UPLOAD = 16,
  SET_SHADER = 17,
  DRAW = 18,
  MOVE_RG_TO_BA = 19,
};

struct TextureAnimPcUpload {
  u32 data;
  u16 width;
  u16 height;
  u32 dest;
  u8 format;
  u8 pad[3];
};
static_assert(sizeof(TextureAnimPcUpload) == 16);

struct TextureAnimPcTransform {
  u32 src_tbp;
  u32 dst_tbp;
  u32 pad0;
  u32 pad1;
};

void TextureAnimator::handle_texture_anim_data(DmaFollower& dma,
                                               const u8* ee_mem,
                                               TexturePool* texture_pool) {
  dprintf("animator\n");
  m_current_shader = {};
  glBindVertexArray(m_vao);
  glBindBuffer(GL_ARRAY_BUFFER, m_vertex_buffer);
  glUseProgram(m_shader_id);
  glDepthMask(GL_FALSE);
  m_next_gl_texture = 0;  // reset temp texture allocator.

  bool done = false;
  while (!done) {
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
          handle_draw(dma);
        } break;
        case FINISH_ARRAY:
          done = true;
          break;
        case MOVE_RG_TO_BA: {
          auto p = scoped_prof("rg-to-ba");
          handle_rg_to_ba(tf);
        } break;
        default:
          fmt::print("bad imm: {}\n", vif0.immediate);
          ASSERT_NOT_REACHED();
      }
    } else {
      dprintf("[tex anim] unhandled VIF in main loop\n");
      fmt::print("{} {}\n", vif0.print(), tf.vifcode1().print());
      ASSERT_NOT_REACHED();
    }
  }

  // TODO: do something with the result textures.

  for (auto& [tbp, entry] : m_dest_textures) {
    if (entry.needs_pool_creation) {
      TextureInput in;
      in.gpu_texture = entry.tex.value().texture();
      in.w = entry.tex_width;
      in.h = entry.tex_height;
      in.debug_page_name = "PC-ANIM";
      in.debug_name = std::to_string(tbp);
      in.id = texture_pool->allocate_pc_port_texture(GameVersion::Jak2);
      entry.pool_gpu_tex = texture_pool->give_texture_and_load_to_vram(in, tbp);
      entry.needs_pool_creation = false;
      dprintf("create texture %d\n", tbp);
    } else if (entry.needs_pool_update) {
      texture_pool->update_gl_texture(entry.pool_gpu_tex, entry.tex_width, entry.tex_height,
                                      entry.tex.value().texture());
      texture_pool->move_existing_to_vram(entry.pool_gpu_tex, tbp);
      entry.needs_pool_update = false;
      dprintf("update texture %d\n", tbp);
    } else {
      texture_pool->move_existing_to_vram(entry.pool_gpu_tex, tbp);
      dprintf("no change %d\n", tbp);
    }
  }

  glDepthMask(GL_TRUE);
  glEnable(GL_DEPTH_TEST);
  glColorMask(true, true, true, true);
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

void TextureAnimator::handle_rg_to_ba(const DmaTransfer& tf) {
  dprintf("[tex anim] rg -> ba\n");
  ASSERT(tf.size_bytes == sizeof(TextureAnimPcTransform));
  auto* data = (const TextureAnimPcTransform*)(tf.data);
  dprintf("  src: %d, dest: %d\n", data->src_tbp, data->dst_tbp);
  const auto& src = m_dest_textures.find(data->src_tbp);
  const auto& dst = m_dest_textures.find(data->dst_tbp);
  if (src != m_dest_textures.end() && dst != m_dest_textures.end()) {
    {
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
    }

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

void TextureAnimator::handle_upload_clut_16_16(const DmaTransfer& tf, const u8* ee_mem) {
  dprintf("[tex anim] upload clut 16 16\n");
  ASSERT(tf.size_bytes == sizeof(TextureAnimPcUpload));
  auto* upload = (const TextureAnimPcUpload*)(tf.data);
  ASSERT(upload->width == 16);
  ASSERT(upload->height == 16);
  dprintf("  dest is 0x%x\n", upload->dest);
  auto& vram = m_vram_entries[upload->dest];
  vram.kind = VramEntry::Kind::CLUT16_16_IN_PSM32;
  vram.data.resize(16 * 16 * 4);
  vram.width = upload->width;
  vram.height = upload->height;
  memcpy(vram.data.data(), ee_mem + upload->data, vram.data.size());
}

void TextureAnimator::handle_generic_upload(const DmaTransfer& tf, const u8* ee_mem) {
  dprintf("[tex anim] upload generic @ 0x%lx\n", tf.data - ee_mem);
  ASSERT(tf.size_bytes == sizeof(TextureAnimPcUpload));
  auto* upload = (const TextureAnimPcUpload*)(tf.data);
  dprintf(" size %d x %d\n", upload->width, upload->height);
  dprintf(" dest is 0x%x\n", upload->dest);
  auto& vram = m_vram_entries[upload->dest];

  switch (upload->format) {
    case (int)GsTex0::PSM::PSMCT32:
      vram.kind = VramEntry::Kind::GENERIC_PSM32;
      vram.data.resize(upload->width * upload->height * 4);
      vram.width = upload->width;
      vram.height = upload->height;
      memcpy(vram.data.data(), ee_mem + upload->data, vram.data.size());
      break;
    case (int)GsTex0::PSM::PSMT8:
      vram.kind = VramEntry::Kind::GENERIC_PSMT8;
      vram.data.resize(upload->width * upload->height);
      vram.width = upload->width;
      vram.height = upload->height;
      memcpy(vram.data.data(), ee_mem + upload->data, vram.data.size());
      break;
    default:
      fmt::print("Unhandled format: {}\n", upload->format);
      ASSERT_NOT_REACHED();
  }
}

void TextureAnimator::handle_erase_dest(DmaFollower& dma) {
  printf("[tex anim] erase destination texture\n");
  // auto& out = m_new_dest_textures.emplace_back();
  DestinationTextureEntry* entry = nullptr;

  // first transfer will be a bunch of ad (modifies the shader)
  {
    auto ad_transfer = dma.read_and_advance();
    ASSERT(ad_transfer.size_bytes == 10 * 16);
    ASSERT(ad_transfer.vifcode0().kind == VifCode::Kind::FLUSHA);
    ASSERT(ad_transfer.vifcode1().kind == VifCode::Kind::DIRECT);
    const u64* ad_data = (const u64*)(ad_transfer.data + 16);

    for (int i = 0; i < 9; i++) {
      dprintf(" ad: 0x%lx 0x%lx\n", ad_data[i * 2], ad_data[i * 2 + 1]);
    }
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

    // see if we already have an OpenGL texture:
    const auto& existing_dest = m_dest_textures.find(dest_texture_address);
    bool existing_opengl = existing_dest != m_dest_textures.end();
    bool can_reuse = true;
    if (existing_opengl) {
      if (existing_dest->second.tex_height != tex_height ||
          existing_dest->second.tex_width != tex_width) {
        dprintf(" can't reuse, size mismatch\n");
        can_reuse = false;
      }
    } else {
      dprintf(" can't reuse, first time using this address\n");
      can_reuse = false;
    }

    if (can_reuse) {
      entry = &existing_dest->second;
    } else {
      if (existing_opengl) {
        existing_dest->second.tex.reset();
        entry = &existing_dest->second;
        entry->needs_pool_update = true;
      } else {
        entry = &m_dest_textures[dest_texture_address];
        entry->needs_pool_creation = true;
      }
      entry->tex.emplace(tex_width, tex_height, GL_UNSIGNED_INT_8_8_8_8_REV);
    }

    entry->tex_width = tex_width;
    entry->tex_height = tex_height;
    entry->dest_texture_address = dest_texture_address;
  }

  // next transfer is the erase. This is done with alpha blending off
  {
    auto clear_transfer = dma.read_and_advance();
    ASSERT(clear_transfer.size_bytes == 16 * 4);
    math::Vector<u32, 4> rgba_u32;
    memcpy(rgba_u32.data(), clear_transfer.data + 16, 16);
    entry->rgba_clear = rgba_u32.cast<u8>();
    dfmt(" clear: {}\n", entry->rgba_clear.to_string_hex_byte());
  }

  // create the opengl output texture.

  // do the clear:
  {
    FramebufferTexturePairContext ctxt(entry->tex.value());
    float positions[3 * 4] = {0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0};
    glUniform3fv(m_uniforms.positions, 4, positions);
    glUniform1i(m_uniforms.enable_tex, 0);
    glUniform4f(m_uniforms.rgba, entry->rgba_clear[0], entry->rgba_clear[1], entry->rgba_clear[2],
                entry->rgba_clear[3]);
    glUniform4i(m_uniforms.channel_scramble, 0, 1, 2, 3);
    glBindTexture(GL_TEXTURE_2D, m_dummy_texture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glDisable(GL_BLEND);
    glDisable(GL_DEPTH_TEST);
    glColorMask(true, true, true, true);
    glDrawArrays(GL_TRIANGLE_FAN, 0, 4);
  }

  // set as active
  m_current_dest_tbp = entry->dest_texture_address;
}

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

void TextureAnimator::handle_draw(DmaFollower& dma) {
  // NOTE: assuming ABE set from the template here. If this function is used for other templates,
  // we'll need to actually check.
  dprintf("[tex anim] Draw\n");
  DrawData draw_data;
  auto draw_xfer = dma.read_and_advance();
  ASSERT(draw_xfer.size_bytes == sizeof(DrawData));
  memcpy(&draw_data, draw_xfer.data, sizeof(DrawData));

  if (m_current_shader.source_texture_set) {
    auto& dest_te = m_dest_textures.at(m_current_dest_tbp);
    FramebufferTexturePairContext ctxt(*dest_te.tex);
    GLuint gpu_texture = make_or_get_gpu_texture_for_current_shader();
    set_up_opengl_for_shader(m_current_shader, gpu_texture, true);  // ABE forced on here.
    set_uniforms_from_draw_data(draw_data, dest_te.tex_width, dest_te.tex_height);
    {
      ASSERT(dest_te.tex);
      glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
    }
    // debug_save_opengl_texture("opengl_draw_result.png", dest_te.tex->texture());
    // debug_save_opengl_texture("opengl_test.png", gpu_texture);
    // ASSERT_NOT_REACHED();
  } else {
    ASSERT_NOT_REACHED();
  }
}

const u32* TextureAnimator::get_current_clut_16_16_psm32() {
  const auto& clut_lookup = m_vram_entries.find(m_current_shader.tex0.cbp());
  if (clut_lookup == m_vram_entries.end()) {
    printf("set shader referenced an unknown clut texture in %d\n", m_current_shader.tex0.cbp());
    ASSERT_NOT_REACHED();
  }

  if (clut_lookup->second.kind != VramEntry::Kind::CLUT16_16_IN_PSM32) {
    ASSERT_NOT_REACHED();
  }

  return (const u32*)clut_lookup->second.data.data();
}

void TextureAnimator::load_clut_to_converter() {
  const auto& clut_lookup = m_vram_entries.find(m_current_shader.tex0.cbp());
  if (clut_lookup == m_vram_entries.end()) {
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
  GLuint gl_tex = alloc_gl_texture();
  glBindTexture(GL_TEXTURE_2D, gl_tex);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
               data);
  glBindTexture(GL_TEXTURE_2D, 0);
  return gl_tex;
}

GLuint TextureAnimator::make_or_get_gpu_texture_for_current_shader() {
  const auto& lookup = m_vram_entries.find(m_current_shader.tex0.tbp0());
  if (lookup == m_vram_entries.end()) {
    // try dest textures
    const auto& dest_lookup = m_dest_textures.find(m_current_shader.tex0.tbp0());
    if (dest_lookup != m_dest_textures.end()) {
      ASSERT(dest_lookup->second.tex);
      return dest_lookup->second.tex->texture();
    }

    // TODO: could try ones from the previous frame?

    printf("referenced an unknown texture in %d\n", m_current_shader.tex0.tbp0());
    ASSERT_NOT_REACHED();
  }

  auto* vram_entry = &lookup->second;

  // see what format the source is
  switch (vram_entry->kind) {
    // data on the CPU, in PSM32
    case VramEntry::Kind::GENERIC_PSM32:
      // see how we're reading it:
      switch (m_current_shader.tex0.psm()) {
        // reading as a different format, needs scrambler.
        case GsTex0::PSM::PSMT8: {
          int w = 1 << m_current_shader.tex0.tw();
          int h = 1 << m_current_shader.tex0.th();
          ASSERT(w == vram_entry->width * 2);
          ASSERT(h == vram_entry->height * 2);

          Timer timer;
          m_converter.upload_width(vram_entry->data.data(), m_current_shader.tex0.tbp0(),
                                   vram_entry->width, vram_entry->height);

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
    case VramEntry::Kind::GENERIC_PSMT8: {
      fmt::print("drawing: {}\n", (int)m_current_shader.tex0.psm());
      ASSERT(m_current_shader.tex0.psm() == GsTex0::PSM::PSMT8);
      ASSERT(m_current_shader.tex0.cpsm() == 0);  // psm32.
      int tw = 1 << m_current_shader.tex0.tw();
      int th = 1 << m_current_shader.tex0.th();
      ASSERT(tw == vram_entry->width);
      ASSERT(th == vram_entry->height);
      std::vector<u32> rgba_data(tw * th);
      const u32* clut = get_current_clut_16_16_psm32();
      for (int r = 0; r < th; r++) {
        for (int c = 0; c < tw; c++) {
          rgba_data[c + r * tw] = clut[vram_entry->data[c + r * tw]];
        }
      }
      return make_temp_gpu_texture(rgba_data.data(), tw, th);
    }

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
  ASSERT(shader.tex0.tcc() == 1);
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
  bool alpha_test_used_as_mask = false;

  // test
  if (shader.test.alpha_test_enable()) {
    auto atst = shader.test.alpha_test();
    if (atst == GsTest::AlphaTest::ALWAYS) {
      do_alpha_test = false;
      // atest effectively disabled - everybody passes.
    } else if (atst == GsTest::AlphaTest::NEVER) {
      do_alpha_test = false;

      switch (shader.test.afail()) {
        case GsTest::AlphaFail::RGB_ONLY:
          alpha_test_used_as_mask = true;
          glColorMask(true, true, true, false);
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

  if (!alpha_test_used_as_mask) {
    glColorMask(true, true, true, true);
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
