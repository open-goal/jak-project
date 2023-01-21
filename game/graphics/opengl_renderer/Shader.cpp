#include "Shader.h"

#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"

#include "game/graphics/pipelines/opengl.h"

Shader::Shader(const std::string& shader_name, GameVersion version) {
  std::string height_scale = version == GameVersion::Jak1 ? "1.0" : "0.5";
  // read the shader source
  auto vert_src =
      file_util::read_text_file(file_util::get_file_path({shader_folder, shader_name + ".vert"}));
  auto frag_src =
      file_util::read_text_file(file_util::get_file_path({shader_folder, shader_name + ".frag"}));

  vert_src = std::regex_replace(vert_src, std::regex("HEIGHT_SCALE"), height_scale);

  m_vert_shader = glCreateShader(GL_VERTEX_SHADER);
  const char* src = vert_src.c_str();
  glShaderSource(m_vert_shader, 1, &src, nullptr);
  glCompileShader(m_vert_shader);

  constexpr int len = 1024;
  int compile_ok;
  char err[len];

  glGetShaderiv(m_vert_shader, GL_COMPILE_STATUS, &compile_ok);
  if (!compile_ok) {
    glGetShaderInfoLog(m_vert_shader, len, nullptr, err);
    lg::error("Failed to compile vertex shader {}:\n{}", shader_name.c_str(), err);
    m_is_okay = false;
    return;
  }

  m_frag_shader = glCreateShader(GL_FRAGMENT_SHADER);
  src = frag_src.c_str();
  glShaderSource(m_frag_shader, 1, &src, nullptr);
  glCompileShader(m_frag_shader);

  glGetShaderiv(m_frag_shader, GL_COMPILE_STATUS, &compile_ok);
  if (!compile_ok) {
    glGetShaderInfoLog(m_frag_shader, len, nullptr, err);
    lg::error("Failed to compile fragment shader {}:\n{}", shader_name.c_str(), err);
    m_is_okay = false;
    return;
  }

  m_program = glCreateProgram();
  glAttachShader(m_program, m_vert_shader);
  glAttachShader(m_program, m_frag_shader);
  glLinkProgram(m_program);

  glGetProgramiv(m_program, GL_LINK_STATUS, &compile_ok);
  if (!compile_ok) {
    glGetProgramInfoLog(m_program, len, nullptr, err);
    lg::error("Failed to link shader {}:\n{}", shader_name.c_str(), err);
    m_is_okay = false;
    return;
  }

  glDeleteShader(m_vert_shader);
  glDeleteShader(m_frag_shader);
  m_is_okay = true;
}

void Shader::activate() const {
  ASSERT(m_is_okay);
  glUseProgram(m_program);
}

ShaderLibrary::ShaderLibrary(GameVersion version) {
  at(ShaderId::SOLID_COLOR) = {"solid_color", version};
  at(ShaderId::DIRECT_BASIC) = {"direct_basic", version};
  at(ShaderId::DIRECT_BASIC_TEXTURED) = {"direct_basic_textured", version};
  at(ShaderId::DEBUG_RED) = {"debug_red", version};
  at(ShaderId::SPRITE) = {"sprite_3d", version};
  at(ShaderId::SKY) = {"sky", version};
  at(ShaderId::SKY_BLEND) = {"sky_blend", version};
  at(ShaderId::TFRAG3) = {"tfrag3", version};
  at(ShaderId::TFRAG3_NO_TEX) = {"tfrag3_no_tex", version};
  at(ShaderId::SPRITE3) = {"sprite3_3d", version};
  at(ShaderId::DIRECT2) = {"direct2", version};
  at(ShaderId::EYE) = {"eye", version};
  at(ShaderId::GENERIC) = {"generic", version};
  at(ShaderId::OCEAN_TEXTURE) = {"ocean_texture", version};
  at(ShaderId::OCEAN_TEXTURE_MIPMAP) = {"ocean_texture_mipmap", version};
  at(ShaderId::OCEAN_COMMON) = {"ocean_common", version};
  at(ShaderId::SHRUB) = {"shrub", version};
  at(ShaderId::SHADOW) = {"shadow", version};
  at(ShaderId::COLLISION) = {"collision", version};
  at(ShaderId::MERC2) = {"merc2", version};
  at(ShaderId::SPRITE_DISTORT) = {"sprite_distort", version};
  at(ShaderId::SPRITE_DISTORT_INSTANCED) = {"sprite_distort_instanced", version};
  at(ShaderId::POST_PROCESSING) = {"post_processing", version};
  at(ShaderId::DEPTH_CUE) = {"depth_cue", version};
  at(ShaderId::EMERC) = {"emerc", version};

  for (auto& shader : m_shaders) {
    ASSERT_MSG(shader.okay(), "error compiling shader");
  }
}
