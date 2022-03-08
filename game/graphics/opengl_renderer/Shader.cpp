#include "Shader.h"
#include "common/util/FileUtil.h"
#include "common/log/log.h"
#include "game/graphics/pipelines/opengl.h"
#include "common/util/Assert.h"

Shader::Shader(const std::string& shader_name) {
  // read the shader source
  auto vert_src =
      file_util::read_text_file(file_util::get_file_path({shader_folder, shader_name + ".vert"}));
  auto frag_src =
      file_util::read_text_file(file_util::get_file_path({shader_folder, shader_name + ".frag"}));

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
    lg::error("Failed to compile vertex shader {}:\n{}\n", shader_name.c_str(), err);
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
    lg::error("Failed to compile fragment shader {}:\n{}\n", shader_name.c_str(), err);
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
    lg::error("Failed to link shader {}:\n{}\n", shader_name.c_str(), err);
    m_is_okay = false;
    return;
  }

  glDeleteShader(m_vert_shader);
  glDeleteShader(m_frag_shader);
  m_is_okay = true;
}

void Shader::activate() {
  ASSERT(m_is_okay);
  glUseProgram(m_program);
}

ShaderLibrary::ShaderLibrary() {
  at(ShaderId::TEST_SHADER) = {"test_shader"};
  at(ShaderId::DIRECT_BASIC) = {"direct_basic"};
  at(ShaderId::DIRECT_BASIC_TEXTURED) = {"direct_basic_textured"};
  at(ShaderId::DEBUG_RED) = {"debug_red"};
  at(ShaderId::SPRITE) = {"sprite_3d"};
  at(ShaderId::SKY) = {"sky"};
  at(ShaderId::SKY_BLEND) = {"sky_blend"};
  at(ShaderId::TFRAG3) = {"tfrag3"};
  at(ShaderId::TFRAG3_NO_TEX) = {"tfrag3_no_tex"};
  at(ShaderId::SPRITE3) = {"sprite3_3d"};
  at(ShaderId::DIRECT2) = {"direct2"};
  at(ShaderId::EYE) = {"eye"};
  at(ShaderId::GENERIC) = {"generic"};
}
