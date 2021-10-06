#pragma once

#include <string>

#include "common/common_types.h"

class Shader {
 public:
  static constexpr char shader_folder[] = "game/graphics/opengl_renderer/shaders/";
  Shader(const std::string& shader_name);
  Shader() = default;
  void activate();
  bool okay() const { return m_is_okay; }
  u64 id() const { return m_program; }

 private:
  u64 m_frag_shader = 0;
  u64 m_vert_shader = 0;
  u64 m_program = 0;
  bool m_is_okay = false;
};

// note: update the constructor in Shader.cpp
enum class ShaderId {
  TEST_SHADER = 0,
  DIRECT_BASIC = 1,
  DIRECT_BASIC_TEXTURED = 2,
  DIRECT_BASIC_TEXTURED_TCC0 = 3,
  DEBUG_RED = 4,
  SPRITE_CPU = 5,
  SPRITE_CPU_AFAIL = 6,
  SKY,
  SKY_BLEND,
  MAX_SHADERS
};

class ShaderLibrary {
 public:
  ShaderLibrary();
  Shader& operator[](ShaderId id) { return m_shaders[(int)id]; }

  Shader& at(ShaderId id) { return m_shaders[(int)id]; }

 private:
  Shader m_shaders[(int)ShaderId::MAX_SHADERS];
};