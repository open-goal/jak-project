#pragma once

#include <string>

class Shader {
 public:
  static constexpr char shader_folder[] = "game/graphics/opengl_renderer/shaders/";
  Shader(const std::string& shader_name);
  Shader() = default;
  void activate();
  bool okay() const { return m_is_okay; }
  uint id() const { return m_program; }

 private:
  uint m_frag_shader = 0;
  uint m_vert_shader = 0;
  uint m_program = 0;
  bool m_is_okay = false;
};

// note: update the constructor in Shader.cpp
enum class ShaderId { TEST_SHADER = 0, DIRECT_BASIC = 1, DIRECT_BASIC_TEXTURED = 2, MAX_SHADERS };

class ShaderLibrary {
 public:
  ShaderLibrary();
  Shader& operator[](ShaderId id) { return m_shaders[(int)id]; }

  Shader& at(ShaderId id) { return m_shaders[(int)id]; }

 private:
  Shader m_shaders[(int)ShaderId::MAX_SHADERS];
};