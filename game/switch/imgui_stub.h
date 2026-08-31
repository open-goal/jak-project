#pragma once

/*!
 * @file imgui_stub.h
 * Drop-in replacement for third-party/imgui/imgui.h on Switch. ImGui is dev-only debug UI
 * tooling woven through ~35 files across game/graphics/ as per-renderer debug windows -- rather
 * than guard every individual call site, this mirrors the ~58 ImGui:: functions and handful of
 * types actually used, all as no-ops. Widgets that report "did this change / was this clicked"
 * always return false; nothing here is ever actually drawn since the real ImGui backend
 * (imgui_impl_sdl3/imgui_impl_opengl3) is never initialized for Switch (see opengl.cpp).
 */

#include <cstdint>
#include <string>

struct ImVec2 {
  float x = 0, y = 0;
  ImVec2() = default;
  ImVec2(float _x, float _y) : x(_x), y(_y) {}
};
struct ImVec4 {
  float x = 0, y = 0, z = 0, w = 0;
  ImVec4() = default;
  ImVec4(float _x, float _y, float _z, float _w) : x(_x), y(_y), z(_z), w(_w) {}
};

struct ImFont {};
struct ImGuiViewport {
  ImVec2 Pos;
  ImVec2 Size;
  ImVec2 WorkPos;
  ImVec2 WorkSize;
};
struct ImDrawList {
  void AddRectFilled(const ImVec2&, const ImVec2&, uint32_t, float = 0.0f, int = 0) {}
};
struct ImGuiStyle {
  ImVec4 Colors[64] = {};
  ImVec2 FramePadding;
};

using ImTextureID = void*;
using ImWchar = unsigned short;
using ImU32 = uint32_t;
#define IM_COL32(R, G, B, A) \
  (((ImU32)(A) << 24) | ((ImU32)(B) << 16) | ((ImU32)(G) << 8) | ((ImU32)(R)))
using ImGuiWindowFlags = int;
using ImGuiCond = int;
using ImGuiCol = int;
using ImGuiInputTextFlags = int;
using ImGuiSelectableFlags = int;
using ImGuiTreeNodeFlags = int;
using ImGuiConfigFlags = int;

// Real imgui uses plain (non-class) C enums named with a trailing underscore, e.g.
// `ImGuiInputTextFlags_`, whose enumerators are accessible both bare and as
// `ImGuiInputTextFlags_::ImGuiInputTextFlags_Foo` (C++11 allows qualifying a plain enum's
// enumerators by its type name) -- game code uses both forms, so these have to be real enums,
// not macros.
enum ImGuiWindowFlags_ {
  ImGuiWindowFlags_AlwaysAutoResize = 0,
  ImGuiWindowFlags_NoDecoration = 0,
  ImGuiWindowFlags_NoFocusOnAppearing = 0,
  ImGuiWindowFlags_NoNav = 0,
  ImGuiWindowFlags_NoSavedSettings = 0,
};
enum ImGuiCond_ { ImGuiCond_Always = 0 };
enum ImGuiCol_ {
  ImGuiCol_Text = 0,
  ImGuiCol_Button = 0,
  ImGuiCol_Header = 0,
  ImGuiCol_HeaderActive = 0,
  ImGuiCol_HeaderHovered = 0,
  ImGuiCol_MenuBarBg = 0,
};
enum ImGuiInputTextFlags_ {
  ImGuiInputTextFlags_None = 0,
  ImGuiInputTextFlags_AutoSelectAll = 0,
  ImGuiInputTextFlags_CharsDecimal = 0,
  ImGuiInputTextFlags_ReadOnly = 0,
};
enum ImGuiSelectableFlags_ { ImGuiSelectableFlags_DontClosePopups = 0 };
enum ImGuiConfigFlags_ { ImGuiConfigFlags_NoMouseCursorChange = 0 };
#define IMGUI_CHECKVERSION() ((void)0)

struct ImGuiIO {
  ImGuiConfigFlags ConfigFlags = 0;
  const char* IniFilename = nullptr;
  const char* LogFilename = nullptr;
  bool WantCaptureKeyboard = false;
  bool WantCaptureMouse = false;
};

namespace ImGui {
inline void CreateContext() {}
inline void DestroyContext() {}
inline ImGuiIO& GetIO() {
  static ImGuiIO io;
  return io;
}
inline ImGuiStyle& GetStyle() {
  static ImGuiStyle style;
  return style;
}
inline ImVec4 GetStyleColorVec4(ImGuiCol) {
  return ImVec4(0, 0, 0, 0);
}
inline ImGuiViewport* GetMainViewport() {
  static ImGuiViewport vp;
  return &vp;
}
inline void NewFrame() {}
inline void Render() {}
inline void* GetDrawData() {
  return nullptr;
}

inline bool Begin(const char*, bool* = nullptr, ImGuiWindowFlags = 0) {
  return false;
}
inline void End() {}
inline bool BeginMenu(const char*, bool = true) {
  return false;
}
inline void EndMenu() {}
inline bool BeginCombo(const char*, const char*, int = 0) {
  return false;
}
inline void EndCombo() {}
inline bool Combo(const char*, int*, const char* const[], int, int = -1) {
  return false;
}
inline bool MenuItem(const char*, const char* = nullptr, bool = false, bool = true) {
  return false;
}
inline bool BeginMainMenuBar() {
  return false;
}
inline void EndMainMenuBar() {}
inline void BeginDisabled(bool = true) {}
inline void EndDisabled() {}

inline void Text(const char*, ...) {}
inline void TextColored(const ImVec4&, const char*, ...) {}
inline void TextUnformatted(const char*, const char* = nullptr) {}
inline void TextWrapped(const char*, ...) {}
inline bool Button(const char*, const ImVec2& = ImVec2(0, 0)) {
  return false;
}
inline bool RadioButton(const char*, bool) {
  return false;
}
inline bool Selectable(const char*, bool = false, ImGuiSelectableFlags = 0,
                       const ImVec2& = ImVec2(0, 0)) {
  return false;
}
inline bool ListBox(const char*, int*, const char* const[], int, int = -1) {
  return false;
}
inline void Dummy(const ImVec2&) {}
inline ImVec2 CalcTextSize(const char*, const char* = nullptr, bool = false, float = -1.0f) {
  return ImVec2(0, 0);
}
inline ImDrawList* GetWindowDrawList() {
  static ImDrawList dl;
  return &dl;
}
inline ImVec2 GetWindowPos() {
  return ImVec2(0, 0);
}
inline bool Checkbox(const char*, bool* v) {
  (void)v;
  return false;
}
inline bool SliderFloat(const char*, float*, float, float, const char* = "%.3f", int = 0) {
  return false;
}
inline bool InputText(const char*, char*, size_t, ImGuiInputTextFlags = 0, void* = nullptr,
                      void* = nullptr) {
  return false;
}
inline bool InputText(const char*, std::string*, ImGuiInputTextFlags = 0, void* = nullptr,
                      void* = nullptr) {
  return false;
}
inline bool InputInt(const char*, int*, int = 1, int = 100, ImGuiInputTextFlags = 0) {
  return false;
}
inline bool InputInt2(const char*, int[2], ImGuiInputTextFlags = 0) {
  return false;
}
inline bool InputFloat(const char*, float*, float = 0.0f, float = 0.0f, const char* = "%.3f",
                       ImGuiInputTextFlags = 0) {
  return false;
}
inline bool TreeNode(const char*) {
  return false;
}
inline bool TreeNode(const char*, const char*, ...) {
  return false;
}
inline void TreePop() {}
inline void Image(ImTextureID, const ImVec2&) {}
inline void PlotLines(const char*, const float*, int, int = 0, const char* = nullptr,
                      float = 3.40282e+38F, float = 3.40282e+38F, ImVec2 = ImVec2(0, 0)) {}
inline void PlotLines(const char*, float (*)(void*, int), void*, int, int = 0,
                      const char* = nullptr, float = 3.40282e+38F, float = 3.40282e+38F,
                      ImVec2 = ImVec2(0, 0)) {}

inline void SameLine(float = 0.0f, float = -1.0f) {}
inline void Separator() {}
inline void NewLine() {}
inline void PushID(const char*) {}
inline void PushID(int) {}
inline void PopID() {}
inline void PushStyleColor(ImGuiCol, const ImVec4&) {}
inline void PushStyleColor(ImGuiCol, ImU32) {}
inline void PopStyleColor(int = 1) {}
inline void SetItemDefaultFocus() {}
inline void SetNextWindowPos(const ImVec2&, ImGuiCond = 0, const ImVec2& = ImVec2(0, 0)) {}
inline void SetNextWindowBgAlpha(float) {}
inline void SetNextItemOpen(bool, ImGuiCond = 0) {}
inline bool IsItemHovered(int = 0) {
  return false;
}

// jak-project's own style helpers (third-party/imgui/imgui_style.h), not real imgui API.
inline void applyFontStyle() {}
inline void applyAlternateStyle() {}
inline void applyClassicStyle() {}
}  // namespace ImGui
