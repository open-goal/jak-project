#include "gfx_test.h"

#include "game/system/hid/sdl_util.h"

#include "third-party/glad/include/glad/glad.h"

namespace tests {
void to_json(json& j, const GPUTestOutput& obj) {
  json_serialize(success);
  json_serialize(error);
  json_serialize(errorCause);
  json_serialize_optional(gpuRendererString);
  json_serialize_optional(gpuVendorString);
}

GPUTestOutput run_gpu_test(const std::string& test_type) {
  lg::info("Running GPU Test - {}", test_type);
  GPUTestOutput output = {false, "", ""};
  if (test_type == "opengl") {
    if (SDL_Init(SDL_INIT_VIDEO) < 0) {
      output = {false, "SDL initialization failed",
                sdl_util::log_and_return_error("SDL initialization failed")};
      return output;
    }
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 4);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);
#ifdef __APPLE__
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 1);
#endif
    SDL_Window* window =
        SDL_CreateWindow("OpenGL Version", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 800,
                         600, SDL_WINDOW_OPENGL | SDL_WINDOW_HIDDEN);
    if (!window) {
      output = {false, "SDL window creation failed",
                sdl_util::log_and_return_error("SDL initialization failed")};
      SDL_Quit();
      return output;
    }
    SDL_GLContext glContext = SDL_GL_CreateContext(window);
    if (!glContext) {
      output = {false, "Required OpenGL Version is not supported",
                sdl_util::log_and_return_error("SDL initialization failed")};
    } else {
      gladLoadGLLoader((GLADloadproc)SDL_GL_GetProcAddress);
      if (!gladLoadGL()) {
        output = {false, "Unable to init GLAD", ""};
      } else {
        output = {true, "", ""};
        const auto rendererString = glGetString(GL_RENDERER);
        if (rendererString) {
          output.gpuRendererString = (const char*)rendererString;
        }
        const auto vendorString = glGetString(GL_VENDOR);
        if (vendorString) {
          output.gpuVendorString = (const char*)vendorString;
        }
      }
      SDL_GL_DeleteContext(glContext);
    }
    SDL_DestroyWindow(window);
    SDL_Quit();
  } else {
    lg::error("Invalid GPU test type - {}", test_type);
  }
  return output;
}
};  // namespace tests
