add_library(imgui
        imgui.cpp
        imgui_draw.cpp
        imgui_tables.cpp
        imgui_widgets.cpp
        imgui_impl_sdl.cpp
        imgui_impl_opengl3.cpp
        imgui_stdlib.cpp
        imgui_style.cpp)

target_link_libraries(imgui SDL2::SDL2)
