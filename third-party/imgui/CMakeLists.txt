
add_library(imgui
        imgui.cpp
        imgui_draw.cpp
        imgui_tables.cpp
        imgui_widgets.cpp
        imgui_impl_glfw.cpp
        imgui_impl_opengl3.cpp "imgui_stdlib.cpp" "imgui_stdlib.h")

target_link_libraries(imgui glfw)
