add_executable(lsp main.cpp   "transport/stdio.h" "transport/stdio.cpp" "handlers/lsp_handler.h" "workspace.h" "workspace.cpp")

target_link_libraries(lsp common decomp)

