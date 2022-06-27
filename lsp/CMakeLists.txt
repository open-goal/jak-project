add_executable(lsp main.cpp lsp_server.cpp transport.h)

target_link_libraries(lsp common decomp)

