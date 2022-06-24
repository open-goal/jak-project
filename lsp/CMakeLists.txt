add_executable(lsp main.cpp "inmemoryconnector.h" "cpphttplibconnector.h" "warehouseapp.cpp")

target_link_libraries(lsp common decomp)

