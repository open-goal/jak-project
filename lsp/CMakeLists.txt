include_directories(./)

add_executable(lsp
  main.cpp
  transport/stdio.cpp
  state/workspace.cpp
  handlers/lsp_router.cpp
  handlers/initialize.hpp
  handlers/text_document/document_symbol.hpp
  handlers/text_document/document_synchronization.hpp
  protocol/common_types.cpp
  protocol/document_symbols.cpp
  protocol/document_synchronization.cpp)

target_link_libraries(lsp common decomp)

