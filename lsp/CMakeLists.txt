include_directories(./)

add_executable(lsp
main.cpp
transport/stdio.cpp
state/workspace.cpp
handlers/lsp_router.cpp
"protocol/error_codes.h"
handlers/text_document/document_symbol.hpp)

target_link_libraries(lsp common decomp)

