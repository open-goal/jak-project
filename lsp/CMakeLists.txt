add_executable(lsp
  main.cpp
  transport/stdio.cpp
  state/workspace.cpp
  handlers/lsp_router.cpp
  protocol/common_types.cpp
  protocol/completion.cpp
  protocol/document_symbols.cpp
  protocol/document_synchronization.cpp
  protocol/document_diagnostics.cpp
  protocol/hover.cpp
  state/data/mips_instruction.cpp)

target_link_libraries(lsp common decomp)

