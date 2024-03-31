add_executable(lsp
  handlers/lsp_router.cpp
  handlers/initialize.cpp
  handlers/text_document/completion.cpp
  handlers/text_document/document_color.cpp
  handlers/text_document/document_symbol.cpp
  handlers/text_document/document_synchronization.cpp
  handlers/text_document/formatting.cpp
  handlers/text_document/go_to.cpp
  handlers/text_document/hover.cpp
  handlers/text_document/type_hierarchy.cpp
  main.cpp
  protocol/common_types.cpp
  protocol/completion.cpp
  protocol/document_color.cpp
  protocol/document_diagnostics.cpp
  protocol/document_symbols.cpp
  protocol/document_synchronization.cpp
  protocol/formatting.cpp
  protocol/hover.cpp
  protocol/progress_report.cpp
  protocol/type_hierarchy.cpp
  state/data/mips_instruction.cpp
  state/lsp_requester.cpp
  state/workspace.cpp
  transport/stdio.cpp
  lsp_util.cpp)

target_compile_definitions(lsp PRIVATE -DJSON_DIAGNOSTICS=1)

target_link_libraries(lsp common decomp compiler tree-sitter)
