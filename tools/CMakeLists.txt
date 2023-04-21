add_subdirectory(level_tools)

add_executable(dgo_unpacker
        dgo_unpacker.cpp)
target_link_libraries(dgo_unpacker common)

add_executable(dgo_packer
        dgo_packer.cpp)
target_link_libraries(dgo_packer common)

add_executable(memory_dump_tool
        memory_dump_tool/main.cpp)
target_link_libraries(memory_dump_tool common decomp)

add_executable(type_searcher
        type_searcher/main.cpp)
target_link_libraries(type_searcher common decomp)

add_executable(formatter
        formatter/main.cpp)
target_link_libraries(formatter common tree-sitter)
