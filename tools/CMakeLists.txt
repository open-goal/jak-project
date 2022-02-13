add_subdirectory(level_tools)

add_executable(dgo_unpacker
        dgo_unpacker.cpp)
target_link_libraries(dgo_unpacker common)

add_executable(dgo_packer
        dgo_packer.cpp)
target_link_libraries(dgo_packer common)

add_executable(memory_dump_tool
        MemoryDumpTool/main.cpp)
target_link_libraries(memory_dump_tool common decomp)

