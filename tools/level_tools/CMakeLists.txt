

add_library(level_tools goal_data_reader.cpp BspHeader.cpp)
target_link_libraries(level_tools fmt common decomp)

add_executable(level_dump level_dump/main.cpp)
target_link_libraries(level_dump fmt common decomp level_tools)