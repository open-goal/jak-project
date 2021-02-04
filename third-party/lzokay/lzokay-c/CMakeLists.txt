project(lzokay-c VERSION 0.1 LANGUAGES C CXX)

add_library(lzokay-c lzokay-c.cpp lzokay-c.h)
target_link_libraries(lzokay-c PRIVATE lzokay)
set_target_properties(lzokay-c PROPERTIES CXX_STANDARD 14)
