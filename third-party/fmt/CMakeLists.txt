set(CMAKE_CXX_FLAGS "-O3")

include_directories(../)
add_library(fmt SHARED format.cc)