if (UNIX)
    set(CMAKE_CXX_FLAGS "-O3")
else ()
    set(CMAKE_CXX_FLAGS "/EHsc")
endif (UNIX)

include_directories(../)
add_library(tiny_gltf tiny_gltf.cpp)

